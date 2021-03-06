#include "include/rcore.h"
#include "include/scores.h"

#define DEBUG_BEFORE() \
  if (debuglevel > 0) { \
    Rprintf("----------------------------------------------------------------\n"); \
    Rprintf("* processing node %s.\n", CHAR(STRING_ELT(cur, 0))); \
  }/*THEN*/

/* R frontend: compute the score component for each target node. */
SEXP per_node_score(SEXP network, SEXP data, SEXP score, SEXP targets,
    SEXP extra_args, SEXP debug) {

SEXP result;

  /* allocate the return value. */
  PROTECT(result = allocVector(REALSXP, length(targets)));
  /* compute the score componenets. */
  c_per_node_score(network, data, score, targets, extra_args, isTRUE(debug),
    REAL(result));
  /* set labels on the computed score components. */
  setAttrib(result, R_NamesSymbol, targets);

  UNPROTECT(1);

  return result;

}/*PER_NODE_SCORE*/

/* C backend: compute the score component for each target node. */
void c_per_node_score(SEXP network, SEXP data, SEXP score, SEXP targets,
    SEXP extra_args, int debuglevel, double *res) {

int i = 0, ntargets = length(targets);
score_e s = score_label(CHAR(STRING_ELT(score, 0)));
double nparams = 0, *k = NULL;
SEXP cur, iss, prior, beta, exp, phi, weights;
weights = getListElement(extra_args, "weights");

  /* allocate dummy variable for the current node's label. */
  PROTECT(cur = allocVector(STRSXP, 1));

  switch(s) {

    /* discrete log-likelihood score. */
    case LOGLIK:
      for (i = 0; i < ntargets; i++) {

        SET_STRING_ELT(cur, 0, STRING_ELT(targets, i));
        DEBUG_BEFORE();
        res[i] = loglik_dnode(cur, network, data, NULL, debuglevel, weights);

      }/*FOR*/
      break;

    /* Gaussian log-likelihood score. */
    case LOGLIK_G:
      for (i = 0; i < ntargets; i++) {

        SET_STRING_ELT(cur, 0, STRING_ELT(targets, i));
        DEBUG_BEFORE();
        res[i] = loglik_gnode(cur, network, data, NULL, debuglevel);

      }/*FOR*/
      break;

    /* Conditional Linear Gaussian log-likelihood score. */
    case LOGLIK_CG:
      for (i = 0; i < ntargets; i++) {

        SET_STRING_ELT(cur, 0, STRING_ELT(targets, i));
        DEBUG_BEFORE();
        res[i] = loglik_cgnode(cur, network, data, NULL, debuglevel);

      }/*FOR*/
      break;

    /* AIC and BIC scores, discrete data. */
    case AIC:
    case BIC:

      k = REAL(getListElement(extra_args, "k"));

      for (i = 0; i < ntargets; i++) {

        SET_STRING_ELT(cur, 0, STRING_ELT(targets, i));
        DEBUG_BEFORE();
        res[i] = loglik_dnode(cur, network, data, &nparams, debuglevel, weights);
        res[i] -= (*k) * nparams;

        if (debuglevel > 0)
          Rprintf("  > penalty is %lf x %.0lf = %lf.\n", *k, nparams, (*k) * nparams);

      }/*FOR*/
      break;

    /* AIC and BIC scores, Gaussian data. */
    case AIC_G:
    case BIC_G:

      k = REAL(getListElement(extra_args, "k"));

      for (i = 0; i < ntargets; i++) {

        SET_STRING_ELT(cur, 0, STRING_ELT(targets, i));
        DEBUG_BEFORE();
        res[i] = loglik_gnode(cur, network, data, &nparams, debuglevel);
        res[i] -= (*k) * nparams;

        if (debuglevel > 0)
          Rprintf("  > penalty is %lf x %.0lf = %lf.\n", *k, nparams, (*k) * nparams);

      }/*FOR*/
      break;

    /* AIC and BIC scores, Conditional Linear Gaussian data. */
    case AIC_CG:
    case BIC_CG:

      k = REAL(getListElement(extra_args, "k"));

      for (i = 0; i < ntargets; i++) {

        SET_STRING_ELT(cur, 0, STRING_ELT(targets, i));
        DEBUG_BEFORE();
        res[i] = loglik_cgnode(cur, network, data, &nparams, debuglevel);
        res[i] -= (*k) * nparams;

        if (debuglevel > 0)
          Rprintf("  > penalty is %lf x %.0lf = %lf.\n", *k, nparams, (*k) * nparams);

      }/*FOR*/
      break;

    /* Bayesian Dirichlet equivalent score (BDe) and sparse score (BDs). */
    case BDE:
    case BDS:

      iss = getListElement(extra_args, "iss");
      prior = getListElement(extra_args, "prior");
      beta = getListElement(extra_args, "beta");

      for (i = 0; i < ntargets; i++) {

        SET_STRING_ELT(cur, 0, STRING_ELT(targets, i));
        DEBUG_BEFORE();
        res[i] = dirichlet_node(cur, network, data, iss, FALSE, prior, beta,
                   R_NilValue, (s == BDS), debuglevel, weights);

      }/*FOR*/
      break;

    /* Bayesian Dirichlet score with Jeffrey's prior, K2 score. */
    case BDJ:
    case K2:
      for (i = 0; i < ntargets; i++) {

        PROTECT(iss = (s == K2) ? ScalarReal(1) : ScalarReal(0.5));

        SET_STRING_ELT(cur, 0, STRING_ELT(targets, i));
        DEBUG_BEFORE();
        res[i] = dirichlet_node(cur, network, data, iss, TRUE,
                   R_NilValue, R_NilValue, R_NilValue, FALSE, debuglevel, weights);

        UNPROTECT(1);

      }/*FOR*/
      break;

    /* Bayesian Gaussian equivalent score (BGe). */
    case BGE:

      iss = getListElement(extra_args, "iss");
      phi = getListElement(extra_args, "phi");
      prior = getListElement(extra_args, "prior");
      beta = getListElement(extra_args, "beta");

      for (i = 0; i < ntargets; i++) {

        SET_STRING_ELT(cur, 0, STRING_ELT(targets, i));
        DEBUG_BEFORE();
        res[i] = wishart_node(cur, network, data, iss, phi, prior, beta, debuglevel);

      }/*FOR*/
      break;

    /* Mixture Bayesian Dirichlet equivalent score (mBDe). */
    case MBDE:

      iss = getListElement(extra_args, "iss");
      exp = getListElement(extra_args, "exp");
      prior = getListElement(extra_args, "prior");
      beta = getListElement(extra_args, "beta");

      for (i = 0; i < ntargets; i++) {

        SET_STRING_ELT(cur, 0, STRING_ELT(targets, i));
        DEBUG_BEFORE();
        res[i] = dirichlet_node(cur, network, data, iss, FALSE, prior, beta, exp,
                   FALSE, debuglevel, weights);

      }/*FOR*/
      break;

    default:
      error("unknown score function.");

  }/*SWITCH*/

  UNPROTECT(1);

}/*C_PER_NODE_SCORE*/

