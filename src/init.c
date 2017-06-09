#include "include/rcore.h"
#include <R_ext/Rdynload.h>

extern SEXP all_equal_bn(SEXP, SEXP);
extern SEXP allsubs_test(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP alpha_star(SEXP, SEXP, SEXP);
extern SEXP amat2arcs(SEXP, SEXP);
extern SEXP aracne(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP arcs_cg_assumptions(SEXP, SEXP, SEXP);
extern SEXP arcs_rbind(SEXP, SEXP, SEXP);
extern SEXP arcs2amat(SEXP, SEXP);
extern SEXP arcs2elist(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP bn_recovery(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP bootstrap_arc_coefficients(SEXP, SEXP);
extern SEXP bootstrap_reduce(SEXP);
extern SEXP bootstrap_strength_counters(SEXP, SEXP, SEXP, SEXP);
extern SEXP cache_partial_structure(SEXP, SEXP, SEXP, SEXP);
extern SEXP cache_structure(SEXP, SEXP, SEXP);
extern SEXP castelo_completion(SEXP, SEXP, SEXP);
extern SEXP ccgpred(SEXP, SEXP, SEXP, SEXP);
extern SEXP cdpred(SEXP, SEXP, SEXP, SEXP);
extern SEXP cg_banned_arcs(SEXP, SEXP);
extern SEXP cgpred(SEXP, SEXP, SEXP);
extern SEXP cgsd(SEXP, SEXP, SEXP);
extern SEXP chow_liu(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP class_err(SEXP, SEXP);
extern SEXP configurations(SEXP, SEXP, SEXP);
extern SEXP cpdag(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP cpdist_lw(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP dag2ug(SEXP, SEXP, SEXP);
extern SEXP data_frame_finite(SEXP);
extern SEXP data_type(SEXP);
extern SEXP dataframe_column(SEXP, SEXP, SEXP);
extern SEXP dedup(SEXP, SEXP, SEXP);
extern SEXP dpred(SEXP, SEXP, SEXP, SEXP);
extern SEXP elist2arcs(SEXP);
extern SEXP empty_graph(SEXP, SEXP);
extern SEXP entropy_loss(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP fast_cglm(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP fast_lm(SEXP, SEXP, SEXP, SEXP);
extern SEXP fit2arcs(SEXP);
extern SEXP fitted_mb(SEXP, SEXP);
extern SEXP fitted_vs_data(SEXP, SEXP, SEXP);
extern SEXP get_test_counter();
extern SEXP gpred(SEXP, SEXP, SEXP);
extern SEXP has_pdag_path(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP hc_opt_step(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP hc_to_be_added(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP ide_cozman_graph(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP increment_test_counter(SEXP);
extern SEXP indep_test(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP is_cauchy_schwarz(SEXP);
extern SEXP is_dag(SEXP, SEXP);
extern SEXP is_listed(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP is_pdag_acyclic(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP is_row_equal(SEXP, SEXP);
extern SEXP is_symmetric(SEXP);
extern SEXP lw_weights(SEXP, SEXP, SEXP, SEXP);
extern SEXP mappred(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP match_brace(SEXP, SEXP, SEXP, SEXP);
extern SEXP mean_strength(SEXP, SEXP, SEXP);
extern SEXP mi(SEXP, SEXP, SEXP, SEXP);
extern SEXP minimal_data_frame(SEXP);
extern SEXP minimal_table(SEXP, SEXP);
extern SEXP naivepred(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP nbr2arcs(SEXP);
extern SEXP normalize_cpt(SEXP);
extern SEXP nparams_cgnet(SEXP, SEXP, SEXP);
extern SEXP nparams_fitted(SEXP, SEXP, SEXP);
extern SEXP num_arcs(SEXP);
extern SEXP onLoad();
extern SEXP onUnload();
extern SEXP ordered_graph(SEXP, SEXP, SEXP);
extern SEXP pdag_extension(SEXP, SEXP, SEXP);
extern SEXP pdag2dag(SEXP, SEXP);
extern SEXP per_node_score(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP rbn_master(SEXP, SEXP, SEXP, SEXP);
extern SEXP reset_test_counter();
extern SEXP root_nodes(SEXP, SEXP);
extern SEXP roundrobin_test(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP score_cache_fill(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP score_delta(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP shd(SEXP, SEXP, SEXP);
extern SEXP smart_network_averaging(SEXP, SEXP, SEXP);
extern SEXP subsets(SEXP, SEXP);
extern SEXP tabu_hash(SEXP, SEXP, SEXP, SEXP);
extern SEXP tabu_step(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP tiers(SEXP, SEXP);
extern SEXP topological_ordering(SEXP, SEXP, SEXP, SEXP);
extern SEXP tree_directions(SEXP, SEXP, SEXP, SEXP);
extern SEXP unique_arcs(SEXP, SEXP, SEXP);
extern SEXP vstructures(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP which_undirected(SEXP, SEXP);

#define CALL_ENTRY(fun, args) \
  {"call_"#fun,                (DL_FUNC) &fun,                 args}

static const R_CallMethodDef CallEntries[] = {
  CALL_ENTRY(all_equal_bn, 2),
  CALL_ENTRY(allsubs_test, 12),
  CALL_ENTRY(alpha_star, 3),
  CALL_ENTRY(amat2arcs, 2),
  CALL_ENTRY(aracne, 5),
  CALL_ENTRY(arcs_cg_assumptions, 3),
  CALL_ENTRY(arcs_rbind, 3),
  CALL_ENTRY(arcs2amat, 2),
  CALL_ENTRY(arcs2elist, 6),
  CALL_ENTRY(bn_recovery, 5),
  CALL_ENTRY(bootstrap_arc_coefficients, 2),
  CALL_ENTRY(bootstrap_reduce, 1),
  CALL_ENTRY(bootstrap_strength_counters, 4),
  CALL_ENTRY(cache_partial_structure, 4),
  CALL_ENTRY(cache_structure, 3),
  CALL_ENTRY(castelo_completion, 3),
  CALL_ENTRY(ccgpred, 4),
  CALL_ENTRY(cdpred, 4),
  CALL_ENTRY(cg_banned_arcs, 2),
  CALL_ENTRY(cgpred, 3),
  CALL_ENTRY(cgsd, 3),
  CALL_ENTRY(chow_liu, 7),
  CALL_ENTRY(class_err, 2),
  CALL_ENTRY(configurations, 3),
  CALL_ENTRY(cpdag, 8),
  CALL_ENTRY(cpdist_lw, 5),
  CALL_ENTRY(dag2ug, 3),
  CALL_ENTRY(data_frame_finite, 1),
  CALL_ENTRY(data_type, 1),
  CALL_ENTRY(dataframe_column, 3),
  CALL_ENTRY(dedup, 3),
  CALL_ENTRY(dpred, 4),
  CALL_ENTRY(elist2arcs, 1),
  CALL_ENTRY(empty_graph, 2),
  CALL_ENTRY(entropy_loss, 5),
  CALL_ENTRY(fast_cglm, 5),
  CALL_ENTRY(fast_lm, 4),
  CALL_ENTRY(fit2arcs, 1),
  CALL_ENTRY(fitted_mb, 2),
  CALL_ENTRY(fitted_vs_data, 3),
  CALL_ENTRY(get_test_counter, 0),
  CALL_ENTRY(gpred, 3),
  CALL_ENTRY(has_pdag_path, 8),
  CALL_ENTRY(hc_opt_step, 10),
  CALL_ENTRY(hc_to_be_added, 7),
  CALL_ENTRY(ide_cozman_graph, 8),
  CALL_ENTRY(increment_test_counter, 1),
  CALL_ENTRY(indep_test, 9),
  CALL_ENTRY(is_cauchy_schwarz, 1),
  CALL_ENTRY(is_dag, 2),
  CALL_ENTRY(is_listed, 5),
  CALL_ENTRY(is_pdag_acyclic, 5),
  CALL_ENTRY(is_row_equal, 2),
  CALL_ENTRY(is_symmetric, 1),
  CALL_ENTRY(lw_weights, 4),
  CALL_ENTRY(mappred, 7),
  CALL_ENTRY(match_brace, 4),
  CALL_ENTRY(mean_strength, 3),
  CALL_ENTRY(mi, 4),
  CALL_ENTRY(minimal_data_frame, 1),
  CALL_ENTRY(minimal_table, 2),
  CALL_ENTRY(naivepred, 7),
  CALL_ENTRY(nbr2arcs, 1),
  CALL_ENTRY(normalize_cpt, 1),
  CALL_ENTRY(nparams_cgnet, 3),
  CALL_ENTRY(nparams_fitted, 3),
  CALL_ENTRY(num_arcs, 1),
  CALL_ENTRY(onLoad, 0),
  CALL_ENTRY(onUnload, 0),
  CALL_ENTRY(ordered_graph, 3),
  CALL_ENTRY(pdag_extension, 3),
  CALL_ENTRY(pdag2dag, 2),
  CALL_ENTRY(per_node_score, 6),
  CALL_ENTRY(rbn_master, 4),
  CALL_ENTRY(reset_test_counter, 0),
  CALL_ENTRY(root_nodes, 2),
  CALL_ENTRY(roundrobin_test, 9),
  CALL_ENTRY(score_cache_fill, 13),
  CALL_ENTRY(score_delta, 9),
  CALL_ENTRY(shd, 3),
  CALL_ENTRY(smart_network_averaging, 3),
  CALL_ENTRY(subsets, 2),
  CALL_ENTRY(tabu_hash, 4),
  CALL_ENTRY(tabu_step, 13),
  CALL_ENTRY(tiers, 2),
  CALL_ENTRY(topological_ordering, 4),
  CALL_ENTRY(tree_directions, 4),
  CALL_ENTRY(unique_arcs, 3),
  CALL_ENTRY(vstructures, 5),
  CALL_ENTRY(which_undirected, 2),
  {NULL, NULL, 0}
};

void R_init_bnlearn(DllInfo *dll) {

  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);

}/*R_INIT_BNLEARN*/

