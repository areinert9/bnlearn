
# check the data set.
check.data = function(x, allowed.types = available.data.types,
    allow.levels = FALSE, allow.missing = FALSE) {

  # check the data are there.
  if (missing(x))
    stop("the data are missing.")
  # x must be a data frame.
  if (!is.data.frame(x))
    stop("the data must be in a data frame.")
  # x must be a data frame with at least one column.
  if (ncol(x) == 0)
    stop("the data must be in a data frame with at least one column.")
  # check the data for NULL/NaN/NA.
  if (!allow.missing && missing.data(x))
    stop("the data set contains NULL/NaN/NA values.")
  # check which type of data we are dealing with.
  type = data.type(x)

  # check whether the variables are either all continuous or all discrete.
  check.label(type, choices = allowed.types, labels = data.type.labels,
    argname = "data type")

  # checks specific to a particular data type.
  if (type %in% discrete.data.types) {

    for (col in names(x)) {

      # check the number of levels of discrete variables, to guarantee that
      # the degrees of freedom of the tests are positive.
      if (nlevels(x[, col]) < 2)
        stop("variable ", col, " must have at least two levels.")

      # warn about levels with zero frequencies, it's not necessarily wrong
      # (data frame subsetting) but sure is fishy.
      if (!allow.levels && any(minimal.table(x[, col, drop = FALSE]) == 0))
        warning("variable ", col, " has levels that are not observed in the data.")

    }#FOR

  }#THEN
  else if (type == "continuous") {

    # all values must be finite to have finite mean and variance.
    check.data.frame.finite(x)

  }#THEN

  return(type)

}#CHECK.DATA

# check nodes (not necessarily from a bn object).
check.nodes = function(nodes, graph = NULL, min.nodes = 1, max.nodes = Inf) {

  # a node is needed.
  if (missing(nodes))
    stop("no node specified.")
  # nodes must be a vector of character strings.
  if (!is(nodes, "character"))
    stop("nodes must be a vector of character strings, the labels of the nodes.")
  # no duplicates allowed.
  if (any(duplicated(nodes)))
     stop("node labels must be unique.")
  # no empty strings.
  if (any(is.na(nodes)) || any(nodes == ""))
    stop("an empty string is not a valid node label.")
  # maximum number of nodes requirement.
  if (length(nodes) > max.nodes)
    stop("at most ", max.nodes, " node(s) needed.")
  # minimum number of nodes requirement (usually 1).
  if (length(nodes) < min.nodes)
    stop("at least ", min.nodes, " node(s) needed.")
  # node must be a valid node label.
  if (!is.null(graph)) {

    if (is(graph, "bn")) {

      if (any(nodes %!in% names(graph$nodes)))
        stop("node(s)", paste0(" '", nodes[nodes %!in% names(graph$nodes)], "'"),
             " not present in the graph.")

    }#THEN
    else if (is(graph, "bn.fit")) {

      if (any(nodes %!in% names(graph)))
        stop("node(s)", paste0(" '", nodes[nodes %!in% names(graph)], "'"),
             " not present in the graph.")

    }#THEN
    else if (is.character(graph)) {

      if (any(nodes %!in% graph))
        stop("node(s)", paste0(" '", nodes[nodes %!in% graph], "'"),
             " not present in the graph.")

    }#THEN

  }#THEN

}#CHECK.NODES

# check an arc set.
check.arcs = function(arcs, nodes) {

  # sanitize the set of arcs.
  if (is(arcs, "matrix") || is(arcs, "data.frame")) {

     if (dim(arcs)[2] != 2)
       stop("arc sets must have two columns.")
     if (!all(sapply(arcs, class) == "character"))
       stop("node labels in arc sets must be character strings.")

     if (is.data.frame(arcs))
       arcs = as.matrix(cbind(as.character(arcs[, 1]),
         as.character(arcs[, 2])))

     # be sure to set the column names.
     dimnames(arcs) = list(c(), c("from", "to"))

  }#THEN
  else if (is.character(arcs)) {

    # if there is an even number of labels fit them into a 2-column matrix.
    if ((length(arcs) %% 2) != 0)
      stop("arc sets must have two columns.")

    arcs = matrix(arcs, ncol = 2, byrow = TRUE,
              dimnames = list(c(), c("from", "to")))

  }#THEN
  else {

     stop("an arc set must be a matrix or data.frame with two columns.")

  }#ELSE

  # nodes must be valid node labels.
  if (any(arcs %!in% nodes))
    stop("node(s)", paste0(" '", unique(arcs[arcs %!in% nodes]), "'"),
         " not present in the graph.")

  # remove duplicate arcs.
  arcs = unique.arcs(arcs, nodes, warn = TRUE)

  # check there are no loops among the arcs.
  loop = (arcs[, "from"] == arcs[, "to"])

  if (any(loop))
    stop("invalid arcs that are actually loops:\n",
         paste("  ", arcs[loop, 1], "->", arcs[loop, 2], "\n"))

  return(arcs)

}#CHECK.ARCS

# build a valid whitelist.
build.whitelist = function(whitelist, nodes, data, algo, criterion) {

  if (is.null(whitelist)) {

    # no whitelist, nothing to do.
    return(NULL)

  }#THEN

  if (is(whitelist, c("matrix", "data.frame"))) {

    if (dim(whitelist)[2] != 2)
      stop("whitelist must have two columns.")

    if (is.data.frame(whitelist))
      whitelist = as.matrix(cbind(as.character(whitelist[, 1]),
        as.character(whitelist[, 2])))

  }#THEN
  else if (is.character(whitelist)) {

    if (length(whitelist) != 2)
      stop("whitelist must have two columns.")

    whitelist = matrix(whitelist, ncol = 2, byrow = TRUE)

  }#THEN
  else {

    stop("whitelist must be a matrix or data.frame with two columns.")

  }#ELSE

  # drop duplicate rows.
  whitelist = unique.arcs(whitelist, nodes, warn = TRUE)
  # add column names for easy reference.
  colnames(whitelist) = c("from", "to")

  # check all the names in the whitelist against the column names of x.
  if (any(unique(as.vector(whitelist)) %!in% nodes))
    stop("unknown node label present in the whitelist.")
  # check that whitelisted arcs do not violate parametric assumptions.
  whitelist = check.arcs.against.assumptions(whitelist, data, criterion)

  if (algo %in% score.based.algorithms) {

    # the whitelist should contain only directed arcs; extend the implied CPDAG
    # instead of picking arc directions at random to avoid loops.
    whitelist = cpdag.arc.extension(whitelist, nodes = nodes)

  }#THEN
  else if (algo %in% mim.based.algorithms) {

    # all arcs in the whitelist are treated as undirected, because these
    # algorithms operate in the space of undirected graphs.
    whitelist = unique.arcs(arcs.rbind(whitelist, whitelist,
                  reverse2 = TRUE), nodes)

  }#THEN

  # if the whitelist itself contains cycles, no acyclic graph
  # can be learned.
  if (!is.acyclic(whitelist, nodes = nodes,
         directed = (algo %in% c(constraint.based.algorithms, "aracne"))))
    stop("this whitelist does not allow an acyclic graph.")

  return(whitelist)

}#BUILD.WHITELIST

check.arcs.against.assumptions = function(arcs, data, criterion) {

  if (criterion %in% c(available.mixedcg.tests, available.mixedcg.scores)) {

    # arcs cannot point from continuous nodes to discrete nodes.
    if (is.null(arcs)) {

      arcs = list.cg.illegal.arcs(nodes = names(data), variables = data)

    }#THEN
    else {

      arcs = .Call(call_arcs_cg_assumptions,
                   arcs = arcs,
                   nodes = names(data),
                   data = data)

    }#ELSE

  }#THEN

  return(arcs)

}#CHECK.ARCS.AGAINST.ASSUMPTIONS

list.cg.illegal.arcs = function(nodes, variables) {

  .Call(call_cg_banned_arcs,
        nodes = nodes,
        variables = variables)

}#LIST.ILLEGAL.ARCS

# build a valid blacklist.
build.blacklist = function(blacklist, whitelist, nodes, algo) {

  if (!is.null(blacklist)) {

    if (is(blacklist, c("matrix", "data.frame"))) {

      if (dim(blacklist)[2] != 2)
        stop("blacklist must have two columns.")

      if (is.data.frame(blacklist))
        blacklist = as.matrix(cbind(as.character(blacklist[, 1]),
          as.character(blacklist[, 2])))

    }#THEN
    else if (is.character(blacklist)) {

      if (length(blacklist) != 2)
        stop("blacklist must have two columns.")

      blacklist = matrix(blacklist, ncol = 2, byrow = TRUE)

    }#THEN
    else {

      stop("blacklist must be a matrix or data.frame with two columns.")

    }#ELSE

    # check all the names in the blacklist against the column names of x.
    if (any(unique(as.vector(blacklist)) %!in% nodes))
      stop("unknown node label present in the blacklist.")

    if (algo %in% mim.based.algorithms) {

      # all arcs in the whitelist are treated as undirected, because these
      # algorithms operate in the space of undirected graphs.
      blacklist = arcs.rbind(blacklist, blacklist, reverse2 = TRUE)

    }#THEN

    # drop duplicate rows.
    blacklist = unique.arcs(blacklist, nodes)

  }#THEN

  # update blacklist to agree with whitelist.
  # NOTE: whitelist and blacklist relationship is the same as hosts.allow
  # and hosts.deny.
  if (!is.null(whitelist)) {

    # if x -> y is whitelisted but y -> x is not, it is to be blacklisted.
    to.add = apply(whitelist, 1, function(x)
               is.whitelisted(whitelist, x[c(2, 1)]))
    blacklist = arcs.rbind(blacklist, whitelist[!to.add, c(2, 1)])

    # if x -> y is whitelisted, it is to be removed from the blacklist.
    if (!is.null(blacklist)) {

      blacklist = blacklist[!apply(blacklist, 1,
        function(x){ is.whitelisted(whitelist, x) }),]

      # also drop duplicate rows.
      blacklist = unique.arcs(matrix(blacklist, ncol = 2, byrow = FALSE), nodes)

    }#THEN

  }#THEN

  # set the column names.
  if (!is.null(blacklist))
    colnames(blacklist) = c("from", "to")

  return(blacklist)

}#BUILD.BLACKLIST

# check the list of networks passed to custom.strength().
check.customlist = function(custom, nodes) {

  # check
  if (!is(custom, "list"))
    stop("networks must be a list of objects of class 'bn' or of arc sets.")
  if (!all(sapply(custom, function(x) { is(x, "bn") || is(x, "matrix") })))
    stop("x must be a list of objects of class 'bn' or of arc sets.")

  validate = function(custom, nodes) {

    if (is(custom, "bn")) {

      check.nodes(names(custom$nodes), graph = nodes, min.nodes = length(nodes),
        max.nodes = length(nodes))

    }
    else if (is(custom, "matrix")) {

      check.arcs(arcs = custom, nodes = nodes)

    }#THEN
    else {

      stop("x must be a list of objects of class 'bn' or of arc sets.")

    }#ELSE

  return(TRUE)

  }#VALIDATE

  if (!all(sapply(custom, validate, nodes = nodes)))
    stop("x must be a list of objects of class 'bn' or of arc sets.")

}#CHECK.CUSTOMLIST

# check test labels.
check.test = function(test, data) {

  # check which type of data we are dealing with.
  type = data.type(data)

  if (!missing(test) && !is.null(test)) {

    # check the test label.
    check.label(test, choices = available.tests, labels = test.labels,
      argname = "conditional independence test", see = "bnlearn-package")
    # check if it's the right test for the data (discrete, continuous).
    if ((type != "ordered") && (test %in% available.ordinal.tests))
      stop("test '", test, "' may be used with ordinal data only.")
    if ((type %!in% discrete.data.types) && (test %in% available.discrete.tests))
      stop("test '", test, "' may be used with discrete data only.")
    if ((type != "continuous") && (test %in% available.continuous.tests))
      stop("test '", test, "' may be used with continuous data only.")
    if ((type != "mixed-cg") && (test %in% available.mixedcg.tests))
      stop("test '", test, "' may be used with a mixture of continuous and discrete data only.")

    return(test)

  }#THEN
  else {

    if (type == "ordered")
      return("jt")
    else if (type %in% c("factor", "mixed-do"))
      return("mi")
    else if (type == "continuous")
      return("cor")
    else if (type == "mixed-cg")
      return("mi-cg")

  }#ELSE

}#CHECK.TEST

check.criterion = function(criterion, data) {

  if (!missing(criterion) && !is.null(criterion)) {

    # check and return errors from minimal.check.labels().
    check.label(criterion, choices = c(available.tests, available.scores),
      labels = c(test.labels, score.labels), argname = "criterion",
      see = "bnlearn-package")

  }#THEN
  else {

    # set the defaults using check.score() and check.test().
    if (criterion %in% available.tests)
      criterion = check.test(criterion, data)
    else if (criterion %in% available.scores)
      criterion = check.score(criterion, data)

  }#ELSE

  return(criterion)

}#CHECK.CRITERION

# check loss functions' labels.
check.loss = function(loss, data, bn) {

  # check which type of data we are dealing with.
  type = data.type(data)

  if (!is.null(loss)) {

    # check the loss function.
    check.label(loss, choices = loss.functions, labels = loss.labels,
      argname = "loss function", see = "bn.cv")

    if ((type %!in% discrete.data.types) && (loss %in% discrete.loss.functions))
      stop("loss function '", loss, "' may be used with discrete data only.")
    if ((type != "continuous") && (loss %in% continuous.loss.functions))
      stop("loss function '", loss, "' may be used with continuous data only.")
    if ((type != "mixed-cg") && (loss %in% mixedcg.loss.functions))
      stop("loss function '", loss, "' may be used with a mixture of continuous and discrete data only.")

    return(loss)

  }#THEN
  else {

    if ((is.character(bn) && (bn %in% classifiers)) ||
         is(bn, c("bn.naive", "bn.tan")))
      return("pred")
    if (type %in% discrete.data.types)
      return("logl")
    else if (type == "continuous")
      return("logl-g")
    else if (type == "mixed-cg")
      return("logl-cg")

  }#ELSE

}#CHECK.LOSS

# check the method used to fit the parameters of the network.
check.fitting.method = function(method, data) {

  # check which type of data we are dealing with.
  type = data.type(data)

  if (!is.null(method)) {

    # check the fitting method.
    check.label(method, choices = available.fitting.methods,
      labels = fitting.labels, argname = "fitting method", see = "bn.fit")
    # Bayesian parameter estimation is implemented only for discrete data.
    if ((type %in% c("continuous", "mixed-cg")) && (method == "bayes"))
      stop("Bayesian parameter estimation for (conditional) Gaussian Bayesian networks is not implemented.")

    return(method)

  }#THEN
  else {

    return("mle")

  }#ELSE

}#CHECK.FITTING.METHOD

# check the method used for prediction.
check.prediction.method = function(method, data) {

  if (!missing(method) && !is.null(method)) {

    check.label(method, choices = available.prediction.methods,
      labels = prediction.labels, argname = "prediction method", see = "predict")

    return(method)

  }#THEN
  else {

    return("parents")

  }#ELSE

}#CHECK.PREDICTION.METHOD

# check the method used to discretize the data.
check.discretization.method = function(method) {

  if (!missing(method) && !is.null(method)) {

    check.label(method, choices = available.discretization.methods,
      labels = discretization.labels, argname = "discretization method",
      see = "discretize")

    return(method)

  }#THEN
  else {

    return("quantile")

  }#ELSE

}#CHECK.DISCRETIZATION.METHOD

# check the estimator for the mutual information.
check.mi.estimator = function(estimator, data) {

  # check which type of data we are dealing with.
  type = data.type(data)

  if (!missing(estimator) && !is.null(estimator)) {

    check.label(estimator, choices = available.mi,
      labels = mi.estimator.labels, argname = "mutual information estimator")

    # check if it's the right estimator for the data (discrete, continuous).
    if ((type %!in% discrete.data.types) &&
        (estimator %in% available.discrete.mi))
      stop("estimator '", estimator, "' may be used with discrete data only.")
    if ((type != "continuous") && (estimator %in% available.continuous.mi))
      stop("estimator '", estimator, "' may be used with continuous data only.")

    return(estimator)

  }#THEN
  else {

    if (type %in% discrete.data.types)
      return("mi")
    else
      return("mi-g")

  }#ELSE

}#CHECK.MI.ESTIMATOR

# check the method used for cross-validation.
check.cv.method = function(method) {

  if (!missing(method) && !is.null(method)) {

    check.label(method, choices = available.cv.methods,
      labels = cv.labels, argname = "cross-validation method", see = "bn.cv")

    return(method)

  }#THEN
  else {

    return("k-fold")

  }#ELSE

}#CHECK.CV.METHOD

# is the data of a particular type?
data.type = function(data) {

  .Call(call_data_type,
        data = data)

}#DATA.TYPE

# there are missing data?
missing.data = function(data) {

  !all(complete.cases(data))

}#MISSING.DATA

# sanitize the extra arguments passed to the random graph generation algorithms.
check.graph.generation.args = function(method, nodes, extra.args) {

  if (method == "ordered") {

    if (!is.null(extra.args$prob)) {

      # prob must be numeric.
      if (!is.probability(extra.args$prob))
        stop("the branching probability must be a numeric value in [0,1].")

    }#THEN
    else {

      # this default produces graphs with about the same number of
      # arcs as there are nodes.
      extra.args$prob = 2 / (length(nodes) - 1)

    }#ELSE

  }#THEN
  else if (method %in% c("ic-dag", "melancon")) {

    if (!is.null(extra.args$every)) {

      if (!is.positive(extra.args$every))
        stop("'every' must be a positive integer number.")

    }#THEN
    else {

      extra.args$every = 1

    }#ELSE

    if (!is.null(extra.args$burn.in)) {

      if (!is.positive(extra.args$burn.in))
        stop("the burn in length must be a positive integer number.")

    }#THEN
    else {

      extra.args$burn.in = 6 * length(nodes)^2

    }#ELSE

    if (!is.null(extra.args$max.in.degree)) {

      if (!is.positive.integer(extra.args$max.in.degree))
        stop("the maximum in-degree must be a positive integer number.")

      if (extra.args$max.in.degree >= length(nodes)) {

        warning("a node cannot have an in-degree greater or equal to the number of nodes in the graph.")
        warning("the condition on the in-degree will be ignored.")

      }#THEN

    }#THEN
    else {

      extra.args$max.in.degree = Inf

    }#ELSE

    if (!is.null(extra.args$max.out.degree)) {

      if (!is.positive.integer(extra.args$max.out.degree))
        stop("the maximum out-degree must be a positive integer number.")

      if (extra.args$max.out.degree >= length(nodes)) {

        warning("a node cannot have an out-degree greater or equal to the number of nodes in the graph.")
        warning("the condition on the out-degree will be ignored.")

      }#THEN

    }#THEN
    else {

      extra.args$max.out.degree = Inf

    }#ELSE

    if (!is.null(extra.args$max.degree)) {

      if (!is.positive.integer(extra.args$max.degree))
        stop("the maximum out-degree must be a positive integer number.")

      if (is.finite(extra.args$max.in.degree) &&
          extra.args$max.in.degree > extra.args$max.degree)
        stop("the maximun in-degree must be lesser or equal to the maximum degree.")

      if (is.finite(extra.args$max.out.degree) &&
          extra.args$max.out.degree > extra.args$max.degree)
        stop("the maximun out-degree must be lesser or equal to the maximum degree.")

      if (extra.args$max.degree >= length(nodes)) {

        warning("a node cannot have a degree greater or equal to the number of nodes in the graph.")
        warning("the condition on the degree will be ignored.")

      }#THEN

    }#THEN
    else {

      extra.args$max.degree = Inf

    }#ELSE

  }#THEN

  check.unused.args(extra.args, graph.generation.extra.args[[method]])

  return(extra.args)

}#CHECK.GRAPH.GENERATION.ARGS

# check bootstrap arguments (when they are passed as variable length args).
check.bootstrap.args = function(extra.args, network, data) {

  # check the number of bootstrap replicates.
  extra.args$R = check.replicates(extra.args$R)
  # check the size of each bootstrap sample.
  extra.args$m = check.bootsize(extra.args$m, data)
  # check the learning algorithm.
  algorithm = check.learning.algorithm(extra.args[["algorithm"]], bn = network)
  # check the extra arguments for the learning algorithm.
  algorithm.args = check.learning.algorithm.args(extra.args[["algorithm.args"]],
                     algorithm = algorithm, bn = network)

  extra.args[["algorithm"]] = algorithm
  extra.args[["algorithm.args"]] = algorithm.args

  # remap additional arguments used in hybrid algorithms.
  if (algorithm %in% hybrid.algorithms) {

    # there's no need to sanitize these parameters, it's done either in
    # bnlearn() or in greedy.search() already.
    if (is.null(extra.args[["algorithm.args"]]$restrict))
      extra.args[["algorithm.args"]]$restrict = network$learning$restrict
    if (is.null(extra.args[["algorithm.args"]]$maximize))
      extra.args[["algorithm.args"]]$maximize = network$learning$maximize
    if (is.null(extra.args[["algorithm.args"]]$test))
      extra.args[["algorithm.args"]]$test = network$learning$rstest
    if (is.null(extra.args[["algorithm.args"]]$score))
      extra.args[["algorithm.args"]]$score = network$learning$maxscore

  }#THEN

  # warn about unused arguments.
  check.unused.args(extra.args, c("R", "m", "algorithm", "algorithm.args"))

  return(extra.args)

}#CHECK.BOOTSTRAP.ARGS

# sanitize the extra arguments passed to cross-validation methods.
check.cv.args = function(method, extra.args, data) {

  n = nrow(data)

  if (method %in% c("k-fold", "hold-out")) {

    # check the number of splits.
    if (!is.null(extra.args$k)) {

      if (!is.positive.integer(extra.args$k))
        stop("the number of splits must be a positive integer number.")
      if (extra.args$k == 1)
        stop("the number of splits must be at least 2.")
      if (n < extra.args$k)
        stop("insufficient sample size for ", extra.args$k, " subsets.")

    }#THEN
    else {

      extra.args$k = 10

    }#ELSE

    # check the number of runs.
    if (!is.null(extra.args$runs)) {

      if (!is.positive.integer(extra.args$runs))
        stop("the number of runs must be a positive integer number.")

    }#TTHEN
    else {

      extra.args$runs = 1

    }#ELSE

  }#THEN

  if (method == "hold-out") {

    # check the size of the test subsets in hold-put cross-validation.
    if (!is.null(extra.args$m)) {

      if (!is.positive.integer(extra.args$m))
        stop("the size of the test subset must be a positive integer number.")
      if (extra.args$m >= n)
        stop("insufficient sample size for a test subset of size ",
          extra.args$m, ".")

    }#THEN
    else {

      extra.args$m = ceiling(n / 10)

    }#ELSE

  }#THEN

  if (method == "custom-folds") {

    if (!is.null(extra.args$folds)) {

      if (!is.list(extra.args$folds))
        stop("folds must be specified via a list of indices.")
      if (length(extra.args$folds) < 2)
        stop("at least two folds are needed.")
      if (any(sapply(extra.args$folds, length) == 0))
        stop("some folds contain no observations.")
      if (any(!sapply(extra.args$folds, is.positive.vector)))
        stop("observation indices must be positive integer numbers.")

      merged = unlist(extra.args$folds)

      if (any(duplicated(merged)))
        stop("some observations are included in more than one fold.")
      if (any(merged > n))
        stop("observation indices are too high (sample size is ", n, ").")
      if (length(merged) != n)
        stop("not all observations are assigned to a fold.")

    }#THEN
    else {

      stop("custom folds are missing, with no default.")

    }#ELSE

  }#THEN

  # warn about unused arguments.
  check.unused.args(extra.args, cv.extra.args[[method]])

  return(extra.args)

}#CHECK.CV.ARGS

# sanitize the extra arguments passed to the conditional probability algorithms.
check.cpq.args = function(fitted, event, extra.args, method, action) {

  if (method %in% c("ls", "lw")) {

    if (!is.null(extra.args$n)) {

      if (!is.positive.integer(extra.args$n))
        stop("the number of observations to be sampled must be a positive integer number.")

    }#THEN
    else {

      # this is a rule of thumb, the error of the estimate has no closed-form
      # expression (Koller & Friedman).
      if (!is(fitted, "bn.fit.gnet"))
        extra.args$n = 5000 * max(1, round(log10(nparams.fitted(fitted))))
      else
        extra.args$n = 500 * nparams.fitted(fitted)

    }#ELSE

    if (!is.null(extra.args$batch)) {

      if ((action == "cpdist") && (method == "lw")) {

        extra.args$batch = NULL
        warning(" 'batch' will be ignored for speed and memory efficience.")

      }#THEN
      else {

        if (!is.positive.integer(extra.args$batch))
          stop("the number of observations to be sampled must be a positive integer number.")

        if (extra.args$batch > extra.args$n) {

          warning("cannot generate a batch bigger than the whole generated data set.")
          warning("batch size will be ignored.")

        }#THEN

      }#ELSE

    }#THEN
    else {

      # perform small simulations in a single batch, and split larger ones.
      extra.args$batch = min(extra.args$n, 10^4)

    }#ELSE

    if (!is.null(extra.args$query.nodes)) {

      check.nodes(extra.args$query.nodes, graph = fitted)

      # make sure the nodes to be simulated are included.
      if (action == "cpdist")
        extra.args$query.nodes = c(event, extra.args$query.nodes)

    }#THEN

  }#THEN

  # warn about unused arguments.
  check.unused.args(extra.args, cpq.extra.args[[method]])

  return(extra.args)

}#CHECK.CPQ.ARGS

# sanitize the extra arguments passed to loss functions.
check.loss.args = function(loss, bn, nodes, data, extra.args) {

  valid.args = loss.extra.args[[loss]]

  if (loss %in% c("pred", "pred-lw", "pred-lw-cg", "cor", "cor-lw", "cor-lw-cg",
                  "mse", "mse-lw", "mse-lw-cg")) {

    if (!is.null(extra.args$target)) {

      if (!is.string(extra.args$target) || (extra.args$target %!in% nodes))
        stop("target node must be a single, valid node label for the network.")

      # in hybrid networks, check the target has the right data type.
      if (loss %in% c("cor-lw-cg", "mse-lw-cg"))
        if (!is(data[, extra.args$target], "numeric"))
          stop("the target node must be a continuous variable.")
      if (loss == "pred-lw-cg")
        if (!is(data[, extra.args$target], "factor"))
          stop("the target node must be a factor.")

    }#THEN
    else {

      # the target node is obvious for classifiers.
      if (is(bn, c("bn.naive", "bn.tan"))) {

        if (is(bn, "bn"))
          extra.args$target = bn$learning$args$training
        else
          extra.args$target = attr(bn, "training")

      }#THEN
      else {

        stop("missing target node for which to compute the prediction error.")

      }#ELSE

    }#ELSE

    # check the prior distribution.
    if ((bn %in% classifiers) || is(bn, c("bn.naive", "bn.tan"))) {

      extra.args$prior = check.classifier.prior(extra.args$prior, data[, extra.args$target])
      valid.args = c(valid.args, "prior")

    }#THEN

  }#THEN

  if (loss %in% c("pred-lw", "pred-lw-cg", "cor-lw", "cor-lw-cg", "mse-lw",
                  "mse-lw-cg")) {

    # number of particles for likelihood weighting.
    if (!is.null(extra.args$n)) {

      if (!is.positive.integer(extra.args$n))
        stop("the number of observations to be sampled must be a positive integer number.")

    }#THEN
    else {

      extra.args$n = 500

    }#ELSE

    # which nodes to predict from.
    if (!is.null(extra.args$from))
      check.nodes(extra.args$from, graph = names(data), min.nodes = 1)
    else
      extra.args$from = setdiff(names(data), extra.args$target)

  }#THEN

  # warn about unused arguments.
  check.unused.args(extra.args, valid.args)

  return(extra.args)

}#CHECK.LOSS.ARGS

# sanitize the extra arguments passed to fitting functions.
check.fitting.args = function(method, network, data, extra.args) {

  if (method == "bayes") {

    # check the imaginary sample size.
    extra.args$iss = check.iss(iss = extra.args$iss,
      network = network, data = data)

  }#THEN

  # warn about unused arguments.
  check.unused.args(extra.args, fitting.extra.args[[method]])

  return(extra.args)

}#CHECK.FITTING.ARGS

# sanitize the extra arguments passed to discretization methods.
check.discretization.args = function(method, data, breaks, extra.args) {

  # check which type of data we are dealing with.
  type = data.type(data)

  if (method == "hartemink") {

    if (type %in% discrete.data.types) {

      extra.args$ibreaks = nlevels(data[, 1])
      warning("data are already discrete, 'ibreaks' and 'idisc' are ignored.")

    }#THEN
    else {

      if (!is.null(extra.args$idisc)) {

        other.methods = available.discretization.methods[available.discretization.methods != "hartemink"]

        check.label(extra.args$idisc, choices = other.methods,
          labels = discretization.labels, argname = "initial discretization method",
          see = "discretize")

      }#THEN
      else {

        # default to quantile discretization as per Hartemink's recommendation.
        extra.args$idisc = "quantile"

      }#ELSE

      if (!is.null(extra.args$ibreaks)) {

        if (!is.positive.integer(extra.args$ibreaks))
          stop("the number of initial breaks must be a positive integer number.")
        if (extra.args$ibreaks < breaks)
          stop("insufficient number of levels, at least ", breaks, " required.")

      }#THEN
      else {

        ndata = nrow(data)

        if (ndata > 500)
          extra.args$ibreaks = 100
        else if (ndata > 100)
          extra.args$ibreaks = 50
        else if (ndata > 50)
          extra.args$ibreaks = 20
        else if (ndata > 10)
          extra.args$ibreaks = 10
        else
          extra.args$ibreaks = ndata

      }#ELSE

    }#ELSE

  }#THEN

  # warn about unused arguments.
  check.unused.args(extra.args, discretization.extra.args[[method]])

  return(extra.args)

}#CHECK.DISCRETIZATION.ARGS

# sanitize the extra arguments passed to Bayesian classifiers.
check.classifier.args = function(method, data, training, explanatory,
    extra.args) {

  if (method == "tree.bayes") {

    # check the label of the mutual information estimator.
    extra.args$estimator = check.mi.estimator(extra.args$estimator, data)

    # check the node to use the root of the tree (if not specified pick the first
    # explanatory variable assuming natural ordering).
    if (!is.null(extra.args$root))
      check.nodes(extra.args$root, graph = explanatory, max.nodes = 1)
    else
      extra.args$root = explanatory[1]

  }#THEN

  return(extra.args)

}#CHECK.CLASSIFIER.ARGS

# warn about unused arguments.
check.unused.args = function(dots, used.args) {

  if (is(dots, "list"))
    unused.args = (names(dots) %!in% used.args)
  else
    unused.args = setdiff(dots, used.args)
  if (any(unused.args))
    warning("unused argument(s):", paste0(" '", names(dots)[unused.args], "'"), ".")

}#CHECK.UNUSED.ARGS

# take care of meaningless dots arguments in plot functions.
sanitize.plot.dots = function(dots, meaningless) {

  # warn about them.
  if (any(names(dots) %in% meaningless))
    warning("arguments ", paste(meaningless, collapse = ", "),
      " will be silently ignored.")
  # nuke them from orbit.
  for (m in meaningless)
    dots[[m]] = NULL

  return(dots)

}#PROCESS.PLOT.DOTS

# check the the target nominal type I error rate
check.alpha = function(alpha, network = NULL) {

  # check the the target nominal type I error rate
  if (!missing(alpha) && !is.null(alpha)) {

    # validate alpha.
    if (!is.probability(alpha))
      stop("alpha must be a numerical value in [0,1].")

  }#THEN
  else {

    # check if there is an alpha value stored in the bn object;
    # otherwise use the usual 0.05 value.
    if (!is.null(network$learning$args$alpha))
      alpha = network$learning$args$alpha
    else
      alpha = 0.05

  }#ELSE

  return(alpha)

}#CHECK.ALPHA

# check the number of permutation/boostrap samples.
check.B = function(B, criterion) {

  if (criterion %in% resampling.tests) {

    if (!missing(B) && !is.null(B)) {

      if (!is.positive.integer(B))
        stop("the number of permutations/bootstrap replications must be a positive integer number.")

      B = as.integer(B)

    }#THEN
    else {

      if (criterion %in% semiparametric.tests)
        B = 100L
      else
        B = 5000L

    }#ELSE

  }#THEN
  else {

    if (!missing(B) && !is.null(B))
      warning("this test does not require any permutations/bootstrap resampling, ignoring B.\n")

    B = NULL

  }#ELSE

  return(B)

}#CHECK.B

check.amat = function(amat, nodes) {

  # a node is needed.
  if (missing(amat))
    stop("no adjacency matrix specified.")
  # the adjacency matrix must, well, be a matrix.
  if (!is(amat, "matrix") || (ncol(amat) != nrow(amat)) || (length(dim(amat)) != 2))
    stop("an adjacency matrix must be a 2-dimensional square matrix.")
  # check the dimensions against the number of nodes in the graph.
  if (any(dim(amat) != length(nodes)))
    stop("the dimensions of the adjacency matrix do not agree with the number of nodes in the graph.")
  # column names must be valid node labels.
  if (!is.null(colnames(amat)))
    if (any(colnames(amat) %!in% nodes))
      stop("node (column label) not present in the graph.")
  # column names must be valid node labels.
  if (!is.null(rownames(amat)))
    if (any(rownames(amat) %!in% nodes))
      stop("node (row label) not present in the graph.")
  # column names must match with row names.
  if (!is.null(colnames(amat)) && !is.null(rownames(amat))) {

    if (!identical(colnames(amat), rownames(amat)))
      stop("row/column names mismatch in the adjacency matrix.")

    if (!identical(colnames(amat), nodes) || !identical(rownames(amat), nodes)) {

      warning("rearranging the rows/columns of the adjacency matrix.")

      amat = amat[nodes, nodes, drop = FALSE]

    }#THEN

  }#THEN
  # make really sure the adjacency matrix is made up of integers.
  if (storage.mode(amat) != "integer")
    storage.mode(amat) = "integer"
  # check the elements of the matrix.
  if (!all((amat == 0L) | (amat == 1L)))
    stop("all the elements of an adjacency matrix must be equal to either 0 or 1.")
  # no arcs from a node to itself.
  if (any(diag(amat) != 0))
    stop("the elements on the diagonal must be zero.")

  return(amat)

}#CHECK.AMAT

check.covariance = function(m) {

  # the adjacency matrix must, well, be a matrix.
  if (!is(m, "matrix") || (ncol(m) != nrow(m)) || (length(dim(m)) != 2))
    stop("a covariance matrix must be a 2-dimensional square matrix.")
  # check the elements of the matrix.
  if (!is.numeric(m))
    stop("the elements of a covariance matrix must be real numbres.")
  # check whether the matrix is symmetric.
  if (!is.symmetric(m))
    stop("a covariance matrix must be symmetric.")
  # check whether the matrix obeys the Cauchy-Schwarz theorem.
  if (!is.cauchy.schwarz(m))
    stop("a covariance matrix must obey the Cauchy-Schwarz theorem.")

}#CHECK.COVARIANCE

# check an object of class bn.
check.bn = function(bn) {

  if (missing(bn))
    stop("an object of class 'bn' is required.")
  if (!is(bn, "bn")) {

    stop(sprintf("%s must be an object of class 'bn'.",
           deparse(substitute(bn))))

  }#THEN

}#CHECK.BN

# check two bn's against each other.
match.bn = function(bn1, bn2) {

  # the two networks must have the same node set.
  nodes1 = names(bn1$nodes)
  nodes2 = names(bn2$nodes)

  equal = setequal(nodes1, nodes2) && (length(nodes1) == length(nodes2))

  if (!equal)
    stop("the two networks have different node sets.")

}#MATCH.BN

# check an object of class bn or bn.fit.
check.bn.or.fit = function(bn) {

  if (missing(bn))
    stop("an object of class 'bn' or 'bn.fit' is required.")
  if (!is(bn, "bn") && !is(bn, "bn.fit")) {

    stop(sprintf("%s must be an object of class 'bn' or 'bn.fit'.",
           deparse(substitute(bn))))

  }#THEN

}#CHECK.BN.OR.FIT

# check an object of class bn.fit.
check.fit = function(bn) {

  if (missing(bn))
    stop("an object of class 'bn.fit' is required.")
  if (!is(bn, "bn.fit")) {

    stop(sprintf("%s must be an object of class 'bn.fit'.",
           deparse(substitute(bn))))

  }#THEN

}#CHECK.FIT

# check the structure of a naive Bayes classifier.
check.bn.naive = function(bn) {

  # check whether it's a valid bn/bn.fit object.
  check.bn.or.fit(bn)
  # there must be a single root node, check.
  root = root.leaf.nodes(bn, leaf = FALSE)

  if (length(root) != 1)
    stop("a naive Bayes classifier can have only one root node, the training variable.")

  # cache the node labels.
  if (is(bn, "bn"))
    nodes = names(bn$nodes)
  else
    nodes = names(bn)
  # get the explanatory variables.
  explanatory = nodes[nodes != root]
  leafs = root.leaf.nodes(bn, leaf = TRUE)
  # all the explanatory variables must be leaf nodes, check.
  if (!identical(sort(explanatory), sort(leafs)))
    stop("all the explanatory variables must be leaf nodes.")
  # all the explanatory variables must have a single parent, the root node, check.
  nparents = sapply(explanatory, function(node) { length(parents(bn, node))  })

  if (any(nparents != 1))
    stop("all the explanatory variables must be children of the training variable.")

}#CHECK.BN.NAIVE

# check the structure of a naive Bayes classifier.
check.bn.tan = function(bn) {

  # check whether it's a valid bn/bn.fit object.
  check.bn.or.fit(bn)
  # there must be a single root node, check.
  root = root.leaf.nodes(bn, leaf = FALSE)

  if (length(root) != 1)
    stop("a naive Bayes classifier can have only one root node, the training variable.")

  # that root node must be the training variable, check.
  if (is(bn, "bn")) {

    # double check just in case.
    check.nodes(bn$learning$args$training)

    nodes = names(bn$nodes)
    training = bn$learning$args$training

  }#THEN
  else {

    # double check just in case.
    check.nodes(attr(bn, "training"))

    nodes = names(bn)
    training = attr(bn, "training")

  }#ELSE

  if (!identical(training, root))
    stop("the training node is not the only root node in the graph.")

  # get the explanatory variables.
  explanatory = nodes[nodes != root]
  # all the explanatory variables save one must have exactly two parents, check.
  nparents = sapply(explanatory, function(node) { length(parents(bn, node))  })

  if (!( (length(which(nparents == 2)) == length(explanatory) - 1) && (length(which(nparents == 1)) == 1) ))
    stop("the explanatory variables must form a tree.")

}#CHECK.BN.TAN

# check an object of class bn.strength.
check.bn.strength = function(strength, nodes) {

  if (missing(strength))
    stop("an object of class 'bn.strength' is required.")
  if (!is(strength, "bn.strength")) {

    stop(sprintf("%s must be an object of class 'bn.strength'.",
           deparse(substitute(strength))))

  }#THEN
  if (ncol(strength) %!in% 3:4)
    stop("objects of class 'bn.strength' must have 3 or 4 columns.")
  if (!identical(names(strength), c("from", "to", "strength")) &&
      !identical(names(strength), c("from", "to", "strength", "direction")))
    stop("objects of class 'bn.strength' must be data frames with column names ",
         "'from', 'to', 'strength' and (optionally) 'direction'.")
  if (any(c("method", "threshold") %!in% names(attributes(strength))))
    stop("objects of class 'bn.strength' must have a 'method' and a 'strength' attributes.")
  if (!missing(nodes))
    check.arcs(strength[, c("from", "to"), drop = FALSE], nodes)

}#CHECK.BN.STRENGTH

# sanitize the threshold value.
check.threshold = function(threshold, strength) {

  if (missing(threshold))
    threshold = attr(strength, "threshold")
  else {

    s = strength[, "strength"]

    if (!is.numeric(threshold) || (length(threshold) != 1) || is.nan(threshold))
      stop("the threshold must be a numeric value.")
    if ((threshold < min(s)) || (threshold > max(s)))
      warning("the threshold is outside the range of the strength values.")

  }#ELSE

  return(threshold)

}#CHECK.THRESHOLD

# check parameters related to the random restart functions.
check.restart = function(restart) {

  # set the default value if not specified.
  if (is.null(restart) || (restart == 0))
      return(0)

  if (!is.positive.integer(restart))
    stop("the number of random restarts must be a non-negative numeric value.")
  else
    return(restart)

}#CHECK.RESTART

check.perturb = function(perturb) {

  # set the default value if not specified.
  if (is.null(perturb))
      return(1)

  if (!is.positive.integer(perturb))
    stop("the number of changes at each radom restart must be a non-negative numeric value.")
  else
    return(perturb)

}#CHECK.PERTURB

# check the maximum number of iterations.
check.max.iter = function(max.iter) {

  # set the default value if not specified.
  if (is.null(max.iter))
    return(Inf)

  if ((max.iter != Inf) && !is.positive.integer(max.iter))
    stop("the maximum number of iterations must be a positive integer number.")
  else
    return(max.iter)

}#CHECK.MAX.ITER

# check arguments related to the tabu list.
check.tabu = function(tabu) {

  # set the default value if not specified.
  if (is.null(tabu))
    return(10)

  if (!is.positive.integer(tabu))
    stop("the length of the tabu list must be a positive integer number.")
  else
    return(tabu)

}#CHECK.TABU

check.max.tabu = function(max, tabu) {

  if (is.null(max))
    return(tabu)

  # check the number of iterations the algorithm can perform without
  # improving the best network score.
  if (!is.positive.integer(max))
    stop("the maximum number of iterations without any score improvement must be a positive integer number.")
  # the tabu list should be longer than that, otherwise the search can do a
  # U-turn and return to the local maximum it left before (thus creating an
  # endless loop).
  if (max > tabu)
    stop("the maximum number of iterations without any score improvement must not be grater than the length of the tabu list.")

  return(max)

}#CHECK.MAX.TABU

# check bn metadata against the data it's used with.
check.bn.vs.data = function(bn, data) {

  # check which type of data we are dealing with.
  type = data.type(data)

  # the number of variables must be the same
  if (length(names(bn$nodes)) != ncol(data))
    stop("the network and the data have different numbers of variables.")
  # the variables must be the same.
  if (length(setdiff(names(bn$nodes), names(data))) != 0)
    stop("the variables in the data and in the network do not match.")
  # data type versus network structure.
  if (type == "mixed-cg")
    check.arcs.against.assumptions(bn$arcs, data, "mi-cg")

}#CHECK.BN.VS.DATA

# check bn.fit metadata against the data it's used with.
check.fit.vs.data = function(fitted, data, subset) {

  fitted.names = names(fitted)
  # check which type of data we are dealing with.
  dtype = data.type(data)

  if (missing(subset)) {

    # the number of variables must be the same.
    if (length(fitted.names) != ncol(data))
      stop("the network and the data have different numbers of variables.")
    # the variables must be the same.
    if (length(setdiff(fitted.names , names(data))) != 0)
      stop("the variables in the data and in the network do not match.")

    subset = fitted.names

  }#THEN
  else {

    # the number of variables must not exceed that of the network.
    if (length(subset) > length(fitted.names))
      stop("the data have more variables than the network.")
    # all the variables in the subset must be present in the data.
    absent = (subset %!in% names(data))
    if (any(absent))
      stop("required variables '", paste(subset[absent], collapse = " "),
           "' are not present in the data.")

  }#ELSE

  .Call(call_fitted_vs_data,
        fitted = fitted,
        data = data,
        subset = subset)

}#CHECK.FIT.VS.DATA

# check bn.fit.{d,g}node metadata against the data it's used with.
check.fit.node.vs.data = function(fitted, data) {

  relevant = c(fitted$node, fitted$parents)
  # check which type of data we are dealing with.
  type = data.type(data)

  # check whether all relevant nodes are in the data.
  if (any(relevant %!in% names(data)))
    stop("not all required nodes are present in the data.")
  # data type versus network type.
  if (is(fitted, "bn.fit.dnode") && (type == "continuous"))
      stop("continuous data and discrete network.")
  if (is(fitted, "bn.fit.gnode") &&
      (type %in% discrete.data.types))
    stop("discrete data and continuous network.")
  # double-check the levels of the variables against those of the nodes.
  if (is(fitted, "bn.fit.dnode")) {

    for (node in relevant) {

      data.levels = levels(data[, node])
      if (length(relevant) == 1)
        node.levels = dimnames(fitted$prob)[[1]]
      else
        node.levels = dimnames(fitted$prob)[[node]]

      if (!identical(data.levels, node.levels))
        stop("the levels of node '", node, "' do not match the levels of the ",
             "corresponding variable in the data.")

    }#FOR

  }#THEN

}#CHECK.FIT.NODE.VS.DATA

# check the label of a learning algorithm.
check.learning.algorithm = function(algorithm, class = "all", bn) {

  # select the right class of algorithms.
  ok = character(0)

  if ("constraint" %in% class)
    ok = c(ok, constraint.based.algorithms)
  if ("markov.blanket" %in% class)
    ok = c(ok, markov.blanket.algorithms)
  if ("neighbours" %in% class)
    ok = c(ok, local.search.algorithms)
  if ("score" %in% class)
    ok = c(ok, score.based.algorithms)
  if ("mim" %in% class)
    ok = c(ok, mim.based.algorithms)
  if ("classifier" %in% class)
    ok = c(ok, classifiers)
  if ("all" %in% class)
    ok = available.learning.algorithms

  if (missing(algorithm) || is.null(algorithm)) {

    # use the one specified by the bn object as the default.
    if (missing(bn))
      stop("the learning algorithm must be a character string.")
    else if (is(bn, "bn"))
      algorithm = bn$learning$algo

  }#THEN
  else {

    check.label(algorithm, choices = ok, labels = method.labels,
      argname = "learning algorithm", see = "bnlearn-package")

  }#ELSE

  return(algorithm)

}#CHECK.LEARNING.ALGORITHM

# check the arguments of a learning algorithm (for use in bootstrap).
check.learning.algorithm.args = function(args, algorithm, bn) {

  # convert args into a list, if it's not one already.
  if (!is.list(args))
      args = as.list(args)

  # if a reference bn is specified, guess as many parameters as possbile.
  if (!(missing(algorithm) || missing(bn))) {

    # use the same score/conditional independence test.
    if (algorithm %in% constraint.based.algorithms) {

      # it's essential to check it's actually an independence test,
      # it could be a score function or NA.
      if ("test" %!in% names(args))
        if (bn$learning$test %in% available.tests)
          args$test = bn$learning$test

      # set the appropriate value for the optimization flag.
      if ("optimized" %!in% names(args))
        args$optimized = bn$learning$optimized

      # pass along all the parameters in bn$learning$args.
      if (length(bn$learning$args) > 0) {

        if ("alpha" %!in% names(args))
          args$alpha = bn$learning$args$alpha

        if ("test" %in% names(args) && ("B" %!in% names(args)))
          if (args$test %in% resampling.tests)
            args$B = bn$learning$args$B

      }#THEN

    }#THEN
    else if (algorithm %in% score.based.algorithms) {

      if ("score" %!in% names(args))
        if (bn$learning$test %in% available.scores)
          args$score = bn$learning$test

      # set the appropriate value for the optimization flag.
      if ("optimized" %!in% names(args))
        args$optimized = bn$learning$optimized

      # pass along the relevant parameters in bn$learning$args if the score
      # function is the same (hint: different scores have paramenters with
      # the same name but different meanings).
      if (("score" %in% names(args)) && (args$score == bn$learning$test))
        for (arg in names(bn$learning$args))
          if ((arg %!in% names(args)) && (arg %in% (score.extra.args[[args$score]])))
            args[[arg]] = bn$learning$args[[arg]]

    }#THEN

    # pass along whitelist and blacklist.
    if (!is.null(bn$learning$whitelist))
      args$whitelist = bn$learning$whitelist
    if (!is.null(bn$learning$blacklist))
      args$blacklist = bn$learning$blacklist

  }#THEN

  # remove any spurious x arguments, the data are provided by the bootstrap.
  if ("x" %in% names(args)) {

    args$x = NULL

    warning("removing 'x' from 'algorithm.args', the data set is provided by the bootstrap sampling.")

  }#THEN

  return(args)

}#CHECK.LEARNING.ALGORITHM.ARGS

# check the number of bootstrap replicates.
check.replicates = function(R, default = 200) {

  if (missing(R) || is.null(R))
    R = default
  else if (!is.positive.integer(R))
    stop("the number of bootstrap replicates must be a positive integer.")

  return(R)

}#CHECK.RESAMPLING

# check the size of bootstrap replicates.
check.bootsize = function(m, data, default = nrow(data)) {

  if (missing(m) || is.null(m))
    m = default
  else if (!is.positive.integer(m))
    stop("bootstrap sample size must be a positive integer.")

  return(m)

}#CHECK.BOOTSIZE

# check a prior distribution against the observed variable.
check.classifier.prior = function(prior, training) {

  if (missing(prior) || is.null(prior)) {

    # use the empirical probabilities in the fitted network, or a flat prior
    # as a last resort.
    if (is(training, c("bn.fit.dnode", "bn.fit.onode")))
      prior = training$prob
    else
      prior = rep(1, nlevels(training))

  }#THEN
  else {

    if (is(training, c("bn.fit.dnode", "bn.fit.onode")))
      nlvls = dim(training$prob)[1]
    else
      nlvls = nlevels(training)

    if (length(prior) != nlvls)
      stop("the prior distribution and the training variable have a different number of levels.")
    if (!is.nonnegative.vector(prior))
      stop("the prior distribution must be expressed as a probability vector.")

    # make sure the prior probabilities sum to one.
    prior = prior / sum(prior)

  }#ELSE

  return(prior)

}#CHECK.CLASSIFIER.PRIOR

# check a vector of weights.
check.weights = function(weights, len) {

  if (missing(weights) || is.null(weights)) {

    weights = rep(1, len)

  }#THEN
  else {

    if (!is.nonnegative.vector(weights))
      stop("missing or negative weights are not allowed.")

    if (length(weights) != len)
      stop("wrong number of weights, ", length(weights),
        " weights while ", len, " are needed.")

    weights = prop.table(weights)

  }#ELSE

  return(weights)

}#CHECK.WEIGHTS

# check evidence in list format for mutilated networks.
check.mutilated.evidence = function(evidence, graph) {

  # check whether evidence is there.
  if (missing(evidence))
    stop("evidence must be a list with elements named after the nodes in the graph.")
  # if evindence is TRUE there's nothing to check.
  if (identical(evidence, TRUE))
    return(TRUE)
  # check whether evidence is a named list.
  if (!is(evidence, "list"))
    stop("evidence must be a list with elements named after the nodes in the graph.")
  # check the node labels in evidence.
  check.nodes(names(evidence), graph = graph)

  if (is(graph, "bn")) {

    # check the network is completely directed.
    if (!is.dag(graph$arcs, names(graph$nodes)))
      stop("the graph is only partially directed.")

  }#THEN
  else {

     # check the evidence is appropriate for the nodes.
     for (fixed in names(evidence)) {

       # extract the node and the evidence.
       cur = graph[[fixed]]
       ev = evidence[[fixed]]

       if (is(cur, c("bn.fit.dnode", "bn.fit.onode"))) {

         if (is.factor(ev))
           evidence[[fixed]] = ev = as.character(ev)

         if (!is.string.vector(ev) || any(ev %!in% dimnames(cur$prob)[[1]]))
           stop("the evidence for node ", fixed, " must be valid levels.")

       }#THEN
       else if (is(cur, "bn.fit.gnode")) {

         # for continuous nodes evidence must be real numbers.
         if (!is.real.vector(ev) || (length(ev) %!in% 1:2))
           stop("the evidence ", fixed, " must be a real number or a finite interval.")
         storage.mode(ev) = "double"
         # make sure interval boundaries are in the right order.
         evidence[[fixed]] = sort(ev)

       }#THEN

     }#FOR

  }#THEN

  return(evidence)

}#CHECK.MUTILATED.EVIDENCE

# check the string representation (aka the formula) of a network.
check.modelstring = function(string) {

  # check the type.
  if (!is.string(string))
    stop("string must be a character string.")

  # check the syntax (separate regexps for root nodes and non-root ndoes).
  correct.format = paste("^(",
    "\\[[^\\[\\]\\|:]+?\\]",
    "|",
    "\\[[^\\[\\]\\|:]+?\\|[^\\[\\]\\|:]+?([:]{0,1}[^\\[\\]\\|:])*?\\]",
  ")+$", sep = "")

  if (!grepl(correct.format, string, perl = TRUE))
    stop("malformed model string format (see ?modelstring).")

}#CHECK.MODELSTRING

