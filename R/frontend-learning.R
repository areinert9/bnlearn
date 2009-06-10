
# Grow-Shrink frontend.
gs = function(x, cluster = NULL, whitelist = NULL, blacklist = NULL,
    test = NULL, alpha = 0.05, B = NULL, debug = FALSE, optimized = TRUE,
    strict = FALSE, undirected = FALSE) {

  bnlearn(x = x, cluster = cluster, whitelist = whitelist,
    blacklist = blacklist, test = test, alpha = alpha, B = B, debug = debug,
    optimized = optimized, strict = strict, undirected = undirected)

}#GS

# Incremental Association frontend.
iamb = function(x, cluster = NULL, whitelist = NULL, blacklist = NULL,
    test = NULL, alpha = 0.05, B = NULL, debug = FALSE, optimized = TRUE,
    strict = FALSE, undirected = FALSE) {

  bnlearn(x = x, cluster = cluster, whitelist = whitelist,
    blacklist = blacklist, test = test, alpha = alpha, B = B, method = "iamb",
    debug = debug, optimized = optimized, strict = strict,
    undirected = undirected)

}#IAMB

# Fast-IAMB frontend.
fast.iamb = function(x, cluster = NULL, whitelist = NULL, blacklist = NULL,
    test = NULL, alpha = 0.05, B = NULL, debug = FALSE, optimized = TRUE,
    strict = FALSE, undirected = FALSE) {

  bnlearn(x = x, cluster = cluster, whitelist = whitelist,
    blacklist = blacklist, test = test, alpha = alpha, B = B,
    method = "fast-iamb", debug = debug, optimized = optimized,
    strict = strict, undirected = undirected)

}#FAST.IAMB

# Inter-IAMB frontend.
inter.iamb = function(x, cluster = NULL, whitelist = NULL, blacklist = NULL,
    test = NULL, alpha = 0.05, B = NULL, debug = FALSE, optimized = TRUE,
    strict = FALSE, undirected = FALSE) {

  bnlearn(x = x, cluster = cluster, whitelist = whitelist,
    blacklist = blacklist, test = test, alpha = alpha, B = B,
    method = "inter-iamb", debug = debug, optimized = optimized,
    strict = strict, undirected =  undirected)

}#INTER.IAMB

# MMPC frontend.
mmpc = function(x, cluster = NULL, whitelist = NULL, blacklist = NULL,
    test = NULL, alpha = 0.05, B = NULL, debug = FALSE, optimized = TRUE,
    strict = FALSE) {

  bnlearn(x = x, cluster = cluster, whitelist = whitelist,
    blacklist = blacklist, test = test, alpha = alpha, B = B,
    method = "mmpc", debug = debug, optimized = optimized,
    strict = strict, undirected = TRUE)

}#MMPC

# Hill Climbing greedy search frontend.
hc = function(x, start = NULL, whitelist = NULL, blacklist = NULL,
    score = NULL, ..., debug = FALSE, restart = 0, perturb = 1,
    max.iter = Inf, optimized = TRUE) {

  greedy.search(x = x, start = start, whitelist = whitelist,
    blacklist = blacklist, score = score, heuristic = "hc",
    ..., debug = debug, restart = restart, perturb = perturb,
   max.iter = max.iter, optimized = optimized)

}#HC
