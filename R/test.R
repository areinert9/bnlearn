
# conditional and unconditional independence tests (vectorized in x, scalar
# in y).
indep.test = function(x, y, sx, data, test, B = 0L, alpha = 1, learning = TRUE, weights = weights) {

  .Call(call_indep_test,
        x = x,
        y = y,
        sx = sx[sx != ""],
        data = data,
        test = test,
        B = B,
        alpha = alpha,
        learning = learning,
        weights = weights)

}#INDEP.TEST

# test against all possible subsets of the conditioning set (scalar in both x
# and y).
allsubs.test = function(x, y, sx, fixed = character(0), data, test, B = 0L,
    alpha = 1, min = 0, max = length(sx), debug = FALSE, weights = weights) {

  sx = sx[sx != ""]
  if (missing(max))
    max = length(sx)

  .Call(call_allsubs_test,
        x = x,
        y = y,
        sx = sx,
        fixed = fixed,
        data = data,
        test = test,
        B = B,
        alpha = alpha,
        min = as.integer(min),
        max = as.integer(max),
        debug = debug,
        weights = weights)

}#ALLSUBS.TEST

# test each variable in turn given the rest as a conditioning set.
roundrobin.test = function(x, z, fixed, data, test, B = 0L, alpha = 1,
    debug = FALSE, weights) {

  if (length(z) == 0)
    return(structure(numeric(0), names = character(0)))

  .Call(call_roundrobin_test,
        x = x,
        z = z,
        fixed = fixed,
        data = data,
        test = test,
        B = B,
        alpha = alpha,
        debug = debug,
        weights = weights)

}#ROUNDROBIN.TEST

# Mutual Information (discrete data)
mi.test = function(x, y, ndata, gsquare = TRUE, adjusted = FALSE) {

  .Call(call_mi,
        x = x,
        y = y,
        gsquare = gsquare,
        adjusted = adjusted)

}#MI.TEST

