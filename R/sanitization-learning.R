
# check the threshold on the maximum number of parents.
check.maxp = function(maxp, data) {

  if (is.null(maxp)) {

    maxp = Inf

  }#THEN
  else if (!isTRUE(all.equal(maxp, Inf))) {

    if (!is.positive.integer(maxp))
      stop("maxp must be a positive integer number.")
    if (maxp >= ncol(data))
      warning("maximum number of parents should be lower than the number of nodes, the limit will be ignored.")

  }#ELSE

  return(as.numeric(maxp))

}#CHECK.MAXP

