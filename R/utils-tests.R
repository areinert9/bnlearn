
# check whether a graph is completely directed (has no undirected arcs).
is.dag = function(arcs, nodes) {

  if (nrow(arcs) == 0)
    return(TRUE)

  .Call(call_is_dag,
        arcs = factor(arcs),
        nnodes = length(nodes))

}#IS.DAG

# alias of is.dag for readable code.
is.pdag = function(arcs, nodes) !is.dag(arcs, nodes)

# generic is.acyclic backend (calls the right one for the graph at hand).
is.acyclic = function(arcs, nodes, return.nodes = FALSE, directed = FALSE,
    debug = FALSE) {

  .Call(call_is_pdag_acyclic,
        arcs = arcs,
        nodes = nodes,
        return_nodes = return.nodes,
        directed = directed,
        debug = debug)

}#IS.ACYCLIC.BACKEND

# compute the sample size / CPT cells ratio.
obs.per.cell = function(x, y, z = NULL, data, weights) {

  opc = 0
  ndata = sum(weights)
  nlx = nlevels(data[, x])
  nly = nlevels(data[, y])

  # return +Inf for continuous data to bypass countermeasures
  # thought for sparce discrete data.
  if (data.type(data) == "continuous")
    return(Inf)

  if (is.null(z) || (length(z) == 0)) {

    opc = ndata / (nlx * nly)

  }#THEN
  else if (is.character(z)) {

    if (length(z) == 1)
      opc = ndata / (nlx * nly * nlevels(data[, z]))
    else if (length(z) > 1)
      opc = ndata / (nlx * nly *
        prod(sapply(z, function(col) { nlevels(data[, col]) } )))

  }#THEN
  else if (is.factor(z)) {

    opc = ndata / (nlx * nly * nlevels(z))

  }#ELSE

  return(opc)

}#OBS.PER.CELL

