# Complex contagion ####

#' @rdname measure_broker_tie
#' @section Width:
#'   `tie_by_width()` measures the width of the bridge that each tie is in,
#'   after Centola and Macy (2007): the number of ties that join the two
#'   neighbourhoods at its ends.
#'   For a tie between \eqn{i} and \eqn{j}, it counts the ties with one end in
#'   \eqn{N[i] \setminus \{j\}} and the other in \eqn{N[j] \setminus \{i\}},
#'   where \eqn{N[i]} is \eqn{i} with its neighbours.
#'   The tie itself is one of them, so a long tie, between two neighbourhoods
#'   with nothing else in common, has a width of 1,
#'   and a tie inside a cluster has a large width.
#'   A complex contagion, which needs several sources to spread,
#'   passes only along bridges that are wide enough.
#'
#'   This is not the bridge of `tie_is_bridge()`, which marks a tie whose
#'   removal disconnects the network.
#'   A tie in a wide bridge is never such a tie.
#'   Direction is ignored, and a two-mode network is not yet available.
#' @references
#' ## On bridge width
#'   Centola, Damon, and Michael Macy. 2007.
#'   "Complex contagions and the weakness of long ties".
#'   _American Journal of Sociology_ 113(3): 702-734.
#'   \doi{10.1086/521848}
#' @examples
#' tie_by_width(ison_adolescents)
#' @export
tie_by_width <- function(.data){
  .data <- manynet::expect_ties(.data)
  if(manynet::is_cognitive(.data))
    return(.map_css_ties(.data, tie_by_width))
  if(manynet::is_twomode(.data))
    manynet::snet_unavailable("The width of a bridge needs a one-mode network.")
  adj <- .contagion_adjacency(.data)
  g <- manynet::as_igraph(.data)
  ends <- igraph::ends(g, igraph::E(g), names = FALSE)
  out <- apply(ends, 1, function(e){
    i <- e[1]; j <- e[2]
    a <- adj[i, ]; a[i] <- TRUE; a[j] <- FALSE   # N[i] without j
    b <- adj[j, ]; b[j] <- TRUE; b[i] <- FALSE   # N[j] without i
    # ties from a to b, counting a tie with both ends in both sides once
    joins <- adj & outer(a, b) | adj & outer(b, a)
    sum(joins[upper.tri(joins)])
  })
  make_tie_measure(out, .data, measure = "bridge width",
                   range = c(0, Inf), normalization = "none")
}

#' @rdname measure_central_close
#' @section Contagion centrality:
#'   `node_by_contagion()` measures the complex centrality of
#'   Guilbeault and Centola (2021): how far a complex contagion travels when
#'   it starts from the neighbourhood of each node.
#'   For each node \eqn{i}, the contagion starts on \eqn{i} and its neighbours.
#'   A node adopts once at least `threshold` of its neighbours have adopted,
#'   until no more nodes adopt.
#'   The complex path length to each other node \eqn{j} is its distance from
#'   \eqn{i} through the nodes that adopted, or 0 where \eqn{j} did not adopt.
#'   The centrality is the mean of these lengths over the nodes outside the
#'   neighbourhood of \eqn{i}.
#'   So a high value means a contagion from that neighbourhood travels far.
#'
#'   With `threshold = 1` the contagion is simple and reaches every node
#'   in the component.
#'   A `threshold` below 1 is a fraction of each node's neighbours.
#'   This measure needs only a network, not a played diffusion.
#'   Direction and tie weights are ignored,
#'   and a two-mode network is not yet available.
#' @param threshold For `node_by_contagion()`, the number of neighbours that
#'   must adopt before a node adopts, by default 2.
#'   A value below 1 is the fraction of each node's neighbours.
#' @references
#' ## On complex centrality
#'   Guilbeault, Douglas, and Damon Centola. 2021.
#'   "Topological measures for identifying and predicting the spread of
#'   complex contagions".
#'   _Nature Communications_ 12: 4430.
#'   \doi{10.1038/s41467-021-24704-6}
#' @examples
#' node_by_contagion(ison_adolescents)
#' @export
node_by_contagion <- function(.data, threshold = 2){
  .data <- manynet::expect_nodes(.data)
  .data <- .to_aggregated_css(.data)
  if(manynet::is_twomode(.data))
    manynet::snet_unavailable("Complex centrality needs a one-mode network.")
  if(!is.numeric(threshold) || length(threshold) != 1 || is.na(threshold) ||
     threshold <= 0)
    manynet::snet_abort("{.arg threshold} must be one number above 0.")
  adj <- .contagion_adjacency(.data)
  n <- nrow(adj)
  g <- igraph::graph_from_adjacency_matrix(adj * 1, mode = "undirected")
  out <- vapply(seq_len(n), function(i){
    seeds <- adj[i, ]; seeds[i] <- TRUE
    outside <- n - sum(seeds)
    if(outside == 0) return(0)
    adopted <- .contagion_spread(adj, seeds, threshold)
    sub <- which(adopted)
    d <- igraph::distances(igraph::induced_subgraph(g, sub),
                           v = match(i, sub))[1, ]
    lengths <- rep(0, n)
    lengths[sub] <- d
    lengths[seeds] <- 0
    sum(lengths[is.finite(lengths)]) / outside
  }, numeric(1))
  make_node_measure(out, .data, measure = "complex centrality",
                    range = c(0, Inf), normalization = "none")
}

# The symmetric logical adjacency of a one-mode network, with no self-ties
.contagion_adjacency <- function(.data){
  adj <- manynet::as_matrix(manynet::to_undirected(manynet::to_unweighted(.data)))
  dimnames(adj) <- NULL
  adj <- adj != 0 | t(adj != 0)
  diag(adj) <- FALSE
  adj
}

# A deterministic threshold contagion from the seeds: a node adopts once at
# least `threshold` of its neighbours (or that fraction of them, below 1)
# have adopted
.contagion_spread <- function(adj, seeds, threshold){
  active <- seeds
  degree <- rowSums(adj)
  repeat{
    count <- as.numeric(adj %*% active)
    need <- `if`(threshold >= 1, count >= threshold,
                 degree > 0 & count >= threshold * degree)
    new <- !active & need
    if(!any(new)) break
    active <- active | new
  }
  active
}
