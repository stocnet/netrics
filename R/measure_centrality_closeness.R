# Closeness-like centralities ####

#' Measuring nodes closeness-like centrality
#' @name measure_central_close
#' @description
#'   These functions calculate common closeness-related centrality measures 
#'   that rely on path-length for one- and two-mode networks:
#'   
#'   - `node_by_closeness()` measures the closeness centrality of nodes in a 
#'   network.
#'   - `node_by_harmonic()` measures nodes' harmonic centrality or valued 
#'   centrality, which is thought to behave better than reach centrality 
#'   for disconnected networks.
#'   - `node_by_reach()` measures nodes' reach centrality,
#'   or how many nodes they can reach within _k_ steps.
#'   - `node_by_information()` measures nodes' information centrality or 
#'   current-flow closeness centrality.
#'   - `node_by_eccentricity()` measures nodes' eccentricity or maximum distance
#'   from another node in the network.
#'   - `node_by_distance()` measures nodes' geodesic distance from or to a 
#'   given node.
#'   - `node_by_vitality()` measures a network's closeness vitality centrality,
#'   or the change in closeness centrality between networks with and without a
#'   given node.
#'   
#'   All measures attempt to use as much information as they are offered,
#'   including whether the networks are directed, weighted, or multimodal.
#'   If this would produce unintended results, 
#'   first transform the salient properties using e.g. [to_undirected()] functions.
#'   All centrality and centralization measures return normalized measures by default,
#'   including for two-mode networks.
#' @template param_data
#' @template param_norm
#' @template param_dir
#' @family closeness
#' @family centrality
#' @template node_measure
NULL

#' @rdname measure_central_close
#' @param cutoff Maximum path length to use during calculations.
#' @section Closeness centrality: 
#'   Closeness centrality, status centrality, or barycenter centrality is 
#'   defined as the reciprocal of the farness or distance, \eqn{d}, 
#'   from a node to all other nodes in the network:
#'   \deqn{C_C(i) = \frac{1}{\sum_j d(i,j)}}
#'   When (more commonly) normalised, the numerator is instead \eqn{N-1}.
#' @references
#' ## On closeness centrality
#' Bavelas, Alex. 1950. 
#' "Communication Patterns in Task‐Oriented Groups". 
#' _The Journal of the Acoustical Society of America_, 22(6): 725–730.
#' \doi{10.1121/1.1906679}
#' 
#' Harary, Frank. 1959. 
#' "Status and Contrastatus". 
#' _Sociometry_, 22(1): 23–43. 
#' \doi{10.2307/2785610}
#' @examples
#' node_by_closeness(ison_southern_women)
#' @export
node_by_closeness <- function(.data, normalized = TRUE, 
                              direction = "out", cutoff = NULL){
  
  .data <- manynet::expect_nodes(.data)
  weights <- `if`(manynet::is_weighted(.data), 
                  manynet::tie_weights(.data), NA)
  graph <- manynet::as_igraph(.data)
  
  # Do the calculations
  if (manynet::is_twomode(graph) & normalized){
    # farness <- rowSums(igraph::distances(graph = graph))
    closeness <- igraph::closeness(graph = graph, vids = igraph::V(graph), mode = direction)
    other_set_size <- ifelse(igraph::V(graph)$type, sum(!igraph::V(graph)$type), sum(igraph::V(graph)$type))
    set_size <- ifelse(igraph::V(graph)$type, sum(igraph::V(graph)$type), sum(!igraph::V(graph)$type))
    out <- closeness/(1/(other_set_size+2*set_size-2))
  } else {
    cutoff <- if (is.null(cutoff)) -1 else cutoff
    out <- igraph::closeness(graph = graph, vids = igraph::V(graph), mode = direction, 
                             cutoff = cutoff, weights = weights, normalized = normalized)
  }
  out <- make_node_measure(out, .data)
  out
} 

#' @rdname measure_central_close 
#' @section Harmonic centrality:
#'   Harmonic centrality or valued centrality reverses the sum and reciprocal 
#'   operations compared to closeness centrality:
#'   \deqn{C_H(i) = \sum_{i, i \neq j} \frac{1}{d(i,j)}}
#'   where \eqn{\frac{1}{d(i,j)} = 0} where there is no path between \eqn{i} and
#'   \eqn{j}. Normalization is by \eqn{N-1}. 
#'   Since the harmonic mean performs better than the arithmetic mean on
#'   unconnected networks, i.e. networks with infinite distances,
#'   harmonic centrality is to be preferred in these cases.
#' @references
#'   ## On harmonic centrality
#'   Marchiori, Massimo, and Vito Latora. 2000. 
#'   "Harmony in the small-world".
#'   _Physica A_ 285: 539-546.
#'   \doi{10.1016/S0378-4371(00)00311-3}
#'   
#'   Dekker, Anthony. 2005.
#'   "Conceptual distance in social network analysis".
#'   _Journal of Social Structure_ 6(3).
#' @export
node_by_harmonic <- function(.data, normalized = TRUE, cutoff = -1){
  .data <- manynet::expect_nodes(.data)
  out <- igraph::harmonic_centrality(as_igraph(.data), # weighted if present
                                     normalized = normalized, cutoff = cutoff)
  out <- make_node_measure(out, .data)
  out
}

#' @rdname measure_central_close 
#' @section Reach centrality: 
#'   In some cases, longer path lengths are irrelevant and 'closeness' should
#'   be defined as how many others are in a local neighbourhood.
#'   How many steps out this neighbourhood should be defined as is given by 
#'   the 'cutoff' parameter. 
#'   This is usually termed \eqn{k} or \eqn{m} in equations,
#'   which is why this is sometimes called (\eqn{m}- or) 
#'   \eqn{k}-step reach centrality:
#'   \deqn{C_R(i) = \sum_j d(i,j) \leq k}
#'   The maximum reach score is \eqn{N-1}, achieved when the node can reach all
#'   other nodes in the network in \eqn{k} steps or less,
#'   but the normalised version, \eqn{\frac{C_R}{N-1}}, is more common.
#'   Note that if \eqn{k = 1} (i.e. cutoff = 1), then this returns the node's degree.
#'   At higher cutoff reach centrality returns the size of the node's component.
#' @references
#' ## On reach centrality
#' Borgatti, Stephen P., Martin G. Everett, and J.C. Johnson. 2013. 
#' _Analyzing social networks_. 
#' London: SAGE Publications Limited.
#' @examples
#' node_by_reach(ison_adolescents)
#' @export
node_by_reach <- function(.data, normalized = TRUE, cutoff = 2){
  .data <- manynet::expect_nodes(.data)
  if(manynet::is_weighted(.data)){
    tore <- manynet::as_matrix(.data)/mean(manynet::as_matrix(.data))
    out <- 1/tore
  } else out <- igraph::distances(manynet::as_igraph(.data))
  diag(out) <- 0
  out <- rowSums(out <= cutoff)
  if(normalized) out <- out/(manynet::net_nodes(.data)-1)
  out <- make_node_measure(out, .data)
  out
}

#' @rdname measure_central_close 
#' @section Information centrality: 
#'   Information centrality, also known as current-flow centrality, 
#'   is a hybrid measure relating to both path-length and walk-based measures. 
#'   The information centrality of a node is the harmonic average of the 
#'   “bandwidth” or inverse path-length for all paths originating from the node.
#'   
#'   As described in the `{sna}` package, 
#'   information centrality works on an undirected but potentially weighted 
#'   network excluding isolates (which take scores of zero).
#'   It is defined as:
#'   \deqn{C_I = \frac{1}{T + \frac{\sum T - 2 \sum C_1}{|N|}}}
#'   where \eqn{C = B^-1} with \eqn{B} is a pseudo-adjacency matrix replacing
#'   the diagonal of \eqn{1-A} with \eqn{1+k},
#'   and \eqn{T} is the trace of \eqn{C} and \eqn{S_R} an arbitrary row sum 
#'   (all rows in \eqn{C} have the same sum). 
#'   
#'   Nodes with higher information centrality have a large number of short paths
#'   to many others in the network, and are thus considered to have greater
#'   control of the flow of information.
#' @references
#' ## On information centrality
#' Stephenson, Karen, and Marvin Zelen. 1989.
#' "Rethinking centrality: Methods and examples". 
#' _Social Networks_ 11(1):1-37. 
#' \doi{10.1016/0378-8733(89)90016-6}
#' 
#' Brandes, Ulrik, and Daniel Fleischer. 2005. 
#' "Centrality Measures Based on Current Flow". 
#' _Proc. 22nd Symp. Theoretical Aspects of Computer Science_ LNCS 3404: 533-544. 
#' \doi{10.1007/978-3-540-31856-9_44}
#' @export
node_by_information <- function(.data, normalized = TRUE){
  .data <- manynet::expect_nodes(.data)
  thisRequires("sna")
  out <- sna::infocent(manynet::as_network(.data),
                       gmode = ifelse(manynet::is_directed(.data), "digraph", "graph"),
                       diag = manynet::is_complex(.data),
                       rescale = normalized)
  make_node_measure(out, .data)
}

#' @rdname measure_central_close
#' @section Eccentricity centrality: 
#'   Eccentricity centrality, graph centrality, or the Koenig number,
#'   is the (if normalized, inverse of) the distance to the furthest node:
#'   \deqn{C_E(i) = \frac{1}{max_{j \in N} d(i,j)}}
#'   where the distance from \eqn{i} to \eqn{j} is \eqn{\infty} if unconnected.
#'   As such it is only well defined for connected networks.
#' @references
#' ## On eccentricity centrality
#'   Hage, Per, and Frank Harary. 1995.
#'   "Eccentricity and centrality in networks".
#'   _Social Networks_, 17(1): 57-63.
#'   \doi{10.1016/0378-8733(94)00248-9}
#' @export
node_by_eccentricity <- function(.data, normalized = TRUE){
  .data <- manynet::expect_nodes(.data)
  if(!manynet::is_connected(.data)) 
    manynet::snet_unavailable("Eccentricity centrality is only available for connected networks.")
  disties <- igraph::distances(as_igraph(.data))
  out <- apply(disties, 1, max)
  if(normalized) out <- 1/out
  make_node_measure(out, .data)
}

#   - `node_eccentricity()` measures nodes' eccentricity or Koenig number,
#   a measure of farness based on number of links needed to reach 
#   most distant node in the network.
# #' @rdname measure_holes 
# #' @importFrom igraph eccentricity
# #' @export
# cnode_eccentricity <- function(.data){
#  if(missing(.data)) {expect_nodes(); .data <- .G()}
#  out <- igraph::eccentricity(manynet::as_igraph(.data),
#                              mode = "out")
#  make_node_measure(out, .data)
# }

#' @rdname measure_central_close 
#' @param from,to Index or name of a node to calculate distances from or to.
#' @export
node_by_distance <- function(.data, from, to, normalized = TRUE){
  .data <- manynet::expect_nodes(.data)
  if(missing(from) && missing(to)) manynet::snet_abort("Either 'from' or 'to' must be specified.")
  if(!missing(from)) out <- igraph::distances(manynet::as_igraph(.data), v = from) else 
    if(!missing(to)) out <- igraph::distances(manynet::as_igraph(.data), to = to)
    if(normalized) out <- out/max(out)
    make_node_measure(out, .data)
}

#' @rdname measure_central_close 
#' @section Closeness vitality centrality: 
#'   The closeness vitality of a node is the change in the sum of all distances
#'   in a network, also known as the Wiener Index, when that node is removed.
#'   Note that the closeness vitality may be negative infinity if
#'   removing that node would disconnect the network.
#'   Formally:
#'   \deqn{C_V(i) = \sum_{j,k} d(j,k) - \sum_{j,k} d(j,k,G\ i)}
#'   where \eqn{d(j,k,G\ i)} is the distance between nodes \eqn{j} and \eqn{k}
#'   in the network with node \eqn{i} removed.
#' @references
#' ## On closeness vitality centrality
#'   Koschuetzki, Dirk, Katharina Lehmann, Leon Peeters, Stefan Richter,
#'   Dagmar Tenfelde-Podehl, and Oliver Zlotowski. 2005.
#'   "Centrality Indices", in
#'   Brandes, Ulrik, and Thomas Erlebach (eds.). 
#'   _Network Analysis: Methodological Foundations_. 
#'   Springer: Berlin, pp. 16-61.
#' @export
node_by_vitality <- function(.data, normalized = TRUE){
  .data <- manynet::expect_nodes(.data)
  .data <- manynet::as_igraph(.data)
  out <- vapply(manynet::snet_progress_nodes(.data), function(x){
    sum(igraph::distances(.data)) - 
      sum(igraph::distances(manynet::delete_nodes(.data, x)))
  }, FUN.VALUE = numeric(1))
  if(normalized) out <- out/max(out)
  make_node_measure(out, .data)
}

#' @rdname measure_central_close 
#' @section Random walk closeness centrality: 
#'   Random walk closeness centrality is based on the average length of
#'   random walks starting at all other nodes to reach a given node.
#'   It is defined as the inverse of the average hitting time to a node.
#'   This means that higher values are given to nodes that can be reached
#'   more quickly on average by random walks starting at other nodes.
#'   Formally:
#'   \deqn{C_{RW}(i) = \frac{1}{\frac{1}{N-1} \sum_{j \neq i} H_{ji}}}
#'   where \eqn{H_{ji}} is the hitting time from node \eqn{j} to node \eqn{i}.
#' @references
#' ## On random walk closeness centrality
#'   Noh, J.D. and R. Rieger. 2004.
#'   "Random Walks on Complex Networks".
#'   _Physical Review Letters_, 92(11): 118701.
#'   \doi{10.1103/PhysRevLett.92.118701}
#' @export
node_by_randomwalk <- function(.data, normalized = TRUE){
  .data <- manynet::expect_nodes(.data)
  # adjacency and degree matrices
  A <- manynet::as_matrix(manynet::to_multilevel(.data))
  degs <- node_by_deg(.data)
  D <- diag(degs)
  
  # Laplacian
  L <- D - A
  
  # pseudoinverse of Laplacian
  Lplus <- .ginv(L)
  
  volG <- sum(degs)  # total degree (2 * edges)
  n <- manynet::net_nodes(.data)
  
  out <- numeric(n)
  
  for (i in 1:n) {
    # hitting time from j to i
    hitting_times <- sapply(1:n, function(j) {
      if (i == j) return(0)
      volG * (Lplus[j, j] + Lplus[i, i] - 2 * Lplus[i, j])
    })
    # average hitting time to node i
    avg_ht <- mean(hitting_times[-i])
    out[i] <- 1 / avg_ht
  }
  
  make_node_measure(out, .data)
}

# This is a helper function to compute the Moore-Penrose generalized inverse
# of a matrix using its singular value decomposition (SVD).
.ginv <- function (X, tol = sqrt(.Machine$double.eps)){
  if (length(dim(X)) > 2L || !(is.numeric(X) || is.complex(X))) 
    stop("'X' must be a numeric or complex matrix")
  if (!is.matrix(X)) 
    X <- as.matrix(X)
  Xsvd <- svd(X)
  if (is.complex(X)) 
    Xsvd$u <- Conj(Xsvd$u)
  Positive <- Xsvd$d > max(tol * Xsvd$d[1L], 0)
  if (all(Positive)) 
    Xsvd$v %*% (1/Xsvd$d * t(Xsvd$u))
  else if (!any(Positive)) 
    array(0, dim(X)[2L:1L])
  else Xsvd$v[, Positive, drop = FALSE] %*% ((1/Xsvd$d[Positive]) * 
                                               t(Xsvd$u[, Positive, drop = FALSE]))
}

# Tie closeness centrality ####

#' Measuring ties closeness-like centrality
#' @name measure_centralities_close
#' @description
#'   `tie_by_closeness()` measures the closeness of each tie to other ties 
#'   in the network.
#'   
#'   All measures attempt to use as much information as they are offered,
#'   including whether the networks are directed, weighted, or multimodal.
#'   If this would produce unintended results, 
#'   first transform the salient properties using e.g. [to_undirected()] functions.
#'   All centrality and centralization measures return normalized measures by default,
#'   including for two-mode networks.
#' @template param_data
#' @template param_norm
#' @family closeness
#' @family centrality
#' @template tie_measure
NULL

#' @rdname measure_centralities_close 
#' @examples
#' (ec <- tie_by_closeness(ison_adolescents))
#' ison_adolescents %>% mutate_ties(weight = ec)
#' @export
tie_by_closeness <- function(.data, normalized = TRUE){
  .data <- manynet::expect_ties(.data)
  edge_adj <- manynet::to_ties(.data)
  out <- node_by_closeness(edge_adj, normalized = normalized)
  class(out) <- "numeric"
  make_tie_measure(out, .data)
}

# Closeness centralisation ####

#' Measuring networks closeness-like centralisation
#' @name measure_centralisation_close
#' @description
#'   - `net_by_closeness()` measures a network's closeness centralization.
#'   - `net_by_reach()` measures a network's reach centralization.
#'   - `net_by_harmonic()` measures a network's harmonic centralization.
#'   
#'   All measures attempt to use as much information as they are offered,
#'   including whether the networks are directed, weighted, or multimodal.
#'   If this would produce unintended results, 
#'   first transform the salient properties using e.g. [to_undirected()] functions.
#'   All centrality and centralization measures return normalized measures by default,
#'   including for two-mode networks.
#' @template param_data
#' @template param_norm
#' @template param_dir
#' @family closeness
#' @family centrality
#' @template net_measure
#' @param cutoff The maximum path length to consider when calculating betweenness.
#'   If negative or NULL (the default), there's no limit to the path lengths considered.
NULL

#' @rdname measure_centralisation_close 
#' @examples
#' net_by_closeness(ison_southern_women, direction = "in")
#' @export
net_by_closeness <- function(.data, normalized = TRUE,
                             direction = c("all", "out", "in")){
  
  .data <- manynet::expect_nodes(.data)
  direction <- match.arg(direction)
  graph <- manynet::as_igraph(.data)
  
  if (manynet::is_twomode(.data)) {
    clcent <- node_by_closeness(graph, normalized = TRUE)
    mode <- igraph::V(graph)$type
    mode1 <- length(mode) - sum(mode)
    mode2 <- sum(mode)
    out <- list()
    if (direction == "in") {
      out$nodes1 <- sum(max(clcent[!mode]) - clcent[!mode])/(((mode1 - 2)*(mode1 - 1))/(2 * mode1 - 3))
      out$nodes2 <- sum(max(clcent[mode]) - clcent[mode])/(((mode2 - 2)*(mode2 - 1))/(2 * mode2 - 3))
      if (mode1 > mode2) { #28.43
        lhs <- ((mode2 - 1)*(mode1 - 2) / (2 * mode1 - 3))
        rhs <- ((mode2 - 1)*(mode1 - mode2) / (mode1 + mode2 - 2))
        out$nodes1 <- sum(max(clcent[!mode]) - clcent[!mode])/( lhs +  rhs) # 0.2135
      }
      if (mode2 > mode1) {
        lhs <- ((mode1 - 1)*(mode2 - 2) / (2 * mode2 - 3))
        rhs <- ((mode1 - 1)*(mode2 - mode1) / (mode2 + mode1 - 2))
        out$nodes2 <- sum(max(clcent[mode]) - clcent[mode])/( lhs +  rhs)
      }
    } else {
      term1 <- 2*(mode1 - 1) * (mode2 + mode1 - 4)/(3*mode2 + 4*mode1 - 8)
      term2 <- 2*(mode1 - 1) * (mode1 - 2)/(2*mode2 + 3*mode1 - 6)
      term3 <- 2*(mode1 - 1) * (mode2 - mode1 + 1)/(2*mode2 + 3*mode1 - 4)
      out$nodes1 <- sum(max(clcent[!mode]) - clcent) / sum(term1, term2, term3)
      term1 <- 2*(mode2 - 1) * (mode1 + mode2 - 4)/(3*mode1 + 4*mode2 - 8)
      term2 <- 2*(mode2 - 1) * (mode2 - 2)/(2*mode1 + 3*mode2 - 6)
      term3 <- 2*(mode2 - 1) * (mode1 - mode2 + 1)/(2*mode1 + 3*mode2 - 4)
      out$nodes2 <- sum(max(clcent[mode]) - clcent) / sum(term1, term2, term3)
      
      if (mode1 > mode2) {
        term1 <- 2*(mode2 - 1) * (mode2 + mode1 - 2) / (3 * mode2 + 4 * mode1 - 8)
        term2 <- 2*(mode1 - mode2) * (2 * mode2 - 1) / (5 * mode2 + 2 * mode1 - 6)
        term3 <- 2*(mode2 - 1) * (mode1 - 2) / (2 * mode2 + 3 * mode1 - 6)
        term4 <- 2 * (mode2 - 1) / (mode1 + 4 * mode2 - 4)
        out$nodes1 <- sum(max(clcent[!mode]) - clcent) / sum(term1, term2, term3, term4)
      }
      if (mode2 > mode1) {
        term1 <- 2*(mode1 - 1) * (mode1 + mode2 - 2) / (3 * mode1 + 4 * mode2 - 8)
        term2 <- 2*(mode2 - mode1) * (2 * mode1 - 1) / (5 * mode1 + 2 * mode2 - 6)
        term3 <- 2*(mode1 - 1) * (mode2 - 2) / (2 * mode1 + 3 * mode2 - 6)
        term4 <- 2 * (mode1 - 1) / (mode2 + 4 * mode1 - 4)
        out$nodes2 <- sum(max(clcent[mode]) - clcent) / sum(term1, term2, term3, term4)
      }
    }
    out <- c("Mode 1" = out$nodes1, "Mode 2" = out$nodes2)
  } else {
    out <- igraph::centr_clo(graph = graph,
                             mode = direction,
                             normalized = normalized)$centralization
  }
  out <- make_network_measure(out, .data, call = deparse(sys.call()))
  out
}

#' @rdname measure_centralisation_close 
#' @export
net_by_reach <- function(.data, normalized = TRUE, cutoff = 2){
  .data <- manynet::expect_nodes(.data)
  reaches <- node_by_reach(.data, normalized = FALSE, cutoff = cutoff)
  out <- sum(max(reaches) - reaches)
  if(normalized) out <- out / sum(manynet::net_nodes(.data) - reaches)
  make_network_measure(out, .data, call = deparse(sys.call()))
}

#' @rdname measure_centralisation_close
#' @export
net_by_harmonic <- function(.data, normalized = TRUE, cutoff = 2){
  .data <- manynet::expect_nodes(.data)
  harm <- node_by_harmonic(.data, normalized = FALSE, cutoff = cutoff)
  out <- sum(max(harm) - harm)
  if(normalized) out <- out / sum(manynet::net_nodes(.data) - harm)
  make_network_measure(out, .data, call = deparse(sys.call()))
}

