# Cohesion ####

#' Measures of network cohesion
#' @name measure_cohesion
#' @description
#'   These functions return values or vectors relating to how cohesive a network is:
#'   
#'   - `net_by_density()` measures the ratio of ties to the number
#'   of possible ties.
#'   - `net_by_compactness()` measures the average closeness of all pairs
#'   of nodes in the network.
#'   - `net_by_components()` measures the number of (strong) components
#'   in the network.
#'   - `net_by_independence()` measures the independence number, 
#'   or size of the largest independent set in the network.
#'   
#' @template param_data
#' @family cohesion
#' @template net_measure
NULL

#' @rdname measure_cohesion
#' @importFrom igraph edge_density
#' @examples 
#' net_by_density(ison_adolescents)
#' net_by_density(ison_southern_women)
#' @export
net_by_density <- function(.data) {
  .data <- manynet::expect_nodes(.data)
  if (manynet::is_twomode(.data)) {
    mat <- manynet::as_matrix(.data)
    out <- sum(mat) / (nrow(mat) * ncol(mat))
  } else {
    out <- igraph::edge_density(manynet::as_igraph(.data))
  }
  make_network_measure(out, .data, call = deparse(sys.call()))
}

#' @rdname measure_cohesion
#' @section Compactness:
#'   Compactness is the average of the reciprocal distances between all pairs
#'   of nodes:
#'   \deqn{C = \frac{\sum_{i \neq j} \frac{1}{d(i,j)}}{N(N-1)}}
#'   where unreachable pairs contribute \eqn{0}.
#'   Its complement, \eqn{1 - C}, is sometimes called breadth.
#'
#'   Compactness is more discriminating than
#'   [net_by_connectedness()], which counts only whether pairs are reachable at
#'   all. Two networks in which every node can reach every other are equally
#'   connected, but the one in which they do so in fewer steps is more compact.
#'   A complete network scores 1, and an empty network 0.
#'   It is the network-level counterpart of [node_by_harmonic()], such that 
#'   `net_by_compactness(ison_adolescents) == mean(node_by_harmonic(ison_adolescents, normalized = TRUE, cutoff = -1))`.
#'   
#'   Note that this quantity is known in the physics literature as the
#'   _global efficiency_ of a network (Latora and Marchiori 2001).
#'   It is named compactness here for the social network analytic tradition,
#'   partly to avoid confusion with the unrelated
#'   [net_by_efficiency()] (Krackhardt) and [node_by_efficiency()] (Burt).
#' @references
#' ## On compactness
#' Borgatti, Stephen P., Martin G. Everett, Jeffrey C. Johnson,
#' and Filip Agneessens. 2022.
#' _Analyzing Social Networks Using R_, chapter 10.
#' London: SAGE.
#'
#' Latora, Vito, and Massimo Marchiori. 2001.
#' "Efficient Behavior of Small-World Networks".
#' _Physical Review Letters_ 87(19): 198701.
#' \doi{10.1103/PhysRevLett.87.198701}
#' @examples
#' net_by_compactness(ison_adolescents)
#' net_by_compactness(ison_southern_women)
#' @export
net_by_compactness <- function(.data) {
  .data <- manynet::expect_nodes(.data)
  # note that igraph's default mode ignores direction, which would treat a
  # directed network as though every tie ran both ways
  dists <- igraph::distances(manynet::as_igraph(.data), mode = "out")
  recip <- 1/dists
  diag(recip) <- 0 # exclude self-pairs
  recip[!is.finite(recip)] <- 0 # unreachable pairs contribute nothing
  n <- manynet::net_nodes(.data)
  out <- if(n < 2) NaN else sum(recip)/(n*(n-1))
  make_network_measure(out, .data, call = deparse(sys.call()))
}

#' @rdname measure_cohesion
#' @section Components:
#'   To get the 'weak' components of a directed graph, 
#'   please use `manynet::to_undirected()` first.
#' @importFrom igraph components
#' @examples
#' net_by_components(fict_thrones)
#' net_by_components(to_undirected(fict_thrones))
#' @export
net_by_components <- function(.data){
  .data <- manynet::expect_nodes(.data)
  object <- manynet::as_igraph(.data)
  make_network_measure(igraph::components(object, mode = "strong")$no,
                       object, call = deparse(sys.call()))
}

#' @rdname measure_cohesion 
#' @importFrom igraph ivs_size
#' @examples 
#' net_by_independence(ison_adolescents)
#' @export
net_by_independence <- function(.data){
  .data <- manynet::expect_nodes(.data)
  if(manynet::is_twomode(.data)){
    out <- igraph::ivs_size(manynet::to_mode1(manynet::as_igraph(.data)))
  } else {
    out <- igraph::ivs_size(manynet::to_undirected(manynet::as_igraph(.data)))
  }
  make_network_measure(out, .data, call = deparse(sys.call()))
}

# Breadth ####

#' Measures of network breadth
#' @name measure_breadth
#' @description
#'   These functions return values or vectors relating to how broad a network is.
#'   
#'   - `net_by_diameter()` measures the maximum path length in the network.
#'   - `net_by_length()` measures the average path length in the network.
#'   
#' @template param_data
#' @family cohesion
#' @template net_measure
NULL

#' @rdname measure_breadth 
#' @importFrom igraph diameter
#' @examples 
#' net_by_diameter(fict_marvel)
#' net_by_diameter(to_giant(fict_marvel))
#' @export
net_by_diameter <- function(.data){
  .data <- manynet::expect_nodes(.data)
  object <- manynet::as_igraph(.data)
  make_network_measure(igraph::diameter(object, 
                                        directed = manynet::is_directed(object)),
                       object, call = deparse(sys.call()))
}

#' @rdname measure_breadth 
#' @importFrom igraph mean_distance
#' @examples 
#' net_by_length(fict_marvel)
#' net_by_length(to_giant(fict_marvel))
#' @export
net_by_length <- function(.data){
  .data <- manynet::expect_nodes(.data)
  object <- manynet::as_igraph(.data)
  make_network_measure(igraph::mean_distance(object,
                                             directed = manynet::is_directed(object)),
                       object, call = deparse(sys.call()))
}

# Fragmentation ####

#' Measures of network fragmentation
#' @name measure_fragmentation
#' @description
#'   These functions return values relating to how connected a network is
#'   and the number of nodes or edges to remove that would increase fragmentation.
#'   
#'   - `net_by_cohesion()` measures the minimum number of nodes to remove
#'   from the network needed to increase the number of components.
#'   - `net_by_toughness()` measures the number of nodes that would need to be
#'   removed from a network to increase its number of components.
#'   - `net_by_adhesion()` measures the minimum number of ties to remove
#'   from the network needed to increase the number of components.
#'   - `net_by_strength()` measures the number of ties that would need to be
#'   removed from a network to increase its number of components.
#'   
#' @template param_data
#' @family cohesion
#' @template net_measure
NULL

#' @rdname measure_fragmentation 
#' @importFrom igraph cohesion
#' @references
#' ## On cohesion
#' White, Douglas R and Frank Harary. 2001. 
#' "The Cohesiveness of Blocks In Social Networks: Node Connectivity and Conditional Density." 
#' _Sociological Methodology_ 31(1): 305-59.
#' \doi{10.1111/0081-1750.00098}
#' @examples 
#' net_by_cohesion(fict_marvel)
#' net_by_cohesion(to_giant(fict_marvel))
#' @export
net_by_cohesion <- function(.data){
  .data <- manynet::expect_nodes(.data)
  make_network_measure(igraph::cohesion(manynet::as_igraph(.data)), 
                       .data, call = deparse(sys.call()))
}

#' @rdname measure_fragmentation 
#' @importFrom igraph adhesion
#' @examples 
#' net_by_adhesion(fict_marvel)
#' net_by_adhesion(to_giant(fict_marvel))
#' @export
net_by_adhesion <- function(.data){
  .data <- manynet::expect_nodes(.data)
  make_network_measure(igraph::adhesion(manynet::as_igraph(.data)), 
                       .data, call = deparse(sys.call()))
}

#' @rdname measure_fragmentation 
#' @examples 
#' net_by_strength(ison_adolescents)
#' @export
net_by_strength <- function(.data){
  .data <- manynet::expect_nodes(.data)
  n <- manynet::net_ties(.data)
  seties <- unlist(lapply(1:n, utils::combn, x = 1:n, simplify = FALSE), recursive = FALSE)
  out <- vapply(seties, function(x) length(x)/net_by_components(manynet::delete_ties(.data, x)), 
                FUN.VALUE = numeric(1))
  make_network_measure(min(out), .data, call = deparse(sys.call()))
}

#' @rdname measure_fragmentation 
#' @examples 
#' net_by_toughness(ison_adolescents)
#' @export
net_by_toughness <- function(.data){
  .data <- manynet::expect_nodes(.data)
  n <- manynet::net_nodes(.data)
  seties <- unlist(lapply(1:n, utils::combn, x = 1:n, simplify = FALSE), recursive = FALSE)
  out <- vapply(seties, function(x) length(x)/net_by_components(manynet::delete_nodes(.data, x)), 
                FUN.VALUE = numeric(1))
  make_network_measure(min(out), .data, call = deparse(sys.call()))
}

