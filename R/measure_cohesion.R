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
#'   - `net_by_components()` measures the number of components
#'   in the network, either strongly or weakly connected.
#'   - `net_by_independence()` measures the independence number, 
#'   or size of the largest independent set in the network.
#'   
#' @template param_data
#' @template param_connectivity
#' @family cohesion
#' @template net_measure
#' @section Signed networks:
#'   `net_by_compactness()` measures distance, and a negative tie is hostility
#'   rather than a channel along which cohesion travels.
#'   Where the network is signed, it therefore considers only the positive ties.
#'   Use [manynet::to_unsigned()] first to control this yourself.
#'   The other measures in this topic do not depend on distance,
#'   and so use every tie whatever its sign.
#' @section Multilevel networks:
#'   A multilevel network reports itself as two-mode,
#'   but holds ties within a mode as well as between them,
#'   so it cannot be projected onto one mode.
#'   `net_by_independence()` therefore measures a multilevel network whole,
#'   which is the quantity wanted in any case.
#'   The projection remains for genuine two-mode networks,
#'   where no two nodes of one mode are ever tied
#'   and the unprojected answer would be trivially the larger mode.
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
    # counting ties rather than summing weights, so that the two-mode branch
    # stays a ratio of ties to possible ties, as the one-mode branch is
    mat <- manynet::as_matrix(manynet::to_unweighted(.data))
    out <- sum(mat) / (nrow(mat) * ncol(mat))
  } else {
    out <- igraph::edge_density(manynet::as_igraph(.data))
  }
  make_network_measure(out, .data, call = deparse(sys.call()),
                       measure = "density", range = c(0, 1),
                       normalization = "normalized")
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
  dists <- igraph::distances(manynet::as_igraph(.to_positive(.data)),
                             mode = "out")
  recip <- 1/dists
  diag(recip) <- 0 # exclude self-pairs
  recip[!is.finite(recip)] <- 0 # unreachable pairs contribute nothing
  n <- manynet::net_nodes(.data)
  out <- if(n < 2) NaN else sum(recip)/(n*(n-1))
  make_network_measure(out, .data, call = deparse(sys.call()),
                       measure = "compactness", range = c(0, 1),
                       normalization = "normalized")
}

#' @rdname measure_cohesion
#' @importFrom igraph components
#' @examples
#' net_by_components(fict_thrones)
#' net_by_components(fict_thrones, connectivity = "weak")
#' @export
net_by_components <- function(.data, connectivity = c("strong", "weak")){
  connectivity <- match.arg(connectivity)
  .data <- manynet::expect_nodes(.data)
  object <- manynet::as_igraph(.data)
  make_network_measure(igraph::components(object, mode = connectivity)$no,
                       object, call = deparse(sys.call()),
                       measure = "number of components", range = c(1, Inf),
                       normalization = "none", variant = connectivity)
}

#' @rdname measure_cohesion 
#' @importFrom igraph ivs_size
#' @examples 
#' net_by_independence(ison_adolescents)
#' net_by_independence(fict_actually)
#' @export
net_by_independence <- function(.data){
  .data <- manynet::expect_nodes(.data)
  # A multilevel network reports itself as two-mode, but has ties within a
  # mode, so it cannot be projected. It needs no projection either: the
  # independence number of the whole network is already the quantity wanted.
  # The two-mode branch exists because no two nodes of one mode are ever tied
  # there, which would make the answer trivially the size of the larger mode.
  if(manynet::is_twomode(.data) && !.is_multilevel(.data)){
    out <- igraph::ivs_size(manynet::to_mode1(manynet::as_igraph(.data)))
  } else {
    out <- igraph::ivs_size(manynet::to_undirected(manynet::as_igraph(.data)))
  }
  make_network_measure(out, .data, call = deparse(sys.call()),
                       measure = "independence number", range = c(1, Inf),
                       normalization = "none")
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
#' @section Signed networks:
#'   Both measures count path lengths, and a negative tie is hostility rather
#'   than a channel along which cohesion travels.
#'   Where the network is signed, they therefore consider only the positive
#'   ties. Use [manynet::to_unsigned()] first to control this yourself.
#'   
#'   Note that dropping the negative ties can disconnect the network,
#'   in which case the measure covers the reachable pairs only.
NULL

#' @rdname measure_breadth 
#' @importFrom igraph diameter
#' @examples 
#' net_by_diameter(fict_marvel)
#' net_by_diameter(to_giant(fict_marvel))
#' @export
net_by_diameter <- function(.data){
  .data <- manynet::expect_nodes(.data)
  object <- manynet::as_igraph(.to_positive(.data))
  make_network_measure(igraph::diameter(object,
                                        directed = manynet::is_directed(object)),
                       object, call = deparse(sys.call()),
                       measure = "diameter", range = c(0, Inf),
                       normalization = "none")
}

#' @rdname measure_breadth 
#' @importFrom igraph mean_distance
#' @examples 
#' net_by_length(fict_marvel)
#' net_by_length(to_giant(fict_marvel))
#' @export
net_by_length <- function(.data){
  .data <- manynet::expect_nodes(.data)
  object <- manynet::as_igraph(.to_positive(.data))
  make_network_measure(igraph::mean_distance(object,
                                             directed = manynet::is_directed(object)),
                       object, call = deparse(sys.call()),
                       measure = "average path length", range = c(0, Inf),
                       normalization = "none")
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
#' net_by_cohesion(fict_greys)
#' net_by_cohesion(to_giant(fict_greys))
#' @export
net_by_cohesion <- function(.data){
  .data <- manynet::expect_nodes(.data)
  make_network_measure(igraph::cohesion(manynet::as_igraph(.data)),
                       .data, call = deparse(sys.call()),
                       measure = "node connectivity", range = c(0, Inf),
                       normalization = "none")
}

#' @rdname measure_fragmentation 
#' @importFrom igraph adhesion
#' @examples 
#' net_by_adhesion(fict_greys)
#' net_by_adhesion(to_giant(fict_greys))
#' @export
net_by_adhesion <- function(.data){
  .data <- manynet::expect_nodes(.data)
  make_network_measure(igraph::adhesion(manynet::as_igraph(.data)),
                       .data, call = deparse(sys.call()),
                       measure = "tie connectivity", range = c(0, Inf),
                       normalization = "none")
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
  make_network_measure(min(out), .data, call = deparse(sys.call()),
                       measure = "strength", range = c(0, Inf),
                       normalization = "none")
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
  make_network_measure(min(out), .data, call = deparse(sys.call()),
                       measure = "toughness", range = c(0, Inf),
                       normalization = "none")
}

