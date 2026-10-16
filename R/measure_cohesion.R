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
#' @details
#'   For signed networks, `net_by_compactness()` considers only the positive ties.
#'   It measures distance, and a negative tie is hostility
#'   rather than a channel along which cohesion travels.
#'   Use [manynet::to_unsigned()] first to control this yourself.
#'   The other measures in this topic do not depend on distance,
#'   and so use every tie whatever its sign.
#'
#'   For multilevel networks, `net_by_independence()` measures the network whole,
#'   which is the quantity wanted in any case.
#'   A multilevel network reports itself as two-mode,
#'   but holds ties within a mode as well as between them,
#'   so it cannot be projected onto one mode.
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
  .data <- .to_aggregated_css(.data)
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
  .data <- .to_aggregated_css(.data)
  # note that igraph's default mode ignores direction, which would treat a
  # directed network as though every tie ran both ways
  dists <- igraph::distances(manynet::as_igraph(manynet::to_positive(.data)),
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
  .data <- .to_aggregated_css(.data)
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
  .data <- .to_aggregated_css(.data)
  # A multilevel network reports itself as two-mode, but has ties within a
  # mode, so it cannot be projected. It needs no projection either: the
  # independence number of the whole network is already the quantity wanted.
  # The two-mode branch exists because no two nodes of one mode are ever tied
  # there, which would make the answer trivially the size of the larger mode.
  if(manynet::is_twomode(.data) && !manynet::is_multilevel(.data)){
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
#' @details
#'   For signed networks, both measures consider only the positive ties.
#'   They count path lengths, and a negative tie is hostility rather
#'   than a channel along which cohesion travels.
#'   Use [manynet::to_unsigned()] first to control this yourself.
#'   Dropping the negative ties can disconnect the network,
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
  .data <- .to_aggregated_css(.data)
  object <- manynet::as_igraph(manynet::to_positive(.data))
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
  .data <- .to_aggregated_css(.data)
  object <- manynet::as_igraph(manynet::to_positive(.data))
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
#'   from the network needed to increase the number of components,
#'   or the minimum total weight of such ties in a weighted network.
#'   - `net_by_strength()` measures the number of ties that would need to be
#'   removed from a network to increase its number of components.
#'   - `net_by_robustness()` measures how slowly the network's largest component
#'   shrinks as its nodes are removed one at a time,
#'   either at random or in order of some node measure.
#'
#' @template param_data
#' @family cohesion
#' @template net_measure
#' @param limit The largest network `net_by_strength()` and
#'   `net_by_toughness()` will enumerate: the number of ties for the first and
#'   of nodes for the second. By default 20. Above this each stops rather than
#'   running for hours. Raise it to measure a larger network anyway.
#' @section Cost of the enumerated measures:
#'   `net_by_cohesion()` and `net_by_adhesion()` are connectivity problems that
#'   `{igraph}` solves directly, so they run on a network of any size.
#'   `net_by_strength()` and `net_by_toughness()` instead take a minimum over
#'   every subset of the tieset or the nodeset, which is \eqn{2^n} subsets.
#'   Their cost therefore quadruples for every two nodes added: a ring of 16
#'   nodes takes about 8 seconds, one of 20 over two minutes, and one of 30
#'   more than a day. `limit` stops each before it becomes a hang.
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
  .data <- .to_aggregated_css(.data)
  make_network_measure(igraph::cohesion(manynet::as_igraph(.data)),
                       .data, call = deparse(sys.call()),
                       measure = "node connectivity", range = c(0, Inf),
                       normalization = "none")
}

#' @rdname measure_fragmentation 
#' @importFrom igraph adhesion
#' @details
#'   For weighted networks, `net_by_adhesion()` reads a tie's weight as its capacity,
#'   and so returns the smallest total weight of ties whose removal would
#'   increase the number of components, the network's minimum cut,
#'   rather than the smallest number of such ties.
#'   This is also the smallest maximum flow between any two of its nodes;
#'   see [node_by_flow()] for the flow between particular nodes.
#'   Use [manynet::to_unweighted()] first to count ties instead.
#'   The other measures documented here count nodes or ties whatever
#'   their weight.
#'
#'   For signed networks, `net_by_adhesion()` considers only the positive
#'   ties, and says so.
#'   It asks what holds a network together,
#'   and a negative tie is hostility rather than a channel along which
#'   cohesion travels.
#'   Use [manynet::to_unsigned()] first to control this yourself.
#'   `net_by_robustness()` also considers only the positive ties.
#'   The other measures documented here use every tie whatever its sign.
#' @examples 
#' net_by_adhesion(fict_greys)
#' net_by_adhesion(to_giant(fict_greys))
#' @export
net_by_adhesion <- function(.data){
  .data <- manynet::expect_nodes(.data)
  .data <- .to_aggregated_css(.data)
  .data <- manynet::to_positive(.data)
  if(manynet::is_weighted(.data)){
    out <- igraph::min_cut(manynet::as_igraph(.data),
                           capacity = as.numeric(manynet::tie_weights(.data)))
    meas <- "weighted tie connectivity"
  } else {
    out <- igraph::adhesion(manynet::as_igraph(.data))
    meas <- "tie connectivity"
  }
  make_network_measure(out, .data, call = deparse(sys.call()),
                       measure = meas, range = c(0, Inf),
                       normalization = "none")
}

# Both of these take a minimum over every subset of the tieset or the nodeset,
# which is 2^n subsets, each of which is then counted for components. The run
# time quadruples for every two nodes added: a ring of 16 takes 8 seconds, one
# of 20 takes over two minutes, and one of 30 would take more than a day, while
# the subset list itself fails to allocate around 96. A user therefore meets a
# hang long before an error, so the size is checked first and named.
# See https://github.com/stocnet/netrics/issues/34.
.check_enumerable <- function(n, limit, fun, unit){
  if(n <= limit) return(invisible(n))
  manynet::snet_abort("{.fn {fun}} takes a minimum over every subset of the",
                      "{unit}, which doubles in cost with each one added.",
                      "This network has {n} {unit}, above the {.arg limit} of",
                      "{limit}, so the measure would not finish. Raise",
                      "{.arg limit} to try anyway, or measure a smaller",
                      "network, e.g. with {.fn to_giant} or {.fn to_ego}.")
}

#' @rdname measure_fragmentation 
#' @examples 
#' net_by_strength(ison_adolescents)
#' @export
net_by_strength <- function(.data, limit = 20){
  .data <- manynet::expect_nodes(.data)
  .data <- .to_aggregated_css(.data)
  n <- manynet::net_ties(.data)
  .check_enumerable(n, limit, "net_by_strength", "ties")
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
net_by_toughness <- function(.data, limit = 20){
  .data <- manynet::expect_nodes(.data)
  .data <- .to_aggregated_css(.data)
  n <- manynet::net_nodes(.data)
  .check_enumerable(n, limit, "net_by_toughness", "nodes")
  seties <- unlist(lapply(1:n, utils::combn, x = 1:n, simplify = FALSE), recursive = FALSE)
  out <- vapply(seties, function(x) length(x)/net_by_components(manynet::delete_nodes(.data, x)), 
                FUN.VALUE = numeric(1))
  make_network_measure(min(out), .data, call = deparse(sys.call()),
                       measure = "toughness", range = c(0, Inf),
                       normalization = "none")
}

#' @rdname measure_fragmentation
#' @param node_measure How the nodes to remove are chosen.
#'   By default `NULL`, which removes nodes at random,
#'   so that the network suffers failure rather than attack.
#'   Otherwise, at each step the node that [node_is_max()] marks is removed,
#'   choosing at random among any tied for the maximum.
#'   Give a `node_by_*()` function, such as `node_by_degree`,
#'   to recompute the measure on the network remaining after each removal,
#'   which is an adaptive attack.
#'   Give a `node_measure` object, such as `node_by_degree(.data)`,
#'   to remove nodes in the order of the scores the intact network gives them,
#'   which is a static attack.
#'   To pass further arguments, give a function of the network,
#'   e.g. `\(x) node_by_closeness(x, normalized = FALSE)`.
#' @template param_times
#' @section Robustness:
#'   `net_by_robustness()` removes the nodes of a network one at a time,
#'   recording after each removal the share of the original nodes that
#'   remain in the largest weak component.
#'   It returns the mean of these shares over all \eqn{N} removals,
#'   Schneider et al.'s \eqn{R}.
#'   A star attacked at its hub falls apart at once, scoring about \eqn{1/N},
#'   while a complete network loses only one node with each removal,
#'   scoring \eqn{(N-1)/2N}, which approaches the maximum of 0.5.
#'   Comparing a random failure against an attack shows, as Albert, Jeong and
#'   Barabási found, that a network with a few hubs tolerates failure
#'   but is fragile under attack.
#'
#'   The share remaining after each number of removals is kept as the
#'   `curve` attribute of the result, a table with columns
#'   `removed`, the share of the nodes removed, and `giant`,
#'   the share of the original nodes in the largest component,
#'   averaged over the `times` runs.
#'   Retrieve it with `attr(x, "curve")`.
#'
#'   Nodes are chosen at random wherever several are tied,
#'   so the result is averaged over `times` runs, by default 100.
#'   Where an attack meets no ties, every run would be the same,
#'   so it runs once.
#'   Each run computes the measure once per node removed,
#'   so an adaptive attack by an expensive measure, such as betweenness,
#'   on a network of several hundred nodes can take some minutes.
#'   Lower `times`, or attack with a `node_measure` object, to go faster.
#'
#'   For signed networks, 
#'   `net_by_robustness()` considers only the positive ties,
#'   since a negative tie does not hold a component together.
#' 
#'   For two-mode networks, 
#'   [node_is_max()] marks the highest-scoring node in each mode,
#'   so an attack on a two-mode network chooses at random between the two.
#' @references
#' ## On robustness
#' Albert, Réka, Hawoong Jeong, and Albert-László Barabási. 2000.
#' "Error and attack tolerance of complex networks."
#' _Nature_ 406: 378-82.
#' \doi{10.1038/35019019}
#'
#' Schneider, Christian M., André A. Moreira, José S. Andrade, Shlomo Havlin,
#' and Hans J. Herrmann. 2011.
#' "Mitigation of malicious attacks on networks."
#' _Proceedings of the National Academy of Sciences_ 108(10): 3838-41.
#' \doi{10.1073/pnas.1009440108}
#' @examples
#' sf <- generate_scalefree(50, 0.5)
#' net_by_robustness(sf)
#' net_by_robustness(sf, node_measure = node_by_degree)
#' @export
net_by_robustness <- function(.data, node_measure = NULL, times = 100){
  .data <- manynet::expect_nodes(.data)
  .data <- .to_aggregated_css(.data)
  .data <- manynet::to_positive(.data)
  if(!is.numeric(times) || length(times) != 1 || times < 1)
    manynet::snet_abort("{.arg times} must be a single positive number.")
  n <- manynet::net_nodes(.data)
  g <- manynet::as_igraph(.data)
  igraph::V(g)$.id <- seq_len(n)
  # Whether any choice was random, since where none was every run is the same.
  drawn <- FALSE
  pick_one <- function(cand){
    if(length(cand) > 1) drawn <<- TRUE
    cand[sample.int(length(cand), 1)]
  }
  if(is.null(node_measure)){
    attack <- "random failure"
    pick <- function(g) pick_one(seq_len(igraph::vcount(g)))
  } else if(is.function(node_measure)){
    attack <- "adaptive attack"
    pick <- function(g){
      scores <- node_measure(g)
      if(!inherits(scores, "node_measure"))
        manynet::snet_abort("{.arg node_measure} must return a {.cls node_measure},",
                            "as the {.fn node_by_*} functions do.")
      pick_one(which(node_is_max(scores)))
    }
  } else if(inherits(node_measure, "node_measure")){
    if(length(node_measure) != n)
      manynet::snet_abort("{.arg node_measure} holds {length(node_measure)}",
                          "scores, but the network has {n} nodes.")
    attack <- "static attack"
    pick <- function(g){
      alive <- igraph::V(g)$.id
      scores <- structure(as.numeric(node_measure)[alive],
                          mode = attr(node_measure, "mode")[alive],
                          class = class(node_measure))
      pick_one(which(node_is_max(scores)))
    }
  } else manynet::snet_abort("{.arg node_measure} must be NULL,",
                             "a {.fn node_by_*} function,",
                             "or a {.cls node_measure} object.")
  runs <- matrix(NA_real_, times, n + 1)
  for(i in seq_len(times)){
    runs[i,] <- .robustness_run(g, n, pick)
    if(!drawn){
      runs <- runs[i, , drop = FALSE]
      break
    }
  }
  curve <- colMeans(runs)
  out <- make_network_measure(mean(curve[-1]), .data,
                              call = deparse(sys.call()),
                              measure = paste("robustness to", attack),
                              range = c(0, 0.5),
                              normalization = "normalized")
  attr(out, "curve") <- dplyr::tibble(removed = (0:n)/n, giant = curve)
  out
}

# The share of the original n nodes in the largest weak component after each
# of 0 to n removals. Once no ties remain, every node is its own component, so
# the share is 1/n until the last node goes; this is filled in directly rather
# than asking a measure to rank nodes in an empty network.
.robustness_run <- function(g, n, pick){
  giant <- function(g) max(igraph::components(g, mode = "weak")$csize)/n
  out <- numeric(n + 1)
  out[1] <- giant(g)
  for(q in seq_len(n)){
    if(igraph::ecount(g) == 0){
      out[(q + 1):(n + 1)] <- c(rep(1/n, n - q), 0)
      break
    }
    g <- igraph::delete_vertices(g, pick(g))
    out[q + 1] <- if(igraph::vcount(g) > 0) giant(g) else 0
  }
  out
}

