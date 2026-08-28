# Network closure ####

#' Measuring network closure
#' @name measure_closure
#' @description
#'   These functions offer methods for summarising the closure in configurations 
#'   in one-, two-, and three-mode networks:
#'   
#'   - `net_by_reciprocity()` measures reciprocity in a (usually directed) network.
#'   - `net_by_transitivity()` measures transitivity in a network.
#'   - `net_by_cyclicality()` measures cyclicality in a (necessarily directed) network.
#'   - `net_by_equivalency()` measures equivalence or reinforcement 
#'   in a (usually two-mode) network.
#'   - `net_by_congruency()` measures congruency across two two-mode networks.
#'   
#' @details 
#' For one-mode networks, shallow wrappers of igraph versions exist via 
#' `net_reciprocity` and `net_transitivity`.
#' 
#' For two-mode networks, `net_equivalency` calculates the proportion of three-paths in the network
#' that are closed by fourth tie to establish a "shared four-cycle" structure.
#' 
#' For three-mode networks, `net_congruency` calculates the proportion of three-paths 
#' spanning two two-mode networks that are closed by a fourth tie to establish a 
#' "congruent four-cycle" structure.
#' 
#' `net_by_reciprocity()` takes a `variant`: either `"default"`, the share of
#' ties that are reciprocated, or `"ratio"`, the share of dyads that are mutual
#' rather than asymmetric. See `?igraph::reciprocity`.
#' @template param_data
#' @template net_measure
#' @param object2 Optionally, a second (two-mode) matrix, igraph, or tidygraph
#' @template param_variant
#' @param method Deprecated. The former spelling of `variant`.
#'   Still accepted, but warns; please use `variant` instead.
NULL

#' @rdname measure_closure 
#' @importFrom igraph reciprocity
#' @examples
#' net_by_reciprocity(ison_southern_women)
#' @export
net_by_reciprocity <- function(.data, variant = c("default", "ratio"),
                               method = NULL) {
  variant <- resolve_method(variant, method, "variant")
  .data <- manynet::expect_nodes(.data)
  variant <- match.arg(variant, c("default", "ratio"))
  # Both methods return a proportion in [0,1], but of different things: the
  # default is the share of ties that are reciprocated, the ratio the share of
  # dyads that are mutual rather than asymmetric. The variant says which.
  make_network_measure(igraph::reciprocity(manynet::as_igraph(.data),
                                           mode = variant),
                       .data, call = deparse(sys.call()),
                       measure = "reciprocity", range = c(0, 1),
                       normalization = "normalized", variant = variant)
}

#' @rdname measure_closure 
#' @importFrom igraph transitivity
#' @examples
#' net_by_transitivity(ison_adolescents)
#' @export
net_by_transitivity <- function(.data) {
  .data <- manynet::expect_nodes(.data)
  make_network_measure(igraph::transitivity(manynet::as_igraph(.data)),
                       .data, call = deparse(sys.call()),
                       measure = "transitivity", range = c(0, 1),
                       normalization = "normalized")
}

#' @rdname measure_closure
#' @section Cyclicality:
#'   Where transitivity asks how often a two-path \eqn{i \to j \to k} is closed
#'   by a tie \eqn{i \to k}, cyclicality asks how often it is closed in the
#'   other direction, by \eqn{k \to i}:
#'   \deqn{C = \frac{|\{i \to j \to k \to i\}|}{|\{i \to j \to k\}|}}
#'   The two capture different social logics. Transitivity is the signature of
#'   hierarchy and of "a friend of a friend is a friend", while cyclicality is
#'   the signature of generalised exchange, where resources circulate around a
#'   loop rather than flowing consistently in one direction.
#'
#'   A two-mode network contains no cycle of odd length, so it scores 0 here,
#'   just as it does for transitivity. Use `net_by_equivalency()` for closure
#'   in a two-mode network, which counts four-cycles instead.
#'
#'   In an undirected network every two-path closed in one direction is also
#'   closed in the other, so cyclicality and transitivity coincide.
#' @references
#' ## On cyclicality and generalised exchange
#' Bearman, Peter. 1997.
#' "Generalized Exchange".
#' _American Journal of Sociology_ 102(5): 1383-1415.
#' \doi{10.1086/231087}
#' @examples
#' net_by_cyclicality(ison_networkers)
#' @export
net_by_cyclicality <- function(.data) {
  .data <- manynet::expect_nodes(.data)
  # Flattening to a multilevel network gives every node a row and a column,
  # so that a two-mode network can be squared at all. It then scores 0, since
  # it contains no cycle of odd length, which is how `net_by_transitivity()`
  # already treats two modes. Squaring the raw incidence matrix instead
  # errored on uneven modes and returned a meaningless number on even ones.
  mat <- manynet::as_matrix(
    manynet::to_unweighted(manynet::to_multilevel(.data)))
  diag(mat) <- 0
  twopaths <- mat %*% mat
  diag(twopaths) <- 0 # i -> j -> i is not a two-path
  denom <- sum(twopaths)
  # closed cyclically where a tie runs back from k to i
  out <- if(denom == 0) NaN else sum(twopaths * t(mat))/denom
  make_network_measure(out, .data, call = deparse(sys.call()),
                       measure = "cyclicality", range = c(0, 1),
                       normalization = "normalized")
}

#' @rdname measure_closure
#' @section Equivalency:
#'   The `net_by_equivalency()` function calculates the Robins and Alexander (2004)
#'   clustering coefficient for two-mode networks.
#'   The coefficient is a proportion of three-paths, and so is defined on
#'   binary data; weighted networks are dichotomised before it is calculated.
#' @references 
#' ## On equivalency or four-cycles
#' Robins, Garry L, and Malcolm Alexander. 2004. 
#' Small worlds among interlocking directors: Network structure and distance in bipartite graphs. 
#' \emph{Computational & Mathematical Organization Theory} 10(1): 69–94.
#' \doi{10.1023/B:CMOT.0000032580.12184.c0}.
#' @examples
#' net_by_equivalency(ison_southern_women)
#' @export
net_by_equivalency <- function(.data) {
  .data <- manynet::expect_nodes(.data)
  if(manynet::is_weighted(.data))
    manynet::snet_info("Using the unweighted form of the network.")
  if(manynet::is_twomode(.data)){
    mat <- manynet::as_matrix(manynet::to_unweighted(.data))
    c <- ncol(mat)
    indegrees <- colSums(mat)
    twopaths <- crossprod(mat)
    diag(twopaths) <- 0
    out <- sum(twopaths * (twopaths - 1)) /
      (sum(twopaths * (twopaths - 1)) +
         sum(twopaths *
               (matrix(indegrees, c, c) - twopaths)))
    if (is.nan(out)) out <- 1
  } else {
    out <- rowSums(vapply(manynet::snet_progress_nodes(.data), function(i){
      threepaths <- igraph::all_simple_paths(.data, i, cutoff = 3,
                                             mode = "all")
      onepaths <- threepaths[vapply(threepaths, length,
                                    FUN.VALUE = numeric(1))==2]
      threepaths <- threepaths[vapply(threepaths, length,
                                      FUN.VALUE = numeric(1))==4]
      c(sum(sapply(threepaths,"[[",4) %in% sapply(onepaths,"[[",2)),
        length(threepaths))
    }, FUN.VALUE = numeric(2)))
    out <- out[1]/out[2]
  }
  make_network_measure(out, .data, call = deparse(sys.call()),
                       measure = "equivalency", range = c(0, 1),
                       normalization = "normalized")
}

#' @rdname measure_closure 
#' @references 
#' ## On congruency
#' Knoke, David, Mario Diani, James Hollway, and Dimitris C Christopoulos. 2021. 
#' \emph{Multimodal Political Networks}. 
#' Cambridge University Press. Cambridge University Press.
#' \doi{10.1017/9781108985000}
#' @export
net_by_congruency <- function(.data, object2){
  .data <- manynet::expect_nodes(.data)
  if(missing(.data) | missing(object2)) 
    manynet::snet_abort("This function expects two two-mode networks")
  if(!manynet::is_twomode(.data) | !manynet::is_twomode(object2)) 
    manynet::snet_abort("This function expects two two-mode networks")
  if(manynet::net_dims(.data)[2] != manynet::net_dims(object2)[1]) 
    manynet::snet_abort(paste("This function expects the number of nodes",
                              "in the second mode of the first network", "to be the same as the number of nodes",
                              "in the first mode of the second network."))
  mat1 <- manynet::as_matrix(.data)
  mat2 <- manynet::as_matrix(object2)
  connects <- ncol(mat1)
  twopaths1 <- crossprod(mat1)
  indegrees <- diag(twopaths1)
  diag(twopaths1) <- 0
  twopaths2 <- tcrossprod(mat2)
  outdegrees <- diag(twopaths2)
  diag(twopaths2) <- 0
  twopaths <- twopaths1 + twopaths2
  degrees <- indegrees + outdegrees
  output <- sum(twopaths * (twopaths - 1)) /
    (sum(twopaths * (twopaths - 1)) +
       sum(twopaths *
             (matrix(degrees, connects, connects) - twopaths)))
  if (is.nan(output)) output <- 1
  make_network_measure(output, .data, call = deparse(sys.call()),
                       measure = "congruency", range = c(0, 1),
                       normalization = "normalized")
}

# Nodal closure ####

#' Measuring node closure
#' @name measure_closure_node
#' @description
#'   These functions offer methods for summarising the closure in configurations 
#'   in one- and two-mode networks:
#'   
#'   - `node_by_reciprocity()` measures nodes' reciprocity.
#'   - `node_by_transitivity()` measures nodes' transitivity.
#'   - `node_by_equivalency()` measures nodes' equivalence or reinforcement 
#'   in a (usually two-mode) network.
#'   
#' @details 
#' For one-mode networks, shallow wrappers of igraph versions exist via 
#' `node_by_reciprocity` and `node_by_transitivity`.
#' 
#' For two-mode networks, `node_by_equivalency` calculates the proportion of three-paths in the network
#' that are closed by fourth tie to establish a "shared four-cycle" structure.
#' @template param_data
#' @template node_measure
NULL

#' @rdname measure_closure_node 
#' @section Node reciprocity:
#'   A node's reciprocity is the proportion of its ties that are returned.
#'   Where a network is undirected, including where it is two-mode, there is
#'   no direction for a tie to be returned along, so every node scores 1.
#'   This is what `net_by_reciprocity()` reports for such a network too.
#' @examples
#' node_by_reciprocity(ison_networkers)
#' @export
node_by_reciprocity <- function(.data) {
  .data <- manynet::expect_nodes(.data)
  if(manynet::is_weighted(.data))
    manynet::snet_info("Using the unweighted form of the network.")
  # A proportion of a node's ties that are returned, so counts of ties rather
  # than sums of weights: otherwise a reciprocated tie of weight 3 scores 3.
  # Flattening to a multilevel network squares the matrix, so a two-mode
  # network scores 1 throughout: every tie is trivially returned when there is
  # no direction to return along. That is what `net_by_reciprocity()` already
  # reports for any undirected network. Multiplying the raw incidence matrix
  # by its transpose instead errored on uneven modes and returned a
  # meaningless number on even ones.
  out <- manynet::as_matrix(
    manynet::to_unweighted(manynet::to_multilevel(.data)))
  make_node_measure(rowSums(out * t(out))/rowSums(out),
                    .data, measure = "reciprocity", range = c(0, 1),
                    normalization = "normalized")
}

#' @rdname measure_closure_node
#' @section Node transitivity:
#'   A node's transitivity is the proportion of its neighbours that are
#'   themselves connected, which is also known as the _local clustering
#'   coefficient_ of the node.
#' @references
#' ## On the local clustering coefficient
#' Watts, Duncan J., and Steven H. Strogatz. 1998.
#' "Collective dynamics of 'small-world' networks".
#' _Nature_ 393(6684): 440-442.
#' \doi{10.1038/30918}
#'
#' Holland, Paul W., and Samuel Leinhardt. 1971.
#' "Transitivity in structural models of small groups".
#' _Comparative Group Studies_ 2(2): 107-124.
#' \doi{10.1177/104649647100200201}
#' @examples
#' node_by_transitivity(ison_adolescents)
#' @export
node_by_transitivity <- function(.data) {
  .data <- manynet::expect_nodes(.data)
  make_node_measure(igraph::transitivity(manynet::as_igraph(.data), 
                                         type = "local"),
                    .data, measure = "transitivity", range = c(0, 1),
                    normalization = "normalized")
}

#' @rdname measure_closure_node
#' @export
node_by_equivalency <- function(.data) {
  .data <- manynet::expect_nodes(.data)
  if(manynet::is_weighted(.data))
    manynet::snet_info("Using the unweighted form of the network.")
  out <- vapply(manynet::snet_progress_nodes(.data), function(i){
    threepaths <- igraph::all_simple_paths(.data, i, cutoff = 3,
                                          mode = "all")
    onepaths <- threepaths[vapply(threepaths, length, 
                                  FUN.VALUE = numeric(1))==2]
    threepaths <- threepaths[vapply(threepaths, length, 
                                    FUN.VALUE = numeric(1))==4]
    mean(sapply(threepaths,"[[",4) %in% sapply(onepaths,"[[",2))
  }, FUN.VALUE = numeric(1))
  if (any(is.nan(out))) out[is.nan(out)] <- 0
  make_node_measure(out, .data, measure = "equivalency", range = c(0, 1),
                    normalization = "normalized")
}

