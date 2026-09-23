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
  # Flattening to one mode gives every node a row and a column,
  # so that a two-mode network can be squared at all. It then scores 0, since
  # it contains no cycle of odd length, which is how `net_by_transitivity()`
  # already treats two modes. Squaring the raw incidence matrix instead
  # errored on uneven modes and returned a meaningless number on even ones.
  mat <- manynet::as_matrix(
    manynet::to_unweighted(manynet::to_onemode(.data)))
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
#' For one-mode networks, `node_by_reciprocity` is a shallow wrapper of the
#' igraph version, and `node_by_transitivity` offers the igraph version
#' alongside three weighted clustering coefficients.
#'
#' For two-mode networks, `node_by_equivalency` calculates the proportion of three-paths in the network
#' that are closed by fourth tie to establish a "shared four-cycle" structure.
#' @template param_data
#' @template node_measure
#' @template param_variant
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
  # Flattening to one mode squares the matrix, so a two-mode
  # network scores 1 throughout: every tie is trivially returned when there is
  # no direction to return along. That is what `net_by_reciprocity()` already
  # reports for any undirected network. Multiplying the raw incidence matrix
  # by its transpose instead errored on uneven modes and returned a
  # meaningless number on even ones.
  out <- manynet::as_matrix(
    manynet::to_unweighted(manynet::to_onemode(.data)))
  make_node_measure(rowSums(out * t(out))/rowSums(out),
                    .data, measure = "reciprocity", range = c(0, 1),
                    normalization = "normalized")
}

#' @rdname measure_closure_node
#' @section Node transitivity:
#'   A node's transitivity is the proportion of its neighbours that are
#'   themselves connected, which is also known as the _local clustering
#'   coefficient_ of the node.
#'   Where \eqn{a_{ij}} indicates a tie and \eqn{k_i} is the node's degree,
#'   the `"watts"` variant (Watts and Strogatz 1998) counts the node's
#'   closed triangles:
#'   \deqn{C_i = \frac{\sum_{j,h} a_{ij} a_{ih} a_{jh}}{k_i(k_i - 1)}}
#'
#'   The other variants weight each triangle by the ties' weights
#'   \eqn{w_{ij}}, and differ in which weights count:
#'
#'   - `"barrat"` (Barrat et al. 2004) weights each triangle by the mean
#'   weight of the node's own two ties in it, divided by the node's strength
#'   \eqn{s_i}. Only whether the tie opposite the node is present counts:
#'   \deqn{C_i = \frac{1}{s_i(k_i - 1)} \sum_{j,h} \frac{w_{ij} + w_{ih}}{2} a_{ij} a_{ih} a_{jh}}
#'   - `"onnela"` (Onnela et al. 2005) weights each triangle by the geometric
#'   mean of all three of its weights, each divided by the network's largest
#'   weight, so that a triangle closed by a weak tie counts for less:
#'   \deqn{C_i = \frac{1}{k_i(k_i - 1)} \sum_{j,h} (\hat{w}_{ij} \hat{w}_{ih} \hat{w}_{jh})^{1/3}}
#'   - `"zhang"` (Zhang and Horvath 2005) divides the product of the three
#'   weights by the most that the node's own weights allow:
#'   \deqn{C_i = \frac{\sum_{j,h} \hat{w}_{ij} \hat{w}_{ih} \hat{w}_{jh}}{(\sum_j \hat{w}_{ij})^2 - \sum_j \hat{w}_{ij}^2}}
#'
#'   Saramäki et al. (2007) compare the three.
#'   On an unweighted network, all four variants give the same values.
#'   The first choice, `"watts"`, is the default on an unweighted network only.
#'   On a weighted network, the default is `"barrat"`, since this is the
#'   weighted form that reduces most directly to the unweighted one;
#'   use `variant = "watts"` to ignore the weights.
#'   A node with fewer than two ties has no pair of neighbours to close,
#'   and scores `NaN` in every variant.
#'   A two-mode network contains no triangles, so every node scores 0 or `NaN`.
#' @section Directed networks:
#'   Transitivity here ignores the direction of ties.
#'   The weighted variants add the weights of the two directions of a tie,
#'   following Fagiolo (2007).
#'   Every weighted variant is unchanged when all weights are multiplied by the
#'   same number, so this gives the same result as their mean.
#'   To combine the two directions differently, use `manynet::to_undirected()`
#'   first.
#' @section Multiplex networks:
#'   The `"watts"` variant counts a pair of nodes as tied if they are tied in
#'   any layer.
#'   The weighted variants add the weights of the parallel ties between two
#'   nodes, so that, in an unweighted multiplex network, a tie is weighted by
#'   the number of layers it appears in.
#'   To measure one layer alone, use `manynet::to_uniplex()` first.
#' @section Signed networks:
#'   A tie counts here however it is signed, so each tie is read by the
#'   magnitude of its weight.
#'   To consider only the positive ties, use
#'   `manynet::to_unsigned(keep = "positive")` first.
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
#' ## On weighted clustering
#' Barrat, Alain, Marc Barthelemy, Romualdo Pastor-Satorras, and Alessandro Vespignani. 2004.
#' "The architecture of complex weighted networks".
#' _Proceedings of the National Academy of Sciences_ 101(11): 3747-3752.
#' \doi{10.1073/pnas.0400087101}
#'
#' Onnela, Jukka-Pekka, Jari Saramäki, János Kertész, and Kimmo Kaski. 2005.
#' "Intensity and coherence of motifs in weighted complex networks".
#' _Physical Review E_ 71(6): 065103.
#' \doi{10.1103/PhysRevE.71.065103}
#'
#' Zhang, Bin, and Steve Horvath. 2005.
#' "A general framework for weighted gene co-expression network analysis".
#' _Statistical Applications in Genetics and Molecular Biology_ 4(1): 17.
#' \doi{10.2202/1544-6115.1128}
#'
#' Saramäki, Jari, Mikko Kivelä, Jukka-Pekka Onnela, Kimmo Kaski, and János Kertész. 2007.
#' "Generalizations of the clustering coefficient to weighted complex networks".
#' _Physical Review E_ 75(2): 027105.
#' \doi{10.1103/PhysRevE.75.027105}
#'
#' Fagiolo, Giorgio. 2007.
#' "Clustering in complex directed networks".
#' _Physical Review E_ 76(2): 026107.
#' \doi{10.1103/PhysRevE.76.026107}
#' @examples
#' node_by_transitivity(ison_adolescents)
#' node_by_transitivity(ison_networkers)
#' node_by_transitivity(ison_networkers, variant = "onnela")
#' @export
node_by_transitivity <- function(.data,
                                 variant = c("watts", "barrat", "onnela", "zhang")) {
  .data <- manynet::expect_nodes(.data)
  # A tie closes a triangle however it is signed, as the unweighted count
  # already reads it, so a negative weight is read by its magnitude.
  .data <- .to_unsigned(.data)
  # The unweighted coefficient is the historical default, and stays so where
  # there are no weights to use. Where there are, Barrat's is the default,
  # since it reduces to the unweighted one when every weight is equal.
  if(missing(variant) && manynet::is_weighted(.data)){
    variant <- "barrat"
    manynet::snet_info("Using {.val barrat} weighted clustering;",
                       "use {.code variant = \"watts\"} to ignore weights.")
  }
  variant <- match.arg(variant)
  if(variant == "watts"){
    out <- igraph::transitivity(manynet::as_igraph(.data), type = "local")
  } else {
    # igraph's adjacency matrix is square for two-mode networks too, and adds
    # the weights of parallel ties, where `manynet::as_matrix()` returns a
    # list or missing values for some multiplex networks.
    g <- manynet::as_igraph(.data)
    W <- igraph::as_adjacency_matrix(
      g, attr = if(manynet::is_weighted(.data)) "weight" else NULL,
      sparse = FALSE)
    diag(W) <- 0
    # None of the three is defined for directed ties. Summing the two
    # directions follows Fagiolo (2007), and since every variant is unchanged
    # by a common rescaling of the weights, this is the same as their mean.
    if(manynet::is_directed(.data)) W <- W + t(W)
    A <- (W > 0) * 1
    k <- rowSums(A)
    # A node with fewer than two ties divides 0 by 0 in every variant below,
    # and so scores NaN, as igraph's unweighted coefficient does.
    out <- switch(variant,
                  barrat = rowSums(W * (A %*% A)) / (rowSums(W) * (k - 1)),
                  onnela = {
                    W3 <- (W / max(W))^(1/3)
                    diag(W3 %*% W3 %*% W3) / (k * (k - 1))
                  },
                  zhang = {
                    Wh <- W / max(W)
                    diag(Wh %*% Wh %*% Wh) / (rowSums(Wh)^2 - rowSums(Wh^2))
                  })
    out <- unname(out)
  }
  make_node_measure(out, .data,
                    measure = switch(variant,
                                     watts = "transitivity",
                                     barrat = "Barrat transitivity",
                                     onnela = "Onnela transitivity",
                                     zhang = "Zhang-Horvath transitivity"),
                    range = c(0, 1),
                    normalization = "normalized", variant = variant)
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

