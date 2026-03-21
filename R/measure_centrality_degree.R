# Degree-like centralities ####

#' Measuring nodes degree-like centrality
#' @name measure_central_degree
#' @description
#'   These functions calculate common degree-related centrality measures for one- and two-mode networks:
#'   
#'   - `node_by_degree()` measures the degree centrality of nodes in an unweighted network,
#'   or weighted degree/strength of nodes in a weighted network; 
#'   there are several related shortcut functions:
#'     - `node_by_deg()` returns the unnormalised results.
#'     - `node_by_indegree()` returns the `direction = 'in'` results.
#'     - `node_by_outdegree()` returns the `direction = 'out'` results.
#'   - `node_by_multidegree()` measures the ratio between types of ties in a multiplex network.
#'   - `node_by_posneg()` measures the PN (positive-negative) centrality of a signed network.
#'   - `node_by_leverage()` measures the leverage centrality of nodes in a network.
#'   
#'   All measures attempt to use as much information as they are offered,
#'   including whether the networks are directed, weighted, or multimodal.
#'   If this would produce unintended results, 
#'   first transform the salient properties using e.g. [manynet::to_undirected()] functions.
#'   All centrality and centralization measures return normalized measures by default,
#'   including for two-mode networks.
#' @template param_data
#' @template param_norm
#' @template param_dir
#' @family degree
#' @family centrality
#' @template node_measure
#' @param alpha Numeric scalar, the positive tuning parameter introduced in
#'   Opsahl et al (2010) for trading off between degree and strength centrality measures.
#'   By default, `alpha = 0`, which ignores tie weights and the measure is solely based
#'   upon degree (the number of ties).
#'   `alpha = 1` ignores the number of ties and provides the sum of the tie weights 
#'   as strength centrality.
#'   Values between 0 and 1 reflect different trade-offs in the relative contributions of
#'   degree and strength to the final outcome, with 0.5 as the middle ground.
#'   Values above 1 penalise for the number of ties.
#'   Of two nodes with the same sum of tie weights, the node with fewer ties will obtain
#'   the higher score.
#'   This argument is ignored except in the case of a weighted network.
#' @importFrom igraph graph_from_incidence_matrix is_bipartite degree V
#' @references 
#' ## On multimodal centrality
#' Faust, Katherine. 1997. 
#' "Centrality in affiliation networks." 
#' _Social Networks_ 19(2): 157-191.
#' \doi{10.1016/S0378-8733(96)00300-0}
#' 
#' Borgatti, Stephen P., and Martin G. Everett. 1997. 
#' "Network analysis of 2-mode data." 
#' _Social Networks_ 19(3): 243-270.
#' \doi{10.1016/S0378-8733(96)00301-2}
#' 
#' Borgatti, Stephen P., and Daniel S. Halgin. 2011. 
#' "Analyzing affiliation networks." 
#' In _The SAGE Handbook of Social Network Analysis_, 
#' edited by John Scott and Peter J. Carrington, 417–33. 
#' London, UK: Sage.
#' \doi{10.4135/9781446294413.n28}
#' 
#' ## On strength centrality
#' Opsahl, Tore, Filip Agneessens, and John Skvoretz. 2010. 
#' "Node centrality in weighted networks: Generalizing degree and shortest paths." 
#' _Social Networks_ 32, 245-251.
#' \doi{10.1016/j.socnet.2010.03.006}
#' @examples
#' node_by_degree(ison_southern_women)
NULL

#' @rdname measure_central_degree 
#' @section Degree centrality: 
#'   `r manynet:::glossies$degree`
#'   It is also sometimes called the valency of a node, \eqn{d(v)}.
#'   The maximum degree in a network is often denoted \eqn{\Delta (G)} and
#'   the minimum degree in a network \eqn{\delta (G)}.
#'   The total degree of a network is the sum of all degrees, \eqn{\sum_v d(v)}.
#'   The degree sequence is the set of all nodes' degrees,
#'   ordered from largest to smallest.
#'   Directed networks discriminate between 
#'   outdegree (degree of outgoing ties) and
#'   indegree (degree of incoming ties).
#' @importFrom manynet as_igraph is_weighted tie_weights is_twomode is_complex
#' @export
node_by_degree <- function (.data, normalized = TRUE, alpha = 1,
                         direction = c("all","out","in")){
  .data <- manynet::expect_nodes(.data)
  graph <- manynet::as_igraph(.data)
  weights <- `if`(manynet::is_weighted(.data), 
                  manynet::tie_weights(.data), NA)
  direction <- match.arg(direction)
  
  # Do the calculations
  if (manynet::is_twomode(graph) & normalized){
    degrees <- igraph::degree(graph = graph, 
                              v = igraph::V(graph), 
                              mode = direction, 
                              loops = manynet::is_complex(.data))
    other_set_size <- ifelse(igraph::V(graph)$type, 
                             sum(!igraph::V(graph)$type), 
                             sum(igraph::V(graph)$type))
    out <- degrees/other_set_size
  } else {
    if (all(is.na(weights))) {
      out <- igraph::degree(graph = graph, v = igraph::V(graph), 
                     mode = direction, 
                     loops = manynet::is_complex(.data),
                     normalized = normalized)
    }
    else {
      ki <- igraph::degree(graph = graph, v = igraph::V(graph), 
                     mode = direction, 
                     loops = manynet::is_complex(.data))
      si <- igraph::strength(graph = graph, vids = igraph::V(graph), 
                       mode = direction,
                       loops = manynet::is_complex(.data), weights = weights)
      out <- ki * (si/ki)^alpha
      out[is.nan(out)] <- 0
      if(normalized) out <- out/max(out)
    }
  }
  out <- make_node_measure(out, .data)
  out
}

#' @rdname measure_central_degree
#' @export
node_by_deg <- function (.data, alpha = 0, direction = c("all","out","in")){
  .data <- manynet::expect_nodes(.data)
  node_by_degree(.data, normalized = FALSE, alpha = alpha, direction = direction)
}

#' @rdname measure_central_degree
#' @export
node_by_outdegree <- function (.data, normalized = TRUE, alpha = 0){
  .data <- manynet::expect_nodes(.data)
  node_by_degree(.data, normalized = normalized, alpha = alpha, direction = "out")
}

#' @rdname measure_central_degree
#' @export
node_by_indegree <- function (.data, normalized = TRUE, alpha = 0){
  .data <- manynet::expect_nodes(.data)
  node_by_degree(.data, normalized = normalized, alpha = alpha, direction = "in")
}

#' @rdname measure_central_degree
#' @param tie1 Character string indicating the first uniplex network.
#' @param tie2 Character string indicating the second uniplex network.
#' @export
node_by_multidegree <- function (.data, tie1, tie2){
  .data <- manynet::expect_nodes(.data)
  stopifnot(manynet::is_multiplex(.data))
  out <- node_by_degree(manynet::to_uniplex(.data, tie1)) - 
    node_by_degree(manynet::to_uniplex(.data, tie2))
  make_node_measure(out, .data)
}

#' @rdname measure_central_degree
#' @references
#' ## On signed centrality
#' Everett, Martin G., and Stephen P. Borgatti. 2014. 
#' “Networks Containing Negative Ties.” 
#' _Social Networks_ 38:111–20. 
#' \doi{10.1016/j.socnet.2014.03.005}
#' @export
node_by_posneg <- function(.data){
  .data <- manynet::expect_nodes(.data)
  stopifnot(manynet::is_signed(.data))
  pos <- manynet::as_matrix(manynet::to_unsigned(.data, keep = "positive"))
  neg <- manynet::as_matrix(manynet::to_unsigned(.data, keep = "negative"))
  nn <- manynet::net_nodes(.data)
  pn <- pos-neg*2
  diag(pn) <- 0
  idmat <- diag(nn)
  v1 <- matrix(1,nn,1)
  out <- solve(idmat - ((pn%*%t(pn))/(4*(nn-1)^2))) %*% (idmat+( pn/(2*(nn-1)) )) %*% v1
  make_node_measure(out, .data)
}

#' @rdname measure_central_degree
#' @section Leverage centrality: 
#'   Leverage centrality concerns the degree of a node compared with that of its
#'   neighbours, \eqn{J}:
#'   \deqn{C_L(i) = \frac{1}{d(i)} \sum_{j \in J(i)} \frac{d(i) - d(j)}{d(i) + d(j)}}
#' @references
#' ## On leverage centrality
#' Joyce, Karen E., Paul J. Laurienti, Jonathan H. Burdette, and Satoru Hayasaka. 2010.
#' "A New Measure of Centrality for Brain Networks". 
#' _PLoS ONE_ 5(8): e12200.
#' \doi{10.1371/journal.pone.0012200}
#' @export
node_by_leverage <- function(.data){
  .data <- manynet::expect_nodes(.data)
  out <- (node_by_deg(.data) - node_by_neighbours_degree(.data))/
    (node_by_deg(.data) + node_by_neighbours_degree(.data))
  make_node_measure(out, .data)
}

# Degree-like centralities ####

#' Measuring ties degree-like centrality
#' @name measure_centralities_degree
#' @description
#'   `tie_by_degree()` measures the degree centrality of ties in a network
#'   
#'   All measures attempt to use as much information as they are offered,
#'   including whether the networks are directed, weighted, or multimodal.
#'   If this would produce unintended results, 
#'   first transform the salient properties using e.g. [to_undirected()] functions.
#'   All centrality and centralization measures return normalized measures 
#'   by default, including for two-mode networks.
#' @template param_data
#' @template param_norm
#' @family degree
#' @family centrality
#' @template tie_measure
NULL

#' @rdname measure_centralities_degree
#' @examples 
#' tie_by_degree(ison_adolescents)
#' @export
tie_by_degree <- function(.data, normalized = TRUE){
  .data <- manynet::expect_ties(.data)
  edge_adj <- manynet::to_ties(.data)
  out <- node_by_degree(edge_adj, normalized = normalized)
  class(out) <- "numeric"
  make_tie_measure(out, .data)
}

# Degree centralisation ####

#' Measuring networks degree-like centralisation
#' @name measure_centralisation_degree
#' @description
#'   - `net_by_degree()` measures a network's degree centralization; 
#'   there are several related shortcut functions:
#'     - `net_by_indegree()` returns the `direction = 'in'` results.
#'     - `net_by_outdegree()` returns the `direction = 'out'` results.
#'   
#'   All measures attempt to use as much information as they are offered,
#'   including whether the networks are directed, weighted, or multimodal.
#'   If this would produce unintended results, 
#'   first transform the salient properties using e.g. [to_undirected()] functions.
#'   All centrality and centralization measures return normalized measures 
#'   by default, including for two-mode networks.
#'   
#'   For two-mode networks, "all" uses as numerator the sum of differences
#'   between the maximum centrality score for the mode 
#'   against all other centrality scores in the network,
#'   whereas "in" uses as numerator the sum of differences
#'   between the maximum centrality score for the mode 
#'   against only the centrality scores of the other nodes in that mode.
#' @template param_data
#' @template param_norm
#' @template param_dir
#' @family degree
#' @family centrality
#' @template net_measure
NULL

#' @rdname measure_centralisation_degree
#' @examples
#' net_by_degree(ison_southern_women, direction = "in")
#' @export
net_by_degree <- function(.data, normalized = TRUE,
                           direction = c("all", "out", "in")){
  
  .data <- manynet::expect_nodes(.data)
  direction <- match.arg(direction)
  
  if (manynet::is_twomode(.data)) {
    mat <- manynet::as_matrix(.data)
    mode <- c(rep(FALSE, nrow(mat)), rep(TRUE, ncol(mat)))
    
    out <- list()
    if (direction == "all") {
      if (!normalized) {
        allcent <- c(rowSums(mat), colSums(mat))
        out$nodes1 <- sum(max(allcent[!mode]) - allcent)/((nrow(mat) + ncol(mat))*ncol(mat) - 2*(ncol(mat) + nrow(mat) - 1))
        out$nodes2 <- sum(max(allcent[mode]) - allcent)/((nrow(mat) + ncol(mat))*nrow(mat) - 2*(ncol(mat) + nrow(mat) - 1))
      } else if (normalized) {
        allcent <- node_by_degree(.data, normalized = TRUE)
        out$nodes1 <- sum(max(allcent[!mode]) - allcent)/((nrow(mat) + ncol(mat) - 1) - (ncol(mat) - 1) / nrow(mat) - (ncol(mat) + nrow(mat) - 1)/nrow(mat))
        out$nodes2 <- sum(max(allcent[mode]) - allcent)/((ncol(mat) + nrow(mat) - 1) - (nrow(mat) - 1) / ncol(mat) - (nrow(mat)  + ncol(mat) - 1)/ncol(mat))
      }
    } else if (direction == "in" | direction == "out") {
      out$nodes1 <- sum(max(rowSums(mat)) - rowSums(mat))/((ncol(mat) - 1)*(nrow(mat) - 1))
      out$nodes2 <- sum(max(colSums(mat)) - colSums(mat))/((ncol(mat) - 1)*(nrow(mat) - 1))
    }
    out <- c("Mode 1" = out$nodes1, "Mode 2" = out$nodes2)
  } else {
    out <- igraph::centr_degree(graph = .data, mode = direction, 
                                normalized = normalized)$centralization
  }
  out <- make_network_measure(out, .data, call = deparse(sys.call()))
  out
}

#' @rdname measure_centralisation_degree
#' @export
net_by_outdegree <- function(.data, normalized = TRUE){
  .data <- manynet::expect_nodes(.data)
  net_by_degree(.data, normalized = normalized, direction = "out")
}

#' @rdname measure_centralisation_degree
#' @export
net_by_indegree <- function(.data, normalized = TRUE){
  .data <- manynet::expect_nodes(.data)
  net_by_degree(.data, normalized = normalized, direction = "in")
}

