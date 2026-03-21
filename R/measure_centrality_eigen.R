# Eigenvector-like centralities ####

#' Measuring nodes eigenvector-like centrality
#' @name measure_central_eigen
#' @description
#'   These functions calculate common eigenvector-related centrality 
#'   measures, or walk-based eigenmeasures, for one- and two-mode networks:
#'   
#'   - `node_eigenvector()` measures the eigenvector centrality of nodes 
#'   in a network.
#'   - `node_power()` measures the Bonacich, beta, or power centrality of 
#'   nodes in a network.
#'   - `node_alpha()` measures the alpha or Katz centrality of nodes in a 
#'   network.
#'   - `node_pagerank()` measures the pagerank centrality of nodes in a network.
#'   - `node_hub()` measures how well nodes in a network serve as hubs pointing 
#'   to many authorities.
#'   - `node_authority()` measures how well nodes in a network serve as 
#'   authorities from many hubs.
#'   
#'   All measures attempt to use as much information as they are offered,
#'   including whether the networks are directed, weighted, or multimodal.
#'   If this would produce unintended results, 
#'   first transform the salient properties using e.g. [to_undirected()] functions.
#'   All centrality and centralization measures return normalized measures 
#'   by default, including for two-mode networks.
#' @template param_data
#' @template param_norm
#' @family eigenvector
#' @family centrality
#' @template node_measure
NULL

#' @rdname measure_central_eigen
#' @section Eigenvector centrality:
#'   Eigenvector centrality operates as a measure of a node's influence in a network.
#'   The idea is that being connected to well-connected others results in a higher score.
#'   Each node's eigenvector centrality can be defined as:
#'   \deqn{x_i = \frac{1}{\lambda} \sum_{j \in N} a_{i,j} x_j}
#'   where \eqn{a_{i,j} = 1} if \eqn{i} is linked to \eqn{j} and 0 otherwise,
#'   and \eqn{\lambda} is a constant representing the principal eigenvalue.
#'   Rather than performing this iteration,
#'   most routines solve the eigenvector equation \eqn{Ax = \lambda x}.
#'   Note that since `{igraph}` v2.1.1,
#'   the values will always be rescaled so that the maximum is 1.
#' @param scale Logical scalar, whether to rescale the vector so the maximum score is 1. 
#' @details
#'   We use `{igraph}` routines behind the scenes here for consistency and because they are often faster.
#'   For example, `igraph::eigencentrality()` is approximately 25% faster than `sna::evcent()`.
#' @references 
#'   ## On eigenvector centrality
#'   Bonacich, Phillip. 1991. 
#'   “Simultaneous Group and Individual Centralities.” 
#'   _Social Networks_ 13(2):155–68. 
#'   \doi{10.1016/0378-8733(91)90018-O}
#' @examples
#' node_by_eigenvector(ison_southern_women)
#' @export 
node_by_eigenvector <- function(.data, normalized = TRUE, scale = TRUE){
  
  .data <- manynet::expect_nodes(.data)
  weights <- `if`(manynet::is_weighted(.data), 
                  manynet::tie_weights(.data), NA)
  graph <- manynet::as_igraph(.data)
  
  if(!normalized) manynet::snet_info("This function always returns a normalized value now.")
  if(!scale) manynet::snet_info("This function always returns a scaled value now.")
  
  if(!manynet::is_connected(.data)) 
    manynet::snet_warn("Unconnected networks will only allow nodes from one component to have non-zero eigenvector scores.")
  
  # Do the calculations
  if (!manynet::is_twomode(graph)){
    out <- igraph::eigen_centrality(graph = graph, 
                                    directed = manynet::is_directed(graph),
                                    options = igraph::arpack_defaults())$vector
  } else {
    eigen1 <- manynet::to_mode1(graph)
    eigen1 <- igraph::eigen_centrality(graph = eigen1, 
                                       directed = manynet::is_directed(eigen1),
                                       options = igraph::arpack_defaults())$vector
    eigen2 <- manynet::to_mode2(graph)
    eigen2 <- igraph::eigen_centrality(graph = eigen2, 
                                       directed = manynet::is_directed(eigen2),
                                       options = igraph::arpack_defaults())$vector
    out <- c(eigen1, eigen2)
  }
  out <- make_node_measure(out, .data)
  out
}

#' @rdname measure_central_eigen
#' @param exponent Decay rate or attentuation factor for 
#'   the Bonacich power centrality score.
#'   Can be positive or negative.
#' @section Power or beta (or Bonacich) centrality:
#'   Power centrality includes an exponent that weights contributions to a node's
#'   centrality based on how far away those other nodes are.
#'   \deqn{c_b(i) = \sum A(i,j) (\alpha = \beta c(j))}
#'   Where \eqn{\beta} is positive, this means being connected to central people
#'   increases centrality.
#'   Where \eqn{\beta} is negative, this means being connected to central people
#'   decreases centrality 
#'   (and being connected to more peripheral actors increases centrality).
#'   When \eqn{\beta = 0}, this is the outdegree.
#'   \eqn{\alpha} is calculated to make sure the root mean square equals 
#'   the network size.
#' @references 
#' ## On power centrality
#'   Bonacich, Phillip. 1987. 
#'   “Power and Centrality: A Family of Measures.” 
#'   _The American Journal of Sociology_, 92(5): 1170–82.
#' \doi{10.1086/228631}.
#' @importFrom igraph power_centrality
#' @examples
#' node_by_power(ison_southern_women, exponent = 0.5)
#' @export 
node_by_power <- function(.data, normalized = TRUE, scale = FALSE, exponent = 1){
  
  .data <- manynet::expect_nodes(.data)
  weights <- `if`(manynet::is_weighted(.data), 
                  manynet::tie_weights(.data), NA)
  graph <- manynet::as_igraph(.data)
  
  if(var(node_by_deg(graph))==0){
    manynet::snet_minor_info("All nodes have the same degree, so power centrality equals degree centrality.")
    exponent <- 0
  }
  
  # Do the calculations
  if (!manynet::is_twomode(graph)){
    out <- igraph::power_centrality(graph = graph, 
                                    exponent = exponent,
                                    rescale = scale)
    if (normalized) out <- out / sqrt(1/2)
  } else {
    eigen1 <- manynet::to_mode1(graph)
    eigen1 <- igraph::power_centrality(graph = eigen1, 
                                       exponent = exponent,
                                       rescale = scale)
    eigen2 <- manynet::to_mode2(graph)
    eigen2 <- igraph::power_centrality(graph = eigen2, 
                                       exponent = exponent,
                                       rescale = scale)
    out <- c(eigen1, eigen2)
    if (normalized) out <- out / sqrt(1/2)
  }
  out <- make_node_measure(out, .data)
  out
}

#' @rdname measure_central_eigen 
#' @param alpha A constant that trades off the importance of external influence against the importance of connection.
#'   When \eqn{\alpha = 0}, only the external influence matters.
#'   As \eqn{\alpha} gets larger, only the connectivity matters and we reduce to eigenvector centrality.
#'   By default \eqn{\alpha = 0.85}.
#' @section Alpha centrality:
#'   Alpha or Katz (or Katz-Bonacich) centrality operates better than 
#'   eigenvector centrality for directed networks because eigenvector centrality 
#'   will return 0s for all nodes not in the main strongly-connected component.
#'   Each node's alpha centrality can be defined as:
#'   \deqn{x_i = \frac{1}{\lambda} \sum_{j \in N} a_{i,j} x_j + e_i}
#'   where \eqn{a_{i,j} = 1} if \eqn{i} is linked to \eqn{j} and 0 otherwise,
#'   \eqn{\lambda} is a constant representing the principal eigenvalue,
#'   and \eqn{e_i} is some external influence used to ensure that even nodes beyond the main
#'   strongly connected component begin with some basic influence.
#'   Note that many equations replace \eqn{\frac{1}{\lambda}} with \eqn{\alpha},
#'   hence the name.
#'
#'   For example, if \eqn{\alpha = 0.5}, then each direct connection (or alter) would be worth \eqn{(0.5)^1 = 0.5},
#'   each secondary connection (or tertius) would be worth \eqn{(0.5)^2 = 0.25},
#'   each tertiary connection would be worth \eqn{(0.5)^3 = 0.125}, and so on.
#'
#'   Rather than performing this iteration though,
#'   most routines solve the equation \eqn{x = (I - \frac{1}{\lambda} A^T)^{-1} e}.
#' @importFrom igraph alpha_centrality
#' @references 
#' ## On alpha centrality
#'   Katz, Leo 1953. 
#'   "A new status index derived from sociometric analysis". 
#'   _Psychometrika_. 18(1): 39–43.
#' 
#'   Bonacich, P. and Lloyd, P. 2001. 
#'   “Eigenvector-like measures of centrality for asymmetric relations” 
#'   _Social Networks_. 23(3):191-201.
#' @export 
node_by_alpha <- function(.data, alpha = 0.85){
  .data <- manynet::expect_nodes(.data)
  make_node_measure(igraph::alpha_centrality(manynet::as_igraph(.data), 
                                             alpha = alpha),
                    .data)
}

#' @rdname measure_central_eigen 
#' @references 
#' ## On pagerank centrality
#'   Brin, Sergey and Page, Larry. 1998.
#'   "The anatomy of a large-scale hypertextual web search engine".
#'   _Proceedings of the 7th World-Wide Web Conference_. Brisbane, Australia.
#' @export 
node_by_pagerank <- function(.data){
  .data <- manynet::expect_nodes(.data)
  make_node_measure(igraph::page_rank(manynet::as_igraph(.data))$vector,
                    .data)
}

#' @rdname measure_central_eigen 
#' @references 
#' ## On hub and authority centrality
#'   Kleinberg, Jon. 1999.
#'   "Authoritative sources in a hyperlinked environment". 
#'   _Journal of the ACM_ 46(5): 604–632.
#'   \doi{10.1145/324133.324140}
#' @export 
node_by_authority <- function(.data){
  .data <- manynet::expect_nodes(.data)
  make_node_measure(igraph::hits_scores(manynet::as_igraph(.data))$authority,
                    .data)
}

#' @rdname measure_central_eigen 
#' @export 
node_by_hub <- function(.data){
  .data <- manynet::expect_nodes(.data)
  make_node_measure(igraph::hits_scores(manynet::as_igraph(.data))$hub,
                    .data)
}

#' @rdname measure_central_eigen 
#' @section Subgraph centrality:
#'   Subgraph centrality measures the participation of a node in all subgraphs
#'   in the network, giving higher weight to smaller subgraphs.
#'   It is defined as:
#'   \deqn{C_S(i) = \sum_{k=0}^{\infty} \frac{(A^k)_{ii}}{k!}}
#'   where \eqn{(A^k)_{ii}} is the \eqn{i}th diagonal element of the \eqn{k}th power
#'   of the adjacency matrix \eqn{A}, representing the number of closed walks
#'   of length \eqn{k} starting and ending at node \eqn{i}.
#'   Weighting by \eqn{\frac{1}{k!}} ensures that shorter walks contribute more
#'   to the centrality score than longer walks.
#'   
#'   Subgraph centrality is a good choice of measure when the focus is on
#'   local connectivity and clustering around a node,
#'   as it captures the extent to which a node is embedded in tightly-knit
#'   groups within the network.
#'   Note though that because of the way spectral decomposition is used to
#'   calculate this measure, this is not a good measure for very large graphs.
#' @references
#' ## On subgraph centrality
#'   Estrada, Ernesto and Rodríguez-Velázquez, Juan A. 2005.
#'   "Subgraph centrality in complex networks".
#'   _Physical Review E_ 71(5): 056103.
#'   \doi{10.1103/PhysRevE.71.056103}
#' @export 
node_by_subgraph <- function(.data){
  .data <- manynet::expect_nodes(.data)
  make_node_measure(igraph::subgraph_centrality(manynet::as_igraph(.data)),
                    .data)
}

# Eigenvector-like centralities ####

#' Measuring ties eigenvector-like centrality
#' @name measure_centralities_eigen
#' @description
#'   `tie_by_eigenvector()` measures the eigenvector centrality of ties in a 
#'   network.
#'   
#'   All measures attempt to use as much information as they are offered,
#'   including whether the networks are directed, weighted, or multimodal.
#'   If this would produce unintended results, 
#'   first transform the salient properties using e.g. [to_undirected()] functions.
#'   All centrality and centralization measures return normalized measures 
#'   by default, including for two-mode networks.
#' @template param_data
#' @template param_norm
#' @family eigenvector
#' @family centrality
#' @template tie_measure
NULL

#' @rdname measure_centralities_eigen
#' @examples 
#' tie_by_eigenvector(ison_adolescents)
#' @export
tie_by_eigenvector <- function(.data, normalized = TRUE){
  .data <- manynet::expect_ties(.data)
  edge_adj <- manynet::to_ties(.data)
  out <- node_by_eigenvector(edge_adj, normalized = normalized)
  class(out) <- "numeric"
  make_tie_measure(out, .data)
}

# Eigenvector centralisation ####

#' Measuring networks eigenvector-like centralisation
#' @name measure_centralisation_eigen
#' @description
#'   `net_by_eigenvector()` measures the eigenvector centralization for a 
#'   network.
#'   
#'   All measures attempt to use as much information as they are offered,
#'   including whether the networks are directed, weighted, or multimodal.
#'   If this would produce unintended results, 
#'   first transform the salient properties using e.g. [to_undirected()] functions.
#'   All centrality and centralization measures return normalized measures 
#'   by default, including for two-mode networks.
#' @template param_data
#' @template param_norm
#' @family eigenvector
#' @family centrality
#' @template net_measure
NULL

#' @rdname measure_centralisation_eigen 
#' @examples
#' net_by_eigenvector(ison_southern_women)
#' @export
net_by_eigenvector <- function(.data, normalized = TRUE){
  .data <- manynet::expect_nodes(.data)
  if (manynet::is_twomode(.data)) {
    out <- c(igraph::centr_eigen(manynet::as_igraph(manynet::to_mode1(.data)), 
                                 normalized = normalized)$centralization,
             igraph::centr_eigen(manynet::as_igraph(manynet::to_mode2(.data)), 
                                 normalized = normalized)$centralization)
  } else {
    out <- igraph::centr_eigen(manynet::as_igraph(.data), 
                               normalized = normalized)$centralization
  }
  out <- make_network_measure(out, .data, call = deparse(sys.call()))
  out
}


