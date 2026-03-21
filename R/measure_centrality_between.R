# Betweenness centrality ####

#' Measuring nodes betweenness-like centrality
#' @name measure_central_between
#' @description
#'   These functions calculate common betweenness-related centrality measures for one- and two-mode networks:
#'   
#'   - `node_by_betweenness()` measures the betweenness centralities of nodes in a network.
#'   - `node_by_induced()` measures the induced betweenness centralities of nodes in a network.
#'   - `node_by_flow()` measures the flow betweenness centralities of nodes in a network,
#'   which uses an electrical current model for information spreading 
#'   in contrast to the shortest paths model used by normal betweenness centrality.
#'   - `node_by_stress()` measures the stress centrality of nodes in a network.
#'   - `tie_by_betweenness()` measures the number of shortest paths going through a tie.
#'   
#'   All measures attempt to use as much information as they are offered,
#'   including whether the networks are directed, weighted, or multimodal.
#'   If this would produce unintended results, 
#'   first transform the salient properties using e.g. [to_undirected()] functions.
#'   All centrality and centralization measures return normalized measures by default,
#'   including for two-mode networks.
#' @template param_data
#' @template param_norm
#' @family betweenness
#' @family centrality
#' @template node_measure
#' @param cutoff The maximum path length to consider when calculating betweenness.
#'   If negative or NULL (the default), there's no limit to the path lengths considered.
NULL

#' @rdname measure_central_between
#' @section Betweenness centrality: 
#'   Betweenness centrality is based on the number of shortest paths between
#'   other nodes that a node lies upon:
#'   \deqn{C_B(i) = \sum_{j,k:j \neq k, j \neq i, k \neq i} \frac{g_{jik}}{g_{jk}}}
#' @references
#' ## On betweenness centrality
#' Freeman, Linton. 1977. 
#' "A set of measures of centrality based on betweenness". 
#' _Sociometry_, 40(1): 35–41. 
#' \doi{10.2307/3033543}
#' @examples
#' node_by_betweenness(ison_southern_women)
#' @export 
node_by_betweenness <- function(.data, normalized = TRUE, 
                                cutoff = NULL){
  
  .data <- manynet::expect_nodes(.data)
  weights <- `if`(manynet::is_weighted(.data), 
                  manynet::tie_weights(.data), NA)
  graph <- manynet::as_igraph(.data)
  
  # Do the calculations
  if (manynet::is_twomode(graph) & normalized){
    betw_scores <- igraph::betweenness(graph = graph, v = igraph::V(graph), 
                                       directed = manynet::is_directed(graph))
    other_set_size <- ifelse(igraph::V(graph)$type, sum(!igraph::V(graph)$type), sum(igraph::V(graph)$type))
    set_size <- ifelse(igraph::V(graph)$type, sum(igraph::V(graph)$type), sum(!igraph::V(graph)$type))
    out <- ifelse(set_size > other_set_size, 
                  betw_scores/(2*(set_size-1)*(other_set_size-1)), 
                  betw_scores/(1/2*other_set_size*(other_set_size-1)+1/2*(set_size-1)*(set_size-2)+(set_size-1)*(other_set_size-1)))
  } else {
    if (is.null(cutoff)) {
      out <- igraph::betweenness(graph = graph, v = igraph::V(graph), 
                                 directed = manynet::is_directed(graph), weights = weights, 
                                 normalized = normalized)
    } else {
      out <- igraph::betweenness(graph = graph, v = igraph::V(graph), 
                                 directed = manynet::is_directed(graph), 
                                 cutoff = cutoff, 
                                 weights = weights)
    }
  }
  out <- make_node_measure(out, .data)
  out
}

#' @rdname measure_central_between 
#' @section Induced centrality: 
#'   Induced centrality or vitality centrality concerns the change in 
#'   total betweenness centrality between networks with and without a given node:
#'   \deqn{C_I(i) = C_B(G) - C_B(G\ i)}
#' @references
#' ## On induced centrality
#' Everett, Martin and Steve Borgatti. 2010.
#' "Induced, endogenous and exogenous centrality"
#' _Social Networks_, 32: 339-344.
#' \doi{10.1016/j.socnet.2010.06.004}
#' @examples
#' node_by_induced(ison_adolescents)
#' @export 
node_by_induced <- function(.data, normalized = TRUE, 
                            cutoff = NULL){
  .data <- manynet::expect_nodes(.data)
  endog <- sum(node_by_betweenness(.data, normalized = normalized, cutoff = cutoff),
               na.rm = TRUE)
  exog <- vapply(seq.int(manynet::net_nodes(.data)),
                 function(x) sum(node_by_betweenness(manynet::delete_nodes(.data, x),
                                                     normalized = normalized, cutoff = cutoff),
                                 na.rm = TRUE),
                 FUN.VALUE = numeric(1))
  out <- endog - exog
  make_node_measure(out, .data)
}

#' @rdname measure_central_between 
#' @section Flow betweenness centrality: 
#'   Flow betweenness centrality concerns the total maximum flow, \eqn{f},
#'   between other nodes \eqn{j,k} in a network \eqn{G} that a given node mediates:
#'   \deqn{C_F(i) = \sum_{j,k:j\neq k, j\neq i, k\neq i} f(j,k,G) - f(j,k,G\ i)}
#'   When normalized (by default) this sum of differences is divided by the
#'   sum of flows \eqn{f(i,j,G)}.
#' @references
#' ## On flow centrality
#' Freeman, Lin, Stephen Borgatti, and Douglas White. 1991. 
#' "Centrality in Valued Graphs: A Measure of Betweenness Based on Network Flow". 
#' _Social Networks_, 13(2), 141-154.
#' 
#' Koschutzki, D., K.A. Lehmann, L. Peeters, S. Richter, D. Tenfelde-Podehl, and O. Zlotowski. 2005. 
#' "Centrality Indices". 
#' In U. Brandes and T. Erlebach (eds.), _Network Analysis: Methodological Foundations_. 
#' Berlin: Springer.
#' @export 
node_by_flow <- function(.data, normalized = TRUE){
  .data <- manynet::expect_nodes(.data)
  thisRequires("sna")
  out <- sna::flowbet(manynet::as_network(.data),
                      gmode = ifelse(manynet::is_directed(.data), "digraph", "graph"),
                      diag = manynet::is_complex(.data),
                      cmode = ifelse(normalized, "normflow", "rawflow"))
  make_node_measure(out, .data)
}

#' @rdname measure_central_between 
#' @section Stress centrality: 
#'   Stress centrality is the number of all shortest paths or geodesics, \eqn{g}, 
#'   between other nodes that a given node mediates:
#'   \deqn{C_S(i) = \sum_{j,k:j \neq k, j \neq i, k \neq i} g_{jik}}
#'   High stress nodes lie on a large number of shortest paths between other
#'   nodes, and thus associated with bridging or spanning boundaries.
#' @references
#' ## On stress centrality
#'   Shimbel, A. 1953.
#'   "Structural Parameters of Communication Networks".
#'   _Bulletin of Mathematical Biophysics_, 15:501-507.
#'   \doi{10.1007/BF02476438}
#' @export 
node_by_stress <- function(.data, normalized = TRUE){
  .data <- manynet::expect_nodes(.data)
  thisRequires("sna")
  out <- sna::stresscent(manynet::as_network(.data),
                         gmode = ifelse(manynet::is_directed(.data), "digraph", "graph"),
                         diag = manynet::is_complex(.data),
                         rescale = normalized)
  make_node_measure(out, .data)
}

# Tie betweenness centrality ####

#' Measuring ties betweenness-like centrality
#' @name measure_centralities_between
#' @description
#'   `tie_by_betweenness()` measures the number of shortest paths going through a tie.
#'   
#'   All measures attempt to use as much information as they are offered,
#'   including whether the networks are directed, weighted, or multimodal.
#'   If this would produce unintended results, 
#'   first transform the salient properties using e.g. [to_undirected()] functions.
#'   All centrality and centralization measures return normalized measures by default,
#'   including for two-mode networks.
#' @template param_data
#' @template param_norm
#' @family betweenness
#' @family centrality
#' @template tie_measure
NULL

#' @rdname measure_centralities_between
#' @importFrom igraph edge_betweenness
#' @examples
#' (tb <- tie_by_betweenness(ison_adolescents))
#' ison_adolescents %>% mutate_ties(weight = tb)
#' @export
tie_by_betweenness <- function(.data, normalized = TRUE){
  .data <- manynet::expect_ties(.data)
  .data <- manynet::as_igraph(.data)
  eddies <- manynet::as_edgelist(.data)
  eddies <- paste(eddies[["from"]], eddies[["to"]], sep = "-")
  out <- igraph::edge_betweenness(.data)
  names(out) <- eddies
  make_tie_measure(out, .data)
}

# Betweenness centralisation ####

#' Measuring networks betweenness-like centralisation
#' @name measure_centralisation_between
#' @description
#'   `net_by_betweenness()` measures the betweenness centralization for a network.
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
#' @family betweenness
#' @family centrality
#' @template net_measure
NULL

#' @rdname measure_centralisation_between
#' @examples
#' net_by_betweenness(ison_southern_women, direction = "in")
#' @export
net_by_betweenness <- function(.data, normalized = TRUE,
                               direction = c("all", "out", "in")) {
  .data <- manynet::expect_nodes(.data)
  direction <- match.arg(direction)
  graph <- manynet::as_igraph(.data)
  
  if (manynet::is_twomode(.data)) {
    becent <- node_by_betweenness(graph, normalized = FALSE)
    mode <- igraph::V(graph)$type
    mode1 <- length(mode) - sum(mode)
    mode2 <- sum(mode)
    out <- list()
    if (direction == "all") {
      if (!normalized) {
        out$nodes1 <- sum(max(becent[!mode]) - becent) / ((1/2 * mode2 * (mode2 - 1) + 1/2 * (mode1 - 1)*(mode1 - 2) + (mode1 - 1) * (mode2 - 2))*(mode1 + mode2 - 1) + (mode1 - 1))
        out$nodes2 <- sum(max(becent[mode]) - becent) / ((1/2 * mode1 * (mode1 - 1) + 1/2 * (mode2 - 1)*(mode2 - 2) + (mode2 - 1) * (mode1 - 2))*(mode2 + mode1 - 1) + (mode2 - 1))
        if (mode1 > mode2) {
          out$nodes1 <- sum(max(becent[!mode]) - becent) / (2 * (mode1 - 1) * (mode2 - 1) * (mode1 + mode2 - 1) - (mode2 - 1) * (mode1 + mode2 - 2) - 1/2 * (mode1 - mode2) * (mode1 + 3*mode2 - 3))
        }
        if (mode2 > mode1) {
          out$nodes2 <- sum(max(becent[mode]) - becent) / (2 * (mode2 - 1) * (mode1 - 1) * (mode2 + mode1 - 1) - (mode1 - 1) * (mode2 + mode1 - 2) - 1/2 * (mode2 - mode1) * (mode2 + 3*mode1 - 3))
        }
      } else if (normalized) {
        out$nodes1 <- sum(max(becent[!mode]) - becent) / ((1/2 * mode2 * (mode2 - 1) + 1/2 * (mode1 - 1)*(mode1 - 2) + (mode1 - 1) * (mode2 - 2))*(mode1 + mode2 - 1) + (mode1 - 1))
        out$nodes2 <- sum(max(becent[mode]) - becent) / ((1/2 * mode1 * (mode1 - 1) + 1/2 * (mode2 - 1)*(mode2 - 2) + (mode2 - 1) * (mode1 - 2))*(mode2 + mode1 - 1) + (mode2 - 1))
        if (mode1 > mode2) {
          becent <- node_by_betweenness(graph, normalized = TRUE)
          out$nodes1 <- sum(max(becent[!mode]) - becent) / ((mode1 + mode2 - 1) - (((mode2 - 1)*(mode1 + mode2 - 2) + 1/2*(mode1 - mode2)*(mode1 + (3*mode2) - 3)) / (1/2*(mode1*(mode1 - 1)) + 1/2*(mode2 - 1) * (mode2 - 2) + (mode1 - 1) * (mode2 - 1))))
        }
        if (mode2 > mode1) {
          becent <- node_by_betweenness(graph, normalized = TRUE)
          out$nodes2 <- sum(max(becent[mode]) - becent) / ((mode1 + mode2 - 1)*((mode1 - 1)*(mode1 + mode2 - 2) / 2*(mode1 - 1)*(mode2 - 1)))
        }
      }
    } else if (direction == "in") {
      out$nodes1 <- sum(max(becent[!mode]) - becent[!mode])/((mode1 - 1)*(1/2*mode2*(mode2 - 1) + 1/2*(mode1 - 1)*(mode1 - 2) + (mode1 - 1)*(mode2 - 1)))
      out$nodes2 <- sum(max(becent[mode]) - becent[mode])/((mode2 - 1)*(1/2*mode1*(mode1 - 1) + 1/2 * (mode2 - 1) * (mode2 - 2) + (mode2 - 1) * (mode1 - 1)))
      if (mode1 > mode2) {
        out$nodes1 <- sum(max(becent[!mode]) - becent[!mode]) / (2 * (mode1 - 1)^2 * (mode2 - 1))
      }
      if (mode2 > mode1) {
        out$nodes2 <- sum(max(becent[mode]) - becent[mode]) / (2 * (mode2 - 1)^2 * (mode1 - 1))
      }
    }
    out <- c("Mode 1" = out$nodes1, "Mode 2" = out$nodes2)
  } else {
    out <- igraph::centr_betw(graph = graph)$centralization
  }
  out <- make_network_measure(out, .data, call = deparse(sys.call()))
  out
}

