# Eigenvector-like centralities ####

#' Measuring nodes eigenvector-like centrality
#' @name measure_central_eigen
#' @description
#'   These functions calculate common eigenvector-related centrality 
#'   measures, or walk-based eigenmeasures, for one- and two-mode networks:
#'   
#'   - `node_by_eigenvector()` measures the eigenvector centrality of nodes 
#'   in a network.
#'   - `node_by_power()` measures the Bonacich, beta, or power centrality of 
#'   nodes in a network.
#'   - `node_by_alpha()` measures the alpha or Katz centrality of nodes in a 
#'   network.
#'   - `node_by_pagerank()` measures the pagerank centrality of nodes in a network.
#'   - `node_by_hub()` measures how well nodes in a network serve as hubs pointing 
#'   to many authorities.
#'   - `node_by_authority()` measures how well nodes in a network serve as
#'   authorities from many hubs.
#'   - `node_by_subgraph()` measures nodes' participation in all closed walks
#'   in the network, weighting shorter walks more heavily.
#'   - `node_by_posneg()` measures the PN (positive-negative) centrality of a
#'   signed network.
#'
#'   All measures attempt to use as much information as they are offered,
#'   including whether the networks are directed, weighted, or multimodal.
#'   If this would produce unintended results,
#'   first transform the salient properties using e.g. [to_undirected()] functions.
#'   All centrality and centralization measures return normalised or scaled
#'   measures where available, reported when the measure is printed.
#'
#'   Walk-based measures are mostly unbounded, so few of them can be
#'   _normalised_ against a theoretical maximum in the way that degree,
#'   closeness and betweenness can. Most are instead _scaled_ against the
#'   observed maximum, which ranks nodes within one network but does not
#'   give scores that are comparable between networks.
#' @template param_data
#' @template param_norm
#' @param scaled Logical scalar, whether to divide the results by the maximum
#'   observed in this network, so that the highest-scoring node takes the value
#'   one. Note that, unlike normalisation against a theoretical maximum, scaled
#'   scores are not comparable across different networks.
#' @param scale Deprecated; use `scaled` instead.
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
#'   This is not a limitation so much as a property of the measure:
#'   an eigenvector is defined only up to a scalar multiple,
#'   so its scores carry no absolute units to preserve.
#' @details
#'   We use `{igraph}` routines behind the scenes here for consistency and because they are often faster.
#'   For example, `igraph::eigencentrality()` is approximately 25% faster than `sna::evcent()`.
#' @references
#'   ## On eigenvector centrality
#'   Bonacich, Phillip. 1972.
#'   “Factoring and Weighting Approaches to Status Scores and Clique Identification.”
#'   _The Journal of Mathematical Sociology_ 2(1): 113–120.
#'   \doi{10.1080/0022250X.1972.9989806}
#'
#'   Bonacich, Phillip. 1991.
#'   “Simultaneous Group and Individual Centralities.”
#'   _Social Networks_ 13(2):155–68.
#'   \doi{10.1016/0378-8733(91)90018-O}
#' @examples
#' node_by_eigenvector(ison_southern_women)
#' @export 
node_by_eigenvector <- function(.data, normalized = TRUE, scaled = TRUE,
                                scale = NULL){

  .data <- manynet::expect_nodes(.data)
  scaled <- resolve_scaled(scaled, scale)
  weights <- `if`(manynet::is_weighted(.data),
                  manynet::tie_weights(.data), NULL)
  graph <- manynet::as_igraph(.data)

  # An eigenvector is only defined up to a scalar multiple, so its scores
  # carry no absolute units: scaling to the observed maximum is intrinsic to
  # the measure rather than an option. (igraph removed the choice in 2.1.1.)
  # Neither is there a theoretical maximum to normalise against.
  if(!normalized || !scaled)
    manynet::snet_info("Eigenvector scores are defined only up to a scalar multiple, so they are always scaled to the observed maximum; `normalized` and `scaled` have no effect here.")

  if(!manynet::is_connected(.data))
    manynet::snet_warn("Unconnected networks will only allow nodes from one component to have non-zero eigenvector scores.")

  # Do the calculations
  if (!manynet::is_twomode(graph)){
    out <- igraph::eigen_centrality(graph = graph,
                                    directed = manynet::is_directed(graph),
                                    weights = weights,
                                    options = igraph::arpack_defaults())$vector
  } else {
    # The projections carry their own (co-membership count) weights,
    # which igraph picks up from the graph itself.
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
  make_node_measure(out, .data, measure = "eigenvector centrality",
                    range = c(0, 1), normalization = "scaled")
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
node_by_power <- function(.data, normalized = TRUE, scaled = FALSE,
                          scale = NULL, exponent = 1){

  .data <- manynet::expect_nodes(.data)
  scaled <- resolve_scaled(scaled, scale)
  graph <- manynet::as_igraph(.data)

  # `igraph::power_centrality()` operates on the unweighted adjacency matrix
  # and offers no weights argument, so tie weights cannot be honoured here.
  if(manynet::is_weighted(.data))
    manynet::snet_info("Power centrality ignores tie weights; consider {.fn node_by_alpha} for a weighted walk-based measure.")

  if(var(node_by_deg(graph))==0){
    manynet::snet_minor_info("All nodes have the same degree, so power centrality equals degree centrality.")
    exponent <- 0
  }
  
  # Do the calculations
  if (!manynet::is_twomode(graph)){
    out <- igraph::power_centrality(graph = graph,
                                    exponent = exponent,
                                    rescale = scaled)
    if (normalized && !scaled) out <- out / sqrt(1/2)
  } else {
    eigen1 <- manynet::to_mode1(graph)
    eigen1 <- igraph::power_centrality(graph = eigen1,
                                       exponent = exponent,
                                       rescale = scaled)
    eigen2 <- manynet::to_mode2(graph)
    eigen2 <- igraph::power_centrality(graph = eigen2,
                                       exponent = exponent,
                                       rescale = scaled)
    out <- c(eigen1, eigen2)
    if (normalized && !scaled) out <- out / sqrt(1/2)
  }
  # Power centrality is unbounded and may be negative (for a negative
  # exponent), so `normalized` applies a constant factor rather than mapping
  # onto [0,1]; `scaled = TRUE` instead returns shares summing to one.
  make_node_measure(out, .data, measure = "power centrality",
                    range = `if`(scaled, c(0, 1), c(-Inf, Inf)),
                    normalization = `if`(scaled, "proportional", "none"))
}

#' @rdname measure_central_eigen
#' @template param_decay
#' @param alpha Deprecated; use `decay` instead.
#' @section Alpha centrality:
#'   Alpha centrality is also known as Katz centrality, Katz-Bonacich
#'   centrality, or Katz status.
#'   The measure is named for the \eqn{\alpha} of Bonacich and Lloyd, which
#'   trades off the importance of external influence against the importance of
#'   connection: when \eqn{\alpha = 0} only the external influence matters, and
#'   as \eqn{\alpha} grows only the connectivity matters and we reduce to
#'   eigenvector centrality.
#'   Since \eqn{\alpha} is a per-step discount, netrics takes it as `decay`,
#'   the name it uses for that parameter throughout; by default 0.85.
#'   It operates better than
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
node_by_alpha <- function(.data, decay = 0.85, alpha = NULL){
  .data <- manynet::expect_nodes(.data)
  decay <- check_decay(resolve_decay(decay, alpha, "alpha"))
  # Alpha centrality is unbounded and can be negative, so there is no
  # theoretical maximum to normalise against.
  make_node_measure(igraph::alpha_centrality(manynet::as_igraph(.data),
                                             alpha = decay),
                    .data, measure = "alpha centrality",
                    range = c(-Inf, Inf), normalization = "none")
}

#' @rdname measure_central_eigen
#' @section Pagerank centrality:
#'   Pagerank centrality, or the PageRank citation ranking, is the stationary
#'   distribution of a random walk that at each step either follows an outgoing
#'   tie or teleports to a node chosen at random.
#'   Scores are therefore already shares that sum to one.
#'   `decay` is the probability of following a tie rather than teleporting,
#'   elsewhere called the damping factor; by default 0.85.
#'   As it approaches 0 the walk teleports at every step and all nodes score
#'   alike; as it approaches 1 the walk never teleports.
#' @references
#' ## On pagerank centrality
#'   Brin, Sergey and Page, Larry. 1998.
#'   "The anatomy of a large-scale hypertextual web search engine".
#'   _Proceedings of the 7th World-Wide Web Conference_. Brisbane, Australia.
#'
#'   Page, Lawrence, Sergey Brin, Rajeev Motwani, and Terry Winograd. 1999.
#'   "The PageRank Citation Ranking: Bringing Order to the Web".
#'   _Stanford InfoLab Technical Report_ 1999-66.
#' @export
node_by_pagerank <- function(.data, decay = 0.85){
  .data <- manynet::expect_nodes(.data)
  decay <- check_decay(decay)
  # PageRank is a stationary distribution over a random walk, so scores are
  # already shares summing to one and no further rescaling applies.
  make_node_measure(igraph::page_rank(manynet::as_igraph(.data),
                                      damping = decay)$vector,
                    .data, measure = "pagerank centrality",
                    range = c(0, 1), normalization = "proportional")
}

#' @rdname measure_central_eigen
#' @section Hub and authority centrality:
#'   Hub and authority centrality are the two halves of Kleinberg's HITS
#'   (Hyperlink-Induced Topic Search) algorithm, and are computed together:
#'   good authorities are pointed to by good hubs, and good hubs point to good
#'   authorities. `node_by_hub()` and `node_by_authority()` return one each.
#'   In an undirected network the two coincide.
#' @references
#' ## On hub and authority centrality
#'   Kleinberg, Jon. 1999.
#'   "Authoritative sources in a hyperlinked environment".
#'   _Journal of the ACM_ 46(5): 604–632.
#'   \doi{10.1145/324133.324140}
#' @export
node_by_authority <- function(.data, scaled = TRUE){
  .data <- manynet::expect_nodes(.data)
  out <- igraph::hits_scores(manynet::as_igraph(.data), scale = scaled)$authority
  make_node_measure(out, .data, measure = "authority centrality",
                    range = `if`(scaled, c(0, 1), c(0, Inf)),
                    normalization = `if`(scaled, "scaled", "none"))
}

#' @rdname measure_central_eigen
#' @export
node_by_hub <- function(.data, scaled = TRUE){
  .data <- manynet::expect_nodes(.data)
  out <- igraph::hits_scores(manynet::as_igraph(.data), scale = scaled)$hub
  make_node_measure(out, .data, measure = "hub centrality",
                    range = `if`(scaled, c(0, 1), c(0, Inf)),
                    normalization = `if`(scaled, "scaled", "none"))
}

#' @rdname measure_central_eigen
#' @template param_decay
#' @param method Character string indicating which closed walks to count.
#'   By default `"all"`, which is subgraph centrality as usually defined.
#'   `"odd"` counts only walks of odd length and `"even"` only those of even
#'   length; the two sum to `"all"`.
#'   Odd closed walks cannot occur within a bipartite structure, so a node
#'   scoring near zero on `"odd"` sits in a locally two-mode-like neighbourhood.
#'   See [net_by_bipartivity()] for the network-level counterpart.
#' @section Subgraph centrality:
#'   Subgraph centrality measures the participation of a node in all subgraphs
#'   in the network, giving higher weight to smaller subgraphs.
#'   It is defined as:
#'   \deqn{C_S(i) = \sum_{k=0}^{\infty} \frac{\delta^k (A^k)_{ii}}{k!}}
#'   where \eqn{(A^k)_{ii}} is the \eqn{i}th diagonal element of the \eqn{k}th power
#'   of the adjacency matrix \eqn{A}, representing the number of closed walks
#'   of length \eqn{k} starting and ending at node \eqn{i}.
#'   Weighting by \eqn{\frac{1}{k!}} ensures that shorter walks contribute more
#'   to the centrality score than longer walks.
#'   The `decay` parameter \eqn{\delta} tunes that further, discounting each
#'   step by a further factor: at the default of 1 the measure takes its usual
#'   form, and lower values concentrate it on ever shorter walks.
#'
#'   Subgraph centrality is a good choice of measure when the focus is on
#'   local connectivity and clustering around a node,
#'   as it captures the extent to which a node is embedded in tightly-knit
#'   groups within the network.
#'   Note though that because of the way spectral decomposition is used to
#'   calculate this measure, this is not a good measure for very large graphs.
#'
#'   Summing these scores over all nodes gives the network's _Estrada index_,
#'   so a node's subgraph centrality is its contribution to that index.
#' @references
#' ## On subgraph centrality
#'   Estrada, Ernesto and Rodríguez-Velázquez, Juan A. 2005.
#'   "Subgraph centrality in complex networks".
#'   _Physical Review E_ 71(5): 056103.
#'   \doi{10.1103/PhysRevE.71.056103}
#'
#' ## On odd and even closed walks
#'   Estrada, Ernesto and Rodríguez-Velázquez, Juan A. 2005.
#'   "Spectral measures of bipartivity in complex networks".
#'   _Physical Review E_ 72(4): 046105.
#'   \doi{10.1103/PhysRevE.72.046105}
#' @export
node_by_subgraph <- function(.data, decay = 1,
                             method = c("all", "odd", "even")){
  .data <- manynet::expect_nodes(.data)
  method <- match.arg(method)
  decay <- check_decay(decay)
  out <- .closed_walks(.data, decay, method)
  # Subgraph centrality grows exponentially in the number of closed walks and
  # has no theoretical maximum, so no normalisation is offered.
  # Every node has one closed walk of length zero, itself, which the "odd"
  # count alone excludes.
  make_node_measure(out, .data,
                    measure = switch(method,
                                     all = "subgraph centrality",
                                     odd = "odd subgraph centrality",
                                     even = "even subgraph centrality"),
                    range = `if`(method == "odd", c(0, Inf), c(1, Inf)),
                    normalization = "none", variant = method)
}

# Counts each node's closed walks, weighting a walk of length k by
# `decay^k / k!`, which the eigendecomposition of a symmetric adjacency matrix
# evaluates in closed form: `exp` sums walks of every length, while `sinh` and
# `cosh` split that sum into the odd- and even-length walks respectively.
# Shared by `node_by_subgraph()` and `net_by_bipartivity()`.
# Unlike `igraph::subgraph_centrality()` this honours tie weights, which are
# carried by the adjacency matrix itself.
.closed_walks <- function(.data, decay = 1, method = c("all", "odd", "even")) {
  method <- match.arg(method)
  mat <- manynet::as_matrix(manynet::to_multilevel(.data))
  if(!isSymmetric(unname(mat))) {
    manynet::snet_info("Counting closed walks on the undirected form of this network, since the decomposition requires a symmetric matrix.")
    mat <- (mat + t(mat))/2
  }
  eig <- eigen(mat, symmetric = TRUE)
  weights <- switch(method,
                    all = exp(decay * eig$values),
                    odd = sinh(decay * eig$values),
                    even = cosh(decay * eig$values))
  out <- as.numeric((eig$vectors^2) %*% weights)
  names(out) <- rownames(mat)
  out
}

#' @rdname measure_central_eigen
#' @section PN (positive-negative) centrality:
#'   PN centrality extends walk-based centrality to signed networks.
#'   Negative ties are weighted twice as heavily as positive ties,
#'   \eqn{P - 2N}, and the measure is then obtained in closed form by matrix
#'   inversion, so that — like alpha centrality, of which it is the signed
#'   analogue — it counts walks of all lengths with a length discount rather
#'   than counting only direct ties.
#'   Scores centre on 1: nodes above 1 are advantaged by their pattern of
#'   positive and negative ties, and those below 1 disadvantaged.
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
  make_node_measure(out, .data, measure = "PN centrality",
                    range = c(0, Inf), normalization = "none")
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
  edge_adj <- manynet::to_linegraph(.data)
  out <- node_by_eigenvector(edge_adj, normalized = normalized)
  class(out) <- "numeric"
  make_tie_measure(out, .data, measure = "eigenvector centrality",
                   range = c(0, 1), normalization = "scaled")
}

# Eigenvector centralisation ####

#' Measuring networks eigenvector-like centralisation
#' @name measure_centralisation_eigen
#' @description
#'   - `net_by_eigenvector()` measures the eigenvector centralization for a
#'   network as a single score.
#'   - `mode_by_eigenvector()` measures eigenvector centralization separately for
#'   each mode of a two-mode network (via projection to each mode), returning one
#'   score per mode (following Borgatti and Everett, 1997).
#'
#'   All measures attempt to use as much information as they are offered,
#'   including whether the networks are directed, weighted, or multimodal.
#'   If this would produce unintended results,
#'   first transform the salient properties using e.g. [to_undirected()] functions.
#'   All centrality and centralization measures return normalized measures
#'   by default, including for two-mode networks.
#'
#'   For two-mode networks the two modes have different theoretical maxima, so
#'   `net_by_eigenvector()` reports a single network-level score by applying
#'   Freeman's general centralization index over the normalized node eigenvector
#'   scores, whereas `mode_by_eigenvector()` reports the per-mode scores directly.
#' @template param_data
#' @template param_norm
#' @family eigenvector
#' @family centrality
#' @references
#'   Borgatti, Stephen P., and Martin G. Everett. 1997.
#'   "Network analysis of 2-mode data."
#'   _Social Networks_ 19(3): 243-269.
#'   \doi{10.1016/S0378-8733(96)00301-2}
#' @returns
#'   `net_by_eigenvector()` returns a `network_measure` scalar;
#'   `mode_by_eigenvector()` returns a `mode_measure` numeric vector of length two,
#'   giving one centralization score per mode.
NULL

#' @rdname measure_centralisation_eigen
#' @examples
#' net_by_eigenvector(ison_southern_women)
#' @export
net_by_eigenvector <- function(.data, normalized = TRUE){
  .data <- manynet::expect_nodes(.data)
  if (manynet::is_twomode(.data)) {
    # Two-mode eigenvector centralization is intrinsically per mode
    # (see `mode_by_eigenvector()`, following Borgatti and Everett, 1997).
    # For a single network-level score we apply Freeman's general
    # centralization index over the whole node set, using the normalized node
    # eigenvector scores (each in [0, 1]); the numerator's maximum is (n - 1).
    nc <- node_by_eigenvector(.data, normalized = TRUE)
    out <- sum(max(nc) - nc) / (length(nc) - 1)
  } else {
    out <- igraph::centr_eigen(manynet::as_igraph(.data),
                               normalized = normalized)$centralization
  }
  make_network_measure(out, .data, call = deparse(sys.call()),
                       measure = "eigenvector centralisation",
                       range = `if`(normalized, c(0, 1), c(0, Inf)),
                       normalization = `if`(normalized, "normalized", "none"))
}

#' @rdname measure_centralisation_eigen
#' @examples
#' mode_by_eigenvector(ison_southern_women)
#' @export
mode_by_eigenvector <- function(.data, normalized = TRUE){
  .data <- manynet::expect_nodes(.data)
  if (!manynet::is_twomode(.data))
    manynet::snet_abort("`mode_by_eigenvector()` is only defined for two-mode networks; use `net_by_eigenvector()` for one-mode networks.")
  out <- c("Mode 1" = igraph::centr_eigen(manynet::as_igraph(manynet::to_mode1(.data)),
                                          normalized = normalized)$centralization,
           "Mode 2" = igraph::centr_eigen(manynet::as_igraph(manynet::to_mode2(.data)),
                                          normalized = normalized)$centralization)
  make_mode_measure(out, .data, call = deparse(sys.call()),
                    measure = "eigenvector centralisation",
                    range = `if`(normalized, c(0, 1), c(0, Inf)),
                    normalization = `if`(normalized, "normalized", "none"))
}


