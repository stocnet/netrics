#' Memberships in equivalent classes
#' @description 
#'   These functions combine an appropriate `node_x_*()` function
#'   together with methods for calculating the hierarchical clusters
#'   provided by a certain distance calculation.
#'   
#'   - `node_in_equivalence()` assigns nodes membership based on their equivalence 
#'   with respective to some motif/class.
#'   The following functions call this function, together with an appropriate motif.
#'   - `node_in_structural()` assigns nodes membership based on their
#'   having equivalent ties to the same other nodes.
#'   - `node_in_regular()` assigns nodes membership based on their
#'   having equivalent patterns of ties to equivalent others.
#'   - `node_in_automorphic()` assigns nodes membership based on their
#'   having equivalent distances to other nodes.
#'   - `node_in_motif()` assigns nodes membership based on their
#'   participating in local structures at similar rates.
#'
#'   A `plot()` method exists for investigating the dendrogram
#'   of the hierarchical cluster and showing the returned cluster
#'   assignment.
#' @name member_equivalence
#' @template param_data
#' @template param_motf
#' @template node_member
#' @param k Typically a character string indicating which method
#'   should be used to select the number of clusters to return.
#'   By default `"silhouette"`, other options include `"elbow"` and `"strict"`.
#'   `"strict"` returns classes with members only when strictly equivalent.
#'   `"silhouette"` and `"elbow"` select classes based on the distance between
#'   clusters or between nodes within a cluster.
#'   Fewer, identifiable letters, e.g. `"e"` for elbow, is sufficient.
#'   Alternatively, if `k` is passed an integer, e.g. `k = 3`,
#'   then all selection routines are skipped in favour of this number of clusters.
#' @param cluster Character string indicating whether clusters should be 
#'   clustered hierarchically (`"hierarchical"`) or 
#'   through convergence of correlations (`"concor"`). 
#'   Fewer, identifiable letters, e.g. `"c"` for CONCOR, is sufficient.
#' @param distance Character string indicating which distance metric
#'   to pass on to `stats::dist`.
#'   By default `"euclidean"`, but other options include
#'   `"maximum"`, `"manhattan"`, `"canberra"`, `"binary"`, and `"minkowski"`.
#'   Fewer, identifiable letters, e.g. `"e"` for Euclidean, is sufficient.
#' @param max_k Integer indicating the maximum number of (k) clusters
#'   to evaluate.
#'   Ignored when `k = "strict"` or a discrete number is given for `k`.
#' @param Kmax Deprecated. The former spelling of `max_k`.
#'   Still accepted, but warns; please use `max_k` instead.
#' @importFrom stats as.dist hclust cutree coef cor median
#' @source \url{https://github.com/aslez/concoR}
NULL

#' @rdname member_equivalence 
#' @export
node_in_equivalence <- function(.data, motif,
                                k = c("silhouette", "elbow", "strict"),
                                cluster = c("hierarchical", "concor", "cosine"),
                                distance = c("euclidean", "maximum", "manhattan", 
                                             "canberra", "binary", "minkowski"),
                                max_k = 8L, Kmax = NULL){
  max_k <- resolve_max_k(max_k, Kmax)
  .data <- manynet::expect_nodes(.data)
  cluster <- match.arg(cluster)
  manynet::snet_info("Clustering using {.fn cluster_{cluster}}.")
  hc <- switch(cluster,
               hierarchical = cluster_hierarchical(motif,
                                                   match.arg(distance)),
               concor = cluster_concor(.data, motif),
               cosine = cluster_cosine(motif, 
                                       match.arg(distance)))
  
  if(!is.numeric(k)){
    k <- match.arg(k)
    manynet::snet_info("Selecting the number of clusters using {.fn k_{k}}.")
    k <- switch(k,
                strict = k_strict(hc, .data),
                elbow = k_elbow(hc, .data, motif, max_k),
                silhouette = k_silhouette(hc, .data, max_k))
  }
  if(length(k)==0) k <- 1 # in the case of all nodes being in the same cluster
  
  out <- make_node_member(stats::cutree(hc, k), .data)
  attr(out, "hc") <- hc
  attr(out, "k") <- k
  out
}

#' @rdname member_equivalence
#' @examples
#' (nse <- node_in_structural(ison_algebra))
#' @export
node_in_structural <- function(.data,
                               k = c("silhouette", "elbow", "strict"),
                               cluster = c("hierarchical", "concor","cosine"),
                               distance = c("euclidean", "maximum", "manhattan", 
                                            "canberra", "binary", "minkowski"),
                               max_k = 8L, Kmax = NULL){
  max_k <- resolve_max_k(max_k, Kmax)
  .data <- manynet::expect_nodes(.data)
  mat <- node_x_tie(.data)
  if(any(colSums(t(mat))==0)){
    mat <- cbind(mat, (colSums(t(mat))==0))
  } 
  node_in_equivalence(.data, mat, 
                      k = k, cluster = cluster, distance = distance, 
                      max_k = max_k)
}

#' @rdname member_equivalence
#' @param regularity Character string indicating which algorithm should be
#'   used to calculate how regularly equivalent nodes are.
#'   By default `"rolesim"`; `"rege"` is also available.
#'   Fewer, identifiable letters, e.g. `"ro"` for RoleSim, is sufficient.
#'   See [regularity_rolesim()] and [regularity_rege()] for how they differ.
#' @template param_decay
#' @param beta Deprecated; use `decay` instead.
#' @section Regular equivalence:
#'   Two nodes are regularly equivalent if each has ties to the same _kinds_ of
#'   others, even where those others are not the same individuals and are not
#'   equally numerous. A manager with three subordinates and a manager with ten
#'   are regularly equivalent, because what makes them alike is that they both
#'   have subordinates, not how many or which.
#'
#'   The definition is recursive: nodes are equivalent if their alters are
#'   equivalent, whose equivalence depends in turn on _their_ alters.
#'   `node_in_regular()` therefore computes a similarity matrix by iterating
#'   that definition to a fixed point, and then clusters it in the same way as
#'   the other functions here.
#'
#'   Note that this differs from `node_in_motif()`, which compares nodes on how
#'   often they appear embedded in local structures. 
#'   Two nodes can have very similar triad profiles without being regularly equivalent, 
#'   and vice versa, since a motif census counts a node's local configurations 
#'   while regular equivalence asks who its alters are.
#' @examples
#' (nre <- node_in_regular(ison_southern_women))
#' @export
node_in_regular <- function(.data,
                            k = c("silhouette", "elbow", "strict"),
                            cluster = c("hierarchical", "concor","cosine"),
                            distance = c("euclidean", "maximum", "manhattan",
                                         "canberra", "binary", "minkowski"),
                            max_k = 8L,
                            regularity = c("rolesim", "rege"),
                            decay = 0.15, beta = NULL, Kmax = NULL){
  max_k <- resolve_max_k(max_k, Kmax)
  .data <- manynet::expect_nodes(.data)
  regularity <- match.arg(regularity)
  decay <- resolve_decay(decay, beta, "beta")
  manynet::snet_info("Calculating regular equivalence using",
                     "{.fn regularity_{regularity}}.")
  mat <- switch(regularity,
                rolesim = regularity_rolesim(.data, decay = decay),
                rege = regularity_rege(.data))
  node_in_equivalence(.data, mat,
                   k = k, cluster = cluster, distance = distance, max_k = max_k)
}

#' @rdname member_equivalence
#' @section Motif equivalence:
#'   Where the other functions here compare nodes on _whom_ they are tied to,
#'   `node_in_motif()` compares them on _what kinds of local structure_ they sit
#'   in, by clustering a census of the triads (or, for two-mode networks,
#'   tetrads) each node participates in.
#'
#'   This captures similarity of local embedding rather than equivalence of
#'   role. It is well suited to distinguishing nodes that sit in dense,
#'   closed neighbourhoods from those that bridge open ones,
#'   but it is not regular equivalence: see `node_in_regular()` for that.
#'
#'   This function was called `node_in_regular()` prior to version 1.0.0.
#' @examples
#' (nme <- node_in_motif(ison_southern_women, cluster = "concor"))
#' @export
node_in_motif <- function(.data,
                          k = c("silhouette", "elbow", "strict"),
                          cluster = c("hierarchical", "concor","cosine"),
                          distance = c("euclidean", "maximum", "manhattan",
                                       "canberra", "binary", "minkowski"),
                          max_k = 8L, Kmax = NULL){
  max_k <- resolve_max_k(max_k, Kmax)
  .data <- manynet::expect_nodes(.data)
  if(manynet::is_twomode(.data)){
    manynet::snet_info("Since this is a two-mode network,",
              "using {.fn node_x_tetrad} to",
              "profile nodes' embedding in local structures.")
    mat <- as.matrix(node_x_tetrad(.data))
  } else {
    manynet::snet_info("Since this is a one-mode network,",
              "using {.fn node_x_triad} to",
              "profile nodes' embedding in local structures.")
    mat <- node_x_triad(.data)
  }
  if(any(colSums(mat) == 0)) mat <- mat[,-which(colSums(mat) == 0)]
  node_in_equivalence(.data, mat,
                   k = k, cluster = cluster, distance = distance, max_k = max_k)
}

#' @rdname member_equivalence
#' @examples
#' if(require("sna", quietly = TRUE)){
#' (nae <- node_in_automorphic(ison_southern_women,
#'   k = "elbow"))
#' }
#' @export
node_in_automorphic <- function(.data,
                                k = c("silhouette", "elbow", "strict"),
                                cluster = c("hierarchical", "concor","cosine"),
                                distance = c("euclidean", "maximum", "manhattan", 
                                             "canberra", "binary", "minkowski"),
                                max_k = 8L, Kmax = NULL){
  max_k <- resolve_max_k(max_k, Kmax)
  .data <- manynet::expect_nodes(.data)
  mat <- node_x_path(.data)
  node_in_equivalence(.data, mat, 
                   k = k, cluster = cluster, distance = distance, max_k = max_k)
}

#' @rdname member_equivalence
#' @param blocks A character vector of permitted ideal block types,
#'   or a list-matrix giving the permitted types per block position.
#'   See [net_by_inconsistency()] for the available types.
#' @param times Integer number of search iterations.
#'   By default the number of nodes times the number of positions.
#' @section Direct blockmodelling:
#'   The other functions here are _indirect_: they build a similarity between
#'   nodes, cluster it, and read a partition off the result.
#'   `node_in_block()` is _direct_. It searches the space of partitions for
#'   the one that best fits an ideal block structure, scoring each candidate
#'   with [net_by_inconsistency()] and keeping whichever is most consistent.
#'
#'   The advantage is that the criterion being optimised is the one you
#'   actually care about, rather than a similarity that stands in for it,
#'   and that ideal types other than "null and complete" become available —
#'   `blocks = c("nul", "reg")` searches directly for a regular-equivalence
#'   blockmodel.
#'   The cost is that the number of positions `k` must be chosen in advance,
#'   and that the search is stochastic: it explores by random restarts and
#'   perturbations, so repeated runs may return different partitions and a
#'   longer search is more likely to find a good one.
#'   Set a seed for reproducibility, and compare runs with [net_by_inconsistency()].
#' @references
#' ## On direct blockmodelling
#' Doreian, Patrick, Vladimir Batagelj, and Anuska Ferligoj. 2005.
#' _Generalized Blockmodeling_.
#' Cambridge: Cambridge University Press.
#' \doi{10.1017/CBO9780511584176}
#' @examples
#' (nbm <- node_in_block(ison_adolescents, k = 3))
#' net_by_inconsistency(ison_adolescents, nbm)
#' @export
node_in_block <- function(.data, k = 2L,
                               blocks = c("nul", "com"),
                               times = NULL){
  .data <- manynet::expect_nodes(.data)
  if(!is.numeric(k) || k < 2)
    manynet::snet_abort("`k` must be the number of positions sought, at least 2.")
  n <- manynet::net_nodes(.data)
  if(k > n) manynet::snet_abort("`k` cannot exceed the number of nodes.")
  if(is.null(times)) times <- n * k
  fitness <- function(m) as.numeric(net_by_inconsistency(.data, m, blocks = blocks))
  # begin from a random partition into k roughly equal positions
  shuffled <- sample(seq.int(n))
  out <- cut(seq_along(shuffled), k, labels = FALSE)[shuffled]
  fit <- fitness(out)
  soln <- out
  for(t in seq.int(times)){
    soln <- .weakPerturb(soln)
    new_fit <- fitness(soln)
    if(new_fit < fit){
      out <- soln
      fit <- new_fit
    }
    if(t %% 10 == 0) soln <- .strongPerturb(soln)
  }
  out <- make_node_member(out, .data)
  attr(out, "k") <- k
  out
}
