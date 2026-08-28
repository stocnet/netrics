# Methods for calculating coreness ####

#' Methods for calculating coreness
#' @name method_coreness
#' @description
#'   These functions calculate how core-like each node is, returning both a
#'   continuous coreness score and a core/periphery split that
#'   [node_is_core()], [node_by_core()] and [node_in_core()] then use.
#'
#'   - `coreness_correlation()` fits the network to an ideal core-periphery
#'   pattern by correlation.
#'   - `coreness_rich()` ranks nodes by strength and cuts where the tie
#'   weight to higher-ranked neighbours peaks.
#'   - `coreness_transition()` scores nodes with a transition function whose
#'   sharpness and core size are free parameters.
#'   - `coreness_hub()` scores nodes by how well they send to and receive from
#'   the core, which lets core and periphery differ by tie direction.
#'
#'   They differ in what they can use. `coreness_rich()` and
#'   `coreness_hub()` read tie direction and tie weights directly.
#'   `coreness_correlation()` and `coreness_transition()` compare the network
#'   against a symmetric ideal, so they symmetrise a directed network first
#'   and report that they have done so.
#' @template param_data
#' @param direction One of "all" (the default), "out", or "in".
#'   For a directed network, "out" scores nodes on the ties they send and
#'   "in" on the ties they receive.
#'   Ignored for undirected and two-mode networks.
#' @returns A list with two elements:
#'
#'   - `coreness`: a numeric vector between 0 and 1, one value per node,
#'   for how core-like each node is.
#'   - `core`: a logical vector, one value per node, TRUE for the core.
#'
#'   `coreness_hub()` adds `out_core` and `in_core`, the two core sets that a
#'   directed core-periphery structure distinguishes.
#' @references
#' ## On the correlation method
#' Borgatti, Stephen P., and Martin G. Everett. 2000.
#' "Models of core/periphery structures".
#' _Social Networks_ 21(4): 375-395.
#' \doi{10.1016/S0378-8733(99)00019-2}
#'
#' Lip, Sean Z. W. 2011.
#' "A fast algorithm for the discrete core/periphery bipartitioning problem".
#' \doi{10.48550/arXiv.1102.5511}
#'
#' ## On the rich-core method
#' Ma, Athen, and Raul J. Mondragon. 2015.
#' "Rich-cores in networks".
#' _PLoS ONE_ 10(3): e0119678.
#' \doi{10.1371/journal.pone.0119678}
#'
#' ## On the transition method
#' Rombach, Puck, Mason A. Porter, James H. Fowler, and Peter J. Mucha. 2017.
#' "Core-periphery structure in networks (revisited)".
#' _SIAM Review_ 59(3): 619-646.
#' \doi{10.1137/17M1130046}
#'
#' ## On the hub method
#' Elliott, Andrew, Angus Chiu, Marya Bazzi, Gesine Reinert,
#' and Mihai Cucuringu. 2020.
#' "Core-periphery structure in directed networks".
#' _Proceedings of the Royal Society A_ 476(2241): 20190783.
#' \doi{10.1098/rspa.2019.0783}
#' @family methods
NULL

# Every method needs the network as a matrix, oriented by `direction`.
# "out" leaves the matrix as it is, so rows are senders; "in" transposes it,
# so rows are receivers; "all" adds the two, so that a tie in either direction
# counts. A two-mode network has no direction to read, so it is left alone.
.core_matrix <- function(.data, direction = "all"){
  mat <- manynet::as_matrix(.data)
  if(manynet::is_twomode(.data) || !manynet::is_directed(.data)) return(mat)
  switch(direction,
         out = mat,
         `in` = t(mat),
         all = mat + t(mat))
}

# The degree (or, for a weighted network, the strength) that goes with that
# matrix. For a one-mode network this is the row sum of the oriented matrix.
.core_strength <- function(mat, twomode = FALSE){
  if(twomode) c(rowSums(mat), colSums(mat)) else rowSums(mat)
}

# Lip's (2011) cut. Ordering the nodes and adding them to the core one at a
# time, the quantity Z rises by `(k-1) - degi` at each step, so the whole
# sequence can be swept in one pass and the best cut kept. Any ordering may be
# passed: the degrees still measure Z exactly, so a coreness ordering is as
# valid here as the degree ordering Lip uses.
#
# `pairs` is 1 for a symmetric matrix, where the k nodes already in the core
# hold k(k-1)/2 pairs, and 2 for a directed one, where they hold k(k-1)
# ordered pairs and each is either sent or received. In the directed case
# `degi` must be the total of the in- and out-degrees.
.lip_cut <- function(degi, nord, pairs = 1){
  n <- length(degi)
  if(n < 2) return(rep(TRUE, n))
  zbest <- Inf
  kbest <- 0
  z <- sum(degi)/2
  for(k in seq_len(n-1)){
    z <- z + pairs*(k - 1) - degi[nord][k]
    if(z < zbest){
      zbest <- z
      kbest <- k
    }
  }
  seq_len(n) %in% nord[seq_len(kbest)]
}

# The starting points for a restarted search. Raising the scaled degree to a
# ladder of powers sharpens or flattens it, which moves the start toward a
# smaller or a larger core, and the rank vector drops degree magnitude
# altogether. These explore different basins of the objective without any
# randomness, so that two calls on one network return the same answer: a
# descriptive measure that moved between calls would not be much use.
.core_inits <- function(degi, starts){
  powers <- c(1, 0.5, 2, 0.25, 4, 0.125, 8, 16)
  cands <- c(lapply(powers, function(p) .core_scale(degi^p)),
             list(.core_scale(rank(degi))))
  if(starts > length(cands))
    manynet::snet_info("At most {length(cands)} starting points are defined,",
                       "so {.arg starts} is capped there.")
  cands[seq_len(min(starts, length(cands)))]
}

# Scales a vector onto [0,1]. A constant vector has no gradient to report,
# so every node is given the same middling score rather than an NaN.
.core_scale <- function(x){
  rng <- range(x)
  if(!is.finite(rng[1]) || diff(rng) == 0) return(rep(0.5, length(x)))
  (x - rng[1])/diff(rng)
}

# Says once, and only for a directed network, that a method cannot read
# direction and has symmetrised the network to proceed.
.core_symmetrise_info <- function(.data, method){
  if(manynet::is_directed(.data))
    manynet::snet_info("{.fn {method}} compares the network against a",
                       "symmetric ideal, so tie direction is not used.",
                       "For a directed core-periphery structure,",
                       "see {.fn coreness_hub}.")
}

# Correlation ####

#' @rdname method_coreness
#' @section Correlation:
#'   Borgatti and Everett's continuous model gives each node a coreness
#'   \eqn{c_i} between 0 and 1, and compares the network against the ideal
#'   pattern \eqn{c_i c_j} in which two nodes are tied to the extent that both
#'   are core:
#'   \deqn{\rho = \text{cor}(A_{ij}, c_i c_j), i \neq j}
#'   The coreness vector that maximises \eqn{\rho} is the fitted model.
#'   Self-ties are excluded from the correlation, since no node is tied to
#'   itself and including the diagonal pulls every coreness toward zero.
#'
#'   The problem is not convex, so the search is run from several starting
#'   points, ordered by degree, and the best fit is kept.
#'   A weighted network is fitted to its weights, which means that the ideal
#'   pattern is read as how _strongly_ two core nodes should be tied.
#'   To fit the pattern of ties instead of their weights,
#'   use [manynet::to_unweighted()] first.
#'
#'   The search has one free value per node, so its cost grows quickly with
#'   the size of the network. On a large network, lower `starts`, or use
#'   [coreness_rich()], which needs no search at all.
#' @param starts Integer number of starting points for the search,
#'   at most 9. By default 5.
#'   The starting points are fixed rather than random, so that two calls on
#'   the same network return the same answer.
#' @examples
#' coreness_correlation(ison_adolescents)
#' @export
coreness_correlation <- function(.data, direction = c("all","out","in"),
                                 starts = 5L){
  .data <- manynet::expect_nodes(.data)
  direction <- match.arg(direction)
  if(manynet::is_twomode(.data))
    manynet::snet_abort("{.fn coreness_correlation} compares the network",
                        "against a square ideal, which a two-mode network is",
                        "not. Try {.fn coreness_rich} instead.")
  .core_symmetrise_info(.data, "coreness_correlation")
  mat <- .core_matrix(.data, "all")
  n <- nrow(mat)
  offdiag <- which(diag(n) == 0)
  obs <- mat[offdiag]
  obj_fun <- function(c){
    val <- suppressWarnings(stats::cor(obs, outer(c, c)[offdiag]))
    if(!is.finite(val)) return(1e6)
    -val
  }
  # Starting from the degree ordering rather than from a flat vector, which
  # makes the ideal pattern constant and the correlation undefined.
  degi <- .core_scale(rowSums(mat))
  inits <- .core_inits(degi, starts)
  fits <- lapply(inits, function(init)
    stats::optim(init, obj_fun, method = "L-BFGS-B", lower = 0, upper = 1))
  best <- fits[[which.min(vapply(fits, function(f) f$value, numeric(1)))]]
  out <- .core_scale(best$par)
  list(coreness = out,
       core = .lip_cut(rowSums(mat), order(out, decreasing = TRUE)))
}

# Rich-core ####

#' @rdname method_coreness
#' @section Rich-core:
#'   Ma and Mondragon rank the nodes by strength, from strongest to weakest,
#'   and give each node the total weight of its ties to nodes that rank above
#'   it:
#'   \deqn{\sigma_i^+ = \sum_{j : r_j < r_i} w_{ij}}
#'   Walking down the ranking, \eqn{\sigma^+} rises while the nodes added are
#'   still tied to those already above them, and falls once they are not.
#'   The rank at which it peaks is the boundary of the rich core.
#'
#'   The method needs no parameters and no optimisation, and it reads tie
#'   weights and tie direction directly, which makes it the method this
#'   package uses by default for a weighted, directed, or two-mode network.
#'   For a two-mode network the nodes of both modes are ranked together, so
#'   the core may span both.
#'
#'   Note that the core it finds is one whose members are tied to _each other_.
#'   Where a directed network instead has one set that sends and a different
#'   set that receives, \eqn{\sigma^+} never rises, and the method returns a
#'   core of one or two nodes. Use [coreness_hub()] for that structure, which
#'   keeps the two sets apart rather than trying to merge them.
#'
#'   A rich core is not a rich club, which is why this method is not named for
#'   one. A rich club requires the high-degree nodes to be densely tied to one
#'   another, and [net_by_richclub()] measures that density. A rich core only
#'   marks the rank at which nodes stop linking upward, so a network can have
#'   a rich core whose members are not densely tied. The rich core also needs
#'   no null model, where the rich-club coefficient does, since that
#'   coefficient rises with degree even in a random network.
#' @examples
#' coreness_rich(ison_networkers)
#' @export
coreness_rich <- function(.data, direction = c("all","out","in")){
  .data <- manynet::expect_nodes(.data)
  direction <- match.arg(direction)
  twomode <- manynet::is_twomode(.data)
  mat <- .core_matrix(.data, direction)
  stren <- .core_strength(mat, twomode)
  n <- length(stren)
  # A square matrix over all nodes, so that a two-mode network can be walked
  # in the same way as a one-mode one.
  full <- if(twomode){
    sq <- matrix(0, n, n)
    sq[seq_len(nrow(mat)), nrow(mat) + seq_len(ncol(mat))] <- mat
    sq + t(sq)
  } else mat
  nord <- order(stren, decreasing = TRUE)
  # The weight each node sends to those ranked above it.
  sigma <- vapply(seq_len(n), function(k){
    if(k == 1) return(0)
    sum(full[nord[k], nord[seq_len(k-1)]])
  }, numeric(1))
  kbest <- which.max(sigma)
  list(coreness = .core_scale(stren),
       core = seq_len(n) %in% nord[seq_len(kbest)])
}

# Transition ####

#' @rdname method_coreness
#' @section Transition:
#'   Rombach and colleagues score the node at rank \eqn{m} with a transition
#'   function
#'   \deqn{C_m = \frac{1}{1 + \exp(-(m - N\beta)\tan(\pi\alpha/2))}}
#'   where \eqn{\alpha} sets how sharp the boundary between core and periphery
#'   is, from fuzziest at 0 to a clean step at 1, and \eqn{\beta} sets how
#'   large the core is, from every node at 0 to none at 1.
#'   The ordering that maximises the core quality
#'   \eqn{R = \sum_{ij} A_{ij} C_i C_j} is the fitted model.
#'
#'   No single \eqn{\alpha} and \eqn{\beta} is right for every network, so the
#'   score is aggregated over a grid of both, weighting each by the core
#'   quality it achieves, and scaled so that the most core-like node is 1.
#' @param alpha Numeric vector of boundary sharpness values between 0 and 1,
#'   to aggregate over. By default `seq(0.2, 0.8, 0.2)`.
#' @param beta Numeric vector of core size values between 0 and 1,
#'   to aggregate over. By default `seq(0.2, 0.8, 0.2)`.
#' @examples
#' coreness_transition(ison_adolescents)
#' @export
coreness_transition <- function(.data, direction = c("all","out","in"),
                                alpha = seq(0.2, 0.8, 0.2),
                                beta = seq(0.2, 0.8, 0.2)){
  .data <- manynet::expect_nodes(.data)
  direction <- match.arg(direction)
  if(manynet::is_twomode(.data))
    manynet::snet_abort("{.fn coreness_transition} compares the network",
                        "against a square ideal, which a two-mode network is",
                        "not. Try {.fn coreness_rich} instead.")
  .core_symmetrise_info(.data, "coreness_transition")
  mat <- .core_matrix(.data, "all")
  n <- nrow(mat)
  total <- rep(0, n)
  for(a in alpha) for(b in beta){
    cstar <- .transition_values(n, a, b)
    nord <- .transition_order(mat, cstar)
    cvec <- numeric(n)
    cvec[nord] <- cstar
    quality <- sum(mat * outer(cvec, cvec))
    total <- total + cvec*quality
  }
  out <- .core_scale(total)
  list(coreness = out,
       core = .lip_cut(rowSums(mat), order(out, decreasing = TRUE)))
}

# The transition function itself, ascending, so that position `n` is the most
# core-like. `alpha` of 1 would make the tangent infinite, so it is held just
# below, which is a step function to any precision that matters here.
.transition_values <- function(n, alpha, beta){
  m <- seq_len(n)
  1/(1 + exp(-(m - n*beta)*tan(pi*min(alpha, 0.999)/2)))
}

# Finding the ordering that maximises the core quality is a search over
# permutations. Starting from the degree ordering, which is already a good
# guess, pairs are swapped whenever a swap improves the quality, and the
# sweeps stop as soon as one passes without an improvement.
#
# The quality is never recomputed from scratch. Since the matrix is symmetric,
# writing the quality as c'Ac and a swap as c + e(1_u - 1_v) gives
#   dR = 2e((Ac)_u - (Ac)_v) + e^2(A_uu - 2A_uv + A_vv)
# so each candidate costs a constant amount, and only an accepted swap costs
# the linear update of Ac. Without this the search is quartic in the number of
# nodes, and it is run once for every pair of parameters.
.transition_order <- function(mat, cstar, sweeps = 10L){
  n <- nrow(mat)
  nord <- order(rowSums(mat))
  cvec <- numeric(n)
  cvec[nord] <- cstar
  ac <- as.vector(mat %*% cvec)
  for(s in seq_len(sweeps)){
    improved <- FALSE
    for(i in seq_len(n-1)) for(j in seq(i+1, n)){
      u <- nord[i]; v <- nord[j]
      e <- cstar[j] - cstar[i]
      if(e == 0) next
      delta <- 2*e*(ac[u] - ac[v]) + e*e*(mat[u,u] - 2*mat[u,v] + mat[v,v])
      if(delta > 0){
        nord[c(i,j)] <- nord[c(j,i)]
        cvec[u] <- cvec[u] + e
        cvec[v] <- cvec[v] - e
        ac <- ac + e*(mat[,u] - mat[,v])
        improved <- TRUE
      }
    }
    if(!improved) break
  }
  nord
}

# Hub ####

#' @rdname method_coreness
#' @section Hub:
#'   In a directed network a node can be core in whom it reaches and
#'   peripheral in who reaches it. Elliott and colleagues therefore keep two
#'   core sets rather than one: an out-core of nodes that send to the core,
#'   and an in-core of nodes that receive from it.
#'
#'   The two are read from the hub and authority scores that
#'   [node_by_hub()] and [node_by_authority()] already provide: a hub is a
#'   node that points to good authorities, and an authority is a node that
#'   good hubs point to, which is the same mutual definition the two core sets
#'   have. Each set is then cut by the same rule the other methods use.
#'   With `direction = "all"` the returned coreness is the geometric mean of
#'   the two scores, and the core is the set of nodes in both.
#' @examples
#' coreness_hub(ison_networkers)
#' @export
coreness_hub <- function(.data, direction = c("all","out","in")){
  .data <- manynet::expect_nodes(.data)
  direction <- match.arg(direction)
  if(!manynet::is_directed(.data))
    manynet::snet_info("{.fn coreness_hub} distinguishes an out-core from an",
                       "in-core, which an undirected network does not,",
                       "so the two are the same here.")
  hub <- .core_scale(as.numeric(node_by_hub(.data)))
  auth <- .core_scale(as.numeric(node_by_authority(.data)))
  # Both cores are cut against the same directed block structure. What
  # separates them is the ordering: the out-core is swept in hub order, the
  # in-core in authority order.
  mat <- manynet::as_matrix(.data)
  degi <- rowSums(mat) + colSums(mat)
  directed <- if(manynet::is_directed(.data)) 2 else 1
  out_core <- .lip_cut(degi, order(hub, decreasing = TRUE), directed)
  in_core <- .lip_cut(degi, order(auth, decreasing = TRUE), directed)
  coreness <- switch(direction,
                     out = hub,
                     `in` = auth,
                     all = .core_scale(sqrt(hub*auth)))
  core <- switch(direction,
                 out = out_core,
                 `in` = in_core,
                 all = out_core & in_core)
  list(coreness = coreness, core = core,
       out_core = out_core, in_core = in_core)
}
