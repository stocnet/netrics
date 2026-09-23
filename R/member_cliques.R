#' Memberships in maximally diverse cliques
#' @description 
#'   These functions create a vector of nodes' memberships in
#'   cliques:
#'   
#'   - `node_in_roulette()` assigns nodes to maximally diverse groups.
#'   
#' @section Maximally diverse grouping problem: 
#'   This well known computational problem is a NP-hard problem
#'   with a number of relevant applications, 
#'   including the formation of groups of students that have encountered
#'   each other least or least recently.
#'   Essentially, the aim is to return a membership of nodes in cliques
#'   that minimises the sum of their previous (weighted) ties:
#'   
#'   \deqn{\sum_{g=1}^{m} \sum_{i=1}^{n-1} \sum_{j=i+1}^{n} x_{ij} y_{ig} y_{jg}}
#'   
#'   where \eqn{y_{ig} = 1} if node \eqn{i} is in group \eqn{g}, and 0 otherwise.
#'   
#'   \eqn{x_{ij}} is the existing network data.
#'   If this is an empty network, the function will just return cliques.
#' 
#'   A form of the Lai and Hao (2016) iterated maxima search (IMS) is used here.
#'   This performs well for small and moderately sized networks.
#'   It includes both weak and strong perturbations to an initial solution
#'   to ensure that a robust solution from the broader state space is identified.
#'   The user is referred to Lai and Hao (2016) and Lai et al (2021) for more details.
#' @section Repeated rounds:
#'   The history of earlier rounds is held in the network data.
#'   A weighted network costs each pair by the weight of their tie.
#'   A longitudinal network, with a wave for each earlier round,
#'   or a dynamic network, with a `time` (or `begin` and `end`) for each tie,
#'   costs each tie by `decay` for each wave or unit of time before the latest.
#'   With `decay = 1`, every earlier meeting costs the same;
#'   with `decay = 0`, only the latest meetings cost anything.
#'   A date is discounted for each day.
#'   
#'   To record a new round, add the membership's ties to the network
#'   as a new wave, as in the examples.
#' @section Mixing on an attribute:
#'   Where an `attribute` is given, the function also places nodes that
#'   differ on that attribute in the same group,
#'   as in the diversity-based assignment of the grouper package.
#'   Both costs are first scaled to \eqn{[0,1]}.
#'   A categorical attribute costs a pair 1 where their categories differ,
#'   and a numeric attribute costs a pair their absolute difference over its maximum.
#'   `balance` then weighs the attribute against the ties:
#'   0 considers only the ties, and 1 only the attribute.
#' @template param_data
#' @template node_member
#' @name member_cliques
NULL

#' @rdname member_cliques
#' @param groups An integer indicating the number of groups desired.
#' @param num_groups Deprecated. The former spelling of `groups`.
#'   Still accepted, but warns; please use `groups` instead.
#' @param group_size An integer indicating the desired size of most of the groups.
#'   Note that if the number of nodes is not divisible into groups of equal size,
#'   there may be some larger or smaller groups.
#' @template param_times
#' @param decay A proportion between 0 and 1 by which a tie in a longitudinal
#'   or dynamic network is discounted for each wave or unit of time before the latest.
#'   By default 1, so that every earlier tie costs the same.
#' @param attribute Optionally, the name of a node attribute, or a vector
#'   with a value for each node, on which the groups should be mixed.
#' @param balance A proportion between 0 and 1 that weighs mixing on the
#'   `attribute` against keeping tied nodes apart. By default 0.5.
#' @details
#'   `times` defaults to the number of nodes multiplied by the number of groups.
#'   This heuristic may be insufficient for small networks and numbers of groups,
#'   and burdensome for large ones, but can be overwritten.
#'   Each iteration makes a weak perturbation, and keeps it only
#'   if it improves the objective function.
#'   At every 10th iteration, the search starts again from a stronger
#'   perturbation of the best solution so far, a number of successive changes
#'   approximately the number of nodes divided by the number of groups.
#' @references
#' ## On the maximally diverse grouping problem
#' Lai, Xiangjing, and Jin-Kao Hao. 2016. 
#' “Iterated Maxima Search for the Maximally Diverse Grouping Problem.” 
#' _European Journal of Operational Research_ 254(3):780–800. 
#' \doi{10.1016/j.ejor.2016.05.018}.
#' 
#' Lai, Xiangjing, Jin-Kao Hao, Zhang-Hua Fu, and Dong Yue. 2021. 
#' “Neighborhood Decomposition Based Variable Neighborhood Search and Tabu Search for Maximally Diverse Grouping.” 
#' _European Journal of Operational Research_ 289(3):1067–86. 
#' \doi{10.1016/j.ejor.2020.07.048}.
#' @examples
#' node_in_roulette(ison_adolescents, groups = 3)
#' # Mix the groups on an attribute
#' marvel_friends <- to_unsigned(to_uniplex(fict_marvel, "relationship"), "positive")
#' node_in_roulette(marvel_friends, groups = 3, attribute = "Gender")
#' # Record three rounds as waves, discounting older meetings by half
#' net <- create_empty(8)
#' for(round in 1:3){
#'   grp <- node_in_roulette(net, group_size = 4, decay = 0.5)
#'   pairs <- which(as_matrix(grp) == 1 & upper.tri(diag(8)), arr.ind = TRUE)
#'   net <- add_ties(net, pairs, attr_list = list(time = rep(round, nrow(pairs))))
#' }
#' net
#' @export
node_in_roulette <- function(.data, groups, group_size, times = NULL,
                             decay = 1, attribute = NULL, balance = 0.5,
                             num_groups = NULL){
  .data <- manynet::expect_nodes(.data)
  if(manynet::is_twomode(.data))
    manynet::snet_abort("{.fn node_in_roulette} groups the nodes of a one-mode network.")
  # Read before `resolve_groups()` assigns, since assigning to a formal that
  # was missing makes `missing()` FALSE from then on.
  has_groups <- !missing(groups) || !is.null(num_groups)
  groups <- resolve_groups(if(missing(groups)) NULL else groups, num_groups)
  if(!has_groups & missing(group_size)){
    manynet::snet_abort("Either {.arg groups} must indicate the number of groups",
                        "desired, or {.arg group_size} the desired average size",
                        "of the groups.")
  }
  check_decay(decay)
  n <- manynet::net_nodes(.data)
  my_vec <- sample(seq.int(n))
  # Initial partition
  if(has_groups){
    out <- cut(seq_along(my_vec), groups, labels = FALSE)[my_vec]
  } else {
    out <- ceiling(seq_along(my_vec) / group_size)[my_vec]
  }
  if(is.null(times)) times <- n * max(out)
  # Get fitness
  mat <- .roulette_cost(.data, decay)
  if(!is.null(attribute)){
    check_decay(balance, "balance")
    mat <- .roulette_mix(mat, .roulette_dissimilarity(.data, attribute), balance)
  }
  fit <- .clique_cost(out, mat)
  # An iterated local search: `soln` descends by weak moves that improve it,
  # and every 10th iteration restarts from a strong perturbation of the best.
  # A weak move changes one or two nodes, so its change in cost is found from
  # their rows alone. The tolerance keeps rounding from being taken as a gain.
  tol <- sqrt(.Machine$double.eps) * max(1, abs(mat))
  soln <- out
  soln_fit <- fit
  for(t in seq.int(times)){
    cand <- .weakPerturb(soln)
    delta <- .clique_delta(mat, soln, cand)
    if(delta < -tol){
      soln <- cand
      soln_fit <- soln_fit + delta
    }
    if(soln_fit < fit - tol){
      out <- soln
      fit <- soln_fit
    }
    if(t %% 10 == 0){
      soln <- .strongPerturb(out)
      soln_fit <- .clique_cost(soln, mat)
    }
  }
  make_node_member(out, .data)
}

.to_cliques <- function(member){
  (member == t(matrix(member, length(member), length(member))))*1
}

# The cost of a membership: the sum of the costs between every pair of nodes
# in the same group, with each pair counted in both directions.
.clique_cost <- function(member, mat){
  sum(.to_cliques(member) * mat)
}

# The change in `.clique_cost()` from `old` to `new`, read from the rows of the
# nodes that moved. `mat` must be symmetric with a zero diagonal. A pair of one
# moved and one unmoved node is counted in both directions, so twice, while a
# pair of two moved nodes appears in both directions among their own rows.
.clique_delta <- function(mat, old, new){
  moved <- which(old != new)
  if(!length(moved)) return(0)
  part <- function(member){
    rows <- mat[moved, , drop = FALSE] * outer(member[moved], member, "==")
    2 * sum(rows[, -moved, drop = FALSE]) + sum(rows[, moved, drop = FALSE])
  }
  part(new) - part(old)
}

# The cost of placing each pair of nodes in the same group.
# A network that records time weights each tie by `decay` for each wave or
# unit of time before the latest, so that recent meetings cost the most.
# The matrix is made symmetric, since a group holds both nodes of a pair.
.roulette_cost <- function(.data, decay){
  g <- manynet::as_igraph(.data)
  when <- .tie_moments(g)
  if(is.null(when)){
    mat <- manynet::as_matrix(.data)
  } else {
    if(decay < 1)
      manynet::snet_info("Discounting each tie by a {.arg decay} of {decay}",
                         "for each {attr(when, 'unit')} before the latest.")
    n <- igraph::vcount(g)
    w <- igraph::edge_attr(g, "weight")
    if(is.null(w)) w <- rep(1, igraph::ecount(g))
    sign <- igraph::edge_attr(g, "sign")
    if(!is.null(sign)) w <- w * sign
    # A tie still ongoing has no end, and so is as recent as the latest.
    age <- max(when, na.rm = TRUE) - when
    age[is.na(age)] <- 0
    w <- w * decay^age
    el <- igraph::as_edgelist(g, names = FALSE)
    cell <- (el[, 2] - 1) * n + el[, 1]
    sums <- rowsum(w, cell)
    mat <- matrix(0, n, n)
    mat[as.numeric(rownames(sums))] <- sums[, 1]
  }
  mat <- unname(mat + t(mat))
  diag(mat) <- 0
  mat
}

# When each tie was observed, as a number, or NULL for a static network.
# A panel counts its waves in order, since waves may be named by the year
# they were observed in. A stream of events counts its units of time, and
# a date its days.
.tie_moments <- function(g){
  ties <- igraph::edge_attr_names(g)
  if(manynet::is_longitudinal(g)){
    col <- intersect(c("time", "wave", "panel"), ties)[1]
    when <- igraph::edge_attr(g, col)
    return(structure(match(when, sort(unique(when))), unit = "wave"))
  }
  if(!manynet::is_dynamic(g)) return(NULL)
  col <- intersect(c("time", "end", "begin"), ties)[1]
  if(is.na(col)) return(NULL)
  when <- igraph::edge_attr(g, col)
  unit <- if(inherits(when, c("POSIXt", "Date"))) "day" else "unit of time"
  if(inherits(when, "POSIXt")) when <- as.numeric(as.POSIXct(when)) / 86400
  if(inherits(when, "Date")) when <- as.numeric(when)
  if(!is.numeric(when))
    manynet::snet_abort("The {.val {col}} of the ties must be numeric or a date",
                        "for {.arg decay} to discount older ties.")
  structure(when, unit = unit)
}

# How different each pair of nodes is on an attribute, on [0,1]:
# 1 where a category differs, or the absolute difference over its maximum.
.roulette_dissimilarity <- function(.data, attribute){
  attr <- if(is.character(attribute) && length(attribute) == 1)
    manynet::node_attribute(.data, attribute) else attribute
  if(length(attr) != manynet::net_nodes(.data))
    manynet::snet_abort("{.arg attribute} must name a node attribute,",
                        "or give one value for each node.")
  if(is.numeric(attr)){
    out <- abs(outer(attr, attr, "-"))
    top <- max(out, na.rm = TRUE)
    if(top > 0) out <- out / top
  } else out <- outer(attr, attr, "!=") * 1
  out[is.na(out)] <- 0
  diag(out) <- 0
  out
}

# Trades placing tied nodes apart against placing dissimilar nodes together.
# The costs are scaled to [0,1] first, so that `balance` weighs like with like.
.roulette_mix <- function(mat, dissimilarity, balance){
  top <- max(abs(mat))
  if(top > 0) mat <- mat / top
  manynet::snet_info("Mixing the groups on the attribute, with a",
                     "{.arg balance} of {balance} against the ties.")
  (1 - balance) * mat - balance * dissimilarity
}
