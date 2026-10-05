#' Methods for searching for the membership that minimises a cost
#'
#' @description
#'   These functions search the space of memberships for the one that
#'   minimises some cost, such as the misfit of a blockmodel:
#'
#'   - `search_iterated()` makes one random move at a time, keeps it where it
#'   lowers the cost, and restarts from a perturbation of the best membership
#'   every tenth step.
#'   - `search_tabu()` examines every move of one node to another group at
#'   each step, takes the best of them, and forbids the reverse move for a
#'   while, so that it can climb out of a local minimum.
#'
#'   These functions are not intended to be called directly,
#'   but are called within `node_in_block()`, `node_in_faction()`, and
#'   related functions.
#'   They are exported and listed here to provide more detailed documentation.
#' @name method_search
#' @param cost A function that takes a membership vector and
#'   returns a single number, the cost to be minimised.
#' @param init An integer vector giving each node's group,
#'   the membership from which the search begins.
#'   The search keeps the number of groups that it holds.
#' @template param_times
#' @param delta Optionally, a function that takes the current and a candidate
#'   membership vector, and returns the change in cost from the one to the
#'   other. Where the change can be found from the nodes that moved alone,
#'   this is much faster than calculating the cost of every candidate afresh.
#'   By default NULL, in which case `cost` is called on each candidate.
#' @returns
#'   An integer vector the length of `init`,
#'   giving each node's group in the best membership found,
#'   with that membership's cost as the attribute `"cost"`.
NULL

# The smallest change in cost that counts as one, which keeps rounding in a
# running sum of deltas from being taken as a gain.
.search_tol <- function(fit) sqrt(.Machine$double.eps) * max(1, abs(fit))

#' @rdname method_search
#' @section Iterated:
#'   An iterated local search. Each step makes a weak perturbation, which
#'   swaps two nodes between groups or moves one node out of a largest group,
#'   and keeps it only where it lowers the cost.
#'   Every tenth step, the search starts again from a stronger perturbation
#'   of the best membership so far, a number of successive weak perturbations
#'   of approximately the number of nodes divided by the number of groups.
#'   `times` is the number of steps.
#'
#'   The moves keep the groups at near-equal size: groups that begin equal
#'   stay equal, and groups that begin unequal move towards equal sizes.
#'   That is what `node_in_roulette()` needs, but it means this
#'   search cannot find, say, a small core beside a large periphery.
#'   Since the moves are drawn at random, repeated runs may return different
#'   memberships; set a seed to repeat one.
#' @examples
#' mat <- manynet::as_matrix(ison_adolescents)
#' cost <- function(memb) sum(mat[outer(memb, memb, "!=")])
#' search_iterated(cost, init = rep(1:2, 4), times = 50)
#' @export
search_iterated <- function(cost, init, times, delta = NULL){
  .search_iterated(cost, init, times, delta)
}

# `weak` is the weak perturbation. `.swapMove` alone keeps every group at the
# size it began with, which `node_in_roulette()` needs for given group sizes.
.search_iterated <- function(cost, init, times, delta = NULL,
                             weak = .weakPerturb){
  out <- init
  fit <- cost(out)
  tol <- .search_tol(fit)
  soln <- out
  soln_fit <- fit
  for(t in seq_len(times)){
    cand <- weak(soln)
    change <- if(is.null(delta)) cost(cand) - soln_fit else delta(soln, cand)
    if(change < -tol){
      soln <- cand
      soln_fit <- soln_fit + change
    }
    if(soln_fit < fit - tol){
      out <- soln
      fit <- soln_fit
    }
    if(t %% 10 == 0){
      soln <- .strongPerturb(out, weak = weak)
      soln_fit <- cost(soln)
    }
  }
  out <- as.integer(out)
  attr(out, "cost") <- cost(out)
  out
}

#' @rdname method_search
#' @section Tabu:
#'   A tabu search. Each step examines every move of one node into another
#'   group, and takes the move that lowers the cost most: the steepest
#'   descent. Where no move lowers the cost, it takes the move that raises it
#'   least, the mildest ascent, and so leaves a local minimum instead of
#'   stopping in it. To keep the next step from simply undoing that move,
#'   the node may not return to the group it left for the next 15 steps,
#'   unless doing so would beat the best membership found so far.
#'
#'   A run ends after 20 steps in a row that do not improve on its best
#'   membership, or after `times` steps, whichever comes first.
#'   The search makes 10 runs, the first from `init` and the others from
#'   random memberships with the same number of groups, and returns the best
#'   membership of them all.
#'   These three settings are those that the UCINET 'Factions' routine uses by
#'   default, and are fixed here.
#'
#'   No move may empty a group, but otherwise the groups are free to take any
#'   size. A run is deterministic given where it starts, but nine of the ten
#'   starts are random, so set a seed to repeat a result.
#'   Note that the search may still end in a local minimum, and that it
#'   returns one membership even where several share the lowest cost.
#' @references
#' ## On tabu search
#' Glover, Fred. 1989.
#' "Tabu Search — Part I."
#' _ORSA Journal on Computing_ 1(3): 190-206.
#' \doi{10.1287/ijoc.1.3.190}
#'
#' Glover, Fred. 1990.
#' "Tabu Search — Part II."
#' _ORSA Journal on Computing_ 2(1): 4-32.
#' \doi{10.1287/ijoc.2.1.4}
#' @examples
#' search_tabu(cost, init = rep(1:2, 4), times = 50)
#' @export
search_tabu <- function(cost, init, times, delta = NULL){
  tenure <- 15L   # steps for which a node may not return to the group it left
  patience <- 20L # steps in a row without improvement before a run ends
  starts <- 10L   # runs, the first from `init` and the others random
  n <- length(init)
  groups <- sort(unique(init))
  k <- length(groups)
  out <- init
  fit <- cost(out)
  tol <- .search_tol(fit)
  if(k > 1 && n > k) for(s in seq_len(starts)){
    soln <- if(s == 1) init else .random_membership(n, groups)
    soln_fit <- cost(soln)
    run_fit <- soln_fit
    if(soln_fit < fit - tol){
      out <- soln
      fit <- soln_fit
    }
    # the step until which node i may not move into group g
    tabu <- matrix(0L, n, k)
    stalled <- 0L
    for(t in seq_len(times)){
      at <- match(soln, groups)
      sizes <- tabulate(at, k)
      best_change <- Inf
      best_node <- 0L
      best_to <- 0L
      for(i in seq_len(n)){
        if(sizes[at[i]] == 1) next
        for(g in seq_len(k)[-at[i]]){
          cand <- soln
          cand[i] <- groups[g]
          change <- if(is.null(delta)) cost(cand) - soln_fit else
            delta(soln, cand)
          if(change >= best_change) next
          # a tabu move is still taken where it beats the best found so far
          if(tabu[i, g] >= t && soln_fit + change >= fit - tol) next
          best_change <- change
          best_node <- i
          best_to <- g
        }
      }
      if(best_node == 0L) break
      tabu[best_node, at[best_node]] <- t + tenure
      soln[best_node] <- groups[best_to]
      soln_fit <- soln_fit + best_change
      if(soln_fit < run_fit - tol){
        run_fit <- soln_fit
        stalled <- 0L
      } else stalled <- stalled + 1L
      if(soln_fit < fit - tol){
        out <- soln
        fit <- soln_fit
      }
      if(stalled >= patience) break
    }
  }
  out <- as.integer(out)
  attr(out, "cost") <- cost(out)
  out
}

# A random membership in which every group holds at least one node, and the
# groups are otherwise free to take any size.
.random_membership <- function(n, groups){
  k <- length(groups)
  out <- c(groups, groups[sample.int(k, n - k, replace = TRUE)])
  out[sample.int(n)]
}
