# Recursive role similarity ####

#' Methods for calculating regularity
#' @name method_regularity
#' @description
#'   These functions calculate how regularly equivalent each pair of nodes is,
#'   returning a similarity matrix that [node_in_regular()] then clusters.
#'
#'   - `regularity_rolesim()` calculates RoleSim similarity.
#'   - `regularity_rege()` calculates REGE similarity.
#'
#'   Both are recursive: two nodes are similar to the extent that their alters
#'   are similar, which is the defining property of regular equivalence.
#'   They differ in how they pair up two nodes' alters.
#' @template param_data
#' @template param_decay
#' @param beta Deprecated; use `decay` instead.
#' @param iterations Integer number of iterations.
#'   By default 3 for `regularity_rege()`; `regularity_rolesim()` iterates to convergence.
#' @returns A square similarity matrix with one row and column per node.
#' @references
#' ## On RoleSim
#' Jin, Ruoming, Victor E. Lee, and Hui Hong. 2011.
#' "Axiomatic ranking of network role similarity".
#' _Proceedings of the 17th ACM SIGKDD International Conference on Knowledge
#' Discovery and Data Mining_: 922-930.
#' \doi{10.1145/2020408.2020561}
#'
#' ## On REGE
#' White, Douglas R., and Karl P. Reitz. 1983.
#' "Graph and semigroup homomorphisms on networks of relations".
#' _Social Networks_ 5(2): 193-234.
#' \doi{10.1016/0378-8733(83)90025-4}
#' @family methods
NULL

#' @rdname method_regularity
#' @section RoleSim:
#'   RoleSim pairs up two nodes' alters by finding the _maximal matching_
#'   between them, that is, the one-to-one pairing that maximises total
#'   similarity, and then averages over it:
#'   \deqn{s(u,v) = (1-\delta) \frac{\sum_{(x,y) \in M} s(x,y)}{|N(u)| + |N(v)| - |M|} + \delta}
#'   where \eqn{M} is that matching and \eqn{\delta} is `decay`,
#'   which RoleSim calls \eqn{\beta}; by default 0.15.
#'   Because each alter can be used only once, two nodes are similar only if
#'   their neighbourhoods can be lined up as wholes.
#'
#'   RoleSim satisfies the automorphic confirmation property, meaning that
#'   automorphically equivalent nodes always score 1, and it is a metric.
#'   It converges to a unique solution regardless of where it starts,
#'   so the result does not depend on initialisation.
#' @export
regularity_rolesim <- function(.data, decay = 0.15, beta = NULL){
  .data <- manynet::expect_nodes(.data)
  decay <- check_decay(resolve_decay(decay, beta, "beta"))
  mat <- manynet::as_matrix(manynet::to_unweighted(manynet::to_multilevel(.data)))
  n <- nrow(mat)
  nbrs <- .neighbourhoods(mat, manynet::is_directed(.data))
  sim <- matrix(1, n, n) # all nodes begin maximally similar
  for(it in seq_len(100L)){
    new <- .rolesim_step(sim, nbrs, decay, n)
    if(max(abs(new - sim)) < 1e-6){ sim <- new; break }
    sim <- new
  }
  dimnames(sim) <- list(rownames(mat), rownames(mat))
  sim
}

.rolesim_step <- function(sim, nbrs, beta, n){
  new <- diag(n)
  for(u in seq_len(n)) for(v in seq_len(u)){
    # average the matchings over each direction of tie, so that in a directed
    # network nodes must match on both whom they reach and who reaches them
    scores <- vapply(nbrs, function(nb){
      nu <- nb[[u]]; nv <- nb[[v]]
      if(length(nu) == 0 && length(nv) == 0) return(1)
      if(length(nu) == 0 || length(nv) == 0) return(0)
      matched <- .greedy_matching(sim[nu, nv, drop = FALSE])
      matched/(length(nu) + length(nv) - min(length(nu), length(nv)))
    }, FUN.VALUE = numeric(1))
    new[u, v] <- new[v, u] <- (1-beta)*mean(scores) + beta
  }
  diag(new) <- 1
  new
}

# Greedily approximate the maximal matching between two neighbourhoods,
# repeatedly taking the most similar remaining pair. The RoleSim authors show
# this is a bounded approximation of the optimal (Hungarian) matching at a
# fraction of the cost.
.greedy_matching <- function(sub){
  total <- 0
  while(nrow(sub) > 0 && ncol(sub) > 0){
    best <- which.max(sub)
    i <- ((best - 1) %% nrow(sub)) + 1
    j <- ((best - 1) %/% nrow(sub)) + 1
    total <- total + sub[i, j]
    sub <- sub[-i, -j, drop = FALSE]
  }
  total
}

#' @rdname method_regularity
#' @section REGE:
#'   REGE instead pairs each alter with its _best_ counterpart, allowing the
#'   same alter to be used more than once:
#'   \deqn{s(u,v) = \frac{\sum_{x \in N(u)} \max_{y \in N(v)} s(x,y) + \sum_{y \in N(v)} \max_{x \in N(u)} s(x,y)}{|N(u)| + |N(v)|}}
#'
#'   Matching with replacement makes REGE more permissive than RoleSim: a node
#'   with many alters can be judged similar to one with few, if those few
#'   resemble all of the many. Which behaviour is wanted depends on whether
#'   having more alters of a kind is itself part of the role.
#'
#'   REGE is the algorithm UCINET implements, so use it when comparing results
#'   against that software. Unlike RoleSim it has no convergence guarantee and
#'   is sensitive to the number of iterations, so this is fixed rather than run
#'   to convergence.
#'
#'   Note that REGE is defined for _valued_ networks, and weights each matched
#'   pair by how similar the two ties' strengths are.
#'   On an unweighted, connected network it is degenerate: since every node has
#'   an alter that matches every other node's alter perfectly, all nodes come
#'   out maximally equivalent, which is the correct but uninformative answer
#'   that the maximal regular equivalence of a connected graph is a single
#'   class. Use `regularity_rolesim()` for unweighted networks.
#' @export
regularity_rege <- function(.data, iterations = 3){
  .data <- manynet::expect_nodes(.data)
  mat <- manynet::as_matrix(manynet::to_multilevel(.data))
  if(!manynet::is_weighted(.data) && manynet::is_connected(.data))
    manynet::snet_warn("REGE is degenerate on unweighted connected networks,",
                       "where all nodes are maximally regularly equivalent.",
                       "Consider {.fn regularity_rolesim} instead.")
  n <- nrow(mat)
  nbrs <- .neighbourhoods(mat, manynet::is_directed(.data))
  sim <- matrix(1, n, n) # all nodes begin maximally similar
  for(it in seq_len(iterations)){
    new <- diag(n)
    for(u in seq_len(n)) for(v in seq_len(u)){
      # weight each matched pair by how well the two ties' strengths agree,
      # so that equivalence depends on the intensity of ties as well as
      # their existence
      agree <- outer(nbrs[[1]][[u]], nbrs[[1]][[v]], function(a, b)
        pmin(mat[u, a], mat[v, b]) + pmin(mat[a, u], mat[b, v]))
      nu <- nbrs[[1]][[u]]; nv <- nbrs[[1]][[v]]
      if(length(nu) == 0 && length(nv) == 0){
        new[u, v] <- new[v, u] <- 1
      } else if(length(nu) == 0 || length(nv) == 0){
        new[u, v] <- new[v, u] <- 0
      } else {
        wsim <- agree * sim[nu, nv, drop = FALSE]
        # each alter takes its best counterpart, with replacement
        num <- sum(apply(wsim, 1, max)) + sum(apply(wsim, 2, max))
        # normalise by the total tie strength each node has to give, so that
        # a pair scores 1 only if all of it can be matched at equal strength
        den <- sum(mat[u, nu]) + sum(mat[nu, u]) +
          sum(mat[v, nv]) + sum(mat[nv, v])
        new[u, v] <- new[v, u] <- if(den == 0) 0 else num/den
      }
    }
    diag(new) <- 1
    sim <- new
  }
  dimnames(sim) <- list(rownames(mat), rownames(mat))
  sim
}

# A list of neighbourhood sets to match on. Undirected networks have one,
# directed networks two, so that nodes must match on both their outgoing and
# their incoming ties to count as regularly equivalent.
.neighbourhoods <- function(mat, directed){
  n <- nrow(mat)
  outs <- lapply(seq_len(n), function(i) which(mat[i,] > 0))
  if(!directed) return(list(outs))
  ins <- lapply(seq_len(n), function(i) which(mat[,i] > 0))
  list(outs, ins)
}
