# Opinion measures ####

#' Measures of opinion dynamics
#' @name measure_opinion
#' @description
#'   These functions measure where and how fast the opinions in a network
#'   settle, when each node repeatedly takes a weighted mean of the opinions
#'   of the nodes it listens to, as `manynet::play_opinion()` does with
#'   `rule = "degroot"`:
#'
#'   - `node_by_opinion()` measures the opinion that each node ends with.
#'   - `net_by_convergence()` measures the speed at which the opinions settle.
#'
#'   See `node_by_influence()` and `net_by_influence()` for how much
#'   the first opinion of each node shapes the opinions at the end.
#'
#'   Each takes the influence matrix \eqn{W} from `manynet::to_influence()`,
#'   so a tie from \eqn{i} to \eqn{j} means that \eqn{i} listens to \eqn{j}.
#'   Where the network has a nodal attribute named "stubbornness",
#'   each node holds on to its first opinion by that share, from 0 to 1,
#'   as in `manynet::play_opinion()`.
#'   Without it, no node is stubborn, which is DeGroot's model.
#' @section Opinions at the end:
#'   With \eqn{S} the diagonal matrix of stubbornness, the opinions move as
#'   \deqn{x(t+1) = (I - S) W x(t) + S x(0)}
#'   With some stubbornness this settles at the equilibrium of
#'   Friedkin and Johnsen (1990),
#'   \deqn{x(\infty) = (I - (I - S) W)^{-1} S x(0)}
#'   Without stubbornness, every node in a connected network reaches one
#'   consensus: the mean of the first opinions, weighted by `node_by_influence()`.
#'   The result is found from the limit of the matrix powers, so it also
#'   holds where some nodes are stubborn and others are not.
#'   Where the opinions do not settle, for example on a path with no
#'   stubbornness, where they swing back and forth for ever,
#'   a warning says so and the result is the mean of the opinions they swing between.
#' @section Convergence:
#'   `net_by_convergence()` is \eqn{1 - |\lambda|}, where \eqn{|\lambda|} is the
#'   largest modulus among the eigenvalues of \eqn{(I - S) W}
#'   other than those equal to 1.
#'   This is the share of the distance to the opinions at the end that is
#'   closed at each step, in the long run.
#'   A value of 1 settles in one step, and a value of 0 never settles.
#'   It is about opinions and not about the convergence of an estimation.
#' @template param_data
#' @param attribute The name of a numeric nodal attribute that holds the
#'   first opinions. By default "opinions", as in `manynet::play_opinion()`.
#' @family opinion
#' @references
#' ## On opinion dynamics
#'   DeGroot, Morris H. 1974.
#'   "Reaching a consensus".
#'   _Journal of the American Statistical Association_ 69(345): 118-121.
#'   \doi{10.1080/01621459.1974.10480137}
#'
#'   Friedkin, Noah E., and Eugene C. Johnsen. 1990.
#'   "Social influence and opinions".
#'   _Journal of Mathematical Sociology_ 15(3-4): 193-206.
#'   \doi{10.1080/0022250X.1990.9990069}
#'
#'   Golub, Benjamin, and Matthew O. Jackson. 2010.
#'   "Naive learning in social networks and the wisdom of crowds".
#'   _American Economic Journal: Microeconomics_ 2(1): 112-149.
#'   \doi{10.1257/mic.2.1.112}
NULL

#' @rdname measure_opinion
#' @template node_measure
#' @examples
#' path <- manynet::add_node_attribute(manynet::create_explicit(A-B-C),
#'                                     "opinions", c(0, 0.5, 1))
#' node_by_opinion(manynet::add_node_attribute(path, "stubbornness", rep(0.5, 3)))
#' @export
node_by_opinion <- function(.data, attribute = "opinions"){
  .data <- manynet::expect_nodes(.data)
  .data <- .to_aggregated_css(.data)
  x0 <- .opinion_values(.data, attribute)
  limit <- .opinion_limit(.data)
  if(!limit$settles)
    manynet::snet_warn("The opinions in this network do not settle,",
                       "so this is the mean of the opinions they swing between.")
  out <- as.numeric(limit$M %*% x0)
  make_node_measure(out, .data, measure = "opinion at the end",
                    range = range(x0), normalization = "none")
}

#' @rdname measure_opinion
#' @template net_measure
#' @examples
#' net_by_convergence(manynet::create_ring(6))
#' net_by_convergence(manynet::create_filled(6))
#' @export
net_by_convergence <- function(.data){
  .data <- manynet::expect_nodes(.data)
  .data <- .to_aggregated_css(.data)
  A <- .opinion_step(.data)
  vals <- eigen(A, only.values = TRUE)$values
  rest <- Mod(vals[abs(vals - 1) >= 1e-8])
  out <- `if`(length(rest), 1 - max(rest), 1)
  out <- min(max(out, 0), 1)
  if(out < 1e-8)
    manynet::snet_info("The opinions in this network do not settle,",
                       "so the speed is 0.")
  make_network_measure(out, .data, call = deparse(sys.call()),
                       measure = "opinion convergence",
                       range = c(0, 1), normalization = "normalized")
}

# Opinion helpers ####

# The stubbornness of each node: the nodal attribute "stubbornness",
# or 0 for every node, with the checks of `manynet::play_opinion()`.
.opinion_stubbornness <- function(.data){
  n <- manynet::net_nodes(.data)
  if(!"stubbornness" %in% manynet::net_node_attributes(.data))
    return(rep(0, n))
  s <- manynet::node_attribute(.data, "stubbornness")
  if(is.logical(s)) s <- s * 1
  if(!is.numeric(s) || anyNA(s) || any(s < 0 | s > 1))
    manynet::snet_abort("The nodal attribute {.val stubbornness} must hold",
                        "one number from 0 to 1 for each node.")
  as.numeric(s)
}

# The first opinions, from a numeric nodal attribute
.opinion_values <- function(.data, attribute){
  if(!attribute %in% manynet::net_node_attributes(.data))
    manynet::snet_abort("There is no nodal attribute {.val {attribute}} in the network.",
                        "Please name the first opinions with {.arg attribute}.")
  x <- manynet::node_attribute(.data, attribute)
  if(is.logical(x)) x <- x * 1
  if(!is.numeric(x) || anyNA(x))
    manynet::snet_abort("The opinions in {.val {attribute}} must be numbers,",
                        "with no missing values.")
  as.numeric(x)
}

# The matrix of one step, (I - S) W, with W from `manynet::to_influence()`
.opinion_step <- function(.data){
  W <- manynet::as_matrix(manynet::to_influence(.data))
  dimnames(W) <- NULL
  s <- .opinion_stubbornness(.data)
  (1 - s) * W
}

# The matrix M of the opinions at the end, x(Inf) = M x(0).
# After t steps, x(t) = A^t x(0) + (I + A + ... + A^(t-1)) S x(0),
# with A = (I - S) W. Each pass doubles t, so that 64 passes reach 2^64 steps,
# also where some nodes are stubborn and others, in a closed group, are not.
# The steps are lazy, half the old opinion and half the new: this has the same
# end as the update itself where that settles, and it also settles where the
# update swings for ever (a periodic network), at the mean of the swing.
.opinion_limit <- function(.data){
  s <- .opinion_stubbornness(.data)
  A <- .opinion_step(.data)
  n <- nrow(A)
  L <- (diag(n) + A) / 2
  S <- diag(s / 2, n)
  P <- L               # L^t
  B <- diag(n)         # I + L + ... + L^(t-1)
  M <- P + B %*% S
  for(k in 1:64){
    B <- B + P %*% B
    P <- P %*% P
    new <- P + B %*% S
    done <- max(abs(new - M)) < 1e-12
    M <- new
    if(done) break
  }
  list(M = M, settles = .opinion_settles(A))
}

# Whether the update itself settles: no eigenvalue of A other than 1
# has a modulus of 1
.opinion_settles <- function(A){
  vals <- eigen(A, only.values = TRUE)$values
  !any(abs(vals - 1) >= 1e-8 & Mod(vals) > 1 - 1e-8)
}

# Influence ####

#' @rdname measure_central_eigen
#' @section Influence centrality:
#'   `node_by_influence()` measures how much the first opinion of each node
#'   shapes the opinions of every node at the end of an opinion dynamic
#'   such as `manynet::play_opinion()`.
#'   It is the mean, over all nodes, of the weight that the first opinion
#'   of each node has in their opinion at the end
#'   (see `node_by_opinion()`).
#'   Without stubbornness, it is the left eigenvector of the influence matrix
#'   \eqn{W} from `manynet::to_influence()`, and so the social power
#'   of Friedkin (1991) and the influence vector of Golub and Jackson (2010).
#'   Where the network has a nodal attribute named "stubbornness",
#'   it uses the equilibrium of Friedkin and Johnsen (1990).
#'
#'   This is not eigenvector centrality.
#'   Eigenvector centrality is the right eigenvector of the adjacency matrix,
#'   which counts the ties a node receives; influence is the left eigenvector
#'   of \eqn{W}, which weights each node by how much others listen to it.
#'   The two agree only on a regular undirected network.
#'   It has the same form as `node_by_pagerank()`, which is the left
#'   eigenvector of a walk that follows a tie with the same probability
#'   from every node.
#'
#'   By default the scores are divided by the largest score.
#'   With `normalized = FALSE` they are shares that sum to one.
#'
#'   Where the network has negative ties, which repel
#'   (see `manynet::to_influence()`), the weight of a first opinion in the
#'   opinion at the end of another node can be negative:
#'   it pulls that node away from it.
#'   This is influence too, so the score is the mean of the absolute weights.
#'   The mean of the signed weights would let a pull towards and a pull away
#'   cancel. In a balanced network of two camps, for example, the first
#'   opinion of each node has the same weight in every opinion at the end,
#'   positive in its own camp and negative in the other, so every node has
#'   the same influence and not none.
#'   Without negative ties, the two means are the same.
#'   Where the opinions all go to 0, as in a signed network that is not
#'   balanced, no first opinion has any weight at the end,
#'   and every score is 0.
#' @references
#' ## On influence centrality
#'   Friedkin, Noah E. 1991.
#'   "Theoretical foundations for centrality measures".
#'   _American Journal of Sociology_ 96(6): 1478-1504.
#'   \doi{10.1086/229694}
#'
#'   Golub, Benjamin, and Matthew O. Jackson. 2010.
#'   "Naive learning in social networks and the wisdom of crowds".
#'   _American Economic Journal: Microeconomics_ 2(1): 112-149.
#'   \doi{10.1257/mic.2.1.112}
#' @examples
#' node_by_influence(manynet::create_star(5))
#' @export
node_by_influence <- function(.data, normalized = TRUE){
  .data <- manynet::expect_nodes(.data)
  .data <- .to_aggregated_css(.data)
  out <- .influence_shares(.data)
  if(normalized && max(out) > 0) out <- out / max(out)
  make_node_measure(out, .data, measure = "influence centrality",
                    range = c(0, 1),
                    normalization = `if`(normalized, "scaled", "proportional"))
}

# The share of influence of each node: the mean of the absolute weights that
# its first opinion has in the opinions at the end, as a share of their sum.
# Absolute weights, so that a pull away along a negative tie counts as
# influence, and does not cancel a pull towards. Weights below 1e-10 are
# rounding noise and are set to 0, so that scaling does not make them large.
.influence_shares <- function(.data){
  M <- abs(.opinion_limit(.data)$M)
  M[M < 1e-10] <- 0
  out <- colMeans(M)
  total <- sum(out)
  if(total > 0) out / total else out
}

#' @rdname measure_centralisation_eigen
#' @section Influence centralisation:
#'   `net_by_influence()` measures the largest share of influence that
#'   any one node holds (see `node_by_influence()`).
#'   Golub and Jackson (2010) show that a crowd that learns by
#'   taking the mean of what others think is wise, reaching the truth as it
#'   grows, only where this largest share goes to 0.
#'   So a high value means a low capacity for the crowd to be wise.
#'   By default the share is normalised as \eqn{(\max - 1/n) / (1 - 1/n)},
#'   which is 0 where every node has the same influence
#'   and 1 where one node has all of it.
#'   With `normalized = FALSE` it is the largest share itself.
#'   As for `node_by_influence()`, a negative weight counts by its absolute
#'   value, and where no first opinion has any weight at the end,
#'   the value is 0.
#' @examples
#' net_by_influence(manynet::create_ring(6))
#' net_by_influence(manynet::create_star(6))
#' @export
net_by_influence <- function(.data, normalized = TRUE){
  .data <- manynet::expect_nodes(.data)
  .data <- .to_aggregated_css(.data)
  shares <- .influence_shares(.data)
  n <- length(shares)
  out <- max(shares)
  if(normalized) out <- `if`(n > 1, (out - 1/n) / (1 - 1/n), 0)
  out <- min(max(out, 0), 1)
  make_network_measure(out, .data, call = deparse(sys.call()),
                       measure = "influence centralisation",
                       range = c(0, 1),
                       normalization = `if`(normalized, "normalized", "none"))
}
