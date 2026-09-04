# Motifs ####

#' Motifs of network hierarchy
#' @name motif_hierarchy
#' @description
#'   `net_x_hierarchy()` collects the measures of hierarchy into a single motif,
#'   which can be used to compare the relative hierarchy of different networks.
#'   The measures of hierarchy are:
#'   - `net_by_connectedness()` measures the proportion of dyads in the network
#'   that are reachable to one another,
#'   or the degree to which network is a single component.
#'   - `net_by_efficiency()` measures the Krackhardt efficiency score.
#'   - `net_by_upperbound()` measures the Krackhardt (least) upper bound
#'   score.
#'   - `net_by_reciprocity()` measures the proportion of ties in the network that 
#'   are reciprocated,
#'   which is a measure of the degree to which the network is non-hierarchical.
#' 
#' @template param_data
#' @family hierarchy
#' @template net_motif
#' @references
#' ## On hierarchy
#' Krackhardt, David. 1994. 
#' Graph theoretical dimensions of informal organizations. 
#' In Carley and Prietula (eds) _Computational Organizational Theory_, 
#' Hillsdale, NJ: Lawrence Erlbaum Associates. Pp. 89-111. 
#' 
#' Everett, Martin, and David Krackhardt. 2012. 
#' “A second look at Krackhardt's graph theoretical dimensions of informal organizations.”
#' _Social Networks_, 34: 159-163.
#' \doi{10.1016/j.socnet.2011.10.006}
#' @examples 
#' net_x_hierarchy(ison_networkers)
NULL

#' @rdname motif_hierarchy 
#' @export
net_x_hierarchy <- function(.data){
  .data <- manynet::expect_nodes(.data)
  out <- data.frame(Connectedness = net_by_connectedness(.data),
                    InvReciprocity = 1 - net_by_reciprocity(.data),
                    Efficiency = net_by_efficiency(.data),
                    LeastUpperBound = net_by_upperbound(.data))
  make_network_motif(out, .data)
}

# Measures ####

#' Measures of hierarchy
#' @name measure_hierarchy
#' @description
#'   These functions, together with `net_reciprocity()`, are used jointly to
#'   measure how hierarchical a network is:
#'   
#'   - `net_by_connectedness()` measures the proportion of dyads in the network
#'   that are reachable to one another, 
#'   or the degree to which network is a single component.
#'   - `net_by_efficiency()` measures the Krackhardt efficiency score.
#'   - `net_by_upperbound()` measures the Krackhardt (least) upper bound score.
#' 
#' @template param_data
#' @template net_measure
#' @family hierarchy
#' @references
#' ## On hierarchy
#' Krackhardt, David. 1994. 
#' Graph theoretical dimensions of informal organizations. 
#' In Carley and Prietula (eds) _Computational Organizational Theory_, 
#' Hillsdale, NJ: Lawrence Erlbaum Associates. Pp. 89-111. 
#' 
#' Everett, Martin, and David Krackhardt. 2012. 
#' “A second look at Krackhardt's graph theoretical dimensions of informal organizations.”
#' _Social Networks_, 34: 159-163.
#' \doi{10.1016/j.socnet.2011.10.006}
#' @examples 
#' net_by_connectedness(ison_networkers)
#' 1 - net_by_reciprocity(ison_networkers)
#' net_by_efficiency(ison_networkers)
#' net_by_upperbound(ison_networkers)
#' @section Signed networks:
#'   These measures read a tie as a distance, and a negative tie is hostility
#'   rather than a channel along which cohesion travels.
#'   Where the network is signed, they therefore consider only the positive
#'   ties, and say so.
#'   Use [manynet::to_unsigned()] first to control this yourself.

NULL

#' @rdname measure_hierarchy 
#' @export
net_by_connectedness <- function(.data){
  .data <- manynet::expect_nodes(.data)
  .data <- .to_positive(.data)
  dists <- igraph::distances(manynet::as_igraph(.data))
  make_network_measure(1 - sum(dists==Inf)/sum(dists!=0),
                       .data,
                       call = deparse(sys.call()),
                       measure = "connectedness", range = c(0, 1),
                       normalization = "normalized")
}

#' @rdname measure_hierarchy
#' @section Efficiency:
#'   A perfect hierarchy is a tree: every node but the root has exactly one
#'   superior, and there are no ties to spare. Krackhardt's efficiency asks how
#'   close a network comes to that, by counting the ties it carries in excess of
#'   the minimum needed to hold its components together, as a proportion of the
#'   most excess ties it could possibly carry:
#'   \deqn{E = 1 - \frac{|E| - \sum_i (N_i - 1)}{\sum_i \left(M_i - (N_i - 1)\right)}}
#'   where \eqn{N_i} is the size of weak component \eqn{i} and \eqn{M_i} the
#'   number of ties possible within it. A tree or forest scores 1, and a
#'   complete network 0.
#' @export
net_by_efficiency <- function(.data) {
  .data <- manynet::expect_nodes(.data)
  object <- manynet::as_igraph(.data)
  comps <- igraph::components(object, mode = "weak")
  sizes <- comps$csize
  # Every component needs N_i - 1 ties to hold together; anything beyond that
  # is excess, and efficiency is the share of possible excess left unused.
  spanning <- sum(sizes - 1)
  possible <- if(manynet::is_directed(object)) sizes*(sizes-1) else sizes*(sizes-1)/2
  headroom <- sum(possible - (sizes - 1))
  out <- if(headroom == 0) 1 else 1 - (manynet::net_ties(object) - spanning)/headroom
  make_network_measure(out, .data,
                       call = deparse(sys.call()),
                       measure = "efficiency", range = c(0, 1),
                       normalization = "normalized")
}

#' @rdname measure_hierarchy 
#' @export
net_by_upperbound <- function(.data) {
  .data <- manynet::expect_nodes(.data)
  dists <- igraph::distances(.data, mode = "in")
  dists[is.infinite(dists)] <- 0
  dists <- dists[order(rowSums(dists)), order(rowSums(dists))]
  if (max(colSums(dists > 0)) / (manynet::net_nodes(.data)-1) == 1){
    out <- 1
  } else {
    out <- apply(utils::combn(2:nrow(dists), 2), 2, 
                 function(x){
                   ubs <- dists[x,]>0
                   any(ubs[1,]*ubs[2,]==1)
                 })
    out <- sum(out)/length(out)
  }
  make_network_measure(out, .data,
                       call = deparse(sys.call()),
                       measure = "least upper boundedness", range = c(0, 1),
                       normalization = "normalized")
}