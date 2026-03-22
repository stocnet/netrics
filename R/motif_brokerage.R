# Motifs ####

#' Motifs of nodes brokerage
#' @description
#'   `node_x_brokerage()` returns the Gould-Fernandez brokerage
#'   roles played by nodes in a network.
#'   
#' @name motif_brokerage_node
#' @template param_data
#' @template param_memb
#' @family brokerage
#' @template node_motif
#' @param standardized Whether the score should be standardized
#'   into a _z_-score indicating how many standard deviations above
#'   or below the average the score lies.
NULL

#' @rdname motif_brokerage_node 
#' @references 
#' ## On brokerage motifs
#' Gould, Roger V., and Roberto M. Fernandez. 1989. 
#' “Structures of Mediation: A Formal Approach to Brokerage in Transaction Networks.” 
#' _Sociological Methodology_, 19: 89-126.
#' \doi{10.2307/270949}
#' 
#' Jasny, Lorien, and Mark Lubell. 2015. 
#' “Two-Mode Brokerage in Policy Networks.” 
#' _Social Networks_ 41:36–47. 
#' \doi{10.1016/j.socnet.2014.11.005}
#' @examples 
#' node_x_brokerage(ison_networkers, "Discipline")
#' @export
node_x_brokerage <- function(.data, membership, standardized = FALSE){
  thisRequires("sna")
  .data <- manynet::expect_nodes(.data)
  membership <- .resolve_membership(.data, membership)
  if(!manynet::is_twomode(.data)){
    out <- sna::brokerage(manynet::as_network(.data),
                          membership)
    out <- if(standardized) out$z.nli else out$raw.nli
    colnames(out) <- c("Coordinator", "Itinerant", "Gatekeeper", 
                       "Representative", "Liaison", "Total")
  } else {
    out <- suppressWarnings(sna::brokerage(manynet::as_network(manynet::to_mode1(.data)),
                                           membership))
    out <- if(standardized) out$z.nli else out$raw.nli
    out <- out[,-4]
    colnames(out) <- c("Coordinator", "Itinerant", "Gatekeeper", 
                       "Liaison", "Total")
  }
  make_node_motif(out, .data)
}

#' Motifs of network brokerage
#' @description
#'   `net_x_brokerage()` returns the Gould-Fernandez brokerage
#'   roles in a network.
#'   
#' @name motif_brokerage_net
#' @template param_data
#' @template param_memb
#' @family brokerage
#' @template net_motif
#' @param standardized Whether the score should be standardized
#'   into a _z_-score indicating how many standard deviations above
#'   or below the average the score lies.
NULL

#' @rdname motif_brokerage_net 
#' @examples 
#' net_x_brokerage(ison_networkers, "Discipline")
#' @export
net_x_brokerage <- function(.data, membership, standardized = FALSE){
  thisRequires("sna")
  .data <- manynet::expect_nodes(.data)
  membership <- .resolve_membership(.data, membership)
  if(!manynet::is_twomode(.data)){
    out <- sna::brokerage(manynet::as_network(.data),
                          membership)
    out <- if(standardized) out$z.gli else out$raw.gli
    names(out) <- c("Coordinator", "Itinerant", "Gatekeeper", 
                    "Representative", "Liaison", "Total")
  } else {
    out <- suppressWarnings(sna::brokerage(manynet::as_network(manynet::to_mode1(.data)),
                                           membership))
    out <- if(standardized) out$z.gli else out$raw.gli
    names(out) <- c("Coordinator", "Itinerant", "Gatekeeper", 
                    "Representative", "Liaison", "Total")
  }
  make_network_motif(out, .data)
}

# Measures ####

#' Measures of brokerage
#' @description
#'   These functions include ways to measure nodes' brokerage activity and
#'   exclusivity in a network: 
#'   
#'   - `node_by_brokering_activity()` measures nodes' brokerage activity.
#'   - `node_by_brokering_exclusivity()` measures nodes' brokerage exclusivity. 
#'   
#' @name measure_brokerage
#' @template param_data
#' @template param_memb
#' @template node_measure
#' @family brokerage
NULL

#' @rdname measure_brokerage 
#' @references
#' ## On brokerage activity and exclusivity
#'   Hamilton, Matthew, Jacob Hileman, and Orjan Bodin. 2020.
#'   "Evaluating heterogeneous brokerage: New conceptual and methodological approaches
#'   and their application to multi-level environmental governance networks"
#'   _Social Networks_ 61: 1-10.
#'   \doi{10.1016/j.socnet.2019.08.002}
#' @export
node_by_brokering_activity <- function(.data, membership){
  .data <- manynet::expect_nodes(.data)
  twopaths <- .to_twopaths(.data)
  if(!missing(membership)){
    membership <- .resolve_membership(.data, membership)
    twopaths$from_memb <- membership[`if`(manynet::is_labelled(.data),
                                                                          match(twopaths$from, manynet::node_names(.data)),
                                                                          twopaths$from)]
    twopaths$to_memb <- membership[`if`(manynet::is_labelled(.data),
                                                                        match(twopaths$to.y, manynet::node_names(.data)),
                                                                        twopaths$to.y)]
    twopaths <- dplyr::filter(twopaths, from_memb != to_memb)
  }
  # tabulate brokerage
  out <- c(table(twopaths$to))
  # correct ordering
  if(manynet::is_labelled(.data)) out <- out[match(manynet::node_names(.data), names(out))] else {
    temp <- rep(0, manynet::net_nodes(.data))
    temp[as.numeric(names(out))] <- out
    out <- temp
  }
  # missings should be none
  out[is.na(out)] <- 0
  make_node_measure(out, .data)
}

#' @rdname measure_brokerage 
#' @examples
#' node_by_brokering_exclusivity(ison_networkers, "Discipline")
#' @export
node_by_brokering_exclusivity <- function(.data, membership){
  .data <- manynet::expect_nodes(.data)
  twopaths <- .to_twopaths(.data)
  if(!missing(membership)){
    membership <- .resolve_membership(.data, membership)
    twopaths$from_memb <- membership[`if`(manynet::is_labelled(.data),
                                                                          match(twopaths$from, manynet::node_names(.data)),
                                                                          twopaths$from)]
    twopaths$to_memb <- membership[`if`(manynet::is_labelled(.data),
                                                                        match(twopaths$to.y, manynet::node_names(.data)),
                                                                        twopaths$to.y)]
    twopaths <- dplyr::filter(twopaths, from_memb != to_memb)
  }
  # get only exclusive paths
  out <- twopaths %>% dplyr::group_by(from, to.y) %>% dplyr::filter(dplyr::n()==1)
  # tabulate brokerage
  out <- c(table(out$to))
  # correct ordering for named data
  if(manynet::is_labelled(.data)) out <- out[match(manynet::node_names(.data), names(out))] else {
    temp <- rep(0, manynet::net_nodes(.data))
    temp[as.numeric(names(out))] <- out
    out <- temp
  }
  # missings should be none
  out[is.na(out)] <- 0
  make_node_measure(out, .data)
}

# Memberships ####

#' Memberships in brokerage positions
#' 
#' @description
#'   `node_in_brokerage()` returns nodes membership as a powerhouse,
#'   connector, linchpin, or sideliner according to Hamilton et al. (2020).
#'   
#' @name member_brokerage
#' @template param_data
#' @template param_memb
#' @family brokerage
#' @references
#' ## On brokerage activity and exclusivity
#'   Hamilton, Matthew, Jacob Hileman, and Orjan Bodin. 2020.
#'   "Evaluating heterogeneous brokerage: New conceptual and methodological approaches
#'   and their application to multi-level environmental governance networks"
#'   _Social Networks_ 61: 1-10.
#'   \doi{10.1016/j.socnet.2019.08.002}
#' @template node_member
NULL

#' @rdname member_brokerage 
#' @export
node_in_brokering <- function(.data, membership){
  .data <- manynet::expect_nodes(.data)
  activ <- node_by_brokering_activity(.data, membership)
  exclusiv <- node_by_brokering_exclusivity(.data, membership)
  activ <- activ - mean(activ)
  exclusiv <- exclusiv - mean(exclusiv)
  out <- dplyr::case_when(activ > 0 & exclusiv > 0 ~ "Powerhouse",
                          activ > 0 & exclusiv < 0 ~ "Connectors",
                          activ < 0 & exclusiv > 0 ~ "Linchpins",
                          activ < 0 & exclusiv < 0 ~ "Sideliners")
  make_node_member(out, .data)
}

.to_twopaths <- function(.data){
  to <- from <- to.y <- NULL
  if(!manynet::is_directed(.data)){
    el <- manynet::as_edgelist(manynet::to_reciprocated(.data)) 
  } else el <- manynet::as_edgelist(.data)
  twopaths <- dplyr::full_join(el, el, 
                               by = dplyr::join_by(to == from), 
                               relationship = "many-to-many")
  # remove non two-paths
  twopaths <- dplyr::filter(twopaths, !(is.na(from) | is.na(to.y)))
  # remove reciprocated paths
  twopaths <- dplyr::filter(twopaths, from != to.y)
  # remove triads
  twopaths <- dplyr::filter(twopaths, !paste(from, to.y) %in% paste(from, to))
  twopaths
}
