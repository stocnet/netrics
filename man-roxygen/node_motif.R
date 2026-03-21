#' @family motifs
#' @family nodal
#' @returns
#'   A `node_motif` matrix with one row for each node in the network and 
#'   a column for each motif type, 
#'   giving the count of each motif in which each node participates.
#'   It is printed as a tibble, however, to avoid greedy printing.
#'   If the network is labelled, 
#'   then the node names will be in a column named `names`.
