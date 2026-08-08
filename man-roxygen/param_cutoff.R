#' @param cutoff Integer scalar, the maximum path length considered.
#'   Paths longer than this are ignored, which restricts the measure to a
#'   node's local neighbourhood.
#'   Where a measure is defined over all paths by default,
#'   a negative value or `NULL` imposes no limit.
