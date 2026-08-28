#' @param connectivity Character string, "weak" treats a directed network's
#'   components as if the network were undirected, and "strong" requires ties
#'   in both directions between members.
#'   This is ignored for undirected networks, where the two notions coincide.
#'   Note that the default differs by function: functions that assert or count
#'   connectedness default to "strong", while functions that scope or split a
#'   network into components default to "weak".
