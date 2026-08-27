#' @param coreness Which method to use to calculate nodes' coreness.
#'   One of "correlation", "rich", "transition", or "hub";
#'   see [method_coreness] for what each does.
#'   By default NULL, which uses "rich" for a weighted, directed, or
#'   two-mode network, since it is the only method that reads those properties
#'   directly, and "correlation" otherwise.
#' @param direction One of "all" (the default), "out", or "in".
#'   For a directed network, "out" scores nodes on the ties they send and
#'   "in" on the ties they receive.
#'   Ignored for undirected and two-mode networks.
