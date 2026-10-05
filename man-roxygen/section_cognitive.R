#' @section Cognitive social structures:
#'   A cognitive social structure records each node's report of the ties in
#'   the whole network, in a `by` column that names who reported each tie.
#'   Counting every report as a tie of its own would count each tie once for
#'   every perceiver who reports it.
#'   So the functions here first combine the reports into the locally
#'   aggregated structure of Krackhardt (1987), with the intersection rule:
#'   a tie exists if both of its ends report it, and a message says so.
#'   A tie that names no reporter is kept as it is.
#'
#'   A tie-level function still returns one value for each report,
#'   so that the result can be added back to the network it was given.
#'   Each report takes the value of the tie that it reports.
#'   A report of a tie that is not in the aggregated structure takes `NA`,
#'   or `FALSE` for a mark.
#'   `tie_is_random()` is the exception, and draws among the reports.
#'
#'   To combine the reports in a different way, do this before the function,
#'   e.g. with `manynet::to_aggregated(over = "by")`.
#'
#'   Krackhardt, David. 1987.
#'   "Cognitive social structures".
#'   _Social Networks_ 9(2): 109-134.
#'   \doi{10.1016/0378-8733(87)90009-8}
