#' @family measures
#' @returns
#'   A `network_measure` numeric score.
#'
#'   The object also carries the `measure` it computed, the `range` its values
#'   can fall within, and whether and how those values were `normalized`.
#'   These are shown as a one-line header when the object is printed.
#'   Where a measure offers a choice between several ways of counting the
#'   same thing, it also carries the `variant` it used.
#'   All can be retrieved with `attr()`.
