# nocov start
#' Functions that have been renamed, superseded, or are no longer working
#' 
#' `r lifecycle::badge("deprecated")`
#' Generally these functions have been superseded or renamed.
#' Upon using them, a message is provided directing the user to the new function.
#' However, at this stage of package development,
#' we generally clear older defunct functions at each minor release,
#' and so you are strongly encouraged to use the new functions/names/syntax
#' wherever possible and update your scripts accordingly.
#' @name defunct
#' @keywords internal
#' @returns Results as expected 
#'   along with a warning to use new function naming in the future.
NULL

#' @describeIn defunct Deprecated on 2026-08-27.
#'   Renamed `node_by_core()`, for symmetry with `node_is_core()` and
#'   `node_in_core()`, and so that "coreness" names only the peeling depth
#'   that `node_by_kcoreness()` returns.
#' @template param_data
#' @template param_coreness
#' @export
node_by_coreness <- function(.data, coreness = NULL,
                             direction = c("all", "out", "in")) {
  .Deprecated("node_by_core", package = "netrics",
              old = "node_by_coreness")
  node_by_core(.data, coreness = coreness, direction = direction)
}

#' @describeIn defunct Deprecated on 2026-08-28.
#'   Folded into `net_x_triad()`, which now takes the multilevel census
#'   whenever it is given both a one-mode and a two-mode network, rather than
#'   reporting that no such option exists.
#' @template param_data
#' @param object2 A second, two-mode network object.
#' @export
net_x_mixed <- function(.data, object2) {
  .Deprecated("net_x_triad", package = "netrics", old = "net_x_mixed")
  if(missing(object2)) net_x_triad(.data) else net_x_triad(.data, object2)
}

# nocov end