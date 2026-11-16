# nocov start
#' Functions that have been renamed, superseded, or are no longer working
#' 
#' Generally these functions have been superseded or renamed.
#' Upon using them, a warning directs the user to the new function.
#' A function that is only superseded still works without a warning
#' for one minor release.
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
  manynet::snet_deprecated("node_by_coreness", "node_by_core")
  node_by_core(.data, coreness = coreness, direction = direction)
}

#' @describeIn defunct Deprecated on 2026-08-28, defunct on 2026-10-15.
#'   Folded into `net_x_triad()`, which takes the multilevel census
#'   whenever it is given a network holding a one-mode and a two-mode layer.
#' @template param_data
#' @param object2 A second, two-mode network object.
#' @export
net_x_mixed <- function(.data, object2) {
  manynet::snet_deprecated("net_x_mixed", "net_x_triad", "defunct",
                           why = "Give {.fn net_x_triad} one multilevel network, such as {.code fict_marvel}.")
}

#' @describeIn defunct Deprecated on 2026-08-28.
#'   Folded into `node_in_component(connectivity = "weak")`, which now takes
#'   the connectivity wanted as an argument rather than splitting the same
#'   calculation across three function names.
#' @template param_data
#' @export
node_in_weak <- function(.data) {
  manynet::snet_deprecated("node_in_weak", "node_in_component",
                           why = "Give it {.code connectivity = \"weak\"}.")
  node_in_component(.data, connectivity = "weak")
}

#' @describeIn defunct Deprecated on 2026-08-28.
#'   Folded into `node_in_component(connectivity = "strong")`, which is also
#'   what `node_in_component()` does by default.
#' @template param_data
#' @export
node_in_strong <- function(.data) {
  manynet::snet_deprecated("node_in_strong", "node_in_component",
                           why = "Give it {.code connectivity = \"strong\"}.")
  node_in_component(.data, connectivity = "strong")
}

#' @describeIn defunct Superseded by `tie_is_repeated()` on 2026-11-12.
#'   "Multiple" did not say whether a tie repeats an earlier one or coexists
#'   with it, which `manynet::tie_is_parallel()` marks (#38).
#'   This name gives a warning from the next minor release.
#' @template param_data
#' @export
tie_is_multiple <- function(.data) {
  manynet::snet_deprecated("tie_is_multiple", "tie_is_repeated", "superseded")
  tie_is_repeated(.data)
}

# nocov end