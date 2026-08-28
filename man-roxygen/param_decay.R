#' @param decay A proportion between 0 and 1 giving how much of a contribution
#'   survives each additional step of distance or walk length.
#'   Lower values discount more steeply, so that only nearby others count;
#'   higher values discount less, so that longer walks continue to contribute.
#'   The measures that take a `decay` differ in what they discount and in what
#'   value leaves the measure in its most familiar form,
#'   so each documents its own default.
