#' @param standardized Logical scalar. Where `TRUE`, the counts are returned
#'   as z-scores against a null model rather than as raw counts.
#'   This is a different quantity from `normalized`, which divides by a
#'   theoretical maximum, and from `scaled`, which divides by the observed
#'   maximum: a z-score says how far the count departs from what the null
#'   model expects, so it can be negative and has no fixed range.
#'   By default `FALSE`.
