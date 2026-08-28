#' Methods for splitting a continuous score into ordered groups
#' 
#' @description
#'   These functions split a continuous score, such as a coreness score,
#'   into an ordered set of groups:
#'   
#'   - `split_bins()` cuts the range into equal-width bins.
#'   - `split_quantiles()` cuts at the quantiles, so each group holds a
#'   similar number of nodes.
#'   - `split_kmeans()` clusters the scores by k-means, so the cuts fall
#'   where the scores themselves are furthest apart.
#' 
#'   These functions are not intended to be called directly,
#'   but are called within `node_in_core()` and related functions.
#'   They are exported and listed here to provide more detailed documentation.
#' @name method_split
#' @param scores A numeric vector of scores to split.
#' @param groups An integer indicating the number of groups to split into.
#' @returns 
#'   An integer vector the length of `scores`,
#'   giving each score's group index, numbered from the lowest score upwards.
NULL

#' @rdname method_split
#' @section Bins:
#'   Cuts the observed range into `groups` intervals of equal width.
#'   Where the scores are unevenly spread, a bin can end up empty,
#'   so this returns the coarsest picture of the three.
#' @examples
#' split_bins(c(0, 0.1, 0.4, 0.9, 1), 3)
#' @export
split_bins <- function(scores, groups){
  cut(scores, breaks = groups, labels = FALSE)
}

#' @rdname method_split
#' @section Quantiles:
#'   Cuts at the quantiles of the scores, so each group holds a similar
#'   number of nodes whatever the shape of the distribution.
#' @examples
#' split_quantiles(c(0, 0.1, 0.4, 0.9, 1), 3)
#' @export
split_quantiles <- function(scores, groups){
  as.numeric(cut(scores,
                 breaks = stats::quantile(scores,
                                          probs = seq(0, 1,
                                                      length.out = groups + 1)),
                 include.lowest = TRUE, labels = FALSE))
}

#' @rdname method_split
#' @section K-means:
#'   Clusters the scores by k-means, so the cuts fall where the scores are
#'   furthest apart rather than at fixed widths or counts.
#' @examples
#' split_kmeans(c(0, 0.1, 0.4, 0.9, 1), 3)
#' @export
split_kmeans <- function(scores, groups){
  km <- stats::kmeans(scores, centers = groups)
  # k-means numbers its clusters in whatever order it finds them, so the
  # numbers must be put back in score order before they can index the labels.
  order(order(km$centers))[km$cluster]
}
