#' @param k Integer indicating the target number of communities to return.
#'   By default `NULL`, in which case the algorithm returns the number of
#'   communities that it finds itself.
#'   Alternatively, a character string naming a selection method:
#'   `"silhouette"` selects the number that maximises the mean silhouette
#'   width over geodesic distances, `"elbow"` selects the number at the
#'   elbow of the coverage curve, and `"strict"` returns the partition in
#'   which no tie crosses a group, i.e. the components.
#'   Prefer `"silhouette"`; the elbow method is unreliable where the
#'   coverage curve has no clear elbow.
#'   If the algorithm cannot return exactly the number of communities
#'   requested, a warning is given and the nearest number is returned.
#' @param Kmax Integer indicating the maximum number of communities to
#'   evaluate for `"silhouette"` and `"elbow"`. By default `8`.
#'   Otherwise ignored.
#'   Note that for `node_in_louvain()` and `node_in_leiden()` each candidate
#'   requires its own search over the resolution parameter,
#'   so a large `Kmax` is costly on large networks.
