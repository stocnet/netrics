make_node_mark <- function(out, .data) {
  class(out) <- c("node_mark", class(out))
  if (is.null(names(out)) && manynet::is_labelled(.data))
    names(out) <- manynet::node_names(.data)
  attr(out, "mode") <- manynet::node_is_mode(.data)
  out
}

make_tie_mark <- function(out, .data) {
  class(out) <- c("tie_mark", class(out))
  if(manynet::is_labelled(.data)){
    tie_names <- attr(igraph::E(.data), "vnames")
    if(manynet::is_directed(.data)) 
      names(out) <- gsub("\\|", "->", tie_names) else 
        names(out) <- gsub("\\|", "-", tie_names)
  } else {
    ties <- manynet::as_edgelist(.data)[,1:2]
    if(manynet::is_directed(.data)) 
      names(out) <- paste0(ties$from, "->", ties$to) else 
        names(out) <- paste0(ties$from, "-", ties$to)
  }
  out
}

# Interpretive metadata ####

# The vocabulary for how a measure's values have (or have not) been rescaled.
# The distinction matters for interpretation:
#   "normalized"  divided by a theoretical maximum, so values are comparable
#                 across different networks
#   "scaled"      divided by the observed maximum, so the top node is always
#                 exactly 1 and values rank nodes within one network only
#   "proportion"  shares of a fixed total, summing to 1
#   "none"        raw values on the measure's own scale
NORMALIZATIONS <- c("normalized", "scaled", "proportion", "none")

# Attaches the interpretive metadata shared by all measure classes.
# Each argument is optional; absent metadata is simply not set, so measures
# that do not (yet) declare it behave exactly as they did before.
set_measure_attributes <- function(out, measure = NULL, range = NULL,
                                   normalization = NULL) {
  if(!is.null(measure)) attr(out, "measure") <- measure
  if(!is.null(range)) attr(out, "range") <- range
  if(!is.null(normalization)) {
    normalization <- match.arg(normalization, NORMALIZATIONS)
    attr(out, "normalization") <- normalization
  }
  out
}

# `scale` was the original (igraph-inherited) spelling of what is now `scaled`,
# named for symmetry with `normalized`. Accepts the old spelling and warns.
resolve_scaled <- function(scaled, scale = NULL) {
  if(!is.null(scale)) {
    # A real warning rather than `snet_warn()`, which is quiet by default:
    # a renamed argument is something the user needs to act on.
    warning("The `scale` argument has been renamed `scaled`, ",
            "for symmetry with `normalized`. Please use `scaled` instead.",
            call. = FALSE)
    scaled <- scale
  }
  scaled
}

make_node_measure <- function(out, .data, measure = NULL, range = NULL,
                              normalization = NULL) {
  if(manynet::is_labelled(.data)) names(out) <- manynet::node_names(.data)
  class(out) <- c("node_measure", class(out))
  attr(out, "mode") <- manynet::node_is_mode(.data)
  set_measure_attributes(out, measure, range, normalization)
}

make_tie_measure <- function(out, .data, measure = NULL, range = NULL,
                             normalization = NULL) {
  class(out) <- c("tie_measure", class(out))
  if(manynet::is_labelled(.data)){
    tie_names <- attr(igraph::E(.data), "vnames")
    if(manynet::is_directed(.data)) 
      names(out) <- gsub("\\|", "->", tie_names) else 
        names(out) <- gsub("\\|", "-", tie_names)
  } else {
    ties <- manynet::as_edgelist(.data)[,1:2]
    if(manynet::is_directed(.data)) 
      names(out) <- paste0(ties$from, "->", ties$to) else
        names(out) <- paste0(ties$from, "-", ties$to)
  }
  set_measure_attributes(out, measure, range, normalization)
}

make_network_measure <- function(out, .data, call, measure = NULL,
                                 range = NULL, normalization = NULL) {
  class(out) <- c("network_measure", class(out))
  attr(out, "mode") <- manynet::net_dims(.data)
  attr(out, "call") <- call
  set_measure_attributes(out, measure, range, normalization)
}

make_mode_measure <- function(out, .data, call, measure = NULL,
                              range = NULL, normalization = NULL) {
  class(out) <- c("mode_measure", "network_measure", class(out))
  attr(out, "mode") <- manynet::net_dims(.data)
  attr(out, "call") <- call
  set_measure_attributes(out, measure, range, normalization)
}

make_node_member <- function(out, .data) {
  if(is.numeric(out))
    out <- MORELETTERS[out]
  if (manynet::is_labelled(.data)) names(out) <- manynet::node_names(.data)
  class(out) <- c("node_member", class(out))
  attr(out, "mode") <- manynet::node_is_mode(.data)
  out
}

MORELETTERS <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))

make_node_motif <- function(out, .data) {
  class(out) <- c("node_motif", class(out))
  if(manynet::is_twomode(.data)) attr(out, "mode") <- manynet::node_is_mode(.data)
  if(manynet::is_labelled(.data)) attr(out, "dimnames")[[1]] <- manynet::node_names(.data)
  out
}

make_network_motif <- function(out, .data) {
  class(out) <- c("network_motif", class(out))
  attr(out, "mode") <- manynet::net_dims(.data)
  attr(out, "call") <- deparse(sys.calls())
  out
}
