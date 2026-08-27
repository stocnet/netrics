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
#   "proportional" shares of a fixed total, summing to 1
#   "none"        raw values on the measure's own scale
NORMALIZATIONS <- c("normalized", "scaled", "proportional", "none")

# Where a measure offers a choice between several ways of counting the same
# thing, `variant` records which one ran. It is orthogonal to `normalization`:
# the first says *which* quantity was computed, the second *how* its values
# were rescaled, and a measure may meaningfully declare both, as
# `net_by_smallworld()` does in reporting the "SWI" variant as normalised.
# Unlike `NORMALIZATIONS` there is no fixed vocabulary to match against, since
# each family names its own variants.

# Attaches the interpretive metadata shared by all measure classes.
# Each argument is optional; absent metadata is simply not set, so measures
# that do not (yet) declare it behave exactly as they did before.
set_measure_attributes <- function(out, measure = NULL, range = NULL,
                                   normalization = NULL, variant = NULL) {
  if(!is.null(measure)) attr(out, "measure") <- measure
  if(!is.null(range)) attr(out, "range") <- range
  if(!is.null(normalization)) {
    normalization <- match.arg(normalization, NORMALIZATIONS)
    attr(out, "normalization") <- normalization
  }
  if(!is.null(variant)) attr(out, "variant") <- as.character(variant)[1]
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

# Several measures discount a contribution once per step of distance or walk
# length. The literature names that discount differently in each case —
# Bonacich and Lloyd's alpha, RoleSim's beta, PageRank's damping factor,
# the t of subgraph centrality — but it is one parameter, so netrics calls it
# `decay` everywhere: higher values discount less, so longer walks count for
# more. These two helpers keep that vocabulary in step.

# Accepts a superseded spelling and warns, as `resolve_scaled()` does.
resolve_decay <- function(decay, old = NULL, old_name) {
  if(!is.null(old)) {
    warning("The `", old_name, "` argument has been renamed `decay`, ",
            "the name this package uses for a per-step discount. ",
            "Please use `decay` instead.", call. = FALSE)
    decay <- old
  }
  decay
}

# The single bound for every such discount, so that the message and the
# accepted range cannot drift apart between measures.
check_decay <- function(decay, arg = "decay") {
  if(!is.numeric(decay) || length(decay) != 1L || !is.finite(decay) ||
     decay < 0 || decay > 1)
    # `arg` is interpolated by `snet_abort()`, so it is passed as a value
    # rather than pasted into the string.
    manynet::snet_abort("`{arg}` must be a proportion between 0 and 1.")
  decay
}

make_node_measure <- function(out, .data, measure = NULL, range = NULL,
                              normalization = NULL, variant = NULL) {
  if(manynet::is_labelled(.data)) names(out) <- manynet::node_names(.data)
  class(out) <- c("node_measure", class(out))
  attr(out, "mode") <- manynet::node_is_mode(.data)
  set_measure_attributes(out, measure, range, normalization, variant)
}

make_tie_measure <- function(out, .data, measure = NULL, range = NULL,
                             normalization = NULL, variant = NULL) {
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
  set_measure_attributes(out, measure, range, normalization, variant)
}

make_network_measure <- function(out, .data, call, measure = NULL,
                                 range = NULL, normalization = NULL,
                                 variant = NULL) {
  class(out) <- c("network_measure", class(out))
  attr(out, "mode") <- manynet::net_dims(.data)
  attr(out, "call") <- call
  set_measure_attributes(out, measure, range, normalization, variant)
}

make_mode_measure <- function(out, .data, call, measure = NULL,
                              range = NULL, normalization = NULL,
                              variant = NULL) {
  class(out) <- c("mode_measure", "network_measure", class(out))
  attr(out, "mode") <- manynet::net_dims(.data)
  attr(out, "call") <- call
  set_measure_attributes(out, measure, range, normalization, variant)
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
