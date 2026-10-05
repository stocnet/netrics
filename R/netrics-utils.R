# nocov start

# defining global variables more centrally
utils::globalVariables(c(".data", "obs",
                         "from", "to", "name", "weight","sign","wave",
                         "from_memb","to_memb","to.y",
                         "nodes","event","exposure",
                         "student","students","colleges",
                         "node","value","var","active","time",
                         "A","B","C","D",
                         "type",
                         "n"))

# Helper function for declaring available methods
available_methods <- function(fun_vctr) {
  out <- lapply(fun_vctr, function(f) regmatches(utils::.S3methods(f),
                                                 regexpr("\\.", utils::.S3methods(f)),
                                                 invert = TRUE))
  out <- out[lapply(out,length)>0]
  out <- t(as.data.frame(out))
  colnames(out) <- c("from","to")
  rownames(out) <- NULL
  out <- as.data.frame(out)
  manynet::as_matrix(out)
}

# Helper function for checking and downloading packages
thisRequires <- function(pkgname){
  if (!requireNamespace(pkgname, quietly = TRUE) & interactive()) {
    if(utils::askYesNo(msg = paste("The", pkgname, 
                                   "package is required to run this function. Would you like to install", pkgname, "from CRAN?"))) {
      utils::install.packages(pkgname)
    } else {
      manynet::snet_abort(paste("Please install", pkgname, "from CRAN to run this function."))
    }
  }
}

seq_nodes <- function(.data){
  seq.int(manynet::net_nodes(.data))
}

# Resolve membership to a vector:
# if a single character string naming a network attribute is provided,
# retrieve that attribute as a vector; otherwise return the value as-is.
.resolve_membership <- function(.data, membership) {
  if (is.character(membership) && length(membership) == 1 &&
      membership %in% manynet::net_node_attributes(.data)) {
    manynet::node_attribute(.data, membership)
  } else {
    membership
  }
}


# Local-search perturbations over a membership vector, shared by the
# random-restart searches in `node_in_roulette()` and `node_in_block()`.
# A weak perturbation makes one small move; a strong one makes enough moves
# to escape a local optimum.
.weakPerturb <- function(soln){
  gsizes <- table(soln)
  evens <- all(gsizes == max(gsizes))
  if(evens){
    soln <- .swapMove(soln)
  } else {
    if(stats::runif(1)<0.5) soln <- .swapMove(soln) else 
      soln <- .oneMove(soln)
  }
  soln
}

# `sample(x, 1)` draws from `1:x` where `x` is a single number, so a single
# candidate node or group would be replaced by any smaller one.
# This draws from the candidates themselves, however many there are.
.sampleOne <- function(x) x[sample.int(length(x), 1)]

.swapMove <- function(soln){
  from <- sample.int(length(soln), 1)
  others <- which(soln != soln[from])
  if(!length(others)) return(soln)
  to <- .sampleOne(others)
  soln[c(to,from)] <- soln[c(from,to)]
  soln
}

# Moves a node from a largest group to a smaller one. The groups are found by
# their labels rather than by their positions in `table()`, so that the labels
# need not be `1:k`, and a move cannot make a group larger than the largest.
.oneMove <- function(soln){
  groups <- sort(unique(soln))
  gsizes <- tabulate(match(soln, groups), length(groups))
  smaller <- groups[gsizes < max(gsizes)]
  if(!length(smaller)) return(.swapMove(soln))
  from <- .sampleOne(which(soln %in% groups[gsizes == max(gsizes)]))
  soln[from] <- .sampleOne(smaller)
  soln
}

.strongPerturb <- function(soln, strength = 1, weak = .weakPerturb){
  times <- ceiling(strength * length(soln)/max(soln))
  for (t in seq.int(times)){
    soln <- weak(soln)
  }
  soln
}

# nocov end

# A cognitive social structure (CSS) asks every node to report on the ties of
# the whole network, and records who reported each tie in a 'by' column. Since
# 'manynet' 2.4.0, `manynet::as_matrix()` returns such a network as a
# from-to-perceiver array, one for each layer, and not as a square
# matrix. A measure that expects a square matrix then fails, or returns a
# number that means nothing.
#
# This combines the reports into Krackhardt's (1987) locally aggregated
# structure, taking the intersection rule: a tie exists where both of its ends
# report it. The two ends are the nodes that know most about a tie. A tie
# that names no reporter, such as a formal reporting line, is kept as it is.
#
# TODO: `manynet::to_aggregated()` arrived in manynet 2.4.0, but the
# DESCRIPTION floor is 2.3.5, whose `manynet::as_matrix()` already returns the
# perceivers' array. The fallback keeps the report of each tie by its sender,
# where its receiver reports it too, which is the same structure. Remove the
# fallback and call `manynet::to_aggregated()` directly once the floor is
# raised to 2.4.0.
.to_aggregated_css <- function(.data){
  if(!manynet::is_cognitive(.data)) return(.data)
  manynet::snet_info("Combining the perceivers' reports of this cognitive",
                     "social structure into its locally aggregated structure,",
                     "where a tie exists if both of its ends report it.")
  if("to_aggregated" %in% getNamespaceExports("manynet"))
    return(getExportedValue("manynet", "to_aggregated")(.data, over = "by",
                                                        rule = "min",
                                                        reporters = "both"))
  .las_intersection(.data)
}

# The fallback has to return the class it was given, as
# `manynet::to_aggregated()` does, so it filters the ties in place.
.las_intersection <- function(.data){
  g <- manynet::as_igraph(.data)
  by <- igraph::edge_attr(g, "by")
  # A network whose ties name no reporter is already its aggregated structure.
  # Before 'manynet' 2.4.0, coercion could also drop a 'by' column, which would
  # otherwise leave nothing to filter the ties by.
  if(is.null(by)) return(.data)
  ends <- .tie_ends(g)
  tie <- .tie_keys(g)
  reported <- paste(tie, by)
  both <- paste(tie, ends[,1]) %in% reported & paste(tie, ends[,2]) %in% reported
  keep <- is.na(by) | (by == ends[,1] & both)
  out <- manynet::filter_ties(.data, keep)
  manynet::mutate_ties(out, by = NULL)
}

# The two ends of each tie, in the order of `igraph::E()`.
# An undirected tie is the same tie whichever end is listed first.
.tie_ends <- function(g){
  ends <- igraph::ends(g, igraph::E(g), names = FALSE)
  if(!manynet::is_directed(g) && nrow(ends))
    ends <- matrix(c(pmin(ends[,1], ends[,2]), pmax(ends[,1], ends[,2])),
                   ncol = 2)
  ends
}

# One key for each tie, which names its ends and its layer but not who
# reported it, so that the reports of one tie share a key.
.tie_keys <- function(g){
  g <- manynet::as_igraph(g)
  ends <- .tie_ends(g)
  layer <- if("layer" %in% igraph::edge_attr_names(g))
    igraph::edge_attr(g, "layer") else rep("", nrow(ends))
  paste(ends[,1], ends[,2], layer)
}

# A tie-level result has to hold one value for each tie of the network it was
# given, so that it can be added back to that network. In a cognitive social
# structure each report is a tie, and the reports of one tie are parallel ties,
# which a tie measure would otherwise read as a multigraph. This calculates
# the result on the locally aggregated structure instead, and then gives each
# report the value of the tie it reports. A report of a tie that is not in
# that structure gets `NA`, or `FALSE` for a mark.
.map_css_ties <- function(.data, fun, ...){
  agg <- .to_aggregated_css(.data)
  res <- fun(agg, ...)
  idx <- match(.tie_keys(.data), .tie_keys(agg))
  out <- unname(unclass(res))[idx]
  if(is.logical(out)) out[is.na(idx)] <- FALSE
  attrs <- attributes(res)
  attrs$names <- NULL
  attributes(out) <- attrs
  # The constructor names each tie of the network it is given.
  names(out) <- names(make_tie_mark(logical(length(out)), .data))
  out
}

# The cells of an adjacency matrix that are possible ties: a node cannot be tied
# to itself unless the network is complex, and an undirected tie appears twice.
.valid_cells <- function(.data, mat){
  keep <- matrix(TRUE, nrow(mat), ncol(mat))
  if(!manynet::is_twomode(.data)){
    if(!manynet::is_complex(.data)) diag(keep) <- FALSE
    if(!manynet::is_directed(.data)) keep[upper.tri(keep)] <- FALSE
  }
  keep
}
