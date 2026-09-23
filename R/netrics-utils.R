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

.strongPerturb <- function(soln, strength = 1){
  times <- ceiling(strength * length(soln)/max(soln))
  for (t in seq.int(times)){
    soln <- .weakPerturb(soln)
  }
  soln
}

# nocov end

# A 'stocnet' object holds a tie's sign as the sign of its weight, so a signed
# network reaches igraph carrying a `weight` attribute of -1 and 1. igraph's
# shortest path functions read any attribute of that name as a distance, and
# either abort on the negative values or report a negative cycle.
#
# Dropping the attribute would keep the negative ties as paths of length one,
# which is the wrong reading: a negative tie is hostility, not a channel along
# which cohesion travels. Path-based measures therefore run over the positive
# ties alone, as `node_x_clique()` does for the same reason.
.to_positive <- function(.data){
  if(manynet::is_signed(.data)){
    manynet::snet_info("Using only the positive ties,",
                       "since a negative tie does not carry cohesion.")
    manynet::to_unsigned(.data, keep = "positive")
  } else .data
}

# The other half of the signed treatment. Where a measure counts a tie however
# it is signed, as a census does, every non-zero entry is a tie and the sign
# carries nothing: `igraph::triad_census()` reads a signed network this way,
# and `.mixed_census()` makes the same reading explicit. This keeps every tie,
# so a tie-level result still holds one value per tie, which `.to_positive()`
# would not.
#
# TODO: `keep = "both"` arrived in manynet 2.3.2, but the DESCRIPTION floor is
# 2.3.1, which is what CRAN serves and what the CI checks run against. The
# fallback takes each weight's magnitude instead, which is the same operation.
# Remove the fallback and call `manynet::to_unsigned(keep = "both")` directly
# once the floor is raised past 2.3.2.
.to_unsigned <- function(.data){
  if(manynet::is_signed(.data)){
    manynet::snet_info("Reading each tie by its magnitude,",
                       "since a tie counts here however it is signed.")
    if("both" %in% eval(formals(manynet::to_unsigned)$keep))
      manynet::to_unsigned(.data, keep = "both")
    else {
      # The fallback has to return the class it was given, as
      # `manynet::to_unsigned()` does, since the measure that called this
      # passes the result on to its `make_*()` constructor. A sign is held
      # either in a `sign` attribute or as the sign of a weight, so both are
      # covered.
      out <- .data
      if("sign" %in% manynet::net_tie_attributes(out))
        out <- manynet::mutate_ties(out, sign = NULL)
      if("weight" %in% manynet::net_tie_attributes(out))
        out <- manynet::mutate_ties(out, weight = abs(weight))
      out
    }
  } else .data
}

# `manynet::is_multilevel()` is not exported by every 'manynet' version that
# this package supports, so the test is kept here. A multilevel network reports
# itself as two-mode, but interlocks its levels: it has ties both within and
# between the modes. A network whose ties all run between the modes, as
# `ison_southern_women`'s do, is a plain two-mode network. A network whose ties
# all fall within the modes is two networks and not two levels of one.
.is_multilevel <- function(.data){
  .data <- manynet::as_igraph(.data)
  # `to_multilevel()` records levels in a 'lvl' attribute and deletes 'type',
  # so a network that is already converted has to be recognised by its levels.
  if("lvl" %in% igraph::vertex_attr_names(.data))
    return(length(unique(igraph::vertex_attr(.data, "lvl"))) > 1)
  if(!manynet::is_twomode(.data)) return(FALSE)
  if(igraph::ecount(.data) == 0) return(FALSE)
  type <- igraph::vertex_attr(.data, "type")
  ends <- igraph::ends(.data, igraph::E(.data), names = FALSE)
  between <- type[ends[,1]] != type[ends[,2]]
  any(between) && any(!between)
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
