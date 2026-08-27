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

# Compatibility shim: manynet renamed `to_ties()` to `to_linegraph()` in 2.3.0.
# The name is resolved at call time, so this uses `to_linegraph()` where it is
# available and never raises the deprecation warning that `to_ties()` gives
# there. Remove this and call `manynet::to_linegraph()` directly once manynet
# 2.3.x is on CRAN and the DESCRIPTION floor is raised again.
.to_linegraph <- function(.data) {
  ns <- asNamespace("manynet")
  fn <- if (is.null(ns$to_linegraph)) ns$to_ties else ns$to_linegraph
  fn(.data)
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

.swapMove <- function(soln){
  from <- sample(seq.int(length(soln)), 1)
  to <- sample(which(soln != soln[from]), 1)
  soln[c(to,from)] <- soln[c(from,to)]
  soln
}

.oneMove <- function(soln){
  gsizes <- table(soln)
  maxg <- which(gsizes == max(gsizes))
  from <- sample(which(soln %in% maxg), 1)
  soln[from] <- sample(which(gsizes != max(gsizes)), 1)
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
