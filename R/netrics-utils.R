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

# nocov end