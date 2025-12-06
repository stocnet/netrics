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

make_node_measure <- function(out, .data) {
  if(manynet::is_labelled(.data)) names(out) <- manynet::node_names(.data)
  class(out) <- c("node_measure", class(out))
  attr(out, "mode") <- manynet::node_is_mode(.data)
  out
}

make_tie_measure <- function(out, .data) {
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
  out
}

make_network_measure <- function(out, .data, call) {
  class(out) <- c("network_measure", class(out))
  attr(out, "mode") <- manynet::net_dims(.data)
  attr(out, "call") <- call
  out
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

#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`
