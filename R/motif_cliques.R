# Clique participation ####

#' Motifs of clique participation
#' @name motif_clique
#' @template section_cognitive
#' @description
#'   `node_x_clique()` returns which maximal cliques each node belongs to.
#'
#'   A clique is a set of nodes every one of which is tied to every other,
#'   and it is maximal if no further node can be added without breaking that.
#'   Cliques are the strictest notion of a cohesive subgroup,
#'   and unlike the communities returned by `node_in_*()` functions they
#'   _overlap_: a node may belong to many cliques at once, or to none.
#'   That is why this returns an incidence table rather than a membership
#'   vector.
#'
#'   `node_x_percolation()` returns which communities of adjacent cliques
#'   each node belongs to, by clique percolation.
#'   These communities also overlap, but there are fewer of them than there
#'   are cliques, since cliques that share most of their nodes are joined.
#' @template param_data
#' @param min_clique_size Integer, the minimum size of clique to return.
#'   By default 3, since dyads and isolates are trivially cliques.
#'   For a two-mode network, a vector of two values giving the minimum number
#'   of nodes from each mode, by default `c(3, 3)`.
#' @family motifs
#' @template node_motif
#' @section Bicliques:
#'   In a two-mode network no two nodes of the same mode are ever tied
#'   directly, so no set of them is a clique in the ordinary sense.
#'   The two-mode analogue is a _biclique_: a set of nodes from each mode such
#'   that every node of the one is tied to every node of the other.
#'   `node_x_clique()` detects these by connecting nodes that share a partner
#'   before searching, so that a biclique becomes an ordinary clique,
#'   and then keeping only those cliques with at least `min_clique_size` nodes 
#'   from each mode.
#' @section Clique percolation:
#'   Clique percolation (Palla et al. 2005) treats two cliques as adjacent
#'   where they share all but one of `min_clique_size` nodes,
#'   so with the default of 3 where they share a tie.
#'   A community is then a set of cliques that can be reached from each other
#'   through such adjacent cliques,
#'   and a node belongs to every community that holds a clique it is in.
#'   A node in no clique of at least `min_clique_size` belongs to none.
#' @section Signed networks:
#'   Since a clique is a maximally cohesive subgroup, negative ties cannot
#'   contribute to one. Where the network is signed, only its positive ties are
#'   considered. Use [manynet::to_unsigned()] first to control this yourself.
#' @references
#' ## On cliques
#' Luce, R. Duncan, and Albert D. Perry. 1949.
#' "A method of matrix analysis of group structure".
#' _Psychometrika_ 14(2): 95-116.
#' \doi{10.1007/BF02289146}
#' 
#' ## On clique percolation
#' Palla, Gergely, Imre Derényi, Illés Farkas, and Tamás Vicsek. 2005.
#' "Uncovering the overlapping community structure of complex networks in
#' nature and society".
#' _Nature_ 435(7043): 814-818.
#' \doi{10.1038/nature03607}
#' @examples
#' node_x_clique(ison_adolescents)
#' node_x_clique(ison_southern_women, min_clique_size = c(3, 3))
#' @export
node_x_clique <- function(.data, min_clique_size = 3){
  found <- .find_cliques(.data, min_clique_size)
  out <- .clique_incidence(found)
  if(ncol(out) == 0)
    manynet::snet_info("No cliques of at least this size were found.")
  make_node_motif(out, found$.data)
}

#' @rdname motif_clique
#' @examples
#' node_x_percolation(ison_adolescents)
#' @export
node_x_percolation <- function(.data, min_clique_size = 3){
  found <- .find_cliques(.data, min_clique_size)
  inc <- .clique_incidence(found)
  if(ncol(inc) == 0){
    manynet::snet_info("No cliques of at least this size were found.")
    return(make_node_motif(inc, found$.data))
  }
  # two cliques are adjacent where they share all but one of the nodes of the
  # smallest clique admitted, and each component of adjacent cliques is a
  # community
  overlap <- crossprod(inc) >= found$smallest - 1
  diag(overlap) <- FALSE
  comms <- igraph::components(igraph::graph_from_adjacency_matrix(
    overlap * 1, mode = "undirected", diag = FALSE))$membership
  out <- vapply(seq_len(max(comms)), function(k)
    as.integer(rowSums(inc[, comms == k, drop = FALSE]) > 0),
    FUN.VALUE = integer(nrow(inc)))
  out <- matrix(out, nrow = nrow(inc))
  colnames(out) <- paste0("C", seq_len(ncol(out)))
  make_node_motif(out, found$.data)
}

# The maximal cliques of a network, read as `node_x_clique()` documents:
# positive ties only where signed, and bicliques where two-mode.
.find_cliques <- function(.data, min_clique_size){
  .data <- manynet::expect_nodes(.data)
  .data <- .to_aggregated_css(.data)
  twomode <- manynet::is_twomode(.data)
  if(twomode && length(min_clique_size) == 1)
    min_clique_size <- c(min_clique_size, min_clique_size)
  # a clique is a cohesive subgroup, so where ties are signed only the
  # positive ones can contribute to one
  if(manynet::is_signed(.data))
    .data <- manynet::to_unsigned(.data, keep = "positive")
  mat <- manynet::as_matrix(manynet::to_undirected(
    manynet::to_unweighted(manynet::to_onemode(.data))))
  if(twomode){
    # two nodes of a mode that share a partner are made adjacent, so that a
    # biclique becomes an ordinary clique of the combined node set
    mat <- ((mat %*% mat) + mat) > 0
    diag(mat) <- 0
    smallest <- sum(min_clique_size)
  } else smallest <- min_clique_size
  graph <- igraph::graph_from_adjacency_matrix(mat*1, mode = "undirected",
                                               diag = FALSE)
  cliques <- igraph::max_cliques(graph, min = smallest)
  if(twomode){
    modes <- manynet::node_is_mode(.data)
    keep <- vapply(cliques, function(cl)
      sum(!modes[cl]) >= min_clique_size[1] &&
        sum(modes[cl]) >= min_clique_size[2],
      FUN.VALUE = logical(1))
    cliques <- cliques[keep]
  }
  list(.data = .data, cliques = cliques, smallest = smallest)
}

.clique_incidence <- function(found){
  cliques <- found$cliques
  out <- matrix(0L, nrow = manynet::net_nodes(found$.data),
                ncol = length(cliques))
  for(j in seq_along(cliques)) out[as.integer(cliques[[j]]), j] <- 1L
  colnames(out) <- if(length(cliques) > 0)
    paste0("C", seq_along(cliques)) else character(0)
  out
}
