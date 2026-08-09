# Clique participation ####

#' Motifs of clique participation
#' @name motif_clique
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
#' @examples
#' node_x_clique(ison_adolescents)
#' node_x_clique(ison_southern_women, min = c(3, 3))
#' @export
node_x_clique <- function(.data, min = 3){
  .data <- manynet::expect_nodes(.data)
  twomode <- manynet::is_twomode(.data)
  if(twomode && length(min) == 1) min <- c(min, min)
  # a clique is a cohesive subgroup, so where ties are signed only the
  # positive ones can contribute to one
  if(manynet::is_signed(.data))
    .data <- manynet::to_unsigned(.data, keep = "positive")
  mat <- manynet::as_matrix(manynet::to_undirected(
    manynet::to_unweighted(manynet::to_multilevel(.data))))
  if(twomode){
    # two nodes of a mode that share a partner are made adjacent, so that a
    # biclique becomes an ordinary clique of the combined node set
    mat <- ((mat %*% mat) + mat) > 0
    diag(mat) <- 0
    smallest <- sum(min)
  } else smallest <- min
  graph <- igraph::graph_from_adjacency_matrix(mat*1, mode = "undirected",
                                               diag = FALSE)
  cliques <- igraph::max_cliques(graph, min = smallest)
  if(twomode){
    modes <- manynet::node_is_mode(.data)
    keep <- vapply(cliques, function(cl)
      sum(!modes[cl]) >= min[1] && sum(modes[cl]) >= min[2],
      FUN.VALUE = logical(1))
    cliques <- cliques[keep]
  }
  out <- matrix(0L, nrow = manynet::net_nodes(.data),
                ncol = length(cliques))
  for(j in seq_along(cliques)) out[as.integer(cliques[[j]]), j] <- 1L
  colnames(out) <- if(length(cliques) > 0)
    paste0("C", seq_along(cliques)) else character(0)
  if(length(cliques) == 0)
    manynet::snet_info("No cliques of at least this size were found.")
  make_node_motif(out, .data)
}
