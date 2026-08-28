#' Memberships in components
#' @description
#'   These functions create a vector of nodes' memberships in components:
#'
#'   - `node_in_component()` assigns nodes' component membership,
#'   in either the strongly or the weakly connected components.
#'
#'   In graph theory, components, sometimes called connected components,
#'   are induced subgraphs from partitioning the nodes into disjoint sets.
#'   All nodes that are members of the same partition as _i_ are reachable
#'   from _i_.
#'
#'   For directed networks,
#'   strongly connected components consist of subgraphs where there are paths
#'   in each direction between member nodes.
#'   Weakly connected components consist of subgraphs where there is a path
#'   in either direction between member nodes.
#'
#' @template param_data
#' @template param_connectivity
#' @template node_member
#' @name member_components
NULL

#' @rdname member_components
#' @importFrom igraph components
#' @examples
#' ison_monks |> to_uniplex("esteem") |>
#'   mutate_nodes(comp = node_in_component())
#' ison_monks |> to_uniplex("esteem") |>
#'   mutate_nodes(comp = node_in_component(connectivity = "weak"))
#' @export
node_in_component <- function(.data, connectivity = c("strong", "weak")){
  connectivity <- match.arg(connectivity)
  .data <- manynet::expect_nodes(.data)
  if(!manynet::is_graph(.data)) .data <- manynet::as_igraph(.data) # nocov
  make_node_member(igraph::components(.data, mode = connectivity)$membership,
              .data)
}
