# Link communities ####

#' Memberships of ties in link communities
#' @name member_community_link
#' @template section_cognitive
#' @description
#'   `tie_in_community()` assigns each tie to a link community.
#'
#'   Most community detection algorithms partition the nodes of a network,
#'   so that each node belongs to one community only.
#'   Link communities (Ahn et al. 2010) partition the ties instead.
#'   Each tie belongs to one community,
#'   but a node belongs to every community that one of its ties does,
#'   so the communities of nodes can _overlap_ where the communities of ties
#'   do not.
#' @template param_data
#' @family memberships
#' @family tie
#' @family community
#' @returns
#'   A `tie_member` character vector the length of the ties in the network,
#'   of group memberships "A", "B", etc for each tie,
#'   named by the pair of nodes each tie joins.
#' @section Link communities:
#'   Two ties that share a node are similar to the extent that the nodes at
#'   their other ends have the same neighbours.
#'   This is the Jaccard similarity of those two nodes' inclusive
#'   neighbourhoods, that is each node's neighbours together with itself.
#'   Ties that share no node have no similarity.
#'
#'   The ties are then clustered hierarchically by single linkage,
#'   and the tree is cut where the partition density is highest.
#'   See [net_by_linkdensity()] for this density.
#'
#'   The algorithm reads only which nodes are tied.
#'   Ties between the same two nodes, whatever their direction,
#'   are placed in the same community, and tie weights are not used.
#'   A tie from a node to itself joins no two nodes, and returns `NA`.
#' @section Signed networks:
#'   A community is a cohesive subgroup, and negative ties do not carry
#'   cohesion. Where the network is signed, only its positive ties are
#'   clustered, and negative ties return `NA`.
#'   Use [manynet::to_unsigned()] first to control this yourself.
#' @section Large networks:
#'   Every tie is compared with every other tie,
#'   so the time and memory needed grow with the square of the number of ties.
#'   This is practical up to a few thousand ties.
#' @references
#' ## On link communities
#' Ahn, Yong-Yeol, James P. Bagrow, and Sune Lehmann. 2010.
#' "Link communities reveal multiscale complexity in networks".
#' _Nature_ 466(7307): 761-764.
#' \doi{10.1038/nature09182}
#' @examples
#' tie_in_community(ison_adolescents)
#' @export
tie_in_community <- function(.data){
  .data <- manynet::expect_ties(.data)
  if(manynet::is_cognitive(.data))
    return(.map_css_ties(.data, tie_in_community))
  links <- .as_links(.data)
  if(any(links$negative))
    manynet::snet_info("Using only the positive ties,",
                       "since a negative tie does not carry cohesion.")
  nlinks <- nrow(links$ends)
  if(nlinks > 5000)
    manynet::snet_warn("Comparing {nlinks} ties with each other",
                       "may be slow and need a lot of memory.")
  memb <- .cluster_links(links$ends, manynet::net_nodes(.data))
  make_tie_member(memb[links$link], .data)
}

# Link communities read only which pairs of nodes are tied, so parallel and
# reciprocated ties are one link. This gives the two nodes of each link, and
# the link that each tie of the network belongs to. A tie that cannot be part
# of a link community, because it is a loop or negative, belongs to no link.
.as_links <- function(.data){
  graph <- manynet::as_igraph(.data)
  ends <- igraph::ends(graph, igraph::E(graph), names = FALSE)
  ends <- cbind(pmin(ends[,1], ends[,2]), pmax(ends[,1], ends[,2]))
  negative <- as.numeric(manynet::tie_signs(graph)) < 0
  valid <- ends[,1] != ends[,2] & !negative
  keys <- paste(ends[,1], ends[,2])
  keys[!valid] <- NA
  link <- match(keys, unique(keys[valid]))
  list(ends = ends[valid & !duplicated(keys), , drop = FALSE],
       link = link, negative = negative)
}

# The partition of the links with the highest partition density.
.cluster_links <- function(ends, nodes){
  nlinks <- nrow(ends)
  if(nlinks < 2) return(seq_len(nlinks))
  nbrs <- matrix(0, nodes, nodes)
  nbrs[ends] <- 1
  nbrs[ends[, 2:1, drop = FALSE]] <- 1
  diag(nbrs) <- 1
  shared <- tcrossprod(nbrs)
  sizes <- diag(shared)
  jaccard <- shared / (outer(sizes, sizes, "+") - shared)
  # only links that share a node are similar, by how far the nodes at their
  # other ends share neighbours
  dists <- matrix(1, nlinks, nlinks)
  for(node in seq_len(nodes)){
    at <- which(ends[,1] == node | ends[,2] == node)
    if(length(at) < 2) next
    others <- ifelse(ends[at, 1] == node, ends[at, 2], ends[at, 1])
    dists[at, at] <- 1 - jaccard[others, others]
  }
  diag(dists) <- 0
  hc <- stats::hclust(stats::as.dist(dists), method = "single")
  # links merged at a distance of 1 share no node, so no cut is made there
  heights <- unique(hc$height[hc$height < 1])
  best <- seq_len(nlinks)
  best_density <- 0
  for(h in heights){
    memb <- stats::cutree(hc, h = h)
    density <- .link_density(ends, memb)
    if(density > best_density){
      best <- memb
      best_density <- density
    }
  }
  unname(best)
}

# Partition density (Ahn et al. 2010): the mean, weighted by links, of how far
# each community's links exceed those of a tree on its nodes, as a share of
# what a clique on those nodes would add.
.link_density <- function(ends, memb){
  dens <- vapply(split(seq_len(nrow(ends)), memb), function(x){
    m <- length(x)
    n <- length(unique(c(ends[x, ])))
    if(n < 3) return(0)
    m * (m - (n - 1)) / (n * (n - 1) / 2 - (n - 1))
  }, FUN.VALUE = numeric(1))
  sum(dens) / nrow(ends)
}
