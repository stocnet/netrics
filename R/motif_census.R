# Node path ####

#' Motifs of node pathing
#' @name motif_path
#' @description
#'   These functions include ways to take a census of the positions of nodes
#'   in a network: 
#'   
#'   - `node_x_tie()` returns a census of the ties in a network.
#'   For directed networks, out-ties and in-ties are bound together.
#'   For multiplex networks, the various types of ties are bound together.
#'   - `node_x_path()` returns the shortest path lengths
#'   of each node to every other node in the network.
#'   Where the network is weighted, stronger ties are shorter steps:
#'   each tie costs the mean tie weight divided by its own weight.
#'   Nodes that cannot reach each other are an infinite distance apart.
#'   
#' @details
#'   For multiplex networks, `node_x_tie()` binds the layers together,
#'   giving one block of columns
#'   per layer, whatever attribute the network is multiplexed on.
#'   Each block stays the length of the whole nodeset.
#'   To census one layer alone, take it first with [manynet::to_uniplex()].
#'
#'   For signed networks, `node_x_path()` considers only the positive ties.
#'   It reads a tie as a distance, and a negative tie is
#'   hostility rather than a channel along which cohesion travels.
#'   Use [manynet::to_unsigned()] first to control this yourself.
#' @template param_data
#' @template node_motif
#' @importFrom igraph vcount make_ego_graph delete_vertices triad_census

NULL

#' @rdname motif_path 
#' @examples
#' task_eg <- to_named(to_uniplex(ison_algebra, "tasks"))
#' (tie_cen <- node_x_tie(task_eg))
#' @export
node_x_tie <- function(.data){
  .data <- manynet::expect_nodes(.data)
  .data <- .to_aggregated_css(.data)
  object <- manynet::as_igraph(.data)
  # Only tie-level waves split the census; a diffusion model's ties do not change
  waved <- "wave" %in% manynet::net_tie_attributes(object)
  if (manynet::is_multiplex(.data)) {
    # The layers are stacked into one census, which needs them to share a
    # node set. `to_uniplex()` drops the nodes a layer does not tie, so a
    # network mixing a one-mode and a two-mode layer has nothing to stack.
    sizes <- vapply(manynet::layer_names(object),
                    function(l) manynet::net_nodes(manynet::to_uniplex(object, l)),
                    FUN.VALUE = numeric(1))
    if (length(unique(sizes)) > 1)
      manynet::snet_unavailable(
        "A tie census over layers that do not share a node set",
        "is not yet available.",
        "Here the {.val {names(sizes)}} layers hold {sizes} nodes.",
        "Please use {.fn to_uniplex} to census a single layer,",
        "or {.fn net_x_triad}, which does span a one-mode and a two-mode layer.")
  }
  if (manynet::is_directed(object)) {
    if (manynet::is_multiplex(.data)) {
      mat <- do.call(rbind, lapply(manynet::layer_names(object), 
                                   function(x){
                                     rc <- manynet::as_matrix(manynet::to_uniplex(object, x))
                                     rbind(rc, t(rc))
                                   }))
    } else if (waved){
      mat <- do.call(rbind, lapply(unique(manynet::tie_attribute(object, "wave")), 
                                   function(x){
                                     rc <- manynet::as_matrix(manynet::to_waves(object)[[x]])
                                     rbind(rc, t(rc))
                                   }))
      
    } else {
      rc <- manynet::as_matrix(object)
      mat <- rbind(rc, t(rc))
    }
  } else {
    if (manynet::is_multiplex(.data)) {
      mat <- do.call(rbind, lapply(manynet::layer_names(object), 
                                   function(x){
                                     manynet::as_matrix(manynet::to_uniplex(object, x))
                                   }))
    } else if (waved){
      mat <- do.call(rbind, lapply(unique(manynet::tie_attribute(object, "wave")), 
                                   function(x){
                                     manynet::as_matrix(manynet::to_waves(object)[[x]])
                                   }))
    } else if (manynet::is_twomode(.data)) {
      mat <- manynet::as_matrix(manynet::to_onemode(object))
    } else {
      mat <- manynet::as_matrix(object)
    }
  }
  if(manynet::is_labelled(object) & manynet::is_directed(object))
    if(manynet::is_multiplex(.data)){
      rownames(mat) <- apply(expand.grid(c(paste0("from", manynet::node_names(object)),
                                           paste0("to", manynet::node_names(object))),
                                         manynet::layer_names(object)), 
                             1, paste, collapse = "_")
    } else if (waved){
      rownames(mat) <- apply(expand.grid(c(paste0("from", manynet::node_names(object)),
                                           paste0("to", manynet::node_names(object))),
                                         unique(manynet::tie_attribute(object, "wave"))), 
                             1, paste, collapse = "_wave")
    } else {
      rownames(mat) <- rep(c(paste0("from", manynet::node_names(object)),
                             paste0("to", manynet::node_names(object))))
    }
  make_node_motif(t(mat), object)
}

#' @rdname motif_path 
#' @importFrom igraph distances
#' @references 
#' ## On paths
#' Dijkstra, Edsger W. 1959. 
#' "A note on two problems in connexion with graphs". 
#' _Numerische Mathematik_ 1, 269-71.
#' \doi{10.1007/BF01386390}.
#' 
#' Opsahl, Tore, Filip Agneessens, and John Skvoretz. 2010.
#' "Node centrality in weighted networks: Generalizing degree and shortest paths". 
#' _Social Networks_ 32(3): 245-51.
#' \doi{10.1016/j.socnet.2010.03.006}.
#' @examples 
#' node_x_path(ison_adolescents)
#' node_x_path(ison_southern_women)
#' @export
node_x_path <- function(.data){
  .data <- manynet::expect_nodes(.data)
  .data <- .to_aggregated_css(.data)
  .data <- manynet::to_positive(.data)
  object <- manynet::as_igraph(.data)
  if(manynet::is_weighted(.data)){
    # A stronger tie is a shorter step (Opsahl et al. 2010): each tie costs
    # the mean tie weight over its own, so an average tie is one step.
    weights <- igraph::E(object)$weight
    out <- igraph::distances(object, weights = mean(weights)/weights)
  } else out <- igraph::distances(object)
  diag(out) <- 0
  make_node_motif(out, .data)
}

# Node cohesion ####

#' Motifs of node cohesion
#' @name motif_node
#' @description
#'   These functions include ways to take a census of the positions of nodes
#'   in a network: 
#'   
#'   - `node_x_dyad()` returns a census of the dyad configurations
#'   nodes are embedded in.
#'   - `node_x_triad()` returns a census of the triad configurations
#'   nodes are embedded in.
#'   - `node_x_tetrad()` returns a census of nodes' positions
#'   in motifs of four nodes.
#'
#'   The sections below first say how to read each census,
#'   and then how signs, layers, and attributes add to it.
#'   The `node_x_*()` functions use the same labels as the `net_x_*()` functions, 
#'   with one row per node, counting the motifs that each node takes part in.
#'   They do not tell apart the positions a node can hold within a motif,
#'   so in the chain A->B->C all three nodes count one `021C`.
#' @template section_census
#' @template param_data
#' @template param_attr
#' @param ... Not used, but kept for symmetry with [net_x_triad()].
#' @family cohesion
#' @template node_motif
#' @importFrom igraph vcount make_ego_graph delete_vertices triad_census
NULL

#' @rdname motif_node 
#' @references
#' ## On the dyad census
#' Holland, Paul W., and Samuel Leinhardt. 1970. 
#' "A Method for Detecting Structure in Sociometric Data". 
#' _American Journal of Sociology_, 76: 492-513.
#' \doi{10.1016/B978-0-12-442450-0.50028-6}
#' @examples 
#' node_x_dyad(ison_networkers)
#' node_x_dyad(ison_networkers, attribute = "Discipline")
#' @export
node_x_dyad <- function(.data, attribute = NULL, ...) {
  .census_no_dots(...)
  .data <- manynet::expect_nodes(.data)
  .data <- .to_aggregated_css(.data)
  st <- .census_states(.data)
  attr <- .census_attribute(.data, attribute)
  n <- nrow(st$states)
  out <- .census_node(t(utils::combn(n, 2)), n, "dyad",
                      manynet::is_directed(.data), attr, st)
  .census_motif(out, .data, st, node = TRUE)
}

#' @rdname motif_node 
#' @references 
#' ## On the triad census
#' Davis, James A., and Samuel Leinhardt. 1967. 
#' “\href{https://files.eric.ed.gov/fulltext/ED024086.pdf}{The Structure of Positive Interpersonal Relations in Small Groups}.” 55.
#' 
#' Schoch, David. 2023.
#' "signnet: An R package for analyzing signed networks".
#' _Journal of Open Source Software_ 8(81): 4987.
#' \doi{10.21105/joss.04987}
#' 
#' Hollway, James, Alessandro Lomi, Francesca Pallotti, and Christoph Stadtfeld. 2017.
#' “Multilevel Social Spaces: The Network Dynamics of Organizational Fields.” 
#' _Network Science_ 5(2): 187–212.
#' \doi{10.1017/nws.2017.8}
#' @examples 
#' task_eg <- to_named(to_uniplex(ison_algebra, "tasks"))
#' (triad_cen <- node_x_triad(task_eg))
#' @export
node_x_triad <- function(.data, attribute = NULL, ...){
  .census_no_dots(...)
  .data <- manynet::expect_nodes(.data)
  .data <- .to_aggregated_css(.data)
  # the mixed census has no parts of its own, so each node's triads are
  # those the network loses when the node is deleted
  if(manynet::is_twomode(.data)){
    out <- t(sapply(seq.int(manynet::net_nodes(.data)), 
                    function(x) net_x_triad(.data, attribute) - 
                      net_x_triad(manynet::delete_nodes(.data, x), attribute)))
    return(make_node_motif(out, .data))
  }
  st <- .census_states(.data)
  attr <- .census_attribute(.data, attribute)
  out <- .census_node_triads(st, manynet::is_directed(.data), attr)
  .census_motif(out, .data, st, node = TRUE)
}

#' @rdname motif_node
#' @references
#' ## On the tetrad census
#'  Ortmann, Mark, and Ulrik Brandes. 2017. 
#'  “Efficient Orbit-Aware Triad and Quad Census in Directed and Undirected Graphs.” 
#'  \emph{Applied Network Science} 2(1):13. 
#'  \doi{10.1007/s41109-017-0027-2}.
#'  
#'  McMillan, Cassie, and Diane Felmlee. 2020.
#'  "Beyond Dyads and Triads: A Comparison of Tetrads in Twenty Social Networks".
#'  _Social Psychology Quarterly_ 83(4): 383-404.
#'  \doi{10.1177/0190272520944151}
#' @examples 
#' node_x_tetrad(ison_southern_women)
#' node_x_tetrad(ison_adolescents, attribute = rep(c("x", "y"), 4))
#' @export
node_x_tetrad <- function(.data, attribute = NULL, ...){
  .census_no_dots(...)
  .data <- manynet::expect_nodes(.data)
  .data <- .to_aggregated_css(.data)
  st <- .census_tetrad_states(.data)
  attr <- .census_attribute(.data, attribute)
  out <- .census_node_tetrads(st, attr)
  make_node_motif(out, .data)
}

# https://stackoverflow.com/questions/26828301/faster-version-of-combn#26828486
# comb2.int <- function(n, choose = 2){
#   # e.g. n=3 => (1,2), (1,3), (2,3)
#   x <- rep(1:n,(n:1)-1)
#   i <- seq_along(x)+1
#   o <- c(0,cumsum((n-2):1))
#   y <- i-o[x]
#   return(cbind(x,y))
# }
  
# Network cohesion ####

#' Motifs of network cohesion
#' @name motif_net
#' @description
#'   These functions include ways to take a census of the motifs
#'   in a network: 
#'   
#'   - `net_x_dyad()` returns a census of dyad motifs in a network.
#'   - `net_x_triad()` returns a census of triad motifs in a network.
#'   - `net_x_tetrad()` returns a census of tetrad motifs in a network.
#'
#'   The sections below first say how to read each census,
#'   and then how signs, layers, and attributes add to it.
#'   
#' @template section_census
#' @template param_data
#' @template param_attr
#' @param ... Only `net_x_triad()` reads this, and only to take a second,
#'   two-mode network for the mixed census, which is deprecated;
#'   see the section on multiplex and multilevel networks.
#' @family cohesion
#' @template net_motif
#' @references
#' ## On signed and attributed censuses
#' Schoch, David. 2023.
#' "signnet: An R package for analyzing signed networks".
#' _Journal of Open Source Software_ 8(81): 4987.
#' \doi{10.21105/joss.04987}
NULL

#' @rdname motif_net
#' @references
#' ## On the dyad census
#' Holland, Paul W., and Samuel Leinhardt. 1970. 
#' "A Method for Detecting Structure in Sociometric Data". 
#' _American Journal of Sociology_, 76: 492-513.
#' \doi{10.1016/B978-0-12-442450-0.50028-6}
#' 
#' Wasserman, Stanley, and Katherine Faust. 1994. 
#' "Social Network Analysis: Methods and Applications". 
#' Cambridge: Cambridge University Press.
#' @examples 
#' net_x_dyad(manynet::ison_algebra)
#' @export
net_x_dyad <- function(.data, attribute = NULL, ...) {
  .census_no_dots(...)
  .data <- manynet::expect_nodes(.data)
  .data <- .to_aggregated_css(.data)
  if(.census_is_plain(.data, attribute)){
    out <- suppressWarnings(igraph::dyad_census(manynet::as_igraph(.data)))
    out <- unlist(out)
    names(out) <- c("Mutual", "Asymmetric", "Null")
    if (!manynet::is_directed(.data)) out <- out[c(1, 3)]
    return(make_network_motif(out, .data))
  }
  st <- .census_states(.data)
  attr <- .census_attribute(.data, attribute)
  directed <- manynet::is_directed(.data)
  sets <- t(utils::combn(nrow(st$states), 2))
  out <- .census_net(sets, "dyad", directed, attr, st)
  .census_motif(out, .data, st)
}

# Keeps the letters of the layers with the census, so they can be read back.
.census_motif <- function(out, .data, st, node = FALSE){
  out <- if(node) make_node_motif(out, .data) else make_network_motif(out, .data)
  if(!is.null(st$layers)) attr(out, "layers") <- st$layers
  out
}

#' @rdname motif_net
#' @references 
#' ## On the triad census
#' Davis, James A., and Samuel Leinhardt. 1967. 
#' “\href{https://files.eric.ed.gov/fulltext/ED024086.pdf}{The Structure of Positive Interpersonal Relations in Small Groups}.” 55.
#' 
#' Schoch, David. 2023.
#' "signnet: An R package for analyzing signed networks".
#' _Journal of Open Source Software_ 8(81): 4987.
#' \doi{10.21105/joss.04987}
#' 
#' Hollway, James, Alessandro Lomi, Francesca Pallotti, and Christoph Stadtfeld. 2017.
#' “Multilevel Social Spaces: The Network Dynamics of Organizational Fields.” 
#' _Network Science_ 5(2): 187–212.
#' \doi{10.1017/nws.2017.8}
#' @examples 
#' net_x_triad(manynet::ison_adolescents)
#' net_x_triad(ison_networkers, attribute = "Discipline")
#' net_x_triad(to_uniplex(fict_marvel, "relationship"))
#' net_x_triad(fict_marvel)
#' @export
net_x_triad <- function(.data, attribute = NULL, ...) {
  .data <- manynet::expect_nodes(.data)
  .data <- .to_aggregated_css(.data)
  # A second network used to be passed as `object2`, often positionally,
  # where `attribute` now sits. Both spellings still reach the mixed census.
  dots <- list(...)
  object2 <- dots[["object2"]]
  if(is.null(object2) && length(dots) > 0 && is.null(names(dots)))
    object2 <- dots[[1]]
  if(!is.null(attribute) && manynet::is_graph(attribute)){
    object2 <- attribute
    attribute <- NULL
  }
  if(!is.null(object2)){
    manynet::snet_info("Passing a second network to {.fn net_x_triad}",
                       "is deprecated.",
                       "Please pass one multilevel network as {.arg .data}.")
    return(make_network_motif(.mixed_census(.data, object2), .data))
  }
  if(length(dots) > 0) .census_no_dots(...)
  if(manynet::is_multiplex(.data)){
    # a network carrying both a one-mode and a two-mode layer already holds
    # everything the multilevel census needs, so use it rather than refuse
    layers <- manynet::layer_names(.data)
    parts <- lapply(layers, function(l) suppressMessages(manynet::to_uniplex(.data, l)))
    twomode <- vapply(parts, manynet::is_twomode, FUN.VALUE = logical(1))
    # the layers are told apart by their mode rather than by their order,
    # since the census needs the one-mode network first
    if(length(layers) == 2 && sum(twomode) == 1){
      if(!is.null(attribute))
        manynet::snet_unavailable("A mixed census by attribute is not yet available.")
      manynet::snet_info("Taking a mixed census over the",
                         "{.val {layers[!twomode]}} and",
                         "{.val {layers[twomode]}} layers.")
      onemode <- parts[[which(!twomode)]]
      return(make_network_motif(.mixed_census(onemode, parts[[which(twomode)]]),
                                onemode))
    }
    if(any(twomode))
      manynet::snet_unavailable(
        "A triad census over {.val {sum(!twomode)}} one-mode and",
        "{.val {sum(twomode)}} two-mode layers is not yet available.",
        "Please use {.fn to_uniplex} to select one one-mode layer",
        "and one two-mode layer for the mixed census.")
  }
  if (manynet::is_twomode(.data))
    manynet::snet_abort("A twomode or multilevel option for a triad census is not yet implemented.")
  directed <- manynet::is_directed(.data)
  if(.census_is_plain(.data, attribute)){
    out <- suppressWarnings(igraph::triad_census(manynet::as_igraph(.data)))
    names(out) <- .triad_names
    if (!directed) out <- out[c(1, 2, 3, 11, 15, 16)]
    return(make_network_motif(out, .data))
  }
  st <- .census_states(.data)
  attr <- .census_attribute(.data, attribute)
  .census_motif(.census_triads(st, directed, attr), .data, st)
}

#' @rdname motif_net
#' @references
#' ## On the tetrad census
#'  Ortmann, Mark, and Ulrik Brandes. 2017. 
#'  “Efficient Orbit-Aware Triad and Quad Census in Directed and Undirected Graphs.” 
#'  \emph{Applied Network Science} 2(1):13. 
#'  \doi{10.1007/s41109-017-0027-2}.
#'  
#'  McMillan, Cassie, and Diane Felmlee. 2020.
#'  "Beyond Dyads and Triads: A Comparison of Tetrads in Twenty Social Networks".
#'  _Social Psychology Quarterly_ 83(4): 383-404.
#'  \doi{10.1177/0190272520944151}
#' @examples 
#' net_x_tetrad(ison_southern_women)
#' @export
net_x_tetrad <- function(.data, attribute = NULL, ...){
  .census_no_dots(...)
  .data <- manynet::expect_nodes(.data)
  .data <- .to_aggregated_css(.data)
  st <- .census_tetrad_states(.data)
  attr <- .census_attribute(.data, attribute)
  make_network_motif(.census_tetrads(st, attr), .data)
}

# The tetrad census reads every tie as present or absent and undirected,
# whatever its layer, sign, or weight.
.census_tetrad_states <- function(.data){
  if(manynet::is_signed(.data))
    manynet::snet_info("The tetrad census sets signs aside.")
  mat <- manynet::as_matrix(manynet::to_onemode(.data)) != 0
  mat <- (mat | t(mat)) * 1
  diag(mat) <- 0
  list(states = mat, base = 2, layers = NULL, signed = FALSE)
}

# The multilevel triad census of Hollway et al. (2017), over a one-mode
# network and a two-mode network that share their first mode.
# `net_x_triad()` wraps this, either over a supplied pair of networks or over
# the two layers of a multiplex network.
.mixed_census <- function (.data, object2) {
  if(manynet::is_twomode(.data))
    manynet::snet_abort("First object should be a one-mode network")
  if(!manynet::is_twomode(object2))
    manynet::snet_abort("Second object should be a two-mode network")
  if(manynet::net_dims(.data)[1] != manynet::net_dims(object2)[1])
    manynet::snet_abort("Non-conformable arrays")
  # A census counts configurations, so only the presence of a tie matters.
  # The matrices are made binary because the arithmetic below takes the
  # complement of each, which a weight or a negative sign would corrupt:
  # `igraph::triad_census()` ignores weights for the same reason.
  m1 <- (manynet::as_matrix(.data) != 0) * 1
  m2 <- (manynet::as_matrix(object2) != 0) * 1
  cp <- function(m) (-m + 1)
  onemode.reciprocal <- m1 * t(m1)
  onemode.forward <- m1 * cp(t(m1))
  onemode.backward <- cp(m1) * t(m1)
  onemode.null <- cp(m1) * cp(t(m1))
  diag(onemode.forward) <- 0
  diag(onemode.backward) <- 0
  diag(onemode.null) <- 0
  bipartite.twopath <- m2 %*% t(m2)
  bipartite.null <- cp(m2) %*% cp(t(m2))
  bipartite.onestep1 <- m2 %*% cp(t(m2))
  bipartite.onestep2 <- cp(m2) %*% t(m2)
  diag(bipartite.twopath) <- 0
  diag(bipartite.null) <- 0
  diag(bipartite.onestep1) <- 0
  diag(bipartite.onestep2) <- 0
  res <- c("22" = sum(onemode.reciprocal * bipartite.twopath) / 2,
           "21" = sum(onemode.forward * bipartite.twopath) / 2 + sum(onemode.backward * bipartite.twopath) / 2,
           "20" = sum(onemode.null * bipartite.twopath) / 2,
           "12" = sum(onemode.reciprocal * bipartite.onestep1) / 2 + sum(onemode.reciprocal * bipartite.onestep2) / 2,
           "11D" = sum(onemode.forward * bipartite.onestep1) / 2 + sum(onemode.backward * bipartite.onestep2) / 2,
           "11U" = sum(onemode.forward * bipartite.onestep2) / 2 + sum(onemode.backward * bipartite.onestep1) / 2,
           "10" = sum(onemode.null * bipartite.onestep2) / 2 + sum(onemode.null * bipartite.onestep1) / 2,
           "02" = sum(onemode.reciprocal * bipartite.null) / 2,
           "01" = sum(onemode.forward * bipartite.null) / 2 + sum(onemode.backward * bipartite.null) / 2,
           "00" = sum(onemode.null * bipartite.null) / 2)  
  res
}

# Census helpers ####

# The plain census is kept on its fast path wherever there is nothing more for
# it to read: no signs, no attribute, and no one-mode layers to tell apart.
.census_is_plain <- function(.data, attribute){
  is.null(attribute) && !manynet::is_signed(.data) && !.census_is_layered(.data)
}

# Layers are told apart only where they all share the one node set,
# which a multiplex of one-mode layers does.
.census_is_layered <- function(.data){
  manynet::is_multiplex(.data) && !manynet::is_twomode(.data) &&
    length(manynet::layer_names(.data)) > 1
}

# The census functions keep `...` for symmetry with `net_x_triad()`,
# where it still takes a deprecated second network, but use it for nothing.
.census_no_dots <- function(...){
  if(...length() > 0)
    manynet::snet_abort("This census takes no further arguments.",
                        "Did you mean to name the {.arg attribute}?")
}

# The state of each ordered pair of nodes, as one integer.
# Within a layer a pair is 0 (untied), 1 (negatively tied), or 2 (positively
# tied), so that positive ties come first in a motif's canonical order. Over layers these combine as digits in base 3, so one integer records
# which layers tie the pair and with which sign. Weights count only as
# presence, as they do in `igraph::triad_census()`.
.census_states <- function(.data){
  parts <- list(.data)
  layers <- NULL
  if(.census_is_layered(.data)){
    names <- manynet::layer_names(.data)
    split <- lapply(names, function(l)
      suppressMessages(manynet::to_uniplex(.data, l)))
    sizes <- vapply(split, manynet::net_nodes, FUN.VALUE = numeric(1))
    if(all(sizes == manynet::net_nodes(.data))){
      parts <- split
      layers <- stats::setNames(names, .census_letters(names))
    }
  }
  states <- 0
  for(l in seq_along(parts))
    states <- states + .census_layer_state(parts[[l]]) * 3^(l-1)
  diag(states) <- 0 # a census counts no loops
  list(states = states, base = 3^length(parts), layers = layers,
       signed = any(vapply(parts, manynet::is_signed, FUN.VALUE = logical(1))))
}

.census_layer_state <- function(x){
  x <- manynet::to_onemode(x)
  if(manynet::is_signed(x)){
    pos <- manynet::as_matrix(manynet::to_unsigned(x, "positive")) != 0
    neg <- manynet::as_matrix(manynet::to_unsigned(x, "negative")) != 0
    out <- pos * 2
    out[neg] <- 1
  } else out <- (manynet::as_matrix(x) != 0) * 2
  out
}

# One letter for each layer: its first letter where these are all different,
# and otherwise a, b, c, ... in layer order. A lowercase letter marks a
# positive tie and an uppercase letter a negative one.
.census_letters <- function(layers){
  first <- tolower(substr(layers, 1, 1))
  if(anyDuplicated(first) || any(!grepl("^[a-z]$", first)))
    first <- letters[seq_along(layers)]
  first
}

# The groups of an attribute, as 0-based codes. A categorical attribute
# (character, factor, or logical) is grouped by category, with missing values
# a category of their own. An ordered attribute (numeric, or an ordered factor)
# is grouped by distinct value, in order, so that a motif can say which of its
# positions are lower or higher; its values cannot be missing.
.census_attribute <- function(.data, attribute){
  if(is.null(attribute)) return(NULL)
  attr <- .resolve_attribute(.data, attribute)
  ordered <- is.numeric(attr) || is.ordered(attr)
  if(ordered){
    if(anyNA(attr))
      manynet::snet_abort("An ordered attribute cannot have missing values,",
                          "since they cannot be compared.",
                          "Please impute or remove them first.")
    if(is.numeric(attr) && length(unique(attr)) < 10)
      manynet::snet_info("Treating this numeric attribute as ordered.",
                         "If it codes categories, pass it {.code as.factor()} instead.")
    codes <- match(attr, sort(unique(attr))) - 1L
  } else codes <- as.integer(droplevels(factor(attr, exclude = NULL))) - 1L
  list(codes = codes, groups = max(codes) + 1L, ordered = ordered)
}

.census_size <- function(kind) c(dyad = 2L, triad = 3L, tetrad = 4L)[[kind]]

# Every ordering of k positions, one per row.
.census_perms <- function(k){
  if(k == 1) return(matrix(1L))
  sub <- .census_perms(k - 1)
  do.call(rbind, lapply(seq_len(k), function(i)
    cbind(i, matrix(setdiff(seq_len(k), i)[sub], ncol = k - 1))))
}

# The pairs of positions a motif's ties can fall between, in a fixed order.
.census_pairs <- function(k, directed){
  pairs <- which(matrix(TRUE, k, k), arr.ind = TRUE)
  pairs <- pairs[pairs[,1] != pairs[,2], , drop = FALSE]
  if(!directed) pairs <- pairs[pairs[,1] < pairs[,2], , drop = FALSE]
  unname(pairs[order(pairs[,1], pairs[,2]), , drop = FALSE])
}

# A canonical code for each set of nodes (one per row of `sets`).
# Each ordering of the set's positions is read as a number: first whether
# each pair of positions is tied, then the state of each pair (its signs and
# layers), then how the positions compare on any attribute. The largest of these numbers
# over all orderings is the set's code, so two sets share a code exactly where
# some relabelling of their positions makes their ties, signs, layers, and
# categories the same. Reading the bare ties first fixes where in the order a
# motif's ties fall, whatever their signs, layers, or categories.
# `states = NULL` reads every pair as untied, and `states` may also be a
# function of two vectors of nodes returning the state of each pair.
# Codes are tabulated by name, and `as.character()` keeps 15 significant
# digits, so a code may not exceed 1e15.
.census_code <- function(states, sets, directed, attr, base){
  k <- ncol(sets)
  pairs <- .census_pairs(k, directed)
  abase <- .census_abase(attr, k)
  sbase <- if(base > 2) base else 1
  if(2^nrow(pairs) * sbase^nrow(pairs) * abase^k > 1e15)
    manynet::snet_unavailable("A census over this many layers and categories",
                              "is not yet available.",
                              "Please use {.fn to_uniplex} to select fewer layers.")
  perms <- .census_perms(k)
  best <- NULL
  for(p in seq_len(nrow(perms))){
    pos <- sets[, perms[p,], drop = FALSE]
    code <- numeric(nrow(sets))
    if(!is.null(states)){
      get <- if(is.function(states)) states else
        function(i, j) states[cbind(i, j)]
      tied <- lapply(seq_len(nrow(pairs)), function(r)
        get(pos[,pairs[r,1]], pos[,pairs[r,2]]))
      for(t in tied) code <- code * 2 + (t > 0)
      if(sbase > 1) for(t in tied) code <- code * sbase + t
    }
    if(!is.null(attr)){
      av <- matrix(attr$codes[pos], ncol = k)
      # ordered ranks are reversed, so that the largest code lists the
      # positions in ascending order
      av <- if(attr$ordered) k - 1L - .census_rank(av) else .census_pattern(av)
      for(a in seq_len(k)) code <- code * abase + av[,a]
    }
    best <- if(is.null(best)) code else pmax(best, code)
  }
  best
}

.census_abase <- function(attr, k) if(is.null(attr)) 1 else k

# The attribute digits of a code, read back as values that relate as the
# positions do (undoing the reversal of ordered ranks).
.census_adig_values <- function(adig, attr, k){
  if(attr$ordered) k - 1 - adig else adig
}

# Recode each row's values by their rank among the distinct values in that
# row, so that only their order remains: (5, 2, 5) becomes (1, 0, 1).
.census_rank <- function(av){
  k <- ncol(av)
  first <- matrix(TRUE, nrow(av), k)
  for(a in seq_len(k)[-1]) for(b in seq_len(a - 1))
    first[,a] <- first[,a] & av[,a] != av[,b]
  rk <- matrix(0L, nrow(av), k)
  for(a in seq_len(k)) for(b in seq_len(k))
    rk[,a] <- rk[,a] + (first[,b] & av[,b] < av[,a])
  rk
}

# Recode each row's categories by order of first appearance, so that only
# which positions share a category remains: (5, 2, 5) becomes (0, 1, 0).
.census_pattern <- function(av){
  k <- ncol(av)
  rg <- matrix(NA_integer_, nrow(av), k)
  rg[,1] <- 0L
  nxt <- rep(1L, nrow(av))
  for(a in seq_len(k)[-1]){
    for(b in seq_len(a - 1)){
      hit <- is.na(rg[,a]) & av[,a] == av[,b]
      rg[hit,a] <- rg[hit,b]
    }
    new <- is.na(rg[,a])
    rg[new,a] <- nxt[new]
    nxt[new] <- nxt[new] + 1L
  }
  rg
}

.triad_names <- c("003", "012", "102", "021D", "021U", "021C", "111D", "111U",
                  "030T", "030C", "201", "120D", "120U", "120C", "210", "300")
.tetrad_names <- c("E4","I4","H4","L4","D4","U4","Y4","P4","C4","Z4","X4")

# The motifs of the plain census, in the order it reports them.
.census_bases <- function(kind, directed){
  switch(kind,
         dyad = if(directed) c("Mutual", "Asymmetric", "Null") else
           c("Mutual", "Null"),
         triad = if(directed) .triad_names else .triad_names[c(1, 2, 3, 11, 15, 16)],
         tetrad = .tetrad_names)
}

# The plain motif of a symmetric or directed adjacency matrix of k positions.
.census_base <- function(adj, kind, directed){
  switch(kind,
         dyad = if(directed) c("Null", "Asymmetric", "Mutual")[sum(adj) + 1] else
           c("Null", "Mutual")[sum(adj)/2 + 1],
         triad = if(directed)
           .triad_names[which(suppressWarnings(igraph::triad_census(
             igraph::graph_from_adjacency_matrix(adj))) > 0)] else
               c("003", "102", "201", "300")[sum(adj)/2 + 1],
         tetrad = .tetrad_name(adj))
}

# The tetrads are told apart by their number of ties (counted from both ends)
# and the largest degree among their four nodes. Three ties need the smallest
# degree as well, since a line and a triangle with an isolate both reach 2.
.tetrad_name <- function(adj){
  dens <- sum(adj)
  deg <- rowSums(adj)
  if(dens == 0) "E4" else if(dens == 2) "I4" else
    if(dens == 4) c("H4", "L4")[max(deg)] else
      if(dens == 6) (if(max(deg) == 3) "Y4" else if(min(deg) == 0) "D4" else "U4") else
        if(dens == 8) c(NA, "C4", "P4")[max(deg)] else
          if(dens == 10) "Z4" else "X4"
}

# Names each canonical code as `<motif>_<ties>_<relations>`.
# The ties are given, in the order of the canonical labelling, only where the
# network is signed or layered. The relations are given only where an
# attribute is: for each pair of positions in turn (1-2, 1-3, ..., 3-4),
# `=` or `!` where they share a category or not, or `<`, `=`, `>` where the
# first is lower than, equal to, or higher than the second.
.census_label <- function(codes, kind, directed, attr, st){
  k <- .census_size(kind)
  pairs <- .census_pairs(k, directed)
  abase <- .census_abase(attr, k)
  sbase <- if(st$base > 2) st$base else 1
  np <- nrow(pairs)
  ties <- !is.null(st$layers) || st$signed
  vapply(codes, function(code){
    adig <- NULL
    if(!is.null(attr)){
      adig <- .census_adig_values(rev(code %/% abase^(0:(k-1)) %% abase), attr, k)
      code <- code %/% abase^k
    }
    if(sbase > 1){
      tdig <- rev(code %/% sbase^(0:(np-1)) %% sbase)
      code <- code %/% sbase^np
    } else tdig <- rev(code %/% 2^(0:(np-1)) %% 2)
    adj <- matrix(0, k, k)
    adj[pairs[tdig > 0, , drop = FALSE]] <- 1
    if(!directed) adj <- adj + t(adj)
    out <- .census_base(adj, kind, directed)
    if(ties && any(tdig > 0)){
      present <- tdig[tdig > 0]
      if(is.null(st$layers)){
        out <- paste0(out, "_", paste(c("-", "+")[present], collapse = ""))
      } else {
        lets <- names(st$layers)
        toks <- vapply(present, function(t){
          d <- t %/% 3^(seq_along(lets) - 1) %% 3
          paste(ifelse(d == 1, toupper(lets), lets)[d > 0], collapse = "")
        }, FUN.VALUE = character(1))
        out <- paste0(out, "_", paste(toks, collapse = "."))
      }
    }
    if(!is.null(adig)){
      ap <- .census_pairs(k, FALSE)
      a <- adig[ap[,1]]
      b <- adig[ap[,2]]
      rel <- if(attr$ordered) ifelse(a < b, "<", ifelse(a > b, ">", "=")) else
        ifelse(a == b, "=", "!")
      out <- paste0(out, "_", paste(rel, collapse = ""))
    }
    out
  }, FUN.VALUE = character(1))
}

# Orders the classes as the plain census orders its motifs, then by name.
# Where nothing was added to the plain census, every motif is reported,
# even those that do not occur, as the plain census does.
.census_order <- function(counts, kind, directed, attr, st){
  bases <- .census_bases(kind, directed)
  plain <- is.null(attr) && is.null(st$layers) && !st$signed
  if(plain){
    full <- stats::setNames(numeric(length(bases)), bases)
    full[names(counts)] <- counts
    return(full)
  }
  counts <- counts[counts > 0]
  base <- sub("_.*", "", names(counts))
  counts[order(match(base, bases), names(counts), method = "radix")]
}

# Sums weights by group, much faster than `tapply()` over many entries,
# as a vector named by group.
.census_sum <- function(w, group){
  if(!length(w)) return(numeric(0))
  r <- rowsum(as.numeric(w), group)
  stats::setNames(r[,1], rownames(r))
}

# A network census over the given node sets.
.census_net <- function(sets, kind, directed, attr, st, extra = NULL){
  codes <- .census_code(st$states, sets, directed, attr, st$base)
  counts <- c(table(codes), extra)
  counts <- .census_sum(counts, names(counts))
  names(counts) <- .census_label(as.numeric(names(counts)), kind,
                                 directed, attr, st)
  .census_order(c(counts), kind, directed, attr, st)
}

# A node census over the given node sets: each set counts once for each of
# its members.
.census_node <- function(sets, n, kind, directed, attr, st){
  codes <- .census_code(st$states, sets, directed, attr, st$base)
  ucodes <- sort(unique(codes))
  labs <- .census_label(ucodes, kind, directed, attr, st)
  cls <- factor(labs[match(codes, ucodes)], levels = unique(labs))
  out <- unclass(table(factor(c(sets), levels = seq_len(n)),
                       rep(cls, ncol(sets))))
  out <- matrix(out, nrow = n, dimnames = list(NULL, colnames(out)))
  totals <- .census_order(colSums(out), kind, directed, attr, st)
  res <- matrix(0, n, length(totals), dimnames = list(NULL, names(totals)))
  shared <- intersect(names(totals), colnames(out))
  res[, shared] <- out[, shared]
  res
}

# The connected triads of a symmetric presence matrix: each pair of each
# node's neighbours, once, as sorted rows.
.census_connected_triads <- function(und){
  n <- nrow(und)
  ends <- which(und, arr.ind = TRUE)
  nb <- split(ends[,1], factor(ends[,2], levels = seq_len(n)))
  conn <- do.call(rbind, lapply(seq_len(n), function(v){
    x <- nb[[v]]
    if(length(x) < 2) return(NULL)
    p <- utils::combn(length(x), 2)
    cbind(v, x[p[1,]], x[p[2,]])
  }))
  if(is.null(conn)) return(matrix(integer(0), 0, 3))
  .census_unique_sets(conn, n)
}

# Sorts each row of node sets and drops repeated sets.
.census_unique_sets <- function(sets, n){
  # a bubble sort across the columns, which compares all rows at once
  k <- ncol(sets)
  for(i in seq_len(k - 1)) for(j in seq_len(k - i)){
    lo <- pmin(sets[,j], sets[,j+1])
    sets[,j+1] <- pmax(sets[,j], sets[,j+1])
    sets[,j] <- lo
  }
  key <- if(n^ncol(sets) < 2^52){
    key <- 0
    for(j in seq_len(ncol(sets))) key <- key * n + (sets[,j] - 1)
    key
  } else do.call(paste, as.data.frame(sets))
  sets[!duplicated(key), , drop = FALSE]
}

# Codes for small stand-in motifs, each given by the states of its ties
# (one row per motif, in the order of `.census_pairs()`) and the groups of
# its positions, laid out as separate blocks of one network.
.census_standin <- function(digits, cats, attr, directed = FALSE, base = 2){
  if(nrow(cats) == 0) return(numeric(0))
  # many stand-ins are alike, so each distinct one is coded once
  if(is.null(attr)) cats[] <- 0
  kind <- do.call(paste, as.data.frame(cbind(digits, cats)))
  first <- which(!duplicated(kind))
  digits <- digits[first, , drop = FALSE]
  cats <- cats[first, , drop = FALSE]
  k <- ncol(cats)
  C <- nrow(cats)
  pairs <- .census_pairs(k, directed)
  slot <- matrix(0L, k, k)
  slot[pairs] <- seq_len(nrow(pairs))
  if(!directed) slot[pairs[, 2:1, drop = FALSE]] <- seq_len(nrow(pairs))
  states <- function(i, j){
    bi <- (i - 1) %/% k
    pi <- (i - 1) %% k + 1
    pj <- (j - 1) %% k + 1
    out <- numeric(length(i))
    same <- bi == (j - 1) %/% k & pi != pj
    out[same] <- digits[cbind(bi[same] + 1, slot[cbind(pi[same], pj[same])])]
    out
  }
  sat <- attr
  if(!is.null(sat)) sat$codes <- c(t(cats))
  codes <- .census_code(states, matrix(seq_len(k * C), ncol = k, byrow = TRUE),
                        directed, sat, base)
  codes[match(kind, kind[first])]
}

# How a node's group compares with the groups of a part's members (one row
# per part). `classify()` gives, for a categorical attribute, which member it
# shares a category with (1, 2, ...), or none (0); for an ordered attribute,
# where its value falls among the members' distinct values, as 2i - 1 where it
# equals the i-th lowest and 2i where it lies above it. `standin()` gives,
# for each part and class, a value that compares with the members' values as
# any node of that class does.
.census_relclass <- function(members, ordered){
  members <- as.matrix(members)
  r <- ncol(members)
  rows <- seq_len(nrow(members))
  if(!ordered){
    classify <- function(x){
      cls <- integer(nrow(members))
      for(j in r:1) cls[x == members[,j]] <- j
      cls
    }
    standin <- function(c) ifelse(c == 0, -1, members[cbind(rows, pmax(c, 1))])
    return(list(classify = classify, standin = standin, n = r + 1, classes = 0:r))
  }
  srt <- members # a bubble sort across the columns, for all rows at once
  for(i in seq_len(r - 1)) for(j in seq_len(r - i)){
    lo <- pmin(srt[,j], srt[,j+1])
    srt[,j+1] <- pmax(srt[,j], srt[,j+1])
    srt[,j] <- lo
  }
  fresh <- cbind(TRUE, srt[, -1, drop = FALSE] != srt[, -r, drop = FALSE])
  u <- matrix(NA_real_, nrow(members), r) # the distinct values, in order
  cnt <- integer(nrow(members))
  for(j in seq_len(r)){
    cnt <- cnt + fresh[,j]
    u[cbind(rows[fresh[,j]], cnt[fresh[,j]])] <- srt[fresh[,j], j]
  }
  classify <- function(x){
    below <- integer(nrow(members))
    equal <- logical(nrow(members))
    for(j in seq_len(r)){
      below <- below + (fresh[,j] & srt[,j] < x)
      equal <- equal | srt[,j] == x
    }
    2L * below + as.integer(equal)
  }
  standin <- function(c){
    i <- (c + 1) %/% 2 # which value it equals, or lies above (0 = below all)
    eq <- c %% 2 == 1
    out <- numeric(length(c))
    out[eq] <- u[cbind(rows[eq], i[eq])]
    ab <- !eq
    v1 <- ifelse(i[ab] == 0, u[rows[ab], 1] - 1, u[cbind(rows[ab], pmax(i[ab], 1))])
    v2 <- ifelse(i[ab] >= cnt[ab], v1 + 2, u[cbind(rows[ab], pmin(i[ab] + 1, r))])
    out[ab] <- (v1 + v2) / 2
    out
  }
  list(classify = classify, standin = standin, n = 2 * r + 1,
       classes = seq_len(2 * r + 1) - 1L)
}

# Counting only the connected sets pays where they are few. In a dense
# network nearly every set is connected, and building each from its parts
# repeats it many times, so there every set is enumerated instead.
.census_dense <- function(states, k){
  und <- states != 0
  deg <- rowSums(und | t(und))
  n <- length(deg)
  paths <- sum(choose(deg, 2))
  built <- if(k == 3) paths else 3 * paths * mean(deg)
  built > choose(n, k)
}

# The plain tetrad of each set of four nodes, read directly from its number
# of ties and the degrees within it, which is much faster than the canonical
# code where every set must be classified.
.census_tetrad_plain <- function(und, sets){
  pr <- .census_pairs(4, FALSE)
  tie <- lapply(seq_len(nrow(pr)), function(r)
    und[cbind(sets[,pr[r,1]], sets[,pr[r,2]])])
  deg <- lapply(1:4, function(v) Reduce(`+`, tie[pr[,1] == v | pr[,2] == v]))
  ties <- Reduce(`+`, tie)
  hi <- do.call(pmax, deg)
  lo <- do.call(pmin, deg)
  out <- character(nrow(sets))
  out[ties == 0] <- "E4"
  out[ties == 1] <- "I4"
  out[ties == 2] <- ifelse(hi[ties == 2] == 1, "H4", "L4")
  out[ties == 3] <- ifelse(hi[ties == 3] == 3, "Y4",
                           ifelse(lo[ties == 3] == 0, "D4", "U4"))
  out[ties == 4] <- ifelse(hi[ties == 4] == 3, "P4", "C4")
  out[ties == 5] <- "Z4"
  out[ties == 6] <- "X4"
  out
}

# The tetrad parts are counted by group, which is quick for a few groups but
# not for many, such as the distinct values of a continuous attribute. Then
# every set of four nodes is enumerated instead, where there are not too many.
.census_tetrad_full <- function(st, attr){
  if(is.null(attr) || attr$groups <= 12) return(FALSE)
  if(choose(nrow(st$states), 4) > 1e7)
    manynet::snet_unavailable(
      "A tetrad census by an attribute with {.val {attr$groups}} values",
      "over {.val {nrow(st$states)}} nodes is not yet available.",
      "Please use {.fn cut} to put the attribute into fewer groups first.")
  TRUE
}

# The tetrad census enumerates only the connected sets of four nodes, by
# extending each connected triad with a neighbour. The disconnected tetrads
# are counted instead: a connected triad and an isolate (L4, D4), two ties
# that share no node and are not tied to each other (H4), one tie and two
# isolates (I4), and no tie at all (E4).
# The result is a set of parts, from which both the network census and each
# node's census are tabulated.
.census_tetrad_parts <- function(st, attr){
  und <- st$states != 0
  n <- nrow(und)
  K <- if(is.null(attr)) 1L else attr$groups
  ac <- if(is.null(attr)) integer(n) else attr$codes
  sizes <- tabulate(ac + 1L, nbins = K)
  tied <- function(a, b) und[cbind(a, b)]
  conn <- .census_connected_triads(und)
  nc <- nrow(conn)
  # connected tetrads: a connected triad and a neighbour of one of its nodes
  ends <- which(und, arr.ind = TRUE)
  nb <- split(ends[,1], factor(ends[,2], levels = seq_len(n)))
  deg <- lengths(nb)
  quad <- do.call(rbind, lapply(1:3, function(j){
    v <- conn[,j]
    cbind(conn[rep(seq_len(nc), deg[v]), , drop = FALSE],
          unlist(nb[v], use.names = FALSE))
  }))
  if(is.null(quad)) quad <- matrix(integer(0), 0, 4)
  quad <- quad[quad[,4] != quad[,1] & quad[,4] != quad[,2] &
                 quad[,4] != quad[,3], , drop = FALSE]
  if(nrow(quad)) quad <- .census_unique_sets(quad, n)
  qcode <- .census_code(st$states, quad, FALSE, attr, 2)
  # each connected tetrad's connected triads, and the node left out
  key3 <- function(a, b, c) ((a - 1) * n + (b - 1)) * n + (c - 1)
  ckey <- key3(conn[,1], conn[,2], conn[,3])
  qtri <- do.call(rbind, lapply(1:4, function(out){
    tri <- quad[, -out, drop = FALSE]
    ok <- tied(tri[,1], tri[,2]) + tied(tri[,1], tri[,3]) +
      tied(tri[,2], tri[,3]) >= 2
    cbind(t = match(key3(tri[ok,1], tri[ok,2], tri[ok,3]), ckey),
          out = quad[ok, out])
  }))
  if(is.null(qtri)) qtri <- matrix(integer(0), 0, 2)
  # a connected triad and a node of each category tied to none of it
  near <- tabulate(qtri[,1] + nc * ac[qtri[,2]], nbins = nc * K)
  memb <- matrix(0, nc, K)
  for(j in 1:3) memb[cbind(seq_len(nc), ac[conn[,j]] + 1)] <- 
    memb[cbind(seq_len(nc), ac[conn[,j]] + 1)] + 1
  iso3 <- matrix(rep(sizes, each = nc), nc, K) - memb - matrix(near, nc, K)
  pres3 <- cbind(tied(conn[,1], conn[,2]), tied(conn[,1], conn[,3]),
                 tied(conn[,2], conn[,3]))
  rows <- rep(seq_len(nc), K)
  tcode3 <- matrix(.census_standin(
    # pairs 1-2, 1-3, 1-4, 2-3, 2-4, 3-4, the fourth node untied
    cbind(pres3[rows, 1:2, drop = FALSE], 0, pres3[rows, 3], 0, 0),
    cbind(matrix(ac[conn], ncol = 3)[rows, , drop = FALSE],
          rep(seq_len(K) - 1, each = nc)), attr), nc, K)
  # ties, typed by the categories of their ends, where type t is the
  # unordered pair (t %/% K, t %% K)
  ties <- which(und & upper.tri(und), arr.ind = TRUE)
  m <- nrow(ties)
  tkey <- (ties[,1] - 1) * n + ties[,2]
  tie_of <- function(a, b) match((pmin(a, b) - 1) * n + pmax(a, b), tkey)
  type_of <- function(a, b) pmin(ac[a], ac[b]) * K + pmax(ac[a], ac[b])
  tk <- type_of(ties[,1], ties[,2])
  types <- expand.grid(c2 = seq_len(K) - 1, c1 = seq_len(K) - 1)
  types <- sort(with(types[types$c1 <= types$c2, ], c1 * K + c2))
  nt <- length(types)
  ntype <- tabulate(match(tk, types), nbins = nt)
  # for each tie, the ties of each type that share no node with it and are
  # not tied to it: all ties of that type, less itself, less those sharing
  # a node (from the connected triads), less those tied to it (from the
  # connected tetrads)
  h4e <- matrix(rep(ntype, each = m), m, nt)
  h4e[cbind(seq_len(m), match(tk, types))] <- 
    h4e[cbind(seq_len(m), match(tk, types))] - 1
  pairs <- NULL
  for(p in list(c(1, 2, 1, 3), c(1, 2, 2, 3), c(1, 3, 2, 3))){
    ok <- tied(conn[,p[1]], conn[,p[2]]) & tied(conn[,p[3]], conn[,p[4]])
    pairs <- rbind(pairs, cbind(tie_of(conn[ok,p[1]], conn[ok,p[2]]),
                                tie_of(conn[ok,p[3]], conn[ok,p[4]])))
  }
  for(p in list(c(1, 2, 3, 4), c(1, 3, 2, 4), c(1, 4, 2, 3))){
    ok <- tied(quad[,p[1]], quad[,p[2]]) & tied(quad[,p[3]], quad[,p[4]])
    pairs <- rbind(pairs, cbind(tie_of(quad[ok,p[1]], quad[ok,p[2]]),
                                tie_of(quad[ok,p[3]], quad[ok,p[4]])))
  }
  if(!is.null(pairs) && nrow(pairs)){
    both <- rbind(pairs, pairs[, 2:1, drop = FALSE])
    less <- tabulate(both[,1] + m * (match(tk[both[,2]], types) - 1),
                     nbins = m * nt)
    h4e <- h4e - matrix(less, m, nt)
  }
  # for each tie, the pairs of nodes of each type that are tied neither to
  # it nor to each other: all pairs of its isolates, less those tied (H4)
  iso2 <- .census_one_tie_isolates(st, ac, K, ties)
  i4e <- vapply(types, function(t){
    a <- iso2[, t %/% K + 1]
    b <- iso2[, t %% K + 1]
    if(t %/% K == t %% K) choose(a, 2) else a * b
  }, FUN.VALUE = numeric(m))
  i4e <- matrix(i4e, m, nt) - h4e
  # codes for two ties of the given types, or one tie and two isolates
  tt <- expand.grid(e = seq_len(nt), f = seq_len(nt)) # so codes are [e, f]
  ecat <- cbind(types[tt$e] %/% K, types[tt$e] %% K)
  fcat <- cbind(types[tt$f] %/% K, types[tt$f] %% K)
  hcode <- matrix(.census_standin(matrix(c(1, 0, 0, 0, 0, 1), nrow(tt), 6, byrow = TRUE),
                                  cbind(ecat, fcat), attr), nt, nt)
  icode <- matrix(.census_standin(matrix(c(1, 0, 0, 0, 0, 0), nrow(tt), 6, byrow = TRUE),
                                  cbind(ecat, fcat), attr), nt, nt)
  list(n = n, K = K, ac = ac, sizes = sizes, quad = quad, qcode = qcode,
       conn = conn, iso3 = iso3, tcode3 = tcode3, qtri = qtri,
       ties = ties, tk = match(tk, types), types = types, ntype = ntype,
       tie_of = tie_of, h4e = h4e, i4e = i4e, iso2 = iso2,
       hcode = hcode, icode = icode)
}

# The network tetrad census, from its parts.
.census_tetrads <- function(st, attr){
  if(nrow(st$states) < 4)
    return(.census_order(numeric(0), "tetrad", FALSE, attr, st))
  if(.census_tetrad_full(st, attr) || .census_dense(st$states, 4)){
    sets <- t(utils::combn(nrow(st$states), 4))
    if(!is.null(attr)) return(.census_net(sets, "tetrad", FALSE, attr, st))
    return(.census_order(c(table(.census_tetrad_plain(st$states != 0, sets))),
                         "tetrad", FALSE, attr, st))
  }
  p <- .census_tetrad_parts(st, attr)
  m <- nrow(p$ties)
  nt <- length(p$types)
  ecol <- rep(p$tk, nt)
  fcol <- rep(seq_len(nt), each = m)
  codes <- c(p$qcode, c(p$tcode3), p$hcode[cbind(ecol, fcol)],
             p$icode[cbind(ecol, fcol)])
  # each H4 is seen from both of its ties
  wts <- c(rep(1, length(p$qcode)), c(p$iso3), c(p$h4e) / 2, c(p$i4e))
  tot <- .census_null_tetrads(p$sizes, attr)
  seen <- .census_sum(wts, .census_nullcode(codes, FALSE, attr, 2, k = 4))
  empty <- tot$totals - c(seen[as.character(tot$codes)])
  empty[is.na(empty)] <- tot$totals[is.na(empty)]
  counts <- .census_sum(c(wts, empty), c(codes, tot$codes))
  names(counts) <- .census_label(as.numeric(names(counts)), "tetrad",
                                 FALSE, attr, st)
  .census_order(c(counts), "tetrad", FALSE, attr, st)
}

# Each node's tetrad census, from the same parts. A node counts the
# connected tetrads it is in, and the disconnected tetrads in which it is
# part of a component, as the network census does. It also counts those in
# which it is an isolate: all the parts of each kind, less those it is in or
# tied to.
.census_node_tetrads <- function(st, attr){
  n <- nrow(st$states)
  if(n >= 4 && (.census_tetrad_full(st, attr) || .census_dense(st$states, 4))){
    sets <- t(utils::combn(n, 4))
    if(!is.null(attr)) return(.census_node(sets, n, "tetrad", FALSE, attr, st))
    cls <- factor(rep(.census_tetrad_plain(st$states != 0, sets), 4),
                  levels = .tetrad_names)
    out <- unclass(table(factor(c(sets), levels = seq_len(n)), cls))
    return(matrix(out, n, dimnames = list(NULL, .tetrad_names)))
  }
  p <- .census_tetrad_parts(st, attr)
  ac <- p$ac
  K <- p$K
  ties <- p$ties
  m <- nrow(ties)
  nt <- length(p$types)
  nc <- nrow(p$conn)
  node <- code <- wt <- NULL
  add <- function(nd, cd, w){
    node <<- c(node, nd)
    code <<- c(code, cd)
    wt <<- c(wt, rep_len(w, length(nd)))
  }
  # (a) connected tetrads, for each of their nodes
  add(c(p$quad), rep(p$qcode, 4), 1)
  # (b) a connected triad and an isolate: as one of the triad
  for(j in 1:3) add(rep(p$conn[,j], K), c(p$tcode3), c(p$iso3))
  # ... and as the isolate: every triad, less those the node is in or near
  for(g in seq_len(K) - 1){
    who <- which(ac == g)
    if(!length(who) || !nc) next
    per <- table(p$tcode3[, g + 1])
    add(rep(who, each = length(per)), rep(as.numeric(names(per)), length(who)),
        rep(c(per), length(who)))
  }
  for(j in 1:3) add(p$conn[,j], p$tcode3[cbind(seq_len(nc), ac[p$conn[,j]] + 1)], -1)
  add(p$qtri[,2], p$tcode3[cbind(p$qtri[,1], ac[p$qtri[,2]] + 1)], -1)
  # (c, d) two ties, or one tie and two isolates: as an end of the tie
  ecol <- rep(p$tk, nt)
  fcol <- rep(seq_len(nt), each = m)
  for(j in 1:2){
    add(rep(ties[,j], nt), p$hcode[cbind(ecol, fcol)], c(p$h4e))
    add(rep(ties[,j], nt), p$icode[cbind(ecol, fcol)], c(p$i4e))
  }
  # (d) one tie and two isolates, as one of the isolates: for each tie it is
  # isolated from, the tie's other isolates, less itself, less those tied to
  # it (which make an H4 with the tie instead)
  ktype <- function(g, c) pmin(g, c) * K + pmax(g, c)
  tcol <- match(outer(seq_len(K) - 1, seq_len(K) - 1, ktype), p$types)
  tcol <- matrix(tcol, K, K) # the type of a pair of categories
  # ties each node is in, or near (from the connected triads)
  inc <- rbind(cbind(e = seq_len(m), k = ties[,1]), cbind(e = seq_len(m), k = ties[,2]))
  nearby <- NULL
  for(o in list(c(1, 2, 3), c(1, 3, 2), c(2, 3, 1))){
    ok <- st$states[cbind(p$conn[,o[1]], p$conn[,o[2]])] != 0
    nearby <- rbind(nearby, cbind(e = p$tie_of(p$conn[ok,o[1]], p$conn[ok,o[2]]),
                                  k = p$conn[ok,o[3]]))
  }
  notiso <- rbind(inc, nearby)
  for(c in if(m) seq_len(K) - 1 else integer(0)){
    # all ties, for every node; then less the ties the node is in or near
    w_all <- p$iso2[, c + 1]
    for(g in seq_len(K) - 1){
      who <- which(ac == g)
      if(!length(who)) next
      cd <- p$icode[cbind(p$tk, tcol[g + 1, c + 1])]
      self <- if(g == c) 1 else 0
      per <- .census_sum(w_all - self, cd)
      add(rep(who, each = length(per)), rep(as.numeric(names(per)), length(who)),
          rep(c(per), length(who)))
    }
    e <- notiso[,1]
    k <- notiso[,2]
    self <- as.numeric(ac[k] == c)
    add(k, p$icode[cbind(p$tk[e], tcol[cbind(ac[k] + 1, c + 1)])],
        -(p$iso2[cbind(e, c + 1)] - self))
  }
  # less, for each tie (k, y) at the node, the ties isolated from both,
  # which are H4s rather than I4s
  for(j in if(m) 1:2 else integer(0)){
    k <- ties[,j]
    y <- ties[,3 - j]
    for(f in seq_len(nt)){
      add(k, p$icode[cbind(rep(f, m), tcol[cbind(ac[k] + 1, ac[y] + 1)])],
          -p$h4e[, f])
    }
  }
  ucode <- sort(unique(code))
  out <- matrix(0, n, length(ucode))
  agg <- .census_sum(wt, (match(code, ucode) - 1) * n + node)
  out[as.numeric(names(agg))] <- agg
  # (e) the empty tetrads: all those holding the node, less the others
  nulls <- .census_nullcode(ucode, FALSE, attr, 2, k = 4)
  empties <- lapply(seq_len(K) - 1, function(g)
    .census_null_tetrads(p$sizes, attr, given = g))
  ecode <- sort(unique(unlist(lapply(empties, `[[`, "codes"))))
  emat <- matrix(0, n, length(ecode))
  for(g in seq_len(K)){
    who <- ac == g - 1
    if(!any(who)) next
    cd <- empties[[g]]$codes
    u <- unique(cd)
    e <- vapply(u, function(c) sum(empties[[g]]$totals[cd == c]), numeric(1))
    emat[who, match(u, ecode)] <- rep(e, each = sum(who))
  }
  held <- t(rowsum(t(out), nulls))
  hc <- match(as.numeric(colnames(held)), ecode)
  emat[, hc] <- emat[, hc] - held
  res <- t(rowsum(t(cbind(out, emat)), c(ucode, ecode)))
  labs <- .census_label(as.numeric(colnames(res)), "tetrad", FALSE, attr, st)
  colnames(res) <- labs
  totals <- .census_order(colSums(res) / 4, "tetrad", FALSE, attr, st)
  final <- matrix(0, n, length(totals), dimnames = list(NULL, names(totals)))
  shared <- intersect(names(totals), labs)
  final[, shared] <- res[, shared]
  final
}

# For each tie, how many nodes of each category are tied to neither end.
.census_one_tie_isolates <- function(st, ac, K, ties){
  sub <- list(states = st$states, base = 2, layers = NULL, signed = FALSE)
  attr <- if(K > 1) list(codes = ac, groups = K, ordered = FALSE) else NULL
  parts <- .census_triad_parts(sub, FALSE, attr)
  parts$iso[match((ties[,1] - 1) * nrow(st$states) + ties[,2], parts$tkey), ,
            drop = FALSE]
}

# The number of empty tetrads of each kind of group, with their codes.
# With `given`, the tetrads are those holding a node of that group,
# counted over the other nodes.
.census_null_tetrads <- function(sizes, attr, given = NULL){
  .census_null_coded(sizes, 4, FALSE, attr, 2, given)
}

# The number of empty motifs of k nodes of each kind of group, with their
# codes: each kind is a list of groups standing for its relations.
.census_null_coded <- function(sizes, k, directed, attr, base, given = NULL){
  if(is.null(attr)){
    n <- sum(sizes)
    total <- if(is.null(given)) choose(n, k) else choose(n - 1, k - 1)
    return(list(codes = 0, totals = total))
  }
  nc <- if(attr$ordered) .census_null_ordered(sizes, k, given) else
    .census_null_categorical(sizes, k, given)
  sat <- attr
  sat$codes <- unlist(nc$reps)
  codes <- .census_code(NULL, matrix(seq_along(sat$codes), ncol = k, byrow = TRUE),
                        directed, sat, base)
  list(codes = codes, totals = nc$totals)
}

# Empty motifs by which of their positions share a category.
.census_null_categorical <- function(sizes, k, given = NULL){
  esym <- function(x, r){ # elementary symmetric polynomial of degree r
    e <- c(1, rep(0, r))
    for(s in x) e[2:(r + 1)] <- e[2:(r + 1)] + s * e[1:r]
    e[r + 1]
  }
  if(!is.null(given)) sizes[given + 1] <- sizes[given + 1] - 1
  n <- sum(sizes)
  c2 <- choose(sizes, 2)
  if(k == 3 && is.null(given)){
    aaa <- sum(choose(sizes, 3))
    aab <- sum(c2 * (n - sizes))
    return(list(reps = list(c(0,0,0), c(0,0,1), c(0,1,2)),
                totals = c(aaa, aab, choose(n, 3) - aaa - aab)))
  }
  if(k == 3){
    own <- sizes[given + 1]
    rest <- sizes[-(given + 1)]
    return(list(reps = list(c(0,0,0), c(0,0,1), c(0,1,1), c(0,1,2)),
                totals = c(choose(own, 2), own * sum(rest), sum(choose(rest, 2)),
                           esym(rest, 2))))
  }
  if(is.null(given))
    return(list(reps = list(c(0,0,0,0), c(0,0,0,1), c(0,0,1,1), c(0,0,1,2),
                            c(0,1,2,3)),
                totals = c(sum(choose(sizes, 4)),
                           sum(choose(sizes, 3) * (n - sizes)),
                           (sum(c2)^2 - sum(c2^2)) / 2,
                           sum(vapply(seq_along(sizes), function(i)
                             c2[i] * esym(sizes[-i], 2), FUN.VALUE = numeric(1))),
                           esym(sizes, 4))))
  o <- sizes[given + 1]
  rest <- sizes[-(given + 1)]
  list(reps = list(c(0,0,0,0), c(0,0,0,1), c(0,1,1,1), c(0,0,1,1),
                   c(0,0,1,2), c(0,1,1,2), c(0,1,2,3)),
       totals = c(choose(o, 3), choose(o, 2) * sum(rest), sum(choose(rest, 3)),
                  o * sum(choose(rest, 2)), o * esym(rest, 2),
                  sum(choose(rest, 2) * (sum(rest) - rest)), esym(rest, 3)))
}

# Empty motifs by the order of their positions' values. Each kind is a
# composition of k, e.g. (2, 1) for two equal values below a third, and is
# counted over the distinct values in order.
.census_null_ordered <- function(sizes, k, given = NULL){
  comps <- .census_compositions(k)
  fill <- function(s, comp){ # ways to fill groups of these sizes, in order
    if(!length(comp) || !length(s)) return(as.numeric(!length(comp)))
    a <- choose(s, comp[1])
    for(j in seq_along(comp)[-1]) a <- choose(s, comp[j]) * c(0, cumsum(a))[seq_along(s)]
    sum(a)
  }
  L <- length(sizes)
  totals <- vapply(comps, function(cc){
    if(is.null(given)) return(fill(sizes, cc))
    sum(vapply(seq_along(cc), function(j){
      fill(sizes[seq_len(given)], cc[seq_len(j - 1)]) *
        choose(sizes[given + 1] - 1, cc[j] - 1) *
        fill(sizes[seq_len(L) > given + 1], cc[seq_along(cc) > j])
    }, FUN.VALUE = numeric(1)))
  }, FUN.VALUE = numeric(1))
  list(reps = lapply(comps, function(cc) rep(seq_along(cc) - 1, cc)),
       totals = totals)
}

# Every composition of k: the ordered ways of splitting it into parts.
.census_compositions <- function(k){
  if(k == 0) return(list(integer(0)))
  unlist(lapply(seq_len(k), function(first)
    lapply(.census_compositions(k - first), function(rest) c(first, rest))),
    recursive = FALSE)
}

# The triad census enumerates only the connected triads, those with at least
# two ties, which are the pairs of each node's neighbours. The triads holding
# one tie, and the empty triads, are far more numerous in a sparse network,
# and are counted instead, for each tie and each category of the third node,
# from what the connected triads say about the tie's neighbourhood
# (an analytic approach, as in Ribeiro et al. 2022, section 3.2).
# The result is a set of weighted parts, each with a canonical code, from
# which both the network census and each node's census are tabulated.
.census_triad_parts <- function(st, directed, attr){
  n <- nrow(st$states)
  K <- if(is.null(attr)) 1L else attr$groups
  acodes <- if(is.null(attr)) integer(n) else attr$codes
  sizes <- tabulate(acodes + 1L, nbins = K)
  und <- st$states != 0
  und <- und | t(und)
  diag(und) <- FALSE
  conn <- .census_connected_triads(und)
  ccode <- .census_code(st$states, conn, directed, attr, st$base)
  # the ties, and for each, how many nodes of each category are tied to
  # neither of its ends: those make a one-tie triad with it
  ties <- which(und & upper.tri(und), arr.ind = TRUE)
  m <- nrow(ties)
  tkey <- (ties[,1] - 1) * n + ties[,2]
  opp <- list(c(1, 2, 3), c(1, 3, 2), c(2, 3, 1)) # a pair, and its third
  near <- numeric(m * K)
  for(o in opp){
    tied <- und[cbind(conn[,o[1]], conn[,o[2]])]
    t <- match((conn[tied,o[1]] - 1) * n + conn[tied,o[2]], tkey)
    near <- near + tabulate(t + m * acodes[conn[tied,o[3]]], nbins = m * K)
  }
  iso <- matrix(rep(sizes, each = m), m, K) - matrix(near, m, K)
  iso[cbind(seq_len(m), acodes[ties[,1]] + 1)] <- 
    iso[cbind(seq_len(m), acodes[ties[,1]] + 1)] - 1
  iso[cbind(seq_len(m), acodes[ties[,2]] + 1)] <- 
    iso[cbind(seq_len(m), acodes[ties[,2]] + 1)] - 1
  # the code of a one-tie triad depends only on the tie's states and on how
  # the third node's group compares with its ends', so each tie is coded once
  # for each way of comparing, on small stand-in triads
  tcode <- .census_one_tie_codes(st, ties, directed, attr, acodes, K)
  # the empty triads, from the category sizes, less those seen to hold a tie
  nullof <- function(codes) .census_nullcode(codes, directed, attr, st$base)
  seen <- .census_sum(c(rep(1, nrow(conn)), c(iso)),
                 c(nullof(ccode), nullof(c(tcode))))
  tot <- .census_null_totals(sizes, NULL, directed, attr, st$base)
  empty <- tot$totals - c(seen[as.character(tot$codes)])
  empty[is.na(empty)] <- tot$totals[is.na(empty)]
  list(n = n, acodes = acodes, K = K, conn = conn, ccode = ccode,
       ties = ties, tkey = tkey, tcode = tcode, iso = iso,
       empty = stats::setNames(empty, tot$codes))
}

# The code of the triad that each tie makes with an untied third node of
# each group, as a matrix with one row per tie and one column per group.
.census_one_tie_codes <- function(st, ties, directed, attr, acodes, K){
  m <- nrow(ties)
  pairs <- .census_pairs(3, directed)
  digits <- matrix(0, m, nrow(pairs))
  digits[, which(pairs[,1] == 1 & pairs[,2] == 2)] <- st$states[ties]
  if(directed)
    digits[, which(pairs[,1] == 2 & pairs[,2] == 1)] <- 
      st$states[ties[, 2:1, drop = FALSE]]
  if(is.null(attr) || !m)
    return(matrix(.census_standin(digits, matrix(0, m, 3), attr, directed,
                                  st$base), m, K))
  ends <- cbind(acodes[ties[,1]], acodes[ties[,2]])
  rc <- .census_relclass(ends, attr$ordered)
  nk <- rc$n
  third <- unlist(lapply(rc$classes, function(cl) rc$standin(rep(cl, m))))
  codes <- matrix(.census_standin(
    digits[rep(seq_len(m), nk), , drop = FALSE],
    cbind(ends[rep(seq_len(m), nk), , drop = FALSE], third),
    attr, directed, st$base), m, nk)
  matrix(vapply(seq_len(K) - 1L, function(g){
    codes[cbind(seq_len(m), rc$classify(g) + 1)]
  }, FUN.VALUE = numeric(m)), m, K)
}

# The canonical code of the empty motif of k nodes with the same categories
# as each canonical code given: the code's category digits, coded again
# without ties.
.census_nullcode <- function(codes, directed, attr, base, k = 3){
  if(is.null(attr)) return(numeric(length(codes)))
  abase <- .census_abase(attr, k)
  u <- unique(codes)
  digits <- matrix(vapply((k-1):0, function(p) u %/% abase^p %% abase,
                          FUN.VALUE = numeric(length(u))), ncol = k)
  sat <- attr
  sat$codes <- c(t(.census_adig_values(digits, attr, k)))
  out <- .census_code(NULL, matrix(seq_len(k * length(u)), ncol = k, byrow = TRUE),
                      directed, sat, base)
  out[match(codes, u)]
}

# The number of empty triads of each kind of group, with their codes.
# With `given`, the triads are those holding a node of that group,
# counted over the other nodes.
.census_null_totals <- function(sizes, given, directed, attr, base){
  .census_null_coded(sizes, 3, directed, attr, base, given)
}

# The network triad census, from its parts.
.census_triads <- function(st, directed, attr){
  if(nrow(st$states) < 3)
    return(.census_order(numeric(0), "triad", directed, attr, st))
  if(.census_dense(st$states, 3))
    return(.census_net(t(utils::combn(nrow(st$states), 3)), "triad",
                       directed, attr, st))
  parts <- .census_triad_parts(st, directed, attr)
  counts <- .census_sum(c(rep(1, length(parts$ccode)), c(parts$iso), parts$empty),
                   c(parts$ccode, c(parts$tcode), as.numeric(names(parts$empty))))
  names(counts) <- .census_label(as.numeric(names(counts)), "triad",
                                 directed, attr, st)
  .census_order(c(counts), "triad", directed, attr, st)
}

# Each node's triad census, from the same parts: a node counts the connected
# triads it is in, the one-tie triads at either end of its ties, and those
# in which it is the untied third node, which are all the ties of each kind
# less those it is tied to or near. Its empty triads follow as before.
.census_node_triads <- function(st, directed, attr){
  n <- nrow(st$states)
  if(n >= 3 && .census_dense(st$states, 3))
    return(.census_node(t(utils::combn(n, 3)), n, "triad", directed, attr, st))
  parts <- .census_triad_parts(st, directed, attr)
  ac <- parts$acodes
  ties <- parts$ties
  m <- nrow(ties)
  tcode <- parts$tcode
  node <- c(c(parts$conn), rep(ties[,1], parts$K), rep(ties[,2], parts$K),
            ties[,1], ties[,2])
  code <- c(rep(parts$ccode, 3), c(tcode), c(tcode),
            tcode[cbind(seq_len(m), ac[ties[,1]] + 1)],
            tcode[cbind(seq_len(m), ac[ties[,2]] + 1)])
  wt <- c(rep(1, length(parts$conn)), c(parts$iso), c(parts$iso),
          rep(-1, 2 * m))
  # less, for each node, the ties that make a connected triad with it
  for(o in list(c(1, 2, 3), c(1, 3, 2), c(2, 3, 1))){
    conn <- parts$conn
    tied <- .census_tied(st$states, conn[,o[1]], conn[,o[2]])
    t <- match((conn[tied,o[1]] - 1) * n + conn[tied,o[2]], parts$tkey)
    third <- conn[tied,o[3]]
    node <- c(node, third)
    code <- c(code, tcode[cbind(t, ac[third] + 1)])
    wt <- c(wt, rep(-1, length(t)))
  }
  ucode <- sort(unique(code))
  out <- matrix(0, n, length(ucode))
  idx <- cbind(node, match(code, ucode))
  agg <- .census_sum(wt, (idx[,2] - 1) * n + idx[,1])
  out[as.numeric(names(agg))] <- agg
  # plus every tie of each kind, as the untied third node
  for(g in seq_len(parts$K)){
    who <- ac == g - 1
    if(!any(who)) next
    per <- tabulate(match(tcode[,g], ucode), nbins = length(ucode))
    out[who,] <- sweep(out[who, , drop = FALSE], 2, per, "+")
  }
  # the empty triads: all those holding the node, less those holding a tie
  nulls <- .census_nullcode(ucode, directed, attr, st$base)
  sizes <- tabulate(ac + 1L, nbins = parts$K)
  empties <- lapply(seq_len(parts$K) - 1, function(g)
    .census_null_totals(sizes, g, directed, attr, st$base))
  ecode <- sort(unique(unlist(lapply(empties, `[[`, "codes"))))
  emat <- matrix(0, n, length(ecode))
  for(g in seq_len(parts$K)){
    who <- ac == g - 1
    if(!any(who)) next
    cd <- empties[[g]]$codes
    u <- unique(cd)
    e <- vapply(u, function(c) sum(empties[[g]]$totals[cd == c]), numeric(1))
    emat[who, match(u, ecode)] <- rep(e, each = sum(who))
  }
  held <- t(rowsum(t(out), nulls))
  emat[, match(as.numeric(colnames(held)), ecode)] <- 
    emat[, match(as.numeric(colnames(held)), ecode)] - held
  all <- c(ucode, ecode)
  res <- cbind(out, emat)
  res <- t(rowsum(t(res), all))
  labs <- .census_label(as.numeric(colnames(res)), "triad", directed, attr, st)
  colnames(res) <- labs
  totals <- .census_order(colSums(res) / 3, "triad", directed, attr, st)
  final <- matrix(0, n, length(totals), dimnames = list(NULL, names(totals)))
  shared <- intersect(names(totals), labs)
  final[, shared] <- res[, shared]
  final
}

# Whether each pair of nodes is tied, in either direction.
.census_tied <- function(states, a, b){
  states[cbind(a, b)] != 0 | states[cbind(b, a)] != 0
}

# Exposure ####

#' Motifs of node exposure
#' @name motif_exposure
#' @description
#'   `node_x_exposure()` produces a motif matrix of nodes' exposure to 
#'   infection/adoption by time step.
#' 
#' @template param_data
#' @family diffusion
#' @template node_motif
NULL

#' @rdname motif_exposure
#' @examples
#' node_x_exposure(play_diffusion(create_tree(12)))
#' @export
node_x_exposure <- function(.data){
  if(inherits(.data, "diff_model")){
    diff_model <- manynet::as_tidygraph(.data)
    times <- diff_model$t
    out <- sapply(times, function(x){
      inf <- node_is_infected(diff_model, time = x)
      if(sum(inf)==1) manynet::as_matrix(.data)[inf,] else
        colSums(manynet::as_matrix(.data)[inf,])
    })
  } else {
    times <- manynet::as_diffusion(.data)$time
    out <- sapply(times, function(x){
      inf <- node_is_infected(.data, time = x)
      if(sum(inf)==1) manynet::as_matrix(.data)[inf,] else
        colSums(manynet::as_matrix(.data)[inf,])
    })
  }
  colnames(out) <- paste0("t",times)
  make_node_motif(out, .data)
}

# Hazard ####

#' Motifs of network hazard
#' @name motif_hazard
#' @description
#'   `net_x_hazard()` measures the hazard rate or instantaneous probability that
#'   nodes will adopt/become infected at that time.
#' 
#' @template param_data
#' @family diffusion
#' @template net_motif
NULL

#' @rdname motif_hazard
#' @section Hazard rate: 
#' The hazard rate is the instantaneous probability of adoption/infection 
#' at each time point (Allison 1984).
#' In survival analysis, hazard rate is formally defined as:
#'
#' \deqn{%
#' \lambda(t)=\lim_{h\to +0}\frac{F(t+h)-F(t)}{h}\frac{1}{1-F(t)} %
#' }{%
#' \lambda(t-1)= lim (t -> +0) [F(t+h)-F(t)]/h * 1/[1-F(t)] %
#' }
#'
#' By approximating \eqn{h=1}, we can rewrite the equation as
#'
#' \deqn{%
#' \lambda(t)=\frac{F(t+1)-F(t)}{1-F(t)} %
#' }{%
#' \lambda(t-1)= [F(t+1)-F(t)]/[1-F(t)] %
#' }
#'
#' If we estimate \eqn{F(t)}, 
#' the probability of not having adopted the innovation in time \eqn{t}, 
#' from the proportion of adopters in that time, 
#' such that \eqn{F(t) \sim q_t/n}{F(t) ~ q(t)/n}, we now have (ultimately for \eqn{t>1}):
#'
#' \deqn{%
#' \lambda(t)=\frac{q_{t+1}/n-q_t/n}{1-q_t/n} = \frac{q_{t+1} - q_t}{n - q_t} = \frac{q_t - q_{t-1}}{n - q_{t-1}} %
#' }{%
#' \lambda(t-1)= [q(t+1)/n-q(t)/n]/[1-q(t)/n] = [q(t+1) - q(t)]/[n - q(t)] = [q(t) - q(t-1)]/[n - q(t-1)] %
#' }
#' 
#' where \eqn{q_i}{q(i)} is the number of adopters in time \eqn{t}, 
#' and \eqn{n} is the number of vertices in the graph.
#'
#' The shape of the hazard rate indicates the pattern of new adopters over time.
#' Rapid diffusion with convex cumulative adoption curves will have 
#' hazard functions that peak early and decay over time. 
#' Slow concave cumulative adoption curves will have 
#' hazard functions that are low early and rise over time.
#' Smooth hazard curves indicate constant adoption whereas 
#' those that oscillate indicate variability in adoption behavior over time.
#' @source `{netdiffuseR}`
#' @references
#' ## On hazard rates
#' Allison, Paul D. 1984. 
#' _Event history analysis: Regression for longitudinal event data_. 
#' London: Sage Publications.
#' \doi{10.4135/9781412984195}
#'
#' Wooldridge, Jeffrey M. 2010. 
#' _Econometric Analysis of Cross Section and Panel Data_ (2nd ed.). 
#' Cambridge: MIT Press.
#' @examples
#' # To calculate the hazard rates at each time point
#'   smeg <- generate_smallworld(15, 0.025)
#' net_x_hazard(play_diffusion(smeg, transmissibility = 0.3))
#' @export
net_x_hazard <- function(.data){
  diff_model <- manynet::as_diffusion(.data)
  out <- (diff_model$I - dplyr::lag(diff_model$I)) / 
    (diff_model$n - dplyr::lag(diff_model$I))
  if(inherits(.data, "diff_model")) 
    net <- attr(.data, "network") else 
      net <- .data
  names(out) <- paste0("t", diff_model$t)
  make_network_motif(out, net)
}


