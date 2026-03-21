# Node path ####

#' Motifs of nodes pathing
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
#'   
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
  object <- manynet::as_igraph(.data)
  # edge_names <- net_tie_attributes(object)
  if (manynet::is_directed(object)) {
    if (manynet::is_multiplex(.data)) {
      mat <- do.call(rbind, lapply(unique(manynet::tie_attribute(object, "type")), 
                                   function(x){
                                     rc <- manynet::as_matrix(manynet::to_uniplex(object, x))
                                     rbind(rc, t(rc))
                                   }))
    } else if (manynet::is_longitudinal(object)){
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
      mat <- do.call(rbind, lapply(unique(manynet::tie_attribute(object, "type")), 
                                   function(x){
                                     manynet::as_matrix(manynet::to_uniplex(object, x))
                                   }))
    } else if (manynet::is_longitudinal(object)){
      mat <- do.call(rbind, lapply(unique(manynet::tie_attribute(object, "wave")), 
                                   function(x){
                                     manynet::as_matrix(manynet::to_waves(object)[[x]])
                                   }))
    } else if (manynet::is_twomode(.data)) {
      mat <- manynet::as_matrix(manynet::to_multilevel(object))
    } else {
      mat <- manynet::as_matrix(object)
    }
  }
  if(manynet::is_labelled(object) & manynet::is_directed(object))
    if(manynet::is_multiplex(.data)){
      rownames(mat) <- apply(expand.grid(c(paste0("from", manynet::node_names(object)),
                                           paste0("to", manynet::node_names(object))),
                                         unique(manynet::tie_attribute(object, "type"))), 
                             1, paste, collapse = "_")
    } else if (manynet::is_longitudinal(object)){
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
  if(manynet::is_weighted(.data)){
    tore <- manynet::as_matrix(.data)/mean(manynet::as_matrix(.data))
    out <- 1/tore
  } else out <- igraph::distances(manynet::as_igraph(.data))
  diag(out) <- 0
  make_node_motif(out, .data)
}

# Node cohesion ####

#' Motifs of nodes cohesion
#' @name motif_node
#' @description
#'   These functions include ways to take a census of the positions of nodes
#'   in a network: 
#'   
#'   - `node_x_tie()` returns a census of the ties in a network.
#'   For directed networks, out-ties and in-ties are bound together.
#'   For multiplex networks, the various types of ties are bound together.
#'   - `node_x_triad()` returns a census of the triad configurations
#'   nodes are embedded in.
#'   - `node_x_tetrad()` returns a census of nodes' positions
#'   in motifs of four nodes.
#'   - `node_x_path()` returns the shortest path lengths
#'   of each node to every other node in the network.
#'   
#' @template param_data
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
#' @export
node_x_dyad <- function(.data) {
  .data <- manynet::expect_nodes(.data)
  if(is_weighted(.data)){
    .data <- manynet::to_unweighted(.data)
    manynet::snet_info("Ignoring tie weights.")
  }
  mat <- manynet::as_matrix(.data)
  out <- t(vapply(seq_nodes(.data), function(x){
    vec <- mat[x,] + mat[,x]
    c(sum(vec==2), sum(vec==1), sum(vec==0))
  }, FUN.VALUE = numeric(3)))
  colnames(out) <- c("Mutual", "Asymmetric", "Null")
  if (!manynet::is_directed(.data)) out <- out[,c(1, 3)]
  make_node_motif(out, .data)
}

#' @rdname motif_node 
#' @references 
#' ## On the triad census
#' Davis, James A., and Samuel Leinhardt. 1967. 
#' “\href{https://files.eric.ed.gov/fulltext/ED024086.pdf}{The Structure of Positive Interpersonal Relations in Small Groups}.” 55.
#' @examples 
#' task_eg <- to_named(to_uniplex(ison_algebra, "tasks"))
#' (triad_cen <- node_x_triad(task_eg))
#' @export
node_x_triad <- function(.data){
  .data <- manynet::expect_nodes(.data)
  out <- t(sapply(seq.int(manynet::net_nodes(.data)), 
                  function(x) net_x_triad(.data) - net_x_triad(manynet::delete_nodes(.data, x))))
  make_node_motif(out, .data)
}

# #' @rdname motif_node
# #' @section Quad census:
# #'   The quad census uses the `{oaqc}` package to do
# #'   the heavy lifting of counting the number of each orbits.
# #'   See `vignette('oaqc')`.
# #'   However, our function relabels some of the motifs
# #'   to avoid conflicts and improve some consistency with
# #'   other census-labelling practices.
# #'   The letter-number pairing of these labels indicate
# #'   the number and configuration of ties.
# #'   For now, we offer a rough translation:
# #' 
# #' | migraph | Ortmann and Brandes      
# #' | ------------- |------------- |
# #' | E4  | co-K4
# #' | I40, I41  | co-diamond
# #' | H4  | co-C4
# #' | L42, L41, L40 | co-paw
# #' | D42, D40 | co-claw
# #' | U42, U41 | P4
# #' | Y43, Y41 | claw
# #' | P43, P42, P41 | paw
# #' | 04 | C4
# #' | Z42, Z43 | diamond
# #' | X4 | K4
# #' 
# #' See also [this list of graph classes](https://www.graphclasses.org/smallgraphs.html#nodes4).

#' @rdname motif_node
#' @section Tetrad census:
#'   The nodal tetrad census counts the number of four-node configurations
#'   that each node is embedded in.
#'   The function returns a matrix with a special naming convention:
#'   - E4 (aka co-K4): This is an empty set of four nodes; no ties
#'   - I4 (aka co-diamond): This is a set of four nodes with just one tie
#'   - H4 (aka co-C4): This set of four nodes includes two non-adjacent ties
#'   - L4 (aka co-paw): This set of four nodes includes two adjacent ties
#'   - D4 (aka co-claw): This set of four nodes includes three adjacent ties,
#'   in the form of a triangle with one isolate
#'   - U4 (aka P4, four-actor line): This set of four nodes includes three ties 
#'   arranged in a line
#'   - Y4 (aka claw): This set of four nodes includes three ties all adjacent
#'   to a single node
#'   - P4 (aka paw, kite): This set of four nodes includes four ties arranged
#'   as a triangle with an extra tie hanging off of one of the nodes
#'   - C4 (aka bifan): This is a symmetric box or 4-cycle or set of shared choices
#'   - Z4 (aka diamond): This resembles C4 but with an extra tie cutting across the box
#'   - X4 (aka K4): This resembles C4 but with two extra ties cutting across the box;
#'   a realisation of all possible ties
#'   
#'   Graphs of these motifs can be shown using 
#'   `plot(node_by_tetrad(ison_southern_women))`.
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
#' @export
node_x_tetrad <- function(.data){
  .data <- manynet::expect_nodes(.data)
  cmbs <- utils::combn(1:manynet::net_nodes(.data), 4)
  mat <- manynet::as_matrix(manynet::to_onemode(.data))
  dd <- apply(cmbs, 2, function(x) c(sum(mat[x,x]), 
                                     max(rowSums(mat[x,x]))))
  
  types <- rep(NA, ncol(cmbs))
  types[dd[1,] == 0] <- "E4"
  types[dd[1,] == 2] <- "I4"
  types[dd[1,] == 4 & dd[2,] == 1] <- "H4"
  types[dd[1,] == 4 & dd[2,] == 2] <- "L4"
  types[dd[1,] == 6 & dd[2,] == 2] <- "D4"
  types[dd[1,] == 6 & dd[2,] == 1] <- "U4"
  types[dd[1,] == 6 & dd[2,] == 3] <- "Y4"
  types[dd[1,] == 8 & dd[2,] == 3] <- "P4"
  types[dd[1,] == 8 & dd[2,] == 2] <- "C4"
  types[dd[1,] == 10] <- "Z4"
  types[dd[1,] == 12] <- "X4"
  
  appears <- sapply(seq.int(manynet::net_nodes(.data)), 
         function(x) types[which(cmbs == x, arr.ind = TRUE)[,2]])
  out <- apply(appears, 2, table)

  if(is.list(out)){
    out <- as.matrix(dplyr::bind_rows(out))
  } else out <- as.matrix(as.data.frame(t(out)))
  out.order <- c("E4","I4","H4","L4","D4","U4","Y4","P4","C4","Z4","X4")
  out <- out[,match(out.order, colnames(out))]
  colnames(out) <- out.order
  out[is.na(out)] <- 0

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
  
# #' @export
# node_igraph_census <- function(.data, normalized = FALSE){
#     out <- igraph::motifs(manynet::as_igraph(.data), 4)
#     if(manynet::is_labelled(.data))
#       rownames(out) <- manynet::node_names(.data)
#     colnames(out) <- c("co-K4",
#                        "co-diamond",
#                        "co-C4",
#                        "co-paw",
#                        "co-claw",
#                        "P4",
#                        "claw",
#                        "paw",
#                        "C4",
#                        "diamond",
#                        "K4")
#     make_node_motif(out, .data)
# }

# Network cohesion ####

#' Motifs of network cohesion
#' @name motif_net
#' @description
#'   These functions include ways to take a census of the graphlets
#'   in a network: 
#'   
#'   - `net_x_dyad()` returns a census of dyad motifs in a network.
#'   - `net_x_triad()` returns a census of triad motifs in a network.
#'   - `net_x_tetrad()` returns a census of tetrad motifs in a network.
#'   - `net_x_mixed()` returns a census of triad motifs that span
#'   a one-mode and a two-mode network.
#'   
#'   See also \href{https://www.graphclasses.org/smallgraphs.html}{graph classes}.
#'   
#' @template param_data
#' @family cohesion
#' @template net_motif
#' @param object2 A second, two-mode network object.
NULL

#' @rdname motif_net
#' @section Dyad census: 
#'   The dyad census counts the number of mutual, asymmetric, and null dyads 
#'   in a network.
#'   For directed networks, 
#'   - Mutual dyads have ties in both directions
#'   - Asymmetric dyads have a tie in one direction only
#'   - Null dyads have no ties
#'   
#'   Note that for undirected and two-mode networks,
#'   only mutual and null dyads are possible, 
#'   as the concept of an asymmetric dyad does not apply.
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
net_x_dyad <- function(.data) {
  .data <- manynet::expect_nodes(.data)
  out <- suppressWarnings(igraph::dyad_census(manynet::as_igraph(.data)))
  out <- unlist(out)
  names(out) <- c("Mutual", "Asymmetric", "Null")
  if (!manynet::is_directed(.data)) out <- out[c(1, 3)]
  make_network_motif(out, .data)
}

#' @rdname motif_net
#' @section Triad census:
#'  The triad census counts the number of three-node configurations in the network.
#'  The function returns a matrix with a special naming convention:
#'  - 003: This is an empty triad; no ties
#'  - 012: This triad includes one tie
#'  - 102: This triad includes two ties, but they are not reciprocated
#'  - 021D: This triad includes two ties, one of which is reciprocated, and the other is directed towards the reciprocated tie
#'  - 021U: This triad includes two ties, one of which is
#'  reciprocated, and the other is directed away from the reciprocated tie
#'  - 021C: This triad includes two ties, one of which is reciprocated, and the other is directed between the two non-reciprocated nodes
#'  - 111D: This triad includes three ties, two of which are
#'  reciprocated, and the other is directed towards the reciprocated ties
#'  - 111U: This triad includes three ties, two of which are
#'  reciprocated, and the other is directed away from the reciprocated ties
#'  - 030T: This triad includes three ties, all of which are
#'  directed in a transitive manner (i.e. A->B, B->C, A->C)
#'  - 030C: This triad includes three ties, all of which are
#'  directed in a cyclic manner (i.e. A->B, B->C
#'  A->C)
#'  - 201: This triad includes three ties, all of which are reciproc
#'  ated (i.e. A<->B, B<->C, A<->C)
#'  - 120D: This triad includes four ties, three of which are
#'  reciprocated, and the other is directed towards the reciprocated ties
#'  - 120U: This triad includes four ties, three of which are
#'  reciprocated, and the other is directed away from the reciprocated ties
#'  - 120C: This triad includes four ties, three of which are
#'  reciprocated, and the other is directed between the two non-reciprocated
#'  - 210: This triad includes five ties, four of which are reciprocated, and the other is directed between the two non-reciprocated
#'  - 300: This triad includes six ties, all of which are reciprocated
#'  
#'  Note that for undirected and two-mode networks, only 003, 102, and 201 are possible,
#'  as the other configurations rely on the concept of directionality.
#' @references 
#' ## On the triad census
#' Davis, James A., and Samuel Leinhardt. 1967. 
#' “\href{https://files.eric.ed.gov/fulltext/ED024086.pdf}{The Structure of Positive Interpersonal Relations in Small Groups}.” 55.
#' @examples 
#' net_x_triad(manynet::ison_adolescents)
#' @export
net_x_triad <- function(.data) {
  .data <- manynet::expect_nodes(.data)
  if (manynet::is_twomode(.data)) {
    manynet::snet_abort("A twomode or multilevel option for a triad census is not yet implemented.")
  } else {
    out <- suppressWarnings(igraph::triad_census(as_igraph(.data)))
    names(out) <- c("003", "012", "102", "021D",
                    "021U", "021C", "111D", "111U",
                    "030T", "030C", "201", "120D",
                    "120U", "120C", "210", "300")
    if (!manynet::is_directed(.data)) out <- out[c(1, 2, 3, 11, 15, 16)]
    make_network_motif(out, .data)
  }
}

#' @rdname motif_net
#' @section Tetrad census:
#'   The tetrad census counts the number of four-node configurations in the network.
#'   The function returns a matrix with a special naming convention:
#'   - E4 (aka co-K4): This is an empty set of four nodes; no ties
#'   - I4 (aka co-diamond): This is a set of four nodes with just one tie
#'   - H4 (aka co-C4): This set of four nodes includes two non-adjacent ties
#'   - L4 (aka co-paw): This set of four nodes includes two adjacent ties
#'   - D4 (aka co-claw): This set of four nodes includes three adjacent ties,
#'   in the form of a triangle with one isolate
#'   - U4 (aka P4, four-actor line): This set of four nodes includes three ties 
#'   arranged in a line
#'   - Y4 (aka claw): This set of four nodes includes three ties all adjacent
#'   to a single node
#'   - P4 (aka paw, kite): This set of four nodes includes four ties arranged
#'   as a triangle with an extra tie hanging off of one of the nodes
#'   - C4 (aka bifan): This is a symmetric box or 4-cycle or set of shared choices
#'   - Z4 (aka diamond): This resembles C4 but with an extra tie cutting across the box
#'   - X4 (aka K4): This resembles C4 but with two extra ties cutting across the box;
#'   a realisation of all possible ties
#'   
#'   Graphs of these motifs can be shown using 
#'   `plot(net_x_tetrad(ison_southern_women))`.
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
net_x_tetrad <- function(.data){
  .data <- manynet::expect_nodes(.data)
  cmbs <- utils::combn(1:manynet::net_nodes(.data), 4)
  mat <- manynet::as_matrix(manynet::to_onemode(.data))
  dens <- apply(cmbs, 2, function(x) sum(mat[x,x]))
  
  E4 <- sum(dens == 0)
  I4 <- sum(dens == 1)
  
  if(any(dens==2)){
    if(sum(dens==2)>1){
      twosies <- apply(cmbs[,dens==2], 2, function(x) max(rowSums(mat[x,x])))
    } else twosies <- max(rowSums(mat[cmbs[,dens==2], cmbs[,dens==2]]))
    H4 <- sum(twosies==1)
    L4 <- sum(twosies==2)
  } else H4 <- L4 <- 0
  
  if(any(dens==3)){
    if(sum(dens==3)>1){
      threesies <- apply(cmbs[,dens==3], 2, function(x) max(rowSums(mat[x,x])))
    } else threesies <- max(rowSums(mat[cmbs[,dens==3], cmbs[,dens==3]]))
    D4 <- sum(threesies==2)
    U4 <- sum(threesies==1)
    Y4 <- sum(threesies==3)
  } else D4 <- U4 <- Y4 <- 0
  
  if(any(dens==4)){
    if(sum(dens==4)>1){
      foursies <- apply(cmbs[,dens==4], 2, function(x) max(rowSums(mat[x,x])))
    } else foursies <- max(rowSums(mat[cmbs[,dens==4], cmbs[,dens==4]]))
    P4 <- sum(foursies==3)
    C4 <- sum(foursies==2)
  } else P4 <- C4 <- 0
  
  Z4 <- sum(dens == 5)
  X4 <- sum(dens == 6)
  
  out <- c(E4 = E4, I4 = I4, H4 = H4, L4 = L4, D4 = D4, U4 = U4, Y4 = Y4, 
           P4 = P4, C4 = C4, Z4 = Z4, X4 = X4)
  make_network_motif(out, .data)
}

#' @rdname motif_net 
#' @source Alejandro Espinosa 'netmem'
#' @references 
#' ## On the mixed census
#' Hollway, James, Alessandro Lomi, Francesca Pallotti, and Christoph Stadtfeld. 2017.
#' “Multilevel Social Spaces: The Network Dynamics of Organizational Fields.” 
#' _Network Science_ 5(2): 187–212.
#' \doi{10.1017/nws.2017.8}
#' @examples 
#' net_x_mixed(fict_marvel)
#' @export
net_x_mixed <- function (.data, object2) {
  .data <- manynet::expect_nodes(.data)
  if(missing(object2) && manynet::is_multiplex(.data)) {
    object2 <- manynet::to_uniplex(.data, unique(manynet::tie_attribute(.data, "type"))[2])
    .data <- manynet::to_uniplex(.data, unique(manynet::tie_attribute(.data, "type"))[1])
  }
  if(manynet::is_twomode(.data))
    manynet::snet_abort("First object should be a one-mode network")
  if(!manynet::is_twomode(object2))
    manynet::snet_abort("Second object should be a two-mode network")
  if(manynet::net_dims(.data)[1] != manynet::net_dims(object2)[1])
    manynet::snet_abort("Non-conformable arrays")
  m1 <- manynet::as_matrix(.data)
  m2 <- manynet::as_matrix(object2)
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
  make_network_motif(res, .data)
}

# Exposure ####

#' Motifs of nodes exposure
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
  names(out) <- paste0("t", diff_model$time)
  make_network_motif(out, net)
}


