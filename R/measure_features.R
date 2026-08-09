# Topological features ####

#' Measuring network topological features
#' @name measure_features
#' @description
#'   These functions measure topological features that are intrinsic to a
#'   network, in the sense that they require nothing of the user beyond the
#'   network itself:
#'   
#'   - `net_by_richclub()` measures the rich-club coefficient of a network.
#'   - `net_by_smallworld()` measures the small-world coefficient for one- or 
#'   two-mode networks. Small-world networks can be highly clustered and yet
#'   have short path lengths.
#'   - `net_by_scalefree()` measures the exponent of a fitted
#'   power-law distribution. An exponent between 2 and 3 usually indicates 
#'   a power-law distribution.
#'   - `net_by_balance()` measures the structural balance index on 
#'   the proportion of balanced triangles,
#'   ranging between `0` if all triangles are imbalanced and 
#'   `1` if all triangles are balanced.
#' 
#' @template param_data
#' @family features
#' @template net_measure
NULL

#' @rdname measure_features
#' @references
#' ## On the rich-club coefficient
#' Zhou, Shi, and Raul J. Mondragon. 2004.
#' "The Rich-Club Phenomenon in the Internet Topology".
#' _IEEE Communications Letters_, 8(3): 180-182.
#' \doi{10.1109/lcomm.2004.823426}
#' @examples
#' net_by_richclub(ison_adolescents)
#' @export
net_by_richclub <- function(.data){
  .data <- manynet::expect_nodes(.data)
  coefs <- vector()
  temp <- .data
  for(k in seq_len(max(node_by_deg(temp)))){
    richclub <- manynet::to_subgraph(temp, node_by_deg(temp) >= k)
    nk <- manynet::net_nodes(richclub)
    ek <- ifelse(manynet::is_directed(temp),
                 manynet::net_ties(richclub), 
                 2*manynet::net_ties(richclub))
    coefs <- c(coefs, (ek)/(nk*(nk-1)))
  }
  
  .elbow_finder <- function(x_values, y_values) {
    # Max values to create line
    # if(min(x_values)==1) x_values <- x_values[2:length(x_values)]
    # if(min(y_values)==0) y_values <- y_values[2:length(y_values)]
    max_df <- data.frame(x = c(1, min(which(y_values == 1))), 
                         y = c(min(y_values), max(y_values)))
    # Creating straight line between the max values
    fit <- stats::lm(max_df$y ~ max_df$x)
    # Distance from point to line
    distances <- vector()
    for (i in seq_len(length(x_values))) {
      distances <- c(distances,
                     abs(stats::coef(fit)[2]*x_values[i] -
                           y_values[i] +
                           coef(fit)[1]) /
                       sqrt(stats::coef(fit)[2]^2 + 1^2))
    }
    # Max distance point
    x_max_dist <- x_values[which.max(distances)]
    x_max_dist
  }
  
  coefs[is.nan(coefs)] <- 1
  if(length(which(coefs == 1)) == 0) out <- 0 else
    out <- coefs[.elbow_finder(seq_along(coefs), coefs)]
  # max(coefs, na.rm = TRUE)
  make_network_measure(out, .data, call = deparse(sys.call()))
}
#' @rdname measure_features 
#' @param times Integer of number of simulations.
#' @param method There are three small-world measures implemented:
#'   - "sigma" is the original equation from Watts and Strogatz (1998),
#'     \deqn{\frac{\frac{C}{C_r}}{\frac{L}{L_r}}}, 
#'     where \eqn{C} and \eqn{L} are the observed 
#'     clustering coefficient and path length, respectively,
#'     and \eqn{C_r} and \eqn{L_r} are the averages obtained from
#'     random networks of the same dimensions and density.
#'     A \eqn{\sigma > 1} is considered to be small-world,
#'     but this measure is highly sensitive to network size.
#'  -  "omega" (the default) is an update from Telesford et al. (2011),
#'     \deqn{\frac{L_r}{L} - \frac{C}{C_l}},
#'     where \eqn{C_l} is the clustering coefficient for a lattice graph
#'     with the same dimensions.
#'     \eqn{\omega} ranges between 0 and 1, 
#'     where 1 is as close to a small-world as possible.
#'  -  "SWI" is an alternative proposed by Neal (2017),
#'     \deqn{\frac{L - L_l}{L_r - L_l} \times \frac{C - C_r}{C_l - C_r}},
#'     where \eqn{L_l} is the average path length for a lattice graph
#'     with the same dimensions.
#'     \eqn{SWI} also ranges between 0 and 1 with the same interpretation, 
#'     but where there may not be a network for which \eqn{SWI = 1}.
#' @seealso [net_by_transitivity()] and [net_by_equivalency()]
#'   for how clustering is calculated
#' @references 
#' ## On small-worldliness
#' Watts, Duncan J., and Steven H. Strogatz. 1998. 
#'   “Collective Dynamics of ‘Small-World’ Networks”. 
#'   _Nature_ 393(6684):440–42.
#'   \doi{10.1038/30918}
#' 
#' Telesford QK, Joyce KE, Hayasaka S, Burdette JH, Laurienti PJ. 2011. 
#'   "The ubiquity of small-world networks". 
#'   _Brain Connectivity_ 1(5): 367–75.
#'   \doi{10.1089/brain.2011.0038}
#'   
#' Neal, Zachary P. 2017. 
#'   "How small is it? Comparing indices of small worldliness". 
#'   _Network Science_. 5 (1): 30–44.
#'   \doi{10.1017/nws.2017.5}
#' @examples
#' net_by_smallworld(ison_brandes)
#' net_by_smallworld(ison_southern_women)
#' @export
net_by_smallworld <- function(.data, 
                               method = c("omega", "sigma", "SWI"),
                               times = 100) {
  
  .data <- manynet::expect_nodes(.data)
  method <- match.arg(method)
  
  if(manynet::is_twomode(.data)){
    co <- net_by_equivalency(.data)
    cr <- mean(vapply(1:times, 
                      function(x) net_by_equivalency(manynet::generate_random(.data)),
                      FUN.VALUE = numeric(1)))
    if(method %in% c("omega", "SWI")){
      cl <- net_by_equivalency(manynet::create_ring(.data))
    }
  } else {
    co <- net_by_transitivity(.data)
    cr <- mean(vapply(1:times, 
                            function(x) net_by_transitivity(manynet::generate_random(.data)),
                            FUN.VALUE = numeric(1)))
    if(method %in% c("omega", "SWI")){
      cl <- net_by_transitivity(manynet::create_lattice(.data))
    }
  }
  
  lo <- net_by_length(.data)
  lr <- mean(vapply(1:times, 
                         function(x) net_by_length(manynet::generate_random(.data)),
                         FUN.VALUE = numeric(1)))
  if(method == "SWI"){
    ll <- net_by_length(manynet::create_ring(.data))
  }
  
  out <- switch(method,
                "omega" = (lr/lo - co/cl),
                "sigma" = (co/cr)/(lo/lr),
                "SWI" = ((lo - ll)/(lr - ll))*((co - cr)/(cl - cr)))
  make_network_measure(out,
                       .data, call = deparse(sys.call()))
}
#' @rdname measure_features 
#' @importFrom igraph fit_power_law
#' @references
#' ## On scale-free networks
#' Barabasi, Albert-Laszlo, and Reka Albert. 1999.
#' "Emergence of scaling in random networks",
#' _Science_, 286(5439): 509-512.
#' \doi{10.1126/science.286.5439.509}
#' 
#' Clauset, Aaron, Cosma Rohilla Shalizi, and Mark E.J. Newman. 2009.
#' "Power-law distributions in empirical data",
#' _SIAM Review_, 51(4): 661-703.
#' \doi{10.1137/070710111}
#' 
#' Stumpf, Michael P.H., and Mason Porter. 2012.
#' "Critical truths about power laws",
#' _Science_, 335(6069): 665-666.
#' \doi{10.1126/science.1216142}
#' 
#' Holme, Petter. 2019.
#' "Rare and everywhere: Perspectives on scale-free networks",
#' _Nature Communications_, 10(1): 1016.
#' \doi{10.1038/s41467-019-09038-8}
#' @examples 
#' net_by_scalefree(ison_adolescents)
#' net_by_scalefree(generate_scalefree(50, 1.5))
#' net_by_scalefree(create_lattice(100))
#' @export
net_by_scalefree <- function(.data){
  .data <- manynet::expect_nodes(.data)
  out <- igraph::fit_power_law(node_by_deg(.data))
  if ("KS.p" %in% names(out) && !is.null(out$KS.p) && !is.na(out$KS.p) && out$KS.p < 0.05) 
    manynet::snet_info("Note: Kolmogorov-Smirnov test that data could have been drawn",
                       "from a power-law distribution rejected.")
  make_network_measure(out$alpha, .data, 
                       call = deparse(sys.call()))
}
#' @rdname measure_features 
#' @source `{signnet}` by David Schoch
#' @references
#' ## On balance theory
#' Heider, Fritz. 1946.
#' "Attitudes and cognitive organization".
#' _The Journal of Psychology_, 21: 107-112.
#' \doi{10.1080/00223980.1946.9917275}
#' 
#' Cartwright, D., and Frank Harary. 1956.
#' "Structural balance: A generalization of Heider's theory".
#' _Psychological Review_, 63(5): 277-293.
#' \doi{10.1037/h0046049}
#' @examples
#' net_by_balance(to_uniplex(fict_marvel, "relationship"))
#' @export
net_by_balance <- function(.data) {
  
  .data <- manynet::expect_nodes(.data)
  .count_signed_triangles <- function(.data){
    g <- manynet::as_igraph(.data)
    if (!"sign" %in% igraph::edge_attr_names(g)) {
      manynet::snet_abort("network does not have a sign edge attribute")
    }
    if (igraph::is_directed(g)) {
      manynet::snet_abort("g must be undirected")
    }
    eattrV <- igraph::edge_attr(g, "sign")
    if (!all(eattrV %in% c(-1, 1))) {
      manynet::snet_abort("sign may only contain -1 and 1")
    }
    tmat <- t(matrix(igraph::triangles(g), nrow = 3))
    if (nrow(tmat) == 0) {
      warning("g does not contain any triangles")
      return(c(`+++` = 0, `++-` = 0, `+--` = 0, `---` = 0))
    }
    emat <- t(apply(tmat, 1, function(x) c(igraph::get_edge_ids(g, 
                                                                x[1:2]), 
                                           igraph::get_edge_ids(g, x[2:3]), 
                                           igraph::get_edge_ids(g, 
                                                                                                                               x[c(3, 1)]))))
    emat[, 1] <- eattrV[emat[, 1]]
    emat[, 2] <- eattrV[emat[, 2]]
    emat[, 3] <- eattrV[emat[, 3]]
    emat <- t(apply(emat, 1, sort))
    emat_df <- as.data.frame(emat)
    res <- stats::aggregate(list(count = rep(1, nrow(emat_df))), 
                            emat_df, length)
    tri_counts <- c(`+++` = 0, `++-` = 0, `+--` = 0, `---` = 0)
    tmp_counts <- res[, 4]
    if (nrow(res) == 1) {
      names(tmp_counts) <- paste0(c("+", "-")[(rev(res[1:3]) == 
                                                 -1) + 1], collapse = "")
    }
    else {
      names(tmp_counts) <- apply(res[, 1:3], 1, function(x) paste0(c("+", 
                                                                     "-")[(rev(x) == -1) + 1], collapse = ""))
    }
    tri_counts[match(names(tmp_counts), names(tri_counts))] <- tmp_counts
    tri_counts
  }
  
  if (!manynet::is_signed(.data)) {
    manynet::snet_abort("network does not have a sign edge attribute")
  }
  if (manynet::is_directed(.data)) {
    manynet::snet_abort("object must be undirected")
  }
  g <- manynet::as_igraph(.data)
  eattrV <- igraph::edge_attr(g, "sign")
  if (!all(eattrV %in% c(-1, 1))) {
    manynet::snet_abort("sign may only contain -1 and 1")
  }
  tria_count <- .count_signed_triangles(g)
  make_network_measure(unname((tria_count["+++"] + tria_count["+--"])/sum(tria_count)),
                       .data, 
                       call = deparse(sys.call()))
}

# Structural fit ####

#' Measuring how well a structure fits a network
#' @name measure_fit
#' @description
#'   These functions measure how well some proposed structure describes a
#'   network. Unlike the intrinsic properties in [measure_features], each takes
#'   a structure from the user — a core-periphery mark, or a partition of the
#'   nodes — and returns how closely the observed network corresponds to it:
#'
#'   - `net_by_core()` measures the correlation between a network
#'   and a core-periphery model with the same dimensions.
#'   - `net_by_factions()` measures the correlation between a network
#'   and a component model with the same dimensions.
#'   - `net_by_modularity()` measures the modularity of a network
#'   based on nodes' membership in defined clusters.
#'   - `net_by_inconsistency()` measures how far a partition's blocks depart from
#'   ideal block types.
#'
#'   These are the natural companions to the `node_in_*()` functions, which
#'   propose a structure; these say how good that proposal is.
#'   Where a partition is expected but none is given, the network is
#'   partitioned into two using [node_in_partition()].
#'
#'   Note that they are not on a common scale, and do not all run in the same
#'   direction, so they are not interchangeable:
#'
#'   | measure | compares the network against | range | better |
#'   | --- | --- | --- | --- |
#'   | `net_by_core()` | a core-periphery model | -1 to 1 | higher |
#'   | `net_by_factions()` | a components model | -1 to 1 | higher |
#'   | `net_by_modularity()` | the partition's communities | -0.5 to 1 | higher |
#'   | `net_by_inconsistency()` | ideal block types | 0 upwards | **lower** |
#'
#'   Compare partitions using one measure at a time.
#'
#' @template param_data
#' @template param_memb
#' @family features
#' @template net_measure
NULL

#' @rdname measure_fit
#' @param mark A logical vector indicating which nodes belong to the core.
#' @param method Which method of the following to use to calculate the fit of
#'   the core assignment to a core-periphery model.
#'   "correlation" calculates the correlation between the empirical network and
#'   an ideal typical network, and "ident" calculates the Euclidean distances
#'   between the same.
#'   "ndiff", however, calculates how distinct the core and periphery groups are
#'   based on the difference in coreness scores between the least core-like
#'   member of the core and the most core-like member of the periphery.
#'   "diff" is similar to "ndiff", but multiplies the raw "ndiff" score by the
#'   square root of the size of the core, thus penalising large cores.
#' @section Core-Periphery: 
#'   `net_core()` calculates the Pearson correlation between the given network, 
#'   where the nodes in the core are assigned by some given mark, and an ideal
#'   typical core-periphery network with the same number of nodes in the core
#'   and the periphery.
#' @references 
#' ## On core-periphery
#' Borgatti, Stephen P., and Martin G. Everett. 2000. 
#' “Models of Core/Periphery Structures.” 
#' _Social Networks_ 21(4):375–95.
#' \doi{10.1016/S0378-8733(99)00019-2}
#' @examples 
#' net_by_core(ison_adolescents)
#' net_by_core(ison_southern_women)
#' @export
net_by_core <- function(.data,
                        mark = NULL,
                        method = c("correlation","ident","ndiff", "diff")){
  .data <- manynet::expect_nodes(.data)
  if(is.null(mark)) mark <- node_is_core(.data)
  
  method <- match.arg(method)
  if(method == "correlation"){
    out <- stats::cor(c(manynet::as_matrix(.data)), 
                      c(manynet::as_matrix(manynet::create_core(.data, mark = mark))))
  } else if(method == "ident"){
    out <- sqrt(sum((manynet::as_matrix(.data) - 
                       manynet::as_matrix(manynet::create_core(.data, mark = mark)))^2))
  } else if(method %in% c("ndiff","diff")){
    # Sort nodes by coreness
    c_scores <- node_by_coreness(.data)
    core <- c_scores[mark]
    periphery <- c_scores[!mark]
    
    min_core <- min(core)
    max_periphery <- max(periphery)
    
    diff1 <- sum(min_core - periphery)
    diff2 <- sum(core - max_periphery)
    
    if(method == "ndiff"){
      out <- (diff1 + diff2) / length(c_scores)  # Normalize
    } else if(method == "diff"){
      out <- (diff1 + diff2) * sqrt(sum(mark))
    } 
  } else manynet::snet_unavailable(method)
  make_network_measure(out, .data, call = deparse(sys.call()))
}

#' @rdname measure_fit 
#' @examples 
#'   net_by_factions(ison_southern_women)
#' @export
net_by_factions <- function(.data,
                            membership = NULL){
  .data <- manynet::expect_nodes(.data)
  membership <- .resolve_membership(.data, membership)
  if(is.null(membership)){
    manynet::snet_info("No membership vector assigned.",
                       "Partitioning the network using {.fn node_in_partition}.")
    membership <- node_in_partition(.data)
  }
  out <- stats::cor(c(manynet::as_matrix(.data)), 
                    c(manynet::as_matrix(manynet::create_components(.data,
                                                                    membership = membership))))
  make_network_measure(out, .data, call = deparse(sys.call()))
}

#' @rdname measure_fit
#' @section Modularity:
#'   Modularity measures the difference between the number of ties within each community
#'   from the number of ties expected within each community in a random graph
#'   with the same degrees, and ranges between -1 and +1.
#'   Modularity scores of +1 mean that ties only appear within communities,
#'   while -1 would mean that ties only appear between communities.
#'   A score of 0 would mean that ties are half within and half between communities,
#'   as one would expect in a random graph.
#'   
#'   Modularity faces a difficult problem known as the resolution limit 
#'   (Fortunato and Barthélemy 2007).
#'   This problem appears when optimising modularity,
#'   particularly with large networks or depending on the degree of interconnectedness,
#'   can miss small clusters that 'hide' inside larger clusters.
#'   In the extreme case, this can be where they are only connected
#'   to the rest of the network through a single tie.
#'   To help manage this problem, a `resolution` parameter is added. 
#'   Please see the argument definition for more details.
#' @param resolution A proportion indicating the resolution scale.
#'   By default 1, which returns the original definition of modularity.
#'   The higher this parameter, the more smaller communities will be privileged.
#'   The lower this parameter, the fewer larger communities are likely to be found.
#' @examples 
#' net_by_modularity(ison_adolescents, 
#'   node_in_partition(ison_adolescents))
#' net_by_modularity(ison_southern_women, 
#'   node_in_partition(ison_southern_women))
#' @references 
#' ## On modularity
#' Newman, Mark E.J. 2006.
#' "Modularity and community structure in networks",
#' _Proceedings of the National Academy of Sciences_ 103(23): 8577-8696.
#' \doi{https://doi.org/10.1073/pnas.0601602103}
#' 
#' Murata, Tsuyoshi. 2010. 
#' "Modularity for Bipartite Networks". 
#' In: Memon, N., Xu, J., Hicks, D., Chen, H. (eds) 
#' _Data Mining for Social Network Data. Annals of Information Systems_, Vol 12. 
#' Springer, Boston, MA. 
#' \doi{10.1007/978-1-4419-6287-4_7}
#' @export
net_by_modularity <- function(.data, 
                              membership = NULL, 
                              resolution = 1){
  .data <- manynet::expect_nodes(.data)
  membership <- .resolve_membership(.data, membership)
  if(is.null(membership)){
    manynet::snet_info("Since no membership argument has been provided,",
                       "a partition of the network into two will be calculated and used.")
    membership <- node_in_partition(.data)
  }
  if(!is.numeric(membership)) membership <- as.numeric(as.factor(membership))
  if(!manynet::is_graph(.data)) .data <- as_igraph(.data)
  if(manynet::is_twomode(.data)){
    make_network_measure(igraph::modularity(manynet::to_multilevel(.data), 
                                            membership = membership,
                                            resolution = resolution), 
                         .data, call = deparse(sys.call()))
  } else make_network_measure(igraph::modularity(.data, 
                                                 membership = membership,
                                                 resolution = resolution),
                              .data, call = deparse(sys.call()))
}

#' @rdname measure_fit
#' @param blocks A character vector of permitted ideal block types,
#'   or a list-matrix giving the permitted types for each block position.
#'   By default `c("nul", "com")`, which is structural blockmodelling.
#'   See the section below.
#' @section Blockmodelling:
#'   A blockmodel proposes that a partition reduces a network to a small number
#'   of positions, so that every block — the ties running from one position to
#'   another — is of some simple ideal type.
#'   `net_by_inconsistency()` measures how far the network departs from that proposal,
#'   by counting the ties that would have to be added or removed to make every
#'   block ideal, normalized by the number of cells.
#'   **Lower is better**: 0 means the partition fits perfectly.
#'
#'   This is a _distance_ from an ideal image rather than a measure of fit —
#'   hence the name, and hence its running the opposite way to the rest of this
#'   page. Three consequences are worth knowing:
#'
#'   - **Its complement is not a proportion.** The criterion mixes units: `nul`
#'   and `com` count cells, while `reg` counts empty rows and columns, and all
#'   are divided by the cell count. So do not read \eqn{1 - x} as the share of
#'   the network that the blockmodel gets right.
#'   - **It is not bounded above by 1.** That holds only for cell-counting
#'   vocabularies such as `c("nul", "com")`. With `reg` permitted it can exceed
#'   1 — on `ison_adolescents`, `blocks = "reg"` over singleton positions
#'   reaches about 1.57.
#'   - **The vocabularies behave very differently at fine partitions.** Giving
#'   every node its own position scores 0 under `c("nul", "com")`, since each
#'   block is then a single cell and trivially ideal, but scores its _worst_
#'   under `"reg"`, since each block then has an empty row and column.
#'
#'   For a correlation-scaled, higher-is-better reading of the common structural
#'   case, see [net_by_factions()]. The two are related but not equivalent:
#'   `net_by_factions()` fixes the image — complete on the diagonal, null off it
#'   — whereas `net_by_inconsistency(blocks = c("nul", "com"))` lets each block take
#'   whichever of the two ideals fits it better, and so is more permissive.
#'
#'   The ideal types are:
#'   \describe{
#'     \item{`nul`}{a null block, containing no ties.}
#'     \item{`com`}{a complete block, containing every possible tie.}
#'     \item{`reg`}{a regular block, in which every row and every column has at
#'       least one tie, though not necessarily all of them.}
#'     \item{`rdo`, `cdo`}{a row- or column-dominant block, containing at least
#'       one complete row or column.}
#'     \item{`dnc`}{"do not care": a block left unconstrained.}
#'   }
#'
#'   `blocks` is a _vocabulary_ rather than an assignment: each block is scored
#'   at the lowest inconsistency of any permitted type, and the results summed.
#'   Any subset may be given, and the two conventional choices are
#'   `c("nul", "com")` for structural equivalence and `c("nul", "reg")` for
#'   regular equivalence.
#'
#'   Note that permitting more types can only lower the criterion, since each
#'   block gains more ways to be satisfied. The size of the vocabulary is
#'   therefore itself a modelling choice, and criterion values are comparable
#'   across partitions only when the same vocabulary is used for each.
#'
#'   For fully generalized blockmodelling, pass a `g` by `g` list-matrix
#'   naming the types permitted at each position separately,
#'   e.g. `reg` on the diagonal and `nul` off it for a "cohesive positions"
#'   model.
#' @references
#' ## On generalized blockmodelling
#' Doreian, Patrick, Vladimir Batagelj, and Anuska Ferligoj. 2005.
#' _Generalized Blockmodeling_.
#' Cambridge: Cambridge University Press.
#' \doi{10.1017/CBO9780511584176}
#' @examples
#' net_by_inconsistency(ison_hightech, node_in_regular(ison_hightech))
#' # a regular-equivalence vocabulary instead of a structural one
#' net_by_inconsistency(ison_hightech, node_in_structural(ison_hightech), 
#'        blocks = c("nul", "reg"))
#' @export
net_by_inconsistency <- function(.data, membership = NULL,
                                 blocks = c("nul", "com")){
  .data <- manynet::expect_nodes(.data)
  membership <- .resolve_membership(.data, membership)
  if(is.null(membership)){
    manynet::snet_info("No membership vector assigned.",
                       "Partitioning the network using {.fn node_in_partition}.")
    membership <- node_in_partition(.data)
  }
  mat <- manynet::as_matrix(manynet::to_unweighted(manynet::to_multilevel(.data)))
  memb <- as.numeric(as.factor(membership))
  g <- max(memb)
  loops <- manynet::is_complex(.data)
  total <- 0
  for(i in seq_len(g)) for(j in seq_len(g)){
    permitted <- .permitted_blocks(blocks, i, j)
    sub <- mat[memb == i, memb == j, drop = FALSE]
    # a node cannot be tied to itself unless the network is complex, so
    # the diagonal of a diagonal block is not evidence either way
    if(i == j && !loops) diag(sub) <- NA
    total <- total + min(vapply(permitted, .block_inconsistency, sub,
                                FUN.VALUE = numeric(1)))
  }
  cells <- if(loops) length(mat) else length(mat) - nrow(mat)
  make_network_measure(total/cells, .data, call = deparse(sys.call()))
}

# Resolve the vocabulary permitted at block position (i,j), which is either
# shared across all blocks or given per position for generalized blockmodelling.
.permitted_blocks <- function(blocks, i, j){
  out <- if(is.matrix(blocks) || is.list(blocks) && !is.null(dim(blocks)))
    blocks[[i, j]] else blocks
  out <- match.arg(out, c("nul", "com", "reg", "rdo", "cdo", "dnc"),
                   several.ok = TRUE)
  out
}

# The number of ties that would have to be added to, or removed from, a block
# for it to match a given ideal type. NA cells (a diagonal block's diagonal)
# are not counted either way.
.block_inconsistency <- function(type, sub){
  if(length(sub) == 0) return(0)
  switch(type,
         # a null block should be empty, so every tie present is an error
         nul = sum(sub, na.rm = TRUE),
         # a complete block should be full, so every tie absent is an error
         com = sum(sub == 0, na.rm = TRUE),
         # a regular block needs every row and column to be non-empty
         reg = sum(rowSums(sub, na.rm = TRUE) == 0) +
           sum(colSums(sub, na.rm = TRUE) == 0),
         # a dominant block needs one complete row (or column), so the error
         # is how far the nearest row (column) falls short of being complete
         rdo = min(rowSums(sub == 0, na.rm = TRUE)),
         cdo = min(colSums(sub == 0, na.rm = TRUE)),
         dnc = 0)
}
