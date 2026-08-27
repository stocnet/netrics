# Community clustering ####

# Helpers for targeting a number of communities ####

# Validates `k`, returning NULL, a single integer, or a method name.
check_k <- function(k, .data){
  if(is.null(k)) return(NULL)
  if(is.character(k)) return(match.arg(k, c("silhouette", "elbow", "strict")))
  if(!is.numeric(k) || length(k) != 1 || k < 1 || k %% 1 != 0)
    manynet::snet_abort("`k` must be a single positive integer,",
                        "or one of {.val silhouette}, {.val elbow}, {.val strict}.")
  if(k > manynet::net_nodes(.data))
    manynet::snet_abort("`k` cannot exceed the number of nodes.")
  as.integer(k)
}

# Geodesic distances, for scoring candidate partitions.
node_dists <- function(.data){
  d <- igraph::distances(manynet::as_igraph(.data))
  d[is.infinite(d)] <- manynet::net_nodes(.data) # unconnected pairs
  d
}

# Mean silhouette width of one membership vector.
# Note this is the per-partition core of `k_silhouette()`, which cannot be
# called here because it reads `hc$distances`, which only
# `cluster_hierarchical()` attaches and community dendrograms lack.
sil_score <- function(memb, d){
  if(length(unique(memb)) < 2) return(NA_real_)
  mean(vapply(seq_along(memb), function(i){
    wig <- which(memb == memb[i])
    wig <- wig[wig != i]
    # a node alone in its group scores 0, per Rousseeuw
    if(length(wig) == 0) return(0)
    ai <- mean(d[i, wig])
    wog <- which(memb != memb[i])
    bi <- min(vapply(unique(memb[wog]),
                     function(b) mean(d[i, wog[memb[wog] == b]]),
                     FUN.VALUE = numeric(1)))
    (bi - ai)/max(ai, bi)
  }, FUN.VALUE = numeric(1)))
}

# Share of ties that fall inside a group.
coverage <- function(.data, memb){
  e <- igraph::as_edgelist(manynet::as_igraph(.data), names = FALSE)
  if(nrow(e) == 0) return(0)
  mean(memb[e[,1]] == memb[e[,2]])
}

# Selects among candidate partitions, given in increasing k.
select_k <- function(parts, .data, method){
  ks <- vapply(parts, function(p) length(unique(p)), FUN.VALUE = integer(1))
  if(method == "silhouette"){
    d <- node_dists(.data)
    scores <- vapply(parts, sil_score, d = d, FUN.VALUE = numeric(1))
    if(all(is.na(scores))) return(parts[[1]])
    parts[[which.max(scores)]]
  } else {
    cvs <- vapply(parts, function(p) coverage(.data, p), FUN.VALUE = numeric(1))
    parts[[which(ks == elbow_point(ks, cvs))[1]]]
  }
}

# Greedily merges the pair of groups whose merge best preserves modularity.
merge_to_k <- function(.data, memb, k){
  gr <- manynet::as_igraph(.data)
  while(length(unique(memb)) > k){
    gs <- unique(memb)
    best <- NULL
    bestq <- -Inf
    for(i in seq_along(gs)) for(j in seq_along(gs)) if(i < j){
      cand <- memb
      cand[cand == gs[j]] <- gs[i]
      q <- igraph::modularity(gr, as.integer(factor(cand)))
      if(q > bestq){ bestq <- q; best <- c(gs[i], gs[j]) }
    }
    memb[memb == best[2]] <- best[1]
  }
  as.integer(factor(memb))
}

# Bisects the resolution parameter of `fun` to reach k communities.
# The number of communities rises with the resolution, but not strictly,
# so the best result found is kept and the iteration cap stops the search.
cut_res <- function(fun, gr, k, lower = 1e-6, upper = 100, iter = 40){
  best <- NULL
  bestk <- NA
  for(i in seq_len(iter)){
    mid <- (lower + upper)/2
    memb <- fun(gr, resolution = mid)$membership
    found <- length(unique(memb))
    if(is.na(bestk) || abs(found - k) < abs(bestk - k)){
      best <- memb
      bestk <- found
    }
    if(found == k) return(memb)
    if(found < k) lower <- mid else upper <- mid
  }
  best
}

# Cuts a hierarchical clustering at `no` groups.
# The merge tree can be incomplete, on an unconnected network or where the
# algorithm stopped splitting. igraph then warns and returns more groups than
# asked, which `report_k()` reports in the package's own style.
cut_tree <- function(clust, no){
  tryCatch(suppressWarnings(igraph::cut_at(clust, no = no)),
           error = function(e) clust$membership)
}

# Warns where the requested number of communities was not reached.
report_k <- function(memb, k){
  found <- length(unique(memb))
  if(is.numeric(k) && found != k)
    manynet::snet_warn("This algorithm returns {found} communities here,",
                       "and not the {k} requested.")
  memb
}

# The partition in which no tie crosses a group.
strict_memb <- function(.data){
  manynet::snet_info("Returning the components partition,",
                     "in which no tie crosses a group.")
  igraph::components(manynet::as_igraph(.data))$membership
}

# Resolves `k` for one algorithm.
# `at_k(no)` returns a membership vector with `no` groups,
# and `default()` returns the algorithm's own partition.
apply_k <- function(k, Kmax, .data, at_k, default){
  n <- manynet::net_nodes(.data)
  memb <- if(is.null(k)) default() else
    if(identical(k, "strict")) strict_memb(.data) else
      if(is.character(k)) select_k(lapply(2:min(Kmax, n), at_k), .data, k) else
        at_k(k)
  report_k(memb, k)
}

#' Memberships in communities
#' @name member_community
#' @description
#'   `node_in_community()` runs through all available community detection algorithms 
#'   for a given network type, finds the algorithm that returns the
#'   largest modularity score, and returns the corresponding membership
#'   partition.
#'   Where feasible (a small enough network), the optimal problem solving
#'   technique is used to ensure the maximal modularity partition.
#'   For larger networks, it identifies the applicable algorithms and 
#'   finds the algorithm that maximises modularity and 
#'   returns that membership vector.
#'   
#' @template param_data
#' @template param_k
#' @family community
#' @template node_member
NULL

#' @rdname member_community
#' @export
node_in_community <- function(.data, k = NULL, Kmax = 8L){
  .data <- manynet::expect_nodes(.data)
  k <- check_k(k, .data)
  if(is.null(k) && manynet::net_nodes(.data)<100){
    # don't use node_in_betweenness because slow and poorer quality to optimal
    manynet::snet_success("{.fn node_in_optimal} available and", 
                          "will return the highest modularity partition.")
    netrics::node_in_optimal(.data)
  } else {
    if(is.null(k)){
      manynet::snet_info("Excluding {.fn node_in_optimal} because network rather large.")
      poss_algs <- c("node_in_infomap",
                     "node_in_spinglass",
                     "node_in_fluid",
                     "node_in_louvain",
                     "node_in_leiden",
                     "node_in_greedy",
                     "node_in_eigen",
                     "node_in_walktrap")
    } else {
      manynet::snet_info("Considering only those algorithms that accept {.arg k}.")
      poss_algs <- c("node_in_fluid",
                     "node_in_louvain",
                     "node_in_leiden",
                     "node_in_labels",
                     "node_in_partition",
                     "node_in_greedy",
                     "node_in_eigen",
                     "node_in_walktrap",
                     "node_in_betweenness")
    }
    if(manynet::net_nodes(.data)>=100){
      notforlarge <- intersect(poss_algs, "node_in_betweenness")
      if(length(notforlarge)){
        manynet::snet_info("Excluding {.fn {notforlarge}} because network rather large.")
        poss_algs <- setdiff(poss_algs, notforlarge)
      }
    }
    if(!manynet::is_connected(.data)){
      notforconnected <- intersect(poss_algs, c("node_in_spinglass", 
                                                "node_in_fluid"))
      if(length(notforconnected)){
        manynet::snet_info("Excluding {.fn {notforconnected}} because network unconnected.")
        poss_algs <- setdiff(poss_algs, notforconnected)
      }
    }
    if(manynet::is_directed(.data)){
      notfordirected <- intersect(poss_algs, c("node_in_louvain", 
                                               "node_in_leiden",
                                               "node_in_labels",
                                               "node_in_partition",
                                               "node_in_eigen"))
      if(length(notfordirected)){
        manynet::snet_info("Excluding {.fn {notfordirected}} because network directed.")
        poss_algs <- setdiff(poss_algs, notfordirected)
      }
    }
    manynet::snet_info("Considering each of {.fn {poss_algs}}.")
    # `snet_progress_along()` returns nothing unless verbosity is "verbose",
    # so fall back to a plain sequence to keep the loop running when quiet
    idx <- manynet::snet_progress_along(poss_algs)
    if(length(idx) != length(poss_algs)) idx <- seq_along(poss_algs)
    candidates <- lapply(idx, function(comm){
      memb <- if(is.null(k)) get(poss_algs[comm])(.data) else
        suppressWarnings(get(poss_algs[comm])(.data, k = k, Kmax = Kmax))
      mod <- net_by_modularity(.data, memb)
      list(memb, mod)
    })
    mods <- unlist(sapply(candidates, "[", 2))
    maxmod <- which.max(mods)
    manynet::snet_success("{.fn {poss_algs[maxmod]}} returns the highest modularity ({round(mods[maxmod],3)}).")
    out <- candidates[[maxmod]][[1]]
    if(is.numeric(k) && length(unique(out)) != k)
      manynet::snet_warn("No available algorithm returns {k} communities here.",
                         "Returning {length(unique(out))} instead.")
    out
  }
}

# #' @rdname member_community_hier 
# #' @section Ensemble:
# #'   Ensemble-based community detection runs community detection
# #'   algorithms over multilayer or multiplex networks.
# #' @references
# #' ## On ensemble-based community detection
# #' Tagarelli, Andrea, Alessia Amelio, and Francesco Gullo. 2017.
# #' "Ensemble-based Community Detection in Multilayer Networks".
# #' _Data Mining and Knowledge Discovery_, 31: 1506-1543.
# #' \doi{10.1007/s10618-017-0528-8}
# #' @examples
# #' node_in_ensemble(ison_adolescents)
# #' @export
# node_in_ensemble <- function(.data, linkage_constraint = TRUE){
#   if(missing(.data)) {expect_nodes(); .data <- .G()}
#   clust <- igraph::cluster_walktrap(manynet::as_igraph(.data))
#   out <- clust$membership
#   make_node_member(out, .data)
#   out <- make_node_member(out, .data)
#   attr(out, "hc") <- stats::as.hclust(clust, 
#                                       use.modularity = igraph::is_connected(.data))
#   attr(out, "k") <- max(clust$membership)
#   out
# }

# Non-hierarchical community clustering ####

#' Memberships in non-hierarchical communities
#' @name member_community_non
#' @description
#'   These functions offer algorithms for partitioning
#'   networks into sets of communities:
#' 
#'   - `node_in_optimal()` is a problem-solving algorithm that seeks to maximise 
#'   modularity over all possible partitions.
#'   - `node_in_partition()` is a greedy, iterative, deterministic
#'   partitioning algorithm that results in two equally-sized communities.
#'   - `node_in_infomap()` is an algorithm based on the information in random walks.
#'   - `node_in_spinglass()` is a greedy, iterative, probabilistic algorithm, 
#'   based on analogy to model from statistical physics.
#'   - `node_in_fluid()` is a propogation-based partitioning algorithm,
#'   based on analogy to model from fluid dynamics.
#'   - `node_in_louvain()` is an agglomerative multilevel algorithm that seeks to maximise 
#'   modularity over all possible partitions.
#'   - `node_in_leiden()` is an agglomerative multilevel algorithm that seeks to maximise
#'   the Constant Potts Model over all possible partitions.
#'   - `node_in_labels()` is a fast, propagation-based algorithm in which nodes
#'   iteratively adopt whichever community label is most common among their neighbours.
#'
#'   The different algorithms offer various advantages in terms of computation time,
#'   availability on different types of networks, ability to maximise modularity,
#'   and their logic or domain of inspiration.
#'   
#' @template param_data
#' @template param_k
#' @family community
#' @template node_member
NULL

#' @rdname member_community_non 
#' @section Optimal:
#'   The general idea is to calculate the modularity of all possible partitions,
#'   and choose the community structure that maximises this modularity measure.
#'   Note that this is an NP-complete problem with exponential time complexity.
#'   The guidance in the igraph package is networks of <50-200 nodes is probably fine.
#' @references
#' ## On optimal community detection
#' Brandes, Ulrik, Daniel Delling, Marco Gaertler, Robert Gorke, Martin Hoefer, Zoran Nikoloski, Dorothea Wagner. 2008.
#' "On Modularity Clustering", 
#' _IEEE Transactions on Knowledge and Data Engineering_ 20(2):172-188.
#' @examples
#' node_in_optimal(ison_adolescents)
#' @export
node_in_optimal <- function(.data){
  .data <- manynet::expect_nodes(.data)
  if(manynet::net_nodes(.data)>100) 
    manynet::snet_warn("This algorithm may take some time", 
    "or even run out of memory on such a large network.")
  out <- igraph::cluster_optimal(manynet::as_igraph(.data)
  )$membership
  make_node_member(out, .data)
}

#' @rdname member_community_non 
#' @section Partition:
#'   The general idea is to assign nodes to two groups, and then iteratively 
#'   swap pairs of nodes (one from each group) that give a positive sum of net tie costs,
#'   where the net tie cost of a node is the difference between the sum 
#'   of the weights of ties to nodes in the other group (external costs) and 
#'   the sum of the weights of ties to nodes in the same group (internal costs).
#'   Where `k` is greater than two, the same swap pass is run for every pair of
#'   groups, and the rounds repeat until no swap improves the partition.
#'   This is a deterministic algorithm that will always return the same partition 
#'   for a given network, but it is not guaranteed to maximise modularity.
#'   Note that this algorithm is only applicable to undirected, unipartite networks, 
#'   and returns `k` communities of equal size (or as close to equal as possible).
#' @references
#' ## On partitioning community detection
#' Kernighan, Brian W., and Shen Lin. 1970.
#' "An efficient heuristic procedure for partitioning graphs."
#' _The Bell System Technical Journal_ 49(2): 291-307.
#' \doi{10.1002/j.1538-7305.1970.tb01770.x}
#' @examples
#' node_in_partition(ison_adolescents)
#' node_in_partition(ison_southern_women)
#' @export
node_in_partition <- function(.data, k = 2L, Kmax = 8L){
  .data <- manynet::expect_nodes(.data)
  k <- check_k(k, .data)
  n <- manynet::net_nodes(.data)
  g <- manynet::as_matrix(manynet::to_multilevel(.data))
  at_k <- function(no) kl_partition(g, n, no)
  memb <- apply_k(k, Kmax, .data, at_k = at_k, default = function() at_k(2L))
  make_node_member(memb, .data)
}

# One pass of net-cost swaps between two groups.
# The net cost of a node is the sum of the weights of its ties to the other
# group (external) less the sum of the weights of its ties within its own
# group (internal). Pairs whose net costs sum to zero or more are swapped.
kl_swap <- function(g, a, b){
  intergroup <- g[a, b, drop = FALSE]
  a.net <- rowSums(intergroup) - rowSums(g[a, a, drop = FALSE])
  b.net <- colSums(intergroup) - rowSums(g[b, b, drop = FALSE])
  a.ord <- a[order(a.net, decreasing = TRUE)]
  b.ord <- b[order(b.net, decreasing = TRUE)]
  a.sort <- sort(a.net, decreasing = TRUE)
  b.sort <- sort(b.net, decreasing = TRUE)
  len <- min(length(a.sort), length(b.sort))
  if(len == 0) return(list(a = a, b = b, swapped = FALSE))
  index <- which(a.sort[seq_len(len)] + b.sort[seq_len(len)] >= 0)
  if(length(index) == 0) return(list(a = a, b = b, swapped = FALSE))
  a.new <- a.ord
  b.new <- b.ord
  a.new[index] <- b.ord[index]
  b.new[index] <- a.ord[index]
  list(a = a.new, b = b.new, swapped = TRUE)
}

# k-way Kernighan-Lin. Nodes start in k groups of near-equal size, in node
# order, and every pair of groups is swept until no round makes a swap.
kl_partition <- function(g, n, k, rounds = 50){
  memb <- sort(rep(seq_len(k), length.out = n))
  groups <- lapply(seq_len(k), function(i) which(memb == i))
  for(r in seq_len(rounds)){
    moved <- FALSE
    for(i in seq_len(k)) for(j in seq_len(k)) if(i < j){
      res <- kl_swap(g, groups[[i]], groups[[j]])
      if(res$swapped){
        groups[[i]] <- res$a
        groups[[j]] <- res$b
        moved <- TRUE
      }
    }
    if(!moved) break
  }
  out <- integer(n)
  for(i in seq_len(k)) out[groups[[i]]] <- i
  out
}

#' @rdname member_community_non 
#' @section Infomap:
#'   Motivated by information theoretic principles, this algorithm tries to build 
#'   a grouping that provides the shortest description length for a random walk,
#'   where the description length is measured by the expected number of bits 
#'   per node required to encode the path.
#' @param times Integer indicating number of simulations/walks used.
#'   By default, `times=50`.
#' @references
#' ## On infomap community detection
#' Rosvall, M, and C. T. Bergstrom. 2008.
#' "Maps of information flow reveal community structure in complex networks", 
#' _PNAS_ 105:1118.
#' \doi{10.1073/pnas.0706851105}
#' 
#' Rosvall, M., D. Axelsson, and C. T. Bergstrom. 2009.
#' "The map equation", 
#' _Eur. Phys. J. Special Topics_ 178: 13. 
#' \doi{10.1140/epjst/e2010-01179-1}
#' @examples
#' node_in_infomap(ison_adolescents)
#' @export
node_in_infomap <- function(.data, times = 50){
  .data <- manynet::expect_nodes(.data)
  out <- igraph::cluster_infomap(manynet::as_igraph(.data), 
                                 nb.trials = times
  )$membership
  make_node_member(out, .data)
}

#' @rdname member_community_non 
#' @param max_k Integer constant, the number of spins to use as an upper limit
#'   of communities to be found. Some sets can be empty at the end.
#' @param resolution The Reichardt-Bornholdt “gamma” resolution parameter for modularity.
#'   By default 1, making existing and non-existing ties equally important.
#'   Smaller values make existing ties more important,
#'   and larger values make missing ties more important.
#' @section Spin-glass:
#'   This is motivated by analogy to the Potts model in statistical physics.
#'   Each node can be in one of _k_ "spin states",
#'   and ties (particle interactions) provide information about which pairs of nodes 
#'   want similar or different spin states.
#'   The final community definitions are represented by the nodes' spin states
#'   after a number of updates.
#'   A different implementation than the default is used in the case of signed networks,
#'   such that nodes connected by negative ties will be more likely found in separate communities.
#' @references
#' ## On spinglass community detection
#' Reichardt, Jorg, and Stefan Bornholdt. 2006.
#' "Statistical Mechanics of Community Detection"
#' _Physical Review E_, 74(1): 016110–14.
#' \doi{10.1073/pnas.0605965104}
#' 
#' Traag, Vincent A., and Jeroen Bruggeman. 2009.
#' "Community detection in networks with positive and negative links".
#' _Physical Review E_, 80(3): 036115.
#' \doi{10.1103/PhysRevE.80.036115}
#' @examples
#' node_in_spinglass(ison_adolescents)
#' @export
node_in_spinglass <- function(.data, max_k = 200, resolution = 1){
  .data <- manynet::expect_nodes(.data)
  if(!igraph::is_connected(.data)) # note manynet::is_connected will return false
    manynet::snet_unavailable("This algorithm only works for connected networks.",
                     "We suggest using `to_giant()`", 
                     "to select the largest component.") else {
      out <- igraph::cluster_spinglass(manynet::as_igraph(.data), 
                                       spins = max_k, gamma = resolution,
                                       implementation = ifelse(manynet::is_signed(.data), "neg", "orig")
      )$membership
      make_node_member(out, .data)
    }
}

#' @rdname member_community_non 
#' @section Fluid:
#'   The general idea is to observe how a discrete number of fluids interact, expand and contract, 
#'   in a non-homogenous environment, i.e. the network structure.
#'   Unlike the `{igraph}` implementation that this function wraps,
#'   this function iterates over all possible numbers of communities and returns the membership
#'   associated with the highest modularity.
#' @references
#' ## On fluid community detection
#' Parés Ferran, Dario Garcia Gasulla, Armand Vilalta, Jonatan Moreno, Eduard Ayguade, Jesus Labarta, Ulises Cortes, and Toyotaro Suzumura. 2018. 
#' "Fluid Communities: A Competitive, Scalable and Diverse Community Detection Algorithm". 
#' In: _Complex Networks & Their Applications VI_
#' Springer, 689: 229.
#' \doi{10.1007/978-3-319-72150-7_19}
#' @examples
#' node_in_fluid(ison_adolescents)
#' @export
node_in_fluid <- function(.data, k = NULL, Kmax = 8L) {
  .data <- manynet::expect_nodes(.data)
  k <- check_k(k, .data)
  .data <- manynet::as_igraph(.data)
  if (!igraph::is_connected(.data)) {
    manynet::snet_unavailable("This algorithm only works for connected networks.",
                     "We suggest using `to_giant()`", 
                     "to select the largest component.")
  } else {
    if(manynet::is_complex(.data)){
      manynet::snet_info("This algorithm only works for simple networks.", 
                      "Converting to simplex.")
      .data <- manynet::to_simplex(.data)
    }
    if(manynet::is_directed(.data)){
      manynet::snet_info("This algorithm only works for undirected networks.", 
                      "Converting to undirected")
      .data <- manynet::to_undirected(.data)
    }
    at_k <- function(no) igraph::membership(
      igraph::cluster_fluid_communities(.data, no.of.communities = no))
    memb <- apply_k(k, Kmax, .data, at_k = at_k, default = function(){
      mods <- vapply(seq_nodes(.data), function(x)
        igraph::modularity(.data, membership = igraph::membership(
          igraph::cluster_fluid_communities(.data, x))),
        FUN.VALUE = numeric(1))
      at_k(which.max(mods))
    })
    make_node_member(memb, .data)
  }
}

#' @rdname member_community_non 
#' @section Louvain:
#'   The general idea is to take a hierarchical approach to optimising the modularity criterion.
#'   Nodes begin in their own communities and are re-assigned in a local, greedy way:
#'   each node is moved to the community where it achieves the highest contribution to modularity.
#'   When no further modularity-increasing reassignments are possible, 
#'   the resulting communities are considered nodes (like a reduced graph),
#'   and the process continues.
#'   Where `k` is given, the resolution parameter is searched for the value
#'   that returns that number of communities, and `resolution` is ignored.
#' @references
#' ## On Louvain community detection
#' Blondel, Vincent, Jean-Loup Guillaume, Renaud Lambiotte, Etienne Lefebvre. 2008.
#' "Fast unfolding of communities in large networks",
#' _J. Stat. Mech._ P10008.
#' @examples
#' node_in_louvain(ison_adolescents)
#' @export
node_in_louvain <- function(.data, k = NULL, Kmax = 8L, resolution = 1){
  .data <- manynet::expect_nodes(.data)
  k <- check_k(k, .data)
  if(manynet::is_directed(.data)){
    manynet::snet_info("This algorithm only works for undirected networks.", 
              "Converting to undirected")
    .data <- manynet::to_undirected(.data)
  }
  gr <- manynet::as_igraph(.data)
  memb <- apply_k(k, Kmax, .data,
                  at_k = function(no) cut_res(igraph::cluster_louvain, gr, no),
                  default = function()
                    igraph::cluster_louvain(gr, resolution = resolution)$membership)
  make_node_member(memb, .data)
}

#' @rdname member_community_non 
#' @section Leiden:
#'   The general idea is to optimise the Constant Potts Model, 
#'   which does not suffer from the resolution limit, instead of modularity.
#'   As outlined in the `{igraph}` package, 
#'   the Constant Potts Model object function is:
#'   
#'   \deqn{\frac{1}{2m} \sum_{ij}(A_{ij}-\gamma n_i n_j)\delta(\sigma_i, \sigma_j)}
#'   
#'   where _m_ is the total tie weight, 
#'   \eqn{A_{ij}} is the tie weight between _i_ and _j_,
#'   \eqn{\gamma} is the so-called resolution parameter,
#'   \eqn{n_i} is the node weight of node _i_,
#'   and \eqn{\delta(\sigma_i, \sigma_j) = 1} if and only if
#'   _i_ and _j_ are in the same communities and 0 otherwise.
#'   Compared to the Louvain method, the Leiden algorithm additionally
#'   tries to avoid unconnected communities.
#'   Where `k` is given, the resolution parameter is searched for the value
#'   that returns that number of communities, and `resolution` is ignored.
#' @references
#' ## On Leiden community detection
#' Traag, Vincent A., Ludo Waltman, and Nees Jan van Eck. 2019. 
#' "From Louvain to Leiden: guaranteeing well-connected communities", 
#' _Scientific Reports_, 9(1):5233. 
#' \doi{10.1038/s41598-019-41695-z}
#' @examples
#' node_in_leiden(ison_adolescents)
#' @export
node_in_leiden <- function(.data, k = NULL, Kmax = 8L, resolution = 1){
  .data <- manynet::expect_nodes(.data)
  k <- check_k(k, .data)
  if(manynet::is_directed(.data)){
    manynet::snet_info("This algorithm only works for undirected networks.", 
              "Converting to undirected")
    .data <- manynet::to_undirected(.data)
  }
  if(is.null(k) && manynet::is_weighted(.data)){ # Traag resolution default
    n <- manynet::net_nodes(.data)
    resolution <- sum(manynet::tie_weights(.data))/(n*(n - 1)/2)
  }
  gr <- manynet::as_igraph(.data)
  memb <- apply_k(k, Kmax, .data,
                  at_k = function(no) cut_res(igraph::cluster_leiden, gr, no),
                  default = function()
                    igraph::cluster_leiden(gr, resolution = resolution)$membership)
  make_node_member(memb, .data)
}

#' @rdname member_community_non
#' @section Label propagation:
#'   Every node is initially given a unique label.
#'   Nodes are then visited in random order, each adopting whichever label is
#'   most frequent among its neighbours, until no node has a label that a
#'   majority of its neighbours does not share.
#'   Densely connected groups quickly converge on a common label,
#'   which is what makes the communities.
#'
#'   This is the fastest of the algorithms here, running in near-linear time,
#'   which makes it useful on large networks where the others are infeasible.
#'   The trade-off is that it is stochastic: because both the visiting order and
#'   ties between equally frequent labels are broken at random, repeated runs on
#'   the same network can return different partitions,
#'   and on sparse networks it may return a single community.
#'   Set a seed for reproducibility, or use `node_in_community()` to select
#'   among algorithms by modularity.
#'
#'   Where `k` is given, the algorithm becomes semi-supervised.
#'   The `k` nodes of highest degree are each given a distinct, fixed label,
#'   every other node starts with a label of its own,
#'   and propagation runs as normal.
#'   Seeding alone tends to leave more than `k` labels standing,
#'   so any surplus groups are then merged in the order that best preserves
#'   modularity, until exactly `k` communities remain.
#' @references
#' ## On label propagation community detection
#' Raghavan, Usha Nandini, Reka Albert, and Soundar Kumara. 2007.
#' "Near linear time algorithm to detect community structures in large-scale networks",
#' _Physical Review E_, 76(3):036106.
#' \doi{10.1103/PhysRevE.76.036106}
#' @examples
#' node_in_labels(ison_adolescents)
#' @export
node_in_labels <- function(.data, k = NULL, Kmax = 8L){
  .data <- manynet::expect_nodes(.data)
  k <- check_k(k, .data)
  if(manynet::is_directed(.data)){
    manynet::snet_info("This algorithm only works for undirected networks.",
              "Converting to undirected")
    .data <- manynet::to_undirected(.data)
  }
  gr <- manynet::as_igraph(.data)
  n <- manynet::net_nodes(.data)
  at_k <- function(no){
    if(no >= n) return(seq_len(n))
    seeds <- order(igraph::degree(gr), decreasing = TRUE)[seq_len(no)]
    init <- seq_len(n)
    init[seeds] <- seq_len(no)
    init[-seeds] <- (no + 1):n
    fixed <- rep(FALSE, n)
    fixed[seeds] <- TRUE
    memb <- suppressWarnings(igraph::cluster_label_prop(
      gr, initial = init, fixed = fixed)$membership)
    merge_to_k(.data, memb, no)
  }
  memb <- apply_k(k, Kmax, .data, at_k = at_k,
                  default = function() igraph::cluster_label_prop(gr)$membership)
  make_node_member(memb, .data)
}

# Hierarchical community clustering ####

#' Memberships in hierarchical communities
#' @name member_community_hier
#' @description
#'   These functions offer algorithms for hierarchically clustering
#'   networks into communities. Since all of the following are hierarchical,
#'   their dendrograms can be plotted:
#' 
#'   - `node_in_betweenness()` is a hierarchical, decomposition algorithm
#'   where edges are removed in decreasing order of the number of
#'   shortest paths passing through the edge.
#'   - `node_in_greedy()` is a hierarchical, agglomerative algorithm, 
#'   that tries to optimize modularity in a greedy manner.
#'   - `node_in_eigen()` is a top-down, hierarchical algorithm.
#'   - `node_in_walktrap()` is a hierarchical, agglomerative algorithm based on random walks.
#'  
#'   The different algorithms offer various advantages in terms of computation time,
#'   availability on different types of networks, ability to maximise modularity,
#'   and their logic or domain of inspiration.
#'   
#' @template param_data
#' @template param_k
#' @template node_member
#' @family community
NULL

#' @rdname member_community_hier 
#' @section Edge-betweenness:
#'   This is motivated by the idea that edges connecting different groups 
#'   are more likely to lie on multiple shortest paths when they are the 
#'   only option to go from one group to another. 
#'   This method yields good results but is very slow because of 
#'   the computational complexity of edge-betweenness calculations and 
#'   the betweenness scores have to be re-calculated after every edge removal. 
#'   Networks of ~700 nodes and ~3500 ties are around the upper size limit 
#'   that are feasible with this approach. 
#' @references
#' ## On edge-betweenness community detection
#' Newman, Mark, and Michelle Girvan. 2004.
#' "Finding and evaluating community structure in networks." 
#' _Physical Review E_ 69: 026113.
#' \doi{10.1103/PhysRevE.69.026113}
#' @examples
#' node_in_betweenness(ison_adolescents)
#' @export
node_in_betweenness <- function(.data, k = NULL, Kmax = 8L){
  .data <- manynet::expect_nodes(.data)
  k <- check_k(k, .data)
  if(manynet::net_nodes(.data)>100) 
    manynet::snet_warn("This algorithm may take some time", 
                                "or even run out of memory on such a large network.")
  clust <- suppressWarnings(igraph::cluster_edge_betweenness(
    manynet::as_igraph(.data)))
  memb <- apply_k(k, Kmax, .data,
                  at_k = function(no) cut_tree(clust, no),
                  default = function() clust$membership)
  out <- make_node_member(memb, .data)
  attr(out, "hc") <- stats::as.hclust(clust, 
                                      use.modularity = igraph::is_connected(.data))
  attr(out, "k") <- length(unique(memb))
  out
}

#' @rdname member_community_hier 
#' @section Fast-greedy:
#'   Initially, each node is assigned a separate community.
#'   Communities are then merged iteratively such that each merge
#'   yields the largest increase in the current value of modularity,
#'   until no further increases to the modularity are possible.
#'   The method is fast and recommended as a first approximation 
#'   because it has no parameters to tune. 
#'   However, it is known to suffer from a resolution limit.
#' @references
#' ## On fast-greedy community detection
#' Clauset, Aaron, Mark E.J. Newman, and Cristopher Moore. 2004.
#' "Finding community structure in very large networks."
#' _Physical Review E_, 70: 066111.
#' \doi{10.1103/PhysRevE.70.066111}
#' @examples
#' node_in_greedy(ison_adolescents)
#' @export
node_in_greedy <- function(.data, k = NULL, Kmax = 8L){
  .data <- manynet::expect_nodes(.data)
  k <- check_k(k, .data)
  clust <- igraph::cluster_fast_greedy(manynet::to_undirected(manynet::as_igraph(.data)))
  memb <- apply_k(k, Kmax, .data,
                  at_k = function(no) cut_tree(clust, no),
                  default = function() clust$membership)
  out <- make_node_member(memb, .data)
  attr(out, "hc") <- stats::as.hclust(clust, 
                                      use.modularity = igraph::is_connected(.data))
  attr(out, "k") <- length(unique(memb))
  out
}

#' @rdname member_community_hier 
#' @section Leading eigenvector:
#'   In each step, the network is bifurcated such that modularity increases most.
#'   The splits are determined according to the leading eigenvector of the modularity matrix.
#'   A stopping condition prevents tightly connected groups from being split further.
#'   Note that due to the eigenvector calculations involved,
#'   this algorithm will perform poorly on degenerate networks,
#'   but will likely obtain a higher modularity than fast-greedy (at some cost of speed).
#' @references
#' ## On leading eigenvector community detection
#' Newman, Mark E.J. 2006.
#' "Finding community structure using the eigenvectors of matrices"
#' _Physical Review E_ 74:036104.
#' \doi{10.1103/PhysRevE.74.036104}
#' @examples
#' node_in_eigen(ison_adolescents)
#' @export
node_in_eigen <- function(.data, k = NULL, Kmax = 8L){
  .data <- manynet::expect_nodes(.data)
  k <- check_k(k, .data)
  if(manynet::is_directed(.data)){
    manynet::snet_info("This algorithm only works for undirected networks.", 
              "Converting to undirected")
    .data <- manynet::to_undirected(.data)
  }
  clust <- igraph::cluster_leading_eigen(manynet::as_igraph(.data))
  memb <- apply_k(k, Kmax, .data,
                  at_k = function(no) cut_tree(clust, no),
                  default = function() clust$membership)
  out <- make_node_member(memb, .data)
  attr(out, "hc") <- stats::as.hclust(clust)
  attr(out, "k") <- length(unique(memb))
  out
}

#' @rdname member_community_hier 
#' @section Walktrap:
#'   The general idea is that random walks on a network are more likely to stay 
#'   within the same community because few edges lead outside a community.
#'   By repeating random walks of 4 steps many times,
#'   information about the hierarchical merging of communities is collected.
#' @param steps Integer indicating the length of the random walks.
#'   By default `steps = 4`, as in `{igraph}`.
#'   Longer walks reach further and tend to return fewer, larger communities.
#' @references
#' ## On walktrap community detection
#' Pons, Pascal, and Matthieu Latapy. 2005.
#' "Computing communities in large networks using random walks".
#' 1-20.
#' \doi{10.48550/arXiv.physics/0512106}
#' @examples
#' node_in_walktrap(ison_adolescents)
#' @export
node_in_walktrap <- function(.data, k = NULL, Kmax = 8L, steps = 4){
  .data <- manynet::expect_nodes(.data)
  k <- check_k(k, .data)
  clust <- igraph::cluster_walktrap(manynet::as_igraph(.data), steps = steps)
  memb <- apply_k(k, Kmax, .data,
                  at_k = function(no) cut_tree(clust, no),
                  default = function() clust$membership)
  out <- make_node_member(memb, .data)
  attr(out, "hc") <- stats::as.hclust(clust, 
                                      use.modularity = igraph::is_connected(.data))
  attr(out, "k") <- length(unique(memb))
  out
}

