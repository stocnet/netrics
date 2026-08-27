# Marking core ####

#' Marking nodes as core or periphery
#' @name mark_core
#' @description
#'   `node_is_core()` identifies whether nodes belong to the core of the 
#'   network, as opposed to the periphery.
#' 
#' @template param_data
#' @family core-periphery
#' @template node_mark
#' @template param_coreness
#' @param centrality Deprecated; use `coreness` instead.
NULL

#' @rdname mark_core
#' @section Core-periphery: 
#'   This function is used to identify which nodes should belong to the core,
#'   and which to the periphery.
#'   It seeks to minimize the following quantity:
#'   \deqn{Z(S_1) = \sum_{(i<j)\in S_1} \textbf{I}_{\{A_{ij}=0\}} + \sum_{(i<j)\notin S_1} \textbf{I}_{\{A_{ij}=1\}}}
#'   where nodes \eqn{\{i,j,...,n\}} are ordered in descending coreness,
#'   \eqn{A} is the adjacency matrix,
#'   and the indicator function is 1 if the predicate is true or 0 otherwise.
#'   Note that minimising this quantity maximises density in the core block
#'   and minimises density in the periphery block;
#'   it ignores ties between these blocks.
#'
#'   Which ordering the nodes are swept in depends on the method named by
#'   `coreness`, for which see [method_coreness].
#' @references
#' ## On core-periphery partitioning
#' Borgatti, Stephen P., and Martin G. Everett. 2000. 
#' "Models of core/periphery structures". 
#' _Social Networks_, 21(4), 375-395. 
#' \doi{10.1016/S0378-8733(99)00019-2}
#' 
#' Lip, Sean Z. W. 2011. 
#' "A fast algorithm for the discrete core/periphery bipartitioning problem".
#' \doi{10.48550/arXiv.1102.5511}
#' @examples 
#' node_is_core(ison_adolescents)
#' ison_adolescents |> 
#'    mutate(corep = node_is_core())
#' @export
node_is_core <- function(.data, centrality = c("degree", "eigenvector")){
  .data <- manynet::expect_nodes(.data)
  centrality <- match.arg(centrality)
  if(manynet::is_directed(.data)) warning("Asymmetric core-periphery not yet implemented.")
  if(centrality == "degree"){
    degi <- node_by_degree(.data, normalized = FALSE, 
                           alpha = ifelse(manynet::is_weighted(.data), 1, 0))
  } else if (centrality == "eigenvector") {
    degi <- node_by_eigenvector(.data, normalized = FALSE)
  } else manynet::snet_abort("This function expects either 'degree' or 'eigenvector' method to be specified.")
  nord <- order(degi, decreasing = TRUE)
  zbest <- manynet::net_nodes(.data)*3
  kbest <- 0
  z <- 1/2*sum(degi)
  for(k in 1:(manynet::net_nodes(.data)-1)){
    z <- z + k - 1 - degi[nord][k]
    if(z < zbest){
      zbest <- z
      kbest <- k
    }
  }
  out <- ifelse(seq_len(manynet::net_nodes(.data)) %in% nord[seq_len(kbest)],
                1,2)
  make_node_mark(out==1, .data)
}

# Measuring core ####

#' Measuring nodes' coreness
#' @name measure_core
#' @description
#'   These functions identify nodes belonging to (some level of) the core of a network:
#'   
#'   - `node_by_core()` returns a continuous measure of how closely each node
#'   resembles a typical core node.
#'   - `node_by_kcoreness()` assigns nodes to their level of k-coreness.
#' 
#' @template param_data
#' @family core-periphery
#' @template node_measure
NULL

#' @rdname measure_core
#' @section k-coreness:
#'   k-coreness captures the maximal subgraphs in which each vertex has at least
#'   degree _k_, where _k_ is also the order of the subgraph.
#'   As described in `igraph::coreness`,
#'   a node's coreness is _k_ if it belongs to the _k_-core
#'   but not to the (_k_+1)-core.
#' @references
#' ## On k-coreness
#' Seidman, Stephen B. 1983. 
#' "Network structure and minimum degree". 
#' _Social Networks_, 5(3), 269-287.
#' \doi{10.1016/0378-8733(83)90028-X}
#' 
#' Batagelj, Vladimir, and Matjaz Zaversnik. 2003. 
#' "An O(m) algorithm for cores decomposition of networks". 
#' _arXiv preprint_ cs/0310049.
#' \doi{10.48550/arXiv.cs/0310049}
#' @examples
#' node_by_kcoreness(ison_adolescents)
#' @export
node_by_kcoreness <- function(.data){
  .data <- manynet::expect_nodes(.data)
  if(!manynet::is_graph(.data)) .data <- manynet::as_igraph(.data)
  out <- igraph::coreness(.data)
  make_node_measure(out, .data, measure = "k-coreness", range = c(0, Inf),
                    normalization = "none")
}

#' @rdname measure_core
#' @template param_coreness
#' @section Coreness:
#'   Where `node_is_core()` forces a yes or no answer,
#'   `node_by_core()` grades how core-like each node is on a scale from
#'   0 to 1. The two agree on which method to use and read the same
#'   `coreness` and `direction` arguments, so the mark is always the cut of
#'   the measure returned here.
#'
#'   Each method uses as much of the network as it can. The rich-core and hub
#'   methods read tie weights and tie direction directly. The correlation and
#'   transition methods compare the network against a symmetric ideal, so they
#'   symmetrise a directed network and say that they have done so.
#'   To keep a method from using a property, transform the network first with
#'   e.g. [manynet::to_undirected()] or [manynet::to_unweighted()].
#'
#'   This function was called `node_by_coreness()` prior to version 1.0.0.
#'   It is now named for the property, as `node_is_core()` and `node_in_core()`
#'   are, which also frees "coreness" from meaning two different things: the
#'   continuous score here, and the peeling depth of `node_by_kcoreness()`.
#' @examples
#' node_by_core(ison_adolescents)
#' node_by_core(ison_networkers, direction = "out")
#' @export
node_by_core <- function(.data, coreness = NULL,
                         direction = c("all","out","in")) {
  .data <- manynet::expect_nodes(.data)
  direction <- match.arg(direction)
  coreness <- check_coreness(.data, coreness)
  out <- run_coreness(.data, coreness, direction)
  make_node_measure(out$coreness, .data, measure = "coreness", range = c(0, 1),
                    normalization = "scaled", variant = coreness)
}

# Membering core ####

#' Memberships in core-periphery categories
#' @name member_core
#' @description
#'   `node_in_core()` categorizes nodes into two or more core/periphery
#'   categories based on their coreness.
#' 
#' @template param_data
#' @family core-periphery
#' @template node_member
NULL

#' @rdname member_core
#' @param groups Number of categories to create. Must be at least 2 and at most
#'   the number of nodes in the network. Default is 3.
#' @param cluster_by Method to use to create the categories.
#'   One of "bins" (equal-width bins), "quantiles" (quantile-based bins),
#'   or "kmeans" (k-means clustering). Default is "bins".
#' @section Core-periphery categories:
#'   This function categorizes nodes based on their coreness into a specified
#'   number of groups. The groups are labeled as "Core", "Semi-core",
#'   "Semi-periphery", and "Periphery" depending on the number of groups
#'   specified.
#'   The categorization can be done using different methods: equal-width bins,
#'   quantile-based bins, or k-means clustering.
#' @references
#' ## On core-periphery categorization
#' Wallerstein, Immanuel. 1974.
#' "Dependence in an Interdependent World: The Limited Possibilities of Transformation Within the Capitalist World Economy."
#' _African Studies Review_, 17(1), 1-26.
#' \doi{https://doi.org/10.2307/523574}
#' @examples
#' node_in_core(ison_adolescents)
#' @export
node_in_core <- function(.data, groups = 3,
                         cluster_by = c("bins","quantiles","kmeans")) {
  if (groups < 2) manynet::snet_abort("Number of categories must be at least 2")
  if (groups > manynet::net_nodes(.data)) manynet::snet_abort("There cannot be more categories than nodes.")
  .data <- manynet::expect_nodes(.data)
  contin <- node_by_coreness(.data)
  cluster_by <- match.arg(cluster_by)
  out <- switch(cluster_by,
                bins = cut(as.numeric(contin), breaks = groups, labels = FALSE),
                quantiles = as.numeric(cut(as.numeric(contin),
                                           breaks = stats::quantile(as.numeric(contin),
                                                                    probs = seq(0, 1, length.out = groups + 1)),
                                           include.lowest = TRUE, labels = FALSE)),
                kmeans = stats::kmeans(as.numeric(contin), centers = groups)$cluster
  )
  
  if (groups == 2) core_labels <- c("Core", "Periphery")
  if (groups == 3) core_labels <- c("Core", "Semi-periphery", "Periphery")
  if (groups == 4) core_labels <- c("Core", "Semi-core", "Semi-periphery", "Periphery")
  if (groups >= 5){
    n_middle <- groups - 2
    middle <- character(n_middle)
    
    for (i in seq_len(n_middle)) {
      if (i %% 2 == 1) {
        middle[i] <- paste0("Semi-periphery-", (i + 1) %/% 2)
      } else {
        middle[i] <- paste0("Semi-core-", i %/% 2)
      }
    }
    middle <- middle[order(middle)]
    
    core_labels <- c("Core", middle, "Periphery")
    if(groups == 5) core_labels[2] <- "Semi-core"
  } 
  out <- rev(core_labels)[out]
  make_node_member(out, .data)
}