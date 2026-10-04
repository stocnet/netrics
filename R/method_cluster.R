#' Methods for equivalence clustering
#' 
#' @description
#'   These functions are used to cluster some motif census object:
#'   
#'   - `cluster_hierarchical()` returns a hierarchical clustering object
#'   created by `stats::hclust()` on the proximities between nodes' profiles.
#'   - `cluster_concor()` returns a hierarchical clustering object
#'   created from a convergence of correlations procedure (CONCOR).
#'   - `cluster_cosine()` is deprecated;
#'   use `cluster_hierarchical()` with `proximity = "cosine"` instead.
#' 
#'   These functions are not intended to be called directly,
#'   but are called within `node_in_equivalence()` and related functions.
#'   They are exported and listed here to provide more detailed documentation.
#' @name method_cluster
#' @inheritParams member_equivalence
#' @returns 
#'   A hierarchical clustering object created by `stats::hclust()`,
#'   with an additional `distances` element containing the distance matrix 
#'   used for clustering and, for `cluster_hierarchical()`,
#'   a `proximity` element containing the node-by-node proximity matrix
#'   those distances were made from.
NULL

# How alike each pair of nodes' profiles are. The measures all live in
# `manynet::to_proximity()`; `dyad = "include"` reads a square census as the
# profile matrix it is and not as a one-mode network.
# "asis" passes on a matrix that is already a node-by-node similarity.
.proximity <- function(motif, proximity = "pearson"){
  motif <- unclass(as.matrix(motif))
  attr(motif, "mode") <- NULL
  if(proximity == "asis") return(motif)
  if(proximity == "correlation") proximity <- "pearson"
  # A node whose profile does not vary has no correlation with any other;
  # `to_proximity()` reports that as 0, so the warning adds nothing.
  out <- withCallingHandlers(
    manynet::to_proximity(motif, similarity = proximity,
                          across = "rows", dyad = "include"),
    warning = function(w) 
      if(grepl("standard deviation is zero", conditionMessage(w)))
        invokeRestart("muffleWarning"))
  dimnames(out) <- list(rownames(motif), rownames(motif))
  out
}

# `1 - P` only works as a dissimilarity where `P` does not exceed 1.
# Counts, covariances and the like are unbounded, and `hclust()` would take
# the negative dissimilarities silently and return negative merge heights.
.as_dissimilarity <- function(P){
  top <- max(P[upper.tri(P) | lower.tri(P)], na.rm = TRUE)
  if(top > 1){
    manynet::snet_info("This proximity is unbounded, so dissimilarities are",
                       "the largest proximity less each proximity.")
    out <- top - P
  } else out <- 1 - P
  diag(out) <- 0
  out
}

#' @rdname method_cluster
#' @section Hierarchical clustering:
#'  This method uses `stats::hclust()` to create a hierarchical clustering object
#'  from the proximities between nodes' profiles in the given motif census.
#'  First a matrix of how alike each pair of nodes' profiles are is created
#'  using [manynet::to_proximity()],
#'  by default their Pearson correlation coefficients.
#'  Then a dissimilarity matrix is created by subtracting these proximities 
#'  from `1`, and this is given to `stats::hclust()` to enable 
#'  dendrogram construction etc.
#'  Each pair of nodes is thus compared once.
#'  
#'  Where `distance` is given, the nodes are compared a second time:
#'  `stats::dist()` measures the distance between each pair of nodes' 
#'  profiles of dissimilarities to all nodes, 
#'  and it is these distances that are clustered.
#'  This was the only behaviour before v1.1.0.
#'  
#'  Some proximities, such as `"count"`, `"match"`, `"crossmin"`,
#'  `"maxcrossmin"`, and `"covariance"`, are not bounded by 1.
#'  These are subtracted from the largest proximity observed instead,
#'  with a message, so that no dissimilarity is negative.
#' @export
cluster_hierarchical <- function(motif, distance = NULL, proximity = "pearson"){
  proximities <- .proximity(motif, proximity)
  dissimilarity <- .as_dissimilarity(proximities)
  distances <- if(is.null(distance)) stats::as.dist(dissimilarity) else
    stats::dist(dissimilarity, method = distance)
  hc <- stats::hclust(distances)
  hc$distances <- distances
  hc$proximity <- proximities
  hc
}

#' @rdname method_cluster
#' @export
cluster_cosine <- function(motif, distance = NULL){
  warning("`cluster_cosine()` is deprecated. ",
          "Please use `cluster_hierarchical()` with `proximity = \"cosine\"` ",
          "instead.", call. = FALSE)
  cluster_hierarchical(motif, distance, proximity = "cosine")
}

# cluster_concor(ison_adolescents)
# cluster_concor(ison_southern_women)
# https://github.com/bwlewis/hclust_in_R/blob/master/hc.R

#' @rdname method_cluster 
#' @section CONCOR:
#'   First a matrix of Pearson correlation coefficients between each pair of 
#'   nodes' profiles in the given motif census is created. 
#'   Then, again, we find the correlations of this square, symmetric matrix,
#'   and continue to do this iteratively until each entry is either `1` or `-1`.
#'   These values are used to split the data into two partitions,
#'   with members either holding the values `1` or `-1`.
#'   This procedure from census to convergence is then repeated within each block,
#'   allowing further partitions to be found.
#'   Unlike UCINET, partitions are continued until there are single members in
#'   each partition.
#'   Then a distance matrix is constructed from records of in which partition phase
#'   nodes were separated, 
#'   and this is given to `stats::hclust()` so that dendrograms etc can be returned.
#' @importFrom stats complete.cases
#' @references 
#' ## On CONCOR clustering
#' Breiger, Ronald L., Scott A. Boorman, and Phipps Arabie. 1975.  
#'   "An Algorithm for Clustering Relational Data with Applications to 
#'   Social Network Analysis and Comparison with Multidimensional Scaling". 
#'   _Journal of Mathematical Psychology_, 12: 328-83.
#'   \doi{10.1016/0022-2496(75)90028-0}.
#' @export
cluster_concor <- function(.data, motif){
  .data <- manynet::expect_nodes(.data)
  split_cor <- function(m0, cutoff = 1) {
    if (ncol(m0) < 2 | all(manynet::to_correlation(m0)==1)) list(m0)
    else {
      mi <- manynet::to_correlation(m0)
      while (any(abs(mi) <= cutoff)) {
        mi <- stats::cor(mi)
        cutoff <- cutoff - 0.0001
      }
      group <- mi[, 1] > 0
      if(all(group)){
       list(m0) 
      } else {
        list(m0[, group, drop = FALSE], 
           m0[, !group, drop = FALSE])
      }
    }
  }
  p_list <- list(t(motif))
  if(is.null(colnames(p_list[[1]]))) 
    colnames(p_list[[1]]) <- paste0("V",1:ncol(p_list[[1]]))
  p_group <- list()
  if(manynet::is_twomode(.data)){
    p_list <- list(p_list[[1]][, !manynet::node_is_mode(.data), drop = FALSE],
                   p_list[[1]][, manynet::node_is_mode(.data), drop = FALSE])
    p_group[[1]] <- lapply(p_list, function(z) colnames(z))
    i <- 2
  } else i <- 1
  while(!all(vapply(p_list, function(x) ncol(x)==1, logical(1)))){
    p_list <- unlist(lapply(p_list,
                            function(y) split_cor(y)),
                     recursive = FALSE)
    p_group[[i]] <- lapply(p_list, function(z) colnames(z))
    if(i > 2 && length(p_group[[i]]) == length(p_group[[i-1]])) break
    i <- i+1
  }
  
  if(manynet::is_labelled(.data)){
    merges <- sapply(rev(1:(i-1)), 
                     function(p) lapply(p_group[[p]], 
                                        function(s){
                                          g <- match(s, manynet::node_names(.data))
                                          if(length(g)==1) c(g, 0, p) else 
                                            if(length(g)==2) c(g, p) else
                                              c(t(cbind(t(utils::combn(g, 2)), p)))
                                        } ))
  } else {
    merges <- sapply(rev(1:(i-1)), 
                     function(p) lapply(p_group[[p]], 
                                        function(s){
                                          g <- as.numeric(gsub("^V","",s))
                                          if(length(g)==1) c(g, 0, p) else 
                                            if(length(g)==2) c(g, p) else
                                              c(t(cbind(t(utils::combn(g, 2)), p)))
                                        } ))
  }
  merges <- c(merges, 
              list(c(t(cbind(t(utils::combn(seq_len(manynet::net_nodes(.data)), 2)), 0)))))
  merged <- matrix(unlist(merges), ncol = 3, byrow = TRUE)
  merged <- merged[!duplicated(merged[,1:2]),]
  merged[,3] <- abs(merged[,3] - max(merged[,3]))
  merged[merged == 0] <- NA
  merged <- merged[stats::complete.cases(merged),]
  merged <- as.data.frame(merged)
  names(merged) <- c("from","to","weight")
  
  distances <- manynet::as_matrix(manynet::as_igraph(merged))
  distances <- distances + t(distances)
  # distances <- distances[-which(rownames(distances)==0),-which(colnames(distances)==0)]
  if(manynet::is_labelled(.data))
    rownames(distances) <- colnames(distances) <- manynet::node_names(.data)
  hc <- hclust(d = as.dist(distances))
  hc$method <- "concor"
  hc$distances <- distances
  hc  
}
