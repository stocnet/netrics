#' Methods for selecting clusters
#' 
#' @description
#'   Finding the optimal number of clusters is generally a balance between
#'   optimal fit statistics, parsimony, and interpretability.
#'   These functions help select the number of clusters to return from `hc`,
#'   some hierarchical clustering object:
#'   
#'   - `k_strict()` selects a number of clusters in which there is no
#'   distance between cluster members.
#'   - `k_elbow()` selects a number of clusters in which there is 
#'   a fair trade-off between parsimony and fit according to the elbow method.
#'   - `k_silhouette()` selects a number of clusters that
#'   maximises the silhouette score.
#' 
#'   These functions are generally not user-facing but used internally
#'   in e.g. the `*_equivalence()` functions.
#'   
#' @inheritParams mark_nodes
#' @param hc A hierarchical clustering object.
#' @returns
#'   A single integer indicating the number of clusters to return.
#' @name method_kselect
NULL

#' @rdname method_kselect 
#' @section Strict method:
#'   The strict method selects the number of clusters in which there is no 
#'   distance between cluster members. 
#'   This is a very conservative method that may be appropriate when the goal is 
#'   to identify clusters of nodes that are exactly the same. 
#'   However, it may not be appropriate in cases where the data is noisy or 
#'   when the clusters are not well-defined, 
#'   as it may result in a large number of small clusters.
#' @export
k_strict <- function(hc, .data){
  zero_merged <- hc$merge[round(hc$height,4) == 0,]
  k <- nrow(zero_merged) + manynet::net_nodes(.data) - sum(zero_merged < 0) + sum(zero_merged > 0)
  k
}

#' @rdname method_kselect 
#' @param motif A motif census object.
#' @param Kmax An integer indicating the maximum number of options to consider.
#'   The minimum of this and the number of nodes in the network is used.
#' @section Elbow method:
#'   The elbow method is a heuristic used in cluster analysis to determine the optimal number of clusters.
#'   It is based on the idea of plotting the within cluster correlation
#'   as a function of the number of clusters and looking for an "elbow"
#'   where there is a significant decrease in the rate of improvement in 
#'   correlation as the number of clusters increases.
#'   The point at which the elbow occurs is often considered a good choice for 
#'   the number  of clusters, as it represents a balance between 
#'   model complexity and fit to the data.
#' @references 
#' ## On the elbow method
#'  Thorndike, Robert L. 1953. 
#'    "Who Belongs in the Family?". 
#'    _Psychometrika_, 18(4): 267–76. 
#'    \doi{10.1007/BF02289263}.
#' @export
k_elbow <- function(hc, .data, motif, Kmax){
  
  thisRequires("sna")
  
  clusterCorr <- function(observed_cor_matrix, cluster_vector) {
    num_vertices = nrow(observed_cor_matrix)
    cluster_cor_mat <- observed_cor_matrix
    
    obycor <- function(i, j) 
      mean(observed_cor_matrix[which(cluster_vector[row(observed_cor_matrix)] ==
                                       cluster_vector[i] &
                                       cluster_vector[col(observed_cor_matrix)] ==
                                       cluster_vector[j])])
    obycor_v <- Vectorize(obycor)
    cluster_cor_mat <- outer(1:num_vertices,
                             1:num_vertices,
                             obycor_v)
    dimnames(cluster_cor_mat) <- dimnames(observed_cor_matrix)
    cluster_cor_mat
  }
  
  elbow_finder <- function(x_values, y_values) {
    # Max values to create line
    if(min(x_values)==1) x_values <- x_values[2:length(x_values)]
    if(min(y_values)==0) y_values <- y_values[2:length(y_values)]
    max_df <- data.frame(x = c(min(x_values), max(x_values)), 
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
  
  vertices <- manynet::net_nodes(.data)
  observedcorrelation <- cor(t(motif))
  
  resultlist <- list()
  correlations <- vector()
  for (i in 2:min(Kmax, vertices)) {
    cluster_result <- list(label = NA, clusters = NA, correlation = NA)
    cluster_result$label <- paste("number of clusters: ", 
                                  i)
    clusters <- stats::cutree(hc, k = i)
    cluster_result$clusters <- clusters
    cluster_cor_mat <- clusterCorr(observedcorrelation, clusters)
    clustered_observed_cors <- sna::gcor(cluster_cor_mat, observedcorrelation)
    cluster_result$correlation <- (clustered_observed_cors)
    resultlist <- c(resultlist, cluster_result)
    correlations <- c(correlations, clustered_observed_cors)
  }
  
  resultlist$correlations <- c(correlations)
  dafr <- data.frame(clusters = 2:min(Kmax, vertices), 
                     correlations = c(correlations))
  correct <- NULL # to satisfy the error god
  
  # k identification method
  elbow_finder(dafr$clusters, dafr$correlations)
}

#' @rdname method_kselect 
#' @section Silhouette method: 
#'   The silhouette method is based on the concept of cohesion and separation. 
#'   Cohesion refers to how closely related the nodes within a cluster are, 
#'   while separation refers to how distinct the clusters are from each other. 
#'   The silhouette score combines these two concepts into a single metric that 
#'   can be used to evaluate the quality of a clustering solution. 
#'   The silhouette score is calculated as follows:
#'   For each node, calculate the average distance to all other nodes in the same cluster (a) 
#'   and the average distance to all other nodes in the next nearest cluster (b). 
#'   The silhouette score for each node is then calculated as:
#'   \deqn{S(i) = \frac{b - a}{\max(a, b)}}
#'   A higher silhouette score indicates that the node is well-matched to its 
#'   own cluster and poorly matched to neighboring clusters.
#'   The silhouette score for the entire clustering is the average silhouette score across all nodes. 
#'   Maximizing the silhouette score across a range of potential clusterings 
#'   allows researchers to identify the number of clusters that best captures 
#'   the underlying structure of the data.
#'   It is particularly useful when the clusters are well-separated.
#' @references 
#' ## On the silhouette method
#' Rousseeuw, Peter J. 1987. 
#'   “Silhouettes: A Graphical Aid to the Interpretation and Validation of Cluster Analysis.” 
#'   _Journal of Computational and Applied Mathematics_, 20: 53–65. 
#'   \doi{10.1016/0377-0427(87)90125-7}.
#' @export
k_silhouette <- function(hc, .data, Kmax){
  if(missing(Kmax)) Kmax <- length(hc$order) else
    Kmax <- min(Kmax, length(hc$order))
  kcs <- 2:min(Kmax, manynet::net_nodes(.data))
  ns <- seq_len(manynet::net_nodes(.data))
  distances <- hc$distances
  ks <- vector()
  for(kc in kcs){
    cand <- stats::cutree(hc, kc)
    ai <- vector()
    bi <- vector()
    for(i in ns){
      wig <- which(cand == cand[i])
      wig <- wig[wig != i]
      ai <- c(ai, 
              ifelse(length(wig)==0,
                     0, mean(as.matrix(distances)[i, wig])))
      wog <- which(cand != cand[i])
      bi <- c(bi, min(vapply(unique(cand[wog]), function(b){
        mean(as.matrix(distances)[i, wog[cand[wog]==b]])
      }, FUN.VALUE = numeric(1))))
    }
    si <- (bi - ai)/
      apply(data.frame(ai, bi), 1, max)
    ks <- c(ks, mean(si))
  }
  k <- which(ks == max(ks)) + 1
  k
}

#' @rdname method_kselect
#' @param sims Integer of how many simulations should be generated as a
#'   reference distribution.
#' @export
k_gap <- function(hc, motif, Kmax, sims = 100) {
  
  if(missing(Kmax)) Kmax <- length(hc$order) else
    Kmax <- min(Kmax, length(hc$order))
  
  # --- helper: within-cluster dispersion Wk ---
  within_disp <- function(motif, clusters) {
    sum(sapply(split(motif, clusters), function(group) {
      group <- matrix(group, ncol = ncol(motif))
      if (nrow(group) <= 1) return(0)
      center <- colMeans(group)
      sum(rowSums((group - center)^2))
    }))
  }
  
  n <- nrow(motif)
  p <- ncol(motif)
  
  # bounding box for reference datasets
  mins <- apply(motif, 2, min)
  maxs <- apply(motif, 2, max)
  
  # storage
  logW <- numeric(Kmax)
  logW_ref <- matrix(0, nrow = sims, ncol = Kmax)
  
  # --- real data W_k ---
  for (k in 1:Kmax) {
    cl <- cutree(hc, k)
    logW[k] <- log(within_disp(motif, cl))
  }
  
  # --- reference datasets ---
  for (b in 1:sims) {
    ref <- matrix(stats::runif(n * p, mins, maxs), nrow = n)
    d_ref <- stats::dist(ref)
    hc_ref <- hclust(d_ref, method = hc$method)
    
    for (k in 1:Kmax) {
      cl_ref <- cutree(hc_ref, k)
      logW_ref[b, k] <- log(within_disp(ref, cl_ref))
    }
  }
  
  # --- gap statistic ---
  gap <- colMeans(logW_ref) - logW
  se  <- sqrt(1 + 1/B) * apply(logW_ref, 2, stats::sd)
  
  # --- Tibshirani 1-SE rule ---
  k <- which(gap[-Kmax] >= gap[-1] - se[-1])[1]
  k
}

