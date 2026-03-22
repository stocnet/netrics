# Change measures ####

#' Measures of network change
#' @name measure_periods
#' @description
#'   `net_by_waves()` measures the number of waves in longitudinal network data.
#' 
#' @template param_data
#' @family change
#' @template net_measure
NULL

#' @rdname measure_periods 
#' @export
net_by_waves <- function(.data){
  .data <- manynet::expect_nodes(.data)
  tie_waves <- length(unique(manynet::tie_attribute(.data, "wave")))
  if(manynet::is_changing(.data)){
    chltime <- manynet::as_changelist(.data)$time
    chg_waves <- (max(chltime)+1) - max(min(chltime)-1, 0)
  } else chg_waves <- 1
  make_network_measure(max(tie_waves, chg_waves),
                       .data, call = deparse(sys.call()))
}

# Change motifs ####

#' Motifs of network change
#' @name motif_periods
#' @description
#'   These functions measure certain topological features of networks:
#'   
#'   - `net_x_change()` measures the Hamming distance between two or more networks.
#'   - `net_x_stability()` measures the Jaccard index of stability between two or more networks.
#'   - `net_x_correlation()` measures the product-moment correlation between two networks.
#' 
#'   These `net_*()` functions return a numeric vector the length of the number
#'   of networks minus one. E.g., the periods between waves.
#' @template param_data
#' @family change
#' @template net_motif
NULL

#' @rdname motif_periods 
#' @param object2 A network object.
#' @export
net_x_change <- function(.data, object2){
  net <- manynet::expect_nodes(.data)
  if(!missing(object2)){
    net <- list(net, object2)
  } else if(manynet::is_longitudinal(net)){
    net <- manynet::to_waves(net)
  }
  if(!manynet::is_list(net))
    manynet::snet_abort("`.data` must be a list of networks or a second network must be provided.")
  periods <- length(net)-1
  out <- vapply(seq.int(periods), function(x){
    net1 <- manynet::as_matrix(net[[x]])
    net2 <- manynet::as_matrix(net[[x+1]])
    sum(net1 != net2)
  }, FUN.VALUE = numeric(1))
  make_network_motif(out, .data)
}

#' @rdname motif_periods 
#' @export
net_x_stability <- function(.data, object2){
  net <- manynet::expect_nodes(.data)
  if(!missing(object2)){
    net <- list(net, object2)
  } else if(manynet::is_longitudinal(net)){
    net <- manynet::to_waves(net)
  }
  if(!manynet::is_list(net))
    manynet::snet_abort("`.data` must be a list of networks or a second network must be provided.")
  periods <- length(net)-1
  out <- vapply(seq.int(periods), function(x){
    net1 <- manynet::as_matrix(net[[x]])
    net2 <- manynet::as_matrix(net[[x+1]])
    n11 <- sum(net1 * net2)
    n01 <- sum(net1==0 * net2)
    n10 <- sum(net1 * net2==0)
    n11 / (n01 + n10 + n11)
  }, FUN.VALUE = numeric(1))
  make_network_motif(out, .data)
}

#' @rdname motif_periods 
#' @export
net_x_correlation <- function(.data, object2){
  .data <- manynet::expect_nodes(.data)
  comp1 <- manynet::as_matrix(.data)
  comp2 <- manynet::as_matrix(object2)
  if(!manynet::is_complex(.data)){
    diag(comp1) <- NA
  }
  if(!manynet::is_directed(.data)){
    comp1[upper.tri(comp1)] <- NA
  }
  out <- cor(c(comp1), c(comp2), use = "complete.obs")
  make_network_motif(out, .data)
}