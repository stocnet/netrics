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
#' @examples
#' net_by_waves(ison_monks)
#' @export
net_by_waves <- function(.data){
  .data <- manynet::expect_nodes(.data)
  # A longitudinal network holds its waves in a `wave` or a `time` tie
  # attribute, and `manynet::net_waves()` reads both since manynet 2.3.0.
  # A changing network counts its changelist instead.
  tie_waves <- manynet::net_waves(.data)
  if(manynet::is_changing(.data)){
    chltime <- manynet::as_changelist(.data)$time
    chg_waves <- (max(chltime)+1) - max(min(chltime)-1, 0)
  } else chg_waves <- 1
  make_network_measure(max(tie_waves, chg_waves),
                       .data, call = deparse(sys.call()),
                       measure = "waves", range = c(1, Inf),
                       normalization = "none")
}

# Change motifs ####

#' Motifs of network change
#' @name motif_periods
#' @description
#'   These functions measure certain topological features of networks:
#'   
#'   - `net_x_change()` measures the Hamming distance between two or more networks.
#'   - `net_x_stability()` measures the Jaccard index of stability between two or more networks.
#'   - `net_x_correlation()` measures the product-moment correlation between two or more networks.
#' 
#'   These `net_*()` functions return a numeric vector the length of the number
#'   of networks minus one. E.g., the periods between waves.
#'   Cells that are missing in either network of a pair are left out.
#' @template param_data
#' @family change
#' @template net_motif
NULL

# The networks to compare in consecutive pairs: the two given, or the waves of
# a longitudinal network.
.as_periods <- function(.data, object2){
  net <- manynet::expect_nodes(.data)
  if(!missing(object2)){
    net <- list(net, object2)
  } else if(manynet::is_longitudinal(net)){
    net <- manynet::to_waves(net)
  }
  if(!manynet::is_list(net))
    manynet::snet_abort("`.data` must be a list of networks or a second network must be provided.")
  net
}

#' @rdname motif_periods 
#' @param object2 A network object.
#' @examples
#' net_x_change(ison_monks)
#' net_x_correlation(ison_monks)
#' @export
net_x_change <- function(.data, object2){
  net <- .as_periods(.data, object2)
  periods <- length(net)-1
  out <- vapply(seq.int(periods), function(x){
    net1 <- manynet::as_matrix(net[[x]])
    net2 <- manynet::as_matrix(net[[x+1]])
    sum(net1 != net2, na.rm = TRUE)
  }, FUN.VALUE = numeric(1))
  make_network_motif(out, .data)
}

#' @rdname motif_periods 
#' @export
net_x_stability <- function(.data, object2){
  net <- .as_periods(.data, object2)
  periods <- length(net)-1
  out <- vapply(seq.int(periods), function(x){
    net1 <- manynet::as_matrix(net[[x]])
    net2 <- manynet::as_matrix(net[[x+1]])
    # ties are counted as present or absent, over the possible ties only
    keep <- .valid_cells(net[[x]], net1) & !is.na(net1) & !is.na(net2)
    net1 <- net1[keep] != 0
    net2 <- net2[keep] != 0
    n11 <- sum(net1 & net2)
    n01 <- sum(!net1 & net2)
    n10 <- sum(net1 & !net2)
    # two networks without any ties are identical
    if(n11 + n01 + n10 == 0) 1 else n11 / (n01 + n10 + n11)
  }, FUN.VALUE = numeric(1))
  make_network_motif(out, .data)
}

#' @rdname motif_periods 
#' @export
net_x_correlation <- function(.data, object2){
  net <- .as_periods(.data, object2)
  periods <- length(net)-1
  out <- vapply(seq.int(periods), function(x){
    net1 <- manynet::as_matrix(net[[x]])
    net2 <- manynet::as_matrix(net[[x+1]])
    if(!identical(dim(net1), dim(net2)))
      manynet::snet_abort("The networks must be of the same dimensions.")
    # correlated over the possible ties only
    keep <- .valid_cells(net[[x]], net1)
    stats::cor(net1[keep], net2[keep], use = "complete.obs")
  }, FUN.VALUE = numeric(1))
  make_network_motif(out, .data)
}
