# net_diffusion ####

#' Measures of network diffusion
#' @template param_attr_diff
#' @name measure_diffusion_net
#' @description
#'   These functions allow measurement of various features of
#'   a diffusion process at the network level:
#'   
#'   - `net_by_transmissibility()` measures the average transmissibility observed
#'   in a diffusion simulation, or the number of new infections over
#'   the number of susceptible nodes.
#'   - `net_by_recovery()` measures the average number of time steps 
#'   nodes remain infected once they become infected.
#'   - `net_by_reproduction()` estimates the basic reproduction number
#'   from the chances of infection and recovery in a diffusion,
#'   and from the structure of the network.
#'   - `net_by_immunity()` measures the proportion of nodes that would need
#'   to be protected through vaccination, isolation, or recovery for herd immunity to be reached.
#'   
#' @param .data Network data with nodal changes,
#'   as created by `play_diffusion()`,
#'   or a valid network diffusion model,
#'   as created by `as_diffusion()`.
#' @template param_norm
#' @template net_measure
#' @family diffusion
#' @examples
#'   smeg <- generate_smallworld(15, 0.025)
#'   smeg_diff <- play_diffusion(smeg, recovery = 0.2)
#'   plot(smeg_diff)
#' @references
#' ## On epidemiological models
#'   Kermack, William O., and Anderson Gray McKendrick. 1927. 
#'   "A contribution to the mathematical theory of epidemics". 
#'   _Proc. R. Soc. London A_ 115: 700-721.
#'   \doi{10.1098/rspa.1927.0118}
NULL

#' @rdname measure_diffusion_net 
#' @section Transmissibility: 
#'   `net_transmissibility()` measures how many directly susceptible nodes
#'   each infected node will infect in each time period, on average.
#'   That is:
#'   \deqn{T = \frac{1}{n}\sum_{j=1}^n \frac{i_{j}}{s_{j}}}
#'   where \eqn{i} is the number of new infections in each time period, \eqn{j \in n},
#'   and \eqn{s} is the number of nodes that could have been infected in that time period
#'   (note that \eqn{s \neq S}, or 
#'   the number of nodes that are susceptible in the population).
#'   \eqn{T} can be interpreted as the proportion of susceptible nodes that are
#'   infected at each time period.
#' @examples
#'   # To calculate the average transmissibility for a given diffusion model
#'   net_by_transmissibility(smeg_diff)
#' @export
net_by_transmissibility <- function(.data, attribute = "diffusion"){
  diff_model <- manynet::as_diffusion(.data, attribute = attribute)
  out <- diff_model$I_new/diff_model$s
  out <- out[-1]
  out <- out[!is.infinite(out)]
  out <- out[!is.nan(out)]
  if(inherits(.data, "diff_model")) 
    net <- attr(.data, "network") else 
      net <- .data
  # Read as a proportion, but the at-risk denominator `s` is recorded at the
  # end of each period rather than the start, so a period in which more nodes
  # were infected than were left at risk can push the ratio above 1. Declared
  # open above rather than claiming a bound the values can break.
  make_network_measure(mean(out, na.rm = TRUE),
                       net,
                       call = deparse(sys.call()),
                       measure = "transmissibility", range = c(0, Inf),
                       normalization = "none")
}

#' @rdname measure_diffusion_net 
#' @param censor Where some nodes have not yet recovered by the end
#'   of the simulation, right censored values can be replaced by the number of steps. 
#'   By default TRUE.
#'   Note that this will likely still underestimate recovery.
#' @section Recovery time: 
#'   `net_recovery()` measures the average number of time steps that
#'   nodes in a network remain infected.
#'   Note that in a diffusion model without recovery, average infection length
#'   will be infinite.
#'   This will also be the case where there is right censoring.
#'   The longer nodes remain infected, the longer they can infect others.
#' @examples
#'   # To calculate the average infection length for a given diffusion model
#'   net_by_recovery(smeg_diff)
#' @export
net_by_recovery <- function(.data, censor = TRUE, attribute = "diffusion"){
  diff_model <- manynet::as_diffusion(.data, attribute = attribute)
  recovs <- node_by_adopt_recovery(.data, attribute = attribute)
  if(censor && any(!is.infinite(recovs) & !is.na(recovs)))
    recovs[is.infinite(recovs)] <- nrow(diff_model)
  if(inherits(.data, "diff_model")) 
    net <- attr(.data, "network") else 
      net <- .data
  make_network_measure(mean(recovs, na.rm = TRUE),
                       net,
                       call = deparse(sys.call()),
                       measure = "recovery time", range = c(0, Inf),
                       normalization = "none")
}

#' @rdname measure_diffusion_net 
#' @section Reproduction number: 
#'   `net_by_reproduction()` estimates the basic reproduction number
#'   \eqn{R_0} of a diffusion: the number of nodes that one typical case
#'   infects when all other nodes are susceptible.
#'   On a network this is
#'   \deqn{R_0 = T \kappa}
#'   where \eqn{T} is the chance that an infectious node infects a susceptible
#'   neighbour before it recovers,
#'   and \eqn{\kappa} is the mean number of other neighbours of a node
#'   that a tie reaches.
#'   So the estimate separates the contagion, \eqn{T}, from the network,
#'   \eqn{\kappa}.
#'
#'   \eqn{T} comes from two chances per step, which are estimated from the
#'   states of the nodes in each step:
#'   \eqn{\hat\tau}, the share of the exposed susceptible nodes that are
#'   infected, and \eqn{\hat\gamma}, the share of the infectious nodes that
#'   recover or die.
#'   A node is exposed when it has a tie from a node that is infectious at
#'   the start and the end of the step.
#'   As in `manynet::play_diffusion()`, a node recovers before it can infect
#'   in a step, so
#'   \deqn{T = \frac{(1 - \gamma) \tau}{\gamma + \tau - \gamma \tau}}
#'   With no recovery, \eqn{T = 1}.
#'   Where no susceptible node was exposed, for example where the seeds
#'   recover in the first step, \eqn{\tau} cannot be estimated,
#'   and the result is `NA` with a warning.
#'   A small outbreak gives a noisy estimate, so where possible pool several
#'   diffusions or use more seeds.
#'
#'   \eqn{\kappa} is the mean excess degree of the network of the nodes that
#'   are not recovered or dead at the start,
#'   \eqn{\sum_i k_i (k_i - 1) / \sum_i k_i},
#'   or \eqn{\sum_i k^{in}_i k^{out}_i / \sum_i k^{in}_i} on a directed network.
#'   It is larger than the mean degree where the degrees are unequal,
#'   because a tie is more likely to reach a node with many ties.
#'
#'   The estimate assumes a simple contagion:
#'   a threshold of 1, with ties that expose the node they go to
#'   (`direction = "in"`), and a chance of infection that does not grow with
#'   the number of infectious neighbours.
#'   It also assumes that the degrees are not correlated, as in a random
#'   network; in a clustered network, such as a lattice, it is too high,
#'   because the neighbours of a case share neighbours.
#'   A two-mode network is not yet available.
#'
#'   Interpretation of the reproduction number is oriented around R = 1.
#'   Where \eqn{R > 1}, the 'disease' will 'infect' more and more
#'   nodes in the network.
#'   Where \eqn{R < 1}, the 'disease' will not sustain itself and eventually
#'   die out.
#'   Where \eqn{R = 1}, the 'disease' will continue as endemic,
#'   if conditions allow.
#' @references
#' ## On the basic reproduction number
#' Diekmann, Odo, Hans J.A.P. Heesterbeek, and Hans J.A.J. Metz. 1990. 
#' "On the definition and the computation of the basic reproduction ratio R0 in models for infectious diseases in heterogeneous populations". 
#' _Journal of Mathematical Biology_, 28(4): 365–82.
#' \doi{10.1007/BF00178324}
#' 
#' Kenah, Eben, and James M. Robins. 2007. 
#' "Second look at the spread of epidemics on networks". 
#' _Physical Review E_, 76(3 Pt 2): 036113.
#' \doi{10.1103/PhysRevE.76.036113}
#'
#' Newman, Mark E.J. 2002.
#' "Spread of epidemic disease on networks".
#' _Physical Review E_, 66(1): 016128.
#' \doi{10.1103/PhysRevE.66.016128}
#' @examples
#'   # To calculate the reproduction number for a given diffusion model
#'   net_by_reproduction(smeg_diff)
#' @export
net_by_reproduction <- function(.data, attribute = "diffusion"){
  parts <- .reproduction_parts(.data, attribute)
  make_network_measure(parts$transmission * parts$excess, parts$net,
                       call = deparse(sys.call()),
                       measure = "reproduction number", range = c(0, Inf),
                       normalization = "none")
}

# The parts of R0 = T * kappa from a played diffusion:
# tau and gamma from the states in each step, T from them, and kappa from
# the network of the nodes that are not removed at the start.
.reproduction_parts <- function(.data, attribute = "diffusion"){
  st <- .diffusion_states(.data, attribute = attribute)
  net <- st$net
  if(manynet::is_twomode(net))
    manynet::snet_unavailable("The reproduction number needs a one-mode network.")
  A <- manynet::as_matrix(manynet::to_unweighted(net))
  dimnames(A) <- NULL
  A <- A != 0
  diag(A) <- FALSE
  if(!manynet::is_directed(net)) A <- A | t(A)
  S <- cbind(st$before, st$states)
  is <- function(r) matrix(S %in% r, nrow(S))
  sus <- is("S"); inf <- is("I"); rem <- is(c("R", "D"))
  exposed <- infected <- infectious <- removed <- 0
  for(k in seq_len(ncol(S) - 1)){
    src <- inf[, k] & inf[, k + 1]
    risk <- sus[, k] & as.numeric(crossprod(A, src)) > 0
    exposed <- exposed + sum(risk)
    infected <- infected + sum(risk & !sus[, k + 1])
    infectious <- infectious + sum(inf[, k])
    removed <- removed + sum(inf[, k] & rem[, k + 1])
  }
  if(exposed == 0)
    manynet::snet_warn("No susceptible node was exposed in this diffusion,",
                       "so the chance of infection cannot be estimated.")
  tau <- `if`(exposed > 0, infected / exposed, NA_real_)
  gamma <- `if`(infectious > 0, removed / infectious, 0)
  trans <- `if`(isTRUE(tau == 0), 0,
                (1 - gamma) * tau / (gamma + tau - gamma * tau))
  keep <- !(S[, 1] %in% c("R", "D"))
  As <- A[keep, keep, drop = FALSE]
  if(manynet::is_directed(net)){
    kin <- colSums(As); kout <- rowSums(As)
    excess <- `if`(sum(kin) > 0, sum(kin * kout) / sum(kin), 0)
  } else {
    k <- rowSums(As)
    excess <- `if`(sum(k) > 0, sum(k * (k - 1)) / sum(k), 0)
  }
  list(net = net, tau = tau, gamma = gamma, transmission = trans,
       excess = excess)
}

#' @rdname measure_diffusion_net 
#' @section Herd immunity: 
#'   `net_immunity()` estimates the proportion of a network
#'   that need to be protected from infection for herd immunity
#'   to be achieved.
#'   This is known as the Herd Immunity Threshold or HIT:
#'   \deqn{1 - \frac{1}{R}}
#'   where \eqn{R} is the reproduction number from `net_by_reproduction()`.
#'   The HIT indicates the threshold at which
#'   the reduction of susceptible members of the network means
#'   that infections will no longer keep increasing.
#'   Note that there may still be more infections after this threshold has been reached,
#'   but there should be fewer and fewer.
#'   These excess infections are called the _overshoot_.
#'   With \eqn{R} from `net_by_reproduction()`, which uses the structure of
#'   the network, this is the share of nodes to protect at random.
#'   Protecting the nodes with the most ties needs fewer.
#'   
#'   Interpretation is quite straightforward.
#'   A HIT or immunity score of 0.75 would mean that 75% of the nodes in the network
#'   would need to be vaccinated or otherwise protected to achieve herd immunity.
#'   To identify how many nodes this would be, multiply this proportion with the number
#'   of nodes in the network.
#'
#'   Where \eqn{R < 1} the diffusion is already sub-critical and dies out of its
#'   own accord, so no one needs protecting and the threshold is reported as 0.
#'   The formula would otherwise return a negative proportion, which has no
#'   interpretation.
#' @references
#' ## On herd immunity
#' Garnett, G.P. 2005.
#' "Role of herd immunity in determining the effect of vaccines against sexually transmitted disease".
#' _The Journal of Infectious Diseases_, 191(1): S97-106.
#' \doi{10.1086/425271}
#' @examples
#'   # Calculating the proportion required to achieve herd immunity
#'   net_by_immunity(smeg_diff)
#'   # To find the number of nodes to be vaccinated
#'   net_by_immunity(smeg_diff, normalized = FALSE)
#' @export
net_by_immunity <- function(.data, normalized = TRUE, attribute = "diffusion"){
  # diff_model <- as_diffusion(.data)
  if(inherits(.data, "diff_model")) 
    net <- attr(.data, "network") else 
      net <- .data
  # Below the epidemic threshold the formula turns negative; no one needs
  # protecting from a diffusion that cannot sustain itself.
  out <- max(1 - 1/net_by_reproduction(.data, attribute = attribute), 0)
  if(!normalized) out <- ceiling(out * manynet::net_nodes(net))
  make_network_measure(out, net,
                       call = deparse(sys.call()),
                       measure = "herd immunity threshold",
                       range = `if`(normalized, c(0, 1), c(0, Inf)),
                       normalization = `if`(normalized, "normalized", "none"))
}

# net_infection ####

#' Measures of network infection
#' @template param_attr_diff
#' @name measure_diffusion_infection
#' @description
#'   These functions allow measurement of various features of
#'   a diffusion process at the network level:
#'   
#'   - `net_by_infection_complete()` measures the number of time steps until
#'   (the first instance of) complete infection.
#'   For diffusions that are not observed to complete,
#'   this function returns the value of `Inf` (infinity).
#'   This makes sure that at least ordinality is respected.
#'   - `net_by_infection_total()` measures the proportion or total number of nodes
#'   that are infected/activated at some time by the end of the diffusion process.
#'   This includes nodes that subsequently recover.
#'   Where reinfection is possible, the proportion may be higher than 1.
#'   - `net_by_infection_peak()` measures the number of time steps until the
#'   highest infection rate is observed.
#'   
#' @inheritParams measure_diffusion_net
#' @family diffusion
#' @template net_measure

#' @rdname measure_diffusion_infection 
#' @examples
#'   smeg <- generate_smallworld(15, 0.025)
#'   smeg_diff <- play_diffusion(smeg)
#'   net_by_infection_complete(smeg_diff)
#' @export
net_by_infection_complete <- function(.data, attribute = "diffusion"){
  diff_model <- manynet::as_diffusion(.data, attribute = attribute)
  out <- which(diff_model$I == diff_model$n)[1]
  if(is.na(out)) out <- Inf
  if(inherits(.data, "diff_model")) 
    net <- attr(.data, "network") else 
      net <- .data
  make_network_measure(out, net,
                       call = deparse(sys.call()),
                       measure = "time to complete infection",
                       range = c(1, Inf), normalization = "none")
}

#' @rdname measure_diffusion_infection
#' @examples
#'   net_by_infection_total(smeg_diff)
#' @export
net_by_infection_total <- function(.data, normalized = TRUE,
                                   attribute = "diffusion"){
  # Normalised against the number of nodes rather than a theoretical maximum:
  # where reinfection is possible a node can be counted more than once, so the
  # proportion is not capped at 1, and the declared range stays open above.
  states <- .diffusion_states(.data, attribute)
  infected <- matrix(states$states %in% "I", nrow(states$states))
  # each move into I, and the seeds infected at the start
  before <- cbind(FALSE, infected[, -ncol(infected), drop = FALSE])
  out <- sum(infected & !before)
  if(normalized) out <- out / manynet::net_nodes(states$net)
  make_network_measure(out, states$net,
                       call = deparse(sys.call()),
                       measure = "total infections", range = c(0, Inf),
                       normalization = `if`(normalized, "normalized", "none"))
}

#' @rdname measure_diffusion_infection 
#' @examples
#'   net_by_infection_peak(smeg_diff)
#' @export
net_by_infection_peak <- function(.data, attribute = "diffusion"){
  diff_model <- manynet::as_diffusion(.data, attribute = attribute)
  if(inherits(.data, "diff_model")) 
    net <- attr(.data, "network") else 
      net <- .data
  out <- which(diff_model$I_new == max(diff_model$I_new))[1]
  make_network_measure(out, net,
                       call = deparse(sys.call()),
                       measure = "time to peak infection",
                       range = c(1, Inf), normalization = "none")
}

# node_diffusion ####

#' Measures of nodes in a diffusion
#' @template param_attr_diff
#' @name measure_diffusion_node
#' @description
#'   These functions allow measurement of various features of
#'   a diffusion process:
#'   
#'   - `node_by_adopt_time()`: Measures the number of time steps until
#'   nodes adopt/become infected
#'   - `node_by_adopt_threshold()`: Measures nodes' thresholds from the amount
#'   of exposure they had when they became infected
#'   - `node_by_adopt_recovery()`: Measures the average length nodes that become
#'   infected remain infected in a compartmental model with recovery
#'   - `node_by_adopt_exposure()`: Measures how many exposures nodes have to 
#'   a given mark
#'   
#' @template param_data
#' @template param_norm
#' @family diffusion
#' @template node_measure
#' @examples
#'   smeg <- generate_smallworld(15, 0.025)
#'   smeg_diff <- play_diffusion(smeg, recovery = 0.2)
#'   plot(smeg_diff)
#' @references
#' ## On diffusion measures
#'   Valente, Tom W. 1995. _Network models of the diffusion of innovations_
#'   (2nd ed.). Cresskill N.J.: Hampton Press.
NULL

#' @rdname measure_diffusion_node 
#' @section Adoption time: 
#'   `node_by_adopt_time()` measures the time units it took 
#'   until each node became infected.
#'   Note that an adoption time of 0 indicates that this was a seed node.
#' @examples
#'   # To measure when nodes adopted a diffusion/were infected
#'   (times <- node_by_adopt_time(smeg_diff))
#' @export
node_by_adopt_time <- function(.data, attribute = "diffusion"){
  states <- .diffusion_states(.data, attribute)
  net <- states$net
  first <- .state_first(states, "I")
  out <- rep(Inf, length(first))
  out[!is.na(first)] <- as.numeric(states$times[first[!is.na(first)]])
  if(manynet::is_labelled(net)) names(out) <- manynet::node_names(net)
  make_node_measure(out, net, measure = "adoption time", range = c(0, Inf),
                    normalization = "none")
}

#' @rdname measure_diffusion_node 
#' @param lag The number of time steps back upon which the thresholds are
#'   inferred.
#'   By default `lag = 1`, which counts only the alters that adopted
#'   before the node did.
#'   `lag = 0` also counts the alters that adopted at the same time step.
#' @param direction Character string, "in" counts the exposure that arrives
#'   along ties from adopters to the node,
#'   "out" along ties from the node to adopters,
#'   and "all" along both.
#'   By default "in", so contagion travels in the direction of the arcs.
#'   Undirected networks give the same result for every direction.
#' @section Thresholds:
#'   `node_by_adopt_threshold()` infers nodes' thresholds based on how much
#'   exposure they had when they were infected.
#'   This inference is of course imperfect,
#'   especially where there is a sudden increase in exposure,
#'   but it can be used heuristically.
#'   In a threshold model, 
#'   nodes activate when \eqn{\sum_{j:\text{active}} w_{ji} \geq \theta_i},
#'   where \eqn{w} is some (potentially weighted) matrix,
#'   \eqn{j} are some already activated nodes,
#'   and \eqn{theta} is some pre-defined threshold value.
#'   Where a fractional threshold is used, the equation is 
#'   \eqn{\frac{\sum_{j:\text{active}} w_{ji}}{\sum_{j} w_{ji}} \geq \theta_i}.
#'   That is, \eqn{theta} is now a proportion,
#'   and works regardless of whether \eqn{w} is weighted or not.
#'   The `normalized` thresholds divide by the in-, out-, or total degree
#'   (or strength, where the network is weighted) that matches `direction`.
#'   Nodes with no ties in that direction that adopt have a threshold of 0.
#'
#'   Seeds have a missing threshold,
#'   since their adoption says nothing about their exposure,
#'   and so they do not bring down an average threshold.
#'   Nodes that never adopt have an infinite threshold,
#'   since their threshold was never reached.
#'   `lag = 0, direction = "out"` reproduces
#'   `netdiffuseR::threshold(valued = TRUE)`, and `lag = 1, direction = "in"`
#'   reproduces `netdiffuseR::threshold(lags = 1, outgoing = FALSE, valued = TRUE)`,
#'   except that `{netdiffuseR}` reports non-adopters as missing
#'   and gives seeds a value where there is no lag,
#'   and counts cumulative rather than current adopters.
#'   By default, `{netdiffuseR}` ignores tie weights,
#'   so the results differ from it on weighted networks.
#' @examples
#'   # To infer nodes' thresholds
#'   node_by_adopt_threshold(smeg_diff)
#' @export
node_by_adopt_threshold <- function(.data, normalized = TRUE, lag = 1,
                                    direction = c("in", "out", "all"),
                                    attribute = "diffusion"){
  direction <- match.arg(direction)
  states <- .diffusion_states(.data, attribute)
  net <- states$net
  n <- manynet::net_nodes(net)
  # nodes adopt when they become latent, or infected where there is no latency
  first <- .state_first(states, c("E", "I"))
  mat <- .exposure_ties(net, !is.na(first))
  directed <- manynet::is_directed(net)
  out <- rep(Inf, n)
  for(k in unique(first[!is.na(first)])){
    adopters <- which(first == k)
    mk <- .state_at(states, states$times[k] - lag) %in% "I"
    # Adopters are not excluded from their own exposure here,
    # so that alters adopting at the same time step count where lag = 0
    out[adopters] <- .adopt_exposure(mat, mk, direction, directed)[adopters]
  }
  # the seeds, whose adoption says nothing about their exposure
  out[which(first == 1)] <- NA
  if(normalized){
    out <- out / .adopt_exposure(mat, rep(TRUE, n), direction, directed)
    out[is.nan(out)] <- 0
  }
  if(manynet::is_labelled(net)) names(out) <- manynet::node_names(net)
  make_node_measure(out, net, measure = "adoption threshold",
                    range = `if`(normalized, c(0, 1), c(0, Inf)),
                    normalization = `if`(normalized, "normalized", "none"))
}

#' @rdname measure_diffusion_node 
#' @section Recovery:
#'   `node_by_adopt_recovery()` measures the average length of time that nodes 
#'   that become infected remain infected in a compartmental model with recovery.
#'   Infections that are not concluded by the end of the study period are
#'   calculated as infinite.
#' @examples
#'   # To measure how long each node remains infected for
#'   node_by_adopt_recovery(smeg_diff)
#' @export
node_by_adopt_recovery <- function(.data, attribute = "diffusion"){
  states <- .diffusion_states(.data, attribute)
  times <- as.numeric(states$times)
  out <- apply(states$states, 1, function(role){
    infected <- role %in% "I"
    before <- c(FALSE, infected[-length(infected)])
    begins <- which(infected & !before)
    ends <- which(role %in% "R" & before)
    if(!length(begins)) return(NA_real_)
    if(length(ends) < length(begins)) return(Inf)
    mean(times[ends] - times[begins])
  })
  make_node_measure(out, states$net, measure = "recovery time", range = c(0, Inf),
                    normalization = "none")
}

#' @rdname measure_diffusion_node
#' @param mark A valid 'node_mark' object or
#'   logical vector (TRUE/FALSE) of length equal to 
#'   the number of nodes in the network.
#' @param time A time point until which infections/adoptions should be
#'   identified. By default `time = 0`.
#' @section Exposure:
#'   `node_exposure()` calculates the number of infected/adopting nodes
#'   to which each susceptible node is exposed.
#'   It usually expects network data and 
#'   an index or mark (TRUE/FALSE) vector of those nodes which are currently infected,
#'   but if a diff_model is supplied instead it will return
#'   nodes exposure at \eqn{t = 0}.
#'   Nodes in the mark have an exposure of 0.
#' @examples
#'   # To measure how much exposure nodes have to a given mark
#'   node_by_adopt_exposure(smeg, mark = c(1,3))
#'   node_by_adopt_exposure(smeg_diff)
#' @export
node_by_adopt_exposure <- function(.data, mark, time = 0,
                                   direction = c("in", "out", "all"),
                                   attribute = "diffusion"){
  direction <- match.arg(direction)
  if(missing(mark)){
    states <- .diffusion_states(.data, attribute)
    mark <- .state_at(states, time) %in% "I"
    .data <- states$net
  } else if(inherits(.data, "diff_model")) .data <- attr(.data, "network")
  .data <- manynet::expect_nodes(.data)
  .data <- .to_aggregated_css(.data)
  .data <- manynet::as_tidygraph(.data)
  if(is.numeric(mark)){
    mk <- rep(FALSE, manynet::net_nodes(.data))
    mk[mark] <- TRUE
  } else mk <- as.logical(mark)
  out <- .adopt_exposure(.exposure_ties(.data, mk), mk, direction,
                         manynet::is_directed(.data))
  out[mk] <- 0
  make_node_measure(out, .data, measure = "exposure", range = c(0, Inf),
                    normalization = "none")
}

# The states of a diffusion, as `manynet::as_diffusion()` reads them:
# the start state in `attribute`, then its changes at each time of the report,
# with the values in the roles "S", "E", "I", "R", or "D" of the compartments.
# Returns the network, the times, the roles before the first time,
# and the roles of each node (row) at each time (column).
.diffusion_states <- function(.data, attribute = "diffusion"){
  if(inherits(.data, "diff_model")){
    net <- attr(.data, "network")
    events <- summary(.data)
    events <- data.frame(time = events$t, node = events$nodes,
                         value = events$event)
    start <- rep("S", manynet::net_nodes(net))
    comps <- NULL
  } else {
    net <- manynet::expect_nodes(.data)
    events <- NULL
    if(manynet::is_changing(net)){
      events <- as.data.frame(manynet::as_changelist(net))
      events <- events[events$var == attribute, , drop = FALSE]
    }
    held <- attribute %in% manynet::net_node_attributes(net)
    if(!held && (is.null(events) || !nrow(events)))
      manynet::snet_abort(c("This network holds no diffusion in {.val {attribute}}.",
                            "i" = "Play one with {.fn play_diffusion}, or name its {.arg attribute}."))
    start <- `if`(held, manynet::node_attribute(net, attribute),
                  rep(NA, manynet::net_nodes(net)))
    comps <- `if`(inherits(net, "stocnet"), net$info$compartments,
                  igraph::graph_attr(manynet::as_igraph(net), "compartments"))
    if(is.null(comps) && is.logical(start)) comps <- list(S = FALSE, I = TRUE)
  }
  if(is.null(events))
    events <- data.frame(time = numeric(0), node = integer(0), value = character(0))
  value <- `if`(is.list(events$value), unlist(events$value), events$value)
  roles <- function(v){
    v <- as.character(v)
    if(is.null(comps)) return(v)
    out <- rep(NA_character_, length(v))
    for(r in names(comps)) out[v %in% as.character(comps[[r]])] <- r
    out
  }
  report <- manynet::as_diffusion(.data, attribute = attribute)
  times <- report[[`if`("time" %in% names(report), "time", "t")]]
  states <- matrix(NA_character_, length(start), length(times))
  now <- start
  for(k in seq_along(times)){
    due <- events$time <= times[k]
    if(k > 1) due <- due & events$time > times[k - 1]
    now[events$node[due]] <- value[due]
    states[, k] <- roles(now)
  }
  list(net = net, times = times, before = roles(start), states = states)
}

# The roles of the nodes at a time, as at the last time of the report
# before or at it
.state_at <- function(states, time){
  k <- which(states$times <= time)
  `if`(length(k), states$states[, max(k)], states$before)
}

# The first time (column) that each node holds one of the roles, or NA
.state_first <- function(states, roles){
  held <- matrix(states$states %in% roles, nrow(states$states))
  apply(held, 1, function(h) match(TRUE, h))
}

# A mark of the nodes in a role at a time
.diffusion_mark <- function(.data, time, attribute, role){
  states <- .diffusion_states(.data, attribute)
  make_node_mark(.state_at(states, time) %in% role, states$net)
}

# The square matrix of all the nodes along which exposure travels.
# A two-mode network that is not multilevel diffuses as `play_diffusion()`
# plays it: marks on one mode expose that mode through the nodes of the other
# mode that they share, and marks on both modes expose along their ties.
.exposure_ties <- function(.data, mark){
  mat <- as.matrix(manynet::as_matrix(.data))
  mat[is.na(mat)] <- 0
  if(!manynet::is_twomode(.data) || manynet::is_multilevel(.data)) return(mat)
  n1 <- nrow(mat)
  first <- seq_len(n1 + ncol(mat)) <= n1
  out <- matrix(0, length(first), length(first))
  if(any(mark) && (all(first[mark]) || !any(first[mark]))){
    on <- if(all(first[mark])) first else !first
    proj <- if(all(first[mark])) mat %*% t(mat) else t(mat) %*% mat
    if(all(mat %in% c(0, 1))) proj <- (proj > 0) * 1
    diag(proj) <- 0
    out[on, on] <- proj
  } else {
    out[first, !first] <- mat
    out[!first, first] <- t(mat)
  }
  out
}

# Sums the (weighted) ties between each node and the marked nodes
# in the given direction, including any tie a marked node has to itself
.adopt_exposure <- function(mat, mark, direction = "in", directed = TRUE){
  ins <- colSums(mat * mark)
  if(!directed || direction == "in") return(ins)
  outs <- rowSums(mat * rep(mark, each = nrow(mat)))
  `if`(direction == "out", outs, ins + outs)
}

# Diffusion membership ####

#' Memberships in a diffusion process
#' @description
#'   `node_in_adopter()` classifies membership of nodes into diffusion categories
#'   by where on the distribution of adopters they fell.
#'   Valente (1995) defines five memberships:
#'   
#'   - _Early adopter_: those with an adoption time less than 
#'   the average adoption time minus one standard deviation of adoptions times
#'   - _Early majority_: those with an adoption time between
#'   the average adoption time and 
#'   the average adoption time minus one standard deviation of adoptions times
#'   - _Late majority_: those with an adoption time between
#'   the average adoption time and 
#'   the average adoption time plus one standard deviation of adoptions times
#'   - _Laggard_: those with an adoption time greater than 
#'   the average adoption time plus one standard deviation of adoptions times
#'   - _Non-adopter_: those without an adoption time,
#'   i.e. never adopted
#'   
#' @template param_data
#' @template node_member
#' @family diffusion
#' @template param_attr_diff
#' @name member_diffusion
#' @references
#' ## On adopter classes
#'   Valente, Tom W. 1995. 
#'   _Network models of the diffusion of innovations_
#'   (2nd ed.). Cresskill N.J.: Hampton Press.
NULL

#' @rdname member_diffusion
#' @examples
#'   smeg <- generate_smallworld(15, 0.025)
#'   smeg_diff <- play_diffusion(smeg, recovery = 0.2)
#'   # To classify nodes by their position in the adoption curve
#'   (adopts <- node_in_adopter(smeg_diff))
#'   summary(adopts)
#' @export
node_in_adopter <- function(.data, attribute = "diffusion"){
  toa <- node_by_adopt_time(.data, attribute = attribute)
  toa[is.infinite(toa)] <- NA
  avg <- mean(toa, na.rm = TRUE)
  sdv <- stats::sd(toa, na.rm = TRUE)
  out <- ifelse(toa < (avg - sdv) | toa == 0, "Early Adopter", 
                ifelse(toa > (avg + sdv), "Laggard",
                       ifelse((avg - sdv) < toa & toa <= avg, "Early Majority", 
                              ifelse(avg < toa & toa <= avg + sdv, "Late Majority", 
                                     "Non-Adopter"))))
  out[is.na(out)] <- "Non-Adopter"
  if(inherits(.data, "diff_model")) 
    net <- attr(.data, "network") else 
      net <- manynet::expect_nodes(.data)
  make_node_member(out, net)
}
