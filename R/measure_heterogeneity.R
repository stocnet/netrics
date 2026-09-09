# Network diversity ####

#' Measures of network diversity
#' @name measure_diverse_net
#' @description
#'   These functions offer ways to measure the heterogeneity of an attribute
#'   across a network, within groups of a network, or the distribution of ties
#'   across this attribute:
#'   
#'   - `net_by_richness()` measures the number of unique categories 
#'   in a network attribute.
#'   - `net_by_diversity()` measures the heterogeneity of ties across a network.
#'   
#' @template param_data
#' @template param_attr
#' @family diversity
#' @template net_measure
#' @param diversity Which method to use for `*_diversity()`.
#'   Either "blau" (Blau's index) or "teachman" (Teachman's index) for categorical attributes, 
#'   or "variation" (coefficient of variation) or "gini" (Gini coefficient) for numeric attributes.
#'   Default is "blau". 
#'   If an incompatible method is chosen for the attribute type,
#'   a suitable alternative will be used instead with a message.
NULL

#' @rdname measure_diverse_net
#' @section Richness:
#'   Richness is a simple count of the number of different categories
#'   present for a given attribute.
#' @references
#' ## On richness
#'   Magurran, Anne E. 1988.
#'   _Ecological Diversity and Its Measurement_.
#'   Princeton: Princeton University Press.
#'   \doi{10.1007/978-94-015-7358-0}
#' @examples
#' net_by_richness(ison_networkers)
#' @export
net_by_richness <- function(.data, attribute){
  .data <- manynet::expect_nodes(.data)
  make_network_measure(length(unique(manynet::node_attribute(.data, attribute))),
                       .data, call = deparse(sys.call()),
                       measure = "richness", range = c(1, Inf),
                       normalization = "none")
}

#' @rdname measure_diverse_net 
#' @section Diversity:
#'    Blau's index (1977) uses a formula known also in other disciplines
#'    by other names 
#'    (Gini-Simpson Index, Gini impurity, Gini's diversity index,
#'    Gibbs-Martin index, and probability of interspecific encounter (PIE)):
#'    \deqn{1 - \sum\limits_{i = 1}^k {p_i^2 }}
#'    where \eqn{p_i} is the proportion of group members in \eqn{i}th category
#'    and \eqn{k} is the number of categories for an attribute of interest.
#'    This index can be interpreted as the probability that two members
#'    randomly selected from a group would be from different categories.
#'    This index finds its minimum value (0) when there is no variety,
#'    i.e. when all individuals are classified in the same category.
#'    The maximum value depends on the number of categories and
#'    whether nodes can be evenly distributed across categories.
#'
#'    The Herfindahl-Hirschman index (HHI) is the complement of Blau's index,
#'    \eqn{\sum\limits_{i = 1}^k {p_i^2}},
#'    and so `1 - net_by_diversity(.data, attribute)` returns it.
#'    Where shares are stated as percentages rather than proportions,
#'    as in the `{hhi}` package,
#'    the index runs from 0 to 10000 and equals `(1 - blau) * 10000`.
#'    Both readings hold for a categorical attribute,
#'    where a share is the frequency of a category,
#'    which is why neither index is offered for a numeric one.
#'
#'    Teachman's index (1980) is based on information theory
#'    and is calculated as:
#'    \deqn{- \sum\limits_{i = 1}^k {p_i \log(p_i)}}
#'    where \eqn{p_i} is the proportion of group members in \eqn{i}th category 
#'    and \eqn{k} is the number of categories for an attribute of interest.
#'    This index finds its minimum value (0) when there is no variety, 
#'    i.e. when all individuals are classified in the same category. 
#'    The maximum value depends on the number of categories and 
#'    whether nodes can be evenly distributed across categories.
#'    It thus shares similar properties to Blau's index,
#'    but includes also a notion of richness that tends to give more weight to 
#'    rare categories and thus tends to highlight imbalances more.
#'
#'    The coefficient of variation (CV) is a standardised measure of dispersion
#'    of a probability distribution or frequency distribution.
#'    It is defined as the ratio of the standard deviation \eqn{\sigma}
#'    to the mean \eqn{\mu}:
#'    \deqn{CV = \frac{\sigma}{\mu}}
#'    It is often expressed as a percentage.
#'    The CV is useful because the standard deviation of data must always be understood
#'    in the context of the mean of the data.
#'    The CV is particularly useful when comparing the degree of variation
#'    from one data series to another,
#'    even if the means are drastically different from each other.
#'
#'    The Gini coefficient is a measure of statistical dispersion
#'    that is intended to represent the income or wealth distribution
#'    of a nation's residents,
#'    and is commonly used as a measure of inequality.
#'    It is defined as a ratio with values between 0 and 1,
#'    where 0 corresponds with perfect equality
#'    (everyone has the same income) and 1 corresponds with perfect inequality
#'    (one person has all the income, and everyone else has zero income).
#'    The Gini coefficient can be calculated from the Lorenz curve,
#'    which plots the proportion of the total income of the population
#'    that is cumulatively earned by the bottom x% of the population.
#'    The Gini coefficient is defined as the area between the line of equality
#'    and the Lorenz curve,
#'    divided by the total area under the line of equality.
#' @references 
#' ## On diversity
#'   Blau, Peter M. 1977. 
#'   _Inequality and heterogeneity_. 
#'   New York: Free Press.
#'   
#'   Teachman, Jay D. 1980. 
#'   Analysis of population diversity: Measures of qualitative variation. 
#'   _Sociological Methods & Research_, 8:341-362.
#'   \doi{10.1177/004912418000800305}
#'   
#'   Page, Scott E. 2010.
#'   _Diversity and Complexity_.
#'   Princeton: Princeton University Press.
#'   \doi{10.1515/9781400835140}
#'
#'   Hirschman, Albert O. 1964.
#'   "The paternity of an index".
#'   _The American Economic Review_, 54(5): 761.
#' @examples
#' marvel_friends <- to_unsigned(to_uniplex(fict_marvel, "relationship"), "positive")
#' net_by_diversity(marvel_friends, "Gender")
#' net_by_diversity(marvel_friends, "Appearances")
#' @export
net_by_diversity <- function(.data, attribute, 
                        diversity = c("blau","teachman","variation","gini")){
  .data <- manynet::expect_nodes(.data)
  blau <- function(features) { 1 - sum((table(features)/length(features))^2) }
  teachman <- function(features) {
    p <- table(features)/length(features)
    -sum(p * log(p))
  }
  cv <- function(values) { 
    stats::sd(values, na.rm = TRUE) / mean(values, na.rm = TRUE) }
  gini <- function(values) {
    x <- sort(values)
    n <- length(x)
    G <- sum(x * (1:n))
    return((2 * G) / (n * sum(x)) - (n + 1) / n)
  }
  attr <- manynet::node_attribute(.data, attribute)
  diversity <- match.arg(diversity)
  if(is.numeric(attr) && diversity %in% c("blau","teachman")){
    manynet::snet_info("{.val {diversity}} index is not appropriate for numeric attributes.")
    manynet::snet_info("Using {.val variation} coefficient instead",
              "({.val gini} coefficient also available).")
    diversity <- "variation"
  }
  if(!is.numeric(attr) && diversity %in% c("variation","gini")){
    manynet::snet_info("{.val {diversity}} coefficient is not appropriate for categorical attributes.")
    manynet::snet_info("Using {.val blau} index instead",
              "({.val teachman} index also available).")
    diversity <- "blau"
  }
  
  out <- switch(diversity,
                blau = blau(attr),
                teachman = teachman(attr),
                variation = cv(attr),
                gini = gini(attr))
  meta <- .diversity_metadata(diversity)
  make_network_measure(out, .data, call = deparse(sys.call()),
                       measure = meta$measure, range = meta$range,
                       normalization = meta$normalization,
                       variant = meta$variant)
}

# Nodal diversity ####

#' Measures of nodes diversity
#' @name measure_diverse_node
#' @description
#'   These functions offer ways to measure the heterogeneity of an attribute
#'   across a network, within groups of a network, or the distribution of ties
#'   across this attribute:
#'   
#'   - `node_by_richness()` measures the number of unique categories 
#'   of an attribute to which each node is connected.
#'   - `node_by_diversity()` measures the heterogeneity of each node's
#'   local neighbourhood.
#'   
#' @template param_data
#' @template param_attr
#' @family diversity
#' @template node_measure
#' @inheritParams measure_diverse_net
NULL

#' @rdname measure_diverse_node 
#' @examples
#' node_by_richness(ison_networkers, "Discipline")
#' @export
node_by_richness <- function(.data, attribute){
  .data <- manynet::expect_nodes(.data)
  out <- vapply(manynet::to_egos(.data, min_dist = 1), 
                function(x) length(unique(manynet::node_attribute(x, attribute))),
                FUN.VALUE = numeric(1))
  # An isolate is connected to no categories at all, so unlike the whole
  # network's richness this can be 0.
  make_node_measure(out, .data, measure = "richness", range = c(0, Inf),
                    normalization = "none")
}

#' @rdname measure_diverse_node 
#' @examples 
#' marvel_friends <- to_unsigned(to_uniplex(fict_marvel, "relationship"), "positive")
#' node_by_diversity(marvel_friends, "Gender")
#' node_by_diversity(marvel_friends, "Attractive")
#' @export
node_by_diversity <- function(.data, attribute, 
                              diversity = c("blau","teachman","variation","gini")){
  .data <- manynet::expect_nodes(.data)
  attr <- manynet::node_attribute(.data, attribute)
  diversity <- match.arg(diversity)
  if(is.numeric(attr) && diversity %in% c("blau","teachman")){
    manynet::snet_info("{.val {diversity}} index is not appropriate for numeric attributes.")
    manynet::snet_info("Using {.val variation} coefficient instead",
                       "({.val gini} coefficient also available).")
    diversity <- "variation"
  }
  if(!is.numeric(attr) && diversity %in% c("variation","gini")){
    manynet::snet_info("{.val {diversity}} coefficient is not appropriate for categorical attributes.")
    manynet::snet_info("Using {.val blau} index instead",
                       "({.val teachman} index also available).")
    diversity <- "blau"
  }
  out <- vapply(igraph::ego(manynet::as_igraph(.data)),
                function(x) net_by_diversity(
                  igraph::induced_subgraph(manynet::as_igraph(.data), x),
                  attribute, diversity = diversity),
                FUN.VALUE = numeric(1))
  meta <- .diversity_metadata(diversity)
  make_node_measure(out, .data, measure = meta$measure, range = meta$range,
                    normalization = meta$normalization, variant = meta$variant)
}

# Network assortativity ####

#' Measures of network assortativity
#' @name measure_assort_net
#' @description
#'   These functions offer ways to measure the distribution or assortativity 
#'   of ties in a network:
#'   
#'   - `net_by_heterophily()` measures how embedded nodes in the network
#'   are within groups of nodes with the same attribute.
#'   - `net_by_homophily()` measures how embedded nodes in the network
#'   are within groups of nodes with the same attribute, but with more options for method.
#'   - `net_by_assortativity()` measures the degree assortativity in a network.
#'   - `net_by_spatial()` measures the spatial association/autocorrelation 
#'   (global Moran's I) in a network.
#'   
#'   Note that for two-mode networks, 
#'   homophily is calculated on the mode with the attribute of interest,
#'   and the other mode is ignored.
#'   If the attribute is present on both modes, 
#'   homophily is calculated on the first mode by default,
#'   but a message is given and the user can choose to calculate homophily on the
#'   other mode instead by subsetting the attribute vector and 
#'   converting the network to a one-mode network.
#' @template param_data
#' @template param_attr
#' @param assortativity Which method to use for `*_homophily()`.
#'   Either "ie" (negative E-I index), "ei" (E-I index), 
#'   "yule" (Yule's Q), or "geary" (Geary's C).
#'   Default is "ie".
#'   The E-I index is the number of ties between (or _external_) nodes
#'   grouped in some mutually exclusive categories
#'   minus the number of ties within (or _internal_) these groups
#'   divided by the total number of ties.
#'   This value can range from 1 to -1,
#'   where 1 indicates ties only between categories/groups and -1 ties only within categories/groups.
#'   
#'   Yule's Q is a measure of association for two binary variables, calculated as:
#'   \deqn{Q = \frac{ad - bc}{ad + bc}}
#'   where \eqn{a} is the number of ties between nodes in the same category,
#'   \eqn{b} is the number of ties between nodes in different categories,
#'   \eqn{c} is the number of non-ties between nodes in the same category,
#'   and \eqn{d} is the number of non-ties between nodes in different categories.
#'   This value can range from -1 to 1, where 1 indicates perfect association
#'   (all ties are between nodes in the same category), -1 indicates perfect disassociation
#'   (all ties are between nodes in different categories), and 0 indicates no association.
#'   
#'   Geary's C is a measure of spatial autocorrelation, calculated as:
#'   \deqn{C = \frac{(n - 1) \sum
#'   \limits_{i=1}^n \sum\limits_{j=1}^n w_{ij} (x_i - x_j)^2}{2W \sum\limits_{i=1}^n (x_i - \bar{x})^2}}
#'   where \eqn{n} is the number of nodes, \eqn{
#'   w_{ij}} is the weight of the tie between nodes \eqn{i} and \eqn{j},
#'   \eqn{x_i} is the attribute value of node \eqn{i},
#'   \eqn{\bar{x}} is the mean attribute value across all nodes, and \eqn{W} is the sum of all tie weights.
#'   This value can range from 0 to 2, where values less than 1
#'   indicate positive autocorrelation (similar values are more likely to be connected),
#'   values greater than 1 indicate negative autocorrelation (dissimilar values are more likely
#'   to be connected), and a value of 1 indicates no autocorrelation.
#'   The upper bound of 2 holds where every tie carries the same weight.
#'   Geary's C reads the weights where the network has them,
#'   and a heavy tie between dissimilar values can then carry the sum past 2,
#'   so a weighted network declares the upper end open.
#'   If an incompatible method is chosen for the attribute type,
#'   a suitable alternative will be used instead with a message.
#' @family diversity
#' @template net_measure
NULL

#' @rdname measure_assort_net 
#' @section Heterophily:
#'   Given a partition of a network into a number of mutually exclusive groups then 
#'   The E-I index is the number of ties between (or _external_) nodes 
#'   grouped in some mutually exclusive categories
#'   minus the number of ties within (or _internal_) these groups
#'   divided by the total number of ties. 
#'   This value can range from 1 to -1,
#'   where 1 indicates ties only between categories/groups and -1 ties only within categories/groups.
#' @references 
#' ## On heterophily
#'   Krackhardt, David, and Robert N. Stern. 1988. 
#'   Informal networks and organizational crises: an experimental simulation. 
#'   _Social Psychology Quarterly_ 51(2): 123-140.
#'   \doi{10.2307/2786835}
#'   
#'   McPherson, Miller, Lynn Smith-Lovin, and James M. Cook. 2001.
#'   "Birds of a Feather: Homophily in Social Networks".
#'   _Annual Review of Sociology_, 27(1): 415-444.
#'   \doi{10.1146/annurev.soc.27.1.415}
#' @examples 
#' marvel_friends <- to_unsigned(to_uniplex(fict_marvel, "relationship"), "positive")
#' net_by_heterophily(marvel_friends, "Gender")
#' net_by_heterophily(marvel_friends, "Attractive")
#' @export
net_by_heterophily <- function(.data, attribute){
  .data <- manynet::expect_nodes(.data)
  m <- manynet::as_matrix(.data)
  if (length(attribute) == 1 && is.character(attribute)) {
    attribute <- manynet::node_attribute(.data, attribute)
  }
  if (is.character(attribute) | is.numeric(attribute)) {
    attribute <- as.factor(attribute)
  }
  same <- outer(attribute, attribute, "==")
  nInternal <- sum(m * same, na.rm = TRUE)
  nExternal <- sum(m, na.rm = TRUE) - nInternal
  ei <- (nExternal - nInternal) / sum(m, na.rm = TRUE)
  make_network_measure(ei, .data, call = deparse(sys.call()),
                       measure = "E-I index", range = c(-1, 1),
                       normalization = "none")
}

#' @rdname measure_assort_net
#' @examples 
#' net_by_homophily(marvel_friends, "Gender")
#' @export
net_by_homophily <- function(.data, attribute,
                             assortativity = c("ie","ei","yule","geary")){
  .data <- manynet::expect_nodes(.data)
  # mode <- attr_mode(.data, attribute)
  # if(is_twomode(.data) && !is.null(mode)){
  #   if(mode){
  #     snet_info("Attribute only present on second mode of two-mode network.")
  #     snet_info("Calculating homophily on first mode instead.")
  #     attribute <- manynet::node_attribute(.data, attribute)[
  #       !manynet::node_is_mode(.data)]
  #   } else {
  #     snet_info("Attribute only present on first mode of two-mode network.")
  #     snet_info("Calculating homophily on second mode instead.")
  #     attribute <- manynet::node_attribute(.data, attribute)[
  #       manynet::node_is_mode(.data)]
  #   }
  #   .data <- manynet::to_mode(.data, mode = mode)
  # }
  if (length(attribute) == 1 && is.character(attribute)) {
    attribute <- manynet::node_attribute(.data, attribute)
  }
  assortativity <- match.arg(assortativity)
  if(is.numeric(attribute) && assortativity %in% c("ie","ei","yule")){
    manynet::snet_info("{.val {assortativity}} index is not appropriate for numeric attributes.")
    manynet::snet_info("Using {.val geary}'s C instead.")
    assortativity <- "geary"
  }
  if(!is.numeric(attribute) && assortativity == "geary"){
    manynet::snet_info("{.val {assortativity}} index is not appropriate for categorical attributes.")
    manynet::snet_info("Using {.val ie} index instead.")
    assortativity <- "ie"
  }
  
  # The E-I index and Yule's Q count ties and non-ties, so they read a binary
  # matrix. Geary's C takes w_ij from the formula, so it keeps the weights.
  m <- if(assortativity == "geary") manynet::as_matrix(.data) else
    manynet::as_matrix(manynet::to_unweighted(.data))
  
  ei <- function(m, attribute){
    same <- outer(attribute, attribute, "==")
    nInternal <- sum(m * same, na.rm = TRUE)
    nExternal <- sum(m, na.rm = TRUE) - nInternal
    (nExternal - nInternal) / sum(m, na.rm = TRUE)
  }
  
  yule <- function(m, attribute){
    same <- outer(attribute, attribute, "==")
    a <- sum(m * same, na.rm = TRUE)
    b <- sum(m * (!same), na.rm = TRUE)
    c <- sum((1 - m) * same, na.rm = TRUE)
    d <- sum((1 - m) * (!same), na.rm = TRUE)
    (a*d - b*c)/(a*d + b*c)
  }
  
  geary <- function(m, attribute){
    # identify valid nodes
    valid <- !is.na(attribute)
    attr_valid <- attribute[valid]
    m_valid <- m[valid, valid, drop = FALSE]
    
    # recompute n and mean on valid nodes
    n <- length(attr_valid)
    xbar <- mean(attr_valid, na.rm = TRUE)
    
    # pairwise squared differences
    diffsq <- (outer(attr_valid, attr_valid, "-"))^2
    
    # weight sum
    W <- sum(m_valid, na.rm = TRUE)
    
    # numerator and denominator
    num <- (n - 1) * sum(m_valid * diffsq, na.rm = TRUE)
    den <- 2 * W * sum((attr_valid - xbar)^2, na.rm = TRUE)
    
    if (den == 0) return(NA_real_)
    num / den
  }
  
  res <- switch(match.arg(assortativity),
                ie = -ei(m, attribute),
                ei = ei(m, attribute),
                yule = yule(m, attribute),
                geary = geary(m, attribute))
  
  meta <- .homophily_metadata(assortativity, manynet::is_weighted(.data))
  make_network_measure(res, .data, call = deparse(sys.call()),
                       measure = meta$measure, range = meta$range,
                       normalization = "none", variant = meta$variant)
}

# As with diversity, the index asked for may not be the index used, so the
# metadata follows the resolved choice. Geary's C is centred on 1 rather than
# 0, and runs the other way: below 1 is similarity, above 1 dissimilarity.
# Both diversity functions may substitute a different index than the one asked
# for, so the metadata is read off the index that actually ran. Blau's and
# Gini's are bounded proportions; Teachman's entropy grows with the number of
# categories, and the coefficient of variation is signed and unbounded.
.diversity_metadata <- function(diversity){
  list(measure = switch(diversity,
                        blau = "Blau's index",
                        teachman = "Teachman's index",
                        variation = "coefficient of variation",
                        gini = "Gini coefficient"),
       range = switch(diversity,
                      blau = , gini = c(0, 1),
                      teachman = c(0, Inf),
                      variation = c(-Inf, Inf)),
       normalization = `if`(diversity %in% c("blau", "gini"),
                            "normalized", "none"),
       variant = diversity)
}

# Geary's C reaches 2 only where every tie carries the same weight. Once the
# weights differ, a heavy tie between dissimilar values can carry the sum past
# that, so a weighted network declares the upper end open.
.homophily_metadata <- function(assortativity, weighted = FALSE){
  list(measure = switch(assortativity,
                        ie = "IE index",
                        ei = "E-I index",
                        yule = "Yule's Q",
                        geary = "Geary's C"),
       range = switch(assortativity,
                      ie = , ei = , yule = c(-1, 1),
                      geary = `if`(weighted, c(0, Inf), c(0, 2))),
       variant = assortativity)
}


attr_mode <- function(.data, attribute){
  if(manynet::is_twomode(.data)){
    miss <- is.na(manynet::node_attribute(.data, attribute))
    mode <- manynet::node_is_mode(.data)
    if(all(miss[mode])) return(FALSE) # attribute only on first (FALSE) mode
    if(all(miss[!mode])) return(TRUE) # attribute only on second (TRUE) mode
  } else NULL
}

#' @rdname measure_assort_net 
#' @importFrom igraph assortativity_degree
#' @references
#' ## On assortativity
#' Newman, Mark E.J. 2002.
#' "Assortative mixing in networks".
#' _Physical Review Letters_, 89(20): 208701.
#' \doi{10.1103/physrevlett.89.208701} 
#' @examples 
#' net_by_assortativity(ison_networkers)
#' @export
net_by_assortativity <- function(.data){
  .data <- manynet::expect_nodes(.data)
  make_network_measure(igraph::assortativity_degree(manynet::as_igraph(.data), 
                                                    directed = manynet::is_directed(.data)),
                       .data, call = deparse(sys.call()),
                       measure = "degree assortativity", range = c(-1, 1),
                       normalization = "none")
}

#' @rdname measure_assort_net
#' @references
#' ## On spatial autocorrelation
#'   Moran, Patrick Alfred Pierce. 1950.
#'   "Notes on continuous stochastic phenomena".
#'   _Biometrika_ 37(1): 17-23.
#'   \doi{10.2307/2332142}
#' @examples 
#' net_by_spatial(ison_lawfirm, "age")
#' @section Spatial autocorrelation:
#'   Moran's I is conventionally read on \eqn{[-1, 1]}, 
#'   where positive values indicate that tied nodes hold similar values 
#'   and negative values that they hold dissimilar ones. 
#'   Its actual bounds, however, are set by the eigenvalues of the weight matrix, 
#'   and on the unstandardised weights used here it can fall outside that interval. 
#'   Its range is therefore declared open at both ends,
#'   and the conventional interval read as a guide rather than a guarantee.
#'
#'   A two-mode network has no ties within a mode,
#'   so where the attribute is present on one mode only,
#'   the network is first projected onto that mode.
#'   Two nodes are then neighbours where they are at distance 2,
#'   weighted by the number of nodes of the other mode that they share.
#'   Those counts make the weight matrix denser and \eqn{W} larger
#'   than in a one-mode network,
#'   so a projected value is read within a network rather than across networks.
#'   Where the attribute is present on both modes,
#'   the whole multilevel matrix is read instead,
#'   and every node and tie is retained.
#'
#'   Nodes with a missing attribute value are dropped,
#'   along with their ties,
#'   and \eqn{N}, \eqn{W} and \eqn{\bar{x}} are recomputed on those that remain.
#'   A missing tie counts as no tie.
#' @export
net_by_spatial <- function(.data, attribute){
  .data <- manynet::expect_nodes(.data)
  x <- manynet::node_attribute(.data, attribute)
  # Moran's I is the correlation of a value with itself across ties, so the
  # attribute has to hold a quantity rather than a category
  if(!is.numeric(x))
    manynet::snet_abort("{.fn net_by_spatial} measures the autocorrelation of",
                        "a numeric attribute, but {.val {attribute}} is",
                        "{.cls {class(x)[1]}}.")
  net <- .data
  if(manynet::is_twomode(net)){
    # There are no within-mode ties to correlate across, so either the mode
    # holding the attribute is projected onto itself, or, where both modes
    # hold it, the cross-mode ties are read as the weight matrix.
    mode <- attr_mode(net, attribute)
    if(is.null(mode)){
      manynet::snet_info("{.val {attribute}} is present on both modes.",
                         "Measuring autocorrelation across the",
                         "multilevel matrix.")
      net <- manynet::to_multilevel(net)
    } else {
      manynet::snet_info("{.val {attribute}} is present on one mode only.",
                         "Projecting onto that mode, so that nodes are",
                         "weighted by the number of nodes of the other",
                         "mode they share.")
      net <- manynet::to_mode(net, mode = `if`(mode, 2, 1))
    }
    x <- manynet::node_attribute(net, attribute)
  }
  w <- manynet::as_matrix(net)
  valid <- !is.na(x)
  if(!all(valid)){
    manynet::snet_info("Dropping {sum(!valid)} node{?s} with a missing",
                       "{.val {attribute}}.")
    x <- x[valid]
    w <- w[valid, valid, drop = FALSE]
  }
  N <- length(x)
  x_bar <- mean(x)
  W <- sum(w, na.rm = TRUE)
  den <- sum((x - x_bar)^2)
  I <- if(N < 2 || W == 0 || den == 0) NA_real_ else
    (N/W) * sum(w * outer(x - x_bar, x - x_bar), na.rm = TRUE) / den
  make_network_measure(I, .data,
                       call = deparse(sys.call()),
                       measure = "Moran's I", range = c(-Inf, Inf),
                       normalization = "none")
}

# Network assortativity ####

#' Measures of nodes assortativity
#' @name measure_assort_node
#' @description
#'   These functions offer ways to measure nodes' assortativity in a network:
#'   
#'   - `node_by_heterophily()` measures each node's embeddedness within groups
#'   of nodes with the same attribute.
#'   - `node_by_homophily()` measures each node's embeddedness within groups
#'   of nodes with the same attribute, using a variety of methods.
#'   
#' @template param_data
#' @template param_attr
#' @family diversity
#' @template node_measure
#' @inheritParams measure_assort_net
NULL

#' @rdname measure_assort_node 
#' @examples 
#' marvel_friends <- to_unsigned(to_uniplex(fict_marvel, "relationship"), "positive")
#' node_by_heterophily(marvel_friends, "Gender")
#' node_by_heterophily(marvel_friends, "Attractive")
#' @export
node_by_heterophily <- function(.data, attribute){
  .data <- manynet::expect_nodes(.data)
  m <- manynet::as_matrix(.data)
  if (length(attribute) == 1 && is.character(attribute)) {
    attribute <- manynet::node_attribute(.data, attribute)
  }
  if (is.character(attribute) | is.numeric(attribute)) {
    attribute <- as.factor(attribute)
  }
  if(anyNA(attribute)){
    m[is.na(attribute),] <- NA
    m[,is.na(attribute)] <- NA
  }
  same <- outer(attribute, attribute, "==")
  nInternal <- rowSums(m * same, na.rm = TRUE)
  nInternal[is.na(attribute)] <- NA
  nExternal <- rowSums(m, na.rm = TRUE) - nInternal
  ei <- (nExternal - nInternal) / rowSums(m, na.rm = TRUE)
  make_node_measure(ei, .data, measure = "E-I index", range = c(-1, 1),
                    normalization = "none")
}

#' @rdname measure_assort_node
#' @export
node_by_homophily <- function(.data, attribute,
                              assortativity = c("ie","ei","yule","geary")){
  .data <- manynet::expect_nodes(.data)
  if (length(attribute) == 1 && is.character(attribute)) {
    attribute <- manynet::node_attribute(.data, attribute)
  }
  assortativity <- match.arg(assortativity)
  if(is.numeric(attribute) && assortativity %in% c("ie","ei","yule")){
    manynet::snet_info("{.val {assortativity}} index is not appropriate for numeric attributes.")
    manynet::snet_info("Using {.val geary}'s C instead.")
    assortativity <- "geary"
  }
  if(!is.numeric(attribute) && assortativity == "geary"){
    manynet::snet_info("{.val {assortativity}} index is not appropriate for categorical attributes.")
    manynet::snet_info("Using {.val ie} index instead.")
    assortativity <- "ie"
  }
  idat <- manynet::as_igraph(.data)
  # The attribute is carried on the network rather than subset per ego, since
  # igraph::ego() lists the ego first while igraph::induced_subgraph() keeps
  # the original node order, so the two orders do not line up.
  idat <- igraph::set_vertex_attr(idat, ".homophily", value = attribute)
  out <- vapply(igraph::ego(idat),
                function(x) net_by_homophily(igraph::induced_subgraph(idat, x),
                                             ".homophily",
                                             assortativity = assortativity),
                FUN.VALUE = numeric(1))
  meta <- .homophily_metadata(assortativity, manynet::is_weighted(.data))
  make_node_measure(out, .data, measure = meta$measure, range = meta$range,
                    normalization = "none", variant = meta$variant)
}

