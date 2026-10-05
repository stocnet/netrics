# Ego-network composition ####

#' Motifs of ego-network composition
#' @name motif_composition
#' @template section_cognitive
#' @description
#'   These functions describe the composition of each node's ego-network,
#'   that is, what the ties and alters surrounding each node look like:
#'
#'   - `node_x_ties()` describes the distribution of each node's tie values,
#'   or, in a multiplex network, how its ties are spread across layers.
#'   - `node_x_alters()` describes the composition of each node's alters on
#'   some attribute.
#'   - `node_x_similarity()` describes how similar each node is to its alters
#'   on some attribute, or, in a two-mode network, to those it shares a node
#'   of the other mode with.
#'
#'   Where the corresponding `node_by_*()` measures collapse this information
#'   into a single score per node, these return the whole table,
#'   which is often what is wanted when exploring ego-networks.
#'   Each branches internally on the type of network or attribute given,
#'   so the same function serves weighted, multiplex, and two-mode networks,
#'   and categorical as well as continuous attributes.
#' @section Multiplex networks:
#'   `node_x_ties()` returns one column per layer, plus a Diversity column,
#'   rather than the distribution of tie values it returns otherwise.
#'   Layers are taken by name, so a network multiplexed on any attribute is
#'   covered, not only one multiplexed on `type`.
#'   Every column stays the length of the whole nodeset,
#'   so a node holding no tie in a layer scores 0 there rather than dropping out.
#' @template param_data
#' @template param_attr
#' @template param_dir
#' @family motifs
#' @family diversity
#' @template node_motif
NULL

#' @rdname motif_composition
#' @section Tie composition:
#'   For a weighted network this returns the distribution of each node's tie
#'   values: how many ties it has, and the sum, mean, standard deviation,
#'   and quartiles of their strengths.
#'   Two nodes may have the same weighted degree while one spreads its
#'   involvement evenly and the other concentrates it in a single strong tie,
#'   and it is the spread rather than the total that distinguishes them.
#'
#'   For a multiplex network it instead returns one column per layer, giving
#'   each node's degree in that layer (or its strength, where the layer is
#'   itself weighted), together with `Diversity`,
#'   the index of qualitative variation across the layers.
#'   This is 0 where a node's ties all fall in a single layer,
#'   and 1 where they are spread evenly across all of them.
#'   Where the interest is in just two of the layers,
#'   [node_by_multidegree()] gives the ratio between them.
#'
#'   For an unweighted, uniplex network only the degree is available,
#'   so this returns that alone.
#'   Isolates have no ties to summarise and so take `NA` for the
#'   distributional columns.
#'
#'   In a directed network, `direction` selects whose ties are described:
#'   a node's outgoing ties, its incoming ties, or both together.
#'   Note that under `"all"` a reciprocated pair is treated as a single
#'   relationship of combined strength, so `Ties` counts a node's distinct
#'   alters rather than its arcs, while `Sum` matches its total degree.
#' @examples
#' node_x_ties(ison_networkers)
#' node_x_ties(ison_algebra)
#' node_x_ties(fict_marvel)
#' @export
node_x_ties <- function(.data, direction = c("all", "out", "in")){
  .data <- manynet::expect_nodes(.data)
  .data <- .to_aggregated_css(.data)
  direction <- match.arg(direction)
  if(manynet::is_multiplex(.data)){
    # `layer_names()` rather than the "type" tie attribute, since a network
    # multiplexed on any other attribute would otherwise return no layers at
    # all and only the Diversity column
    layers <- manynet::layer_names(.data)
    # `uniplex_degree()` keeps each layer at the length of the whole nodeset,
    # which `to_uniplex()` does not
    out <- vapply(layers, function(l)
      uniplex_degree(.data, l, normalized = FALSE, direction = direction),
      FUN.VALUE = numeric(manynet::net_nodes(.data)))
    out <- cbind(out, Diversity = .iqv(out))
  } else if(manynet::is_weighted(.data)){
    mat <- .directed_matrix(.data, direction)
    diag(mat) <- NA # a node's tie to itself is not part of its composition
    out <- t(vapply(seq_len(nrow(mat)), function(i){
      vals <- mat[i,][!is.na(mat[i,])]
      vals <- vals[vals != 0] # only realised ties have a strength
      if(length(vals) == 0)
        return(c(0, 0, rep(NA_real_, 6)))
      c(length(vals), sum(vals), mean(vals),
        stats::sd(vals), min(vals),
        stats::median(vals), max(vals),
        stats::IQR(vals))
    }, FUN.VALUE = numeric(8)))
    colnames(out) <- c("Ties", "Sum", "Mean", "SD",
                       "Min", "Median", "Max", "IQR")
  } else {
    out <- matrix(as.numeric(node_by_degree(.data, normalized = FALSE,
                                            direction = direction)),
                  ncol = 1, dimnames = list(NULL, "Ties"))
    manynet::snet_info("Since this network is neither weighted nor multiplex,",
                       "only nodes' degrees are reported.")
  }
  make_node_motif(out, .data)
}

# Orient a network's matrix so that each row holds the ties a node is to be
# described by: its outgoing ties, its incoming ties, or both. For an
# undirected network all three coincide.
.directed_matrix <- function(.data, direction){
  mat <- manynet::as_matrix(manynet::to_onemode(.data))
  if(!manynet::is_directed(.data)) return(mat)
  switch(direction,
         out = mat,
         `in` = t(mat),
         all = mat + t(mat))
}

# Index of qualitative variation across the columns of a matrix of counts,
# normalising Blau's index by its maximum so that it ranges over [0,1]
# regardless of how many categories there are. Rows summing to zero have no
# distribution to describe and so return NA.
.iqv <- function(counts){
  k <- ncol(counts)
  tot <- rowSums(counts)
  props <- counts/tot
  blau <- 1 - rowSums(props^2)
  out <- if(k > 1) blau/(1 - 1/k) else rep(0, nrow(counts))
  out[tot == 0] <- NA_real_
  out
}

#' @rdname motif_composition
#' @section Alter composition:
#'   Where the attribute is categorical, this returns how many of each node's
#'   alters fall into each category, weighted by tie strength where the network
#'   is weighted.
#'   Where it is continuous, this returns the sum, tie-strength weighted sum,
#'   mean, tie-strength weighted mean, minimum, maximum, range, and standard
#'   deviation of the attribute across each node's alters.
#'
#'   The two weighted columns answer different questions.
#'   The weighted mean (`Weighted`) differs from the mean wherever a node's ties
#'   are of unequal strength: it describes the attribute of the alters a node
#'   is most involved with, rather than of its alters as an undifferentiated set.
#'   The weighted sum (`WeightedSum`) instead multiplies each alter's value by
#'   the strength of the tie to it, as where a tie's weight is an amount of
#'   exposure to that alter.
#'   On an unweighted network these equal the mean and the sum.
#'
#'   Alters whose value is missing are left out of the summary.
#'   Nodes with no alters of known value, including isolates, take `NA`.
#'
#'   In a directed network, `direction` selects which alters are described:
#'   those a node sends ties to (`"out"`), those it receives ties from
#'   (`"in"`), those it does either with (`"all"`), or those it does both with
#'   (`"reciprocated"`).
#'   Under `"all"` and `"reciprocated"`, an alter counts once,
#'   with the combined strength of the ties in both directions.
#'
#'   In a two-mode network where the attribute is held by one mode alone,
#'   a node of that mode has no alters of known value at distance one.
#'   Each node of that mode is instead described by its alters at distance two,
#'   the nodes of its own mode that it shares a node of the other mode with,
#'   weighted by how many it shares (see Tertius similarity below).
#'   Each node of the other mode is described by its alters at distance one,
#'   which hold the attribute.
#'   Where both modes hold the attribute, every node is described by its
#'   alters at distance one.
#'   `direction` applies to one-mode networks only:
#'   a two-mode network is read as undirected.
#'
#'   Any tie counts as a tie here, whatever its sign, and by its magnitude.
#'   Apply [manynet::to_unsigned()] first to consider only positive or only
#'   negative ties.
#' @examples
#' node_x_alters(ison_networkers, "Discipline")
#' node_x_alters(ison_networkers, "Citations")
#' node_x_alters(ison_networkers, "Citations", direction = "reciprocated")
#' node_x_alters(ison_southern_women, "Title")
#' @export
node_x_alters <- function(.data, attribute,
                          direction = c("all", "out", "in", "reciprocated")){
  .data <- manynet::expect_nodes(.data)
  .data <- .to_aggregated_css(.data)
  .data <- manynet::to_unsigned(.data, keep = "both")
  direction <- match.arg(direction)
  attr <- .resolve_attribute(.data, attribute)
  mat <- .alter_matrix(.data, attr, direction)
  known <- !is.na(attr)
  mat[, !known] <- 0 # alters of unknown value are left out
  none <- rowSums(mat != 0) == 0 # nodes with nothing to summarise
  if(.is_categorical(attr)){
    attr <- as.factor(attr)
    out <- vapply(levels(attr), function(l)
      rowSums(mat[, which(attr == l), drop = FALSE]),
      FUN.VALUE = numeric(nrow(mat)))
    out <- matrix(out, nrow = nrow(mat),
                  dimnames = list(NULL, levels(attr)))
    out[none, ] <- NA_real_
  } else {
    attr <- as.numeric(attr)
    out <- t(vapply(seq_len(nrow(mat)), function(i){
      w <- mat[i,]
      alters <- attr[w != 0]
      wts <- w[w != 0]
      if(length(alters) == 0) return(rep(NA_real_, 8))
      c(sum(alters), sum(alters * wts),
        mean(alters), stats::weighted.mean(alters, wts),
        min(alters), max(alters), diff(range(alters)),
        stats::sd(alters))
    }, FUN.VALUE = numeric(8)))
    colnames(out) <- c("Sum", "WeightedSum", "Mean", "Weighted",
                       "Min", "Max", "Range", "SD")
  }
  make_node_motif(out, .data)
}

# The strength of each node's tie to each of its alters, with rows for the
# nodes described and 0 where a node is not an alter. Missing ties count as
# absent, and on an unweighted network every alter has strength 1, so that a
# reciprocated pair is one alter and not two.
.alter_matrix <- function(.data, attr, direction){
  mat <- manynet::as_matrix(manynet::to_onemode(.data))
  mat[is.na(mat)] <- 0
  if(!manynet::is_weighted(.data)) mat <- (mat != 0) * 1
  if(manynet::is_twomode(.data)){
    mode <- manynet::node_is_mode(.data)
    known <- !is.na(attr)
    # Where one mode alone holds the attribute, that mode's nodes are read at
    # distance two, as in `.comparable_matrix()`, but weighted by the number
    # of other-mode nodes they share; the other mode's nodes keep their
    # distance one alters, which already hold the attribute.
    held <- if(!any(known[mode])) !mode else if(!any(known[!mode])) mode
    if(!is.null(held) && any(known)){
      d2 <- mat %*% mat
      d2[outer(mode, mode, "!=")] <- 0
      mat[held, ] <- d2[held, ]
    }
  } else if(manynet::is_directed(.data)){
    both <- mat != 0 & t(mat) != 0
    mat <- switch(direction,
                  out = mat,
                  `in` = t(mat),
                  all = mat + t(mat),
                  reciprocated = (mat + t(mat)) * both)
    if(!manynet::is_weighted(.data)) mat <- (mat != 0) * 1
  }
  diag(mat) <- 0 # a node is not its own alter
  mat
}

#' @rdname motif_composition
#' @section Ego-alter similarity:
#'   Where the attribute is categorical, this returns each node's own two-by-two
#'   table of whether a tie is present and whether the alter shares its
#'   category, together with the summaries built from it:
#'   the proportion of a node's ties that are to others of the same category
#'   (`PctSame`), the EI index (`EI`), which runs from -1 where all of a node's
#'   ties are internal to its own category to +1 where all are external,
#'   the odds ratio and its logarithm, and Yule's Q.
#'
#'   The EI index and the odds ratio answer different questions.
#'   EI describes the mix of a node's ties, and so is sensitive to how large its
#'   category is: in a small category even an indifferent node will have mostly
#'   external ties. The odds ratio and Yule's Q instead compare the ties a node
#'   made against the ties it could have made, and so are not.
#'
#'   Where the attribute is continuous, this returns the mean difference,
#'   mean absolute difference, and mean squared difference between a node and
#'   its alters, followed by three measures of dyadic similarity averaged over
#'   a node's alters: Zegers' coefficient, the ratio of the smaller value to the
#'   larger, and the product.
#' @section Tertius similarity:
#'   In a two-mode network no two nodes of the same mode are ever tied,
#'   so similarity to one's alters cannot be measured directly.
#'   Instead, each node is compared here with those it shares a node of the
#'   other mode with, that is, its alters at distance two.
#'   This is the tertius neighbourhood used by the `tertius()` effect in
#'   `{migraph}` and `{goldfish}`, and described in Haunss and Hollway (2023):
#'   in a discourse network, for example, the actors an actor is compared with
#'   are those making claims about the same concepts.
#'
#'   The same columns are returned as for a one-mode network,
#'   but read at distance two: a node's alters are those it shares some
#'   other-mode node with, however many they share, and the non-alters are
#'   the remaining nodes of its own mode.
#'   Nodes of the other mode are neither alters nor non-alters,
#'   and so are excluded rather than counted as absent ties.
#'   Since a node's alters are always of its own mode,
#'   only that mode's values of the attribute are used;
#'   where an attribute is held by one mode alone,
#'   the other mode's nodes take `NA`.
#' @references
#' ## On tertius effects
#' Haunss, Sebastian, and James Hollway. 2023.
#' "Multimodal mechanisms of political discourse dynamics and the case of
#' Germany's nuclear energy phase-out".
#' _Network Science_ 11(2): 205-223.
#' \doi{10.1017/nws.2022.31}
#'
#' ## On the EI index
#' Krackhardt, David, and Robert N. Stern. 1988.
#' "Informal Networks and Organizational Crises: An Experimental Simulation".
#' _Social Psychology Quarterly_ 51(2): 123-140.
#' \doi{10.2307/2786835}
#'
#' ## On Yule's Q
#' Yule, G. Udny. 1912.
#' "On the Methods of Measuring Association Between Two Attributes".
#' _Journal of the Royal Statistical Society_ 75(6): 579-652.
#' \doi{10.2307/2340126}
#' @examples
#' node_x_similarity(ison_networkers, "Discipline")
#' node_x_similarity(ison_southern_women, "Title")
#' @export
node_x_similarity <- function(.data, attribute){
  .data <- manynet::expect_nodes(.data)
  .data <- .to_aggregated_css(.data)
  attr <- .resolve_attribute(.data, attribute)
  mat <- .comparable_matrix(.data)
  if(.is_categorical(attr)){
    same <- outer(attr, attr, "==")
    same[is.na(mat)] <- NA # nodes that cannot be alters are not compared
    out <- t(vapply(seq_len(nrow(mat)), function(i){
      a <- sum(mat[i,] == 1 & same[i,], na.rm = TRUE)
      b <- sum(mat[i,] == 1 & !same[i,], na.rm = TRUE)
      cc <- sum(mat[i,] == 0 & same[i,], na.rm = TRUE)
      d <- sum(mat[i,] == 0 & !same[i,], na.rm = TRUE)
      pct <- if((a+b) == 0) NA_real_ else a/(a+b)
      ei <- if((a+b) == 0) NA_real_ else (b-a)/(b+a)
      odds <- if(b*cc == 0) NA_real_ else (a*d)/(b*cc)
      yule <- if((a*d + b*cc) == 0) NA_real_ else (a*d - b*cc)/(a*d + b*cc)
      c(a, b, cc, d, pct, ei, odds, log(odds), yule)
    }, FUN.VALUE = numeric(9)))
    colnames(out) <- c("TieSame", "TieDiff", "NoTieSame", "NoTieDiff",
                       "PctSame", "EI", "Odds", "LogOdds", "YulesQ")
  } else {
    attr <- as.numeric(attr)
    diffs <- outer(attr, attr, "-") # ego minus alter
    zeg <- outer(attr, attr, function(x, y)
      ifelse(x^2 + y^2 == 0, NA_real_, (x*y)/(x^2 + y^2)))
    mnmx <- outer(attr, attr, function(x, y)
      ifelse(pmax(x, y) == 0, NA_real_, pmin(x, y)/pmax(x, y)))
    prod <- outer(attr, attr, "*")
    out <- t(vapply(seq_len(nrow(mat)), function(i){
      alters <- which(mat[i,] == 1)
      if(length(alters) == 0) return(rep(NA_real_, 6))
      c(mean(diffs[i, alters]),
        mean(abs(diffs[i, alters])),
        mean(diffs[i, alters]^2),
        mean(zeg[i, alters], na.rm = TRUE),
        mean(mnmx[i, alters], na.rm = TRUE),
        mean(prod[i, alters]))
    }, FUN.VALUE = numeric(6)))
    colnames(out) <- c("Diff", "AbsDiff", "SqDiff",
                       "Zegers", "MinMax", "Product")
  }
  make_node_motif(out, .data)
}

# Network-level homophily ####

#' Motifs of network homophily
#' @name motif_homophily
#' @template section_cognitive
#' @description
#'   `net_x_homophily()` returns the two-by-two table from which network-level
#'   homophily is calculated, together with the summaries built from it.
#'
#'   Where [net_by_heterophily()] returns the EI index alone,
#'   this returns the counts it rests on, so that the index can be interpreted
#'   against the network's own composition.
#'
#'   Note that on a weighted network the two report different values.
#'   A contingency table counts ties, so `net_x_homophily()` treats every tie
#'   alike, whereas [net_by_heterophily()] sums tie weights and so gives more
#'   say to stronger ties. On unweighted networks the two agree exactly.
#'   Apply [manynet::to_unweighted()] first to compare them directly.
#' @template param_data
#' @template param_attr
#' @family motifs
#' @family diversity
#' @template net_motif
#' @section Expected EI:
#'   The EI index depends on how large the categories are, not only on how
#'   nodes choose between them.
#'   A network split into two equal groups will have a lower EI than one in
#'   which a small minority is surrounded by a large majority,
#'   even if nodes in both are equally indifferent to category.
#'
#'   `ExpectedEI` gives the EI that would be observed if ties were distributed
#'   at random across all possible pairs, holding category sizes fixed.
#'   Comparing `EI` against it separates the network's mixing from its
#'   composition: an EI above the expected value indicates more crossing of
#'   category boundaries than chance alone would produce, and one below it
#'   indicates less.
#' @references
#' ## On the EI index
#' Krackhardt, David, and Robert N. Stern. 1988.
#' "Informal Networks and Organizational Crises: An Experimental Simulation".
#' _Social Psychology Quarterly_ 51(2): 123-140.
#' \doi{10.2307/2786835}
#' @examples
#' net_x_homophily(ison_networkers, "Discipline")
#' @export
net_x_homophily <- function(.data, attribute){
  .data <- manynet::expect_nodes(.data)
  .data <- .to_aggregated_css(.data)
  if(manynet::is_twomode(.data))
    manynet::snet_abort("Homophily is only defined for one-mode networks.")
  attr <- .resolve_attribute(.data, attribute)
  if(!.is_categorical(attr)) attr <- as.factor(attr)
  mat <- manynet::as_matrix(manynet::to_unweighted(.data))
  diag(mat) <- NA # self-ties are not homophilous
  same <- outer(attr, attr, "==")
  diag(same) <- NA
  a <- sum(mat != 0 & same, na.rm = TRUE)
  b <- sum(mat != 0 & !same, na.rm = TRUE)
  cc <- sum(mat == 0 & same, na.rm = TRUE)
  d <- sum(mat == 0 & !same, na.rm = TRUE)
  ei <- if((a+b) == 0) NaN else (b-a)/(a+b)
  # the EI expected if the same number of ties were placed at random
  # over all possible pairs, holding the category sizes fixed
  expei <- if((a+b+cc+d) == 0) NaN else ((b+d) - (a+cc))/(a+b+cc+d)
  yule <- if((a*d + b*cc) == 0) NaN else (a*d - b*cc)/(a*d + b*cc)
  out <- c(TieSame = a, TieDiff = b, NoTieSame = cc, NoTieDiff = d,
           PctSame = if((a+b) == 0) NaN else a/(a+b),
           EI = ei, ExpectedEI = expei, YulesQ = yule)
  make_network_motif(out, .data)
}

# Helpers ####

# Which nodes each node is compared with, as a matrix of 1 where a node is an
# alter, 0 where it could have been but is not, and NA where it could not be.
# In a one-mode network a node's alters are simply those it is tied to. In a
# two-mode network no two nodes of the same mode are ever tied, so the nearest
# comparable others are those at distance two: those a node shares a node of
# the other mode with. Nodes of the other mode are then neither alters nor
# non-alters, and so are held out rather than counted as absent ties.
.comparable_matrix <- function(.data){
  mat <- manynet::as_matrix(
    manynet::to_unweighted(manynet::to_onemode(.data)))
  mat[mat != 0] <- 1
  if(manynet::is_twomode(.data)){
    mat <- (mat %*% mat > 0) * 1 # shares at least one node of the other mode
    mode <- manynet::node_is_mode(.data)
    mat[outer(mode, mode, "!=")] <- NA
  }
  diag(mat) <- NA # a node is not its own alter
  mat
}

# Resolve an attribute given either as a name or as a vector, matching how
# `node_by_heterophily()` and friends accept either.
.resolve_attribute <- function(.data, attribute){
  if(length(attribute) == 1 && is.character(attribute))
    attribute <- manynet::node_attribute(.data, attribute)
  if(is.null(attribute))
    manynet::snet_abort("No such attribute found in this network.")
  if(length(attribute) != manynet::net_nodes(.data))
    manynet::snet_abort("`attribute` must be as long as there are nodes.")
  attribute
}

# Character, factor and logical attributes are categorical; numeric ones are
# treated as continuous. Since group codes are often stored as numbers, which
# branch was taken is reported rather than left to be inferred from the output.
.is_categorical <- function(attribute){
  out <- is.character(attribute) || is.factor(attribute) || is.logical(attribute)
  if(!out && is.numeric(attribute) &&
     length(unique(stats::na.omit(attribute))) < 10)
    manynet::snet_info(
      "Treating this numeric attribute as continuous.",
      "If it codes categories, pass it {.code as.factor()} instead.")
  out
}
