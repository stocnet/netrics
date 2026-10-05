# Input shapes. These sweep the functions that a signed or a multilevel
# network once aborted, so that a shape which broke them cannot break them
# again silently. See the "Input shapes" section of .github/CONTRIBUTING.md.
#
# `fict_marvel` is signed and multilevel; `fict_actually` is multilevel alone.

signed_multilevel <- manynet::fict_marvel
multilevel <- manynet::fict_actually

# Functions that read a tie as a distance, and so drop to the positive ties.
signed_distance <- c("node_by_closeness", "node_by_harmonic", "node_by_reach",
                     "node_by_decay", "node_by_integration",
                     "node_by_radiality", "node_by_eccentricity",
                     "node_by_vitality", "node_by_betweenness",
                     "node_by_induced", "node_is_fold")

for(fn in signed_distance){
  test_that(paste(fn, "returns one value per node of a signed network"), {
    expect_length(do.call(fn, list(signed_multilevel)),
                  manynet::net_nodes(signed_multilevel))
  })
}

for(fn in c("net_by_closeness", "net_by_betweenness", "net_by_connectedness",
            "net_by_reach", "net_by_harmonic", "net_by_decay",
            "net_by_integration")){
  test_that(paste(fn, "returns one score for a signed network"), {
    expect_length(do.call(fn, list(signed_multilevel)), 1)
  })
}

for(fn in c("mode_by_closeness", "mode_by_betweenness")){
  test_that(paste(fn, "returns one score per mode of a signed network"), {
    expect_length(do.call(fn, list(signed_multilevel)), 2)
  })
}

# Functions that count a tie however it is signed, and so keep every tie.
for(fn in c("tie_is_transitive", "tie_is_triplet", "tie_is_cyclical",
            "tie_by_betweenness")){
  test_that(paste(fn, "returns one value per tie of a signed network"), {
    expect_length(do.call(fn, list(signed_multilevel)),
                  manynet::net_ties(signed_multilevel))
  })
}

test_that("node_by_hub() and node_by_authority() do not warn when signed", {
  expect_no_warning(node_by_hub(signed_multilevel))
  expect_no_warning(node_by_authority(signed_multilevel))
})

test_that("net_by_modularity() scores a signed network", {
  expect_length(net_by_modularity(signed_multilevel), 1)
})

test_that("node_in_community() uses spinglass where the network is signed", {
  memb <- node_in_community(signed_multilevel)
  expect_length(memb, manynet::net_nodes(signed_multilevel))
  # spinglass needs a connected network and accepts no `k`, so neither a `k`
  # nor an unconnected signed network leaves any algorithm to try
  expect_error(node_in_community(signed_multilevel, k = 3), "signed")
})

# A multilevel network reports itself as two-mode but cannot be projected,
# so these measure it whole.
for(fn in c("node_by_eigenvector", "node_by_power", "node_by_efficiency",
            "node_by_effsize", "node_is_independent", "node_is_core",
            "node_by_core")){
  for(nm in c("signed_multilevel", "multilevel")){
    test_that(paste(fn, "returns one value per node of", nm), {
      net <- get(nm)
      expect_length(do.call(fn, list(net)), manynet::net_nodes(net))
      # and each mode's centralisation is read from the whole network too
      expect_length(mode_by_eigenvector(net), 2)
    })
  }
}

test_that("a plain two-mode network still takes the projected path", {
  sw <- manynet::ison_southern_women
  expect_length(node_is_core(sw), manynet::net_nodes(sw))
  expect_length(node_by_eigenvector(sw), manynet::net_nodes(sw))
  expect_length(node_is_independent(sw), manynet::net_nodes(sw))
})

test_that("net_by_core() and net_by_factions() stop on a multilevel network", {
  # `manynet::create_*()` builds one layer, so there is no ideal to fit
  expect_error(net_by_core(multilevel), "one layer")
  expect_error(net_by_factions(multilevel), "one layer")
})

# A cognitive social structure. Since manynet 2.4.0, and before it,
# `manynet::as_matrix()` returns one from-to-perceiver array per layer here,
# which these functions once aborted on.
# `ison_hightech` records the perceivers' reports only since manynet 2.4.0,
# so the sweeps over it are skipped where an earlier manynet is installed.
css <- manynet::ison_hightech

# Measures are swept on a CSS by `check_cognitive_contract()`. Marks,
# memberships, and motifs have no rosters, so they are swept here over the
# namespace itself, which also reaches any added later. Each should read a CSS
# as its locally aggregated structure, as the measures do, and a tie mark
# should give each report the mark of the tie it reports.
cognitive_arguments_other <- list(
  net_x_brokerage   = list(membership = "dept"),
  node_x_brokerage  = list(membership = "dept"),
  net_x_homophily   = list(attribute = "dept"),
  node_x_alters     = list(attribute = "dept"),
  node_x_similarity = list(attribute = "dept"),
  node_in_roulette  = list(groups = 3),
  node_is_neighbor  = list(node = 1),
  node_is_exposed   = list(mark = c(1, 3)),
  tie_is_path       = list(from = 1, to = 2)
)
# A random draw among the reports is as valid as one among the ties, and
# giving one drawn tie to every one of its reports would break `select`.
cognitive_exempt <- c("tie_is_random")

for (fixture in names(cognitive_fixtures)) {
test_that(paste("marks, memberships, and motifs read the", fixture,
                "CSS as its aggregated structure"), {
  css <- cognitive_fixtures[[fixture]]
  skip_if_not_cognitive(css)
  agg <- suppressMessages(.to_aggregated_css(css))
  report <- match(.tie_keys(css), .tie_keys(agg))
  quietly <- function(fn, .data) {
    set.seed(1234)
    tryCatch(suppressMessages(suppressWarnings(
      do.call(fn, c(list(.data), cognitive_arguments_other[[fn]])))),
      error = function(e) e)
  }
  fns <- setdiff(grep("^(node_is|node_in|node_x|net_x|tie_is)_",
                      getNamespaceExports("netrics"), value = TRUE),
                 cognitive_exempt)
  for (fn in sort(fns)) {
    expected <- quietly(fn, agg)
    # e.g. functions that take a measure, a diffusion, or two networks
    if (inherits(expected, "error")) next
    res <- quietly(fn, css)
    expect_false(inherits(res, "error"),
                 label = paste0(fn, " runs on a cognitive social structure"))
    if (inherits(res, "error")) next
    vals <- unname(unclass(expected))
    if (startsWith(fn, "tie_")) {
      vals <- vals[report]
      vals[is.na(vals)] <- FALSE
    }
    expect_equal(unname(unclass(res)), vals, ignore_attr = TRUE,
                 label = paste0(fn, " on a CSS"),
                 expected.label = "its value on the aggregated network")
  }
})
}

test_that("regularity methods return a square matrix for a CSS", {
  skip_if_not_cognitive(css)
  n <- manynet::net_nodes(css)
  expect_equal(dim(suppressMessages(regularity_rolesim(css))), c(n, n))
  # The aggregated network is unweighted and connected, where REGE warns
  expect_warning(rege <- suppressMessages(regularity_rege(css)), "degenerate")
  expect_equal(dim(rege), c(n, n))
  expect_s3_class(suppressMessages(
    net_by_inconsistency(css, node_in_regular(css))), "network_measure")
})

test_that(".to_aggregated_css() keeps the ties both ends report", {
  # 1-2 is reported by both its ends, 2-3 by both of its, and 1-3 only by
  # node 3 of its ends, since node 2 is not one of them.
  # manynet 2.3.4 requires the reporters to be integers, where 2.4.0 coerces
  el <- data.frame(from = c(1, 1, 2, 2, 3, 1), to = c(2, 2, 3, 3, 1, 3),
                   by = as.integer(c(1, 2, 2, 3, 3, 2)))
  x <- manynet::as_stocnet(el)
  agg <- suppressMessages(.to_aggregated_css(x))
  expect_false(manynet::is_cognitive(agg))
  expect_equal(manynet::net_ties(agg), 2)
  expect_equal(manynet::net_nodes(agg), 3)
  # The fallback for manynet before 2.4.0 builds the same structure
  expect_equal(manynet::as_matrix(.las_intersection(x)),
               manynet::as_matrix(agg))
  # Networks that are not cognitive pass through unchanged
  expect_identical(.to_aggregated_css(manynet::ison_adolescents),
                   manynet::ison_adolescents)
})

test_that(".to_aggregated_css() and its fallback agree on a CSS", {
  skip_if_not_cognitive(css)
  expect_equal(manynet::as_matrix(.las_intersection(css)),
               manynet::as_matrix(suppressMessages(.to_aggregated_css(css))))
})

test_that(".to_aggregated_css() returns the class it was given", {
  skip_if_not_cognitive(css)
  for (x in list(css, manynet::as_tidygraph(css), manynet::as_igraph(css))) {
    expect_s3_class(suppressMessages(.to_aggregated_css(x)), class(x)[1])
    expect_s3_class(.las_intersection(x), class(x)[1])
  }
})
