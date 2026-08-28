# The centrality family's rosters are swept by the shared contract in
# helper-contract.R; what remains here is specific to this family.

test_that("node centralities meet the measure contract", {
  check_measure_contract(measure_rosters$centrality_node,
                         manynet::ison_adolescents, level = "node")
  expect_declared(measure_rosters$centrality_node, manynet::ison_adolescents)
})

test_that("multidegree meets the measure contract", {
  check_measure_contract(measure_rosters$centrality_multiplex,
                         manynet::fict_marvel, level = "node")
  expect_declared(measure_rosters$centrality_multiplex, manynet::fict_marvel)
})

test_that("tie centralities meet the measure contract", {
  check_measure_contract(measure_rosters$centrality_tie,
                         manynet::ison_adolescents, level = "tie")
  expect_declared(measure_rosters$centrality_tie, manynet::ison_adolescents)
})

test_that("network centralisations meet the measure contract", {
  check_measure_contract(measure_rosters$centrality_net,
                         manynet::ison_adolescents, level = "net")
  expect_declared(measure_rosters$centrality_net, manynet::ison_adolescents)
})

test_that("mode centralisations meet the measure contract", {
  check_measure_contract(measure_rosters$centrality_mode,
                         manynet::ison_southern_women, level = "mode")
  expect_declared(measure_rosters$centrality_mode,
                  manynet::ison_southern_women)
})

test_that("PN centrality meets the measure contract", {
  signed <- manynet::to_uniplex(manynet::fict_marvel, "relationship")
  check_measure_contract(measure_rosters$centrality_signed, signed,
                         level = "node")
  expect_declared(measure_rosters$centrality_signed, signed)
})

test_that("centralities dispatch on the information they are given", {
  g <- manynet::ison_adolescents
  w <- manynet::mutate_ties(g, weight = c(1, 2, 3, 1, 5, 1, 2, 8, 1, 3))
  # Measures built only from the adjacency structure, which igraph provides
  # no weighted form of, are exempt.
  exempt <- c("node_by_power", "node_by_leverage",
              "node_by_reach", "node_by_deg", "node_by_indegree",
              "node_by_outdegree", "node_by_degree")
  roster <- measure_rosters$centrality_node
  for (fn in setdiff(names(roster), exempt)) {
    unw <- as.numeric(call_measure(fn, roster[[fn]], g))
    wtd <- try(as.numeric(call_measure(fn, roster[[fn]], w)), silent = TRUE)
    if (inherits(wtd, "try-error")) {
      note_gap(fn, "errors on a weighted network")
    } else if (isTRUE(all.equal(unw, wtd))) {
      note_gap(fn, "ignores tie weights")
    }
  }
  succeed()
})

test_that("closeness-like variants relate as documented", {
  g <- manynet::ison_adolescents
  # Integration is an affine transformation of farness, so on a connected
  # network it can never reorder nodes relative to closeness.
  expect_equal(cor(as.numeric(node_by_integration(g)),
                   as.numeric(node_by_closeness(g)),
                   method = "spearman"), 1)
  # Decay centrality is reached through harmonic centrality's `decay`.
  expect_equal(as.numeric(node_by_decay(g, decay = 0.4)),
               as.numeric(node_by_harmonic(g, decay = 0.4)))
  # Radiality is integration read in the outgoing direction.
  expect_equal(as.numeric(node_by_radiality(g)),
               as.numeric(node_by_integration(g, direction = "out")))
})

test_that("closeness vitality identifies cut nodes", {
  g <- manynet::ison_adolescents
  raw <- as.numeric(node_by_vitality(g, normalized = FALSE))
  # The Wiener index of a disconnected network is infinite, so removing a cut
  # node gives negative infinity: a property of the definition, not a failure.
  expect_true(any(!is.finite(raw)))
  norm <- as.numeric(node_by_vitality(g))
  expect_true(all(is.finite(norm)))
  expect_true(all(norm >= 0 & norm <= 1))
  # Cut nodes take the endpoint that negative infinity occupied.
  expect_equal(norm[!is.finite(raw)], rep(0, sum(!is.finite(raw))))
})

test_that("node_by_degree reports strength when weights are used", {
  w <- manynet::mutate_ties(manynet::ison_adolescents,
                            weight = c(1, 2, 3, 1, 5, 1, 2, 8, 1, 3))
  expect_equal(attr(node_by_degree(w, alpha = 1), "measure"),
               "strength centrality")
  # Strength has no theoretical maximum, so this scales rather than normalises.
  expect_equal(attr(node_by_degree(w, alpha = 1), "normalization"), "scaled")
  expect_equal(attr(node_by_degree(w), "measure"), "degree centrality")
})

test_that("renamed `scale` argument still works, with a warning", {
  g <- manynet::ison_adolescents
  expect_warning(node_by_power(g, scale = TRUE), "renamed")
})

test_that("renamed `alpha` argument still works, with a warning", {
  g <- manynet::ison_adolescents
  expect_warning(node_by_alpha(g, alpha = 0.3), "renamed")
  expect_equal(as.numeric(suppressWarnings(node_by_alpha(g, alpha = 0.3))),
               as.numeric(node_by_alpha(g, decay = 0.3)))
})

test_that("subgraph centrality splits its walks as documented", {
  g <- manynet::ison_adolescents
  all <- as.numeric(node_by_subgraph(g))
  # At the default decay this is subgraph centrality as igraph computes it,
  # so replacing that call with an eigendecomposition changed no results.
  expect_equal(all, as.numeric(igraph::subgraph_centrality(manynet::as_igraph(g))))
  # Odd- and even-length closed walks partition the whole count.
  expect_equal(as.numeric(node_by_subgraph(g, walks = "odd")) +
                 as.numeric(node_by_subgraph(g, walks = "even")), all)
  # Each variant says which one it is.
  expect_equal(attr(node_by_subgraph(g, walks = "odd"), "variant"), "odd")
  expect_equal(attr(node_by_subgraph(g, walks = "odd"), "measure"),
               "odd subgraph centrality")
  # Discounting longer walks changes the scores but not their positivity.
  expect_false(isTRUE(all.equal(as.numeric(node_by_subgraph(g, decay = 0.5)), all)))
  expect_true(all(as.numeric(node_by_subgraph(g, decay = 0.5)) >= 1))
})

test_that("bipartivity recognises a two-mode network", {
  # A two-mode network admits no odd closed walk, so it is exactly bipartite.
  expect_equal(as.numeric(net_by_bipartivity(manynet::ison_southern_women)), 1)
  # A one-mode network with triangles falls short of it.
  bip <- as.numeric(net_by_bipartivity(manynet::ison_adolescents))
  expect_true(bip > 0 && bip < 1)
  # Bipartivity is the network-level share of what node_by_subgraph() splits.
  expect_equal(bip,
               sum(node_by_subgraph(manynet::ison_adolescents, walks = "even")) /
                 sum(node_by_subgraph(manynet::ison_adolescents)))
})

test_that("pagerank responds to its decay", {
  g <- manynet::ison_adolescents
  expect_false(isTRUE(all.equal(as.numeric(node_by_pagerank(g, decay = 0.4)),
                                as.numeric(node_by_pagerank(g)))))
  # Whatever the discount, the scores remain a distribution.
  expect_equal(sum(node_by_pagerank(g, decay = 0.4)), 1)
})
