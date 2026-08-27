test_that("network components works", {
  expect_equal(as.numeric(net_by_components(ison_adolescents)), 1)
})

test_that("network cohesion works", {
  expect_equal(as.numeric(net_by_cohesion(ison_southern_women)), 2)
})

test_that("network adhesion works", {
  expect_equal(as.numeric(net_by_adhesion(ison_southern_women)), 2)
})

test_that("network diameter works", {
  expect_equal(as.numeric(net_by_diameter(ison_southern_women)), 4)
})

test_that("network length works", {
  expect_equal(as.numeric(net_by_length(ison_southern_women)), 2.306, 
               tolerance = 0.001)
})

test_that("net_independence works", {
  expect_values(net_by_independence(ison_adolescents), 4)
})

test_that("net_strength works", {
  expect_values(net_by_strength(ison_adolescents), 0.5)
})

test_that("net_toughness works", {
  expect_values(net_by_toughness(ison_adolescents), 0.5)
})
test_that("network compactness works", {
  expect_equal(as.numeric(net_by_compactness(create_filled(10))), 1)
  expect_equal(as.numeric(net_by_compactness(create_empty(10))), 0)
  # compactness discriminates where connectedness cannot:
  # both are fully connected, but the star is more compact than the ring
  expect_gt(as.numeric(net_by_compactness(create_star(10))),
            as.numeric(net_by_compactness(create_ring(10))))
  expect_equal(as.numeric(net_by_connectedness(create_star(10))),
               as.numeric(net_by_connectedness(create_ring(10))))
  expect_values(net_by_compactness(ison_adolescents), 0.616)
  expect_values(net_by_compactness(ison_southern_women), 0.515)
  # compactness is the network-level counterpart of harmonic centrality,
  # and is the quantity known elsewhere as global efficiency
  expect_equal(as.numeric(net_by_compactness(ison_adolescents)),
               mean(as.numeric(node_by_harmonic(ison_adolescents,
                                                normalized = TRUE,
                                                cutoff = -1))))
})

test_that("net_by_compactness respects tie direction", {
  # igraph's default distance mode ignores direction, which would treat a
  # directed network as though every tie ran both ways
  dir <- to_unweighted(ison_networkers)
  expect_false(isTRUE(all.equal(
    as.numeric(net_by_compactness(dir)),
    as.numeric(net_by_compactness(to_undirected(dir))))))
  # a one-way chain is less compact than the same chain reciprocated
  chain <- matrix(0, 4, 4)
  chain[cbind(1:3, 2:4)] <- 1
  expect_lt(as.numeric(net_by_compactness(chain)),
            as.numeric(net_by_compactness(chain + t(chain))))
})

test_that("path measures work on a network holding signs as negative weights", {
  # `fict_marvel` is signed but not weighted, so its ties reach igraph as a
  # `weight` attribute of -1 and 1, which igraph would read as a distance
  expect_true(manynet::is_signed(fict_marvel))
  expect_false(manynet::is_weighted(fict_marvel))
  expect_s3_class(net_by_diameter(fict_marvel), "network_measure")
  expect_s3_class(net_by_length(fict_marvel), "network_measure")
  expect_s3_class(net_by_compactness(fict_marvel), "network_measure")
  # each equals the same measure over the positive ties taken explicitly
  positive <- manynet::to_unsigned(fict_marvel, keep = "positive")
  expect_equal(as.numeric(net_by_diameter(fict_marvel)),
               as.numeric(net_by_diameter(positive)))
  expect_equal(as.numeric(net_by_length(fict_marvel)),
               as.numeric(net_by_length(positive)))
  expect_equal(as.numeric(net_by_compactness(fict_marvel)),
               as.numeric(net_by_compactness(positive)))
  # and the negative ties really were excluded, not merely stripped of their
  # weight: a network keeping all 1241 ties but forgetting their signs gives a
  # different answer from the 960 positive ties alone
  expect_lt(manynet::net_ties(positive), manynet::net_ties(fict_marvel))
  signless <- igraph::delete_edge_attr(manynet::as_igraph(fict_marvel), "weight")
  expect_false(isTRUE(all.equal(as.numeric(net_by_length(fict_marvel)),
                                igraph::mean_distance(signless))))
})

test_that("an unsigned network is untouched by the sign handling", {
  expect_values(net_by_diameter(ison_adolescents), 4)
  expect_values(net_by_length(ison_adolescents), 2.071)
  expect_values(net_by_compactness(ison_adolescents), 0.616)
})
