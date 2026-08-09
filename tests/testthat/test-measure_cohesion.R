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
