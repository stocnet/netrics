test_that("network features meet the measure contract", {
  check_measure_contract(measure_rosters$features_net,
                         manynet::ison_adolescents, level = "net")
  expect_declared(measure_rosters$features_net, manynet::ison_adolescents)
})

test_that("structural fit measures meet the measure contract", {
  check_measure_contract(measure_rosters$fit_net, manynet::ison_adolescents,
                         level = "net")
  expect_declared(measure_rosters$fit_net, manynet::ison_adolescents)
})

test_that("structural balance meets the measure contract", {
  signed <- manynet::to_uniplex(manynet::fict_marvel, "relationship")
  check_measure_contract(measure_rosters$features_balance, signed, level = "net")
  expect_declared(measure_rosters$features_balance, signed)
})

test_that("measures on several scales report which one they are on", {
  # net_by_core()'s methods return a correlation, a distance, and two signed
  # differences, so a single range would be wrong for three of the four.
  g <- manynet::ison_adolescents
  expect_equal(attr(net_by_core(g), "range"), c(-1, 1))
  expect_equal(attr(net_by_core(g, variant = "ident"), "measure"),
               "core-periphery distance")
  expect_equal(attr(net_by_core(g, variant = "ident"), "range"), c(0, Inf))
  # Sigma is a ratio of ratios with no upper bound; omega and SWI are bounded.
  expect_equal(attr(net_by_smallworld(g, variant = "sigma", times = 20), "range"),
               c(0, Inf))
  # Which of the three coefficients ran is recorded as a variant rather than
  # spelled into the measure name, so the measure stays the same across them.
  expect_equal(attr(net_by_smallworld(g, times = 20), "measure"),
               "small-world coefficient")
  expect_equal(attr(net_by_smallworld(g, times = 20), "variant"), "omega")
  expect_equal(attr(net_by_smallworld(g, variant = "SWI", times = 20), "variant"),
               "SWI")
  # A variant is orthogonal to a normalisation: SWI is both.
  expect_equal(attr(net_by_smallworld(g, variant = "SWI", times = 20),
                    "normalization"), "normalized")
  expect_equal(attr(net_by_core(g, variant = "ident"), "variant"), "ident")
  # The modularity floor moves with the resolution, so the range follows it.
  memb <- node_in_partition(g)
  expect_equal(attr(net_by_modularity(g, memb), "range"), c(-0.5, 1))
  expect_equal(attr(net_by_modularity(g, memb, resolution = 2), "range"),
               c(-Inf, 1))
})
