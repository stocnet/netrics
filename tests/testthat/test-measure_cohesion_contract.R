test_that("network cohesions meet the measure contract", {
  check_measure_contract(measure_rosters$cohesion_net,
                         manynet::ison_adolescents, level = "net")
  expect_declared(measure_rosters$cohesion_net, manynet::ison_adolescents)
})

test_that("network fragmentations meet the measure contract", {
  ring <- manynet::create_ring(6)
  check_measure_contract(measure_rosters$fragmentation_net, ring, level = "net")
  expect_declared(measure_rosters$fragmentation_net, ring)
})

test_that("density stays a proportion whatever it is given", {
  # The two-mode branch counts ties rather than summing weights, so a weighted
  # network cannot report a density above 1.
  sw <- manynet::ison_southern_women
  w <- manynet::mutate_ties(sw, weight = rep(3, manynet::net_ties(sw)))
  expect_equal(as.numeric(net_by_density(w)), as.numeric(net_by_density(sw)))
  expect_lte(as.numeric(net_by_density(w)), 1)
  expect_equal(as.numeric(net_by_density(manynet::create_filled(c(10, 6)))), 1)
  expect_equal(as.numeric(net_by_density(manynet::create_empty(c(10, 6)))), 0)
})
