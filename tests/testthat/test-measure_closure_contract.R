test_that("network closures meet the measure contract", {
  check_measure_contract(measure_rosters$closure_net, manynet::ison_networkers,
                         level = "net")
  expect_declared(measure_rosters$closure_net, manynet::ison_networkers)
})

test_that("node closures meet the measure contract", {
  check_measure_contract(measure_rosters$closure_node,
                         manynet::ison_adolescents, level = "node")
  expect_declared(measure_rosters$closure_node, manynet::ison_adolescents)
})

test_that("closures stay bounded on two-mode and weighted networks", {
  # These are proportions of configurations, so tie weights must not be able to
  # push them above 1 - the reason both dichotomise their input.
  sw <- manynet::ison_southern_women
  expect_lte(as.numeric(net_by_equivalency(sw)), 1)
  expect_true(all(as.numeric(node_by_equivalency(sw)) <= 1))
  nw <- manynet::ison_networkers
  expect_lte(as.numeric(net_by_reciprocity(nw)), 1)
  expect_true(all(as.numeric(node_by_reciprocity(nw)) <= 1))
})

test_that("reciprocity records which of its two methods ran", {
  # Both methods are normalised proportions in [0,1], but of different things,
  # so the variant is what distinguishes the results rather than the range.
  nw <- manynet::ison_networkers
  expect_equal(attr(net_by_reciprocity(nw), "variant"), "default")
  expect_equal(attr(net_by_reciprocity(nw, variant = "ratio"), "variant"), "ratio")
  expect_equal(attr(net_by_reciprocity(nw, variant = "ratio"), "normalization"),
               "normalized")
  # A variant that says nothing about the values would be decorative; these
  # two genuinely differ.
  expect_false(isTRUE(all.equal(as.numeric(net_by_reciprocity(nw)),
                                as.numeric(net_by_reciprocity(nw, variant = "ratio")))))
  # Unrecognised methods are now caught here rather than passed to igraph.
  expect_error(net_by_reciprocity(nw, variant = "nonsense"))
})

test_that("congruency meets the measure contract", {
  # Congruency spans two two-mode networks, so the second mode of the first
  # must match the first mode of the second. That shape does not fit the
  # roster sweep, so it is checked here instead.
  ring <- manynet::create_ring(c(10, 10))
  res <- net_by_congruency(ring, ring)
  expect_s3_class(res, "network_measure")
  expect_equal(attr(res, "measure"), "congruency")
  expect_equal(attr(res, "range"), c(0, 1))
  expect_equal(attr(res, "normalization"), "normalized")
})
