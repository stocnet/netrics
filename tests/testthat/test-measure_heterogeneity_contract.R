# `marvel_friends` has both a categorical ("Gender") and a numeric
# ("Appearances") attribute, which is what makes the runtime index
# substitution testable.

marvel_friends <- manynet::to_unsigned(
  manynet::to_uniplex(manynet::fict_marvel, "relationship"), "positive")

test_that("network heterogeneities meet the measure contract", {
  check_measure_contract(measure_rosters$heterogeneity_net, marvel_friends,
                         level = "net")
  expect_declared(measure_rosters$heterogeneity_net, marvel_friends)
})

test_that("node heterogeneities meet the measure contract", {
  check_measure_contract(measure_rosters$heterogeneity_node, marvel_friends,
                         level = "node")
  expect_declared(measure_rosters$heterogeneity_node, marvel_friends)
})

test_that("spatial autocorrelation meets the measure contract", {
  check_measure_contract(measure_rosters$heterogeneity_spatial,
                         manynet::ison_lawfirm, level = "net")
  expect_declared(measure_rosters$heterogeneity_spatial, manynet::ison_lawfirm)
})

test_that("measures report the index they actually used", {
  # Blau's index is inapplicable to a numeric attribute, so the function
  # substitutes the coefficient of variation. The reported label is the only
  # record of that substitution, so it must follow the substitution.
  expect_equal(attr(net_by_diversity(marvel_friends, "Gender"), "measure"),
               "Blau's index")
  cv <- net_by_diversity(marvel_friends, "Appearances")
  expect_equal(attr(cv, "measure"), "coefficient of variation")
  expect_equal(attr(cv, "normalization"), "none")
  # The variant is read off the resolved index too, so it records the
  # substitution rather than the index that was asked for.
  expect_equal(attr(cv, "variant"), "variation")
  expect_equal(attr(net_by_diversity(marvel_friends, "Gender"), "variant"),
               "blau")
  expect_equal(attr(net_by_homophily(marvel_friends, "Gender",
                                     assortativity = "yule"), "variant"),
               "yule")
  expect_equal(attr(net_by_diversity(marvel_friends, "Appearances",
                                     diversity = "gini"), "measure"),
               "Gini coefficient")
  # And the same for the assortativity indices.
  expect_equal(attr(net_by_homophily(marvel_friends, "Gender"), "measure"),
               "IE index")
  expect_equal(attr(net_by_homophily(marvel_friends, "Gender",
                                     assortativity = "yule"), "measure"),
               "Yule's Q")
})
