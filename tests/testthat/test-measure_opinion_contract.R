# Opinion measures read the first opinions from the nodal attribute
# "opinions", as `manynet::play_opinion()` does.
set.seed(2024)
teens <- manynet::add_node_attribute(manynet::ison_adolescents, "opinions",
                                     stats::runif(8))

test_that("node opinion measures meet the measure contract", {
  check_measure_contract(measure_rosters$opinion_node, teens, level = "node")
  expect_declared(measure_rosters$opinion_node, teens)
})

test_that("network opinion measures meet the measure contract", {
  check_measure_contract(measure_rosters$opinion_net, teens, level = "net")
  expect_declared(measure_rosters$opinion_net, teens)
})

test_that("dispersion measures meet the measure contract", {
  check_measure_contract(measure_rosters$heterogeneity_dispersion,
                         manynet::ison_lawfirm, level = "net")
  expect_declared(measure_rosters$heterogeneity_dispersion, manynet::ison_lawfirm)
})

test_that("concentration measures meet the measure contract", {
  check_measure_contract(measure_rosters$heterogeneity_concentration_net,
                         manynet::ison_lawfirm, level = "net")
  expect_declared(measure_rosters$heterogeneity_concentration_net,
                  manynet::ison_lawfirm)
  check_measure_contract(measure_rosters$heterogeneity_concentration_node,
                         manynet::ison_lawfirm, level = "node")
  expect_declared(measure_rosters$heterogeneity_concentration_node,
                  manynet::ison_lawfirm)
})
