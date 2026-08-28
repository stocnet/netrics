test_that("node hole measures meet the measure contract", {
  check_measure_contract(measure_rosters$holes_node,
                         manynet::ison_adolescents, level = "node")
  expect_declared(measure_rosters$holes_node, manynet::ison_adolescents)
})

test_that("node hole measures meet the contract on two-mode data", {
  check_measure_contract(measure_rosters$holes_node[c("node_by_efficiency",
                                                      "node_by_constraint",
                                                      "node_by_hierarchy")],
                         manynet::ison_southern_women, level = "node")
})

test_that("tie hole measures meet the measure contract", {
  check_measure_contract(measure_rosters$holes_tie,
                         manynet::ison_adolescents, level = "tie")
  expect_declared(measure_rosters$holes_tie, manynet::ison_adolescents)
})
