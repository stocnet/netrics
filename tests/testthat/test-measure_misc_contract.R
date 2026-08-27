# Hierarchy, change, brokerage, and coreness: small families that share the
# same fixtures rather than warranting a roster file each.

test_that("hierarchy measures meet the measure contract", {
  check_measure_contract(measure_rosters$hierarchy_net,
                         manynet::ison_networkers, level = "net")
  expect_declared(measure_rosters$hierarchy_net, manynet::ison_networkers)
})

test_that("the hierarchy dimensions share a scale", {
  # net_x_hierarchy() only compares its four dimensions meaningfully if they
  # all run from 0 to 1.
  out <- net_x_hierarchy(manynet::ison_networkers)
  expect_true(all(vapply(out, function(x) x >= 0 && x <= 1,
                         FUN.VALUE = logical(1))))
  # A tree carries no ties beyond those that connect it; a complete network
  # carries every tie it could.
  expect_equal(as.numeric(net_by_efficiency(manynet::create_tree(8))), 1)
  expect_equal(as.numeric(net_by_efficiency(manynet::create_filled(6))), 0)
})

test_that("coreness measures meet the measure contract", {
  check_measure_contract(measure_rosters$core_node,
                         manynet::ison_adolescents, level = "node")
  expect_declared(measure_rosters$core_node, manynet::ison_adolescents)
})

test_that("brokerage measures meet the measure contract", {
  check_measure_contract(measure_rosters$brokerage_node,
                         manynet::ison_networkers, level = "node")
  expect_declared(measure_rosters$brokerage_node, manynet::ison_networkers)
})

test_that("net_by_waves meets the measure contract", {
  check_measure_contract(measure_rosters$change_net, manynet::fict_thrones,
                         level = "net")
  expect_declared(measure_rosters$change_net, manynet::fict_thrones)
})
