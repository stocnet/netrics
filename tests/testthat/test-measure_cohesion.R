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