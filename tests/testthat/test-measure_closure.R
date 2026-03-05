test_that("network density works", {
  expect_equal(as.numeric(net_density(create_empty(10))), 0)
  expect_equal(as.numeric(net_density(create_empty(c(10,6)))), 0)
  expect_equal(as.numeric(net_density(create_filled(10))), 1)
  expect_equal(as.numeric(net_density(create_filled(c(10,6)))), 1)
  expect_output(print(net_density(create_filled(10))))
})

test_that("one-mode object clustering is reported correctly",{
  expect_equal(as.numeric(net_transitivity(ison_algebra)),
               0.69787, tolerance = 0.001)
})

test_that("two-mode object clustering is reported correctly",{
  expect_equal(as.numeric(net_equivalency(ison_southern_women)),
               0.4677, tolerance = 0.001)
  expect_values(net_equivalency(ison_adolescents), 0.258)
})

test_that("node_equivalency works correctly",{
  expect_equal(as.numeric(node_equivalency(ison_laterals$ison_mm)),
               c(0,1,1,0,0.5,0.5), tolerance = 0.001)
})

test_that("three-mode clustering calculated correctly",{
  mat1 <- manynet::create_ring(c(10,5))
  mat2 <- manynet::create_ring(c(5,8))
  expect_equal(as.numeric(net_congruency(mat1, mat2)),
               0.3684, tolerance = 0.001)
})
