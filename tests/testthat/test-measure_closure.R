test_that("network density works", {
  expect_equal(as.numeric(net_by_density(create_empty(10))), 0)
  expect_equal(as.numeric(net_by_density(create_empty(c(10,6)))), 0)
  expect_equal(as.numeric(net_by_density(create_filled(10))), 1)
  expect_equal(as.numeric(net_by_density(create_filled(c(10,6)))), 1)
  expect_output(print(net_by_density(create_filled(10))))
})

test_that("one-mode object clustering is reported correctly",{
  expect_equal(as.numeric(net_by_transitivity(ison_algebra)),
               0.69787, tolerance = 0.001)
})

test_that("two-mode object clustering is reported correctly",{
  expect_equal(as.numeric(net_by_equivalency(ison_southern_women)),
               0.4677, tolerance = 0.001)
  expect_values(net_by_equivalency(ison_adolescents), 0.258)
})

test_that("three-mode clustering calculated correctly",{
  mat1 <- manynet::create_ring(c(10,5))
  mat2 <- manynet::create_ring(c(5,8))
  expect_equal(as.numeric(net_by_congruency(mat1, mat2)),
               0.3684, tolerance = 0.001)
})

test_that("network cyclicality works", {
  # a pure 3-cycle is fully cyclical but not transitive
  cyc <- matrix(c(0,1,0, 0,0,1, 1,0,0), 3, 3, byrow = TRUE)
  expect_equal(as.numeric(net_by_cyclicality(cyc)), 1)
  # a transitive triple is the reverse
  tri <- matrix(c(0,1,1, 0,0,1, 0,0,0), 3, 3, byrow = TRUE)
  expect_equal(as.numeric(net_by_cyclicality(tri)), 0)
  # undirected networks close two-paths in both directions equally
  expect_equal(as.numeric(net_by_cyclicality(ison_adolescents)),
               as.numeric(net_by_transitivity(ison_adolescents)))
  expect_equal(as.numeric(net_by_cyclicality(ison_networkers)),
               0.5912, tolerance = 0.001)
  expect_output(print(net_by_cyclicality(ison_networkers)))
})
