test_that("concentration finds hot spots and cold spots", {
  path <- manynet::add_node_attribute(manynet::create_ring(6), "x",
                                      c(1, 1, 1, 0, 0, 0))
  path <- manynet::delete_ties(path, 6)  # the tie from 6 back to 1
  gi <- as.numeric(node_by_concentration(path, "x"))
  expect_true(gi[1] > 0 && gi[6] < 0)
  # by hand: node 2 has itself and nodes 1 and 3 in its neighbourhood
  n <- 6; x <- c(1, 1, 1, 0, 0, 0); s <- sqrt(sum(x^2)/n - mean(x)^2)
  expect_equal(gi[2], (3 - 0.5 * 3) / (s * sqrt((n * 3 - 9) / (n - 1))))
  # local Moran's I is positive at both ends, where Gi* has opposite signs
  moran <- as.numeric(node_by_homophily(path, "x", assortativity = "moran"))
  expect_true(moran[1] > 0 && moran[6] > 0)
})

test_that("general G is the share of the products of values on ties", {
  path <- manynet::add_node_attribute(manynet::create_explicit(A-B-C), "x",
                                      c(1, 2, 3))
  # products on ties: 2 and 6, twice each; all products: 2, 3, 6, twice each
  expect_equal(as.numeric(net_by_concentration(path, "x")), 16 / 22)
  neg <- manynet::add_node_attribute(path, "y", c(-1, 0, 1))
  expect_error(net_by_concentration(neg, "y"), "0 or more")
})
