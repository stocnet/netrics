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

test_that("node transitivity offers weighted clustering coefficients", {
  # Without weights, every variant is the Watts-Strogatz coefficient,
  # and the default says so.
  adol <- node_by_transitivity(ison_adolescents)
  expect_equal(attr(adol, "variant"), "watts")
  expect_equal(as.numeric(adol),
               unname(igraph::transitivity(manynet::as_igraph(ison_adolescents),
                                           type = "local")))
  for (v in c("barrat", "onnela", "zhang"))
    expect_equal(as.numeric(node_by_transitivity(ison_adolescents, variant = v)),
                 as.numeric(adol))
  # A weighted triangle A-B-C with a pendant D, checked by hand
  W <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  W["A","B"] <- 1; W["B","C"] <- 2; W["C","A"] <- 3; W["C","D"] <- 4
  W <- W + t(W)
  tri <- manynet::as_igraph(W)
  expect_equal(as.numeric(node_by_transitivity(tri, variant = "barrat")),
               c(1, 1, 0.2778, NaN), tolerance = 0.001)
  expect_equal(as.numeric(node_by_transitivity(tri, variant = "onnela")),
               c(0.4543, 0.4543, 0.1514, NaN), tolerance = 0.001)
  expect_equal(as.numeric(node_by_transitivity(tri, variant = "zhang")),
               c(0.5, 0.75, 0.0577, NaN), tolerance = 0.001)
  # Barrat's is also in igraph, which it should agree with
  set.seed(1)
  rg <- igraph::sample_gnp(15, 0.4)
  igraph::E(rg)$weight <- stats::runif(igraph::ecount(rg), 1, 5)
  expect_equal(as.numeric(node_by_transitivity(rg, variant = "barrat")),
               igraph::transitivity(rg, type = "barrat"))
  # A weighted network defaults to Barrat's, and every variant stays bounded
  expect_equal(attr(node_by_transitivity(ison_networkers), "variant"), "barrat")
  for (v in c("watts", "barrat", "onnela", "zhang")) {
    vals <- as.numeric(node_by_transitivity(ison_networkers, variant = v))
    vals <- vals[is.finite(vals)]
    expect_true(all(vals >= 0 & vals <= 1))
  }
  # A two-mode network contains no triangles
  for (v in c("watts", "barrat", "onnela", "zhang")) {
    vals <- as.numeric(node_by_transitivity(ison_southern_women, variant = v))
    expect_true(all(vals[!is.nan(vals)] == 0))
  }
})
