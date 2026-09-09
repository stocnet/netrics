#*************** Test the heterogeneity family of functions ******************#

test_that("diversity functions works", {
  expect_equal(as.numeric(net_by_diversity(to_uniplex(fict_marvel,"relationship"), "Gender")), 
               0.306, tolerance = 0.001)
  expect_equal(top3(node_by_diversity(ison_lawfirm, "gender")),
               c(0.285, 0.375,0), tolerance = 0.01)
})

test_that("heterophily function works", {
  expect_equal(as.numeric(net_by_heterophily(ison_networkers, "Discipline")), .1704, tolerance = 0.001)
  expect_length(node_by_heterophily(ison_networkers, "Discipline"),
                net_nodes(ison_networkers))
  expect_s3_class(node_by_heterophily(ison_networkers, "Discipline"), "node_measure")
})

test_that("assortativity function works", {
  expect_length(net_by_assortativity(ison_networkers), 1)
})

test_that("richness function works", {
  expect_length(net_by_richness(ison_networkers), 1)
  expect_equal(as.numeric(net_by_richness(ison_networkers)), 3)
  expect_length(node_by_richness(ison_networkers, "type"), 32)
})

test_that("net_spatial works", {
  expect_values(net_by_spatial(ison_lawfirm, "age"), 0.126)
})

test_that("net_by_spatial() names a non-numeric attribute", {
  # Moran's I correlates a quantity across ties, so a category cannot be read
  expect_error(net_by_spatial(ison_lawfirm, "practice"), "numeric")
})

# A two-mode network has no ties within a mode, so `net_by_spatial()` has to
# reshape it before it can correlate an attribute across ties. Which reshaping
# it picks depends on where the attribute sits, and these two tests hold it to
# the matrix each route is meant to read.

test_that("net_by_spatial() projects a two-mode network onto the attribute's mode", {
  set.seed(2025)
  tm <- igraph::set_vertex_attr(as_igraph(ison_southern_women), "age",
                                value = c(round(rnorm(18, 40, 8), 1),
                                          rep(NA_real_, 14)))
  proj <- to_mode(tm, mode = 1)
  w <- as_matrix(proj)
  x <- node_attribute(proj, "age")
  ref <- (18 / sum(w)) * sum(w * outer(x - mean(x), x - mean(x))) /
    sum((x - mean(x))^2)
  expect_values(net_by_spatial(tm, "age"), ref)
})

test_that("net_by_spatial() reads the multilevel matrix where both modes hold the attribute", {
  set.seed(2025)
  tm <- igraph::set_vertex_attr(as_igraph(ison_southern_women), "age",
                                value = round(rnorm(32, 40, 8), 1))
  w <- as_matrix(to_multilevel(tm))
  x <- node_attribute(tm, "age")
  ref <- (32 / sum(w)) * sum(w * outer(x - mean(x), x - mean(x))) /
    sum((x - mean(x))^2)
  expect_values(net_by_spatial(tm, "age"), ref)
})

test_that("net_by_spatial() drops nodes with a missing attribute", {
  ring <- igraph::set_vertex_attr(create_ring(8), "v", value = c(1:7, NA))
  # The four retained ties of the 1:7 path give 2/3, where reading the missing
  # value into the sums would give NA for the whole network.
  expect_values(net_by_spatial(ring, "v"), 0.667)
  local_verbose()
  expect_message(net_by_spatial(ring, "v"), "Dropping")
})

test_that("net_by_spatial() returns NA where the attribute has no variance", {
  flat <- igraph::set_vertex_attr(create_ring(6), "v", value = rep(3, 6))
  expect_true(is.na(as.numeric(net_by_spatial(flat, "v"))))
})

test_that("node_by_homophily() reports Geary's C, and reads a name and a vector alike", {
  set.seed(2025)
  ring <- create_ring(8)
  x <- round(rnorm(8, 50, 10), 2)
  ring <- igraph::set_vertex_attr(ring, "v", value = x)
  named <- node_by_homophily(ring, "v", assortativity = "geary")
  expect_equal(attr(named, "measure"), "Geary's C")
  expect_equal(attr(named, "variant"), "geary")
  # `igraph::ego()` lists the ego first and `induced_subgraph()` does not, so
  # a vector attribute used to be read against the wrong nodes.
  expect_equal(as.numeric(named),
               as.numeric(node_by_homophily(ring, x, assortativity = "geary")))
})

test_that("Geary's C reads tie weights, and declares its range accordingly", {
  ring <- create_ring(5)
  igraph::E(ring)$weight <- c(1, 2, 3, 4, 5)
  ring <- igraph::set_vertex_attr(ring, "v", value = c(1, 2, 3, 4, 5))
  weighted <- net_by_homophily(ring, "v", assortativity = "geary")
  unweighted <- net_by_homophily(to_unweighted(ring), "v",
                                 assortativity = "geary")
  expect_values(weighted, 1.2)
  expect_values(unweighted, 0.8)
  # A heavy tie between dissimilar values can carry a weighted C past 2.
  expect_equal(attr(weighted, "range"), c(0, Inf))
  expect_equal(attr(unweighted, "range"), c(0, 2))
})

test_that("net_by_diversity() substitutes Blau's index for a factor attribute", {
  fct <- igraph::set_vertex_attr(create_ring(6), "f",
                                 value = factor(c("a", "a", "b", "b", "c", "c")))
  # A factor is neither numeric nor character, and used to reach `gini()`,
  # which errors on one.
  res <- net_by_diversity(fct, "f", diversity = "gini")
  expect_equal(attr(res, "measure"), "Blau's index")
  expect_values(res, 0.667)
})
