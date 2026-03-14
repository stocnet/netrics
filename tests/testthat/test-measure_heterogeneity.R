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