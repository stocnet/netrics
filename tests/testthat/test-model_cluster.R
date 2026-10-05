test_that("cluster_hierarchical compares nodes once by default", {
  census <- node_x_tie(ison_algebra)
  one <- cluster_hierarchical(census)
  expect_s3_class(one, "hclust")
  expect_equal(dim(one$proximity), rep(c(net_nodes(ison_algebra)), 2))
  expect_equal(c(as.matrix(one$distances))[2], 
               1 - cor(census[1,], census[2,]))
  two <- cluster_hierarchical(census, distance = "euclidean")
  expect_s3_class(two, "hclust")
  expect_false(isTRUE(all.equal(c(one$distances), c(two$distances))))
})

test_that("cluster_hierarchical keeps unbounded proximities non-negative", {
  census <- node_x_tie(ison_algebra)
  old <- options(manynet_verbosity = "verbose", snet_verbosity = "verbose")
  expect_message(hc <- cluster_hierarchical(census, proximity = "crossmin"),
                 "unbounded")
  options(old)
  expect_true(all(hc$height >= 0))
  expect_true(all(hc$distances >= 0))
})

test_that("cluster_cosine is deprecated", {
  expect_warning(hc <- cluster_cosine(node_x_triad(ison_monks)), "deprecated")
  expect_s3_class(hc, "hclust")
  expect_equal(hc$proximity,
               cluster_hierarchical(node_x_triad(ison_monks),
                                    proximity = "cosine")$proximity)
})

test_that("cluster_cosine clusters nodes and not census features", {
  # `ison_algebra` gives a 16 x 96 census,
  # so a membership of the wrong length is visible here
  expect_equal(length(suppressWarnings(node_in_structural(ison_algebra, 
                                                          cluster = "cosine"))),
               c(net_nodes(ison_algebra)))
  expect_equal(nrow(as.matrix(suppressWarnings(
    cluster_cosine(node_x_tie(ison_algebra)))$distances)),
    c(net_nodes(ison_algebra)))
})

test_that("cluster_concor works", {
  unlab_2mode <- generate_random(c(6,6))
  expect_s3_class(cluster_concor(unlab_2mode, node_x_tetrad(unlab_2mode)), "hclust")
})
