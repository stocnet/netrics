test_that("cluster_cosine works", {
  expect_s3_class(cluster_cosine(node_x_triad(ison_monks), distance = "euclidean"), "hclust")
})

test_that("cluster_cosine clusters nodes and not census features", {
  # `ison_algebra` gives a 16 x 96 census,
  # so a membership of the wrong length is visible here
  expect_equal(length(node_in_structural(ison_algebra, cluster = "cosine")),
               c(net_nodes(ison_algebra)))
  expect_equal(nrow(as.matrix(cluster_cosine(node_x_tie(ison_algebra),
                                             distance = "euclidean")$distances)),
               c(net_nodes(ison_algebra)))
})

test_that("cluster_concor works", {
  unlab_2mode <- generate_random(c(6,6))
  expect_s3_class(cluster_concor(unlab_2mode, node_x_tetrad(unlab_2mode)), "hclust")
})
