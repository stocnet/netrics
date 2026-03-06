test_that("cluster_cosine works", {
  expect_s3_class(cluster_cosine(node_x_triad(ison_monks), distance = "euclidean"), "hclust")
})

test_that("cluster_cosine works", {
  unlab_2mode <- generate_random(c(6,6))
  expect_s3_class(cluster_concor(unlab_2mode, node_x_tetrad(unlab_2mode)), "hclust")
})
