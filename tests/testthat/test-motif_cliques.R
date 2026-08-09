test_that("node_x_clique finds the maximal cliques", {
  res <- node_x_clique(ison_adolescents)
  expect_s3_class(res, "node_motif")
  expect_equal(nrow(res), c(manynet::net_nodes(ison_adolescents)))
  # the same cliques igraph finds
  expect_equal(ncol(res),
               length(igraph::max_cliques(manynet::as_igraph(ison_adolescents),
                                          min = 3)))
  # every returned clique respects the minimum size
  expect_true(all(colSums(res) >= 3))
  expect_true(all(colSums(node_x_clique(ison_adolescents, min_clique_size = 4)) >= 4))
  # and every returned clique really is complete
  mat <- manynet::as_matrix(ison_adolescents)
  for (j in seq_len(ncol(res))) {
    members <- which(res[, j] == 1)
    sub <- mat[members, members]
    diag(sub) <- 1
    expect_true(all(sub == 1))
  }
})

test_that("node_x_clique finds bicliques in two-mode networks", {
  res <- node_x_clique(ison_southern_women, min_clique_size = c(3, 3))
  expect_s3_class(res, "node_motif")
  expect_equal(nrow(res), c(manynet::net_nodes(ison_southern_women)))
  modes <- manynet::node_is_mode(ison_southern_women)
  # each biclique draws at least the minimum from both modes
  expect_true(all(apply(res, 2, function(x)
    sum(x == 1 & !modes) >= 3 && sum(x == 1 & modes) >= 3)))
  # and is complete between the two modes
  mat <- manynet::as_matrix(ison_southern_women)
  for (j in seq_len(ncol(res))) {
    members <- which(res[, j] == 1)
    rows <- members[members <= nrow(mat)]
    cols <- members[members > nrow(mat)] - nrow(mat)
    expect_true(all(mat[rows, cols] == 1))
  }
})

test_that("node_x_clique handles networks with no cliques", {
  res <- node_x_clique(create_empty(6))
  expect_equal(dim(res), c(6L, 0L))
})
