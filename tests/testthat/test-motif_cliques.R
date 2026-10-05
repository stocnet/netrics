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

test_that("node_x_percolation finds overlapping communities", {
  # two triangles that share only node 3, and two that share the tie 6-7
  net <- manynet::as_tidygraph(igraph::graph_from_literal(
    1-2, 1-3, 2-3, 3-4, 3-5, 4-5,
    6-7, 6-8, 7-8, 6-9, 7-9, simplify = TRUE))
  res <- node_x_percolation(net)
  expect_s3_class(res, "node_motif")
  expect_equal(nrow(res), c(manynet::net_nodes(net)))
  # triangles that share a tie are one community, those that share a node two
  expect_equal(ncol(res), 3)
  expect_equal(sort(unname(colSums(res))), c(3, 3, 4))
  # a partition would give every node one community at most
  expect_equal(unname(rowSums(res)), c(1, 1, 2, 1, 1, 1, 1, 1, 1))
  # larger cliques must share more nodes to be joined
  expect_equal(ncol(node_x_percolation(net, min_clique_size = 4)), 0)
})

test_that("node_x_percolation joins the cliques node_x_clique finds", {
  cliques <- node_x_clique(ison_adolescents)
  res <- node_x_percolation(ison_adolescents)
  expect_lte(ncol(res), ncol(cliques))
  # the same nodes are covered either way
  expect_equal(unname(rowSums(res) > 0), unname(rowSums(cliques) > 0))
  expect_s3_class(node_x_percolation(ison_southern_women), "node_motif")
})

test_that("node_x_percolation handles networks with no cliques", {
  res <- node_x_percolation(create_empty(6))
  expect_equal(dim(res), c(6L, 0L))
})
