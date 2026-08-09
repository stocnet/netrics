test_that("net_modularity works for two mode networks", {
  out <- net_by_modularity(ison_southern_women,
                 node_in_partition(ison_southern_women))
  expect_length(out, 1)
})

test_that("net_core works", {
  out <- net_by_core(ison_adolescents)
  expect_values(out, -0.133)
  expect_values(net_by_core(ison_adolescents, method = "ident"), 6.481)
  expect_values(net_by_core(ison_adolescents, method = "diff"), 6.094)
})

test_that("net_by_inconsistency scores a partition against ideal blocks", {
  m <- node_in_structural(ison_adolescents, k = 3)
  expect_s3_class(net_by_inconsistency(ison_adolescents, m), "network_measure")
  # a "do not care" vocabulary can never be inconsistent
  expect_equal(as.numeric(net_by_inconsistency(ison_adolescents, m, blocks = "dnc")), 0)
  # a partition that exactly reproduces components fits perfectly
  pf <- create_components(create_filled(6), membership = c(1, 1, 1, 2, 2, 2))
  expect_equal(as.numeric(net_by_inconsistency(pf, c(1, 1, 1, 2, 2, 2))), 0)
  # permitting more ideal types can only lower the criterion
  expect_lte(
    as.numeric(net_by_inconsistency(ison_adolescents, m,
                             blocks = c("nul", "com", "reg"))),
    as.numeric(net_by_inconsistency(ison_adolescents, m)))
  # a fitted partition beats a random one
  set.seed(1)
  expect_lt(as.numeric(net_by_inconsistency(ison_adolescents, m)),
            as.numeric(net_by_inconsistency(ison_adolescents,
                                     sample(rep(1:3, length.out = 8)))))
  # a generalized, per-position vocabulary is accepted
  b <- matrix(list(), 2, 2)
  b[[1, 1]] <- "reg"; b[[2, 2]] <- "reg"
  b[[1, 2]] <- "nul"; b[[2, 1]] <- "nul"
  expect_s3_class(net_by_inconsistency(ison_adolescents,
                                node_in_structural(ison_adolescents, k = 2),
                                blocks = b), "network_measure")
})

test_that("net_by_inconsistency behaves as a distance, as documented", {
  # bounded in [0,1] for cell-counting vocabularies, since each cell can
  # contribute at most one error
  for (k in 2:8) {
    v <- as.numeric(net_by_inconsistency(ison_adolescents,
                                  cut(seq_len(8), k, labels = FALSE),
                                  blocks = c("nul", "com")))
    expect_gte(v, 0)
    expect_lte(v, 1)
  }
  # but NOT bounded by 1 once `reg` is permitted, because line counts are
  # divided by a cell count
  expect_gt(as.numeric(net_by_inconsistency(ison_adolescents, seq_len(8),
                                     blocks = "reg")), 1)
  # and the vocabularies invert at the finest partition: every block is one
  # cell, so trivially null-or-complete, but never regular
  expect_equal(as.numeric(net_by_inconsistency(ison_adolescents, seq_len(8),
                                        blocks = c("nul", "com"))), 0)
  expect_gt(as.numeric(net_by_inconsistency(ison_adolescents, seq_len(8),
                                     blocks = "reg")), 0)
})
