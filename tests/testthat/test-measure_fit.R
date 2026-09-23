test_that("net_modularity works for two mode networks", {
  out <- net_by_modularity(ison_southern_women,
                 node_in_partition(ison_southern_women))
  expect_length(out, 1)
})

test_that("net_core works", {
  out <- net_by_core(ison_adolescents)
  expect_values(out, -0.133)
  expect_values(net_by_core(ison_adolescents, variant = "ident"), 6.481)
  expect_values(net_by_core(ison_adolescents, variant = "diff"), 5.619)
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

test_that("net_by_divergence is 0 against the network itself", {
  for(v in c("hamming", "jaccard", "portrait"))
    expect_equal(as.numeric(net_by_divergence(ison_adolescents,
                                              ison_adolescents, v)), 0)
  expect_equal(as.numeric(net_by_divergence(manynet::create_ring(10),
                                            manynet::create_ring)), 0)
})

test_that("net_by_divergence gives the expected values", {
  full <- manynet::create_filled(10)
  expect_equal(as.numeric(net_by_divergence(full, manynet::create_empty,
                                            "hamming")), 1)
  expect_equal(as.numeric(net_by_divergence(full, manynet::create_empty,
                                            "jaccard")), 1)
  # two networks without any ties are identical
  expect_equal(as.numeric(net_by_divergence(manynet::create_empty(5),
                                            manynet::create_empty,
                                            "jaccard")), 0)
  # two ties shared of four in either
  a <- manynet::create_explicit(A-B, B-C, C-D)
  b <- manynet::create_explicit(A-B, B-C, A-D)
  expect_equal(as.numeric(net_by_divergence(a, b, "jaccard")), 0.5)
  # by hand, a path of three against a triangle is (2/9 + 6/9*log2(3/2))/2
  p3 <- manynet::create_explicit(A-B, B-C)
  tri <- manynet::create_explicit(A-B, B-C, C-A)
  expect_equal(as.numeric(net_by_divergence(p3, tri, "portrait")),
               (2/9 + 6/9*log2(3/2))/2)
  expect_lt(as.numeric(net_by_divergence(manynet::create_star(10),
                                         manynet::create_star)),
            as.numeric(net_by_divergence(manynet::create_ring(10),
                                         manynet::create_star)))
})

test_that("net_by_divergence stays within 0 and 1", {
  for(net in list(ison_adolescents, ison_algebra, ison_southern_women))
    for(v in c("hamming", "jaccard", "portrait")){
      out <- as.numeric(net_by_divergence(net, manynet::create_filled, v))
      expect_gte(out, 0)
      expect_lte(out, 1)
    }
})

test_that("net_by_divergence falls back to portrait where nodes do not correspond", {
  local_verbose()
  expect_message(out <- net_by_divergence(ison_adolescents,
                                          manynet::create_star, "hamming"),
                 "portrait")
  expect_equal(attr(out, "variant"), "portrait")
  expect_no_message(out <- net_by_divergence(ison_adolescents,
                                             manynet::create_star))
  expect_equal(attr(out, "variant"), "portrait")
  expect_equal(attr(net_by_divergence(ison_adolescents), "variant"), "hamming")
  expect_message(net_by_divergence(ison_algebra, variant = "hamming"),
                 "symmetric")
})

test_that("net_by_divergence matches net_by_inconsistency where it should", {
  # two cliques of four, with one tie missing within and one added between,
  # so every block is best fitted as complete on the diagonal and null off it
  cl <- manynet::create_explicit(A-C, A-D, B-C, B-D, C-D,
                                 E-F, E-G, E-H, F-G, F-H, G-H, D-E)
  m <- c(1, 1, 1, 1, 2, 2, 2, 2)
  expect_equal(as.numeric(net_by_divergence(cl, manynet::create_components,
                                            membership = m)),
               as.numeric(net_by_inconsistency(cl, m)))
  expect_equal(as.numeric(net_by_inconsistency(cl, m)), 2/28)
})
