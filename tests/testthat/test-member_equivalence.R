# # Equivalence clustering tests

test_that("equivalence clustering returns the right class", {
  expect_s3_class(node_in_structural(ison_adolescents, "strict", "hier"), "node_member")
  expect_s3_class(node_in_regular(ison_adolescents), "node_member")
  expect_s3_class(node_in_automorphic(ison_adolescents), "node_member")
  testthat::skip_if_not_installed("sna")
  expect_s3_class(node_in_structural(ison_adolescents, "elbow", "hier"), "node_member")
  expect_s3_class(node_in_structural(ison_adolescents, "elbow", "concor"), "node_member")
})

test_that("equivalence clustering works", {
  expect_equal(node_in_structural(ison_adolescents, "silhouette", "hier"), node_in_structural(ison_adolescents))
  expect_equal(node_in_regular(ison_adolescents), node_in_regular(ison_adolescents, "silhouette", "hier"))
  expect_equal(c(net_nodes(ison_adolescents)), length(node_in_structural(ison_adolescents, "silhouette", "concor")))
  expect_equal(c(net_nodes(ison_adolescents)), length(node_in_structural(ison_adolescents, k = 3, "hier")))
  expect_equal(c(net_nodes(ison_adolescents)), length(node_in_structural(ison_adolescents, "strict", "concor")))
  expect_equal(c(net_nodes(ison_adolescents)), length(node_in_regular(ison_adolescents, cluster = "concor")))
  expect_equal(c(net_nodes(ison_adolescents)), length(node_in_regular(ison_adolescents, "strict")))
  expect_equal(c(net_nodes(ison_southern_women)), length(node_in_automorphic(ison_southern_women, "strict", distance = "binary")))
  expect_equal(c(net_nodes(ison_southern_women)), length(node_in_automorphic(ison_southern_women, distance = "maximum")))
  expect_true("C" %in% node_in_structural(ison_adolescents, k = 3, "concor"))
  expect_true("B" %in% node_in_regular(ison_adolescents, 2))
  expect_true("D" %in% node_in_automorphic(ison_southern_women, 4))
  testthat::skip_if_not_installed("sna")
  expect_equal(c(net_nodes(ison_adolescents)), length(node_in_regular(ison_adolescents, "elbow")))
})

test_that("node_in_motif preserves the former census-based behaviour", {
  expect_s3_class(node_in_motif(ison_adolescents), "node_member")
  expect_equal(c(net_nodes(ison_southern_women)),
               length(node_in_motif(ison_southern_women)))
  # it is built on the triad census for one-mode networks, dropping any
  # triad type that no node participates in
  cens <- node_x_triad(ison_adolescents)
  cens <- cens[, colSums(cens) != 0]
  expect_equal(node_in_motif(ison_adolescents),
               node_in_equivalence(ison_adolescents, cens))
})

test_that("regularity_rolesim satisfies automorphic confirmation", {
  # the spokes of a star are automorphically equivalent, so must score 1
  s <- regularity_rolesim(create_star(8))
  expect_true(all(s[2:8, 2:8] == 1))
  expect_lt(s[1, 2], 1)
  # and it is a symmetric, bounded similarity with a unit diagonal
  r <- regularity_rolesim(ison_adolescents)
  expect_equal(r, t(r))
  expect_true(all(diag(r) == 1))
  expect_true(all(r >= 0 & r <= 1))
  expect_error(regularity_rolesim(ison_adolescents, beta = 2))
})

test_that("regularity_rege discriminates on valued networks", {
  r <- regularity_rege(ison_networkers)
  expect_equal(r, t(r))
  expect_true(all(r >= 0 & r <= 1))
  expect_gt(diff(range(r[upper.tri(r)])), 0.1)
  # but is degenerate on unweighted connected networks: every node comes out
  # maximally regularly equivalent to every other
  expect_true(all(regularity_rege(ison_adolescents) == 1))
})

test_that("node_in_regular uses recursive similarity, not a census", {
  expect_s3_class(node_in_regular(ison_southern_women), "node_member")
  expect_s3_class(node_in_regular(ison_networkers, regularity = "rege"),
                  "node_member")
  # the two algorithms need not agree, since they pair alters differently
  expect_s3_class(node_in_regular(ison_algebra, regularity = "rege"),
                  "node_member")
  # k remains the second argument, as in the sibling functions
  expect_equal(node_in_regular(ison_adolescents, "strict"),
               node_in_regular(ison_adolescents, k = "strict"))
})

test_that("node_in_block searches for a fitting partition", {
  set.seed(123)
  res <- node_in_block(ison_adolescents, k = 3)
  expect_s3_class(res, "node_member")
  expect_length(res, manynet::net_nodes(ison_adolescents))
  expect_lte(length(unique(res)), 3)
  # the search should do at least as well as a random partition
  set.seed(1)
  expect_lte(as.numeric(net_by_inconsistency(ison_adolescents, res)),
             as.numeric(net_by_inconsistency(ison_adolescents,
                                      sample(rep(1:3, length.out = 8)))))
  # it optimises whichever vocabulary it is given
  set.seed(9)
  reg <- node_in_block(ison_adolescents, k = 3, blocks = c("nul", "reg"))
  expect_lte(as.numeric(net_by_inconsistency(ison_adolescents, reg,
                                      blocks = c("nul", "reg"))),
             as.numeric(net_by_inconsistency(ison_adolescents,
                                      node_in_structural(ison_adolescents, k = 3),
                                      blocks = c("nul", "reg"))))
  expect_error(node_in_block(ison_adolescents, k = 1))
  expect_error(node_in_block(ison_adolescents, k = 99))
})
