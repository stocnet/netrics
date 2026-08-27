test_that("node_is_universal works", {
  expect_true(any(node_is_universal(create_star(11))))
})

test_that("node_kcoreness works", {
  expect_equal(top3(node_by_kcoreness(ison_adolescents)), c(1,2,2))
})

test_that("node_in_core works", {
  expect_equal(top3(node_in_core(ison_adolescents)),
               c("Periphery", "Semi-periphery", "Core"))
  expect_output(print(node_in_core(ison_adolescents, groups = 5)), "Semi-periphery-1")
})

test_that("node_in_core labels the most and least core node correctly", {
  # k-means numbers its clusters arbitrarily, so the labels used to land on
  # the wrong nodes entirely.
  cn <- as.numeric(node_by_core(ison_adolescents))
  for (cb in c("bins", "quantiles", "kmeans")) {
    lab <- as.character(node_in_core(ison_adolescents, cluster_by = cb))
    expect_equal(lab[which.max(cn)], "Core", info = cb)
    expect_equal(lab[which.min(cn)], "Periphery", info = cb)
  }
})

test_that("node_in_core numbers middle labels in coreness order", {
  # not alphabetical order, which would put "Semi-core-10" before "Semi-core-2"
  labs <- core_labels(24)
  expect_equal(labs[1], "Core")
  expect_equal(labs[length(labs)], "Periphery")
  expect_equal(labs[2], "Semi-core-1")
  expect_equal(labs[length(labs) - 1], "Semi-periphery-1")
})

test_that("node_in_core returns four sets for a directed network", {
  out <- node_in_core(ison_networkers, direction = "both")
  expect_s3_class(out, "node_member")
  expect_true(all(unique(as.character(out)) %in%
                    c("Core", "Sender", "Receiver", "Periphery")))
  expect_error(node_in_core(ison_adolescents, direction = "both"))
})

test_that("node_by_core reads tie direction", {
  # reversing every tie must swap the out- and in-scores
  out <- as.numeric(node_by_core(ison_networkers, direction = "out"))
  ins <- as.numeric(node_by_core(ison_networkers, direction = "in"))
  expect_false(isTRUE(all.equal(out, ins)))
  rev <- as.numeric(node_by_core(to_redirected(ison_networkers),
                                     direction = "out"))
  expect_equal(rev, ins)
})

test_that("node_by_core reads tie weights", {
  weighted <- as.numeric(node_by_core(ison_networkers))
  binary <- as.numeric(node_by_core(to_unweighted(ison_networkers)))
  expect_false(isTRUE(all.equal(weighted, binary)))
})

test_that("node_by_core works on a two-mode network", {
  # the ideal pattern used to be square, so a non-square network errored
  expect_length(node_by_core(ison_southern_women), 32)
})

test_that("every coreness method returns a coreness and a core", {
  for (fn in list(coreness_correlation, coreness_rich,
                  coreness_transition, coreness_hub)) {
    out <- fn(ison_adolescents)
    expect_length(out$coreness, 8)
    expect_true(all(out$coreness >= 0 & out$coreness <= 1))
    expect_type(out$core, "logical")
    expect_true(any(out$core) && !all(out$core))
  }
})

test_that("the square methods refuse a two-mode network", {
  expect_error(coreness_correlation(ison_southern_women))
  expect_error(coreness_transition(ison_southern_women))
})

test_that("node_is_core still accepts the superseded centrality argument", {
  expect_warning(node_is_core(ison_adolescents, centrality = "degree"))
})

test_that("the superseded node_by_coreness() still works but warns", {
  expect_warning(out <- node_by_coreness(ison_adolescents))
  expect_equal(as.numeric(out), as.numeric(node_by_core(ison_adolescents)))
})

test_that("the methods recover a planted core", {
  set.seed(42)
  # five nodes densely and heavily tied, twenty peripheral nodes tied
  # sparsely and lightly to them and not to each other
  n <- 25
  core <- 1:5
  m <- matrix(0, n, n)
  m[core, core] <- 10
  diag(m) <- 0
  for (i in 6:n) {
    j <- sample(core, 2)
    m[i, j] <- 1
    m[j, i] <- 1
  }
  g <- as_igraph(m, twomode = FALSE)
  expect_equal(which(coreness_rich(g)$core), core)
  expect_equal(which(coreness_rich(to_unweighted(g))$core), core)
})

test_that("the four sets recover a planted directed structure", {
  set.seed(7)
  # nodes 1:4 broadcast to nodes 5:8, and both to a periphery of twelve
  n <- 20
  senders <- 1:4
  receivers <- 5:8
  d <- matrix(0, n, n)
  d[senders, receivers] <- 8
  d[senders, 9:n] <- 1
  for (i in 9:n) d[i, sample(receivers, 1)] <- 1
  out <- as.character(node_in_core(as_igraph(d, twomode = FALSE),
                                   direction = "both"))
  expect_equal(which(out == "Sender"), senders)
  expect_equal(which(out == "Receiver"), receivers)
  expect_equal(sum(out == "Periphery"), 12)
})
