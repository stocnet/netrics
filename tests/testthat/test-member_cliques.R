test_that("node_in_roulette works", {
  res <- node_in_roulette(ison_adolescents, groups = 3)
  expect_s3_class(res, "node_member")
  expect_length(res, net_nodes(ison_adolescents))
  expect_false(res[1] == res[2])
  expect_output(print(res), "3 groups")
  expect_output(print(summary(res)), "Class A:")
})

test_that("node_in_roulette keeps the group sizes it starts from", {
  ring <- create_ring(10)
  for(i in 1:50){
    sizes <- table(node_in_roulette(ring, group_size = 3))
    expect_length(sizes, 4)
    expect_lte(max(sizes), 3)
  }
})

test_that("the local search moves respect single candidates", {
  # One smaller group: `sample(4, 1)` once drew from `1:4`
  soln <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4)
  for(i in 1:100) expect_lte(max(table(netrics:::.oneMove(soln))), 3)
  # One node in the other group
  soln <- c(1, 1, 1, 2)
  for(i in 1:50) expect_equal(sort(netrics:::.swapMove(soln)), sort(soln))
  expect_equal(netrics:::.swapMove(c(1, 1, 1)), c(1, 1, 1))
  # Labels need not be 1:k
  soln <- c(5, 5, 5, 7)
  for(i in 1:20) expect_true(all(netrics:::.oneMove(soln) %in% c(5, 7)))
})

test_that("the change in cost equals the change in the full cost", {
  mat <- netrics:::.roulette_cost(ison_adolescents, 1)
  n <- nrow(mat)
  for(i in 1:100){
    old <- sample(1:3, n, replace = TRUE)
    new <- netrics:::.weakPerturb(old)
    expect_equal(netrics:::.clique_delta(mat, old, new),
                 netrics:::.clique_cost(new, mat) - netrics:::.clique_cost(old, mat))
  }
})

test_that("node_in_roulette discounts older waves and times", {
  # Two waves: 1-2 met in the first, 3-4 in the second
  panel <- add_ties(create_empty(4), matrix(c(1, 2, 3, 4), 2, byrow = TRUE),
                    attr_list = list(time = c(1, 2)))
  expect_true(is_longitudinal(panel))
  cost <- netrics:::.roulette_cost(panel, 0)
  expect_equal(cost[1, 2], 0)
  expect_gt(cost[3, 4], 0)
  expect_equal(netrics:::.roulette_cost(panel, 1)[1, 2],
               netrics:::.roulette_cost(panel, 1)[3, 4])
  expect_error(node_in_roulette(panel, groups = 2, decay = 2), "proportion")
  # A dynamic network discounts by its units of time
  dyn <- netrics:::.roulette_cost(irps_wwi, 0.9)
  expect_true(isSymmetric(dyn))
  expect_s3_class(node_in_roulette(irps_wwi, groups = 2, decay = 0.9), "node_member")
})

test_that("node_in_roulette mixes the groups on an attribute", {
  x <- to_unsigned(to_uniplex(fict_marvel, "relationship"), "positive")
  set.seed(1)
  mixed <- node_in_roulette(x, groups = 3, attribute = "Gender", balance = 1)
  apart <- node_in_roulette(x, groups = 3, attribute = "Gender", balance = 0)
  expect_gt(net_by_diversity(x, "Gender", membership = mixed),
            net_by_diversity(x, "Gender", membership = apart))
  expect_error(node_in_roulette(x, groups = 3, attribute = "Gender", balance = -1),
               "proportion")
  expect_error(node_in_roulette(x, groups = 3, attribute = 1:3))
  expect_error(node_in_roulette(ison_southern_women, groups = 2), "one-mode")
})
