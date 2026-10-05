# Two cliques, of 4 and of 8 nodes, joined by a single tie
two_cliques <- function(){
  m <- matrix(0, 12, 12)
  m[1:4, 1:4] <- 1
  m[5:12, 5:12] <- 1
  m[4, 5] <- m[5, 4] <- 1
  diag(m) <- 0
  m
}
planted <- rep(1:2, c(4, 8))
errors <- function(mat) function(memb){
  same <- outer(memb, memb, "==")
  diag(same) <- NA
  sum(mat[which(!same)]) + sum(mat[which(same)] == 0)
}

test_that("search_tabu finds a planted partition of unequal groups", {
  cost <- errors(two_cliques())
  set.seed(1234)
  res <- search_tabu(cost, init = rep(1:2, 6), times = 24)
  expect_type(res, "integer")
  expect_length(res, 12)
  expect_equal(c(table(res)), c(4, 8), ignore_attr = TRUE)
  expect_equal(length(unique(res[1:4])), 1)
  expect_equal(attr(res, "cost"), cost(planted))
  # the same seed returns the same membership
  set.seed(1234)
  expect_identical(search_tabu(cost, init = rep(1:2, 6), times = 24), res)
})

test_that("search_tabu agrees with or without a delta", {
  cost <- errors(two_cliques())
  delta <- function(old, new) cost(new) - cost(old)
  set.seed(7)
  full <- search_tabu(cost, init = rep(1:2, 6), times = 24)
  set.seed(7)
  expect_identical(search_tabu(cost, init = rep(1:2, 6), times = 24,
                               delta = delta), full)
})

test_that("search_tabu keeps every group it starts with", {
  cost <- errors(two_cliques())
  set.seed(3)
  # a third group can only raise the errors, but it may not be emptied
  expect_equal(length(unique(search_tabu(cost, rep(1:3, 4), times = 36))), 3)
  # where no move is possible, the search returns where it began
  expect_equal(c(search_tabu(cost, rep(1, 12), times = 10)), rep(1, 12))
  expect_equal(c(search_tabu(function(m) 0, 1:3, times = 10)), 1:3)
  # `times` caps the steps of each run
  expect_length(search_tabu(cost, rep(1:2, 6), times = 1), 12)
})

test_that("search_iterated never returns worse than it began", {
  cost <- errors(two_cliques())
  init <- rep(1:2, 6)
  set.seed(1234)
  res <- search_iterated(cost, init, times = 50)
  expect_length(res, 12)
  expect_lte(attr(res, "cost"), cost(init))
  # its moves keep groups that begin equal at equal size
  expect_equal(c(table(res)), c(6, 6), ignore_attr = TRUE)
})
