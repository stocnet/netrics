test <- play_diffusion(create_tree(12), steps = 2)

test_that("infection total works", {
  expect_values(net_by_infection_total(test), 0.583)
})

test_that("infection complete works", {
  expect_values(net_by_infection_complete(test), Inf)
})

# 1 -> 2, 1 -> 3, 2 -> 3, 3 -> 4, 4 -> 5, and an isolate, 6
arcs <- manynet::as_tidygraph(igraph::make_graph(c(1,2, 1,3, 2,3, 3,4, 4,5), n = 6))
arcs_diff <- play_diffusion(arcs, seeds = 1)

test_that("thresholds count only earlier adopters by default", {
  expect_equal(as.numeric(node_by_adopt_threshold(arcs_diff, normalized = FALSE)),
               c(NA, 1, 1, 1, 1, Inf))
  expect_equal(as.numeric(node_by_adopt_threshold(arcs_diff)),
               c(NA, 1, 0.5, 1, 1, Inf))
})

test_that("thresholds count simultaneous adopters without a lag", {
  expect_equal(as.numeric(node_by_adopt_threshold(arcs_diff, normalized = FALSE,
                                                  lag = 0)),
               c(NA, 1, 2, 1, 1, Inf))
})

test_that("thresholds follow the direction of exposure", {
  expect_equal(as.numeric(node_by_adopt_threshold(arcs_diff, lag = 0,
                                                  direction = "out")),
               c(NA, 1, 0, 0, 0, Inf))
  expect_equal(as.numeric(node_by_adopt_threshold(arcs_diff, lag = 0,
                                                  direction = "all")),
               c(NA, 1, 2/3, 0.5, 1, Inf))
})

test_that("exposure follows the direction of exposure", {
  expect_equal(as.numeric(node_by_adopt_exposure(arcs, mark = 3)),
               c(0, 0, 0, 1, 0, 0))
  expect_equal(as.numeric(node_by_adopt_exposure(arcs, mark = 3, direction = "out")),
               c(1, 1, 0, 0, 0, 0))
  expect_equal(as.numeric(node_by_adopt_exposure(arcs, mark = 3, direction = "all")),
               c(1, 1, 0, 1, 0, 0))
})

test_that("diff_model objects keep their diffusion", {
  arcs_model <- suppressWarnings(play_diffusion(arcs, seeds = 1, old_version = TRUE))
  expect_equal(as.logical(node_is_infected(arcs_model, time = 1)),
               c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(as.numeric(node_by_adopt_threshold(arcs_model, normalized = FALSE,
                                                  lag = 0)),
               c(NA, 1, 2, 1, 1, Inf))
})
