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

test_that("two-mode diffusions are exposed through co-affiliation", {
  sw <- manynet::ison_southern_women
  # Evelyn exposes the women she shares an event with, and no events
  expect_equal(as.numeric(node_by_adopt_exposure(sw, mark = 1)),
               c(0, rep(1, 17), rep(0, 14)))
  # marks on both modes expose along the ties between them
  expect_equal(as.numeric(node_by_adopt_exposure(sw, mark = c(1, 19)))[c(2, 20)],
               c(1, 1))
  set.seed(3)
  sw_diff <- play_diffusion(sw)
  thresholds <- as.numeric(node_by_adopt_threshold(sw_diff, normalized = FALSE))
  # every woman who adopted had an infected co-affiliate, and no event adopts
  expect_true(all(thresholds[2:18][is.finite(thresholds[2:18])] >= 1))
  expect_true(all(is.infinite(thresholds[19:32])))
})

test_that("exposure motifs take every kind of diffusion", {
  expect_equal(dim(node_x_exposure(arcs_diff)), c(6, 4))
  arcs_model <- suppressWarnings(play_diffusion(arcs, seeds = 1, old_version = TRUE))
  expect_equal(nrow(node_x_exposure(arcs_model)), 6)
  expect_error(node_x_exposure(arcs), "no diffusion")
})

test_that("diffusions read their start state and their attribute", {
  start <- c("I", "S", "S", "S", "S", "S")
  from_start <- play_diffusion(manynet::add_node_attribute(arcs, "diffusion", start))
  flu <- play_diffusion(manynet::add_node_attribute(arcs, "flu", start), "flu")
  # the seed is in the start state, not in the changes
  for(x in list(from_start, flu)){
    a <- `if`(identical(x, flu), "flu", "diffusion")
    expect_equal(as.numeric(node_is_infected(x, attribute = a)),
                 c(1, 0, 0, 0, 0, 0))
    expect_equal(as.numeric(node_by_adopt_time(x, attribute = a)),
                 c(0, 1, 1, 2, 3, Inf))
    expect_equal(as.numeric(node_by_adopt_threshold(x, normalized = FALSE,
                                                    attribute = a)),
                 c(NA, 1, 1, 1, 1, Inf))
    expect_equal(dim(node_x_exposure(x, attribute = a)), c(6, 4))
  }
  # a state on a network with no changes is read at its start
  still <- manynet::add_node_attribute(arcs, "diffusion", start)
  expect_equal(as.numeric(node_is_exposed(still)), c(0, 1, 1, 0, 0, 0))
  expect_error(node_is_infected(flu), "no diffusion")
})
