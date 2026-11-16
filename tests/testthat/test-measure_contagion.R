# A ring lattice of 20 nodes, each tied to its two nearest neighbours on each
# side, so that a long tie across it joins neighbourhoods with nothing else
# in common
lattice <- manynet::create_ring(20, width = 2)

test_that("a long tie is the narrowest bridge", {
  long <- manynet::add_ties(lattice, c(1, 11))
  width <- as.numeric(tie_by_width(long))
  expect_equal(width[length(width)], 1)
  expect_true(all(width[-length(width)] > 1))
})

test_that("bridge width counts the ties between two neighbourhoods", {
  # a triangle: each tie, with the two other ties through the third node
  expect_equal(as.numeric(tie_by_width(manynet::create_filled(3))), rep(3, 3))
  # a path: each tie joins its two ends and nothing else
  expect_equal(as.numeric(tie_by_width(manynet::create_ring(6))), rep(1, 6))
})

test_that("a complex contagion needs wide bridges to travel", {
  # in the lattice, two neighbours are always enough to go on
  expect_true(all(as.numeric(node_by_contagion(lattice, threshold = 2)) > 0))
  # in a simple ring, it never passes the seeds
  expect_equal(as.numeric(node_by_contagion(manynet::create_ring(20),
                                            threshold = 2)), rep(0, 20))
})

test_that("a simple contagion is the mean distance beyond the neighbourhood", {
  ring <- manynet::create_ring(8)
  # from node 1, the nodes outside its neighbourhood are 2, 3, 4, 3, and 2 away
  expect_equal(as.numeric(node_by_contagion(ring, threshold = 1))[1], 14/5)
})

test_that("contagion measures check their inputs", {
  expect_error(node_by_contagion(lattice, threshold = 0), "above 0")
  expect_error(tie_by_width(manynet::ison_southern_women), "one-mode")
})

test_that("the tipping point is the smallest share of seeds that reaches all", {
  # a simple contagion needs one seed in a connected network
  expect_equal(as.numeric(net_by_tipping(lattice, threshold = 1)), 1/20)
  # on a ring, a complex contagion needs every other node
  expect_equal(as.numeric(net_by_tipping(manynet::create_ring(10), threshold = 2,
                                         normalized = FALSE, rule = "order")), 9)
  # in a complete network of 10, a threshold of 3 needs 3 seeds
  expect_equal(as.numeric(net_by_tipping(manynet::create_filled(10), threshold = 3,
                                         normalized = FALSE)), 3)
  # a share of the contacts, here a quarter of 9
  expect_equal(as.numeric(net_by_tipping(manynet::create_filled(10), threshold = 0.25,
                                         normalized = FALSE)), 3)
  # an isolate must be a seed
  iso <- manynet::add_nodes(manynet::create_filled(4), 1)
  expect_equal(as.numeric(net_by_tipping(iso, threshold = 0.5, normalized = FALSE)), 5)
})

test_that("the tipping point agrees with play_diffusion()", {
  set.seed(1)
  group <- manynet::create_filled(50) |>
    manynet::mutate_nodes(resistance = runif(50, 0.1, 0.4))
  k <- as.numeric(net_by_tipping(group, threshold = "resistance",
                                 normalized = FALSE, rule = "order"))
  spread <- function(s) as.numeric(net_by_infection_total(
    manynet::play_diffusion(group, seeds = seq_len(s), threshold = "resistance",
                            steps = 50), normalized = FALSE))
  expect_equal(spread(k), 50)
  expect_lt(spread(k - 1), 50)
})

test_that("the tipping point checks its inputs", {
  expect_error(net_by_tipping(lattice, threshold = 0), "above 0")
  expect_error(net_by_tipping(lattice, threshold = "nothing"), "not a node attribute")
  expect_error(net_by_tipping(manynet::ison_southern_women), "one-mode")
})
