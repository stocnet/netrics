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
