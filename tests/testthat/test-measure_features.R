set.seed(123)

# test_that("net_balance works", {
#   out <- net_balance(ison_marvel_relationships)
#   expect_s3_class(out, "network_measure")
#   expect_equal(as.numeric(out), 0.668, tolerance = 0.01)
#   expect_length(out, 1)
#   expect_error(net_balance(ison_adolescents))
# })

test_that("net_richclub works", {
  out <- net_by_richclub(ison_adolescents)
  expect_values(out, 0.833)
})

test_that("net_by_smallworld places lattices below random networks", {
  set.seed(1)
  # a lattice is clustered with long paths, so omega is negative
  expect_lt(as.numeric(net_by_smallworld(manynet::create_ring(50, width = 2),
                                         times = 20)), -0.3)
  # a sparse random network has short paths but little clustering
  expect_gt(as.numeric(net_by_smallworld(manynet::generate_random(50, 0.1),
                                         times = 20)), 0.5)
  # a dense random network is compared with a lattice of its own density
  expect_gt(as.numeric(net_by_smallworld(manynet::generate_random(60, 0.5),
                                         times = 20)), 0)
  sw <- manynet::generate_smallworld(50, 0.025)
  expect_lt(abs(as.numeric(net_by_smallworld(sw, times = 20))), 0.5)
  expect_gt(as.numeric(net_by_smallworld(sw, "SWI", times = 20)), 0.3)
})

test_that("net_scalefree works", {
  out <- net_by_scalefree(ison_adolescents)
  expect_values(out,3.689)
})

test_that("net_balance works", {
  out <- net_by_balance(irps_wwi)
  expect_values(out,1)
})

wavenet <- ison_adolescents %>%
  mutate_ties(wave = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3))

test_that("net_waves works", {
  # expect_equal(net_waves(ison_adolescents), 1)
  expect_values(net_by_waves(wavenet), 3)
})

timenet <- ison_adolescents %>%
  mutate_ties(time = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3))

test_that("net_by_waves counts waves held in a `time` attribute", {
  # Every bundled longitudinal network holds its waves under `time` rather
  # than `wave`, so reading only `wave` reported one wave for each of them.
  expect_values(net_by_waves(timenet), 3)
  expect_values(net_by_waves(ison_monks), 3)
  expect_values(net_by_waves(ison_adolescents), 1)
})

test_that("net_by_meshedness counts cycles against the planar maximum", {
  # A tree has no cycles
  expect_values(net_by_meshedness(create_tree(12)), 0)
  # A triangle has as many as a planar network of three nodes can have
  expect_values(net_by_meshedness(create_filled(3)), 1)
  # (m - n + c) / (2n - 5): 4 ties, 4 nodes, 1 component
  expect_values(net_by_meshedness(create_ring(4)), 1/3)
  # Direction and parallel ties do not add cycles
  expect_values(net_by_meshedness(create_ring(4, directed = TRUE)), 1/3)
  # Too few nodes for the measure to be defined
  expect_true(is.na(net_by_meshedness(create_filled(2))))
})