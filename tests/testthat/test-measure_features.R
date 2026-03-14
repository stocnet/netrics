set.seed(123)

# test_that("net_balance works", {
#   out <- net_balance(ison_marvel_relationships)
#   expect_s3_class(out, "network_measure")
#   expect_equal(as.numeric(out), 0.668, tolerance = 0.01)
#   expect_length(out, 1)
#   expect_error(net_balance(ison_adolescents))
# })

test_that("net_modularity works for two mode networks", {
  out <- net_by_modularity(ison_southern_women,
                 node_in_partition(ison_southern_women))
  expect_length(out, 1)
})

test_that("net_core works", {
  out <- net_by_core(ison_adolescents)
  expect_values(out, -0.133)
  expect_values(net_by_core(ison_adolescents, method = "ident"), 6.481)
  expect_values(net_by_core(ison_adolescents, method = "diff"), 6.094)
})

test_that("net_richclub works", {
  out <- net_by_richclub(ison_adolescents)
  expect_values(out, 0.833)
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

