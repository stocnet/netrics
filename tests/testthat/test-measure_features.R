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

test_that("net_by_waves counts waves held in a `time` attribute", {
  # These hold their waves under `time` rather than `wave`, so reading only
  # `wave` reported one wave for each of them.
  expect_values(net_by_waves(ison_monks), 3)
  expect_values(net_by_waves(ison_fraternity), 15)
  expect_values(net_by_waves(ison_classmates), 4)
  expect_values(net_by_waves(ison_adolescents), 1)
})
