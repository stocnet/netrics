# Diffusion measures need a played diffusion rather than a plain network, so
# the fixture is seeded to keep the sweep deterministic.

set.seed(2024)
smeg <- manynet::generate_smallworld(15, 0.025)
smeg_diff <- manynet::play_diffusion(smeg)

test_that("network diffusion measures meet the measure contract", {
  check_measure_contract(measure_rosters$diffusion_net, smeg_diff, level = "net")
  expect_declared(measure_rosters$diffusion_net, smeg_diff)
})

test_that("node diffusion measures meet the measure contract", {
  check_measure_contract(measure_rosters$diffusion_node, smeg_diff,
                         level = "node")
  expect_declared(measure_rosters$diffusion_node, smeg_diff)
})

test_that("exposure meets the measure contract", {
  check_measure_contract(measure_rosters$diffusion_exposure, smeg,
                         level = "node")
  expect_declared(measure_rosters$diffusion_exposure, smeg)
})

test_that("herd immunity is never a negative proportion", {
  # Below the epidemic threshold 1 - 1/R turns negative, which has no reading
  # as a share of the network that needs protecting.
  set.seed(1)
  weak <- manynet::play_diffusion(manynet::generate_smallworld(20, 0.05),
                                  transmissibility = 0.2, recovery = 0.3)
  expect_lt(as.numeric(net_by_reproduction(weak)), 1)
  expect_equal(as.numeric(net_by_immunity(weak)), 0)
  expect_equal(as.numeric(net_by_immunity(weak, normalized = FALSE)), 0)
  expect_lte(as.numeric(net_by_immunity(smeg_diff)), 1)
})
