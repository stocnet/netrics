# The registry check. A newly added measure that is not in any roster should
# fail the build rather than quietly escaping the contract sweep, so this
# compares the rosters in helper-contract.R against the namespace itself.

test_that("every exported measure is under the contract", {
  rostered <- unique(unlist(lapply(measure_rosters, names)))
  uncovered <- setdiff(exported_measures(),
                       c(rostered, uncontracted_measures))
  expect_equal(uncovered, character(0),
               label = "measures missing from every roster")
})

test_that("rosters name measures that actually exist", {
  rostered <- unique(unlist(lapply(measure_rosters, names)))
  expect_equal(setdiff(rostered, exported_measures()), character(0),
               label = "rostered names not exported by netrics")
})

# Reported last so that any outstanding gaps appear together at the end.
test_that("outstanding contract gaps are recorded", {
  report_contract_gaps()
  succeed()
})
