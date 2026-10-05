# Every measure, in every roster, is swept on a cognitive social structure,
# so that a new measure cannot count each perceiver's report as a tie.
# See `check_cognitive_contract()` in helper-contract.R.

for (fixture in names(cognitive_fixtures)) {
  for (family in names(measure_rosters)) {
    test_that(paste(family, "measures read the", fixture,
                    "CSS as its aggregated structure"), {
      check_cognitive_contract(measure_rosters[[family]],
                               cognitive_fixtures[[fixture]])
    })
  }
}
