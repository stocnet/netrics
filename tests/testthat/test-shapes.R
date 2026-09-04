# Input shapes. These sweep the functions that a signed or a multilevel
# network once aborted, so that a shape which broke them cannot break them
# again silently. See the "Input shapes" section of .github/CONTRIBUTING.md.
#
# `fict_marvel` is signed and multilevel; `fict_actually` is multilevel alone.

signed_multilevel <- manynet::fict_marvel
multilevel <- manynet::fict_actually

# Functions that read a tie as a distance, and so drop to the positive ties.
signed_distance <- c("node_by_closeness", "node_by_harmonic", "node_by_reach",
                     "node_by_decay", "node_by_integration",
                     "node_by_radiality", "node_by_eccentricity",
                     "node_by_vitality", "node_by_betweenness",
                     "node_by_induced", "node_is_fold")

for(fn in signed_distance){
  test_that(paste(fn, "returns one value per node of a signed network"), {
    expect_length(do.call(fn, list(signed_multilevel)),
                  manynet::net_nodes(signed_multilevel))
  })
}

for(fn in c("net_by_closeness", "net_by_betweenness", "net_by_connectedness",
            "net_by_reach", "net_by_harmonic", "net_by_decay",
            "net_by_integration")){
  test_that(paste(fn, "returns one score for a signed network"), {
    expect_length(do.call(fn, list(signed_multilevel)), 1)
  })
}

for(fn in c("mode_by_closeness", "mode_by_betweenness")){
  test_that(paste(fn, "returns one score per mode of a signed network"), {
    expect_length(do.call(fn, list(signed_multilevel)), 2)
  })
}

# Functions that count a tie however it is signed, and so keep every tie.
for(fn in c("tie_is_transitive", "tie_is_triplet", "tie_is_cyclical",
            "tie_by_betweenness")){
  test_that(paste(fn, "returns one value per tie of a signed network"), {
    expect_length(do.call(fn, list(signed_multilevel)),
                  manynet::net_ties(signed_multilevel))
  })
}

test_that("node_by_hub() and node_by_authority() do not warn when signed", {
  expect_no_warning(node_by_hub(signed_multilevel))
  expect_no_warning(node_by_authority(signed_multilevel))
})

test_that("net_by_modularity() scores a signed network", {
  expect_length(net_by_modularity(signed_multilevel), 1)
})

test_that("node_in_community() uses spinglass where the network is signed", {
  memb <- node_in_community(signed_multilevel)
  expect_length(memb, manynet::net_nodes(signed_multilevel))
  # spinglass needs a connected network and accepts no `k`, so neither a `k`
  # nor an unconnected signed network leaves any algorithm to try
  expect_error(node_in_community(signed_multilevel, k = 3), "signed")
})

# A multilevel network reports itself as two-mode but cannot be projected,
# so these measure it whole.
for(fn in c("node_by_eigenvector", "node_by_power", "node_by_efficiency",
            "node_by_effsize", "node_is_independent", "node_is_core",
            "node_by_core")){
  for(nm in c("signed_multilevel", "multilevel")){
    test_that(paste(fn, "returns one value per node of", nm), {
      net <- get(nm)
      expect_length(do.call(fn, list(net)), manynet::net_nodes(net))
    })
  }
}

test_that("a plain two-mode network still takes the projected path", {
  sw <- manynet::ison_southern_women
  expect_length(node_is_core(sw), manynet::net_nodes(sw))
  expect_length(node_by_eigenvector(sw), manynet::net_nodes(sw))
  expect_length(node_is_independent(sw), manynet::net_nodes(sw))
})

test_that("net_by_core() and net_by_factions() stop on a multilevel network", {
  # `manynet::create_*()` builds one layer, so there is no ideal to fit
  expect_error(net_by_core(multilevel), "one layer")
  expect_error(net_by_factions(multilevel), "one layer")
})

test_that(".to_positive() and .to_unsigned() each keep every node", {
  pos <- .to_positive(signed_multilevel)
  uns <- .to_unsigned(signed_multilevel)
  expect_equal(manynet::net_nodes(pos), manynet::net_nodes(signed_multilevel))
  expect_lt(manynet::net_ties(pos), manynet::net_ties(signed_multilevel))
  # every tie counts in a census, so none is dropped
  expect_equal(manynet::net_ties(uns), manynet::net_ties(signed_multilevel))
  expect_false(manynet::is_signed(uns))
  # an unsigned network passes through both untouched
  expect_identical(.to_positive(manynet::ison_adolescents),
                   manynet::ison_adolescents)
  expect_identical(.to_unsigned(manynet::ison_adolescents),
                   manynet::ison_adolescents)
})
