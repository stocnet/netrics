net_motifs <- funs_objs[grepl("net_x_", names(funs_objs))]
for(fn in names(net_motifs)) {
  for (ob in names(data_objs)) { 
    test_that(paste(fn, "works on", ob), {
      skip_if(grepl("exposure|mixed|hazard", fn))
      skip_if(grepl("triad", fn) && is_twomode(data_objs[[ob]]))
      # homophily is only defined against a one-mode attribute
      skip_if(grepl("homophily", fn) && is_twomode(data_objs[[ob]]))
      if(grepl("brokerage|homophily", fn)){
        if(ob == "attribute")
          expect_s3_class(net_motifs[[fn]](data_objs[[ob]], "group"), "network_motif") else
            succeed("Only used for attribute objects")
      } else if(grepl("correlation|change|stability", fn)){
        if(ob == "labelled")
          expect_s3_class(net_motifs[[fn]](data_objs[[ob]], data_objs[[ob]]), "network_motif") else
            succeed("Only used for multi objects")
      } else {
        expect_s3_class(net_motifs[[fn]](data_objs[[ob]]), "network_motif")
      }
    })
  }
}


test_that("net_x_triad takes a mixed census over a multilevel network", {
  # `net_x_triad()` used to refuse this as "not yet implemented", although
  # `net_x_mixed()` implemented it in the same file
  res <- net_x_triad(fict_marvel)
  expect_s3_class(res, "network_motif")
  expect_length(res, 10)
  expect_equal(names(res),
               c("22", "21", "20", "12", "11D", "11U", "10", "02", "01", "00"))
  # a census counts configurations, so no count can be negative: the signs
  # fict_marvel holds as negative weights must not reach the arithmetic
  expect_true(all(as.numeric(res) >= 0))
  # the same census, asked for by supplying the two networks directly
  one <- manynet::to_uniplex(fict_marvel, "relationship")
  two <- manynet::to_uniplex(fict_marvel, "affiliation")
  expect_equal(as.numeric(net_x_triad(one, two)), as.numeric(res))
})

test_that("net_x_triad leaves the ordinary census alone", {
  res <- net_x_triad(ison_adolescents)
  expect_s3_class(res, "network_motif")
  expect_equal(names(res), c("003", "012", "102", "201", "210", "300"))
  # a multiplex network of one-mode layers only still gets the flat census
  expect_length(net_x_triad(ison_algebra), 16)
  # and a two-mode network with no one-mode layer remains unavailable
  expect_error(net_x_triad(ison_southern_women), "not yet implemented")
})

test_that("net_x_mixed is deprecated in favour of net_x_triad", {
  expect_warning(res <- net_x_mixed(fict_marvel), "deprecated")
  expect_equal(as.numeric(res), as.numeric(net_x_triad(fict_marvel)))
})

test_that("net_x_stability returns the Jaccard index of ties", {
  # two ties shared of four in either
  a <- manynet::create_explicit(A-B, B-C, C-D)
  b <- manynet::create_explicit(A-B, B-C, A-D)
  expect_equal(as.numeric(net_x_stability(a, b)), 0.5)
  expect_equal(as.numeric(net_x_stability(a, a)), 1)
  # a directed tie that is turned around is not the same tie
  d1 <- manynet::create_explicit(A-+B, B-+C)
  d2 <- manynet::create_explicit(B-+A, B-+C)
  expect_equal(as.numeric(net_x_stability(d1, d2)), 1/3)
  # a tie's weight does not count it more than once
  w <- manynet::add_tie_attribute(a, "weight", c(5, 1, 1))
  expect_equal(as.numeric(net_x_stability(w, b)), 0.5)
  # two networks without any ties are identical
  expect_equal(as.numeric(net_x_stability(manynet::create_empty(4),
                                          manynet::create_empty(4))), 1)
  out <- as.numeric(unlist(net_x_stability(ison_monks)))
  expect_true(all(out >= 0 & out <= 1))
})
