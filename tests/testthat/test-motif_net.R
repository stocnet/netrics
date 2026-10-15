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
  # a multiplex network of one-mode layers is split by layer (#24),
  # whose variants still sum to the 16 flat motifs
  expect_true(all(sub("_.*", "", names(net_x_triad(ison_algebra))) %in%
                    .triad_names))
  # and a two-mode network with no one-mode layer remains unavailable
  expect_error(net_x_triad(ison_southern_women), "not yet implemented")
})

test_that("net_x_mixed is defunct in favour of net_x_triad", {
  expect_error(net_x_mixed(fict_marvel), "defunct")
})

test_that("net_x_triad still takes a second network, with a message", {
  local_verbose()
  one <- manynet::to_uniplex(fict_marvel, "relationship")
  two <- manynet::to_uniplex(fict_marvel, "affiliation")
  res <- as.numeric(suppressMessages(net_x_triad(fict_marvel)))
  # positionally, where `attribute` now sits, and by its old name
  expect_message(pos <- net_x_triad(one, two), "deprecated")
  expect_message(named <- net_x_triad(one, object2 = two), "deprecated")
  expect_equal(as.numeric(pos), res)
  expect_equal(as.numeric(named), res)
  expect_error(net_x_triad(fict_marvel, attribute = "Gender"), "not yet available")
})

# Summing a split census over the variants of each plain motif
# should give back the plain census.
census_sums <- function(x, plain){
  out <- stats::setNames(numeric(length(plain)), names(plain))
  sums <- tapply(as.numeric(x), sub("_.*", "", names(x)), sum)
  out[names(sums)] <- sums
  out
}

test_that("split censuses sum to the plain census", {
  for(ob in c("directed", "labelled", "attribute", "weighted")){
    net <- data_objs[[ob]]
    grp <- rep(c("A", "B", "C"), length.out = net_nodes(net))
    for(fn in c("net_x_dyad", "net_x_triad", "net_x_tetrad")){
      f <- get(fn)
      plain <- suppressMessages(f(manynet::to_unsigned(net, "both")))
      split <- suppressMessages(f(net, attribute = grp))
      expect_equal(unname(census_sums(split, plain)),
                   unname(as.numeric(plain)), info = paste(fn, ob))
    }
  }
})

test_that("signed censuses read the sign of each tie", {
  tri <- manynet::create_explicit(A-B, B-C, A-C)
  tri <- manynet::add_tie_attribute(tri, "sign", c(1, 1, -1))
  res <- net_x_triad(tri)
  expect_equal(names(res), "300_++-")
  expect_equal(as.numeric(res), 1)
  dir <- manynet::create_explicit(A-+B, B-+A, A-+C)
  dir <- manynet::add_tie_attribute(dir, "sign", c(1, -1, -1))
  res <- net_x_dyad(dir)
  expect_equal(as.numeric(res[c("Mutual_+-", "Asymmetric_-", "Null")]),
               c(1, 1, 1))
  # the tetrad census sets signs aside
  local_verbose()
  expect_message(net_x_tetrad(data_objs$labelled), "signs aside")
})

test_that("attributed censuses compare each pair of positions", {
  out <- manynet::create_explicit(A-+B, A-+C)
  # a sender of one category, and two receivers that share another
  expect_equal(names(net_x_triad(out, attribute = c("x", "y", "y"))), "021D_!!=")
  # the categories themselves do not matter, nor how many there are
  expect_equal(names(net_x_triad(out, attribute = c("a", "q", "q"))), "021D_!!=")
  big <- manynet::add_nodes(out, 2)
  res <- net_x_triad(big, attribute = c("a", "b", "c", "d", "e"))
  expect_equal(sum(res), choose(5, 3))
  expect_true(all(grepl("_!!!$", names(res))))
  # ordered values are compared in turn: the sender is above both
  # receivers, and the first receiver above the second
  expect_equal(names(net_x_triad(out, attribute = c(3, 1, 2))), "021D_>><")
  expect_equal(names(net_x_triad(out, attribute = factor(c("hi", "lo", "mid"),
    levels = c("lo", "mid", "hi"), ordered = TRUE))), "021D_>><")
  one <- manynet::create_explicit(A-+B, C)
  expect_equal(names(net_x_triad(one, attribute = c(1, 2, 3))), "012_<<<")
  expect_equal(names(net_x_triad(one, attribute = c(2, 1, 3))), "012_><<")
  # an ordered attribute cannot have missing values
  expect_error(net_x_triad(one, attribute = c(1, NA, 3)), "missing")
  # a numeric attribute with few values may code categories
  local_verbose()
  expect_message(net_x_dyad(ison_adolescents, attribute = rep(1:2, 4)), "as.factor")
})

test_that("multiplex censuses read the layers of each tie", {
  res <- suppressMessages(net_x_triad(ison_algebra))
  expect_equal(attr(res, "layers"),
               c(s = "social", t = "tasks", f = "friends"))
  expect_false(anyDuplicated(names(res)) > 0)
  flat <- suppressWarnings(igraph::triad_census(
    igraph::simplify(manynet::as_igraph(ison_algebra))))
  names(flat) <- .triad_names
  expect_equal(unname(census_sums(res, flat)), unname(flat))
  # B-C is tied in both layers, A-B only in layer a
  two <- manynet::to_undirected(manynet::as_tidygraph(
    data.frame(from = c("A", "B", "B"), to = c("B", "C", "C"),
               type = c("a", "a", "b"))))
  expect_equal(names(net_x_triad(two)), "201_ab.a")
  expect_equal(as.numeric(net_x_dyad(two)), c(1, 1, 1))
})

test_that("net_x_tetrad counts each tetrad once", {
  res <- net_x_tetrad(ison_southern_women)
  expect_equal(sum(res), choose(net_nodes(ison_southern_women), 4))
  # a line of four is U4, not D4
  expect_equal(names(which(net_x_tetrad(create_explicit(A-B, B-C, C-D)) > 0)),
               "U4")
  expect_equal(names(which(net_x_tetrad(create_explicit(A-B, B-C, A-C, D)) > 0)),
               "D4")
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
  # the waves of a longitudinal network are correlated in consecutive pairs
  expect_length(unlist(net_x_correlation(ison_monks)), length(out))
})

test_that("node censuses sum to the network census", {
  set.seed(42)
  for(directed in c(FALSE, TRUE)){
    g <- igraph::sample_gnm(25, 50, directed = directed)
    igraph::E(g)$sign <- sample(c(-1, 1), 50, replace = TRUE)
    g <- manynet::as_tidygraph(g)
    for(a in list(NULL, rep(c("a", "b", "c"), length.out = 25),
                  rep(letters[1:5], length.out = 25))){
      net <- suppressMessages(net_x_triad(g, attribute = a))
      node <- suppressMessages(node_x_triad(g, attribute = a))
      expect_equal(colSums(node) / 3, c(unclass(net))[colnames(node)],
                   ignore_attr = TRUE)
      net <- suppressMessages(net_x_tetrad(g, attribute = a))
      node <- suppressMessages(node_x_tetrad(g, attribute = a))
      expect_equal(colSums(node) / 4, c(unclass(net))[colnames(node)],
                   ignore_attr = TRUE)
      expect_equal(sum(net), choose(25, 4))
    }
  }
})

test_that("disconnected tetrads are counted by category", {
  # A-B and C-D tied, E and F isolated; A, C, E are x and B, D, F are y
  g <- create_explicit(A-B, C-D, E, F)
  res <- net_x_tetrad(g, attribute = rep(c("x", "y"), 3))
  expect_equal(sum(res), choose(6, 4))
  expect_equal(as.numeric(res["H4_!!==!!"]), 1)
  # each tie with any two of the other four nodes, except the other tie:
  # per tie, one x-x pair and one y-y pair, which both share the category of
  # one end, and three x-y pairs
  expect_equal(sum(res[grepl("^I4", names(res))]), 10)
  expect_equal(as.numeric(res[c("I4_!!!===", "I4_!!==!!")]), c(4, 6))
})
