test_that("tie_in_community finds link communities", {
  # two triangles that share only node 3
  net <- manynet::as_tidygraph(igraph::graph_from_literal(
    1-2, 1-3, 2-3, 3-4, 3-5, 4-5))
  res <- tie_in_community(net)
  expect_s3_class(res, "tie_member")
  expect_length(res, c(manynet::net_ties(net)))
  expect_equal(names(res), c("1-2", "1-3", "2-3", "3-4", "3-5", "4-5"))
  # each triangle is one community of ties
  expect_equal(unname(unclass(res)), rep(c("A", "B"), each = 3))
  # so node 3 belongs to both, which no node membership can say
  expect_equal(as.numeric(net_by_linkdensity(net, res)), 1)
})

test_that("tie memberships are labelled beyond 702 groups", {
  expect_equal(netrics:::.group_labels(c(1, 26, 27, 702, 703, NA)),
               c("A", "Z", "AA", "ZZ", "AAA", NA))
  net <- igraph::make_graph(c(rbind(seq(1, 1600, 2), seq(2, 1600, 2))),
                            directed = FALSE)
  res <- tie_in_community(net)
  expect_false(anyNA(res))
  expect_length(unique(res), 800)
  expect_false(anyNA(node_in_component(net)))
})

test_that("tie_in_community keeps a place for every tie", {
  res <- tie_in_community(ison_adolescents)
  expect_length(res, c(manynet::net_ties(ison_adolescents)))
  expect_false(anyNA(res))
  # directed and reciprocated ties
  directed <- manynet::to_uniplex(ison_algebra, "friends")
  res <- tie_in_community(directed)
  expect_length(res, c(manynet::net_ties(directed)))
  expect_match(names(res)[1], "->")
  ends <- igraph::ends(manynet::as_igraph(directed),
                       igraph::E(manynet::as_igraph(directed)), names = FALSE)
  keys <- paste(pmin(ends[,1], ends[,2]), pmax(ends[,1], ends[,2]))
  # ties between the same two nodes share a community
  expect_true(all(tapply(unclass(res), keys,
                         function(x) length(unique(x))) == 1))
  # negative ties belong to none
  signed <- manynet::to_uniplex(manynet::fict_marvel, "relationship")
  res <- suppressMessages(tie_in_community(signed))
  expect_length(res, c(manynet::net_ties(signed)))
  expect_equal(unname(is.na(unclass(res))),
               as.numeric(manynet::tie_signs(signed)) < 0)
  # networks without ties to compare
  expect_length(tie_in_community(create_empty(4)), 0)
})
