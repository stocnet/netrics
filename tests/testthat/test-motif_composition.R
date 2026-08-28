test_that("node_x_ties branches on network type", {
  # weighted networks get the full distribution of tie values
  ws <- node_x_ties(ison_networkers)
  expect_s3_class(ws, "node_motif")
  expect_equal(colnames(ws), c("Ties", "Sum", "Mean", "SD",
                               "Min", "Median", "Max", "IQR"))
  # the sum of a node's tie values is its strength, i.e. `alpha = 1`
  expect_equal(unname(ws[, "Sum"]),
               as.numeric(node_by_degree(ison_networkers, normalized = FALSE,
                                         alpha = 1)))
  expect_true(all(ws[, "Min"] <= ws[, "Max"], na.rm = TRUE))

  # multiplex networks get one column per layer, plus diversity
  ms <- node_x_ties(ison_algebra)
  expect_equal(ncol(ms), length(unique(tie_attribute(ison_algebra, "type"))) + 1)
  expect_true("Diversity" %in% colnames(ms))
  expect_true(all(ms[, "Diversity"] >= 0 & ms[, "Diversity"] <= 1, na.rm = TRUE))

  # plain networks have only degree to report
  ps <- node_x_ties(ison_adolescents)
  expect_equal(colnames(ps), "Ties")
  expect_equal(unname(ps[, "Ties"]),
               as.numeric(node_by_degree(ison_adolescents, normalized = FALSE)))
})

test_that("node_x_ties diversity behaves at its bounds", {
  # ties concentrated in one layer are minimally diverse,
  # ties spread evenly across layers are maximally so
  expect_equal(unname(.iqv(matrix(c(4, 0, 0), 1, 3))), 0)
  expect_equal(unname(.iqv(matrix(c(2, 2, 2), 1, 3))), 1)
  # an isolate has no distribution to describe
  expect_true(is.na(.iqv(matrix(c(0, 0, 0), 1, 3))))
})

test_that("node_x_alters branches on attribute type", {
  # categorical attributes give one column per category
  cat_res <- node_x_alters(ison_networkers, "Discipline")
  expect_equal(sort(colnames(cat_res)),
               sort(unique(as.character(node_attribute(ison_networkers,
                                                       "Discipline")))))
  # continuous attributes give distributional summaries
  con_res <- node_x_alters(ison_networkers, "Citations")
  expect_equal(colnames(con_res), c("Sum", "Mean", "Weighted",
                                    "Min", "Max", "Range", "SD"))
  expect_true(all(con_res[, "Min"] <= con_res[, "Max"], na.rm = TRUE))
  expect_true(all(con_res[, "Range"] ==
                    con_res[, "Max"] - con_res[, "Min"], na.rm = TRUE))
  # isolates have no alters to summarise
  iso <- add_node_attribute(create_empty(4), "x", c(1, 2, 3, 4))
  expect_true(all(is.na(node_x_alters(iso, "x")[, "Mean"])))
})

test_that("node_x_similarity branches on attribute type", {
  cat_res <- node_x_similarity(ison_networkers, "Discipline")
  expect_equal(colnames(cat_res),
               c("TieSame", "TieDiff", "NoTieSame", "NoTieDiff",
                 "PctSame", "EI", "Odds", "LogOdds", "YulesQ"))
  # the four cells partition every other node
  expect_true(all(rowSums(cat_res[, 1:4]) ==
                    manynet::net_nodes(ison_networkers) - 1))
  # the EI column is the node-level heterophily measure
  expect_equal(unname(round(cat_res[, "EI"], 4)),
               unname(round(as.numeric(
                 node_by_heterophily(to_unweighted(ison_networkers),
                                     "Discipline")), 4)))

  con_res <- node_x_similarity(ison_networkers, "Citations")
  expect_equal(colnames(con_res), c("Diff", "AbsDiff", "SqDiff",
                                    "Zegers", "MinMax", "Product"))
  expect_true(all(con_res[, "AbsDiff"] >= 0, na.rm = TRUE))
  expect_true(all(con_res[, "SqDiff"] >= 0, na.rm = TRUE))

  expect_error(node_x_similarity(ison_networkers, "nonexistent"))
})

test_that("node_x_similarity compares two-mode nodes at distance two", {
  res <- node_x_similarity(ison_southern_women, "Title")
  expect_s3_class(res, "node_motif")
  expect_equal(colnames(res),
               c("TieSame", "TieDiff", "NoTieSame", "NoTieDiff",
                 "PctSame", "EI", "Odds", "LogOdds", "YulesQ"))
  women <- !node_is_mode(ison_southern_women)
  # only nodes of a node's own mode are counted, as alters or as non-alters
  expect_true(all(rowSums(res[women, 1:4]) == sum(women) - 1))
  # the alters are those at distance two, not those tied
  d2 <- to_unweighted(to_mode1(ison_southern_women))
  expect_equal(unname(res[women, "TieSame"] + res[women, "TieDiff"]),
               as.numeric(node_by_degree(d2, normalized = FALSE)))
  # the events hold no title of their own, so have nothing to compare on
  expect_true(all(is.na(res[!women, "PctSame"])))
})

test_that("net_x_homophily agrees with net_by_heterophily", {
  res <- net_x_homophily(ison_adolescents,
                         rep(c("A", "B"), 4))
  expect_s3_class(res, "network_motif")
  expect_equal(names(res), c("TieSame", "TieDiff", "NoTieSame", "NoTieDiff",
                             "PctSame", "EI", "ExpectedEI", "YulesQ"))
  # on unweighted networks the EI column is exactly net_by_heterophily()
  g <- add_node_attribute(ison_adolescents, "grp", rep(c("A", "B"), 4))
  expect_equal(unname(net_x_homophily(g, "grp")["EI"]),
               as.numeric(net_by_heterophily(g, "grp")))
  uw <- to_unweighted(ison_networkers)
  expect_equal(unname(net_x_homophily(uw, "Discipline")["EI"]),
               as.numeric(net_by_heterophily(uw, "Discipline")))
  # but on weighted networks they differ, since one counts ties and the
  # other sums weights
  expect_false(isTRUE(all.equal(
    unname(net_x_homophily(ison_networkers, "Discipline")["EI"]),
    as.numeric(net_by_heterophily(ison_networkers, "Discipline")))))

  expect_error(net_x_homophily(ison_southern_women, "type"))
})

test_that("attribute resolution accepts names and vectors alike", {
  expect_equal(node_x_alters(ison_networkers, "Citations"),
               node_x_alters(ison_networkers,
                             node_attribute(ison_networkers, "Citations")))
  expect_error(node_x_alters(ison_networkers, "nonexistent"))
  expect_error(node_x_alters(ison_networkers, c(1, 2, 3)))
})

test_that("node_x_ties finds layers whatever the tie attribute is called", {
  # `ison_algebra` multiplexes on a "type" attribute, the others on "layer".
  # Reading "type" alone returned no layers at all and only a Diversity
  # column of NAs, without erroring.
  alg <- node_x_ties(ison_algebra)
  expect_s3_class(alg, "node_motif")
  expect_equal(colnames(alg), c("social", "tasks", "friends", "Diversity"))
  monks <- node_x_ties(ison_monks)
  expect_equal(colnames(monks),
               c("like", "esteem", "influence", "praise", "Diversity"))
  expect_false(all(is.na(monks[, "Diversity"])))
  expect_true(all(monks[, "like"] >= 0))
})

test_that("node_x_ties keeps every node where a layer drops some", {
  # `to_uniplex()` reduces fict_marvel's relationship layer to 53 of its 194
  # nodes, so the layers must be padded back to the whole nodeset
  res <- node_x_ties(fict_marvel)
  expect_s3_class(res, "node_motif")
  expect_equal(nrow(res), as.integer(manynet::net_nodes(fict_marvel)))
  expect_equal(colnames(res), c("relationship", "affiliation", "Diversity"))
  expect_false(all(is.na(res[, "Diversity"])))
})
