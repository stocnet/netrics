node_motifs <- funs_objs[grepl("node_x_", names(funs_objs))]
for(fn in names(node_motifs)) {
  for (ob in names(data_objs)) { 
    test_that(paste(fn, "works on", ob), {
      skip_if(grepl("triad|dyad", fn) && is_twomode(data_objs[[ob]]))
      if(grepl("brokerage|alters|similarity", fn)){
        if(ob == "attribute")
          expect_s3_class(node_motifs[[fn]](data_objs[[ob]], "group"), "node_motif") else
            succeed("Only used for attribute objects")
      } else if(grepl("exposure", fn)){
        if(ob == "diffusion")
          expect_s3_class(node_motifs[[fn]](data_objs[[ob]]), "node_motif") else
            succeed("Only used for diffusion objects")
      } else {
        expect_s3_class(node_motifs[[fn]](data_objs[[ob]]), "node_motif")
      }
    })
  }
}

# # Census function family tests
set.seed(123)
task_eg <- to_named(to_uniplex(ison_algebra, "tasks"))

test_that("node_x_tie census works", {
  test <- node_x_tie(task_eg)
  expect_equal(test[1:4], rep(0, 4))
  expect_output(print(test), "fromA")
  expect_equal(nrow(summary(test, membership = node_in_roulette(task_eg, 3))),
               3)
})

test_that("node_x_dyad census works", {
  test <- node_x_dyad(ison_adolescents)
  expect_equal(colnames(test)[1:2], c("Mutual", "Null"))
})

test_that("node_x_triad census works", {
  test <- node_x_triad(task_eg)
  expect_equal(top3(test[,16]), c(7,8,6))
  expect_equal(colnames(test)[1:3], c("003", "012", "102"))
})

test_that("net_x_dyad census works", {
  test <- net_x_dyad(ison_adolescents)
  expect_equal(test[[1]], 10)
  expect_equal(test[[2]], 18)
  expect_equal(names(test), c("Mutual", "Null"))
  expect_output(print(test), "Mutual")
})

test_that("net_x_triad census works", {
  test <- net_x_triad(ison_adolescents)
  expect_equal(test[[1]], 13)
  expect_equal(test[[3]], 29)
  expect_equal(names(test), c("003", "012", "102", "201", "210", "300"))
  expect_equal(names(summary(test)), c("003", "012", "102", "201", "210", "300"))
  # Error
  expect_error(net_x_triad(ison_southern_women))
})

test_that("net_x_tetrad census works", {
  test <- net_x_tetrad(ison_southern_women)
  expect_values(c(test)[1], 12388)
})

test_that("node_x_tetrad census works", {
  test <- node_x_tetrad(ison_southern_women)
  expect_equal(test[1,1], 1241)
})

test_that("net_mixed census works", {
  marvel_friends <- to_unsigned(to_uniplex(fict_marvel, "relationship"), "positive")
  test <- net_x_triad(marvel_friends, to_uniplex(fict_marvel, "affiliation"))
  expect_equal(unname(test[1]), 1137)
  expect_equal(names(test[1]), "22")
  # Errors
  expect_error(net_x_triad(ison_southern_women,
                            to_uniplex(fict_marvel, "affiliation")))
  expect_error(net_x_triad(to_uniplex(fict_marvel, "affiliation"),
                                    ison_southern_women))
  expect_error(net_x_triad(ison_karateka,
                            to_uniplex(fict_marvel, "affiliation")))
})

test <- node_x_path(ison_southern_women)
test_that("node path census works", {
  expect_equal(c(net_nodes(ison_adolescents)),
               nrow(node_x_path(ison_adolescents)))
  expect_true(nrow(node_x_path(ison_southern_women)) ==
                ncol(node_x_path(ison_southern_women)))
})

test_that("node_x_brokerage works", {
  test <- node_x_brokerage(ison_networkers, "Discipline")
  expect_equal(dim(test), c(32,6))
})

test_that("net_x_brokerage works", {
  test <- net_x_brokerage(ison_networkers, "Discipline")
  expect_equal(top3(names(test)), c("Coordinator","Itinerant","Gatekeeper"))
})

test_that("node_x_tie finds layers whatever the tie attribute is called", {
  # ison_monks multiplexes on "layer", so reading "type" gave it no layers
  res <- node_x_tie(ison_monks)
  expect_s3_class(res, "node_motif")
  expect_equal(nrow(res), manynet::net_nodes(ison_monks))
  expect_s3_class(node_x_tie(ison_algebra), "node_motif")
})

test_that("node_x_tie reports layers that cannot be stacked", {
  # fict_marvel's layers hold different node sets, so no one census spans them
  withr::local_options(snet_verbosity = "verbose")
  expect_error(node_x_tie(fict_marvel), "node set")
})

test_that("node_x_triad reaches the mixed census through net_x_triad", {
  skip_on_cran()
  # node_x_triad() is a leave-one-out difference of net_x_triad(), so it gains
  # the multilevel census without any code of its own
  res <- node_x_triad(fict_marvel)
  expect_s3_class(res, "node_motif")
  expect_equal(dim(res), c(manynet::net_nodes(fict_marvel), 10L))
  expect_equal(colnames(res),
               c("22", "21", "20", "12", "11D", "11U", "10", "02", "01", "00"))
})
