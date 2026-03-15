node_motifs <- funs_objs[grepl("node_x_", names(funs_objs))]
for(fn in names(node_motifs)) {
  for (ob in names(data_objs)) { 
    test_that(paste(fn, "works on", ob), {
      skip_if(grepl("triad|dyad", fn) && is_twomode(data_objs[[ob]]))
      if(grepl("brokerage", fn)){
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
  test <- net_x_mixed(marvel_friends, to_uniplex(fict_marvel, "affiliation"))
  expect_equal(unname(test[1]), 1137)
  expect_equal(names(test[1]), "22")
  # Errors
  expect_error(net_x_mixed(ison_southern_women,
                            to_uniplex(fict_marvel, "affiliation")))
  expect_error(net_x_mixed(to_uniplex(fict_marvel, "affiliation"),
                                    ison_southern_women))
  expect_error(net_x_mixed(ison_karateka,
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
