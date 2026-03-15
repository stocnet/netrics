test_tbl <- as_tidygraph(ison_southern_women)
test_igr <- ison_southern_women
test_mat <- as_matrix(ison_southern_women)

test_that("one mode degree centrality calculated correctly",{
  expect_equal(top5(node_by_degree(ison_adolescents, normalized = FALSE)), c(1,4,4,2,3))
})

test_that("one mode strength centrality calculated correctly",{
  expect_equal(top5(node_by_degree(to_unweighted(ison_networkers), direction = "in", normalized = FALSE)), 
               c(29, 24, 11, 18, 8))
  expect_equal(top5(node_by_degree(ison_networkers, direction = "in", normalized = FALSE, alpha = 1)), 
               c(2495, 1212, 101, 322, 89))
})

test_that("two mode degree centrality calculated correctly",{
  expect_equal(top5(node_by_degree(test_mat, normalized = FALSE)), c(8,7,8,7,4))
  expect_equal(top5(node_by_degree(test_igr, normalized = FALSE)), c(8,7,8,7,4))
  expect_equal(top5(with_graph(test_tbl, node_by_degree(normalized = FALSE))), c(8,7,8,7,4))
  expect_equal(bot3(node_by_degree(test_mat, normalized = FALSE)), c(6,3,3))
  expect_equal(bot3(node_by_degree(test_igr, normalized = FALSE)), c(6,3,3))
  expect_equal(bot3(with_graph(test_tbl, node_by_degree(normalized = FALSE))), c(6,3,3))
  expect_equal(top5(node_by_degree(test_mat, normalized = TRUE)), c(0.5714, .5, .5714, .5, .2857))
  expect_equal(top5(node_by_degree(test_igr, normalized = TRUE)), c(0.5714, .5, .5714, .5, .2857))
  expect_equal(top5(with_graph(test_tbl, node_by_degree(normalized = TRUE))), c(0.5714, .5, .5714, .5, .2857))
  expect_equal(bot3(node_by_degree(test_mat, normalized = TRUE)), c(.3333, .1667, .1667))
  expect_equal(bot3(node_by_degree(test_igr, normalized = TRUE)), c(.3333, .1667, .1667))
  expect_equal(bot3(with_graph(test_tbl, node_by_degree(normalized = TRUE))), c(.3333, .1667, .1667))
})

test_that("one mode closeness centrality calculated correctly",{
  expect_equal(top3(node_by_closeness(ison_adolescents, normalized = FALSE)), c(0.059, 0.091, 0.091), tolerance = 0.01)
})

test_that("two mode closeness centrality calculated correctly",{
  expect_equal(top5(node_by_closeness(test_mat, normalized = FALSE)), c(0.0167, 0.0152, 0.0167, 0.0152, 0.0125))
  expect_equal(top5(node_by_closeness(test_igr, normalized = FALSE)), c(0.0167, 0.0152, 0.0167, 0.0152, 0.0125))
  expect_equal(top5(with_graph(test_tbl, node_by_closeness(normalized = FALSE))), c(0.0167, 0.0152, 0.0167, 0.0152, 0.0125))
  expect_equal(bot3(node_by_closeness(test_mat, normalized = FALSE)), c(0.0128, 0.0119, 0.0119))
  expect_equal(bot3(node_by_closeness(test_igr, normalized = FALSE)), c(0.0128, 0.0119, 0.0119))
  expect_equal(bot3(with_graph(test_tbl, node_by_closeness(normalized = FALSE))), c(0.0128, 0.0119, 0.0119))
  expect_equal(top5(node_by_closeness(test_mat, normalized = TRUE)), c(0.8000, 0.7273, 0.8000, 0.7273, 0.6000))
  expect_equal(top5(node_by_closeness(test_igr, normalized = TRUE)), c(0.8000, 0.7273, 0.8000, 0.7273, 0.6000))
  expect_equal(top5(with_graph(test_tbl, node_by_closeness(normalized = TRUE))), c(0.8000, 0.7273, 0.8000, 0.7273, 0.6000))
  expect_equal(bot3(node_by_closeness(test_mat, normalized = TRUE)), c(0.5641, 0.5238, 0.5238))
  expect_equal(bot3(node_by_closeness(test_igr, normalized = TRUE)), c(0.5641, 0.5238, 0.5238))
  expect_equal(bot3(with_graph(test_tbl, node_by_closeness(normalized = TRUE))), c(0.5641, 0.5238, 0.5238))
})

test_that("one mode betweenness centrality calculated correctly",{
  expect_equal(top3(node_by_betweenness(ison_adolescents, normalized = FALSE)), c(0, 7.5, 5.5), tolerance = 0.001)
})

test_that("two mode betweenness centrality calculated correctly",{
  expect_equal(top5(node_by_betweenness(test_mat, normalized = FALSE)), c(42.9802, 22.8541, 38.9796, 22.0215, 4.7153))
  expect_equal(top5(node_by_betweenness(test_igr, normalized = FALSE)), c(42.9802, 22.8541, 38.9796, 22.0215, 4.7153))
  expect_equal(top5(with_graph(test_tbl, node_by_betweenness(normalized = FALSE))), c(42.9802, 22.8541, 38.9796, 22.0215, 4.7153))
  expect_equal(bot3(node_by_betweenness(test_mat, normalized = FALSE)), c(8.1786, 1.0128, 1.0128))
  expect_equal(bot3(node_by_betweenness(test_igr, normalized = FALSE)), c(8.1786, 1.0128, 1.0128))
  expect_equal(bot3(with_graph(test_tbl, node_by_betweenness(normalized = FALSE))), c(8.1786, 1.0128, 1.0128))
  expect_equal(top3(node_by_betweenness(test_mat, normalized = TRUE),4), c(0.0972, 0.0517, 0.0882))
  expect_equal(top3(node_by_betweenness(test_igr, normalized = TRUE),4), c(0.0972, 0.0517, 0.0882))
  expect_equal(top3(with_graph(test_tbl, node_by_betweenness(normalized = TRUE)),4), c(0.0972, 0.0517, 0.0882))
  expect_equal(bot3(node_by_betweenness(test_mat, normalized = TRUE),4), c(0.0181, 0.0022, 0.0022))
  expect_equal(bot3(node_by_betweenness(test_igr, normalized = TRUE),4), c(0.0181, 0.0022, 0.0022))
  expect_equal(bot3(with_graph(test_tbl, node_by_betweenness(normalized = TRUE)),4), c(0.0181, 0.0022, 0.0022))
})

test_that("one mode eigenvector centrality calculated correctly",{
  # expect_equal(top3(node_eigenvector(ison_adolescents, normalized = FALSE)), c(0.16, 0.491, 0.529), tolerance = 0.001)
  expect_equal(top3(node_by_eigenvector(ison_adolescents)), c(0.303, 0.928, 1), tolerance = 0.001)
})

test_that("two mode eigenvector centrality calculated correctly",{
  expect_equal(top3(node_by_eigenvector(test_mat)), c(0.9009, 0.8497, 1))
  expect_equal(bot3(node_by_eigenvector(test_mat)), c(0.4764, 0.2907, 0.2907))
  expect_equal(top3(node_by_eigenvector(test_igr)), c(0.9009, 0.8497, 1))
  expect_equal(bot3(node_by_eigenvector(test_igr)), c(0.4764, 0.2907, 0.2907))
})

test_that("summary node measure works", {
  expect_equal(names(summary(node_by_degree(ison_adolescents))), 
               c("Minimum","Maximum","Mean","StdDev","Missing"))
})

test_that("summary net measure works", {
  expect_match(summary(net_by_degree(ison_adolescents)), "z =")
})

# ####### Centralization

test_that("one-mode centralisation is calculated correctly", {
  expect_equal(as.numeric(net_by_degree(ison_adolescents)), 0.2142, tolerance = 0.001)
  expect_equal(as.numeric(net_by_closeness(ison_adolescents)), 0.3195, tolerance = 0.001)
  expect_equal(as.numeric(net_by_betweenness(ison_adolescents)), 0.3401, tolerance = 0.001)
  expect_equal(as.numeric(net_by_eigenvector(ison_adolescents)), 0.5479, tolerance = 0.001)
})

test_that("two mode degree centralisation calculated correctly", {
  expect_equal(as.numeric(net_by_degree(ison_southern_women, normalized = FALSE)), c(0.2021, 0.5253), tolerance = 0.001)
  expect_equal(as.numeric(net_by_degree(ison_southern_women, direction = "in")), c(0.249, 0.484), tolerance = 0.001)
  expect_equal(as.numeric(net_by_degree(ison_southern_women, normalized = TRUE)), c(0.245, 0.493), tolerance = 0.001)
})

test_that("two mode closeness centralisation calculated correctly", {
  expect_equal(as.numeric(net_by_closeness(ison_southern_women, normalized = TRUE)), c(0.293, 0.452), tolerance = 0.001)
  expect_equal(as.numeric(net_by_closeness(ison_southern_women, direction = "in")), c(0.224, 0.537), tolerance = 0.001)
})

test_that("two mode betweenness centralisation calculated correctly", {
  expect_equal(as.numeric(net_by_betweenness(ison_southern_women, normalized = FALSE)), c(0.0733, 0.2113), tolerance = 0.001)
  expect_equal(as.numeric(net_by_betweenness(ison_southern_women, direction = "in")), c(0.082, 0.202), tolerance = 0.001)
  expect_equal(as.numeric(net_by_betweenness(ison_southern_women, normalized = TRUE)), c(0.0739, 0.2113), tolerance = 0.001)
})

test_that("net_measure class works", {
  expect_output(print(net_by_degree(ison_algebra)))
})

# ####### Edge centrality
test_that("tie_betweenness works", {
  expect_equal(unname(tie_by_betweenness(ison_adolescents)[1:3]),
               c(7,3,5), tolerance = 0.001)
})

test_that("tie_closeness works", {
  expect_equal(unname(tie_by_closeness(ison_adolescents)[1:3]),
               c(0.562,0.692,0.600), tolerance = 0.001)
})
