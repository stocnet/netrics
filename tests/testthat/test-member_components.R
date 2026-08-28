test_that("node_in_component works", {
  comp <- ison_monks %>% to_uniplex("esteem") %>%
    node_in_component()
  expect_s3_class(comp, "node_member")
  expect_equal(length(unique(comp)),
               c(net_by_components(to_uniplex(ison_monks, "esteem"))))
  comp <- ison_monks %>% to_uniplex("esteem") %>%
    to_undirected() %>%
    node_in_component()
  expect_equal(length(unique(comp)),
               length(unique(node_in_component(to_uniplex(ison_monks, "esteem"),
                                               connectivity = "weak"))))
})

test_that("node_in_component's connectivity argument works", {
  # a directed acyclic network has one weak component but as many strong
  # components as it has nodes, so the two connectivities must differ
  dag <- manynet::create_tree(6, directed = TRUE)
  expect_equal(length(unique(node_in_component(dag, connectivity = "weak"))), 1)
  expect_equal(length(unique(node_in_component(dag, connectivity = "strong"))),
               as.numeric(manynet::net_nodes(dag)))
  # the no-argument call is unchanged, that is, strong
  expect_equal(c(node_in_component(dag)),
               c(node_in_component(dag, connectivity = "strong")))
  # connectivity is ignored for undirected networks
  expect_equal(c(node_in_component(to_undirected(dag), connectivity = "strong")),
               c(node_in_component(to_undirected(dag), connectivity = "weak")))
  expect_error(node_in_component(dag, connectivity = "loose"))
})

test_that("node_in_component works for two-mode networks", {
  expect_output(print(node_in_component(ison_southern_women)), "1 group")
})

test_that("deprecated component functions still return correct results", {
  esteem <- to_uniplex(ison_monks, "esteem")
  expect_warning(weak <- node_in_weak(esteem))
  expect_warning(strong <- node_in_strong(esteem))
  expect_equal(c(weak), c(node_in_component(esteem, connectivity = "weak")))
  expect_equal(c(strong), c(node_in_component(esteem, connectivity = "strong")))
})
