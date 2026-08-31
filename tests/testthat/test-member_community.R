test_that("node_kernighanlin algorithm works", {
  expect_s3_class(node_in_partition(ison_adolescents), "node_member")
  expect_length(node_in_partition(ison_adolescents), 
                net_nodes(ison_adolescents))
  expect_false(any(node_in_partition(ison_adolescents) > "B"))
})

test_that("node_edge_betweenness algorithm works", {
  expect_s3_class(node_in_betweenness(ison_adolescents), "node_member")
  expect_length(node_in_betweenness(ison_adolescents), 
                net_nodes(ison_adolescents))
})

test_that("node_fast_greedy algorithm works", {
  expect_s3_class(node_in_greedy(ison_southern_women), "node_member")
  expect_length(node_in_greedy(ison_southern_women), 
                net_nodes(ison_southern_women))
})

test_that("node_walktrap algorithm works", {
  expect_s3_class(node_in_walktrap(ison_southern_women), "node_member")
  expect_length(node_in_walktrap(ison_southern_women), 
                net_nodes(ison_southern_women))
})

test_that("node_in_community uses node_in_optimal on small networks", {
  local_verbose()
  # `capture_messages()` takes every message, so none reaches the console.
  # `expect_message()` takes only the first, and lets the rest print.
  expect_match(capture_messages(node_in_community(manynet::create_ring(10))),
               "optimal", all = FALSE)
  expect_match(capture_messages(node_in_community(manynet::create_ring(200))),
               "xcluding", all = FALSE)
})
test_that("label propagation membership works", {
  # stochastic, so assert on structure rather than exact labels
  set.seed(1234)
  res <- node_in_labels(ison_adolescents)
  expect_s3_class(res, "node_member")
  expect_length(res, manynet::net_nodes(ison_adolescents))
  expect_gte(length(unique(res)), 1)
  expect_output(print(node_in_labels(ison_adolescents)))
  # directed networks are converted rather than refused
  expect_length(node_in_labels(ison_networkers),
                manynet::net_nodes(ison_networkers))
})

# Target number of communities ####

test_that("every k-capable algorithm returns exactly k communities", {
  fns <- list(betweenness = node_in_betweenness, greedy = node_in_greedy,
              walktrap = node_in_walktrap, louvain = node_in_louvain,
              leiden = node_in_leiden, fluid = node_in_fluid,
              labels = node_in_labels, partition = node_in_partition)
  for(nm in names(fns)) for(k in 2:4){
    set.seed(1234)
    res <- fns[[nm]](ison_adolescents, k = k)
    expect_s3_class(res, "node_member")
    expect_length(res, net_nodes(ison_adolescents))
    expect_equal(length(unique(res)), k)
  }
  # node_in_eigen stops splitting early on this network, so it cannot reach k
  set.seed(1234)
  expect_s3_class(node_in_eigen(ison_adolescents, k = 3), "node_member")
})

test_that("k is recorded in the k attribute of hierarchical memberships", {
  expect_equal(attr(node_in_betweenness(ison_adolescents, k = 3), "k"), 3)
  expect_equal(attr(node_in_greedy(ison_adolescents, k = 4), "k"), 4)
  expect_equal(attr(node_in_walktrap(ison_adolescents, k = 2), "k"), 2)
})

test_that("k is validated", {
  expect_error(node_in_louvain(ison_adolescents, k = 0))
  expect_error(node_in_louvain(ison_adolescents, k = 1000))
  expect_error(node_in_louvain(ison_adolescents, k = 0.5))
  expect_error(node_in_louvain(ison_adolescents, k = c(2,3)))
  expect_error(node_in_louvain(ison_adolescents, k = "nonsense"))
})

test_that("k accepts the selection methods", {
  for(nm in c("node_in_betweenness", "node_in_greedy", "node_in_walktrap",
              "node_in_louvain", "node_in_leiden", "node_in_fluid",
              "node_in_labels", "node_in_partition")){
    set.seed(1234)
    sil <- get(nm)(ison_adolescents, k = "silhouette")
    expect_s3_class(sil, "node_member")
    expect_gte(length(unique(sil)), 2)
    set.seed(1234)
    expect_s3_class(get(nm)(ison_adolescents, k = "elbow"), "node_member")
  }
  # strict returns the components, so one community on a connected network
  expect_equal(length(unique(node_in_betweenness(ison_adolescents,
                                                 k = "strict"))), 1)
})

test_that("an unreachable k warns and returns the nearest", {
  # two components cannot be merged into one community
  unconn <- manynet::create_components(8, membership = c(1,1,1,1,2,2,2,2))
  expect_warning(node_in_betweenness(unconn, k = 1), "communities")
  expect_equal(length(unique(suppressWarnings(node_in_betweenness(unconn, k = 1)))), 2)
})

test_that("node_in_partition preserves its two-group result", {
  expect_equal(unname(as.character(node_in_partition(ison_adolescents))),
               c("B","A","A","A","B","B","A","B"))
  expect_equal(unname(as.character(node_in_partition(ison_adolescents, k = 2))),
               c("B","A","A","A","B","B","A","B"))
})

test_that("node_in_community accepts k", {
  set.seed(1234)
  res <- node_in_community(ison_adolescents, k = 3)
  expect_s3_class(res, "node_member")
  expect_equal(length(unique(res)), 3)
})

test_that("node_in_walktrap passes steps to igraph", {
  expect_s3_class(node_in_walktrap(ison_adolescents, steps = 2), "node_member")
  expect_length(node_in_walktrap(ison_adolescents, steps = 8),
                net_nodes(ison_adolescents))
})

test_that("node_in_community consensus recovers planted components", {
  set.seed(1234)
  # four disjoint cliques, so every algorithm must agree on the partition
  planted <- manynet::create_components(120, membership = rep(1:4, each = 30))
  res <- node_in_community(planted, consensus = TRUE, times = 2)
  expect_s3_class(res, "node_member")
  expect_length(res, manynet::net_nodes(planted))
  expect_equal(length(unique(res)), 4)
  expect_equal(length(unique(paste(res, rep(1:4, each = 30)))), 4)
})

test_that("node_in_community consensus accepts k", {
  set.seed(1234)
  res <- node_in_community(ison_adolescents, k = 3, consensus = TRUE, times = 2)
  expect_s3_class(res, "node_member")
  expect_length(res, manynet::net_nodes(ison_adolescents))
  expect_equal(length(unique(res)), 3)
})

test_that("node_in_community ignores consensus where optimal is available", {
  local_verbose()
  small <- manynet::create_ring(10)
  # snet_info() signals a cli message, not an R condition
  expect_match(capture_messages(node_in_community(small, consensus = TRUE)),
               "Ignoring", all = FALSE)
  expect_equal(as.character(suppressMessages(node_in_community(small, consensus = TRUE))),
               as.character(suppressMessages(node_in_optimal(small))))
})
