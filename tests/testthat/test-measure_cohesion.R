test_that("network components works", {
  expect_equal(as.numeric(net_by_components(ison_adolescents)), 1)
})

test_that("net_by_components' connectivity argument works", {
  # a directed acyclic network has one weak component but as many strong
  # components as it has nodes, so the two connectivities must differ
  dag <- manynet::create_tree(6, directed = TRUE)
  expect_equal(as.numeric(net_by_components(dag, connectivity = "weak")), 1)
  expect_equal(as.numeric(net_by_components(dag, connectivity = "strong")),
               as.numeric(manynet::net_nodes(dag)))
  # the no-argument call is unchanged, that is, strong
  expect_equal(as.numeric(net_by_components(dag)),
               as.numeric(net_by_components(dag, connectivity = "strong")))
  # connectivity is ignored for undirected networks
  expect_equal(as.numeric(net_by_components(ison_adolescents,
                                            connectivity = "strong")),
               as.numeric(net_by_components(ison_adolescents,
                                            connectivity = "weak")))
  expect_error(net_by_components(dag, connectivity = "loose"))
})

test_that("network cohesion works", {
  expect_equal(as.numeric(net_by_cohesion(ison_southern_women)), 2)
})

test_that("network adhesion works", {
  expect_equal(as.numeric(net_by_adhesion(ison_southern_women)), 2)
})

test_that("network adhesion reads tie weights as capacities", {
  ring <- igraph::make_ring(5)
  igraph::E(ring)$weight <- c(1, 5, 2, 9, 3)
  out <- net_by_adhesion(ring)
  expect_equal(as.numeric(out), 3)
  expect_equal(attr(out, "measure"), "weighted tie connectivity")
  expect_equal(as.numeric(net_by_adhesion(manynet::to_unweighted(ring))), 2)
  # the minimum cut is the smallest maximum flow between any two nodes
  flows <- vapply(1:5, function(i)
    min(node_by_flow(ring, from = i, normalized = FALSE), na.rm = TRUE),
    FUN.VALUE = numeric(1))
  expect_equal(as.numeric(out), min(flows))
})

test_that("network adhesion reads a signed network's positive ties", {
  # a triangle whose one negative tie leaves a path of two positive ties
  tri <- igraph::make_ring(3)
  igraph::E(tri)$sign <- c(1, 1, -1)
  tri <- manynet::as_stocnet(tri)
  expect_true(manynet::is_signed(tri))
  expect_equal(as.numeric(net_by_adhesion(tri)), 1)
  expect_equal(as.numeric(net_by_adhesion(manynet::to_unsigned(tri, keep = "both"))), 2)
})

test_that("network diameter works", {
  expect_equal(as.numeric(net_by_diameter(ison_southern_women)), 4)
})

test_that("network length works", {
  expect_equal(as.numeric(net_by_length(ison_southern_women)), 2.306, 
               tolerance = 0.001)
})

test_that("net_independence works", {
  expect_values(net_by_independence(ison_adolescents), 4)
})

test_that("net_strength works", {
  expect_values(net_by_strength(ison_adolescents), 0.5)
})

test_that("net_by_strength() and net_by_toughness() guard their size", {
  # Both take a minimum over 2^n subsets, so above `limit` they stop rather
  # than run for hours. See #34.
  expect_error(net_by_toughness(fict_actually), "would not finish")
  expect_error(net_by_strength(fict_actually), "would not finish")
  # a raised `limit` still measures
  expect_values(net_by_toughness(create_ring(12), limit = 12), 1)
})

test_that("net_toughness works", {
  expect_values(net_by_toughness(ison_adolescents), 0.5)
})

test_that("net_by_robustness keeps a curve that only falls", {
  set.seed(1234)
  sf <- generate_scalefree(30, 0.5)
  for (nm in list(NULL, node_by_degree, node_by_degree(sf))) {
    out <- net_by_robustness(sf, node_measure = nm, times = 5)
    curve <- attr(out, "curve")
    expect_equal(nrow(curve), 31)
    expect_equal(range(curve$removed), c(0, 1))
    expect_true(all(diff(curve$giant) <= 0))
    expect_equal(curve$giant[31], 0)
    # R is the mean share over the removals, excluding the intact network
    expect_equal(as.numeric(out), mean(curve$giant[-1]))
    expect_gt(as.numeric(out), 0)
    expect_lte(as.numeric(out), 0.5)
  }
})

test_that("net_by_robustness matches its known extremes", {
  # a star attacked at its hub leaves only isolates, scoring (N-1)/N^2
  expect_equal(as.numeric(net_by_robustness(create_star(20), node_by_degree)),
               19/400)
  # a complete network loses one node per removal, scoring (N-1)/2N
  expect_equal(as.numeric(net_by_robustness(create_filled(10))), 9/20)
})

test_that("net_by_robustness separates failure from attack", {
  set.seed(1234)
  sf <- generate_scalefree(50, 0.5)
  expect_gt(as.numeric(net_by_robustness(sf, times = 20)),
            as.numeric(net_by_robustness(sf, node_by_degree, times = 20)))
  expect_match(attr(net_by_robustness(sf, times = 2), "measure"), "failure")
  expect_match(attr(net_by_robustness(sf, node_by_degree, times = 2), "measure"),
               "adaptive")
  expect_match(attr(net_by_robustness(sf, node_by_degree(sf), times = 2),
                    "measure"), "static")
})

test_that("net_by_robustness is reproducible with a seed", {
  set.seed(1)
  a <- net_by_robustness(ison_adolescents, times = 10)
  set.seed(1)
  b <- net_by_robustness(ison_adolescents, times = 10)
  expect_equal(a, b)
})

test_that("net_by_robustness reads weak components and positive ties", {
  # a one-way chain has no strong component larger than a node, but holds
  # together weakly, so it scores as the undirected path would
  # (the same seed for both, so that tied nodes are chosen alike)
  chain <- create_tree(6, directed = TRUE)
  set.seed(1)
  a <- net_by_robustness(chain, node_by_degree(chain))
  set.seed(1)
  b <- net_by_robustness(to_undirected(chain), node_by_degree(chain))
  expect_equal(as.numeric(a), as.numeric(b))
  # a negative tie holds no component together
  tri <- igraph::make_ring(3)
  igraph::E(tri)$sign <- c(1, 1, -1)
  tri <- manynet::as_stocnet(tri)
  set.seed(1)
  a <- net_by_robustness(tri)
  set.seed(1)
  b <- net_by_robustness(manynet::to_unsigned(tri, keep = "positive"))
  expect_equal(as.numeric(a), as.numeric(b))
})

test_that("net_by_robustness refuses what it cannot rank by", {
  expect_error(net_by_robustness(ison_adolescents, node_measure = "degree"),
               "node_measure")
  expect_error(net_by_robustness(ison_adolescents,
                                 node_measure = node_by_degree(ison_karateka)),
               "scores")
  expect_error(net_by_robustness(ison_adolescents,
                                 node_measure = \(x) rep(1, net_nodes(x))),
               "must return")
  expect_error(net_by_robustness(ison_adolescents, times = 0), "times")
})
test_that("network compactness works", {
  expect_equal(as.numeric(net_by_compactness(create_filled(10))), 1)
  expect_equal(as.numeric(net_by_compactness(create_empty(10))), 0)
  # compactness discriminates where connectedness cannot:
  # both are fully connected, but the star is more compact than the ring
  expect_gt(as.numeric(net_by_compactness(create_star(10))),
            as.numeric(net_by_compactness(create_ring(10))))
  expect_equal(as.numeric(net_by_connectedness(create_star(10))),
               as.numeric(net_by_connectedness(create_ring(10))))
  expect_values(net_by_compactness(ison_adolescents), 0.616)
  expect_values(net_by_compactness(ison_southern_women), 0.515)
  # compactness is the network-level counterpart of harmonic centrality,
  # and is the quantity known elsewhere as global efficiency
  expect_equal(as.numeric(net_by_compactness(ison_adolescents)),
               mean(as.numeric(node_by_harmonic(ison_adolescents,
                                                normalized = TRUE,
                                                cutoff = -1))))
})

test_that("net_by_compactness respects tie direction", {
  # igraph's default distance mode ignores direction, which would treat a
  # directed network as though every tie ran both ways
  dir <- to_unweighted(ison_networkers)
  expect_false(isTRUE(all.equal(
    as.numeric(net_by_compactness(dir)),
    as.numeric(net_by_compactness(to_undirected(dir))))))
  # a one-way chain is less compact than the same chain reciprocated
  chain <- matrix(0, 4, 4)
  chain[cbind(1:3, 2:4)] <- 1
  expect_lt(as.numeric(net_by_compactness(chain)),
            as.numeric(net_by_compactness(chain + t(chain))))
})

test_that("path measures work on a network holding signs as negative weights", {
  # `fict_marvel` is signed but not weighted, so its ties reach igraph as a
  # `weight` attribute of -1 and 1, which igraph would read as a distance
  expect_true(manynet::is_signed(fict_marvel))
  expect_false(manynet::is_weighted(fict_marvel))
  expect_s3_class(net_by_diameter(fict_marvel), "network_measure")
  expect_s3_class(net_by_length(fict_marvel), "network_measure")
  expect_s3_class(net_by_compactness(fict_marvel), "network_measure")
  # each equals the same measure over the positive ties taken explicitly
  positive <- manynet::to_unsigned(fict_marvel, keep = "positive")
  expect_equal(as.numeric(net_by_diameter(fict_marvel)),
               as.numeric(net_by_diameter(positive)))
  expect_equal(as.numeric(net_by_length(fict_marvel)),
               as.numeric(net_by_length(positive)))
  expect_equal(as.numeric(net_by_compactness(fict_marvel)),
               as.numeric(net_by_compactness(positive)))
  # and the negative ties really were excluded, not merely stripped of their
  # weight: a network keeping all 1241 ties but forgetting their signs gives a
  # different answer from the 960 positive ties alone
  expect_lt(manynet::net_ties(positive), manynet::net_ties(fict_marvel))
  # manynet carries the sign as a negative 'weight', so dropping it drops both
  signless <- igraph::delete_edge_attr(manynet::as_igraph(fict_marvel), "weight")
  expect_false(isTRUE(all.equal(as.numeric(net_by_length(fict_marvel)),
                                igraph::mean_distance(signless))))
})

test_that("an unsigned network is untouched by the sign handling", {
  expect_values(net_by_diameter(ison_adolescents), 4)
  expect_values(net_by_length(ison_adolescents), 2.071)
  expect_values(net_by_compactness(ison_adolescents), 0.616)
})

test_that("net_by_independence measures a multilevel network whole", {
  # a multilevel network reports itself as two-mode but has ties within a
  # mode, so the bipartite projection it used to attempt is invalid
  expect_true(manynet::is_twomode(fict_actually))
  expect_true(manynet::is_multilevel(fict_actually))
  expect_values(net_by_independence(fict_actually), 76)
  # a genuine two-mode network still gets the projection
  expect_false(manynet::is_multilevel(ison_southern_women))
  expect_values(net_by_independence(ison_southern_women), 2)
  expect_values(net_by_independence(ison_adolescents), 4)
})
