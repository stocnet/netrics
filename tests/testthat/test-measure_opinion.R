path <- manynet::add_node_attribute(manynet::create_explicit(A-B-C),
                                    "opinions", c(0, 0.5, 1))

test_that("stubbornness gives the equilibrium of Friedkin and Johnsen", {
  half <- manynet::add_node_attribute(path, "stubbornness", rep(0.5, 3))
  expect_equal(as.numeric(node_by_opinion(half)), c(0.25, 0.5, 0.75))
})

test_that("without stubbornness the nodes reach the weighted consensus", {
  star <- manynet::add_node_attribute(manynet::create_star(5), "opinions",
                                      c(1, 0, 0, 0, 0))
  # the star swings between the centre and the leaves, so a warning says so
  expect_warning(out <- node_by_opinion(star), "do not settle")
  expect_equal(as.numeric(out), rep(0.5, 5))
})

test_that("the closed form agrees with play_opinion() after many steps", {
  set.seed(1)
  net <- manynet::generate_random(12, 0.4)
  net <- manynet::add_node_attribute(net, "opinions", stats::runif(12))
  net <- manynet::add_node_attribute(net, "stubbornness", stats::runif(12) / 2)
  played <- manynet::play_opinion(net, stubbornness = "stubbornness",
                                  steps = 500, tolerance = 0)
  last <- unlist(utils::tail(manynet::as_opinion(played, "opinions"), 1))
  expect_equal(as.numeric(node_by_opinion(net)), unname(last), tolerance = 1e-8)
})

test_that("a fully stubborn node has all the influence", {
  stubborn <- manynet::add_node_attribute(path, "stubbornness", c(1, 0, 0))
  expect_equal(as.numeric(node_by_influence(stubborn, normalized = FALSE)),
               c(1, 0, 0))
})

test_that("influence without stubbornness is the left eigenvector of W", {
  expect_equal(as.numeric(node_by_influence(manynet::create_star(5),
                                            normalized = FALSE)),
               c(0.5, rep(0.125, 4)))
  expect_equal(as.numeric(node_by_influence(manynet::create_star(5))),
               c(1, rep(0.25, 4)))
})

test_that("influence centralisation is 0 on a ring and higher on a star", {
  expect_equal(as.numeric(net_by_influence(manynet::create_ring(6))), 0)
  expect_equal(as.numeric(net_by_influence(manynet::create_star(6))), 0.4)
  expect_equal(as.numeric(net_by_influence(manynet::create_star(6),
                                           normalized = FALSE)), 0.5)
})

test_that("convergence is the speed at which opinions settle", {
  expect_equal(as.numeric(net_by_convergence(manynet::create_filled(6))), 0.8)
  half <- manynet::add_node_attribute(path, "stubbornness", rep(0.5, 3))
  expect_equal(as.numeric(net_by_convergence(half)), 0.5)
  expect_equal(as.numeric(suppressMessages(
    net_by_convergence(manynet::create_ring(6)))), 0, tolerance = 1e-8)
})

test_that("opinion measures check their inputs", {
  expect_error(node_by_opinion(path, "votes"), "no nodal attribute")
  bad <- manynet::add_node_attribute(path, "stubbornness", c(2, 0, 0))
  expect_error(node_by_influence(bad), "from 0 to 1")
})

test_that("polarisation and disagreement measure a numeric attribute", {
  # two camps, each at one value and tied only within itself
  camps <- manynet::as_tidygraph(igraph::make_graph(c(1,2, 3,4), n = 4,
                                                    directed = FALSE))
  camps <- manynet::add_node_attribute(camps, "x", c(0, 0, 1, 1))
  expect_equal(as.numeric(net_by_polarisation(camps, "x")), 0.25)
  expect_equal(as.numeric(net_by_disagreement(camps, "x")), 0)
  # the same values, but tied across the camps
  across <- manynet::as_tidygraph(igraph::make_graph(c(1,3, 2,4), n = 4,
                                                     directed = FALSE))
  across <- manynet::add_node_attribute(across, "x", c(0, 0, 1, 1))
  expect_equal(as.numeric(net_by_disagreement(across, "x")), 1)
  expect_error(net_by_polarisation(manynet::ison_lawfirm, "office"), "numeric")
})
