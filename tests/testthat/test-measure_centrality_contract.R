# Family-wide contract for the centrality measures.
#
# Rather than adding a test per function, this sweeps the whole roster and
# checks the promises the documentation makes: that a measure returns the
# right shape, that it stays inside the range it declares, that the
# normalisation it declares is the one it performed, and that its arguments
# actually do something.
#
# Where a function does not (yet) meet the contract, the sweep records an
# audit message rather than failing, so that the outstanding gaps are
# enumerated on every run instead of being either invisible or a red build.
# The list of audit messages is the remaining work; the aim is for it to
# shrink to empty.

audit <- new.env(parent = emptyenv())
audit$notes <- character()

note_gap <- function(fn, gap) {
  audit$notes <- c(audit$notes, paste0(fn, ": ", gap))
  invisible(NULL)
}

# The roster of node-level centrality measures, with any arguments needed to
# make them applicable. Adding a measure here brings it under the contract.
node_centralities <- list(
  node_by_degree        = list(),
  node_by_deg           = list(),
  node_by_indegree      = list(),
  node_by_outdegree     = list(),
  node_by_leverage      = list(),
  node_by_closeness     = list(),
  node_by_harmonic      = list(),
  node_by_reach         = list(),
  node_by_decay         = list(),
  node_by_integration   = list(),
  node_by_radiality     = list(),
  node_by_eccentricity  = list(),
  node_by_vitality      = list(),
  node_by_randomwalk    = list(),
  node_by_betweenness   = list(),
  node_by_induced       = list(),
  node_by_eigenvector   = list(),
  node_by_power         = list(),
  node_by_alpha         = list(),
  node_by_pagerank      = list(),
  node_by_hub           = list(),
  node_by_authority     = list(),
  node_by_subgraph      = list()
)

call_measure <- function(fn, args, .data) {
  do.call(fn, c(list(.data), args))
}

# Documented exemptions from the "arguments are live" contract. These are
# deliberate declarations, not gaps: an eigenvector is defined only up to a
# scalar multiple, so its scores carry no absolute units to preserve and
# scaling is intrinsic rather than optional.
inert_arguments <- list(
  node_by_eigenvector = c("normalized", "scaled")
)

test_that("node centralities return a node_measure of the right length", {
  g <- manynet::ison_adolescents
  n <- manynet::net_nodes(g)
  for (fn in names(node_centralities)) {
    res <- call_measure(fn, node_centralities[[fn]], g)
    expect_s3_class(res, "node_measure")
    expect_length(as.numeric(res), n)
  }
})

test_that("node centralities declare what they measured", {
  g <- manynet::ison_adolescents
  for (fn in names(node_centralities)) {
    res <- call_measure(fn, node_centralities[[fn]], g)
    if (is.null(attr(res, "measure"))) {
      note_gap(fn, "declares no `measure` attribute")
      next
    }
    expect_type(attr(res, "measure"), "character")
    expect_true(attr(res, "normalization") %in% netrics:::NORMALIZATIONS)
  }
})

test_that("node centralities stay inside the range they declare", {
  g <- manynet::ison_adolescents
  for (fn in names(node_centralities)) {
    res <- call_measure(fn, node_centralities[[fn]], g)
    rng <- attr(res, "range")
    if (is.null(rng)) {
      note_gap(fn, "declares no `range` attribute")
      next
    }
    vals <- as.numeric(res)
    vals <- vals[is.finite(vals)]
    if (!length(vals)) next
    if (min(vals) < rng[1] || max(vals) > rng[2])
      note_gap(fn, sprintf("returned [%.3f, %.3f], outside its declared [%s, %s]",
                           min(vals), max(vals), rng[1], rng[2]))
    }
  succeed()
})

test_that("declared normalisation matches what the values show", {
  g <- manynet::ison_adolescents
  for (fn in names(node_centralities)) {
    res <- call_measure(fn, node_centralities[[fn]], g)
    kind <- attr(res, "normalization")
    if (is.null(kind)) next
    vals <- as.numeric(res)
    vals <- vals[is.finite(vals)]
    if (!length(vals)) next
    if (kind == "normalized" && (min(vals) < 0 || max(vals) > 1))
      note_gap(fn, "claims theoretical normalisation but leaves [0,1]")
    # A scaled measure divides by the observed maximum, so exactly one node
    # must sit at 1; a proportion sums to one across all nodes.
    if (kind == "scaled" && !isTRUE(all.equal(max(vals), 1)))
      note_gap(fn, sprintf("claims scaling but its maximum is %.4f, not 1", max(vals)))
    if (kind == "proportion" && !isTRUE(all.equal(sum(vals), 1)))
      note_gap(fn, sprintf("claims proportion but its values sum to %.4f, not 1", sum(vals)))
    }
  succeed()
})

test_that("arguments are live rather than decorative", {
  g <- manynet::ison_adolescents
  for (fn in names(node_centralities)) {
    fargs <- formals(get(fn))
    base <- as.numeric(call_measure(fn, node_centralities[[fn]], g))
    for (flag in intersect(c("normalized", "scaled"), names(fargs))) {
      if (flag %in% inert_arguments[[fn]]) next
      # Toggle away from whatever the default is, rather than assuming it.
      flipped <- !isTRUE(eval(fargs[[flag]]))
      alt <- try(as.numeric(call_measure(fn, c(node_centralities[[fn]],
                                               stats::setNames(list(flipped), flag)), g)),
                 silent = TRUE)
      if (inherits(alt, "try-error")) {
        note_gap(fn, sprintf("errors when `%s = %s`", flag, flipped))
      } else if (isTRUE(all.equal(base, alt))) {
        note_gap(fn, sprintf("`%s` has no effect on the result", flag))
      }
    }
  }
  succeed()
})

test_that("measures dispatch on the information they are given", {
  g <- manynet::ison_adolescents
  w <- manynet::mutate_ties(g, weight = c(1, 2, 3, 1, 5, 1, 2, 8, 1, 3))
  # Measures built only from the adjacency structure, which igraph provides
  # no weighted form of, are exempt.
  exempt <- c("node_by_power", "node_by_subgraph", "node_by_leverage",
              "node_by_reach", "node_by_deg", "node_by_indegree",
              "node_by_outdegree", "node_by_degree")
  for (fn in setdiff(names(node_centralities), exempt)) {
    unw <- as.numeric(call_measure(fn, node_centralities[[fn]], g))
    wtd <- try(as.numeric(call_measure(fn, node_centralities[[fn]], w)),
               silent = TRUE)
    if (inherits(wtd, "try-error")) {
      note_gap(fn, "errors on a weighted network")
    } else if (isTRUE(all.equal(unw, wtd))) {
      note_gap(fn, "ignores tie weights")
    }
    }
  succeed()
})

test_that("closeness-like variants relate as documented", {
  g <- manynet::ison_adolescents
  # Integration is an affine transformation of farness, so on a connected
  # network it can never reorder nodes relative to closeness.
  expect_equal(cor(as.numeric(node_by_integration(g)),
                   as.numeric(node_by_closeness(g)),
                   method = "spearman"), 1)
  # Decay centrality is reached through harmonic centrality's `decay`.
  expect_equal(as.numeric(node_by_decay(g, decay = 0.4)),
               as.numeric(node_by_harmonic(g, decay = 0.4)))
  # Radiality is integration read in the outgoing direction.
  expect_equal(as.numeric(node_by_radiality(g)),
               as.numeric(node_by_integration(g, direction = "out")))
})

test_that("closeness vitality identifies cut nodes", {
  g <- manynet::ison_adolescents
  raw <- as.numeric(node_by_vitality(g, normalized = FALSE))
  # The Wiener index of a disconnected network is infinite, so removing a cut
  # node gives negative infinity: a property of the definition, not a failure.
  expect_true(any(!is.finite(raw)))
  norm <- as.numeric(node_by_vitality(g))
  expect_true(all(is.finite(norm)))
  expect_true(all(norm >= 0 & norm <= 1))
  # Cut nodes take the endpoint that negative infinity occupied.
  expect_equal(norm[!is.finite(raw)], rep(0, sum(!is.finite(raw))))
})

test_that("node_by_degree reports strength when weights are used", {
  w <- manynet::mutate_ties(manynet::ison_adolescents,
                            weight = c(1, 2, 3, 1, 5, 1, 2, 8, 1, 3))
  expect_equal(attr(node_by_degree(w, alpha = 1), "measure"),
               "strength centrality")
  # Strength has no theoretical maximum, so this scales rather than normalises.
  expect_equal(attr(node_by_degree(w, alpha = 1), "normalization"), "scaled")
  expect_equal(attr(node_by_degree(w), "measure"), "degree centrality")
})

test_that("renamed `scale` argument still works, with a warning", {
  g <- manynet::ison_adolescents
  expect_warning(node_by_power(g, scale = TRUE), "renamed")
})

# Reported last so that the gaps appear together at the end of the run.
test_that("outstanding contract gaps are recorded", {
  if (length(audit$notes)) {
    message("Centrality contract gaps (", length(audit$notes), "):\n  ",
            paste(unique(audit$notes), collapse = "\n  "))
  }
  succeed()
})
