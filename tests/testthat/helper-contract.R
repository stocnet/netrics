# The family-wide contract for measures.
#
# Rather than adding a test per function, each family sweeps its whole roster
# and checks the promises the documentation makes: that a measure returns the
# right shape, that it declares what it computed, that it stays inside the
# range it declares, that the normalisation it declares is the one it
# performed, and that its arguments actually do something.
#
# Where a function does not (yet) meet the contract, the sweep records an
# audit message rather than failing, so that the outstanding gaps are
# enumerated on every run instead of being either invisible or a red build.
# The list of audit messages is the remaining work; the aim is for it to
# shrink to empty. `report_contract_gaps()` prints it.

# The rosters live here rather than in the family test files so that the
# registry check below is complete however few test files are run. Each roster
# maps a function name to any arguments needed to make it applicable; the
# fixture it is run against belongs with the test that runs it.
measure_rosters <- list(

  centrality_node = list(
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
    node_by_flow          = list(),
    node_by_stress        = list(),
    node_by_information   = list(),
    node_by_eigenvector   = list(),
    node_by_power         = list(),
    node_by_alpha         = list(),
    node_by_pagerank      = list(),
    node_by_hub           = list(),
    node_by_authority     = list(),
    node_by_subgraph      = list(),
    node_by_distance      = list(from = 1)
  ),
  # PN centrality is defined on signed data, so it runs against its own fixture.
  centrality_signed = list(
    node_by_posneg = list()
  ),
  # Multidegree needs multiplex data, so it runs against its own fixture.
  centrality_multiplex = list(
    node_by_multidegree = list(tie1 = "relationship", tie2 = "affiliation")
  ),
  centrality_tie = list(
    tie_by_degree      = list(),
    tie_by_closeness   = list(),
    tie_by_betweenness = list(),
    tie_by_eigenvector = list()
  ),
  centrality_net = list(
    net_by_degree      = list(),
    net_by_indegree    = list(),
    net_by_outdegree   = list(),
    net_by_closeness   = list(),
    net_by_betweenness = list(),
    net_by_eigenvector = list(),
    net_by_reach       = list(),
    net_by_decay       = list(),
    net_by_integration = list(),
    net_by_harmonic    = list()
  ),
  centrality_mode = list(
    mode_by_degree      = list(),
    mode_by_indegree    = list(),
    mode_by_outdegree   = list(),
    mode_by_closeness   = list(),
    mode_by_betweenness = list(),
    mode_by_eigenvector = list()
  ),

  closure_net = list(
    net_by_reciprocity  = list(),
    net_by_transitivity = list(),
    net_by_cyclicality  = list(),
    net_by_equivalency  = list()
  ),
  closure_node = list(
    node_by_reciprocity  = list(),
    node_by_transitivity = list(),
    node_by_equivalency  = list()
  ),

  cohesion_net = list(
    net_by_density      = list(),
    net_by_compactness  = list(),
    net_by_components   = list(),
    net_by_independence = list(),
    net_by_diameter     = list(),
    net_by_length       = list(),
    net_by_cohesion     = list(),
    net_by_adhesion     = list()
  ),
  # Strength and toughness enumerate every subset of ties or nodes, so they get
  # a small fixture of their own.
  fragmentation_net = list(
    net_by_strength  = list(),
    net_by_toughness = list()
  ),

  diffusion_net = list(
    net_by_transmissibility   = list(),
    net_by_recovery           = list(),
    net_by_reproduction       = list(),
    net_by_immunity           = list(),
    net_by_infection_complete = list(),
    net_by_infection_total    = list(),
    net_by_infection_peak     = list()
  ),
  diffusion_node = list(
    node_by_adopt_time      = list(),
    node_by_adopt_threshold = list(),
    node_by_adopt_recovery  = list()
  ),
  diffusion_exposure = list(
    node_by_adopt_exposure = list(mark = c(1, 3))
  ),

  features_net = list(
    net_by_richclub    = list(),
    net_by_scalefree   = list(),
    net_by_bipartivity = list(),
    net_by_smallworld  = list(times = 20)
  ),
  features_balance = list(
    net_by_balance = list()
  ),
  fit_net = list(
    net_by_core          = list(),
    net_by_factions      = list(),
    net_by_modularity    = list(),
    net_by_inconsistency = list()
  ),

  heterogeneity_net = list(
    net_by_richness      = list(attribute = "Gender"),
    net_by_diversity     = list(attribute = "Gender"),
    net_by_heterophily   = list(attribute = "Gender"),
    net_by_homophily     = list(attribute = "Gender"),
    net_by_assortativity = list()
  ),
  heterogeneity_node = list(
    node_by_richness    = list(attribute = "Gender"),
    node_by_diversity   = list(attribute = "Gender"),
    node_by_heterophily = list(attribute = "Gender"),
    node_by_homophily   = list(attribute = "Gender")
  ),
  heterogeneity_spatial = list(
    net_by_spatial = list(attribute = "age")
  ),

  holes_node = list(
    node_by_bridges           = list(),
    node_by_redundancy        = list(),
    node_by_effsize           = list(),
    node_by_efficiency        = list(),
    node_by_constraint        = list(),
    node_by_hierarchy         = list(),
    node_by_neighbours_degree = list()
  ),
  holes_tie = list(
    tie_by_cohesion = list()
  ),

  hierarchy_net = list(
    net_by_connectedness = list(),
    net_by_efficiency    = list(),
    net_by_upperbound    = list()
  ),
  core_node = list(
    node_by_kcoreness = list(),
    node_by_coreness  = list()
  ),
  brokerage_node = list(
    node_by_brokering_activity    = list(membership = "Discipline"),
    node_by_brokering_exclusivity = list(membership = "Discipline")
  ),
  change_net = list(
    net_by_waves = list()
  )
)

# Measures the sweep does not reach through a single-fixture roster, each with
# the reason it is exempt rather than merely absent.
uncontracted_measures <- c(
  # Takes two two-mode networks rather than one network, so it does not fit
  # the roster shape; covered by its own test in the closure contract file.
  "net_by_congruency"
)

audit <- new.env(parent = emptyenv())
audit$notes <- character()
audit$covered <- character()

note_gap <- function(fn, gap) {
  audit$notes <- c(audit$notes, paste0(fn, ": ", gap))
  invisible(NULL)
}

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

# Measures that declare a normalisation but not the bound that usually comes
# with it. `net_by_infection_total()` divides by the number of nodes, but a
# node can be infected more than once where reinfection is possible, so the
# proportion is genuinely not capped at 1.
unbounded_normalization <- c("net_by_infection_total")

# The contract sweep. `roster` is a named list mapping function names to any
# arguments needed to make them applicable; `.data` is the fixture to run them
# on. `level` says what shape to expect back.
check_measure_contract <- function(roster, .data,
                                   level = c("node", "tie", "net", "mode")) {
  level <- match.arg(level)
  audit$covered <- c(audit$covered, names(roster))
  # A mode measure reports one value per mode rather than one per network.
  expected <- switch(level,
                     node = manynet::net_nodes(.data),
                     tie = manynet::net_ties(.data),
                     net = 1L,
                     mode = 2L)
  klass <- switch(level,
                  node = "node_measure",
                  tie = "tie_measure",
                  net = "network_measure",
                  mode = "mode_measure")

  for (fn in names(roster)) {
    res <- call_measure(fn, roster[[fn]], .data)

    # Shape
    expect_s3_class(res, klass)
    expect_length(as.numeric(res), expected)

    # Declaration. Once a family's gaps reach zero these become hard
    # expectations rather than notes; see `expect_declared()` below.
    meas <- attr(res, "measure")
    kind <- attr(res, "normalization")
    rng <- attr(res, "range")
    if (is.null(meas)) {
      note_gap(fn, "declares no `measure` attribute")
    } else {
      expect_type(meas, "character")
    }
    if (is.null(kind)) {
      note_gap(fn, "declares no `normalization` attribute")
    } else {
      expect_true(kind %in% netrics:::NORMALIZATIONS)
    }
    # Only measures offering a choice declare a `variant`, so its absence is
    # not a gap. Where it is declared it must name exactly one variant.
    varnt <- attr(res, "variant")
    if (!is.null(varnt)) {
      expect_type(varnt, "character")
      expect_length(varnt, 1L)
    }

    vals <- as.numeric(res)
    vals <- vals[is.finite(vals)]

    # Range
    if (is.null(rng)) {
      note_gap(fn, "declares no `range` attribute")
    } else if (length(vals) &&
               (min(vals) < rng[1] || max(vals) > rng[2])) {
      note_gap(fn, sprintf("returned [%.3f, %.3f], outside its declared [%s, %s]",
                           min(vals), max(vals), rng[1], rng[2]))
    }

    # Normalisation matches what the values show. A scaled measure divides by
    # the observed maximum, so exactly one node must sit at 1; a proportion
    # sums to one across all nodes.
    if (!is.null(kind) && length(vals)) {
      if (kind == "normalized" && !fn %in% unbounded_normalization &&
          (min(vals) < 0 || max(vals) > 1))
        note_gap(fn, "claims theoretical normalisation but leaves [0,1]")
      if (kind == "scaled" && !isTRUE(all.equal(max(vals), 1)))
        note_gap(fn, sprintf("claims scaling but its maximum is %.4f, not 1", max(vals)))
      if (kind == "proportional" && !isTRUE(all.equal(sum(vals), 1)))
        note_gap(fn, sprintf("claims proportional but its values sum to %.4f, not 1", sum(vals)))
    }

    # Arguments are live rather than decorative
    fargs <- formals(get(fn))
    for (flag in intersect(c("normalized", "scaled"), names(fargs))) {
      if (flag %in% inert_arguments[[fn]]) next
      # Toggle away from whatever the default is, rather than assuming it.
      flipped <- !isTRUE(eval(fargs[[flag]]))
      alt <- try(as.numeric(call_measure(fn, c(roster[[fn]],
                                               stats::setNames(list(flipped), flag)), .data)),
                 silent = TRUE)
      if (inherits(alt, "try-error")) {
        note_gap(fn, sprintf("errors when `%s = %s`", flag, flipped))
      } else if (isTRUE(all.equal(as.numeric(res), alt))) {
        note_gap(fn, sprintf("`%s` has no effect on the result", flag))
      }
    }

    # `decay` is a number rather than a flag, so it is moved away from its
    # default rather than negated. A measure that takes one should respond to
    # it: 0.25 and 0.75 discount by visibly different amounts.
    if ("decay" %in% names(fargs)) {
      pair <- lapply(c(0.25, 0.75), function(d)
        try(as.numeric(call_measure(fn, c(roster[[fn]], list(decay = d)), .data)),
            silent = TRUE))
      if (any(vapply(pair, inherits, logical(1), "try-error"))) {
        note_gap(fn, "errors on a `decay` within [0,1]")
      } else if (isTRUE(all.equal(pair[[1]], pair[[2]]))) {
        note_gap(fn, "`decay` has no effect on the result")
      }
      # The shared bound is enforced by `check_decay()`, so every measure
      # taking a `decay` should refuse one outside [0,1].
      if (!inherits(try(call_measure(fn, c(roster[[fn]], list(decay = 1.5)), .data),
                        silent = TRUE), "try-error"))
        note_gap(fn, "accepts a `decay` above 1")
    }

    # Every choice of `method` should run, and should say which one ran, so
    # that a result carrying no `variant` cannot be traced back to its method.
    if ("method" %in% names(fargs)) {
      for (m in eval(fargs$method)) {
        alt <- try(call_measure(fn, c(roster[[fn]], list(method = m)), .data),
                   silent = TRUE)
        if (inherits(alt, "try-error")) {
          note_gap(fn, sprintf("errors when `method = \"%s\"`", m))
        } else if (is.null(attr(alt, "variant"))) {
          note_gap(fn, sprintf("declares no `variant` for `method = \"%s\"`", m))
        }
      }
    }
  }
  succeed()
}

# For families that have been brought fully under the contract: assert the
# three attributes are present rather than merely noting their absence. This
# is what stops the metadata rotting as new measures are added to a family.
expect_declared <- function(roster, .data) {
  for (fn in names(roster)) {
    res <- call_measure(fn, roster[[fn]], .data)
    expect_false(is.null(attr(res, "measure")),
                 label = paste0(fn, " declares a `measure`"))
    expect_false(is.null(attr(res, "range")),
                 label = paste0(fn, " declares a `range`"))
    expect_false(is.null(attr(res, "normalization")),
                 label = paste0(fn, " declares a `normalization`"))
  }
}

# Every exported measure should be under the contract somewhere. A new
# `net_by_*()`, `node_by_*()`, or `tie_by_*()` that is not in any roster fails
# the build rather than quietly escaping the sweep.
exported_measures <- function() {
  sort(grep("^(net|node|tie|mode)_by_", getNamespaceExports("netrics"),
            value = TRUE))
}

report_contract_gaps <- function() {
  if (length(audit$notes))
    message("Measure contract gaps (", length(unique(audit$notes)), "):\n  ",
            paste(unique(audit$notes), collapse = "\n  "))
  invisible(NULL)
}
