# netrics 1.0.1

## Methods

- Fixed `cluster_cosine()` to cluster nodes and not census features (thanks @Kaladani)

# netrics 1.0.0

## Package

- Removed CRAN version check from `.onAttach()` making `library(netrics)` faster to attach
- Fixed release workflow doubling `actions/actions/checkout` path segment
- Updated GitHub Actions workflows to latest major action versions
- Updated CONTRIBUTING to be clearer about documentation, website and NEWS conventions
- Added roxygen templates to standardise argument vocabulary
  - `param_cutoff`
  - `param_decay`
  - `param_times`
  - `param_variant`
  - `param_standardized`
  - `param_connectivity`
- Updated the website function overview to use the `NEWS.md` family headings
- Updated the README to recommend installing the whole family via `{migraph}`

## Measures

- Improved `make_*_measure()` to record algorithm details so results can be read without the script/manual
  - `measure` actually calculated, e.g. `node_by_degree()` is "strength centrality" on a weighted network
  - `normalization`, one of `"normalized"`, `"scaled"`, `"proportional"` or `"none"`
  - `range`, the theoretical range of the returned values
  - `variant` computed where a measure offers a choice, e.g. `net_by_reciprocity()` reports "ratio" when asked
  - Printing is a companion change in `{manynet}`, which defaults to previous behavior
  - Added `measure`, `range`, `normalization`, and `variant` reporting to every measure where applicable
- Updated documentation such that measures that functions and certain arguments produce are discoverable by name
  - `node_by_betweenness(cutoff = k)` is distance-bounded or range-limited betweenness
  - `node_by_reach(cutoff = k)` is geodesic k-path centrality
  - `node_by_closeness()` as the Sabidussi index
  - `node_by_degree()` on a weighted network as strength or weighted degree centrality
  - `node_by_alpha()` as Katz status
  - `node_by_hub()` and `node_by_authority()` as the two halves of Kleinberg's HITS
  - `node_by_transitivity()` as the local clustering coefficient
  - `tie_by_betweenness()` as edge betweenness
  - `node_by_subgraph()` as a node's contribution to the Estrada index
  - `node_by_induced()` and `node_by_vitality()` as delta centrality
  - `node_by_information()` as the closeness member of the current-flow family
  - Stopped `node_by_induced()` also calling itself "vitality centrality"
- Improved specificity of arguments, separating normalising from scaling
  - Renamed `scale` argument to `scaled`; old spelling still works but warns
  - Corrected claim that all measures return normalized values by default
- Improved consistency by consolidating every per-step discount as `decay`
  - Always proportional [0,1] where higher values discount less
  - Was `alpha` in `node_by_alpha()`, `beta` in `regularity_rolesim()`
    - `alpha` now only refers to Opsahl et al.'s trade-off between degree and strength in `node_by_degree()`
  - Added `decay` argument to `node_by_harmonic()`, and `node_by_decay()` as a shortcut for decay centrality
  - Added `decay` to `node_by_pagerank()`, exposing the damping factor previously fixed at 0.85
  - Added `decay` to `node_by_subgraph()`, weighting closed walks by length, which Estrada calls `t`
  - Old spellings still work but warn, as `scale` does
- Fixed `node_by_degree()` to default to `alpha = 0` to match documentation
- Fixed `mode_by_betweenness()` to accept only `"all"` and `"in"`, as implemented
- Fixed `node_by_reach()` counting the node itself so normalised scores could exceed 1
- Fixed `node_by_eigenvector()` discarding tie weights it had computed
- Fixed `tie_by_betweenness()` and `node_by_randomwalk()` accepting `normalized` and then ignoring it
- Fixed `node_by_betweenness()` accepting `normalized` and then ignoring it when given a `cutoff`
- Fixed how `node_by_vitality()` treats cut nodes
  - Unnormalised returns `-Inf` for cut nodes as the Wiener index definition requires
  - Normalised rescales finite scores onto `[0,1]` and places cut nodes at 0
- Fixed `net_by_efficiency()` to implement Krackhardt's share of excess ties
  - `net_x_hierarchy()` now compares four quantities already on `[0,1]`
- Fixed `net_by_immunity()` returning a negative herd immunity threshold when \eqn{R < 1}
- Fixed `net_by_density()`, `net_by_equivalency()` and `node_by_reciprocity()` summing tie weights
- Improved `node_by_closeness()` to validate `direction` via `match.arg()`
- Removed `direction` from `net_by_betweenness()` which never used it
- Moved `node_by_posneg()` to the eigenvector doc group
- Improved `node_by_subgraph()`
  - Now honours tie weights
  - Added `walks=` to choose which closed walks to count: `"odd"`, `"even"` or `"all"`
- Updated references in centrality documentation
  - Corrected `node_by_eigenvector()` to cite Bonacich (1972), not only (1991)
  - Added Freeman (1978) to `node_by_degree()` and the centralisation functions
  - Added Sabidussi (1966) to closeness
  - Added Boldi and Vigna (2014) to harmonic
  - Added Borgatti and Everett (2006) to reach
  - Added Brandes (2008) and Ercsey-Ravasz et al. (2012) to betweenness
  - Added Watts/Strogatz (1998) and Holland/Leinhardt (1971) to node transitivity
  - Added Page et al. (1999) to pagerank
- Added `net_by_bipartivity()` for how close a network is to being bipartite
- Added `net_by_cyclicality()` for detecting generalised exchange
- Added `net_by_compactness()` for the average closeness of all pairs of nodes
- Added `node_by_integration()` and `net_by_integration()` for Valente and Foreman's integration and radiality
- Added `node_by_radiality()` as a shortcut for `node_by_integration(direction = "out")`
- Added `net_by_inconsistency()` for how far a partition's blocks depart from ideal types
  - Ideal types are `nul`, `com`, `reg`, `rdo`, `cdo` and `dnc`
  - Generalises `net_by_factions()` beyond structural equivalence
- Fixed `node_by_equivalency()` erroring on any network, despite being documented for the two-mode case
- Fixed `node_by_diversity()` reporting undefined objects when substituting an inapplicable index
- Fixed `net_by_transmissibility()` declaring itself a proportion
- Fixed `net_by_balance()` erroring on networks holding signs as negative weights
- Fixed `net_by_diameter()`, `net_by_length()` and `net_by_compactness()` on signed networks
  - Now consider only positive ties for distances
- Fixed `node_by_reciprocity()` to return 1 throughout for any undirected network
- Fixed `node_by_information()` on rectangular matrices by using `manynet::to_multilevel()`
- Fixed `net_by_independence()` erroring on multilevel networks by measuring whole
- Fixed `net_by_waves()` reporting one wave where waves are held as `time`
- Added `connectivity=` to `net_by_components()` for counting weak as well as strong components
  - Defaults to `"strong"`, so existing scripts are unaffected
  - The connectivity counted is reported as the measure's `variant` when the result is printed

## Memberships

- Added `k=` to community detection functions to target a specific number of communities (thanks @tomasdiviak)
  - Hierarchical algorithms cut their dendrograms at `k`
  - `node_in_louvain()` and `node_in_leiden()` search the resolution parameter for the value that returns `k`
  - `node_in_fluid()` passes `k` straight to the algorithm, which also makes it much faster
  - `node_in_labels()` seeds `k` fixed labels and merges any surplus groups by modularity
  - `node_in_partition()` is now a k-way Kernighan-Lin, and no longer returns only two groups
  - `node_in_community()` considers only these algorithms when `k` is given
  - `k` also accepts `"silhouette"`, `"elbow"`, and `"strict"`, as in `node_in_equivalence()`
  - Note `k=` is now positioned second, so positional calls must name arguments
- Fixed `node_in_fluid()` and `node_in_spinglass()` aborting silently on disconnected networks
- Added `node_in_labels()` for label propagation community detection
- Renamed `times=` in `node_in_walktrap()` to `steps=`
- Added `consensus=` to `node_in_community()` for combining partitions of all applicable algorithms
  - Runs each algorithm (stochastic ones `times`), then converges on common groupings
  - `consensus = FALSE` default, and ignored where network small enough for `node_in_optimal()`
  - Fixed returning nothing but an error whenever verbosity was not `"verbose"`
- Renamed `node_by_coreness()` to `node_by_core()`
  - Fixed search starting points rather than random
  - Fixed it returning identical scores for a directed network and its reverse
  - Fixed it erroring on two-mode networks whose modes are of unequal size
- Improved `node_in_core()`
  - Renamed `centrality=` to `coreness=`
    - `"rich"` by default for weighted, directed or two-mode networks
    - `"correlation"` otherwise
  - Adds `direction=` for directed networks
    - `"Sender"` for core out-ties and periphery in-ties
    - `"Receiver"` for core in-ties and periphery out-ties
  - Fixed sorting numbered middle labels alphabetically or from arbitrary cluster numbers
- Improved `node_in_equivalence()` to announce the `cluster_*()` and `k_*()` used
- Added `node_in_block()` for direct blockmodelling for partitions that minimise `net_by_inconsistency()`
- Fixed `node_in_regular()` to compute regular equivalence correctly
  - Choose between `regularity = "rolesim"` (default) and `"rege"` for recursive similarity
  - Note existing scripts calling `node_in_regular()` will now return more correct results
  - Moved counting of motif types to `node_in_motif()`,
  though neither is Burt's equivalence or an orbit-aware census (thanks @Kaladani)
- Renamed `Kmax=` to `max_k=` in the community and equivalence functions
- Renamed `num_groups=` to `groups=` in `node_in_roulette()`
- Renamed `cluster_by=` to `split=` in `node_in_core()`
- Added `connectivity=` to `node_in_component()` for weak as well as strong component membership
  - Defaults to `"strong"`, so existing scripts are unaffected
  - Ignored for undirected networks, where the two notions coincide
  - Deprecated `node_in_weak()` and `node_in_strong()`

## Motifs

- Improved `net_x_triad()`
  - Added a mixed census for multiplex networks by folding in `net_x_mixed()`
  - Will fire by default for multiplex networks, taking layers by mode rather than by position
  - Deprecated `net_x_mixed()`
- Added `node_x_clique()`, returning which maximal cliques each node belongs to
  - It branches on two-mode networks to find bicliques (closes #8, thanks @noortjemay)
  - Note that it considers only positive ties, since a clique is a cohesive subgroup
- Improved `node_x_tie()`
  - Fixed erroring on diffusion models which affected `node_in_equivalence()` and `node_in_structural()`
  - Fixed erroring on any multiplex network not multiplexed on a `type` tie attribute
- Added `node_x_ties()`, describing the distribution of each node's tie values
  - In a multiplex network it describes their spread across layers
- Added `node_x_alters()` for describing composition of each node's alters
- Added `node_x_similarity()` for describing similarity of each node to its alters
  - For two-mode networks, compares each node with those at distance two
- Added `net_x_homophily()` for the table behind the EI index against expected baseline

## Methods

- Added `regularity_rolesim()` and `regularity_rege()`, recursive role similarity methods
  - Note `regularity_rege()` is degenerate on unweighted connected networks, where it warns
- Added coreness methods for core-periphery analysis, each returning mark, member, and measure
  - `coreness_correlation()` is Borgatti and Everett's continuous model, fixed to exclude self-ties
  - `coreness_rich()` is Ma and Mondragon's rich-core for directed and two-mode networks
  - `coreness_hub()` is Elliott and colleagues' more granualr directed core-periphery
  - `coreness_transition()` is Rombach and colleagues' core score over boundary sharpness and core size
- Added `split_bins()`, `split_quantiles()` and `split_kmeans()`
  - Each splits a continuous score into an ordered set of groups

## Tutorials

- Updated position tutorial to use `node_in_regular()` for regular equivalence rather than the triad census
- Updated topology tutorial with weighted, directed, and continuous core-periphery

# netrics 0.4.1

## Package

- Fixed the website deploy job installing `Config/Needs/check` packages instead of `Config/Needs/website`, which meant `{learnr}` was never actually installed before the pkgdown deploy step

## Tutorials

- Added missing alt-text to the decorative gifs in the centrality tutorial's article, which was failing pkgdown's accessibility check
- Fixed broken glossary references (`equivalency`, `partition`, `faction`) in the community tutorial that had no matching `{manynet}` glossary entry, which was breaking the tutorial's article rendering
- Added a section on Gould and Fernandez's brokerage roles (`node_x_brokerage()` and `net_x_brokerage()`) to the position tutorial

# netrics 0.4.0

## Measures

- `net_by_degree()`, `net_by_indegree()`, `net_by_outdegree()`, `net_by_betweenness()`, `net_by_closeness()`, and `net_by_eigenvector()` now return a single network-level score for two-mode networks (via Freeman's general centralization index over the mode-normalized node scores), consistent with returning a scalar `network_measure` for all networks
  - This is a behavioural change: these functions previously returned a length-two vector (one score per mode) for two-mode networks
- Added a `mode_by_*()` family (`mode_by_degree()`, `mode_by_indegree()`, `mode_by_outdegree()`, `mode_by_betweenness()`, `mode_by_closeness()`, `mode_by_eigenvector()`) that returns the per-mode centralization scores for two-mode networks, following Borgatti and Everett (1997); these error on one-mode networks
- Fixed `net_by_betweenness()` to respect its `normalized` argument for one-mode networks, which was previously ignored because `igraph::centr_betw()` always applied its default normalization
- Fixed `net_by_closeness()` and `mode_by_closeness()` to pass their `direction` argument through to the underlying node scores, so `direction = "in"`/`"all"` is now effective for two-mode networks

## Tutorials

- Improved the centrality tutorial (`netrics1`) with a new interactive style, and added an article version to the website
  - Added more examples to the centrality tutorial for degree-style analysis of directed and weighted networks
  - Added extensions to the centrality tutorial's betweenness, closeness, and eigenvector sections (the latter including power and influence)
  - Added a "which centrality?" section to the centrality tutorial
  - Improved the centrality tutorial by moving degree distribution and centralisation together and expanding the discussion
- Improved the community tutorial (`netrics2`) with a new version
- Improved the position tutorial (`netrics3`) with a new interactive style
  - Added sections introducing regular and automorphic equivalence in more detail to the position tutorial
  - Added a section on structural folds and ties that torture to the position tutorial
- Improved the topology tutorial (`netrics4`) with a new interactive style
  - Added a section on degree mixing measures for characterising centralisation to the topology tutorial
  - Added a section on further generators, such as fire, islands, and citations, to the topology tutorial

# netrics 0.3.1

## Package

- Updated manynet dependency to 2.1.2 to fix reverse dependency issue

# netrics 0.3.0

## Package

- Improved docs/examples to use base R pipe (|>) instead of magrittr's %>%
- Improved dependency requirements (R >= 4.1.0)
- Improved startup messages to be more succinct

## Measures

- Fixed `node_by_homophily()` to work when attribute is provided as a vector (e.g., a membership vector)
- Improved `node_by_homophily()` to avoid calling `as_igraph()` multiple times

## Motifs

- Fixed `net_x_hazard()` to use `diff_model$t` for naming the returned data frame columns, rather than the deprecated `diff_model$time`

## Tutorials

- Updated topology tutorial to use base R pipe (|>) instead of magrittr's %>%
- Updated centrality tutorial to use base R pipe (|>) instead of magrittr's %>%

# netrics 0.2.3

## Tutorials

- Fixed object reference in position tutorial

# netrics 0.2.2

## Package

- Updated logos

## Tutorials

- Updated centrality tutorial
- Updated community tutorial
- Updated position tutorial
- Updated topology tutorial

# netrics 0.2.1

## Package

- Added network analysis tutorials from `{manynet}`

## Memberships

- Added more explanation for `node_in_partition()`

# netrics 0.2.0

## Package

- Added roxygen2 parameter templates (`param_attr`, `param_data`,
  `param_dir`, `param_memb`, `param_motf`, `param_norm`, `param_select`) and
  net/node/tie-level templates (`net_measure`, `net_motif`, `node_mark`,
  `node_measure`, `node_member`, `node_motif`, `tie_mark`, `tie_measure`) for
  consistent function documentation.
- Fixed startup messages.

## Measures

- Renamed `node_adoption_time()` to `node_by_adopt_time()`
- Renamed `node_thresholds()` to `node_by_adopt_threshold()`
- Renamed `node_exposure()` to `node_by_adopt_exposure()`  
- Renamed `node_recovery()` to `node_by_adopt_recovery()`  
- Separated centralisation scripts into different files per measure type
  (betweenness, closeness, degree, eigenvector) for easier maintenance,
  and into different documentation per level (node, tie) for better organization.
- Updated and separated brokerage, diversity/assortativity, cohesion, closure, 
  cliques, components, features, and hierarchy documentation by level.

## Memberships

- Separated `node_in_community()` documentation from the hierarchical
  and non-hierarchical community-detection algorithms.
- Core documentation split into separate mark, measure, and member pages.
- Improved various functions that rely on a membership argument to accept
  both a membership vector and a string identifier of a network attribute.

## Motifs

- Renamed `net_by_change()` to `net_x_change()` and related functions to 
  reflect their motif (subgraph-counting) nature.

## Methods

- Added gap method for cluster *k*-selection in `method_k()`.
- Renamed `model_k()` to `method_k()` and related cluster-selection utilities
  renamed for clarity.

# netrics 0.1.0

## Release notes

`{netrics}` 0.1.0 is the first formal release of the package as a standalone
analytic engine for the [stocnet](https://github.com/stocnet) ecosystem.
The analytic functions — marks, measures, motifs, and memberships — have been
extracted from `{manynet}` and `{migraph}` into this dedicated package, with
consistent naming conventions and a range of bug fixes.

## New naming conventions

All functions now follow a consistent verb–object–qualifier naming scheme:

- **Marks** (`node_is_*()`, `tie_is_*()`): logical vectors identifying which nodes
  or ties hold a particular structural property.
- **Measures** (`*_by_*()`): numeric vectors at the network (`net_by_*()`),
  node (`node_by_*()`), or tie (`tie_by_*()`) level.
- **Motifs** (`*_x_*()`): tabular counts of nodes' or networks' participation
  in structural sub-patterns.
- **Memberships** (`*_in_*()`): categorical vectors assigning nodes to groups
  (components, communities, equivalence classes, etc.).

Functions previously named with other prefixes (e.g. `node_centrality_*`,
`net_cohesion_*`, `node_equivalency_*`) have been renamed to follow the
`*_by_*()` / `*_x_*()` / `*_in_*()` convention.
`tie_by_cohesion()` now correctly returns a `tie_measure` class object.

## Functions moved from `{manynet}` / `{migraph}`

The following groups of functions have been moved into `{netrics}`:

### Marks
- `node_is_core()`, `node_is_cutpoint()`, `node_is_exposed()`,
  `node_is_fold()`, `node_is_independent()`, `node_is_infected()`,
  `node_is_isolate()`, `node_is_latent()`, `node_is_max()`,
  `node_is_mean()`, `node_is_mentor()`, `node_is_min()`,
  `node_is_neighbor()`, `node_is_pendant()`, `node_is_random()`,
  `node_is_recovered()`, `node_is_universal()`
- `tie_is_bridge()`, `tie_is_cyclical()`, `tie_is_feedback()`,
  `tie_is_imbalanced()`, `tie_is_loop()`, `tie_is_max()`,
  `tie_is_min()`, `tie_is_multiple()`, `tie_is_path()`,
  `tie_is_random()`, `tie_is_reciprocated()`, `tie_is_simmelian()`,
  `tie_is_transitive()`, `tie_is_triangular()`, `tie_is_triplet()`

### Measures
- Network-level: `net_by_adhesion()`, `net_by_assortativity()`,
  `net_by_balance()`, `net_by_betweenness()`, `net_by_change()`,
  `net_by_closeness()`, `net_by_cohesion()`, `net_by_components()`,
  `net_by_congruency()`, `net_by_connectedness()`, `net_by_core()`,
  `net_by_correlation()`, `net_by_degree()`, `net_by_density()`,
  `net_by_diameter()`, `net_by_diversity()`, `net_by_efficiency()`,
  `net_by_eigenvector()`, `net_by_equivalency()`, `net_by_factions()`,
  `net_by_harmonic()`, `net_by_heterophily()`, `net_by_hierarchy()`,
  `net_by_homophily()`, `net_by_immunity()`, `net_by_indegree()`,
  `net_by_independence()`, `net_by_infection_complete()`,
  `net_by_infection_peak()`, `net_by_infection_total()`,
  `net_by_length()`, `net_by_modularity()`, `net_by_outdegree()`,
  `net_by_reach()`, `net_by_reciprocity()`, `net_by_recovery()`,
  `net_by_reproduction()`, `net_by_richclub()`, `net_by_richness()`,
  `net_by_scalefree()`, `net_by_smallworld()`, `net_by_spatial()`,
  `net_by_stability()`, `net_by_strength()`, `net_by_toughness()`,
  `net_by_transitivity()`, `net_by_transmissibility()`,
  `net_by_upperbound()`, `net_by_waves()`
- Node-level: `node_by_adoption_time()`, `node_by_alpha()`,
  `node_by_authority()`, `node_by_betweenness()`, `node_by_bridges()`,
  `node_by_brokering_activity()`, `node_by_brokering_exclusivity()`,
  `node_by_closeness()`, `node_by_constraint()`, `node_by_coreness()`,
  `node_by_deg()`, `node_by_degree()`, `node_by_distance()`,
  `node_by_diversity()`, `node_by_eccentricity()`,
  `node_by_efficiency()`, `node_by_effsize()`, `node_by_eigenvector()`,
  `node_by_equivalency()`, `node_by_exposure()`, `node_by_flow()`,
  `node_by_harmonic()`, `node_by_heterophily()`, `node_by_hierarchy()`,
  `node_by_homophily()`, `node_by_hub()`, `node_by_indegree()`,
  `node_by_induced()`, `node_by_information()`, `node_by_kcoreness()`,
  `node_by_leverage()`, `node_by_multidegree()`,
  `node_by_neighbours_degree()`, `node_by_outdegree()`,
  `node_by_pagerank()`, `node_by_posneg()`, `node_by_power()`,
  `node_by_randomwalk()`, `node_by_reach()`, `node_by_reciprocity()`,
  `node_by_recovery()`, `node_by_redundancy()`, `node_by_richness()`,
  `node_by_stress()`, `node_by_subgraph()`, `node_by_thresholds()`,
  `node_by_transitivity()`, `node_by_vitality()`
- Tie-level: `tie_by_betweenness()`, `tie_by_closeness()`,
  `tie_by_cohesion()`, `tie_by_degree()`, `tie_by_eigenvector()`

### Motifs
- `net_x_brokerage()`, `net_x_dyad()`, `net_x_hazard()`,
  `net_x_mixed()`, `net_x_tetrad()`, `net_x_triad()`
- `node_x_brokerage()`, `node_x_dyad()`, `node_x_exposure()`,
  `node_x_path()`, `node_x_tetrad()`, `node_x_tie()`, `node_x_triad()`

### Memberships
- `node_in_adopter()`, `node_in_automorphic()`,
  `node_in_betweenness()`, `node_in_brokering()`,
  `node_in_community()`, `node_in_component()`, `node_in_core()`,
  `node_in_eigen()`, `node_in_equivalence()`, `node_in_fluid()`,
  `node_in_greedy()`, `node_in_infomap()`, `node_in_leiden()`,
  `node_in_louvain()`, `node_in_optimal()`, `node_in_partition()`,
  `node_in_regular()`, `node_in_roulette()`, `node_in_spinglass()`,
  `node_in_strong()`, `node_in_structural()`, `node_in_walktrap()`,
  `node_in_weak()`

## Bug fixes

- `node_is_isolate()` and `node_is_pendant()` now work correctly with signed networks.
- `tie_is_random()` now correctly returns a `tie_mark` class object (previously returned a node mark).
- `node_by_authority()` and `node_by_hub()` updated to use current `{igraph}` API.
- `node_by_brokering_activity()` and `node_by_brokering_exclusivity()` now handle unlabelled networks correctly.
- `node_by_homophily()` no longer resolves the attribute to a vector prematurely.
- `node_by_pagerank()` updated to correctly extract the vector output from `{igraph}`.
- `node_by_power()` reverts to a lower exponent (closer to degree centrality) when there is no degree variation.
- `node_by_randomwalk()` now works with two-mode networks.
- `net_by_degree()`, `net_by_harmonic()`, and `net_by_reach()` now consistently include the function call in the returned object.
- `net_by_richclub()` returns 0 (rather than erroring) when all nodes have equivalent degree.
- `net_by_smallworld()` and `node_by_bridges()` now use internal `{netrics}` functions rather than `{manynet}` equivalents.
- `net_by_waves()` returns 1 for cross-sectional networks and correctly returns a network measure class.
- `net_x_hierarchy()` correctly classified as a motif function.
- `node_in_community()` now delegates to `{netrics}` membership functions internally.
- Dyad census fixed to handle two-mode networks.
- Equivalence *k*-assignment fixed for the degenerate case where every node is placed in the same cluster.
- `tie_by_cohesion()` now correctly returns a `tie_measure` class object.
