# netrics 0.2.0

## Package

- Added roxygen2 parameter templates (`param_attr`, `param_data`,
  `param_dir`, `param_memb`, `param_motf`, `param_norm`, `param_select`) and
  net/node/tie-level templates (`net_measure`, `net_motif`, `node_mark`,
  `node_measure`, `node_member`, `node_motif`, `tie_mark`, `tie_measure`) for
  consistent function documentation.
- Fixed startup messages.

## Measuring

- Renamed `node_adoption_time()` to `node_by_adopt_time()`
- Renamed `node_thresholds()` to `node_by_adopt_threshold()`
- Renamed `node_exposure()` to `node_by_adopt_exposure()`  
- Renamed `node_recovery()` to `node_by_adopt_recovery()`  
- Separated centralisation scripts into different files per measure type
  (betweenness, closeness, degree, eigenvector) for easier maintenance,
  and into different documentation per level (node, tie) for better organization.
- Updated and separated brokerage, diversity/assortativity, cohesion, closure, 
  cliques, components, features, and hierarchy documentation by level.

## Members

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
