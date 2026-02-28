
<!-- README.md is generated from README.Rmd. Please edit that file -->

# netrics

<img src="man/figures/logo.png" align="right" alt="netrics logo" width="150"/>

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
![CRAN/METACRAN](https://img.shields.io/cran/v/netrics) ![GitHub release
(latest by
date)](https://img.shields.io/github/v/release/stocnet/netrics) ![GitHub
Release
Date](https://img.shields.io/github/release-date/stocnet/netrics)
[![Codecov test
coverage](https://codecov.io/gh/stocnet/netrics/branch/main/graph/badge.svg)](https://app.codecov.io/gh/stocnet/netrics?branch=main)
<!-- [![CodeFactor](https://www.codefactor.io/repository/github/stocnet/manynet/badge)](https://www.codefactor.io/repository/github/stocnet/manynet) -->
<!-- [![CII Best Practices](https://bestpractices.coreinfrastructure.org/projects/4559/badge)](https://bestpractices.coreinfrastructure.org/projects/4559) -->
<!-- [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7076396.svg)](https://doi.org/10.5281/zenodo.7076396) -->
<!-- see https://zenodo.org/record/7076396 -->
<!-- ![GitHub All Releases](https://img.shields.io/github/downloads/stocnet/migraph/total) -->
<!-- badges: end -->

## About the package

While many awesome packages for network analysis exist for R, all with
their own offerings and advantages, they also all have their own
vocabulary, syntax, and expected formats for data inputs and analytic
outputs. Many of these packages only work on *some* types of networks
(usually one-mode, simple, directed or undirected networks) for *some*
types of analysis; if you want to analyse a different type of network or
try a different analysis, a different package is needed. And they can
rely on a very different visual language (and sometimes plotting
engine), which can mess up your pretty presentation or paper. This can
make learning and using network analysis tools in R challenging.

By contrast, `{netrics}` offers *many* analytic tools that work on
*many* (if not most) types and kinds of networks. It helps researchers
make, modify, mark, measure, and identify nodes’ motifs and memberships
in networks. For graph drawing see
[`{autograph}`](https://stocnet.github.io/autograph/), and for further
testing and modelling capabilities see
[`{migraph}`](https://stocnet.github.io/migraph/) and the other
[stocnet](https://github.com/stocnet) packages.

- [Marking](#marking)
- [Motifs](#motifs)
- [Memberships](#memberships)
- [Measuring](#measuring)
- [Tutorials](#tutorials)
- [Installation](#installation)
  - [Stable](#stable)
  - [Development](#development)
- [Relationship to other packages](#relationship-to-other-packages)
- [Funding details](#funding-details)

## Marking

`{netrics}` includes four special groups of functions, each with their
own pretty `print()` and `plot()` methods: marks, measures, motifs, and
memberships. Marks are logical scalars or vectors, measures are numeric,
memberships categorical, and motifs result in tabular outputs.

`{manynet}`’s `*is_*()` functions offer fast logical tests of various
properties. Whereas `is_*()` returns a single logical value for the
network, `node_is_*()` returns a logical vector the length of the number
of nodes in the network, and `tie_is_*()` returns a logical vector the
length of the number of ties in the network.

- `node_is_core()`, `node_is_cutpoint()`, `node_is_exposed()`,
  `node_is_fold()`, `node_is_independent()`, `node_is_infected()`,
  `node_is_isolate()`, `node_is_latent()`, `node_is_max()`,
  `node_is_mean()`, `node_is_mentor()`, `node_is_min()`,
  `node_is_neighbor()`, `node_is_pendant()`, `node_is_random()`,
  `node_is_recovered()`, `node_is_universal()`
- `tie_is_bridge()`, `tie_is_cyclical()`, `tie_is_feedback()`,
  `tie_is_forbidden()`, `tie_is_imbalanced()`, `tie_is_loop()`,
  `tie_is_max()`, `tie_is_min()`, `tie_is_multiple()`, `tie_is_path()`,
  `tie_is_random()`, `tie_is_reciprocated()`, `tie_is_simmelian()`,
  `tie_is_transitive()`, `tie_is_triangular()`, `tie_is_triplet()`

The `*is_max()` and `*is_min()` functions are used to identify the
maximum or minimum, respectively, node or tie according to some measure
(see below).

## Motifs

`{manynet}`‘s `*by_*()` functions tabulate nodes’ frequency in various
motifs. These include:

- `net_by_adhesion()`, `net_by_assortativity()`, `net_by_balance()`,
  `net_by_betweenness()`, `net_by_change()`, `net_by_closeness()`,
  `net_by_cohesion()`, `net_by_components()`, `net_by_congruency()`,
  `net_by_connectedness()`, `net_by_core()`, `net_by_correlation()`,
  `net_by_degree()`, `net_by_density()`, `net_by_diameter()`,
  `net_by_diversity()`, `net_by_efficiency()`, `net_by_eigenvector()`,
  `net_by_equivalency()`, `net_by_factions()`, `net_by_harmonic()`,
  `net_by_heterophily()`, `net_by_hierarchy()`, `net_by_homophily()`,
  `net_by_immunity()`, `net_by_indegree()`, `net_by_independence()`,
  `net_by_infection_complete()`, `net_by_infection_peak()`,
  `net_by_infection_total()`, `net_by_length()`, `net_by_modularity()`,
  `net_by_outdegree()`, `net_by_reach()`, `net_by_reciprocity()`,
  `net_by_recovery()`, `net_by_reproduction()`, `net_by_richclub()`,
  `net_by_richness()`, `net_by_scalefree()`, `net_by_smallworld()`,
  `net_by_spatial()`, `net_by_stability()`, `net_by_strength()`,
  `net_by_toughness()`, `net_by_transitivity()`,
  `net_by_transmissibility()`, `net_by_upperbound()`, `net_by_waves()`,
  `node_by_adoption_time()`, `node_by_alpha()`, `node_by_authority()`,
  `node_by_betweenness()`, `node_by_bridges()`,
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
  `node_by_transitivity()`, `node_by_vitality()`,
  `tie_by_betweenness()`, `tie_by_closeness()`, `tie_by_cohesion()`,
  `tie_by_degree()`, `tie_by_eigenvector()`

## Memberships

`{manynet}`‘s `*in_*()` functions identify nodes’ membership in some
grouping, such as a community or component. These functions always
return a character vector, indicating e.g. that the first node is a
member of group “A”, the second in group “B”, etc.

- `node_in_adopter()`, `node_in_automorphic()`, `node_in_betweenness()`,
  `node_in_brokering()`, `node_in_community()`, `node_in_component()`,
  `node_in_core()`, `node_in_eigen()`, `node_in_equivalence()`,
  `node_in_fluid()`, `node_in_greedy()`, `node_in_infomap()`,
  `node_in_leiden()`, `node_in_louvain()`, `node_in_optimal()`,
  `node_in_partition()`, `node_in_regular()`, `node_in_roulette()`,
  `node_in_spinglass()`, `node_in_strong()`, `node_in_structural()`,
  `node_in_walktrap()`, `node_in_weak()`

For example `node_brokerage_census()` returns the frequency of nodes’
participation in Gould-Fernandez brokerage roles for a one-mode network,
and the Jasny-Lubell brokerage roles for a two-mode network.

These can be analysed alone, or used as a profile for establishing
equivalence. `{manynet}` offers both HCA and CONCOR algorithms, as well
as elbow, silhouette, and strict methods for *k*-cluster selection.

<img src="https://www.jameshollway.com/post/migraph/dendroPlot.png" alt="Plot of a dendrogram of structural equivalence"/>

`{manynet}` also includes functions for establishing membership on other
bases, such as typical community detection algorithms, as well as
component and core-periphery partitioning algorithms.

## Measuring

`{manynet}` also offers a large and growing smorgasbord of measures that
can be used at the node, tie, and network level to measure some feature,
property, or quantity of the network. Each recognises whether the
network is directed or undirected, weighted or unweighted, one-mode or
two-mode. All return normalized values wherever possible, though this
can be overrided. Here are some examples:

- *Centrality*: `node_degree()`, `node_closeness()`,
  `node_betweenness()`, and `node_eigenvector()`, `net_degree()`,
  `net_closeness()`, `net_betweenness()`, and `net_eigenvector()`
- *Cohesion*: `net_density()`, `net_reciprocity()`,
  `net_transitivity()`, `net_equivalency()`, and `net_congruency()`
- *Hierarchy*: `net_connectedness()`, `net_efficiency()`,
  `net_upperbound()`
- *Resilience*: `net_components()`, `net_cohesion()`, `net_adhesion()`,
  `net_diameter()`, `net_length()`
- *Innovation*: e.g. `node_redundancy()`, `node_effsize()`,
  `node_efficiency()`, `node_constraint()`, `node_hierarchy()`
- *Diversity*: `net_richness()`, `net_diversity()`, `net_heterophily()`,
  `net_assortativity()`, `node_richness()`, `node_diversity()`,
  `node_heterophily()`, `node_assortativity()`
- *Topology*: e.g. `net_core()`, `net_factions()`, `net_modularity()`,
  `net_smallworld()`, `net_balance()`
- *Diffusion*: e.g. `net_reproduction()`, `net_immunity()`,
  `node_thresholds()`

There is a lot here, so we recommend you explore [the list of
functions](https://stocnet.github.io/migraph/reference/index.html) to
find out more.

## Tutorials

This package includes tutorials to help new and experienced users learn
how they can conduct social network analysis using the package. These
tutorials leverage the additional package `{learnr}` (see
[here](https://rstudio.github.io/learnr/)), but we have made it easy to
use `{manynet}` or `{migraph}` tutorials right out of the box:

``` r
run_tute()
#> Checking tutorials in stocnet packages ■■■■■■■■■■■ 33% | …
#> # A tibble: 10 × 3
#>    package   name      title                   
#>    <chr>     <chr>     <chr>                   
#>  1 manynet   tutorial0 Intro to R              
#>  2 manynet   tutorial1 Data                    
#>  3 autograph tutorial2 Visualisation           
#>  4 manynet   tutorial3 Centrality              
#>  5 manynet   tutorial4 Cohesion and Community  
#>  6 manynet   tutorial5 Position and Equivalence
#>  7 manynet   tutorial6 Topology and Resilience 
#>  8 migraph   tutorial7 Diffusion and Learning  
#>  9 migraph   tutorial8 Diversity and Regression
#> 10 migraph   tutorial9 Modelling with ERGMs
#> ℹ You can run a tutorial by typing e.g `run_tute('tutorial1')` or `run_tute('Data')` into the console.
# run_tute("tutorial1")
```

## Installation

### Stable

The easiest way to install the latest stable version of `{manynet}` is
via CRAN. Simply open the R console and enter:

`install.packages('manynet')`

`library(manynet)` will then load the package and make the data and
tutorials (see below) contained within the package available.

### Development

For the latest development version, for slightly earlier access to new
features or for testing, you may wish to download and install the
binaries from Github or install from source locally. The latest binary
releases for all major OSes – Windows, Mac, and Linux – can be found
[here](https://github.com/stocnet/manynet/releases/latest). Download the
appropriate binary for your operating system, and install using an
adapted version of the following commands:

- For Windows:
  `install.packages("~/Downloads/manynet_winOS.zip", repos = NULL)`
- For Mac:
  `install.packages("~/Downloads/manynet_macOS.tgz", repos = NULL)`
- For Unix:
  `install.packages("~/Downloads/manynet_linuxOS.tar.gz", repos = NULL)`

To install from source the latest main version of `{manynet}` from
Github, please install the `{remotes}` package from CRAN and then:

- For latest stable version:
  `remotes::install_github("stocnet/manynet")`
- For latest development version:
  `remotes::install_github("stocnet/manynet@develop")`

### Other sources

Those using Mac computers may also install using Macports:

`sudo port install R-manynet`

## Relationship to other packages

This package stands on the shoulders of several incredible packages.

In terms of the objects it works with, this package aims to provide an
updated, more comprehensive replacement for `{intergraph}`. As such it
works with objects in `{igraph}` and `{network}` formats, but also
equally well with base matrices and edgelists (data frames), and formats
from several other packages.

The user interface is inspired in some ways by Thomas Lin Pedersen’s
excellent `{tidygraph}` package, though makes some different decisions,
and uses the quickest `{igraph}` or `{network}` routines where
available.

`{manynet}` has inherited most of its core functionality from its
maternal package, `{migraph}`. `{migraph}` continues to offer more
analytic and modelling functions that builds upon the architecture
provided by `{manynet}`. For more, please check out `{migraph}`
directly.

## Funding details

Development on this package has been funded by the Swiss National
Science Foundation (SNSF) [Grant Number
188976](https://data.snf.ch/grants/grant/188976): “Power and Networks
and the Rate of Change in Institutional Complexes” (PANARCHIC).
