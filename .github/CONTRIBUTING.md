# Contributing

Contributions to `netrics`, 
whether in the form of issue identification, bug fixes, new code or documentation 
are encouraged and welcome.

## Aims

Here is some things that Guy Kawasaki, Silicon Valley venture capitalist,
learned from Steve Jobs:

- "Experts" are clueless. Especially self-declared ones.
- Customers cannot tell you what they need. They can help with evolution, but not revolution.
- Biggest challenges beget the best work.
- Design counts. Users will see the skin/UI of your product, not the great algorithms.
- Big graphics, big fonts.
- Jump curves---do things 10 times better, not 10 percent.
- All that truly matters is whether something works or doesn't work. Open or close, iPhone or Android, car or train, doesn't matter---make
it work.
- "Value" is different from "price". There is a class of people who do care about value. Ease of use -> less support costs. You have to create a unique and valuable product as an engineer.
- Real CEOs can demo. If you can't demo your own product, then quit.
- Real entrepreneurs ship, not slip.
- Some things need to be believed to be seen.

## Git

`stocnet` projects are maintained using the git version control system.
A plain-English introduction to git can be found [here](https://blog.red-badger.com/2016/11/29/gitgithub-in-plain-english).
I recommend you read this before continuing. 
A more recent motivation can be found [here](https://www.r-bloggers.com/2024/04/git-gud-version-control-best-practices/).
It will explain the basics of git version control, committing and repos, pulling and pushing,
branching and merging.

Using git from the command line on your lap- or desktop can be intimidating,
but I recommend [Fork](https://git-fork.com) software for Mac and Windows.
This allows mostly visual management of commits, diffs, branches, etc.
There are various other git software packages available, but this one is fairly fully featured.

The GitHub page allows to access the issues assigned to you and check the commits.
You can also access the documents in the repository, 
although this won't be necessary after you have cloned it on your computer via Fork.

## Style

In terms of style, we are aiming for pleasant predictability in terms of user experience.
To that end, we have a regular syntax that users can rely on producing expected effects.

## Package architecture

### Project overview

`netrics` is an R package (part of the [stocnet](https://github.com/stocnet) ecosystem) providing the *analytic engine* for network analysis: marks, measures, memberships, and motifs for nodes, ties, and networks. 
It depends on `{manynet}` (see below). Division of labour to keep in mind when adding functions:
- `{manynet}`: network classes/coercion (`as_*()`) and network-level logical tests (e.g. `is_directed()`, `is_twomode()`).
- `{autograph}`: functions for drawing graphs and plotting network analytic or modelling results and diagnostics, along with deep (often institutional) theming. All plot methods should live here.
- `{netrics}` (this package): everything analytic — marks, measures, memberships, motifs — at the node, tie, and network level.
- `{migraph}`: functions for testing and modelling, e.g. QAP/MRQAP and diffusion models

### Common commands

This is a standard R package developed with `devtools`/`roxygen2`. 
Run these from an R console with the working directory set to the package root (or via `Rscript -e`).

- Install dependencies / load for development: `devtools::load_all()`
- Regenerate docs & NAMESPACE after editing roxygen comments: `devtools::document()`
- Run full test suite: `devtools::test()`
- Run a single test file: `devtools::test(filter = "measure_centrality")` (matches `test-measure_centrality.R`), or `testthat::test_file("tests/testthat/test-measure_centrality.R")`
- Full package check (mirrors CI): `devtools::check()` or `rcmdcheck::rcmdcheck()`
- Lint: `lintr::lint_package()`
- Spell check: `spelling::spell_check_package()`
- Build pkgdown site locally: `pkgdown::build_site()`

There is no non-R build system — no package.json/Makefile.

### Function family naming (the core convention)

Functions are grouped into four families by naming pattern, each with dedicated `print()` S3 methods and a `make_*()` constructor in [R/class_metrics.R](../R/class_metrics.R):

| Family | Pattern | Level | Returns | Constructor |
|---|---|---|---|---|
| Marks | `node_is_*()`, `tie_is_*()` | node/tie (network `is_*()` are in `{manynet}`) | logical vector | `make_node_mark()`, `make_tie_mark()` |
| Measures | `net_by_*()`, `mode_by_*()`, `node_by_*()`, `tie_by_*()` | network/mode/node/tie | numeric (vector) | `make_network_measure()`, `make_mode_measure()`, `make_node_measure()`, `make_tie_measure()` |
| Memberships | `node_in_*()` | node | character vector (group labels, via `MORELETTERS`) | `make_node_member()` |
| Motifs | `net_x_*()`, `node_x_*()` | network/node | tabular | `make_network_motif()`, `make_node_motif()` |

When adding a new analytic function, pick the family that matches its semantics and follow the existing naming scheme exactly.
This predictability is a stated project goal.

### Method helper naming

Besides the four analytic families, some functions take a **character argument that selects a method**.
These are not S3 methods; dispatch is by `switch()`. The rule is that the function called is named **`<argument>_<value>`**, so `k = "elbow"` calls `k_elbow()`, `cluster = "concor"` calls `cluster_concor()`, and `regularity = "rege"` calls `regularity_rege()`.
Users can therefore find the implementation, and its documentation, from the argument alone.

**Each family is named for what it returns** — not for the concept it serves, and not for the function that calls it:

| Rd name | Returns | Functions | Argument |
|---|---|---|---|
| `method_kselect` | an integer, the number of clusters | `k_*` | `k =` |
| `method_cluster` | an `hclust` clustering object | `cluster_*` | `cluster =` |
| `method_regularity` | a node-by-node similarity matrix | `regularity_*` | `regularity =` |

Apply that test when naming a new family. For example, `equivalence_*` would be the wrong name for `regularity_*`, even though those methods are only ever called from `node_in_regular()`: they return a *similarity*, which `cluster_*()` only later partitions into an equivalence. Naming the step for the pipeline's eventual output rather than its own return value breaks the rule.

Two further points of style:

- Pick a word narrow enough to own the family. `regularity` is preferred over `similarity` because the latter is broad enough to be overrun later, and because generic similarities (`to_cosine()`, `to_correlation()`) belong to `{manynet}` and are consumed here through `distance =` and `cluster_*()`, so they would never live in this family anyway.
- The dispatching function should name the method in its `snet_info()` message by interpolation, e.g. `manynet::snet_info("...using {.fn regularity_{regularity}}.")`. This surfaces the convention to users at run time, and makes it obvious if the argument and the prefix ever drift apart.

One known exception: `node_in_equivalence()`'s `motif =` argument is fed by `node_x_*()` functions rather than `motif_*()` ones. Motifs are one of the four core families above and cannot be renamed to suit this rule, so leave that as it is.

### Naming within the membership family

`node_in_*()` names divide into two kinds, and new functions should follow whichever fits:

- **Group-nouns** name the grouping itself, and are the generic entry point where there is one: `node_in_community()` tries every applicable algorithm and returns the highest-modularity partition; `node_in_component()` sits above `node_in_strong()`/`node_in_weak()`. Also `node_in_core()`, `node_in_block()`.
- **Algorithm names** name one specific method: `node_in_louvain()`, `node_in_leiden()`, `node_in_walktrap()`, `node_in_infomap()`, `node_in_spinglass()`, `node_in_roulette()`, `node_in_partition()` (Kernighan–Lin).

Two rules about number:

- **Number follows level, not stem.** `node_in_*()` is singular, because a node belongs to one group; `net_by_*()` takes the plural when the measure concerns all of them. Hence `node_in_component()` with `net_by_components()`. Do not "correct" one of a pair to match the other — the mismatch is the convention. (Note that `net_by_*` names ending in *s* are not all plurals: `betweenness`, `compactness`, `richness` and `toughness` are abstract nouns. The real plurals are `components`, `factions` and `waves`.)
- **The rule is about number, not about the stem.** It settles whether to write `component` or `components` at a given level; it does not establish that a stem is the right one. `net_by_components()` returns a count *of the components*, a fact about the things named — but a measure of, say, how far a partition departs from an ideal structure is not a fact about those groups in that way, and should be named for the quantity it returns instead. That is why the blockmodelling criterion is `net_by_inconsistency()` rather than `net_by_blocks()`, even though its partitions come from `node_in_block()`.
- **Never plural in `node_in_*()`**, both because of the rule above and because `to_*s()` already means "returns a list" in `{manynet}` (`to_components()`, `to_egos()`).

Finally, avoid words that imply another stocnet package's remit. `{netrics}` is descriptive; statistical modelling and testing belong to `{migraph}`. This is why the direct blockmodelling search is `node_in_block()` rather than `node_in_blockmodel()`, even though "blockmodel" is the literature's term — prose and `@section` headings should still say blockmodelling, since it is only the exported name that signals remit.

### Function body convention

Functions consistently:
1. Coerce/validate input via `manynet::expect_nodes()` / `manynet::as_igraph()` etc.
2. Branch on `manynet::is_twomode()`, `manynet::is_weighted()`, `manynet::is_directed()`, `manynet::is_complex()` to handle one-mode/two-mode, weighted/unweighted, directed/undirected cases distinctly.
3. Compute the result (often via `{igraph}`, but use whichever is the fastest implementation).
4. Wrap the output with the matching `make_*()` constructor to attach the S3 class and labels (node/tie names via `manynet::node_names()`, mode attribute via `manynet::node_is_mode()`/`manynet::net_dims()`).

All `manynet`/`igraph` calls use explicit `::` namespacing rather than importing whole namespaces (`{manynet}` and `{igraph}` are still listed in `@importFrom` roxygen tags per-file for NAMESPACE generation).

### File organization

`R/` files are organized by function family and topic, not one-file-per-function: e.g. `measure_centrality_degree.R`, `measure_cohesion.R`, `member_community.R`, `motif_brokerage.R`, `mark_nodes.R`/`mark_ties.R`. Related functions (e.g. `node_by_degree()` and its shortcuts `node_by_deg()`, `node_by_indegree()`, `node_by_outdegree()`) share one `@name`/roxygen block and file.

Shared roxygen documentation blocks live in `man-roxygen/` as `@template` fragments (e.g. `param_data.R`, `node_measure.R`, `param_norm.R`) — reuse these templates via `@template` tags instead of re-writing standard `@param`/`@returns` docs.

### Tests

Tests in `tests/testthat/` mirror the `R/` files (e.g. `test-measure_centrality.R`, `test-member_community.R`). 
`tests/testthat/helper-netrics.R` defines shared custom expectations/helpers used across tests:
- `expect_values(object, ref)` — compares rounded numeric output against reference values.
- `expect_mark(object, ref, top)` — compares character/label output.
- `top3()`/`bot3()`/`top5()`/`bot5()` — pull top/bottom N values (rounded) from a result for use as terse reference vectors in assertions.

`testthat` edition 3 with parallel execution is configured in `DESCRIPTION` (`Config/testthat/parallel: true`). 
`Config/testthat/start-first` prioritizes `tutorials_netrics, measure_net, member_nodes, measure_nodes`.

### Branching and CI

- `main` is the release branch; `develop` is the working branch (clone/work on `develop`).
- PRs into `main` trigger [prchecks.yml](workflows/prchecks.yml): R CMD check (macOS/Windows/Linux), binary build, codecov, lintr, spell check, and PR metadata checks (DESCRIPTION version bump, PR title/description conventions).
- Merges/pushes to `main` trigger [pushrelease.yml](workflows/pushrelease.yml): check, auto-bump version tag, GitHub release with binaries, then pkgdown site deploy.
- Commits should reference an existing GitHub issue number (`#123`), see below.

## Fork

### Cloning
Once you have downloaded Fork, the first thing you have to do is to 
clone the remote repository on your computer. 
Before cloning, you will be able to choose on which `branch` you want to work: 
develop or main. 

### Pull 
This command allows you to `pull` changes from the remote repository to your local repository on Sourcetree.
Make sure you do that before starting working on your files so you have the newest versions. 
When pulling, make sure you choose master or develop, 
depending on the branch you decided to work with. 
Once you pulled, you have now all the new commits and files and 
you can start working on your assigned tasks.
Note that you can access and open the files either from the Finder or from Fork. 
Some documents might be stored using Large File Storage (LFS) to save space on the repository. 

### Commit and Push

Once you have made modifications on a file and saved them, it will appear in your `commit` window. 
Here you can control one last time your file, write the commit message with the 
issue reference (see below) and commit. 
Once your commit is ready, you can `push` them to the origin/main repository.
Note that you can click the "push immediately" box in the commit window 
if you don't want to do it in two steps. 
If you are working on a separate branch, 
it is important to select this branch when pushing to origin/main.

## Issues and tests

Please use the issues tracker on GitHub to identify any function-related issues.
You can use these issues to track progress on the issue and 
to comment or continue a conversation on that issue.
Currently issue tracking is only open to those involved in the project.

The most useful issues are ones that precisely identify an error,
or propose a test that should pass but instead fails.
This package uses the `testthat` package for testing functions.
Please see the [testthat website](https://testthat.r-lib.org) for more details.

## Bug fixing or adding new code

Independent or assigned code contributions are most welcome.
When writing new code, please follow 
[standard R guidelines](https://www.r-bloggers.com/🖊-r-coding-style-guide/). 
It can help to use packages such as `lintr`, `goodpractice` and `formatR` 
to ensure these are followed.

Currently, commits can only be pushed to GitHub where they reference an existing issue.
If no issue exists for the code you have developed, please add an issue first before pushing.
Once the issue exists, you will need to mention the issue number (preceded by a hash symbol: #)
in the commit description:

` Resolved #31 by adding a new function that does things, also updated documentation `

Where the issue hash (i.e. #31) is preceded by
`resolve`, `resolves`, `resolved`, `close`, `closes`, `closed`, `fix`, `fixes`, or `fixed`
(capitalised or not),
Github will automatically updated the status of the issue(s) mentioned.

Our current syntactical standard is to mention the issue first and then 
provide a short description of what the committed changes do 
in relation to that issue.
Any ancillary changes can be mentioned after a comma.

## Documentation

A final way of contributing to the package is in developing the 
vignettes/articles that illustrate the value added in the package. 
Please contact me with any proposals here.

Please note that the `netrics` project is released with a 
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). 
By contributing to this project, you agree to abide by its terms.

