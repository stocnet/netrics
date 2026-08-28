# Contributing

Contributions to `netrics`, 
whether in the form of issue identification, bug fixes, new code or documentation 
are encouraged and welcome.

Please note that the `netrics` project is released with a 
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). 
By contributing to this project, you agree to abide by its terms.

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

### Identifying issues

Please use the issues tracker on GitHub to identify any function-related issues.
You can use these issues to track progress on the issue and
to comment or continue a conversation on that issue.
The most useful issues are ones that precisely identify an error,
or propose a test that should pass but instead fails.
Examples for documentation are also most welcome.

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

Commits may reference an existing GitHub issue number.
Where the issue number is preceded by `resolve`/`resolves`/`resolved`,
`close`/`closes`/`closed`, or `fix`/`fixes`/`fixed` (capitalised or not),
GitHub updates the status of the issue automatically.

### Branching and CI

- `main` is the release branch; `develop` is the working branch (clone/work on `develop`).
- PRs into `main` trigger [prchecks.yml](workflows/prchecks.yml): R CMD check (macOS/Windows/Linux), binary build, codecov, lintr, spell check, a check that the tutorial articles are in sync with the tutorials, and PR metadata checks (DESCRIPTION version bump, PR title/description conventions).
- Merges/pushes to `main` trigger [pushrelease.yml](workflows/pushrelease.yml): check, auto-bump version tag, GitHub release with binaries, then pkgdown site deploy.
- Commits should reference an existing GitHub issue number (`#123`), see below.

## Style

In terms of style, we are aiming for pleasant predictability in terms of user experience.
To that end, we have a regular syntax that users can rely on producing expected effects.
Functions in the same family (`node_by_*()`, `node_in_*()`, `node_x_*()`, etc.)
should share argument order and naming,
so that behaviour is guessable across the family.

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
- Code coverage: `covr::package_coverage()`
- Rebuild `README.md` from `README.Rmd`: `devtools::build_readme()`
- Check every topic is in the pkgdown index: `pkgdown::check_pkgdown()`
- Build pkgdown site locally: `pkgdown::build_site()`

There is no non-R build system — no package.json/Makefile.
Roxygen is configured with `markdown = TRUE`;
`NAMESPACE` and all `man/*.Rd` files are generated — never hand-edit them.
Some other files are generated rather than edited directly — `README.md` and the tutorial
articles in `vignettes/articles/`.
See [README and website](#readme-and-website) below for which source each is built from.

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

Where one concept appears in several families, give it one stem in each:
`node_is_core()`, `node_by_core()` and `node_in_core()` are the mark, the measure and
the membership of the same idea.

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
| `method_coreness` | a continuous coreness score plus a core/periphery split | `coreness_*` | `coreness =` |

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

### Argument names and vocabulary

One word means one thing across the package, and one thing takes one word.
Before adding an argument, look for the name the package already uses for that idea:

| Argument | Means |
|---|---|
| `normalized` | divide by a theoretical maximum, so scores compare across networks |
| `scaled` | divide by the observed maximum, so the highest-scoring node takes 1 |
| `decay` | any per-step discount, always a proportion on `[0,1]` where higher values discount less |
| `alpha` | only Opsahl et al.'s trade-off between degree and strength in `node_by_degree()` |
| `direction` | `"all"`, `"in"` or `"out"`, validated with `match.arg()` |
| `cutoff` | a geodesic distance bound |
| `k` | a target number of groups, or the name of a `k_*` selection method |
| `cluster`, `coreness`, `regularity` | select a method helper, as above |

Four points follow from this:

- Prefer an existing argument to a new one.
  `decay` replaced four names for one parameter: `decay` in `node_by_harmonic()`,
  `alpha` in `node_by_alpha()`, `beta` in `regularity_rolesim()`,
  and PageRank's damping factor, which was not exposed at all.
- Validate a shared argument in one helper, e.g. `check_decay()`
  ([R/class_metrics.R](../R/class_metrics.R)),
  so that the bound and the message cannot drift apart between measures.
- Document it once as a `man-roxygen/param_*.R` template
  (`param_decay`, `param_cutoff`, `param_norm`), and `@template` it everywhere.
- Where a word cannot carry the same meaning everywhere, that is a sign it is the wrong word,
  not a licence to overload it.
  `node_by_power(exponent=)` is deliberately not a `decay`,
  because a negative exponent inverts the measure rather than discounting it.

### Function body convention

Functions consistently:
1. Coerce/validate input via `manynet::expect_nodes()` / `manynet::as_igraph()` etc.
2. Branch on `manynet::is_twomode()`, `manynet::is_weighted()`, `manynet::is_directed()`, `manynet::is_complex()` to handle one-mode/two-mode, weighted/unweighted, directed/undirected cases distinctly.
3. Compute the result (often via `{igraph}`, but use whichever is the fastest implementation).
4. Wrap the output with the matching `make_*()` constructor to attach the S3 class and labels (node/tie names via `manynet::node_names()`, mode attribute via `manynet::node_is_mode()`/`manynet::net_dims()`).

All `manynet`/`igraph` calls use explicit `::` namespacing rather than importing whole namespaces (`{manynet}` and `{igraph}` are still listed in `@importFrom` roxygen tags per-file for NAMESPACE generation).

Where the function is a measure, the `make_*_measure()` call also declares
`measure`, `range`, `normalization` and `variant`
(see [Adding a measure](#adding-a-measure)).

### Input shapes

Most defects reported against this package are not wrong arithmetic.
They are a network shape the function did not expect.
Before finishing a function, run it on a signed, a weighted, a directed, a two-mode,
a multiplex, a multilevel and a longitudinal network, and decide each case deliberately:

- **Signed.** A `stocnet` object holds a tie's sign as the sign of its weight,
  so a signed network reaches `{igraph}` carrying a `weight` attribute of -1 and 1.
  Shortest-path functions read any attribute of that name as a distance,
  and either abort on the negative values or report a negative cycle.
  Where the measure concerns cohesion or distance, call `.to_positive()`
  ([R/netrics-utils.R](../R/netrics-utils.R)), which drops to the positive ties and says so.
  Do not simply drop the attribute: that reads a negative tie as a path of length one,
  when a negative tie is hostility rather than a channel along which cohesion travels.
- **Multiplex.** Take one layer at a time with `manynet::to_uniplex()`.
  That drops nodes holding none of the retained ties, so results of different lengths
  would otherwise be recycled against each other; `uniplex_degree()`
  ([R/measure_centrality_degree.R](../R/measure_centrality_degree.R))
  restores the whole nodeset, and is the pattern to follow.
- **Multilevel.** A multilevel network reports itself as two-mode,
  but holds ties within a mode as well as between them, so it cannot be projected.
  Measure it whole rather than projecting, as `net_by_independence()` now does.
- **Longitudinal and diffusion.** Nodes may change over waves while the ties do not,
  so do not assume a nodeset and a tieset of matching length or wave.

Document each decision in the roxygen block with an `@section` named for the shape,
e.g. `@section Signed networks:` or `@section Multilevel networks:`.
Say what the function does with such a network,
and how the user can control it themselves, e.g. with `manynet::to_unsigned()`.

### Adding a measure

1. Name it for its family and level, and put it in the `R/measure_*.R` file of its topic,
   sharing a roxygen block with the functions it belongs with.
2. Coerce, branch on the input shapes above, compute,
   then wrap the result with the matching `make_*_measure()`.
3. Declare `measure`, `range`, `normalization` and `variant` in that call.
   These attributes are what lets a result be read without the manual,
   so a measure that resolves its method at run time declares the method it actually used,
   as `net_by_diversity()` and `net_by_core()` do.
4. Add it to the right roster in
   [tests/testthat/helper-contract.R](../tests/testthat/helper-contract.R),
   giving any arguments it needs to be applicable.
   `test-measure_registry_contract.R` compares the rosters against the namespace,
   so a measure in no roster fails the build rather than escaping the sweep.
   Where a measure cannot take the roster's shape, add it to `uncontracted_measures`
   with a comment saying why, and cover it in its family's contract file instead.
5. Add assertions to the mirroring `test-measure_*.R`.
6. Check the website still builds, and add one `NEWS.md` bullet.

An exemption is a declaration, not a gap: where an argument cannot have an effect,
record it in the exemption list with the reason, as the eigenvector measures do.

### Renaming or retiring a name

A rename is cheap for us and expensive for users, so each one carries a shim:

- **A renamed function** gets a forwarding shim in
  [R/netrics-defunct.R](../R/netrics-defunct.R), which calls `.Deprecated()`
  and is documented with `@describeIn defunct Deprecated on <date>.`
  plus one sentence on why the new name is better.
  These shims are cleared at each minor release.
- **A renamed argument** keeps the old spelling as an argument defaulting to `NULL`,
  resolved by a `resolve_*()` helper in [R/class_metrics.R](../R/class_metrics.R)
  that warns and returns the new value.
  Use base `warning()` there rather than `manynet::snet_warn()`:
  `snet_*()` output is quiet by default, and a renamed argument is something
  the user must act on.
- **Fold rather than duplicate where the new name subsumes the old.**
  `net_x_mixed()` became a case of `net_x_triad()`, which now takes the multilevel census
  whenever it is given both a one-mode and a two-mode network.
- **Update the prose too.** The README, the tutorials and the vignettes advertise
  function names, so grep for the old name after every rename.

### File organization

`R/` files are organized by function family and topic, not one-file-per-function: e.g. `measure_centrality_degree.R`, `measure_cohesion.R`, `member_community.R`, `motif_brokerage.R`, `mark_nodes.R`/`mark_ties.R`. Related functions (e.g. `node_by_degree()` and its shortcuts `node_by_deg()`, `node_by_indegree()`, `node_by_outdegree()`) share one `@name`/roxygen block and file.

Shared roxygen documentation blocks live in `man-roxygen/` as `@template` fragments (e.g. `param_data.R`, `node_measure.R`, `param_norm.R`) — reuse these templates via `@template` tags instead of re-writing standard `@param`/`@returns` docs.

### Tests

This package uses the `testthat` package for testing functions.
Please see the [testthat website](https://testthat.r-lib.org) for more details.
`testthat` edition 3 with parallel execution is configured in `DESCRIPTION` (`Config/testthat/parallel: true`). 
`Config/testthat/start-first` should prioritise the test files that take longest to run.

The main testing is *functional* (family-enumerating) testing.
Rather than a test per function, each family sweeps its whole roster from
[tests/testthat/helper-contract.R](../tests/testthat/helper-contract.R)
and checks the promises the documentation makes:
that a measure returns the right shape, declares what it computed,
stays inside the range it declares, performs the normalisation it declares,
and that its arguments actually do something.
Where a function does not yet meet the contract,
the sweep records an audit message rather than failing,
so the outstanding gaps are enumerated on every run instead of being
either invisible or a red build.
`report_contract_gaps()` prints that list, which is the remaining work,
and the aim is for it to shrink to empty.

`test-tutorials_netrics.R` evaluates the code chunks of the tutorials in `inst/tutorials/`,
so tutorial code that errors or raises a deprecation warning fails the suite.

Any additional testing that is required for particular functions is covered in
test files that mirror the `R/` files (e.g. `test-measure_centrality.R`, `test-member_community.R`). 
`tests/testthat/helper-netrics.R` defines shared custom expectations/helpers used across tests:
- `expect_values(object, ref)` — compares rounded numeric output against reference values.
- `expect_mark(object, ref, top)` — compares character/label output.
- `top3()`/`bot3()`/`top5()`/`bot5()` — pull top/bottom N values (rounded) from a result for use as terse reference vectors in assertions.
The aim is to work towards comprehensive coverage,
so each change should be fully covered by tests.
However, we also need to keep an eye on the clock:
CRAN complains if tests take too long,
so use small fixtures or skip taxing tests.
`# nocov start` and `# nocov end` can be used to exclude lines or functions
that are too difficult to cover.

### Dependencies

`netrics` `Depends` on `manynet` (network classes, coercion and logical tests),
`Imports` `dplyr` and `igraph` (>= 2.1.0),
and lists `autograph`, `sna` and `testthat` under `Suggests`,
so code paths depending on a suggested package must guard with `requireNamespace()`
or skip gracefully when it is unavailable.

The declared minimum of each `stocnet` dependency is the version on CRAN,
so that CI can install it.
Where `netrics` needs something that only a newer, unreleased `manynet` has,
reach it through a shim in [R/netrics-utils.R](../R/netrics-utils.R)
rather than by raising the minimum.
Resolve the name at call time from the namespace, as `.to_linegraph()` does for
`manynet::to_linegraph()`, which was `to_ties()` before manynet 2.3.0.
Test for the function rather than for the version string,
because a pre-release development build can carry the version
without yet exporting the function.
Delete each shim once the minimum is raised past the version that added the function.

### Console messaging

All user-facing messages go through the `snet_*()` wrappers exported by `{manynet}`,
rather than base `message()`/`stop()`/`warning()` or `{cli}` calls directly:

| Wrapper | Use for |
|---|---|
| `snet_abort()` | errors: the function cannot proceed |
| `snet_warn()` | the function proceeds, but the user should know something |
| `snet_info()` | notable information about what was done, e.g. a defaulted argument or the method dispatched to |
| `snet_minor_info()` | incidental detail |
| `snet_success()` | confirmation that a requested operation completed |
| `snet_prompt()` | interactive questions to the user |
| `snet_unavailable()` | not-yet-implemented features |
| `snet_progress_step()`, `snet_progress_along()`, `snet_progress_seq()`, `snet_progress_nodes()` | progress reporting in longer-running loops |

Every wrapper except `snet_abort()` (and `snet_prompt()`) is silenced by
`options(snet_verbosity = "quiet")`, which is the *default* —
so informational output must never be load-bearing,
and errors must carry everything the user needs to act.
Users opt in with e.g. `options(snet_verbosity = "verbose")`.

These wrappers pass their input to `{cli}`, so:

- Braces interpolate, replacing `paste()`: `snet_abort("{.val {unknown}} is not a recognised method.")`.
- Use `{cli}` inline classes to mark up what you refer to — `{.fn}` for functions,
  `{.arg}`/`{.var}` for arguments and variables, `{.val}` for values,
  `{.url}` for links — so that styling stays consistent across the ecosystem.
- Use `{cli}`'s pluralisation rather than hand-written branches:
  `snet_warn("Node{?s} {.val {missing}} {?was/were} dropped.")`.
- Multiple strings can be passed as separate arguments for multiline messages.

Messages, warnings, and errors should be written in a way that is useful for new and advanced users alike.
This might include listing likely causes, mentioning objects or variables explicitly,
and indicating next actions clearly.
Prefer "`{.arg alpha}` must be a single number between 0 and 1" over "invalid input".
Functions that dispatch on a character argument should name the method they chose,
e.g. `manynet::snet_info("...using {.fn regularity_{regularity}}.")`,
which surfaces the method-helper convention above at run time.

### Documentation

Roxygen is configured with `markdown = TRUE`;
`NAMESPACE` and all `man/*.Rd` files are generated — never hand-edit them.
Run `devtools::document()` after changing any roxygen comment.

- Reuse the shared `@template` fragments in `man-roxygen/` (e.g. `param_data.R`,
  `node_measure.R`, `param_norm.R`) instead of re-writing standard `@param`/`@returns` docs.
  If you find yourself writing the same `@param` twice, add a template.
  Indeed, prefer defining fewer arguments, so if alpha and beta are both decays,
  just use `decay=` as the argument.
- Related functions share one roxygen block via `@name`/`@rdname`,
  matching the file organisation above.
- Every exported function needs a runnable `@examples` block:
  examples are run by R CMD check, and they are also the fastest documentation for users.
  Prefer the bundled `ison_*`/`fict_*` networks over ad hoc constructions,
  unless they take too long to run.
- Cite the source of a measure with `@references` in the ecosystem's format
  (authors, year, title, journal, and `\doi{}` where available),
  so that users can trace an implementation back to its definition.
- Documented behaviour and implemented behaviour must agree.
  Several past bugs were documentation claiming a default or a normalisation that the code did not apply,
  so when you change a default, search the roxygen and templates for it too.

### README and website

The README offers a landing page for new users, both on the GitHub repository
as well as on the website.
As such, it should make a compelling case for the value added of the package,
and not drift out of date.
Note that `README.md` is generated from `README.Rmd` — edit `README.Rmd` and re-knit
(`devtools::build_readme()`), never edit `README.md` directly.

The website is created by pkgdown from [pkgdown/_pkgdown.yml](../pkgdown/_pkgdown.yml),
and is deployed automatically when changes reach `main`.
Please make sure that the pkgdown website will build correctly before opening a PR:

```r
pkgdown::check_pkgdown()              # every topic is in the index
pkgdown::build_site(preview = FALSE)  # everything else
```

The most common failure is a new exported function that is not picked up under the
function overview (the `reference:` section of `_pkgdown.yml`) —
pkgdown requires *every* exported topic to appear there exactly once, or it will not build.
Where possible, add functions to an existing subtitle's `starts_with()`/`contains()` pattern
(e.g. a new `node_is_*()` mark or `node_in_*()` membership needs no change),
and only list the topic explicitly where it does not fit a pattern.
A helper that users are not meant to call takes `@keywords internal` instead.
These `reference:` titles are also the headings used in `NEWS.md` (see below),
so keep the two in step.

The `{learnr}` tutorials in `inst/tutorials/` are the source.
`vignettes/articles/*.Rmd` are their static pkgdown twins, and are *generated* from them by
[data-raw/build_tutorial_articles.R](../data-raw/build_tutorial_articles.R).
Never edit an article by hand:
the next regeneration discards the edit,
and [prchecks.yml](workflows/prchecks.yml) fails the PR for drift meanwhile.

After adding or changing functionality,
ask whether a reader learning the package would meet it, and if so:

1. Edit the tutorial in `inst/tutorials/<tute>/*.Rmd`,
   adding the function to the topic it belongs to in an `exercise=TRUE` chunk,
   with a sentence saying what it is for.
2. Re-run `Rscript data-raw/build_tutorial_articles.R`,
   and commit the regenerated article.
3. Run `testthat::test_file("tests/testthat/test-tutorials_netrics.R")`,
   which evaluates every chunk, so new tutorial code is tested.

A tutorial is also where a renamed function shows up as stale,
so check the tutorials whenever you rename one.
New tutorials need an entry under `articles:` in `_pkgdown.yml`.

### `NEWS.md` conventions

`NEWS.md` groups each version's changes under `##` headings that mirror the website
function overview (`pkgdown/_pkgdown.yml` `reference:` titles).
Lead with `## Package` (package-wide/website/infrastructure changes),
then the function families in overview order:
`## Marks`, `## Measures`, `## Memberships`, `## Motifs`, `## Methods`.
Put `## Tutorials` and any `## Data` section at the end.
Each heading appears at most once per version.

Start each bullet with a verb matching the change type:

- `Added ...` — new functionality
- `Fixed ...` — bug fixes; if it relates to a GitHub issue, suffix with `(closing #123)`
- `Renamed ... to ...` — function or data name migrations
- `Improved ...` — functional updates to existing behaviour
- `Updated ...` — documentation changes

Any of these verbs can also lead a sub-bullet,
though `Improved ...` is perhaps most commonly used to cluster changes 
relating to a single function.
If so, the function need only be named once, at the top;
sub-bullets will obviously relate to that function.

If a cited GitHub issue was **not** authored by @jhollway, thank the author with an
`@`-tag in the bullet.

#### Grouping

Group first, and only then write the bullets.
The more entries a version holds, the more this matters.

- Cluster related changes as indented sub-bullets under a lead bullet.
- Where several changes concern one function, lead with an `Improved ...` bullet naming
  the function, and put the individual `Fixed ...`/`Added ...` points beneath it,
  so the cluster groups by function rather than by change type.
- Under such a lead bullet, do not name the function again in the sub-bullets,
  since the lead bullet already carries it.
- Where one decision runs across many functions, lead with the decision rather than
  with each function, as the `decay` and measure-attribute entries do.
- Sub-bullets indent by two spaces, and nest at most one level further (four spaces).

#### Writing the bullets

`NEWS.md` is read by users scanning for what changed, not by reviewers reading prose,
so each bullet is a headline rather than a sentence,
so avoid over-punctuation or over-explanation.
Details can be added to the function documentation, if necessary.

- No full stop at the end of a bullet
- Keep every bullet to one line of fewer than 81 characters ideally
  (a few more or less is fine)
  - If a bullet wraps, it holds too much: shorten it,
    or split it into a lead bullet and sub-bullets
- One clause where possible, and at most one comma
  - Use a semicolon for a short second clause, e.g. "old spelling still works but warns"
  - Use a sub-bullet where the second clause needs more room than that
- Name the function or object in backticks and say what changed to it,
  dropping scaffolding like "This change ...", "In order to ...", or "as part of an effort to"
- Keep the *what*, and add the *why* only where the behaviour would otherwise look arbitrary
- No trailing rationale, no restating the same change twice in different words,
  and no marketing adjectives such as "comprehensive" or "robust"
- A sub-bullet does not need a verb: it can state the consequence,
  the previous behaviour, or an example call
- Cut a sub-bullet that only restates what the lead bullet already implies
- Where several bullets describe parallel changes, reuse the sentence structure,
  so that a reader sees the parallelism at a glance
- Use one word for one thing throughout a version's entries,
  rather than varying the wording for effect

For example, instead of:

> Fixed a bug where, in some cases, `node_by_reach()` was counting the node itself,
> which meant that normalised scores could exceed 1.

write:

> Fixed `node_by_reach()` counting the node itself so normalised scores no longer exceed 1

and instead of:

> Added a new function, `net_by_compactness()`, which is a useful measure that
> calculates the average closeness of all pairs of nodes in the network.

write:

> Added `net_by_compactness()` for the average closeness of all pairs of nodes
