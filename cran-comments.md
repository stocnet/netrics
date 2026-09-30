## Test environments

* local R installation, macOS 26.5.2, aarch64-apple-darwin23, R 4.6.1 (release)

## R CMD check results

0 errors | 0 warnings | 0 notes

This release adds `net_by_divergence()`, several arguments to existing
measures and memberships, and fixes how the functions read a two-mode network
and a cognitive social structure.

It also prepares for manynet 2.4.0, which follows this submission.
The local check gives 0 errors, 0 warnings, and 0 notes against manynet 2.3.4
(the current CRAN version), 2.3.5, and 2.4.0.
