## Test environments

* local macOS 27.0.1, aarch64-apple-darwin, R 4.6.1
* GitHub Actions, `R CMD check --as-cran`:
  * macOS Tahoe 26.7.2 (macos-26-arm64), aarch64-apple-darwin23, R 4.6.2 (release)
  * Windows Server 2025 (windows-2025), x86_64-w64-mingw32, R 4.6.2 ucrt (release)
  * Ubuntu 26.04.1 LTS, x86_64-pc-linux-gnu, R 4.6.2 (release)

## R CMD check results

0 errors | 0 warnings | 0 notes

This release is part of a coordinated update after manynet 2.5.0,
which is now on CRAN. netrics 1.2.0 needs manynet (>= 2.5.0).
autograph 1.3.0 is submitted at the same time.

This release adds measures of opinion dynamics, complex contagion,
main paths, and spatial hot spots, and fixes the diffusion measures.
No exported function is removed. `tie_is_multiple()` is kept as a
wrapper for its new name, `tie_is_repeated()`.

## Reverse dependencies

We checked the 2 reverse dependencies on CRAN,
with netrics 1.2.0 and manynet 2.5.0:

* migraph 1.7.2: 0 errors | 0 warnings | 0 notes,
  with autograph 1.2.8 and with autograph 1.3.0.
* autograph 1.2.8: 1 error in a tutorial test,
  `graphs(to_subgraphs(fict_lotr, "Race"), waves = c(1, 2, 3, 4))`.
  The same error occurs with netrics 1.1.0, so netrics does not cause it.
  It comes from manynet 2.5.0, and autograph 1.3.0 fixes it.

The netrics check also passes with autograph 1.2.8 and with autograph 1.3.0.
