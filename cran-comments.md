## Test environments

* local R installation, macOS 26.5.2, aarch64-apple-darwin23, R 4.6.1 (release)
* macOS 26.4 (on Github), R 4.6.1
* Microsoft Windows Server 2025 10.0.26100 (on Github), R 4.6.1
* Ubuntu 24.04.4 (on Github), R 4.6.1

## R CMD check results

0 errors | 0 warnings | 0 notes

This release fixes the two test failures that netrics 1.0.1 shows against
manynet 2.3.4, which is currently in the submission queue. 
Both come from changes to reporting inherited from manynet rather than from the measures themselves.
The test suite here passes against manynet 2.3.1 and 2.3.4.
