## Test environments

* local R installation, aarch64-apple-darwin20, R 4.5.2
* macOS 15.7.4 (on Github), R 4.5.2
* Microsoft Windows Server 2025 10.0.26100 (on Github), R 4.5.2
* Ubuntu 24.04.3 (on Github), R 4.5.2

## R CMD check results

0 errors | 0 warnings | 0 notes

- This package is ready for submission to CRAN. 
The check results are clean, with no errors, warnings, or notes across all tested environments.
Note that there are some conflicts with manynet functions,
but these are not causing any issues with the package itself and 
will be resolved by removing them from manynet once netrics is available on CRAN.
