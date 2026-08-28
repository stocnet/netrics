test_that("netrics tutorial code runs without warnings or errors", {
  # Running every chunk of the four tutorials takes about twenty minutes, which
  # is the whole of this package's check time. The tutorials exercise the same
  # functions that the other test files cover, so CRAN gains little from them.
  skip_on_cran()
  skip_if_not_installed("netrics", minimum_version = "0.2.2")
  for(tute in find_pkg_tutorial_paths("netrics")){
    expect_null(check_tute_functions(tute), 
                info = paste("Error in tutorial", basename(tute)))
  }
})