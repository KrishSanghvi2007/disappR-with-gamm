# The app server test (tests/scripts/app_test.R) as part of the unit tests, so that continuous integration runs it on
# every push (0.23.2; code review). It drives the real server with shiny::testServer() and renders every output.
test_that("every app output renders without an error", {
  skip_on_cran()
  skip_if_not_installed("callr")
  skip_if_not_installed("shiny")
  root <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  script <- file.path(root, "tests", "scripts", "app_test.R")
  skip_if_not(file.exists(script) && file.exists(file.path(root, "inst", "app", "global.R")), "package sources not available")
  # absolute paths: the script runs with the package root as its working directory (0.24.1)
  res <- callr::rscript(script, wd = root, show = FALSE, fail_on_status = FALSE, spinner = FALSE)
  expect_identical(res$status, 0L, info = paste(utils::tail(strsplit(res$stdout, "\n")[[1]], 25), collapse = "\n"))
})
