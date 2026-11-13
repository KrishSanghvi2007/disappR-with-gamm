# disappR test tiers, run in order from the package root:  Rscript tests/scripts/run_all.R
#   0 unit tests (testthat)                     tests/testthat/
#   1 smoke (engine checks as a script)            tests/scripts/smoke_test.R
#   2 golden regression (optional reference)       tests/golden/  (set DISAPPR_GOLDEN_REF=/path/to/reference.rds)
#   3 adversarial, metamorphic, oracle, export     tests/scripts/robustness_suite.R
#   4 app server (reactive behaviour)              tests/scripts/app_test.R
#   5 stress (out-of-sample data)                  tests/scripts/stress_test.R
#   6 empirical replication and simulations        tests/scripts/stress_empirical.R
#   7 sweep of the 0.20.9-0.20.19 changes          tests/scripts/sweep_0_20.R
#   8 complex scenarios (0.22.7)                   tests/scripts/complex_scenarios.R
#   9 trait-specific ALR/AFR snapshot (0.24.2)     tests/scripts/trait_ages_snapshot.R
# Each tier runs in a fresh R process; the script exits with status 1 if any tier fails.
# From an R console (RStudio included), with the working directory at the package root:
#   source("tests/scripts/run_all.R")
# In a console a failure stops with an error instead of quitting R.
if (!file.exists("DESCRIPTION")) stop("Run from the package root (the folder containing DESCRIPTION).")
rscript <- file.path(R.home("bin"), "Rscript")
run <- function(label, args) {
  cat("\n=====", label, "=====\n")
  t0 <- Sys.time()
  status <- suppressWarnings(system2(rscript, args))
  cat(sprintf("----- %s: %s (%.0fs)\n", label, if (identical(status, 0L)) "passed" else "FAILED", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  identical(status, 0L)
}
res <- c(unit = run("0 unit tests", c("-e", shQuote("testthat::test_local('.', stop_on_failure = TRUE)"))))
res["smoke"] <- run("1 smoke", "tests/scripts/smoke_test.R")
ref <- Sys.getenv("DISAPPR_GOLDEN_REF")
if (nzchar(ref)) {
  new <- tempfile(fileext = ".rds")
  res["golden"] <- run("2 golden capture", c("tests/golden/golden_capture.R", ".", new)) &&
    run("2 golden compare", c("tests/golden/golden_compare.R", shQuote(ref), new))
} else cat("\n===== 2 golden regression: skipped (set DISAPPR_GOLDEN_REF to a reference capture) =====\n")
res["robustness"] <- run("3 robustness", "tests/scripts/robustness_suite.R")
res["app"] <- run("4 app server", "tests/scripts/app_test.R")
res["stress"] <- run("5 stress", "tests/scripts/stress_test.R")
res["empirical"] <- run("6 empirical and simulations", "tests/scripts/stress_empirical.R")
res["sweep"] <- run("7 sweep of the 0.20.9-0.20.19 changes", "tests/scripts/sweep_0_20.R")
res["complex"] <- run("8 complex scenarios", "tests/scripts/complex_scenarios.R")
res["trait_ages"] <- run("9 trait-specific ALR/AFR snapshot", "tests/scripts/trait_ages_snapshot.R")
cat("\nSummary:", paste(names(res), ifelse(res, "passed", "FAILED"), sep = ": ", collapse = "; "), "\n")
if (!all(res)) {
  if (interactive()) stop("Some test tiers failed: ", paste(names(res)[!res], collapse = ", "), call. = FALSE)
  quit(status = 1)
}
