# ---------------------------------------------------------------------------
# All out-of-app diagnostics, each in a fresh R process.
#
# From a terminal, in the package source directory:
#     Rscript tests/diagnostics/run_diagnostics.R
#     Rscript tests/diagnostics/run_diagnostics.R --with-ui
#
# From the R console, in the package source directory:
#     setwd("~/Desktop/disappR")
#     source("tests/diagnostics/run_diagnostics.R")
#     run_diagnostics()              # or run_diagnostics(with_ui = TRUE)
#
# disappR must be installed; the working directory must be the package source.
# ---------------------------------------------------------------------------
run_diagnostics <- function(with_ui = FALSE, only = NULL) {
  here <- if (dir.exists("tests/diagnostics")) "tests/diagnostics" else "."
  if (!file.exists(file.path(here, "01_suggests_matrix.R")))
    stop("Run this from the package source directory, e.g. setwd(\"~/Desktop/disappR\").", call. = FALSE)
  scripts <- c("01_suggests_matrix.R", "02_export_reproduction.R", "03_import_fuzz.R",
               "04_locale_encoding.R", "05_determinism_scale.R", "06_package_checks.R")
  if (isTRUE(with_ui)) scripts <- c(scripts, "07_ui_walkthrough.R")
  if (!is.null(only)) scripts <- scripts[grepl(paste(only, collapse = "|"), scripts)]
  Sys.setenv(NOT_CRAN = "true")      # inherited by every script; shinytest2 refuses to start without it
  status <- integer(0)
  for (s in scripts) {
    cat("\n", strrep("=", 78), "\n", s, "\n", strrep("=", 78), "\n", sep = "")
    status[s] <- system2(file.path(R.home("bin"), "Rscript"), shQuote(file.path(here, s)),
                         stdout = "", stderr = "")
  }
  cat("\n", strrep("=", 78), "\n", sep = "")
  for (s in names(status))
    cat(sprintf("%-28s %s\n", s, if (status[[s]] == 0) "pass" else paste("FAILED, status", status[[s]])))
  invisible(status)
}

if (!interactive()) {
  st <- run_diagnostics(with_ui = "--with-ui" %in% commandArgs(trailingOnly = TRUE))
  quit(status = if (any(st != 0)) 1L else 0L)
}
