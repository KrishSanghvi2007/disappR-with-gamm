# Shared set-up for the 0.20.0 validation scripts. Run from the package root, e.g.
#   Rscript inst/validation/v0_20/run_all.R
# Every script uses the app's own functions on simulated data whose answer is known.
if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION")) pkgload::load_all(".", quiet = TRUE) else library(disappR)
N_SEEDS <- as.integer(Sys.getenv("DISAPPR_VALIDATION_SEEDS", "3"))
OUT_DIR <- file.path("inst", "validation", "results")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
simulate_prepared <- function(...) {
  cfg <- utils::modifyList(TOY_DEFAULTS, list(...))
  raw <- simulate_toy_data(cfg)
  b <- standardise_data(raw, toy_mapping(raw))
  b$truth <- attr(raw, "truth")
  b
}
write_result <- function(x, name) {
  f <- file.path(OUT_DIR, paste0("v0_20_", name, ".csv"))
  utils::write.csv(x, f, row.names = FALSE)
  message("written: ", f)
  invisible(f)
}
