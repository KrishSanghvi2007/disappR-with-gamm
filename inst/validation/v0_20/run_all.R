# Runs the three 0.20.0 validations and writes their results to inst/validation/results/v0_20_*.csv.
# Runtime: tens of minutes with the default 3 seeds (10 for the bootstrap); set DISAPPR_VALIDATION_SEEDS to change.
for (f in c("step4_reconstruction.R", "grader_calibration.R", "bootstrap_calibration.R")) {
  message("== ", f)
  source(file.path("inst", "validation", "v0_20", f), echo = FALSE)
}
