# ---------------------------------------------------------------------------
# Diagnostic 5: determinism, and behaviour on a large dataset
#
#   Rscript tests/diagnostics/05_determinism_scale.R
#
# Same input twice must give byte-identical results; a large dataset must finish in
# reasonable time and memory, with the fit timings reported.
# ---------------------------------------------------------------------------
# Safe to run from the R console as well as from Rscript: only a non-interactive session exits.
`%||%` <- function(a, b) if (is.null(a) || !length(a) || (length(a) == 1 && is.na(a))) b else a
finish <- function(fails, message_ok, message_bad) {
  cat("\n", if (fails) sprintf(message_bad, fails) else message_ok, sep = "")
  if (!interactive()) quit(status = if (fails) 1L else 0L)
  invisible(fails)
}

library(disappR)
stopifnot(requireNamespace("digest", quietly = TRUE))
fails <- 0L

run_once <- function(seed) {
  set.seed(seed)                                   # the session seed must not matter: the simulator takes its own
  sim <- disappr_simulate(n_id = 300, mean_ls = 20, sd_type = "intercept", sd_dir = 1,
                          strength = "dramatic", trait = "mass", form = "Quadratic", seed = 99)
  x <- disappr_prepare(sim, disappR:::toy_mapping(sim))
  f <- disappr_fit(x, models = c("M1", "M2", "M4", "M7"), age_function = "Quadratic", family = "gaussian")
  list(aic = f$aic$AIC, coefs = f$coefficients$Estimate, evidence = disappr_interpret(f))
}
a <- run_once(1); b <- run_once(2)
same <- identical(digest::digest(a), digest::digest(b))
cat(if (same) "ok   " else "FAIL ", "identical results from two runs of the same seeded input\n")
if (!same) {
  fails <- fails + 1L
  cat("     AIC differences:", paste(signif(a$aic - b$aic, 3), collapse = ", "), "\n")
}

cat("\nScale test\n")
for (n in c(1000L, 5000L, 10000L)) {
  set.seed(7)
  sim <- disappr_simulate(n_id = n, mean_ls = 15, sd_type = "slope", sd_dir = 1,
                          strength = "moderate", trait = "mass", form = "Quadratic", seed = 7)
  x <- disappr_prepare(sim, disappR:::toy_mapping(sim))
  t0 <- proc.time()[["elapsed"]]
  f <- tryCatch(disappr_fit(x, models = c("M1", "M2", "M4"), age_function = "Quadratic", family = "gaussian"),
                error = function(e) e)
  dt <- proc.time()[["elapsed"]] - t0
  if (inherits(f, "error")) {
    cat(sprintf("FAIL %6d individuals (%6d rows): %s\n", n, nrow(x$data), conditionMessage(f)))
    fails <- fails + 1L
  } else {
    cat(sprintf("ok   %6d individuals (%6d rows) in %5.1f s, peak %s\n",
                n, nrow(x$data), dt, format(utils::object.size(f), units = "MB")))
  }
}
finish(fails, "Deterministic, and scales without error.\n", "%d problem(s).\n")
