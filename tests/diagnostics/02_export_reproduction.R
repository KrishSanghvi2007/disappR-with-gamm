# ---------------------------------------------------------------------------
# Diagnostic 2: does the exported R script reproduce the app's own numbers?
#
#   Rscript tests/diagnostics/02_export_reproduction.R
#
# For each example: fit in this session, write the script the app would give the
# user, run it in a clean process, and compare the AIC table and the fixed-effect
# coefficients of every model.
# ---------------------------------------------------------------------------
# Safe to run from the R console as well as from Rscript: only a non-interactive session exits.
`%||%` <- function(a, b) if (is.null(a) || !length(a) || (length(a) == 1 && is.na(a))) b else a
finish <- function(fails, message_ok, message_bad) {
  cat("\n", if (fails) sprintf(message_bad, fails) else message_ok, sep = "")
  if (!interactive()) quit(status = if (fails) 1L else 0L)
  invisible(fails)
}

library(disappR)
keys <- c("fly", "warner", "bouwhuis", "sanghvi_female", "wynn", "bichet_marmot")
tol <- 1e-6
fails <- 0L

for (key in keys) {
  ex <- disappr_example(key)
  x <- disappr_prepare(ex$data, ex$mapping)
  fit <- disappr_fit(x, models = ex$settings$models, age_function = ex$settings$age_function,
                     family = ex$settings$family, extra = ex$settings$extra,
                     random_slope = disappR:::EXAMPLES[[key]]$random_structure %||% FALSE)
  # the script reads its data by file name, so give it a directory containing exactly that file
  dir <- file.path(tempdir(), paste0("export_", key))
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(ex$data, file.path(dir, paste0(key, ".csv")), row.names = FALSE)
  code <- disappr_code(fit, x, source_label = paste0(key, ".csv"))
  script <- file.path(dir, "analysis.R")
  writeLines(code, script)

  got <- tryCatch(callr::r(function(script) {
    setwd(dirname(script))
    e <- new.env()
    sys.source(script, envir = e)
    obj <- mget(ls(e), envir = e)
    fits <- Filter(function(z) inherits(z, c("lmerMod", "glmerMod", "glmmTMB", "lme")), obj)
    lapply(fits, function(f) list(aic = stats::AIC(f), beta = stats::coef(summary(f))))
  }, args = list(script = script), show = FALSE), error = function(e) e)

  if (inherits(got, "error")) {
    cat(sprintf("FAIL %-18s the exported script did not run: %s\n", key, conditionMessage(got)))
    fails <- fails + 1L
    next
  }
  here <- vapply(fit$fits[!vapply(fit$fits, is.null, logical(1))], stats::AIC, numeric(1))
  there <- sort(vapply(got, function(z) z$aic, numeric(1)))
  ok <- length(there) >= 1 && all(vapply(sort(here), function(a) any(abs(there - a) < tol), logical(1)))
  cat(sprintf("%s %-18s %d model(s) in the script, AIC agreement: %s\n",
              if (ok) "ok  " else "FAIL", key, length(there), if (ok) "exact" else "DIFFERENT"))
  if (!ok) {
    fails <- fails + 1L
    cat("     app:", paste(round(sort(here), 4), collapse = ", "), "\n")
    cat("     script:", paste(round(there, 4), collapse = ", "), "\n")
  }
}
finish(fails, "Every exported script reproduces the app.\n", "%d example(s) did not reproduce.\n")
