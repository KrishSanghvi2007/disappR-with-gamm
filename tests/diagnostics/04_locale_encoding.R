# ---------------------------------------------------------------------------
# Diagnostic 4: locale, decimal separator and encoding
#
#   Rscript tests/diagnostics/04_locale_encoding.R
#
# Sorting, number parsing and label handling are locale-sensitive. Each locale runs
# in its own process; results must be identical across all of them.
# ---------------------------------------------------------------------------
# Safe to run from the R console as well as from Rscript: only a non-interactive session exits.
`%||%` <- function(a, b) if (is.null(a) || !length(a) || (length(a) == 1 && is.na(a))) b else a
finish <- function(fails, message_ok, message_bad) {
  cat("\n", if (fails) sprintf(message_bad, fails) else message_ok, sep = "")
  if (!interactive()) quit(status = if (fails) 1L else 0L)
  invisible(fails)
}

locales <- c("C", "en_GB.UTF-8", "de_DE.UTF-8", "tr_TR.UTF-8")   # Turkish catches dotless-i bugs
`%||%` <- function(a, b) if (is.null(a)) b else a
probe <- function(loc, outdec) {
  callr::r(function(loc, outdec) {
    `%||%` <- function(a, b) if (is.null(a)) b else a
    Sys.setlocale("LC_ALL", loc)
    options(OutDec = outdec)
    library(disappR)
    ex <- disappr_example("pasztor_apollo")
    x <- disappr_prepare(ex$data, ex$mapping)
    f <- disappr_fit(x, models = c("M1", "M2", "M4"), age_function = "Quadratic", family = "gaussian")
    if (!isTRUE(f$ok) || is.null(f$aic$AIC))
      stop("the fit itself failed here: ", paste(f$message %||% "no message", collapse = " "))
    list(choices = paste(names(disappR:::EXAMPLE_CHOICES), collapse = "|"),
         aic = round(as.numeric(f$aic$AIC), 6),
         models = paste(f$aic$Model, collapse = ","),
         n = nrow(x$data))
  }, args = list(loc = loc, outdec = outdec), show = FALSE)
}
ref <- NULL; fails <- 0L
for (loc in locales) {
  for (outdec in c(".", ",")) {
    r <- tryCatch(probe(loc, outdec), error = function(e) e)
    tag <- sprintf("%-12s OutDec '%s'", loc, outdec)
    if (inherits(r, "error")) { cat("FAIL", tag, conditionMessage(r), "\n"); fails <- fails + 1L; next }
    if (is.null(ref)) { ref <- r; cat("ref ", tag, "\n"); next }
    same <- identical(r$choices, ref$choices) && isTRUE(all.equal(r$aic, ref$aic)) && identical(r$models, ref$models)
    cat(if (same) "ok  " else "FAIL", tag, if (same) "" else "-> dropdown order or AIC differ", "\n")
    if (!same) fails <- fails + 1L
  }
}
finish(fails, "Identical in every locale tested.\n", "%d locale combination(s) differ.\n")
