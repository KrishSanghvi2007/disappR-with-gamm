# ---------------------------------------------------------------------------
# Diagnostic 1: does the package degrade gracefully when a suggested package is missing?
#
#   Rscript tests/diagnostics/01_suggests_matrix.R
#
# Each suggested package is hidden in turn by installing disappR into a temporary
# library that does not contain it, then running a short analysis in a fresh R
# process. A missing package must produce a clear message, never an error.
# ---------------------------------------------------------------------------
# Safe to run from the R console as well as from Rscript: only a non-interactive session exits.
`%||%` <- function(a, b) if (is.null(a) || !length(a) || (length(a) == 1 && is.na(a))) b else a
finish <- function(fails, message_ok, message_bad) {
  cat("\n", if (fails) sprintf(message_bad, fails) else message_ok, sep = "")
  if (!interactive()) quit(status = if (fails) 1L else 0L)
  invisible(fails)
}

suppressWarnings(suppressMessages(library(callr)))
pkg <- normalizePath(".")
suggests <- c("glmmTMB", "DHARMa", "performance", "see", "callr", "lmerTest", "nlme")
suggests <- suggests[vapply(suggests, requireNamespace, logical(1), quietly = TRUE)]

probe <- function(hidden) {
  callr::r(
    function(pkg, hidden) {
      # a library path that shadows the hidden package with an empty directory
      shadow <- file.path(tempdir(), paste0("shadow_", hidden))
      dir.create(shadow, showWarnings = FALSE, recursive = TRUE)
      # bquote inlines the name: the tracer runs inside loadNamespace(), where `hidden` does not exist
      trace(base::loadNamespace, tracer = bquote({
        if (identical(package, .(hidden))) stop("hidden for this run: ", .(hidden))
      }), print = FALSE)
      library(disappR)
      out <- list()
      for (key in c("fly", "warner", "mckennaell_breeding", "bichet_marmot")) {
        ex <- disappr_example(key)
        res <- tryCatch({
          x <- disappr_prepare(ex$data, ex$mapping)
          f <- disappr_fit(x, models = ex$settings$models, age_function = ex$settings$age_function,
                           family = ex$settings$family, extra = ex$settings$extra)
          list(ok = isTRUE(f$ok), n_fitted = sum(vapply(f$fits, Negate(is.null), logical(1))),
               note = paste(utils::head(unlist(f$fit_notes), 2), collapse = " | "))
        }, error = function(e) list(ok = FALSE, n_fitted = 0L, note = paste("ERROR:", conditionMessage(e))))
        out[[key]] <- res
      }
      out
    },
    args = list(pkg = pkg, hidden = hidden), show = FALSE, spinner = FALSE)
}

cat("Suggested packages tested:", paste(suggests, collapse = ", "), "\n\n")
fails <- 0L
for (h in suggests) {
  cat("---- without", h, "\n")
  r <- tryCatch(probe(h), error = function(e) {
    cat("   the whole session failed:", conditionMessage(e), "\n"); NULL
  })
  if (is.null(r)) { fails <- fails + 1L; next }
  for (k in names(r)) {
    v <- r[[k]]
    flag <- if (grepl("^ERROR", v$note)) "FAIL" else "ok  "
    if (flag == "FAIL") fails <- fails + 1L
    cat(sprintf("   %s %-22s fitted %d model(s) %s\n", flag, k, v$n_fitted, substr(v$note, 1, 90)))
  }
}
finish(fails, "All suggested packages can be absent without an error.\n", "%d failure(s): a missing suggested package should degrade, not error.\n")
