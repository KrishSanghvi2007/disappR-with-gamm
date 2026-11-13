# ---------------------------------------------------------------------------
# Diagnostic 3: can any mutation of a good file crash the importer?
#
#   Rscript tests/diagnostics/03_import_fuzz.R [n_mutations]
#
# Each bundled example and stress file is mutated (columns permuted, cells blanked,
# rows truncated, headers duplicated, types swapped, NA floods) and pushed through
# standardise_data(). A mutation may be rejected, but only with a classed condition:
# an uncaught error is a failure.
# ---------------------------------------------------------------------------
# Safe to run from the R console as well as from Rscript: only a non-interactive session exits.
`%||%` <- function(a, b) if (is.null(a) || !length(a) || (length(a) == 1 && is.na(a))) b else a
finish <- function(fails, message_ok, message_bad) {
  cat("\n", if (fails) sprintf(message_bad, fails) else message_ok, sep = "")
  if (!interactive()) quit(status = if (fails) 1L else 0L)
  invisible(fails)
}

library(disappR)
set.seed(1)
n_mut <- as.integer(commandArgs(trailingOnly = TRUE)[1] %||% 40L)
if (is.na(n_mut)) n_mut <- 40L

mutate_df <- function(d, k) {
  switch(as.integer(k %% 8L) + 1L,
    d[, sample(ncol(d)), drop = FALSE],                                   # permute columns
    { i <- sample(nrow(d), max(1, nrow(d) %/% 10)); j <- sample(ncol(d), 1); d[i, j] <- NA; d },
    d[seq_len(max(1L, nrow(d) %/% 50L)), , drop = FALSE],                 # truncate
    { d2 <- d; names(d2)[sample(ncol(d), 1)] <- names(d2)[1]; d2 },       # duplicate header
    { d2 <- d; j <- sample(ncol(d), 1); d2[[j]] <- as.character(d2[[j]]); d2 },
    { d2 <- d; j <- sample(ncol(d), 1); d2[[j]][] <- NA; d2 },            # all-NA column
    d[sample(nrow(d), nrow(d), replace = TRUE), , drop = FALSE],          # resample rows
    { d2 <- d; j <- sample(ncol(d), 1); d2[[j]] <- suppressWarnings(as.numeric(d2[[j]]) * Inf); d2 })
}

crashes <- 0L; tried <- 0L; rejected <- 0L
for (key in names(disappR:::EXAMPLES)) {
  ex <- disappr_example(key)
  for (k in seq_len(n_mut)) {
    d <- try(mutate_df(ex$data, k), silent = TRUE)
    if (inherits(d, "try-error")) next
    tried <- tried + 1L
    out <- tryCatch(disappr_prepare(d, ex$mapping),
                    disappr_integrity_error = function(e) "rejected",
                    disappr_input_error = function(e) "rejected",
                    error = function(e) structure(conditionMessage(e), class = "uncaught"),
                    warning = function(w) "warned")
    if (inherits(out, "uncaught")) {
      crashes <- crashes + 1L
      cat(sprintf("CRASH %-22s mutation %2d: %s\n", key, k, substr(out, 1, 110)))
    } else if (identical(out, "rejected")) rejected <- rejected + 1L
  }
}
cat(sprintf("\n%d mutations tried, %d rejected cleanly, %d uncaught errors.\n", tried, rejected, crashes))
if (!interactive()) quit(status = if (crashes) 1L else 0L)
