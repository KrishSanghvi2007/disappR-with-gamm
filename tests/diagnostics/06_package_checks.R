# ---------------------------------------------------------------------------
# Diagnostic 6: package-level checks that need no fitting
#
#   Rscript tests/diagnostics/06_package_checks.R
#
# Everything here is read-only and fast. Install the packages it needs first:
#   install.packages(c("codetools", "lintr", "spelling", "urlchecker", "devtools"))
# ---------------------------------------------------------------------------
# Safe to run from the R console as well as from Rscript: only a non-interactive session exits.
`%||%` <- function(a, b) if (is.null(a) || !length(a) || (length(a) == 1 && is.na(a))) b else a
finish <- function(fails, message_ok, message_bad) {
  cat("\n", if (fails) sprintf(message_bad, fails) else message_ok, sep = "")
  if (!interactive()) quit(status = if (fails) 1L else 0L)
  invisible(fails)
}

fails <- 0L
note <- function(ok, label, detail = "") {
  cat(if (ok) "ok   " else "FAIL ", label, if (nzchar(detail)) paste0(" - ", detail) else "", "\n", sep = "")
  if (!ok) fails <<- fails + 1L
}

cat("== unbound variables (the class of bug that hid in standardise_data)\n")
if (requireNamespace("codetools", quietly = TRUE)) {
  library(disappR)
  msgs <- character(0)
  codetools::checkUsagePackage("disappR", all = TRUE, report = function(x) msgs <<- c(msgs, x))
  real <- grep("no visible binding|no visible global function|could not find function|may not be defined", msgs, value = TRUE)
  declared <- utils::globalVariables(package = "disappR")
  if (length(declared)) real <- real[!grepl(paste0("\u2018(", paste(declared, collapse = "|"), ")\u2019|'(",
                                                   paste(declared, collapse = "|"), ")'"), real)]
  for (m in utils::head(real, 20)) cat("   ", m, "\n")
  note(!length(real), sprintf("codetools: %d message(s) of interest, %d total", length(real), length(msgs)))
} else note(FALSE, "codetools not installed")

cat("\n== lint\n")
if (requireNamespace("lintr", quietly = TRUE)) {
  l <- lintr::lint_package(linters = lintr::linters_with_defaults(
    line_length_linter = NULL, object_name_linter = NULL, commented_code_linter = NULL,
    cyclocomp_linter = NULL, object_length_linter = NULL, indentation_linter = NULL))
  print(utils::head(l, 20))
  note(TRUE, sprintf("lintr: %d lint(s) - style, review but not a failure", length(l)))
} else note(FALSE, "lintr not installed")

cat("\n== spelling\n")
if (requireNamespace("spelling", quietly = TRUE)) {
  sp <- spelling::spell_check_package(".")
  if (nrow(sp)) print(utils::head(sp, 25))
  note(TRUE, sprintf("spelling: %d unrecognised word(s) - review, most will be jargon", nrow(sp)))
} else note(FALSE, "spelling not installed")

cat("\n== URLs\n")
if (requireNamespace("urlchecker", quietly = TRUE)) {
  u <- try(urlchecker::url_check("."), silent = TRUE)
  note(!inherits(u, "try-error") && nrow(u) == 0, "urlchecker")
  if (!inherits(u, "try-error") && nrow(u)) print(u)
} else note(FALSE, "urlchecker not installed")

cat("\n== bundled data files load, and match the columns each example maps\n")
library(disappR)
for (key in names(disappR:::EXAMPLES)) {
  ex <- try(disappr_example(key), silent = TRUE)
  if (inherits(ex, "try-error")) { note(FALSE, key, "file did not load"); next }
  need <- unlist(ex$mapping[c("id", "age", "trait", "alr", "life", "entry", "group", "covars", "cov_factor", "random")])
  need <- need[nzchar(need) & !grepl("^__", need)]
  miss <- setdiff(need, names(ex$data))
  note(!length(miss), sprintf("%-22s %5d rows, %3d columns", key, nrow(ex$data), ncol(ex$data)),
       if (length(miss)) paste("missing:", paste(miss, collapse = ", ")) else "")
}

cat("\n== file checksums (record these beside the data provenance)\n")
f <- list.files(file.path(system.file("app", package = "disappR"), "data"), pattern = "[.]csv$", full.names = TRUE)
if (!length(f)) f <- list.files("inst/app/data", pattern = "[.]csv$", full.names = TRUE)
print(tools::md5sum(f))

cat("\n== R CMD check\n")
cat("Run separately, it takes a few minutes:\n")
cat("   devtools::check(args = '--as-cran', vignettes = TRUE)\n")
if (!interactive()) quit(status = if (fails) 1L else 0L)
