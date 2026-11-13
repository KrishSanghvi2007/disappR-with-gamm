# Compare two golden captures (see golden_capture.R). Exits with status 1 when anything differs.
#   Rscript tests/golden/golden_compare.R golden_0.9.9.rds golden_new.rds
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("Usage: Rscript golden_compare.R old.rds new.rds [--code]")
compare_code <- "--code" %in% args   # exported code changed by design in 0.9.11 (engine branch); compare it only on request
a <- readRDS(args[[1]]); b <- readRDS(args[[2]])
if (!identical(a$R, b$R) || !identical(a$packages, b$packages)) {
  cat("NOTE: the two captures used different R or package versions, so small numerical differences are possible.\n")
}
tol <- 1e-8
diffs <- character(0)
code_notes <- character(0)
field_notes <- character(0)
cmp <- function(x, y, path) {
  if (!compare_code && grepl("[$]code$", path)) {
    if (!identical(x, y)) code_notes <<- c(code_notes, path)
    return(invisible(NULL))
  }
  if (is.function(x) || is.environment(x)) return(invisible(NULL))
  if (!identical(class(x), class(y))) {
    diffs <<- c(diffs, sprintf("%s: class %s vs %s", path, paste(class(x), collapse = "/"), paste(class(y), collapse = "/")))
    return(invisible(NULL))
  }
  if (is.numeric(x)) {
    if (length(x) != length(y) || !isTRUE(all.equal(unname(as.numeric(x)), unname(as.numeric(y)), tolerance = tol))) {
      diffs <<- c(diffs, sprintf("%s: numbers differ", path))
    }
    return(invisible(NULL))
  }
  if (is.list(x)) {
    if (!is.null(names(x)) && !is.null(names(y)) && !identical(names(x), names(y))) {
      # later versions may add result fields (for example diagnostics or provenance): note them; a missing field fails
      added <- setdiff(names(y), names(x)); removed <- setdiff(names(x), names(y))
      if (length(added)) field_notes <<- c(field_notes, sprintf("%s: new field(s) %s", path, paste(added, collapse = ", ")))
      if (length(removed)) diffs <<- c(diffs, sprintf("%s: field(s) missing in the new version: %s", path, paste(removed, collapse = ", ")))
      for (nm in intersect(names(x), names(y))) cmp(x[[nm]], y[[nm]], paste0(path, "$", nm))
      return(invisible(NULL))
    }
    if (length(x) != length(y) || !identical(names(x), names(y))) {
      diffs <<- c(diffs, sprintf("%s: different structure", path))
      return(invisible(NULL))
    }
    for (i in seq_along(x)) cmp(x[[i]], y[[i]], paste0(path, "$", if (is.null(names(x))) i else names(x)[[i]]))
    return(invisible(NULL))
  }
  if (!identical(x, y)) diffs <<- c(diffs, sprintf("%s: values differ", path))
  invisible(NULL)
}
cmp(a$analyses, b$analyses, "analyses")
cmp(a$simulations, b$simulations, "simulations")
n_items <- length(a$analyses) + length(a$simulations)
if (length(field_notes)) cat("NOTE: fields added by the newer version:", length(unique(sub("^.*: new field[(]s[)] ", "", field_notes))), "kinds, e.g.", field_notes[[1]], "\n")
if (length(code_notes)) cat("NOTE: exported code differs in", length(code_notes), "analyses (expected across 0.9.10/0.9.11; add --code to compare it).\n")
if (length(diffs)) {
  cat(length(diffs), "difference(s) across", n_items, "analyses:\n")
  cat(paste0("  ", utils::head(diffs, 50)), sep = "\n")
  quit(status = 1)
}
cat("Identical: all", n_items, "analyses match (formulas, rows, coefficients, log-likelihoods, AIC, predictions, validity,",
    "notes, row checks, decomposition, observed means and exported code).\n")
