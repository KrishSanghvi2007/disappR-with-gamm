# Trait-specific against dataset-wide ALR/AFR on every bundled example, through the real engine (0.24.2): the R
# counterpart of audit Part J (inst/validation/audit_0_22_0/j_snapshot_before_after.md). Run from the package root:
#   Rscript tests/scripts/trait_ages_snapshot.R
# For each example: how many individuals change AFR or ALR when the box is ticked, the share of expected occasions
# missed under each option, and the documented models' AIC table under each option. Fails only on errors.
app_dir <- file.path("inst", "app")
if (!dir.exists(app_dir)) stop("Run this script from the package root (the folder containing DESCRIPTION).")
old_wd <- setwd(app_dir)
source("global.R")
setwd(old_wd)
errors <- character(0)
first_of <- function(d, v) tapply(d[[v]], d$id, function(x) x[[1]])
for (key in names(EXAMPLES)) {
  ex <- tryCatch(disappr_example(key), error = function(e) NULL)
  if (is.null(ex)) { errors <- c(errors, paste(key, "not loaded")); next }
  out <- list()
  for (sp in c(FALSE, TRUE)) {
    mp <- utils::modifyList(ex$mapping, list(trait_specific_ages = sp))
    b <- tryCatch(standardise_data(ex$data, mp), error = function(e) { errors <<- c(errors, paste(key, sp, conditionMessage(e))); NULL })
    if (is.null(b)) next
    # the app counts missed occasions from AFR unless an AFE is entered on the Sampling tab
    g <- tryCatch(build_missing_grid(b$data, 0, "afr", NA_real_),
                  error = function(e) { errors <<- c(errors, paste(key, sp, "grid:", conditionMessage(e))); NULL })
    spec <- EXAMPLES[[key]]
    r <- tryCatch(fit_model_suite(b$data, b$meta, models = spec$models %||% MODEL_IDS, age_function = spec$age_function %||% "Quadratic",
                                  family = spec$family %||% "gaussian"),
                  error = function(e) { errors <<- c(errors, paste(key, sp, "fit:", conditionMessage(e))); NULL })
    out[[if (sp) "ticked" else "unticked"]] <- list(b = b, miss = if (is.null(g)) NA_real_ else 100 * mean(g$missing), fit = r)
  }
  if (length(out) < 2) next
  u <- out$unticked$b$data; t <- out$ticked$b$data
  ids <- intersect(names(first_of(t, "alr")), names(first_of(u, "alr")))
  n_alr <- sum(first_of(t, "alr")[ids] != first_of(u, "alr")[ids], na.rm = TRUE)
  n_afr <- sum(first_of(t, "entry")[ids] != first_of(u, "entry")[ids], na.rm = TRUE)
  best <- function(r) if (is.null(r)) "-" else { a <- r$aic; a <- a[is.finite(a$AIC), , drop = FALSE]; if (!nrow(a)) "-" else a$Model[which.min(a$AIC)] }
  cat(sprintf("%-24s ALR changes %4d, AFR changes %4d | missing %% unticked %5.1f, ticked %5.1f | best model unticked %s, ticked %s\n",
              key, n_alr, n_afr, out$unticked$miss, out$ticked$miss, best(out$unticked$fit), best(out$ticked$fit)))
  if (n_alr + n_afr > 0 && !is.null(out$unticked$fit) && !is.null(out$ticked$fit)) {
    a <- merge(out$unticked$fit$aic[, c("Model", "AIC")], out$ticked$fit$aic[, c("Model", "AIC")], by = "Model", suffixes = c("_unticked", "_ticked"))
    a$dAIC_unticked <- round(a$AIC_unticked - min(a$AIC_unticked, na.rm = TRUE), 1)
    a$dAIC_ticked <- round(a$AIC_ticked - min(a$AIC_ticked, na.rm = TRUE), 1)
    print(a[, c("Model", "dAIC_unticked", "dAIC_ticked")], row.names = FALSE)
  }
}
cat(if (length(errors)) paste0("\nERRORS (", length(errors), "):\n  ", paste(errors, collapse = "\n  "), "\n") else "\nNo errors.\n")
if (length(errors) && !interactive()) quit(status = 1)
