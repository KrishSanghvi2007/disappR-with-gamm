# GAMMs against GLMMs through the real engines (0.23.1): mirrors Part G of inst/validation/audit_0_22_0, where the
# same comparison runs on Python ports. Run from the package root:   Rscript tests/scripts/gamm_vs_glmm.R
# Set DISAPPR_GVG_REPS to change the replicates per scenario (default 10). Prints tables; fails only on errors.
app_dir <- file.path("inst", "app")
if (!dir.exists(app_dir)) stop("Run this script from the package root (the folder containing DESCRIPTION).")
old_wd <- setwd(app_dir)
on.exit(setwd(old_wd), add = TRUE)
source("global.R")
if (!requireNamespace("mgcv", quietly = TRUE)) { cat("mgcv not installed: nothing to compare.\n"); quit(status = 0) }
reps <- as.integer(Sys.getenv("DISAPPR_GVG_REPS", "10"))
errors <- character(0)
cmap <- list(id = "id", age = "age", trait = "trait", alr = "__AUTO_LAST__", life = "", entry = "__AUTO_FIRST__",
             covars = character(0), cov_factor = character(0), cov_int = character(0), group = "", nested = TRUE,
             random = character(0), censor = "", censor_value = "", condition = "", trials = "", start_mode = "afr",
             start_age = NA_real_, age_round = NA_real_, cov_age = character(0))
shapes <- list(quadratic = function(a) 0.6 * (a - 8) - 0.05 * (a - 8)^2,
               plateau = function(a) 3 * (1 - exp(-0.45 * (a - 1))),
               threshold = function(a) ifelse(a <= 9, 0.35 * a, 0.35 * 9 - 0.5 * (a - 9)),
               logarithmic = function(a) 2 * log(a))
simulate_one <- function(shape, sel, seed, n_id = 150) {
  set.seed(seed)
  ls <- pmin(16, pmax(3, round(stats::rnorm(n_id, 9, 3))))
  zz <- (ls - mean(ls)) / stats::sd(ls)
  lev <- (if (sel == "age-independent") 0.8 * zz else 0) + stats::rnorm(n_id, 0, 0.8)
  slo <- (if (sel == "age-dependent") 0.8 * zz else 0) + stats::rnorm(n_id, 0, 0.3)
  do.call(rbind, lapply(seq_len(n_id), function(i) {
    a <- seq_len(ls[i])
    data.frame(id = i, age = a, trait = shapes[[shape]](a) + lev[i] + slo[i] * (a - 8) / 8 + stats::rnorm(length(a)))
  }))
}
# the three approaches: Models 1, 2 and 4 with a quadratic, with the function chosen by AIC, and as a GAMM
compare_one <- function(b) {
  best_of <- function(aic) { a <- aic[is.finite(aic)]; if (!length(a)) NA_character_ else names(a)[which.min(a)] }
  out <- list()
  rq <- fit_model_suite(b$data, b$meta, models = c("M1", "M2", "M4"), age_function = "Quadratic")
  aq <- stats::setNames(rq$aic$AIC, sub("^Model ", "M", rq$aic$Model))
  out$GLMM_Q <- c(best = best_of(aq), agedep = isTRUE(aq[["M2"]] - aq[["M4"]] > 2))
  fc <- compare_population_functions(b$data, b$meta, model = "M2", functions = c("Linear", "Quadratic", "Cubic", "Logarithmic", "Asymptotic exponential"))
  fsel <- fc$table$Function[which.min(fc$table$AIC)]
  rf <- fit_model_suite(b$data, b$meta, models = c("M1", "M2", "M4"), age_function = fsel)
  af <- stats::setNames(rf$aic$AIC, sub("^Model ", "M", rf$aic$Model))
  out$GLMM_F <- c(best = best_of(af), agedep = isTRUE(af[["M2"]] - af[["M4"]] > 2), fun = fsel)
  gd <- gamm_data(b$data, b$meta)
  pc <- gamm_proxy_comparison(gd, gamm_defaults())
  ag <- stats::setNames(pc$table$AIC, c("M1", "M2", "M4"))
  out$GAMM <- c(best = best_of(ag), agedep = isTRUE(ag[["M2"]] - ag[["M4"]] > 2))
  out
}
cat("== Simulations (", reps, " replicates per scenario)\n", sep = "")
rows <- list()
for (sh in names(shapes)) for (sel in c("none", "age-independent", "age-dependent")) for (k in seq_len(reps)) {
  res <- tryCatch(compare_one(standardise_data(simulate_one(sh, sel, 1000 * k + nchar(sh) + nchar(sel)), cmap)),
                  error = function(e) { errors <<- c(errors, paste(sh, sel, k, conditionMessage(e))); NULL })
  if (is.null(res)) next
  for (ap in names(res)) rows[[length(rows) + 1]] <- data.frame(shape = sh, selection = sel, approach = ap,
                                                                best = res[[ap]][["best"]], agedep = res[[ap]][["agedep"]] == "TRUE")
}
R <- do.call(rbind, rows)
tab <- stats::aggregate(cbind(`age-dependent declared (%)` = 100 * agedep, `Model 4 best (%)` = 100 * (best == "M4"),
                              `Model 1 best (%)` = 100 * (best == "M1")) ~ shape + selection + approach, data = R, FUN = mean)
print(tab[order(tab$shape, tab$selection, tab$approach), ], row.names = FALSE)
cat("\n== Empirical examples (Gaussian traits)\n")
for (key in c("bichet", "moullec_swift", "pasztor_apollo", "wynn", "bouwhuis", "warner", "mckennaell_weight", "szejnersigal_activity")) {
  ex <- tryCatch(disappr_example(key), error = function(e) NULL)
  if (is.null(ex)) { errors <- c(errors, paste(key, "not loaded")); next }
  b <- tryCatch(standardise_data(ex$data, utils::modifyList(ex$mapping, list(covars = character(0), random = character(0)))), error = function(e) NULL)
  if (is.null(b)) { errors <- c(errors, paste(key, "not prepared")); next }
  res <- tryCatch(compare_one(b), error = function(e) { errors <<- c(errors, paste(key, conditionMessage(e))); NULL })
  if (is.null(res)) next
  cat(sprintf("  %-22s GLMM quadratic: %s%s | GLMM %s: %s%s | GAMM: %s%s\n", key,
              res$GLMM_Q[["best"]], if (res$GLMM_Q[["agedep"]] == "TRUE") " (age-dependent)" else "",
              res$GLMM_F[["fun"]], res$GLMM_F[["best"]], if (res$GLMM_F[["agedep"]] == "TRUE") " (age-dependent)" else "",
              res$GAMM[["best"]], if (res$GAMM[["agedep"]] == "TRUE") " (age-dependent)" else ""))
}
cat(if (length(errors)) paste0("\nERRORS (", length(errors), "):\n  ", paste(errors, collapse = "\n  "), "\n") else "\nNo errors.\n")
if (length(errors) && !interactive()) quit(status = 1)
