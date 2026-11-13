# GAMMs against GLMMs through the real engines (step 7): mgcv for the GAMM, lme4 for the GLMMs, on the same simulated
# datasets. (The 0.23.1 audit ran this comparison on a Python port of a GAMM; this script runs the real engine.) Run from the package root:   Rscript tests/scripts/gamm_vs_glmm.R
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
# (no ALR, linear ALR, age-dependent ALR via ti; ML, same records). "Age-dependent declared": the age-dependent model is
# more than 2 AIC better than the additive one. Shape error: RMSE between the fitted within-individual curve (ALR at its
# mean) and the true trajectory, after removing each curve's mean (so only the shape counts).
true_shape <- function(sh, ages) shapes[[sh]](ages)
shape_err <- function(fit_curve, sh) { tc <- true_shape(sh, fit_curve$age); sqrt(mean(((fit_curve$fit - mean(fit_curve$fit)) - (tc - mean(tc)))^2)) }
compare_one <- function(b, sh = NULL) {
  best_of <- function(aic) { a <- aic[is.finite(aic)]; if (!length(a)) NA_character_ else names(a)[which.min(a)] }
  out <- list()
  ages <- seq(min(b$data$age), max(b$data$age), length.out = 60)
  curve_err <- function(res, m) {
    if (is.null(sh)) return(NA_real_)
    pc <- tryCatch(predict_population_curve(res$fits[[m]], res, ages), error = function(e) NULL)
    if (!is.data.frame(pc)) return(NA_real_)
    shape_err(data.frame(age = pc$age, fit = pc$fitted), sh)
  }
  rq <- fit_model_suite(b$data, b$meta, models = c("M1", "M2", "M4"), age_function = "Quadratic")
  aq <- stats::setNames(rq$aic$AIC, sub("^Model ", "M", rq$aic$Model))
  out$GLMM_Q <- c(best = best_of(aq), agedep = isTRUE(aq[["M2"]] - aq[["M4"]] > 2), err = curve_err(rq, "M2"))
  fc <- compare_population_functions(b$data, b$meta, model = "M2", functions = c("Linear", "Quadratic", "Cubic", "Logarithmic", "Asymptotic exponential"))
  fsel <- fc$table$Function[which.min(fc$table$AIC)]
  rf <- fit_model_suite(b$data, b$meta, models = c("M1", "M2", "M4"), age_function = fsel)
  af <- stats::setNames(rf$aic$AIC, sub("^Model ", "M", rf$aic$Model))
  out$GLMM_F <- c(best = best_of(af), agedep = isTRUE(af[["M2"]] - af[["M4"]] > 2), err = curve_err(rf, "M2"), fun = fsel)
  gd <- gamm_data(b$data, b$meta)
  s <- utils::modifyList(gamm_defaults(), list(covars = FALSE, random = FALSE))
  pc <- gamm_proxy_comparison(gd, s)
  tab <- pc$tables$ALR
  ag <- stats::setNames(tab$AIC, c("M1", "M2", "M2s", "M4"))[c("M1", "M2", "M4")]
  rg <- gamm_fit(gd, utils::modifyList(s, list(engine = if (isTRUE(pc$fast)) "gamm4" else "auto")))
  out$GAMM <- c(best = best_of(ag), agedep = isTRUE(ag[["M2"]] - ag[["M4"]] > 2),
                err = if (isTRUE(rg$ok) && !is.null(sh)) shape_err(gamm_curve(rg, n = 60, by_proxy = FALSE), sh) else NA_real_)
  out
}
if (reps > 0) {
cat("== Simulations (", reps, " replicates per scenario)\n", sep = "")
grid <- expand.grid(k = seq_len(reps), sel = c("none", "age-independent", "age-dependent"), sh = names(shapes), stringsAsFactors = FALSE)
one_row <- function(i) {
  sh <- grid$sh[[i]]; sel <- grid$sel[[i]]; k <- grid$k[[i]]
  res <- tryCatch(compare_one(standardise_data(simulate_one(sh, sel, 1000 * k + nchar(sh) + nchar(sel)), cmap), sh),
                  error = function(e) paste(sh, sel, k, conditionMessage(e)))
  if (is.character(res)) return(res)
  do.call(rbind, lapply(names(res), function(ap) data.frame(shape = sh, selection = sel, approach = ap, best = res[[ap]][["best"]],
                                                            agedep = res[[ap]][["agedep"]] == "TRUE", err = as.numeric(res[[ap]][["err"]]))))
}
cores <- max(1L, as.integer(Sys.getenv("DISAPPR_GVG_CORES", "1")))
t0 <- Sys.time()
out <- if (cores > 1 && .Platform$OS.type == "unix") parallel::mclapply(seq_len(nrow(grid)), one_row, mc.cores = cores) else lapply(seq_len(nrow(grid)), one_row)
errors <- c(errors, unlist(Filter(is.character, out)))
R <- do.call(rbind, Filter(is.data.frame, out))
cat(sprintf("%d datasets in %.0f min\n", nrow(grid), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
tab <- stats::aggregate(cbind(`age-dependent declared (%)` = 100 * agedep, `Model 4 best (%)` = 100 * (best == "M4"),
                              `Model 2 best (%)` = 100 * (best == "M2"), `Model 1 best (%)` = 100 * (best == "M1"), `shape error` = err) ~ shape + selection + approach,
                        data = R, FUN = function(x) round(mean(x, na.rm = TRUE), 3), na.action = na.pass)
print(tab[order(tab$shape, tab$selection, tab$approach), ], row.names = FALSE)
cat("\n== Overall, by approach\n")
ov <- stats::aggregate(cbind(`false age-dependent (%)` = 100 * agedep) ~ approach, data = R[R$selection != "age-dependent", ], FUN = mean)
ov$`power, age-dependent (%)` <- stats::aggregate(100 * agedep ~ approach, data = R[R$selection == "age-dependent", ], FUN = mean)[[2]]
ov$`mean shape error` <- stats::aggregate(err ~ approach, data = R, FUN = mean, na.action = na.omit)[[2]]
print(ov, row.names = FALSE)
utils::write.csv(R, file.path(Sys.getenv("DISAPPR_GVG_OUT", tempdir()), "gamm_vs_glmm_replicates.csv"), row.names = FALSE)
}
cat("\n== Empirical examples (Gaussian traits)\n")
for (key in c("bichet", "moullec_swift", "pasztor_apollo", "wynn", "bouwhuis", "warner", "mckennaell_weight", "szejnersigal_activity")) {
  ex <- EXAMPLES[[key]]
  d <- if (is.null(ex)) NULL else load_example_file(ex$file)
  if (is.null(d)) { errors <- c(errors, paste(key, "not loaded")); next }
  b <- tryCatch(standardise_data(d, utils::modifyList(ex$mapping, list(covars = character(0), cov_factor = character(0), cov_int = character(0), random = character(0)))), error = function(e) NULL)
  if (is.null(b)) { errors <- c(errors, paste(key, "not prepared")); next }
  res <- tryCatch(compare_one(b, NULL), error = function(e) { errors <<- c(errors, paste(key, conditionMessage(e))); NULL })
  if (is.null(res)) next
  cat(sprintf("  %-22s GLMM quadratic: %s%s | GLMM %s: %s%s | GAMM: %s%s\n", key,
              res$GLMM_Q[["best"]], if (res$GLMM_Q[["agedep"]] == "TRUE") " (age-dependent)" else "",
              res$GLMM_F[["fun"]], res$GLMM_F[["best"]], if (res$GLMM_F[["agedep"]] == "TRUE") " (age-dependent)" else "",
              res$GAMM[["best"]], if (res$GAMM[["agedep"]] == "TRUE") " (age-dependent)" else ""))
}
cat(if (length(errors)) paste0("\nERRORS (", length(errors), "):\n  ", paste(errors, collapse = "\n  "), "\n") else "\nNo errors.\n")
if (length(errors) && !interactive()) quit(status = 1)
