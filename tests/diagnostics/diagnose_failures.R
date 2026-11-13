# Run from the package source directory after installing the package:
#   source("tests/diagnostics/diagnose_failures.R")
# Prints the real reason each failing combination cannot predict, and the state of the
# individual-function table. Send me the output.
library(disappR)
library(testthat)
tenv <- new.env(parent = asNamespace("disappR"))       # like testthat: internals visible, nothing exported needed
local({
  f <- if (file.exists("tests/testthat/test-combinations.R")) "tests/testthat/test-combinations.R" else
    stop("Run this from the package source directory.", call. = FALSE)
  for (e in parse(f)) {
    # top-level assignments only (the data builders and the combination lists); the test_that() blocks are skipped
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=")) eval(e, envir = tenv)
  }
})
combo_data_extra <- get("combo_data_extra", envir = tenv)

cat("==== 1. why prediction fails in the failing combinations ====\n")
combos <- list(
  list(age_function = "Cubic", among = "same", random_slope = "uncorrelated", standardise = TRUE, family = "poisson", builder = "combo_data"),
  list(age_function = "Cubic", among = "consistent", random_slope = "uncorrelated", standardise = FALSE, family = "nbinom1"),
  list(age_function = "Logarithmic", among = "same", random_slope = "uncorrelated", standardise = FALSE, family = "binomial"),
  list(age_function = "Cubic", among = "linear", random_slope = "none", standardise = TRUE, family = "betabinomial"),
  list(age_function = "Asymptotic exponential", among = "same", random_slope = "correlated", standardise = FALSE, family = "betabinomial"),
  list(age_function = "Exponential (a \u00b7 exp(b \u00b7 age))", among = "linear", random_slope = "none", standardise = TRUE, family = "gaussian")
)
for (cc in combos) {
  b <- if (identical(cc$builder, "combo_data")) get("combo_data", envir = tenv)(cc$family) else combo_data_extra(cc$family)
  r <- try(disappR:::fit_model_suite(b$data, b$meta, models = disappR:::MODEL_IDS,
                                     age_function = cc$age_function, family = cc$family,
                                     random_slope = cc$random_slope, standardise = cc$standardise,
                                     among = cc$among), silent = TRUE)
  cat("\n--", cc$age_function, "/", cc$family, "\n")
  if (inherits(r, "try-error")) { cat("   fit failed:", r, "\n"); next }
  fitted <- Filter(Negate(is.null), r$fits)
  show <- intersect(c("M1", "M2", "M8", "M9", "M10"), names(fitted))
  for (m in show) {
    pc <- disappR:::predict_population_curve(fitted[[m]], r, c(2, 4, 6))
    if (nrow(pc)) {
      cat(sprintf("   %-4s ok, fitted: %s\n", m, paste(signif(pc$fitted, 4), collapse = ", ")))
    } else {
      cat(sprintf("   %-4s FAILED: %s\n", m, attr(pc, "predict_error")))
      cat("        newdata columns:", paste(attr(pc, "predict_newdata_names"), collapse = ", "), "\n")
      cat("        model terms    :", paste(all.vars(stats::formula(fitted[[m]]))[1:12], collapse = ", "), "\n")
    }
  }
}

cat("\n==== 2. the individual-function ranking ====\n")
set.seed(1)
d <- do.call(rbind, lapply(seq_len(30), function(i) data.frame(id = paste0("i", i), age = 1:6,
      trait = 10 - 0.3 * (1:6) + 0.02 * (1:6)^2 + stats::rnorm(6, 0, 0.5))))
fits <- stats::setNames(lapply(disappR:::A3_FUNCTIONS, function(fn) disappR:::fit_individual_function(d, fn)),
                        disappR:::A3_FUNCTIONS)
fits[["Quadratic"]]$fit_stats$AICc <- fits[["Quadratic"]]$fit_stats$AICc - 1000
tab <- disappR:::compare_individual_functions(d, fits = fits)
print(tab[, intersect(c("Function", "N_fitted", "N_common", "Mean_dAICc", "Mean_adj_R2"), names(tab))])
cat("\nN_common is the number of individuals fitted by every function; if it is 0 the ranking falls back to R-squared.\n")
