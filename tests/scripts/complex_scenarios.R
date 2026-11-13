# disappR complex-scenario tier (0.22.7): data structures and combinations beyond the other tiers, run through the
# real engine. Mirrors Part E of inst/validation/audit_0_22_0 (where the same scenarios run on Python ports).
# Run from the package root:   Rscript tests/scripts/complex_scenarios.R
# [FAIL] marks a broken expectation; [NOTE] marks a statistical expectation that can miss by chance on one dataset.
app_dir <- file.path("inst", "app")
if (!dir.exists(app_dir)) stop("Run this script from the package root (the folder containing DESCRIPTION).")
old_wd <- setwd(app_dir)
on.exit(setwd(old_wd), add = TRUE)
source("global.R")
set.seed(2207)

hard_fail <- character(0)
report <- function(label, cond, hard = TRUE) {
  ok <- isTRUE(cond)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else if (hard) "FAIL" else "NOTE", label))
  if (!ok && hard) hard_fail <<- c(hard_fail, label)
  invisible(ok)
}
run <- function(label, expr) tryCatch(expr, error = function(e) {
  hard_fail <<- c(hard_fail, paste(label, "(error)"))
  cat(sprintf("[FAIL] %s: error: %s\n", label, conditionMessage(e)))
  NULL
})
cmap <- function(...) {
  m <- list(id = "id", age = "age", trait = "trait", alr = "__AUTO_LAST__", life = "", entry = "__AUTO_FIRST__",
            covars = character(0), cov_factor = character(0), cov_int = character(0), group = "", nested = TRUE,
            random = character(0), censor = "", censor_value = "", condition = "", trials = "", start_mode = "afr",
            start_age = NA_real_, age_round = NA_real_, cov_age = character(0))
  utils::modifyList(m, list(...))
}
term_p <- function(r, m, parts) {
  cf <- r$coefficients[r$coefficients$Model == model_label(m), , drop = FALSE]
  hit <- vapply(strsplit(cf$Raw_term, ":", fixed = TRUE), function(p) setequal(p, parts), logical(1))
  if (any(hit)) cf$P_value[hit][[1]] else NA_real_
}

cat("== E1 monthly ages, interval typed as 0.0833\n")
d <- do.call(rbind, lapply(1:200, function(i) {
  n <- sample(12:120, 1); a <- round((1:n) / 12, 10); keep <- stats::runif(n) > 0.1
  data.frame(id = sprintf("m%03d", i), age = a[keep], trait = 5 + 0.2 * a[keep] + stats::rnorm(sum(keep)))
}))
b <- run("E1 standardise", standardise_data(d, cmap()))
if (!is.null(b)) {
  de <- run("E1 decomposition", decomposition_trajectory(b$data, step = 0.0833))
  ga <- run("E1 grid auto", build_missing_grid(b$data)); gs <- run("E1 grid typed", build_missing_grid(b$data, step = 0.0833))
  report("E1 no record left out with a rounded monthly interval", !grepl("left out", attr(de, "caution") %||% ""))
  report("E1 missingness the same with the rounded interval", abs(mean(ga$missing) - mean(gs$missing)) < 0.01)
}

cat("== E2 staggered biennial cohorts\n")
d <- do.call(rbind, lapply(1:300, function(i) {
  a <- (1 + i %% 2) + 2 * (0:(sample(3:7, 1) - 1)); data.frame(id = sprintf("s%03d", i), age = a, trait = 10 - 0.3 * a + stats::rnorm(length(a)))
}))
b <- run("E2 standardise", standardise_data(d, cmap()))
if (!is.null(b)) {
  report("E2 interval inferred as 2", isTRUE(infer_age_step(b$data$age, b$data$id) == 2))
  report("E2 no missingness", mean(run("E2 grid", build_missing_grid(b$data))$missing) < 0.01)
  report("E2 decomposition empty (no individual at consecutive occasions; the app says why)", nrow(decomposition_trajectory(b$data)) == 0)
  r <- run("E2 models", fit_model_suite(b$data, b$meta, models = c("M1", "M2"), age_function = "Linear"))
  report("E2 models fit", isTRUE(r$ok))
}

cat("== E3 annual to age 5, then biennial\n")
d <- do.call(rbind, lapply(1:300, function(i) {
  L <- sample(3:15, 1); a <- Filter(function(x) x <= 5 || (x - 5) %% 2 == 0, 1:L)
  data.frame(id = sprintf("f%03d", i), age = a, trait = 10 - 0.2 * a + stats::rnorm(length(a)))
}))
b <- run("E3 standardise", standardise_data(d, cmap()))
if (!is.null(b)) report("E3 planned biennial years count as missed against the annual interval", mean(build_missing_grid(b$data)$missing) > 0.1)

cat("== E4 study window with a calendar-year trend, no selective disappearance\n")
d <- do.call(rbind, lapply(1:400, function(i) {
  birth <- sample(0:39, 1); ls <- max(2, min(18, round(stats::rnorm(1, 8, 3)))); ages <- 1:ls; yrs <- birth + ages
  w <- yrs >= 15 & yrs <= 35
  if (!any(w)) return(NULL)
  a <- ages[w]; y <- yrs[w]
  data.frame(id = i, age = a, year = y, trait = 10 + 0.6 * a - 0.04 * a^2 + stats::rnorm(1) - 0.08 * (y - 25) + stats::rnorm(length(a)))
}))
b0 <- run("E4 standardise", standardise_data(d, cmap()))
b1 <- run("E4 standardise with year", standardise_data(d, cmap(random = "year")))
if (!is.null(b0) && !is.null(b1)) {
  r0 <- run("E4 fit", fit_model_suite(b0$data, b0$meta, models = c("M1", "M2"), age_function = "Quadratic"))
  r1 <- run("E4 fit with year", fit_model_suite(b1$data, b1$meta, models = c("M1", "M2"), age_function = "Quadratic"))
  g0 <- r0$aic$AIC[r0$aic$Model == "Model 1"] - r0$aic$AIC[r0$aic$Model == "Model 2"]
  g1 <- r1$aic$AIC[r1$aic$Model == "Model 1"] - r1$aic$AIC[r1$aic$Model == "Model 2"]
  cat(sprintf("   Model 2 advantage: %.1f AIC without year, %.1f with a year random intercept\n", g0, g1))
  report("E4 a calendar trend under a study window mimics selective disappearance", g0 > 2, hard = FALSE)
  report("E4 a year random intercept removes it", g1 < 2, hard = FALSE)
}

cat("== E5 AFR correlated with lifespan, selection on age at death, no effect of AFR\n")
ind <- data.frame(id = 1:300, z = stats::rnorm(300))
ind$ls <- pmax(3, pmin(20, round(10 + 3 * ind$z)))
ind$afr <- pmax(1, pmin(6, round(3 - 0.8 * ind$z + stats::rnorm(300, 0, 0.6))))
ind$death <- ind$afr + ind$ls - 1
ind$zk <- (ind$death - mean(ind$death)) / stats::sd(ind$death)
d <- do.call(rbind, lapply(seq_len(nrow(ind)), function(i) {
  q <- ind[i, ]; a <- seq(q$afr, q$afr + q$ls - 1)
  data.frame(id = q$id, age = a, trait = 10 + (-0.3 + 0.15 * q$zk) * (a - 8) + stats::rnorm(1) + stats::rnorm(length(a)))
}))
b <- run("E5 standardise", standardise_data(d, cmap()))
if (!is.null(b)) {
  r <- run("E5 fit", fit_model_suite(b$data, b$meta, models = c("M7", "M8", "M10"), age_function = "Linear"))
  if (isTRUE(r$ok)) {
    p10 <- term_p(r, "M10", c("f1", "AFR")); p8 <- term_p(r, "M8", c("f1", "AFR"))
    cat(sprintf("   AFR x age p: Model 10 %s, Model 8 %s\n", format_p(p10), format_p(p8)))
    report("E5 Model 10 reports false selective appearance when AFR is correlated with lifespan", isTRUE(p10 < 0.05), hard = FALSE)
    report("E5 Model 8 does not", isTRUE(p8 > 0.05), hard = FALSE)
  }
}

cat("== E6 unstandardised cubic on ages in days\n")
d <- do.call(rbind, lapply(1:150, function(i) {
  a <- sort(sample(seq(0, 1980, 30), sample(4:11, 1)))
  data.frame(id = i, age = a, trait = 5 + 0.002 * a - 1e-6 * a^2 + stats::rnorm(1) + stats::rnorm(length(a), 0, 0.5))
}))
b <- run("E6 standardise", standardise_data(d, cmap()))
if (!is.null(b)) {
  rr <- run("E6 raw fit", fit_model_suite(b$data, b$meta, models = c("M1", "M2"), age_function = "Cubic", standardise = FALSE))
  rs <- run("E6 standardised fit", fit_model_suite(b$data, b$meta, models = c("M1", "M2"), age_function = "Cubic", standardise = TRUE))
  if (isTRUE(rr$ok) && isTRUE(rs$ok)) {
    ages <- smooth_prediction_ages(b$data$age, 20)
    pr <- predict_population_curve(rr$fits$M1, rr, ages); ps <- predict_population_curve(rs$fits$M1, rs, ages)
    cat(sprintf("   largest difference between raw and standardised predictions: %.2g\n", max(abs(pr$fitted - ps$fitted))))
    report("E6 raw and standardised cubic predictions agree", max(abs(pr$fitted - ps$fitted)) < 1e-2 * max(1, stats::sd(b$data$trait)), hard = FALSE)
    x <- rr$coefficients[rr$coefficients$Model == "Model 1", , drop = FALSE]
    shown <- suppressWarnings(as.numeric(coef_display(x, rr)$Estimate))
    report("E6 no raw cubic coefficient is displayed as 0", !any(shown == 0 & abs(x$Statistic) > 0.1, na.rm = TRUE))
  }
}

cat("== E7 tiny and degenerate data\n")
d <- do.call(rbind, lapply(1:12, function(i) { n <- if (i <= 8) 1 else 3; data.frame(id = i, age = seq_len(n), trait = stats::rnorm(n, 5)) }))
b <- run("E7 standardise", standardise_data(d, cmap()))
if (!is.null(b)) {
  r <- run("E7 fit", fit_model_suite(b$data, b$meta, models = c("M1", "M2", "M3"), age_function = "Quadratic"))
  report("E7 models fit or fail with a message, never an error", !is.null(r))
  run("E7 decomposition", decomposition_trajectory(b$data))
  run("E7 individual functions", compare_individual_functions(b$data))
  run("E7 grid", build_missing_grid(b$data))
  if (isTRUE(r$ok)) {
    run("E7 peak", peak_onset_summary(r, names(r$fits)[[1]], n_draws = 50))
    run("E7 random effects", blup_proxy_covariance(r, names(r$fits)[[1]]))
    run("E7 screen", family_screen(r, names(r$fits)[[1]]))
  }
  report("E7 no step raised an error", !any(grepl("^E7", hard_fail)))
}

cat("== E8 the instant error-family screen\n")
if (HAS_GLMMTMB) {
  sim_counts <- function(gen) do.call(rbind, lapply(1:150, function(i) {
    a <- 1:sample(3:8, 1); mu <- exp(0.8 + 0.25 * a - 0.02 * a^2); data.frame(id = i, age = a, trait = gen(mu))
  }))
  for (cs in list(list("Poisson", function(mu) stats::rpois(length(mu), mu), FALSE),
                  list("negative binomial", function(mu) stats::rnbinom(length(mu), size = 2, mu = mu), TRUE),
                  list("Poisson with 25% extra zeros", function(mu) ifelse(stats::runif(length(mu)) < 0.25, 0, stats::rpois(length(mu), mu)), TRUE))) {
    b <- run(paste("E8", cs[[1]]), standardise_data(sim_counts(cs[[2]]), cmap()))
    if (is.null(b)) next
    r <- run(paste("E8 fit", cs[[1]]), fit_model_suite(b$data, b$meta, models = "M1", age_function = "Quadratic", family = "poisson"))
    if (!isTRUE(r$ok)) next
    scr <- family_screen(r, "M1")
    cat(sprintf("   %s fitted as Poisson: %s\n", cs[[1]], if (length(scr)) paste(scr, collapse = "; ") else "no flag"))
    report(sprintf("E8 %s: screen %s", cs[[1]], if (cs[[3]]) "flags it" else "stays quiet"), identical(length(scr) > 0, cs[[3]]), hard = FALSE)
  }
  d <- do.call(rbind, lapply(1:150, function(i) { a <- 1:sample(3:8, 1); data.frame(id = i, age = a, trait = exp(1 + 0.1 * a + stats::rnorm(1, 0, 0.3) + stats::rnorm(length(a), 0, 0.6))) }))
  b <- run("E8 skewed", standardise_data(d, cmap()))
  if (!is.null(b)) {
    r <- run("E8 skewed fit", fit_model_suite(b$data, b$meta, models = "M1", age_function = "Linear", family = "gaussian"))
    if (isTRUE(r$ok)) report("E8 a strongly skewed positive trait fitted as Gaussian is flagged", any(grepl("skewed", family_screen(r, "M1"))), hard = FALSE)
  }
}

cat("== E9 continuous proportions\n")
if (HAS_GLMMTMB) {
  d <- do.call(rbind, lapply(1:150, function(i) {
    a <- 1:sample(3:8, 1); mu <- stats::plogis(-1 + 0.3 * a + stats::rnorm(1, 0, 0.3)); data.frame(id = i, age = a, trait = stats::rbeta(length(a), mu * 8, (1 - mu) * 8))
  }))
  b <- run("E9 standardise", standardise_data(d, cmap()))
  if (!is.null(b)) {
    cf <- run("E9 family comparison", compare_families(b$data, b$meta, model = "M1", age_function = "Linear", families = DENSITY_FAMILIES))
    if (isTRUE(cf$ok)) {
      tb <- cf$table[order(cf$table$AIC), , drop = FALSE]
      cat(sprintf("   best family: %s\n", tb$Family[[1]]))
      report("E9 the beta family fits beta-distributed proportions best", identical(tb$Family[[1]], family_label("beta")), hard = FALSE)
    }
    d0 <- b$data; d0$trait[1] <- 1
    report("E9 an exact 1 is refused by the beta family with a message", !isTRUE(fit_model_suite(d0, b$meta, models = "M1", family = "beta")$ok))
  }
}

cat("== E10 random structures across fits share one comparison set\n")
d <- do.call(rbind, lapply(1:120, function(i) { a <- 1:sample(4:9, 1); s <- stats::rnorm(1, 0, 0.2); data.frame(id = i, age = a, trait = 10 + (0.3 + s) * a - 0.03 * a^2 + stats::rnorm(1) + stats::rnorm(length(a))) }))
b <- run("E10 standardise", standardise_data(d, cmap()))
if (!is.null(b)) {
  r1 <- run("E10 intercept", fit_model_suite(b$data, b$meta, models = "M4", age_function = "Quadratic", random_slope = "none"))
  r2 <- run("E10 slopes", fit_model_suite(b$data, b$meta, models = "M4", age_function = "Quadratic", random_slope = "uncorrelated"))
  r3 <- run("E10 logarithmic", fit_model_suite(b$data, b$meta, models = "M4", age_function = "Logarithmic", random_slope = "uncorrelated"))
  st <- Filter(Negate(is.null), lapply(list(r1, r2, r3), function(r) if (isTRUE(r$ok)) stored_model_entry(r, "M4", "sig") else NULL))
  for (k in seq_along(st)) st[[k]]$id <- paste0("S", k)
  tb <- stored_comparison_table(st)
  print(tb[, c("ID", "Set", "Specification", "AIC", "dAIC")], row.names = FALSE)
  report("E10 different random structures and functions on the same records form one set", length(unique(tb$Set)) == 1)
}

cat(if (length(hard_fail)) paste0("\nFAILURES (", length(hard_fail), "): ", paste(hard_fail, collapse = "; "), "\n") else "\nNo failures.\n")
if (length(hard_fail) && !interactive()) quit(status = 1)
