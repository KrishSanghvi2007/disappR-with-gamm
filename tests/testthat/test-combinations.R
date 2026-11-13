# Combinations of settings, not only components. A pairwise covering design: every pair of settings across the
# ageing function, among-individual terms, random structure, standardisation and error family appears together
# at least once, in 15 cases instead of all 270. Each case fits all ten models and checks that the fit records
# its settings, that every fitted model predicts, and that the exported script creates every variable its
# formulas use and asks for the same settings. Slow: set DISAPPR_SKIP_COMBOS=true to skip.

COMBOS <- list(
  list(age_function = "Cubic", among = "same", random_slope = "uncorrelated", standardise = TRUE, family = "poisson"),
  list(age_function = "Asymptotic exponential", among = "linear", random_slope = "none", standardise = FALSE, family = "nbinom2"),
  list(age_function = "Linear", among = "linear", random_slope = "correlated", standardise = TRUE, family = "gaussian"),
  list(age_function = "Quadratic", among = "consistent", random_slope = "uncorrelated", standardise = FALSE, family = "gaussian"),
  list(age_function = "Quadratic", among = "same", random_slope = "correlated", standardise = FALSE, family = "poisson"),
  list(age_function = "Logarithmic", among = "consistent", random_slope = "correlated", standardise = TRUE, family = "nbinom2"),
  list(age_function = "Logarithmic", among = "same", random_slope = "none", standardise = TRUE, family = "gaussian"),
  list(age_function = "Linear", among = "consistent", random_slope = "none", standardise = FALSE, family = "poisson"),
  list(age_function = "Logarithmic", among = "linear", random_slope = "uncorrelated", standardise = FALSE, family = "poisson"),
  list(age_function = "Cubic", among = "linear", random_slope = "none", standardise = FALSE, family = "gaussian"),
  list(age_function = "Asymptotic exponential", among = "consistent", random_slope = "correlated", standardise = TRUE, family = "poisson"),
  list(age_function = "Linear", among = "same", random_slope = "uncorrelated", standardise = FALSE, family = "nbinom2"),
  list(age_function = "Asymptotic exponential", among = "same", random_slope = "uncorrelated", standardise = TRUE, family = "gaussian"),
  list(age_function = "Quadratic", among = "linear", random_slope = "none", standardise = TRUE, family = "nbinom2"),
  list(age_function = "Cubic", among = "consistent", random_slope = "correlated", standardise = TRUE, family = "nbinom2")
)

combo_data <- function(family, seed = 21, n = 40) {
  set.seed(seed)
  raw <- do.call(rbind, lapply(seq_len(n), function(i) {
    q <- stats::rnorm(1); afr <- sample(1:2, 1); ls <- max(afr + 3, round(8 + 2 * q)); age <- afr:ls
    eta <- log(12) + 0.08 * age - 0.006 * age^2 + 0.1 * q
    y <- switch(family, gaussian = 20 + 1.0 * age - 0.06 * age^2 + 0.8 * q + stats::rnorm(length(age), 0, 0.8),
                poisson = stats::rpois(length(age), exp(eta)), stats::rnbinom(length(age), size = 3, mu = exp(eta)))
    data.frame(id = paste0("c", i), age = age, LS = ls, AFR = afr, trait = y)
  }))
  standardise_data(raw, list(id = "id", age = "age", trait = "trait", alr = "__AUTO_LAST__", life = "LS", entry = "AFR",
                             covars = character(0), cov_factor = character(0), cov_int = character(0), group = "",
                             nested = TRUE, random = character(0), censor = "", censor_value = "", condition = "",
                             trials = "", start_mode = "afr", start_age = NA_real_, age_round = NA_real_,
                             cov_age = character(0)))
}

test_that("every pair of settings fits, predicts and exports consistently", {
  skip_on_cran()
  skip_if(identical(Sys.getenv("DISAPPR_SKIP_COMBOS"), "true"), "combination tests skipped")
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  for (cc in COMBOS) {
    label <- paste(unlist(cc), collapse = " / ")
    b <- combo_data(cc$family)
    r <- tryCatch(fit_model_suite(b$data, b$meta, models = MODEL_IDS, age_function = cc$age_function, family = cc$family,
                                  random_slope = cc$random_slope, standardise = cc$standardise, among = cc$among),
                  error = function(e) e)
    expect_false(inherits(r, "error"), info = paste(label, if (inherits(r, "error")) conditionMessage(r) else ""))
    if (inherits(r, "error") || !isTRUE(r$ok)) next
    poly <- cc$among %in% c("same", "consistent") && cc$age_function %in% c("Quadratic", "Cubic")
    expect_identical(r$among, if (poly) cc$among else "linear", info = label)
    fitted <- Filter(Negate(is.null), r$fits)
    for (m in names(fitted)) {
      pc <- tryCatch(predict_population_curve(fitted[[m]], r, c(2, 4, 6)), error = function(e) data.frame())
      expect_true(nrow(pc) > 0 && all(is.finite(pc$fitted)), info = paste(label, m, "predicts"))
    }
    script <- model_r_code(r, b$meta, "data.csv")
    expect_length(script, 1)
    expect_false(inherits(tryCatch(parse(text = script), error = function(e) e), "error"), info = paste(label, "exported script parses"))
    for (m in names(fitted)) {
      f <- r$formulas[[m]]
      if (is.null(f)) next
      vars <- all.vars(stats::as.formula(if (grepl("~", f)) f else paste("~", f)))
      for (v in setdiff(vars, c("trait", "id", "age"))) {
        made <- if (grepl("^(ALR|AFR|LS)[23]$", v)) grepl("dat[[paste0(v, 2)]]", script, fixed = TRUE)
                else grepl(paste0("dat\\$", v, "\\s*<-"), script)
        expect_true(made, info = paste(label, m, "script creates", v))
      }
    }
    eng <- paste(engine_script_lines(r, b$meta, "data.csv"), collapse = "\n")
    expect_match(eng, sprintf('age_function = "%s"', cc$age_function), fixed = TRUE, info = label)
    expect_match(eng, sprintf('family = "%s"', cc$family), fixed = TRUE, info = label)
    expect_match(eng, sprintf('among = "%s"', r$among), fixed = TRUE, info = label)
  }
})

# 0.20.2: the families the pairwise design above leaves out - nbinom1, the zero-inflated counts, binomial proportions
# with trials, beta-binomial and binary traits - and the non-linear exponential. Every family appears with more than
# one ageing function, among option, random structure and standardisation setting.
COMBOS_EXTRA <- list(
  list(age_function = "Linear", among = "linear", random_slope = "none", standardise = TRUE, family = "nbinom1"),
  list(age_function = "Cubic", among = "consistent", random_slope = "uncorrelated", standardise = FALSE, family = "nbinom1"),
  list(age_function = "Quadratic", among = "same", random_slope = "none", standardise = TRUE, family = "zip"),
  list(age_function = "Logarithmic", among = "linear", random_slope = "correlated", standardise = FALSE, family = "zip"),
  list(age_function = "Asymptotic exponential", among = "consistent", random_slope = "none", standardise = TRUE, family = "zinb"),
  list(age_function = "Quadratic", among = "linear", random_slope = "uncorrelated", standardise = FALSE, family = "zinb"),
  list(age_function = "Linear", among = "same", random_slope = "correlated", standardise = TRUE, family = "zinb1"),
  list(age_function = "Quadratic", among = "consistent", random_slope = "none", standardise = TRUE, family = "binomial"),
  list(age_function = "Logarithmic", among = "same", random_slope = "uncorrelated", standardise = FALSE, family = "binomial"),
  list(age_function = "Cubic", among = "linear", random_slope = "none", standardise = TRUE, family = "betabinomial"),
  list(age_function = "Asymptotic exponential", among = "same", random_slope = "correlated", standardise = FALSE, family = "betabinomial"),
  list(age_function = "Linear", among = "linear", random_slope = "none", standardise = TRUE, family = "binary"),
  list(age_function = "Quadratic", among = "same", random_slope = "uncorrelated", standardise = TRUE, family = "binary"),
  list(age_function = A3_NONLINEAR, among = "linear", random_slope = "none", standardise = TRUE, family = "gaussian"),
  list(age_function = A3_NONLINEAR, among = "linear", random_slope = "uncorrelated", standardise = TRUE, family = "gaussian"),
  list(age_function = A3_NONLINEAR, among = "linear", random_slope = "none", standardise = TRUE, family = "poisson")
)

combo_data_extra <- function(family, seed = 23, n = 60) {
  set.seed(seed)
  raw <- do.call(rbind, lapply(seq_len(n), function(i) {
    q <- stats::rnorm(1); afr <- sample(1:2, 1); ls <- max(afr + 3, round(8 + 2 * q)); age <- afr:ls
    k <- length(age)
    eta <- log(6) + 0.10 * age - 0.009 * age^2 + 0.15 * q
    p <- stats::plogis(0.8 - 0.25 * (age - 5) + 0.3 * q)
    y <- switch(family,
      nbinom1 = stats::rnbinom(k, size = exp(eta) / 1.5, mu = exp(eta)),
      zip = stats::rpois(k, exp(eta)) * stats::rbinom(k, 1, 0.75),
      zinb = , zinb1 = stats::rnbinom(k, size = 2, mu = exp(eta)) * stats::rbinom(k, 1, 0.75),
      binomial = , betabinomial = stats::rbinom(k, 10, p) / 10,
      binary = stats::rbinom(k, 1, p),
      gaussian = 25 * exp(-0.04 * age) + 0.8 * q + stats::rnorm(k, 0, 0.6),
      stats::rpois(k, exp(eta)))
    data.frame(id = paste0("e", i), age = age, LS = ls, AFR = afr, trait = y, n = 10)
  }))
  standardise_data(raw, list(id = "id", age = "age", trait = "trait", alr = "__AUTO_LAST__", life = "LS", entry = "AFR",
                             covars = character(0), cov_factor = character(0), cov_int = character(0), group = "",
                             nested = TRUE, random = character(0), censor = "", censor_value = "", condition = "",
                             trials = if (family %in% c("binomial", "betabinomial")) "n" else "", start_mode = "afr",
                             start_age = NA_real_, age_round = NA_real_, cov_age = character(0)))
}

test_that("the families and the ageing function the pairwise design leaves out fit, predict on their scale and export", {
  skip_on_cran()
  skip_if(identical(Sys.getenv("DISAPPR_SKIP_COMBOS"), "true"), "combination tests skipped")
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("nlme")
  for (cc in COMBOS_EXTRA) {
    fam <- if (identical(cc$family, "binary")) "binomial" else cc$family
    label <- paste(unlist(cc), collapse = " / ")
    b <- combo_data_extra(cc$family)
    r <- tryCatch(fit_model_suite(b$data, b$meta, models = MODEL_IDS, age_function = cc$age_function, family = fam,
                                  random_slope = cc$random_slope, standardise = cc$standardise, among = cc$among),
                  error = function(e) e)
    expect_false(inherits(r, "error"), info = paste(label, if (inherits(r, "error")) conditionMessage(r) else ""))
    if (inherits(r, "error")) next
    expect_true(isTRUE(r$ok), info = paste(label, "fits:", paste(r$message %||% "", collapse = " ")))
    if (!isTRUE(r$ok)) next
    poly <- cc$among %in% c("same", "consistent") && cc$age_function %in% c("Quadratic", "Cubic")
    expect_identical(r$among, if (poly) cc$among else "linear", info = label)
    fitted <- Filter(Negate(is.null), r$fits)
    for (m in names(fitted)) {
      pc <- tryCatch(predict_population_curve(fitted[[m]], r, c(2, 4, 6)), error = function(e) data.frame())
      ok <- nrow(pc) > 0 && all(is.finite(pc$fitted))
      expect_true(ok, info = paste(label, m, "predicts"))
      if (ok && !identical(fam, "gaussian")) expect_true(all(pc$fitted >= 0), info = paste(label, m, "non-negative"))
      if (ok && fam %in% BINOMIAL_FAMILIES) expect_true(all(pc$fitted <= 1), info = paste(label, m, "a proportion"))
    }
    script <- model_r_code(r, b$meta, "data.csv")
    expect_false(inherits(tryCatch(parse(text = script), error = function(e) e), "error"), info = paste(label, "exported script parses"))
    ev <- tryCatch(models_evidence(r, NA_character_), error = function(e) e)
    expect_false(inherits(ev, "error"), info = paste(label, "evidence"))
  }
})
