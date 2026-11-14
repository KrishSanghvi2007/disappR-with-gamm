# Step 8: Bayesian refits of the step-5 models (R/bayes.R). Everything here runs without Stan except the last test,
# which samples only when DISAPPR_TEST_STAN=true (it compiles a model, about two minutes).

bayes_toy_fit <- function(family = "gaussian", n_id = 60, seed = 4) {
  set.seed(seed)
  d <- do.call(rbind, lapply(seq_len(n_id), function(i) {
    ls <- sample(3:9, 1); a <- 1:ls
    u <- stats::rnorm(1, 0, 0.4)
    eta <- 1 + 0.3 * a - 0.04 * a^2 + 0.05 * ls + u
    y <- switch(family, gaussian = eta + stats::rnorm(ls, 0, 0.5), poisson = stats::rpois(ls, exp(eta / 2)),
                binomial = stats::rbinom(ls, 1, stats::plogis(eta - 2)))
    data.frame(id = i, age = a, trait = y, sex = ifelse(i %% 2, "F", "M"))
  }))
  b <- standardise_data(d, list(id = "id", age = "age", trait = "trait", alr = "__AUTO_LAST__", life = "", entry = "__AUTO_FIRST__",
    covars = "sex", cov_factor = "sex", cov_int = character(0), group = "", nested = TRUE, random = character(0), censor = "",
    censor_value = "", condition = "", trials = "", start_mode = "afr", start_age = NA_real_, age_round = NA_real_, cov_age = character(0)))
  fit_model_suite(b$data, b$meta, c("M1", "M2", "M4"), "Quadratic", family, "none")
}

test_that("the refit uses step 5's formula, family and records", {
  r <- bayes_toy_fit()
  expect_true(r$ok)
  m <- bayes_default_model(r)
  expect_identical(m, sub("^Model ", "M", r$aic$Model[r$aic$Eligible][1]))
  sp <- bayes_spec(r, "M4")
  expect_true(sp$ok)
  expect_identical(sp$formula, paste("trait ~", r$formulas[["M4"]], "+", r$random))
  expect_identical(sp$n, nrow(r$data))
  expect_true(all(c("age", "id", "f1", "f2", "ALR") %in% names(sp$data)))
  expect_true(nrow(sp$freq_curve) > 10)
  expect_true(all(c("Estimate", "SE") %in% names(sp$freq_coef)))
})

test_that("models and families outside phase 1 are refused with a plain message", {
  expect_false(bayes_spec(NULL, "M1")$ok)
  r <- bayes_toy_fit()
  expect_match(bayes_spec(r, "M9")$message, "fitted in step 5")
  r2 <- r; r2$family <- "nbinom2"
  expect_match(bayes_spec(r2, "M1")$message, "Gaussian, Poisson and binomial")
  r3 <- r; r3$disp <- "age"
  expect_match(bayes_spec(r3, "M1")$message, "constant")
  r4 <- r; r4$nonlinear <- TRUE
  expect_match(bayes_spec(r4, "M1")$message, "non-linear")
})

test_that("priors are autoscaled and widen with the width setting", {
  sp <- bayes_spec(bayes_toy_fit(), "M2")
  p1 <- bayes_priors(sp, "narrow"); p2 <- bayes_priors(sp, "default"); p3 <- bayes_priors(sp, "flat")
  expect_equal(unname(p2$b_sd / p1$b_sd), rep(2.5, length(p1$b_sd)))
  expect_equal(unname(p3$b_sd / p2$b_sd), rep(40, length(p2$b_sd)))
  X <- stats::model.matrix(stats::as.formula(paste("~", sp$fixed_rhs)), sp$data)
  expect_equal(unname(p2$b_sd[["f1"]]), 2.5 * stats::sd(sp$data$trait) / stats::sd(X[, "f1"]))
  expect_equal(p2$values[["int_loc"]], mean(sp$data$trait))
  expect_true(all(c("Intercept", "Fixed effect", "Random-effect SDs", "Residual SD") %in% p2$table$Class))
  expect_identical(bayes_width_value("nonsense"), 2.5)
})

test_that("prior curves: wider priors allow more implausible ageing curves", {
  sp <- bayes_spec(bayes_toy_fit(), "M2")
  pn <- bayes_prior_curves(sp, bayes_priors(sp, "narrow"))
  pf <- bayes_prior_curves(sp, bayes_priors(sp, "flat"))
  expect_identical(nrow(pn$curves), 200L * length(sp$ages))
  expect_true(pf$share_outside >= pn$share_outside)
  expect_true(pf$share_outside > 0.9)
  expect_match(bayes_prior_reading(pf, sp, bayes_priors(sp, "flat")), "implausible")
  rb <- bayes_toy_fit("binomial")
  spb <- bayes_spec(rb, "M1")
  expect_identical(spb$brms_family, "bernoulli")
  pb <- bayes_prior_curves(spb, bayes_priors(spb, "flat"))
  expect_true(all(pb$curves$value >= 0 & pb$curves$value <= 1))
  expect_true(pb$share_extreme > 0.5)
  expect_match(bayes_prior_reading(pb, spb, bayes_priors(spb, "flat")), "0 and 1")
})

test_that("curves from fixed-effect draws reproduce the step-5 curve at the estimates", {
  r <- bayes_toy_fit()
  sp <- bayes_spec(r, "M2")
  b <- lme4::fixef(r$fits[["M2"]])
  B <- matrix(b, ncol = 1, dimnames = list(names(b), NULL))
  M <- bayes_curve_matrix(sp, B)
  expect_equal(as.numeric(M[, 1]), sp$freq_curve$fitted, tolerance = 1e-8)
  rp <- bayes_toy_fit("poisson")
  spp <- bayes_spec(rp, "M2")
  bp <- glmmTMB::fixef(rp$fits[["M2"]])$cond
  Mp <- bayes_curve_matrix(spp, matrix(bp, ncol = 1, dimnames = list(names(bp), NULL)))
  expect_equal(as.numeric(Mp[, 1]), spp$freq_curve$fitted, tolerance = 1e-6)
})

test_that("convergence traffic lights follow the published thresholds", {
  st <- bayes_settings("full")
  good <- data.frame(rhat = c(1.001, 1.004), ess_bulk = c(900, 1200), ess_tail = c(800, 1000))
  d <- bayes_diagnostics(good, 0, 0, c(0.8, 0.9, 0.7, 1), st)
  expect_identical(attr(d, "overall"), "good")
  bad <- data.frame(rhat = c(1.001, 1.08), ess_bulk = c(50, 1200), ess_tail = c(300, 1000))
  d2 <- bayes_diagnostics(bad, 3, 0, c(0.8, 0.2), st)
  expect_identical(attr(d2, "overall"), "problem")
  expect_identical(d2$Status[d2$Check == "R-hat (largest)"], "problem")
  expect_identical(d2$Status[d2$Check == "Bulk ESS (smallest)"], "problem")
  expect_identical(d2$Status[d2$Check == "Tail ESS (smallest)"], "caution")
  expect_identical(d2$Status[d2$Check == "Divergent transitions"], "problem")
  expect_identical(d2$Status[d2$Check == "E-BFMI (smallest chain)"], "caution")
  # a draft run never reads as fully converged
  z <- list(diagnostics = d, settings = bayes_settings("draft"))
  expect_identical(bayes_overall(z)$status, "caution")
  expect_match(bayes_overall(z)$text, "draft")
})

test_that("sampler settings are validated", {
  s <- bayes_settings("draft", iter = 50, adapt_delta = 2, max_treedepth = 1)
  expect_identical(s$iter, 1000L)
  expect_identical(s$adapt_delta, 0.95)
  expect_identical(s$max_treedepth, 10L)
  s2 <- bayes_settings("full", iter = 3000, warmup = 1500)
  expect_identical(c(s2$chains, s2$iter, s2$warmup), c(4L, 3000L, 1500L))
})

test_that("parameter labels are readable", {
  expect_identical(bayes_param_label(c("b_Intercept", "b_f1:ALR", "sd_re_year__Intercept", "cor_id__Intercept__f1", "sigma")),
                   c("Intercept", "f1:ALR", "SD year: Intercept", "Correlation id: Intercept, f1", "Residual SD"))
})

test_that("a short Stan fit agrees with step 5 (DISAPPR_TEST_STAN=true)", {
  skip_if_not(identical(Sys.getenv("DISAPPR_TEST_STAN"), "true"), "set DISAPPR_TEST_STAN=true to sample with Stan")
  skip_if_not_installed("brms")
  r <- bayes_toy_fit()
  sp <- bayes_spec(r, "M2")
  z <- bayes_fit(sp, bayes_settings("draft", seed = 3), cache_dir = file.path(tempdir(), "bayes_test"))
  expect_true(z$ok)
  ct <- bayes_compare_table(z, sp)
  expect_true(max(abs(as.numeric(ct$`Shift (step-5 SEs)`)), na.rm = TRUE) < 0.5)
  expect_true(nrow(z$curve) == length(sp$ages))
  expect_false(inherits(tryCatch(parse(text = bayes_code(z, sp)), error = function(e) e), "error"))
  expect_match(bayes_methods(z, sp), "brms")
})
