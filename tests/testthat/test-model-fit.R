test_that("a Gaussian comparison fits, ranks by AIC and labels its p-values", {
  t <- toy_prepared(n_id = 100, seed = 2, sd_type = "dependent")
  r <- fit_model_suite(t$prep$data, t$prep$meta, c("M1", "M2", "M4"), "Quadratic", "gaussian")
  expect_true(isTRUE(r$ok))
  expect_true(all(is.finite(r$aic$AIC)))
  expect_true(all(r$coefficients$P_method %in% c("t, Satterthwaite df", "Wald z, asymptotic")))
})

test_that("the dry run reports shared rows and individuals without fitting", {
  t <- toy_prepared(n_id = 60, seed = 3)
  pv <- fit_model_suite(t$prep$data, t$prep$meta, c("M1", "M2"), "Quadratic", "gaussian", dry_run = TRUE)
  expect_true(isTRUE(pv$ok) && isTRUE(pv$dry_run))
  expect_null(pv$fits)
  expect_lte(pv$n_rows_used, pv$n_rows)
  expect_true(all(c("formulas", "random", "family", "standardise") %in% names(pv)))
})

test_that("data that cannot support the models stop with a message", {
  t <- toy_prepared(n_id = 60, seed = 4)
  raw <- t$raw
  m <- toy_mapping(raw)
  cst <- raw
  cst[[m$trait]] <- 5
  bc <- standardise_data(cst, toy_mapping(cst))
  rc <- fit_model_suite(bc$data, bc$meta, c("M1", "M2"), "Quadratic", "gaussian")
  expect_false(isTRUE(rc$ok))
  expect_match(rc$message, "single value")
  one <- do.call(rbind, lapply(split(raw, raw$ID), function(x) x[nrow(x), , drop = FALSE]))
  bo <- standardise_data(one, toy_mapping(one))
  ro <- fit_model_suite(bo$data, bo$meta, c("M1", "M2"), "Linear", "gaussian")
  expect_false(isTRUE(ro$ok))
  expect_match(ro$message, "single record")
})

test_that("a covariate that duplicates ALR is named before fitting", {
  t <- toy_prepared(n_id = 60, seed = 5)
  dup <- t$raw
  dup$alr_copy <- stats::ave(dup$age, dup$ID, FUN = max)
  md <- toy_mapping(dup)
  md$covars <- "alr_copy"
  bd <- standardise_data(dup, md)
  pv <- fit_model_suite(bd$data, bd$meta, c("M1", "M2", "M4"), "Quadratic", "gaussian", dry_run = TRUE)
  expect_true(any(grepl("ALR", pv$alias_notes)))
})

test_that("the model output table marks age and lifespan-proxy terms, not covariates", {
  raw <- c("(Intercept)", "f1", "f2", "ALR", "f1:ALR", "ALR:f2", "mean_f1", "delta_f1", "mean_f1:delta_f1", "AFR", "LS:f1",
           "cv_sex", "cv_sex:f1", "ALR2")
  expect_identical(key_age_proxy_terms(raw),
                   c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE))
})

test_that("random terms that explain no variance are flagged, with advice that fits the term", {
  vc <- data.frame(Group = c("id", "id", "id", "year", "Residual"), Term = c("(Intercept)", "f1", "(Intercept) ~ f1", "(Intercept)", "Residual"),
                   Type = c("SD", "SD", "Correlation", "SD", "SD"), Variance = c(2, 0, NA, 0.01, 3))
  fl <- negligible_random_terms(vc)
  expect_identical(fl$flag, c(FALSE, TRUE, FALSE, TRUE, FALSE))           # 0 and 0.2% of the variance; residual never
  expect_equal(fl$share[[4]], 0.01 / 5.01)
  glmm <- data.frame(Group = c("id", "year"), Term = "(Intercept)", Type = "SD", Variance = c(0.3, 0.001))
  expect_identical(negligible_random_terms(glmm)$flag, c(FALSE, TRUE))    # link scale: SD 0.03 < 0.05
  expect_match(random_term_flag_text("year", "(Intercept)", FALSE, share = 0.002), "try refitting without that term")
  expect_match(random_term_flag_text("ID", "(Intercept)", TRUE, share = 0.002), "stays in the models")
  expect_match(random_term_flag_text("ID", "age (first ageing term)", TRUE, share = 0), "without this term")   # 0.24.7 wording
})

test_that("the fixed-effect fallback reproduces glmmTMB's own population prediction", {
  skip_if_not_installed("glmmTMB")
  set.seed(3)
  d <- data.frame(id = factor(rep(1:30, each = 5)), f1 = rep(seq(-1, 1, length.out = 5), 30))
  d$trait <- stats::rpois(nrow(d), exp(1 + 0.3 * d$f1))
  fit <- glmmTMB::glmmTMB(trait ~ f1 + (1 | id), data = d, family = stats::poisson())
  nd <- data.frame(f1 = c(-0.5, 0, 0.5), id = d$id[[1]])
  expect_equal(glmmtmb_fixed_predict(fit, nd),
               as.numeric(stats::predict(fit, newdata = nd, re.form = NA, type = "response")), tolerance = 1e-8)
})
