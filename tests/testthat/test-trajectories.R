test_that("population curves do not depend on row order", {
  t <- toy_prepared(n_id = 70, seed = 6)
  set.seed(7)
  cv <- t$raw
  cv$temp <- stats::rnorm(nrow(cv), 20, 3)
  cv$season <- sample(c("dry", "wet"), nrow(cv), replace = TRUE)
  mc <- toy_mapping(cv)
  mc$covars <- c("temp", "season")
  mc$cov_factor <- "season"
  b1 <- standardise_data(cv, mc)
  b2 <- standardise_data(cv[sample(nrow(cv)), , drop = FALSE], mc)
  r1 <- fit_model_suite(b1$data, b1$meta, "M2", "Quadratic", "gaussian")
  r2 <- fit_model_suite(b2$data, b2$meta, "M2", "Quadratic", "gaussian")
  ages <- seq(2, 18, length.out = 9)
  p1 <- predict_population_curve(r1$fits$M2, r1, ages)
  p2 <- predict_population_curve(r2$fits$M2, r2, ages)
  expect_equal(p1$fitted, p2$fitted, tolerance = 1e-6)
})

test_that("prediction ages: a fine grid for curves, observed ages for lines", {
  expect_length(smooth_prediction_ages(c(5, 6, 7, 8)), 100)
  expect_identical(observed_prediction_ages(c(3, 1, 2, 2, NA)), c(1, 2, 3))
})

# ---- 0.20.5: step 4 reports progress, reuses fits it is given, and keeps exponential curves finite ----
test_that("individual fits report progress, and the comparison reuses the fits it is given", {
  set.seed(9)
  d <- do.call(rbind, lapply(seq_len(30), function(i) data.frame(id = paste0("i", i), age = 1:6,
                                                                  trait = 10 - 0.3 * (1:6) + 0.02 * (1:6)^2 + stats::rnorm(6, 0, 0.5))))
  seen <- numeric(0)
  fit_individual_function(d, "Quadratic", progress = function(f, detail) seen <<- c(seen, f))
  expect_true(length(seen) >= 1 && all(diff(seen) > 0) && utils::tail(seen, 1) == 1)
  fits <- stats::setNames(lapply(A3_FUNCTIONS, function(fn) fit_individual_function(d, fn)), A3_FUNCTIONS)
  expect_equal(compare_individual_functions(d, fits = fits), compare_individual_functions(d))
  fits[["Quadratic"]]$fit_stats$AICc <- fits[["Quadratic"]]$fit_stats$AICc - 1000   # a changed fit changes the table
  expect_identical(compare_individual_functions(d, fits = fits)$Function[[1]], "Quadratic")
})

test_that("exponential curves stay finite however extreme the coefficients", {
  expect_true(is.finite(exp_capped(1e6)))
  z <- list(coefs = data.frame(id = "a", a = 1, b = 500), nonlinear = TRUE)
  cv <- individual_curves(z, data.frame(id = "a", age = c(1, 10), trait = c(1, 2)), "a")
  expect_true(nrow(cv) > 0 && all(is.finite(cv$fitted)))
})

# ---- 0.20.12: step 4 fits several functions in one call (run in a separate R process by the app) ----
test_that("several functions are fitted in one call, with progress across all of them", {
  set.seed(10)
  d <- do.call(rbind, lapply(seq_len(20), function(i) data.frame(id = paste0("i", i), age = 1:6, trait = 5 + 0.2 * (1:6) + stats::rnorm(6))))
  seen <- numeric(0)
  z <- fit_individual_functions(d, c("Linear", "Quadratic"), progress = function(f, detail) seen <<- c(seen, f))
  expect_named(z, c("Linear", "Quadratic"))
  expect_gt(nrow(z$Linear$fit_stats), 0)
  expect_equal(z$Quadratic$fit_stats, fit_individual_function(d, "Quadratic")$fit_stats)
  expect_true(all(diff(seen) >= 0) && max(seen) == 1 && min(seen) <= 0.5)
})

test_that("the average trajectory spans ages held by at least five individuals", {
  expect_identical(A3_MIN_IND_PER_AGE, 5L)
  set.seed(4)
  n <- 30
  d <- do.call(rbind, lapply(seq_len(n), function(i) {
    ls <- if (i <= 6) 9 else 5                       # only six individuals reach ages 6-9
    data.frame(id = sprintf("i%02d", i), age = seq_len(ls), trait = 10 - 0.4 * seq_len(ls) + stats::rnorm(ls, 0, 0.2))
  }))
  z <- fit_individual_function(d, "Linear", 1)
  expect_gte(max(z$mean_curve$age), 9)               # five or more individuals at every age here
  expect_true(all(is.finite(z$mean_curve$fitted)))
})

test_that("a function whose AICc cannot be computed is left out of the ranking, not the whole ranking (0.21.15)", {
  set.seed(1)
  d <- do.call(rbind, lapply(seq_len(30), function(i) data.frame(id = paste0("i", i), age = 1:6,
                                                                  trait = 10 - 0.3 * (1:6) + 0.02 * (1:6)^2 + stats::rnorm(6, 0, 0.5))))
  tab <- compare_individual_functions(d)
  ex <- attr(tab, "excluded")
  expect_true("Cubic" %in% names(ex))                            # six records cannot carry a cubic's AICc
  expect_true(tab$N_common[[1]] > 0)                             # the other functions are still compared
  expect_true(is.finite(tab$Mean_dAICc[[1]]))                    # the leader is chosen by AICc, not R-squared
  expect_true(is.na(tab$Mean_dAICc[tab$Function == "Cubic"]))    # and the excluded function has no dAICc
  expect_identical(utils::tail(tab$Function, 1), "Cubic")        # listed last
})

test_that("with records enough for every function, nothing is left out", {
  set.seed(2)
  d <- do.call(rbind, lapply(seq_len(20), function(i) data.frame(id = paste0("i", i), age = 1:10,
                                                                  trait = 5 + 0.4 * (1:10) + stats::rnorm(10, 0, 0.5))))
  expect_false("Cubic" %in% names(attr(compare_individual_functions(d), "excluded")))
})
