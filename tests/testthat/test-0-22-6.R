# 0.22.6: error-family warnings without running the error-family check

test_that("the instant family screen flags overdispersion or excess zeros, and counts fitted as Gaussian", {
  skip_if_not_installed("glmmTMB")
  t <- toy_prepared(n_id = 80, seed = 8, trait = "count_zinb")
  r <- fit_model_suite(t$prep$data, t$prep$meta, models = "M1", age_function = "Quadratic", family = "poisson")
  expect_true(any(grepl("overdispersion|zeros", family_screen(r, "M1"))))
  rg <- fit_model_suite(t$prep$data, t$prep$meta, models = "M1", age_function = "Quadratic", family = "gaussian")
  expect_true(any(grepl("like counts", family_screen(rg, "M1"))))
})

test_that("the error-family comparison covers the continuous families for a positive trait", {
  skip_if_not_installed("glmmTMB")
  t <- toy_prepared(n_id = 60, seed = 9)
  cf <- compare_families(t$prep$data, t$prep$meta, model = "M1", families = DENSITY_FAMILIES)
  expect_true(isTRUE(cf$ok))
  expect_true(all(c("Gaussian (lme4)", "Gamma (log link)", "lognormal (log link)") %in% cf$table$Family))
})
