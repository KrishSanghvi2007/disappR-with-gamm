test_that("every fitted model gets structured diagnostics", {
  t <- toy_prepared(n_id = 80, seed = 13)
  r <- fit_model_suite(t$prep$data, t$prep$meta, c("M1", "M2"), "Quadratic", "gaussian")
  expect_s3_class(r$diagnostics, "data.frame")
  expect_true(all(model_label(names(r$fits)) %in% r$diagnostics$Model))
  expect_true(all(r$diagnostics$`Status from diagnostics` %in% c("Valid", "Caution", "Failed")))
})

test_that("random-effect checks flag a grouping factor with too few levels", {
  t <- toy_prepared(n_id = 60, seed = 14)
  d <- t$prep$data
  d$rt_site <- rep(c("a", "b"), length.out = nrow(d))
  chk <- random_effect_checks(d, "none", "rt_site")
  expect_s3_class(chk, "data.frame")
  expect_true(any(chk$Status == "Caution" & grepl("rt_site", chk$Check, fixed = TRUE)))
})

test_that("predictions at representative ages are reported for the fitted models", {
  t <- toy_prepared(n_id = 80, seed = 15)
  r <- fit_model_suite(t$prep$data, t$prep$meta, c("M1", "M2"), "Quadratic", "gaussian")
  ps <- prediction_summary(r)
  expect_s3_class(ps, "data.frame")
  expect_gte(nrow(ps), 1)
  expect_true(all(startsWith(setdiff(names(ps), "Model"), "Age ")))
})
