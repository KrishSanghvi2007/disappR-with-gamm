# 0.23.2: individual frailty in the discrete-time hazard models

HAZARD_TYPES <- c("GLM", "GLM (frailty variance at zero)", "GLM (frailty model failed)", "GLMM with individual frailty")

test_that("the hazard models say whether an individual frailty was fitted", {
  t <- toy_prepared(n_id = 120, seed = 5)
  ia <- disappearance_data(t$prep$data, t$prep$meta, FALSE)
  z <- disappearance_models(ia)
  if (isTRUE(z$ok)) {
    expect_true(all(z$hazard_model %in% HAZARD_TYPES))
    expect_true("Fit" %in% names(z$table))
    expect_true(is.na(z$p_age_dependence) || (z$p_age_dependence >= 0 && z$p_age_dependence <= 1))
  }
  lt <- life_table(t$prep$data, t$prep$meta)
  expect_true(isTRUE(lt$ok))
  expect_true(lt$hazard_model %in% HAZARD_TYPES)
  expect_true(is.na(lt$p_constant) || (lt$p_constant >= 0 && lt$p_constant <= 1))
})

test_that("without individual labels the hazard model is a plain GLM", {
  set.seed(1)
  d <- data.frame(event = stats::rbinom(100, 1, 0.2), x = stats::rnorm(100))
  h <- hazard_glmm("event ~ x", d)
  expect_identical(h$type, "GLM")
  expect_s3_class(h$fit, "glm")
})
