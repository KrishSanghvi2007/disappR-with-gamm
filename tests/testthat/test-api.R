test_that("the R interface runs a whole analysis", {
  sim <- disappr_simulate(n_id = 60, seed = 12)
  x <- disappr_prepare(sim, toy_mapping(sim))
  fit <- disappr_fit(x, models = c("M1", "M2"))
  expect_s3_class(disappr_compare(fit), "data.frame")
  expect_true(is.list(disappr_diagnose(fit)))
  expect_s3_class(disappr_predict(fit, "M1"), "data.frame")
  expect_true(is.list(disappr_provenance(fit)))
  expect_type(disappr_interpret(fit), "character")
  expect_type(disappr_code(fit, x), "character")
})

test_that("bundled examples load wherever R is running", {
  ex <- disappr_example("bichet")
  expect_s3_class(ex$data, "data.frame")
  expect_gt(nrow(ex$data), 100)
  expect_true(is.list(ex$mapping))
})
