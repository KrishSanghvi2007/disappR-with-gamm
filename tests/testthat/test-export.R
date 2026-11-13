test_that("the exported script parses and uses the shared engine when available", {
  t <- toy_prepared(n_id = 60, seed = 11)
  r <- fit_model_suite(t$prep$data, t$prep$meta, c("M1", "M2"), "Quadratic", "gaussian")
  code <- model_r_code(r, t$prep$meta, "disappR_simulated.csv", source_type = "toy")
  expect_silent(parse(text = code))
  expect_match(code, "use_engine <- requireNamespace", fixed = TRUE)
  expect_match(code, "disappr_fit(x, models = ", fixed = TRUE)
  expect_equal(nrow(analysis_data_export(r)), nrow(r$data))
})
