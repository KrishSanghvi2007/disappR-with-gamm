test_that("the decomposition runs across a regular schedule without a caution", {
  t <- toy_prepared(n_id = 150, seed = 8, sd_type = "none")
  de <- decomposition_trajectory(t$prep$data)
  expect_s3_class(de, "data.frame")
  expect_gt(nrow(de), 5)
  expect_null(attr(de, "caution"))
})

test_that("irregular ages are decomposed on a common grid with a caution", {
  raw <- simulate_toy_data(list(n_id = 150, seed = 9, sd_type = "none"))
  set.seed(1)
  raw$age <- raw$age + stats::runif(nrow(raw), -0.35, 0.35)
  b <- standardise_data(raw, toy_mapping(raw))
  de <- decomposition_trajectory(b$data)
  expect_gt(nrow(de), 5)
  expect_length(attr(de, "caution"), 1)
})
