test_that("simulations are reproducible and leave R's random-number stream unchanged", {
  set.seed(99)
  before <- .Random.seed
  a <- simulate_toy_data(list(n_id = 40, seed = 3))
  expect_identical(before, .Random.seed)
  b <- simulate_toy_data(list(n_id = 40, seed = 3))
  tr <- toy_mapping(a)$trait
  expect_identical(a$age, b$age)
  expect_identical(a[[tr]], b[[tr]])
})
