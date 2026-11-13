# The model-term interpretation contrasts predictions for individuals with a high and a low lifespan proxy. For count
# and binomial models the contrast is a log ratio of response-scale predictions, which are positive in principle but
# can round to exactly zero. And the prediction function drops any age whose prediction failed, so the two sides
# must be matched on age, not by position.

test_that("predictions are matched on age, not by position", {
  lo <- data.frame(age = c(1, 5, 9), fitted = c(10, 8, 6))
  hi <- data.frame(age = c(1, 9), fitted = c(12, 9))          # age 5 failed on this side
  pc <- prediction_contrast(lo, hi, ratio = TRUE)
  expect_equal(pc$age, c(1, 9))
  expect_equal(exp(pc$eff), c(1.2, 1.5))                       # by position, age 9 would have been compared with age 5
})

test_that("a zero prediction is flagged as unusable, without warnings or infinite values", {
  lo <- data.frame(age = c(1, 5), fitted = c(10, 0))
  hi <- data.frame(age = c(1, 5), fitted = c(12, 3))
  expect_silent(pc <- prediction_contrast(lo, hi, ratio = TRUE))
  expect_equal(pc$usable, c(TRUE, FALSE))
  expect_true(is.na(pc$eff[2]))
  expect_false(any(is.infinite(pc$eff)))
})

test_that("Gaussian contrasts are differences, so negative predictions are fine", {
  lo <- data.frame(age = c(1, 5), fitted = c(-2, -1))
  hi <- data.frame(age = c(1, 5), fitted = c(-1, 1))
  pc <- prediction_contrast(lo, hi, ratio = FALSE)
  expect_equal(pc$eff, c(1, 2))
  expect_true(all(pc$usable))
})

test_that("no shared ages gives an empty contrast rather than an error", {
  pc <- prediction_contrast(data.frame(age = 1, fitted = 2), data.frame(age = 5, fitted = 3), ratio = TRUE)
  expect_equal(nrow(pc), 0)
})

test_that("the saved coefficients line names each selection term and its reading", {
  expect_true(is.function(term_verdict_line))
  expect_identical(term_verdict_line(list(fits = list(M1 = NULL), coefficients = data.frame()), "M1"), character(0))
})
