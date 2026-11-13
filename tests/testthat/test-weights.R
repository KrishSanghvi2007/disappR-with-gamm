# Weighting of the gap figure: inverse-variance weights since 0.20.4 (log2 N before). log_n_weight() is kept as a helper.

test_that("log_n_weight() is log2 of the pooled sample size and drops unusable values", {
  expect_equal(log_n_weight(c(8, 64)), c(3, 6))
  expect_true(all(is.na(log_n_weight(c(1, 0, NA, -2)))))
})

test_that("any base of logarithm gives the same weighted trend", {
  set.seed(3)
  x <- 1:12; y <- 0.3 * x + stats::rnorm(12); n <- round(seq(400, 20, length.out = 12))
  b2 <- stats::coef(stats::lm(y ~ x, weights = log2(n)))
  b10 <- stats::coef(stats::lm(y ~ x, weights = log10(n)))
  expect_equal(b2, b10)
})

test_that("bin differences carry inverse-variance weights, and weighting changes the trend", {
  s <- data.frame(facet = "All", bin = factor(rep(c("1: short", "2: long"), each = 6), levels = c("1: short", "2: long")),
                  age = rep(1:6, 2), mean = c(10, 9.8, 9.5, 9.3, 9.0, 8.4, 10.5, 10.4, 10.3, 10.1, 10.0, 11.5),
                  n = c(60, 55, 40, 25, 10, 4, 60, 58, 50, 40, 25, 6),
                  se = c(0.10, 0.12, 0.15, 0.2, 0.35, 0.9, 0.11, 0.12, 0.13, 0.16, 0.22, 0.8))
  dz <- bin_differences(s, "successive")
  expect_equal(dz$n, s$n[1:6] + s$n[7:12])
  expect_equal(dz$var, s$se[1:6]^2 + s$se[7:12]^2)
  expect_equal(dz$w, 1 / (s$se[1:6]^2 + s$se[7:12]^2))
  un <- bin_difference_trends(dz, weighted = FALSE)$Slope_per_age
  wt <- bin_difference_trends(dz, weighted = TRUE)$Slope_per_age
  expect_false(isTRUE(all.equal(un, wt)))
  expect_equal(wt, unname(stats::coef(stats::lm(difference ~ age, data = dz, weights = 1 / var))[2]))
  band <- lm_band(dz$age, dz$difference, w = dz$w)
  expect_true(nrow(band) > 0 && all(is.finite(band$fit)))
})

test_that("a bin with no spread takes the pooled within-bin variance instead of an infinite weight", {
  s <- data.frame(facet = "All", bin = factor(rep(c("1", "2"), each = 3), levels = c("1", "2")), age = rep(1:3, 2),
                  mean = c(0, 1, 2, 0, 1.5, 3), n = c(10, 10, 10, 10, 10, 10), se = c(0, 0.2, 0.3, 0.1, 0.2, 0.3))
  dz <- bin_differences(s, "successive")
  pooled <- sum((s$se^2 * s$n)[s$se > 0] * (s$n[s$se > 0] - 1)) / sum(s$n[s$se > 0] - 1)
  expect_true(all(is.finite(dz$w)))
  expect_equal(dz$var[[1]], pooled / 10 + 0.1^2)
})

test_that("the display marks the last observed record and never draws a death marker", {
  grid <- data.frame(id = c(rep("A", 3), rep("B", 3)), k = rep(0:2, 2), age = rep(1:3, 2), step = 1, anchor = 1,
                     missing = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE))
  x <- grid_display(grid, c("A", "B"), life_known = TRUE)
  expect_identical(x$status[x$id == "A" & x$age == 3], "Last record (ALR)")
  expect_identical(x$status[x$id == "A" & x$age == 2], "Missed")
  expect_false(any(grepl("Death", x$status)))
})

test_that("legend labels wrap onto several lines", {
  expect_identical(wrap_label("Mean of coefficients (typical individual)", 20), "Mean of coefficients\n(typical individual)")
})

test_that("values are clamped near the visible window before drawing", {
  expect_identical(squish_to(c(-1e300, 0, 5, 1e304, NA), c(0, 10)), c(-5, 0, 5, 15, NA))
  expect_identical(squish_to(1:3, c(NA, 1)), 1:3)                 # no window: unchanged
})
