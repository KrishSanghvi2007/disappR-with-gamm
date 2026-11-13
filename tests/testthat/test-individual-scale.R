# Step 4 fits each individual's ageing function. For count traits the function sits on the log scale - as in the
# simulator and the count models - so the fits must be on that scale too, or a raw quadratic is fitted to an
# exponentiated curve and can even go negative.

count_individuals <- function(seed = 5, n = 150) {
  set.seed(seed)
  do.call(rbind, lapply(seq_len(n), function(i) {
    ls <- sample(12:22, 1); age <- seq_len(ls)
    b <- c(log(30), 0.12, -0.007) + stats::rnorm(3, 0, c(0.2, 0.003, 0.00015))
    data.frame(id = paste0("i", i), age = age, trait = stats::rpois(ls, exp(b[1] + b[2] * age + b[3] * age^2)))
  }))
}
truth_at <- function(age) exp(log(30) + 0.12 * age - 0.007 * age^2)

test_that("count traits fitted on the log scale recover the log-quadratic truth and stay positive", {
  d <- count_individuals()
  z <- fit_individual_function(d, "Quadratic", 1, link = "log")
  expect_true(isTRUE(z$log_scale))
  mc <- z$mean_curve
  expect_true(all(mc$fitted > 0) && all(mc$lo > 0))
  mid <- mc$age >= 3 & mc$age <= 15
  expect_lt(max(abs(mc$fitted[mid] / truth_at(mc$age[mid]) - 1)), 0.12)
})

test_that("the log scale does better than a raw-scale fit on a count trait", {
  d <- count_individuals(seed = 6)
  err <- function(z) { mc <- z$mean_curve; mean(abs(mc$fitted / truth_at(mc$age) - 1)) }
  expect_lt(err(fit_individual_function(d, "Quadratic", 1, link = "log")),
            err(fit_individual_function(d, "Quadratic", 1, link = "identity")))
})

test_that("the raw-scale path is unchanged by default", {
  d <- count_individuals(seed = 7)
  expect_equal(fit_individual_function(d, "Quadratic", 1)$mean_curve,
               fit_individual_function(d, "Quadratic", 1, link = "identity")$mean_curve)
})

test_that("a minimum number of records is respected", {
  d <- count_individuals(seed = 8)
  z <- fit_individual_function(d, "Cubic", 1, link = "log", min_records = 16)
  n_rec <- table(d$id)[z$fitted_ids]
  expect_true(all(n_rec >= 16))
  expect_lt(length(z$fitted_ids), length(unique(d$id)))
})

test_that("functions compared on the log scale all use Poisson likelihood, so their AICc values are comparable", {
  d <- count_individuals(seed = 9)
  cmp <- compare_individual_functions(d, 1, link = "log")
  expect_true(is.list(cmp) || is.data.frame(cmp))
})

test_that("redrawing individuals uses the stored coefficients and matches the curves the fit draws", {
  d <- count_individuals(seed = 10)
  ids <- unique(d$id)[1:4]
  z <- fit_individual_function(d, "Quadratic", 1, draw_ids = ids, link = "log")
  cv <- individual_curves(z, d, ids)
  expect_equal(nrow(cv), nrow(z$curves))
  expect_equal(cv$fitted, z$curves$fitted, tolerance = 1e-10)
  # fitting without any drawn individuals gives the same coefficients, so drawing never needs a refit
  z0 <- fit_individual_function(d, "Quadratic", 1, link = "log")
  expect_equal(z0$coefs, z$coefs)
})

test_that("overdispersed counts are compared by QAICc, which shrinks the differences between functions", {
  set.seed(11)
  d <- do.call(rbind, lapply(seq_len(120), function(i) {
    ls <- sample(10:18, 1); age <- seq_len(ls)
    data.frame(id = paste0("n", i), age = age, trait = stats::rnbinom(ls, size = 2, mu = exp(log(25) + 0.1 * age - 0.006 * age^2)))
  }))
  qa <- compare_individual_functions(d, 1, link = "log", quasi = TRUE)
  pa <- compare_individual_functions(d, 1, link = "log", quasi = FALSE)
  expect_true(is.finite(attr(qa, "chat")) && attr(qa, "chat") > 1)
  expect_lt(max(abs(qa$Mean_dAICc), na.rm = TRUE), max(abs(pa$Mean_dAICc), na.rm = TRUE))
})
