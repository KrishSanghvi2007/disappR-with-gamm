# The null-model bootstrap replaces the permutation test in the app. Shuffling lifespans between individuals creates
# records after an individual's last record or death, so anything tied to the observation window beat every shuffle.
# The bootstrap simulates the trait from a model with no lifespan term, keeping every individual's real ages.

boot_data <- function(seed = 7, n = 50) {
  set.seed(seed)
  raw <- do.call(rbind, lapply(seq_len(n), function(i) {
    q <- stats::rnorm(1); ls <- max(4, round(9 + 2 * q)); age <- seq_len(ls)
    data.frame(id = paste0("i", i), age = age, LS = ls,
               trait = 20 + 1.2 * q - (0.4 + 0.08 * q) * age + stats::rnorm(ls, 0, 0.7))
  }))
  standardise_data(raw, list(id = "id", age = "age", trait = "trait", alr = "__AUTO_LAST__", life = "LS",
                             entry = "__AUTO_FIRST__", covars = character(0), cov_factor = character(0),
                             cov_int = character(0), group = "", nested = TRUE, random = character(0), censor = "",
                             censor_value = "", condition = "", trials = "", start_mode = "afr", start_age = NA_real_,
                             age_round = NA_real_, cov_age = character(0)))
}

test_that("every model with a lifespan or mean-age term can be tested, including Models 3 and 5", {
  expect_true(all(c("M2", "M3", "M4", "M5") %in% BOOTSTRAP_MODELS))
  expect_false("M1" %in% BOOTSTRAP_MODELS)
  b <- boot_data()
  expect_error(bootstrap_test(b$data, b$meta, model = "M1", n_boot = 1), "no lifespan")
})

test_that("the bootstrap runs, reports its null model and gives a reading", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  b <- boot_data()
  z <- bootstrap_test(b$data, b$meta, model = "M4", n_boot = 3, settings = list(age_function = "Linear"))
  expect_identical(z$method, "bootstrap")
  expect_true(z$n_ok >= 1 && is.finite(z$observed_gain))
  expect_identical(z$null_model$age_function, "Cubic")
  expect_false(is.null(bootstrap_reading(z)))
})

test_that("the bootstrap leaves the session's random numbers as they were", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  b <- boot_data()
  set.seed(5); before <- stats::runif(1)
  set.seed(5); invisible(bootstrap_test(b$data, b$meta, model = "M2", n_boot = 2, settings = list(age_function = "Linear")))
  expect_identical(stats::runif(1), before)
})

test_that("the reading follows the observed advantage against the null draws", {
  z <- function(obs, null) list(model = "M4", against = "M1", observed_gain = obs, null_gain = null,
                                p_gain = (1 + sum(null >= obs)) / (1 + length(null)))
  expect_identical(bootstrap_reading(z(50, seq(0, 10, length.out = 25)))$pattern, "drops")
  expect_identical(bootstrap_reading(z(1, seq(0, 10, length.out = 25)))$pattern, "no_advantage")
  expect_identical(bootstrap_reading(z(4, seq(0, 10, length.out = 25)))$pattern, "no_drop")
})


test_that("fewer than 19 refitted datasets are reported as too few, whatever the p-value", {
  z <- list(model = "M4", against = "M1", observed_gain = 50, null_gain = seq(0, 10, length.out = 12), n_ok = 12L,
            p_gain = 1 / 13)
  expect_identical(bootstrap_reading(z)$pattern, "too_few")
  expect_identical(formals(bootstrap_test)$n_boot, 39)
})

test_that("the test records the settings of its own observed fit, as the model comparison does", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  b <- boot_data()
  z <- bootstrap_test(b$data, b$meta, model = "M4", n_boot = 2, settings = list(age_function = "Linear", among = "same"))
  r <- fit_model_suite(b$data, b$meta, models = c("M1", "M4"), age_function = "Linear", among = "same")
  expect_identical(z$settings_key, evidence_settings_key(r))   # "linear" among, as the fit records it
})
