# The effect-size and permutation functions have to be RUN to be tested. Static checks passed both, yet
# permutation_test() failed on every call in 0.13.0 to 0.16.1: a per-permutation vector sat on one side of &&,
# which R >= 4.3 rejects ("length = 20 in coercion to logical(1)"). These tests execute both functions end to
# end on a small simulated dataset, so that class of error fails here rather than in front of a user.

effects_data <- function(seed = 7, n = 60) {
  set.seed(seed)
  do.call(rbind, lapply(seq_len(n), function(i) {
    ls <- sample(4:10, 1)
    q <- (ls - 7) / 2
    age <- seq_len(ls)
    data.frame(id = paste0("i", i), age = age, LS = ls,
               trait = 10 + 0.5 * q - 0.2 * age + 0.08 * q * age + stats::rnorm(ls, 0, 0.6))
  }))
}
effects_map <- function() {
  list(id = "id", age = "age", trait = "trait", alr = "__AUTO_LAST__", life = "LS",
       entry = "__AUTO_FIRST__", covars = character(0), cov_factor = character(0),
       cov_int = character(0), group = "", nested = TRUE, random = character(0),
       censor = "", censor_value = "", condition = "", trials = "", start_mode = "afr",
       start_age = NA_real_, age_round = NA_real_, cov_age = character(0))
}

test_that("permutation_test() runs end to end and returns a valid p-value", {
  skip_if_not_installed("lme4")
  b <- standardise_data(effects_data(), effects_map())
  z <- permutation_test(b$data, b$meta, model = "M4", against = "M1", n_perm = 5,
                        settings = list(family = "gaussian", age_function = "Linear"))
  expect_type(z, "list")
  expect_equal(z$n_perm, 5)
  expect_true(z$n_ok >= 1)
  expect_true(is.finite(z$observed_gain))
  expect_true(is.finite(z$p_gain) && z$p_gain > 0 && z$p_gain <= 1)
  # the smallest attainable p-value with n permutations is 1 / (n + 1)
  expect_gte(z$p_gain, 1 / (5 + 1))
})

test_that("permutation_test() actually changes the proxy it permutes", {
  skip_if_not_installed("lme4")
  b <- standardise_data(effects_data(), effects_map())
  # a null identical to the observed data would give a gain equal to the observed one on every run
  z <- permutation_test(b$data, b$meta, model = "M4", against = "M1", n_perm = 8,
                        settings = list(family = "gaussian", age_function = "Linear"))
  expect_false(all(abs(z$null_gain - z$observed_gain) < 1e-6))
})

test_that("permutation_test() is reproducible and leaves the session's random stream untouched", {
  skip_if_not_installed("lme4")
  b <- standardise_data(effects_data(), effects_map())
  set.seed(99); before <- .Random.seed
  z1 <- permutation_test(b$data, b$meta, model = "M4", against = "M1", n_perm = 5,
                         settings = list(family = "gaussian", age_function = "Linear"), seed = 3)
  expect_identical(.Random.seed, before)
  z2 <- permutation_test(b$data, b$meta, model = "M4", against = "M1", n_perm = 5,
                         settings = list(family = "gaussian", age_function = "Linear"), seed = 3)
  expect_equal(z1$null_gain, z2$null_gain)
  expect_equal(z1$p_gain, z2$p_gain)
})

test_that("selection_effect_sizes() runs and returns finite estimates", {
  skip_if_not_installed("lme4")
  b <- standardise_data(effects_data(), effects_map())
  r <- fit_model_suite(b$data, b$meta, models = c("M1", "M2", "M4"), age_function = "Linear", family = "gaussian")
  e <- selection_effect_sizes(r, b$data)
  expect_s3_class(e, "data.frame")
  expect_true(nrow(e) >= 1)
  expect_true(all(is.finite(e$Estimate)))
  expect_true(all(e$Lower <= e$Estimate & e$Estimate <= e$Upper))
})

test_that("the mean-age models are refused, since shuffling lifespan cannot reach them", {
  skip_if_not_installed("lme4")
  b <- standardise_data(effects_data(), effects_map())
  # Models 3 and 5 use each individual's mean age, computed from its own records. Shuffling lifespan leaves it
  # unchanged, so every permutation would reproduce the observed fit and the test would always report p = 1.
  for (m in c("M3", "M5"))
    expect_error(permutation_test(b$data, b$meta, model = m, against = "M1", n_perm = 3,
                                  settings = list(family = "gaussian", age_function = "Linear")),
                 "cannot be tested this way")
  expect_false(any(c("M1", "M3", "M5") %in% PERMUTABLE_MODELS))
})

test_that("permutation_reading() gives the right reading for each pattern", {
  z <- function(obs, null, p, model = "M4") list(model = model, against = "M1", observed_gain = obs,
                                                  null_gain = null, p_gain = p, n_perm = length(null), n_ok = length(null))
  # the advantage depends on the real lifespans
  expect_identical(permutation_reading(z(300, c(-2, 1, 3, 0, 5), 1 / 6))$pattern, "partial")
  expect_identical(permutation_reading(z(300, rep(c(-2, 1, 3, 0, 5), 5), 1 / 26))$pattern, "drops")
  # shuffled lifespans do just as well: flexibility, not selection
  expect_identical(permutation_reading(z(20, c(15, 25, 30, 18, 40), 0.6))$pattern, "no_drop")
  # nothing to explain
  expect_identical(permutation_reading(z(1.5, c(0, 2, 1), 0.5))$pattern, "no_advantage")
  # an interaction model points to heterogeneity in ageing when the advantage does not drop
  rd <- permutation_reading(z(20, c(15, 25, 30, 18, 40), 0.6, model = "M4"))
  expect_match(paste(rd$alternatives, collapse = " "), "random slopes")
  # a 'drops' reading always names the two explanations the shuffle cannot rule out
  rd <- permutation_reading(z(300, rep(c(-2, 1, 3, 0, 5), 5), 1 / 26))
  expect_match(paste(rd$alternatives, collapse = " "), "misspecified ageing function")
  expect_match(paste(rd$alternatives, collapse = " "), "before death")
  expect_null(permutation_reading(NULL))
})
