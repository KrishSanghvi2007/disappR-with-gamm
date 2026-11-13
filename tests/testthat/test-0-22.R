# Tests for the 0.22.0 additions: a sampling interval set by the user, the new families and the age-dependent
# variance, stored-model comparison sets, the share of individual-level wins, peak and onset ages, the random-effect
# covariance with lifespan proxies and the auto-written methods.

map022 <- function() list(id = "id", age = "age", trait = "trait", alr = "__AUTO_LAST__", life = "", entry = "__AUTO_FIRST__",
                          covars = character(0), cov_factor = character(0), cov_int = character(0), group = "", nested = TRUE,
                          random = character(0), censor = "", censor_value = "", condition = "", trials = "", start_mode = "afr",
                          start_age = NA_real_, age_round = NA_real_, cov_age = character(0))

# annual sampling in which a fifth of the individuals also have one mid-year record: the inferred interval is 0.5
half_year_data <- function(seed = 2, n = 300) {
  set.seed(seed)
  do.call(rbind, lapply(seq_len(n), function(i) {
    L <- sample(3:9, 1)
    a <- as.numeric(seq_len(L))
    if (stats::runif(1) < 0.2) a <- sort(c(a, sample(seq_len(L - 1), 1) + 0.5))
    data.frame(id = sprintf("i%03d", i), age = a, trait = 10 - 0.3 * a + stats::rnorm(length(a)), stringsAsFactors = FALSE)
  }))
}

test_that("a sampling interval set by the user replaces the inferred one", {
  expect_true(valid_age_step(1))
  expect_false(valid_age_step(NULL)); expect_false(valid_age_step(NA_real_)); expect_false(valid_age_step(0)); expect_false(valid_age_step(c(1, 2)))
  d <- half_year_data()
  expect_equal(resolve_age_step(d$age, d$id), infer_age_step(d$age, d$id))
  expect_equal(resolve_age_step(d$age, d$id, 1), 1)
  b <- standardise_data(d, map022())
  auto <- decomposition_trajectory(b$data)
  set1 <- decomposition_trajectory(b$data, step = 1)
  expect_equal(infer_age_step(b$data$age, b$data$id), 0.5)
  # with the annual interval the annual links are formed, so far more individuals contribute
  expect_gt(max(set1$n_pairs, na.rm = TRUE), 5 * max(auto$n_pairs, na.rm = TRUE))
  g_auto <- build_missing_grid(b$data)
  g_set <- build_missing_grid(b$data, step = 1)
  expect_equal(g_set$step[[1]], 1)
  expect_lt(mean(g_set$missing), mean(g_auto$missing))
  m <- b$meta; m$age_step <- 1
  expect_equal(life_table(b$data, m)$step, 1)
  # an interval set to the inferred value changes nothing
  expect_equal(decomposition_trajectory(b$data, step = 0.5), auto, ignore_attr = TRUE)
  # an offset first occasion (day 1, then weekly from day 7) keeps its real ages when an interval is set
  bee <- data.frame(id = rep(sprintf("b%02d", 1:40), each = 5), age = rep(c(1, 7, 14, 21, 28), 40))
  set.seed(1); bee$trait <- 5 + 0.1 * bee$age + stats::rnorm(nrow(bee))
  bb <- standardise_data(bee, map022())
  expect_true(all(round(decomposition_trajectory(bb$data, step = 14)$age, 6) %in% c(1, 7, 14, 21, 28)))
})

test_that("the variance-model helpers and family classes are consistent", {
  expect_identical(disp_formula_string("constant", c("f1", "f2")), "~1")
  expect_identical(disp_formula_string("age", c("f1", "f2")), "~ f1")
  expect_identical(disp_formula_string("age_terms", c("f1", "f2")), "~ f1 + f2")
  expect_identical(normalise_disp("nonsense"), "constant")
  expect_identical(family_density_class("gamma"), "continuous")
  expect_identical(family_density_class("nbinom2"), "discrete")
  expect_identical(family_link("beta"), "logit")
  expect_true(all(c("gamma", "lognormal", "beta") %in% MODEL_FAMILIES))
})

test_that("new families and an age-dependent variance fit, and unsuitable values are refused", {
  skip_if_not_installed("glmmTMB")
  t <- toy_prepared(n_id = 60, seed = 3)
  d <- t$prep$data
  r0 <- fit_model_suite(d, t$prep$meta, models = c("M1", "M2"), age_function = "Quadratic")
  rv <- fit_model_suite(d, t$prep$meta, models = c("M1", "M2"), age_function = "Quadratic", disp = "age")
  expect_true(isTRUE(rv$ok))
  expect_true(inherits(rv$fits$M1, "glmmTMB"))
  expect_identical(rv$disp_formula, "~ f1")
  # same records, so the constant and the age-dependent variance can be compared by AIC
  expect_identical(rows_key(r0$data), rows_key(rv$data))
  rg <- fit_model_suite(d, t$prep$meta, models = "M1", age_function = "Quadratic", family = "gamma")
  expect_true(isTRUE(rg$ok))
  d_bad <- d; d_bad$trait[1] <- 0
  expect_false(isTRUE(fit_model_suite(d_bad, t$prep$meta, models = "M1", family = "lognormal")$ok))
  expect_false(isTRUE(fit_model_suite(d, t$prep$meta, models = "M1", family = "beta")$ok))
  # Poisson has no dispersion parameter: the option is ignored and said so
  tc <- toy_prepared(n_id = 60, seed = 3, trait = "count_pois")
  rp <- fit_model_suite(tc$prep$data, tc$prep$meta, models = "M1", family = "poisson", disp = "age")
  expect_identical(rp$disp, "constant")
  expect_true(any(grepl("no dispersion parameter", rp$drop_by)))
})

test_that("the null-model bootstrap carries the variance model of the analysis", {
  skip_if_not_installed("glmmTMB")
  skip_on_cran()
  t <- toy_prepared(n_id = 50, seed = 4)
  z <- bootstrap_test(t$prep$data, t$prep$meta, model = "M2", n_boot = 3,
                      settings = list(age_function = "Linear", disp = "age"))
  expect_identical(z$null_model$disp, "age")
  expect_gte(z$n_ok, 1)
  z0 <- bootstrap_test(t$prep$data, t$prep$meta, model = "M2", n_boot = 2, settings = list(age_function = "Linear"))
  expect_identical(z0$null_model$disp, "constant")
  expect_identical(family_label("betabinomial"), "beta-binomial (logit link)")
})

test_that("stored models are compared only within sets sharing data, rows and likelihood type", {
  mk <- function(id, sig, rows, cls, aic) list(id = id, label = "Model 1", spec = "x", N = 100, df = 4, AIC = aic, fit = "Valid",
                                             data_sig = sig, rows = rows, family_class = cls)
  st <- list(mk("S1", "a", "r1", "continuous", 100), mk("S2", "a", "r1", "continuous", 95),
             mk("S3", "a", "r2", "continuous", 50), mk("S4", "a", "r1", "discrete", 10))
  tab <- stored_comparison_table(st)
  expect_equal(sort(unique(tab$Set)), c("A", "B", "C"))
  expect_false("Weight" %in% names(tab))
  expect_equal(tab$dAIC[tab$ID == "S1"], 5)
  expect_equal(tab$dAIC[tab$ID == "S3"], 0)
  expect_length(stored_set_notes(st), 2)
})

test_that("the individual-level comparison reports each function's share of wins", {
  t <- toy_prepared(n_id = 60, seed = 5)
  tab <- compare_individual_functions(t$prep$data)
  expect_true("Share_best" %in% names(tab))
  s <- tab$Share_best[is.finite(tab$Share_best)]
  expect_true(all(s >= 0 & s <= 100))
  expect_lt(abs(sum(s) - 100), 0.5)   # ties are split, so the shares sum to 100 (to rounding)
})

test_that("peak and onset are read from the predicted population curve", {
  a <- seq(0, 10, length.out = 101)
  tp <- curve_turning_points(a, -(a - 4)^2)
  expect_true(tp$peak_interior); expect_equal(tp$peak, 4); expect_equal(tp$onset, 4)
  expect_identical(curve_turning_points(a, -a)$onset_type, "from_start")
  expect_identical(curve_turning_points(a, a)$onset_type, "no_final_decline")
  t <- toy_prepared(n_id = 80, seed = 7)
  r <- fit_model_suite(t$prep$data, t$prep$meta, models = c("M1", "M4"), age_function = "Quadratic")
  z <- peak_onset_summary(r, "M1", n_draws = 200)
  expect_true(isTRUE(z$ok))
  expect_true(is.character(peak_onset_sentence(z)))
  if (isTRUE(z$turning$peak_interior) && all(is.finite(z$ci$peak))) {
    expect_lte(z$ci$peak[[1]], z$turning$peak + 1e-8)
    expect_gte(z$ci$peak[[2]], z$turning$peak - 1e-8)
  }
})

test_that("random effects are correlated with the lifespan proxies", {
  t <- toy_prepared(n_id = 80, seed = 9, sd_type = "independent")
  r <- fit_model_suite(t$prep$data, t$prep$meta, models = c("M1", "M2"), age_function = "Quadratic")
  z <- blup_proxy_covariance(r, "M1")
  expect_true(isTRUE(z$ok))
  expect_true(all(c("ALR", "LS") %in% z$table$Proxy))
  # level-linked selective disappearance: the M1 intercepts rise with lifespan
  expect_gt(z$table$r[z$table$Proxy == "LS" & z$table$Component == "Level (intercept)"][[1]], 0)
  expect_true("ALR" %in% blup_proxy_covariance(r, "M2")$in_model)
})

test_that("the methods draft describes the selected results and flags conflicting settings", {
  mods <- function(fam) list(family = fam, age_function = "Quadratic", random = "(1 | ID)", among = "linear", standardise = TRUE,
                             disp = "constant", zi = "", models = c("M1", "M4"), n_rows = 500, n_ind = 60, decomposition = TRUE, lmertest = TRUE)
  dat <- list(source = "toy", label = "a simulation", trait = "mass", n_rows = 500, n_ind = 60, n_trait = 500, age_min = 1, age_max = 20,
              step = 1, step_manual = FALSE)
  e1 <- list(title = "Model comparison", section = "5 Modelling", data = "sim", methods = list(data = dat, models = mods("gaussian"), version = "x"))
  e2 <- list(title = "Coefficients of Model 4", section = "5 Modelling", data = "sim", methods = list(data = dat, models = mods("gamma"), version = "x"))
  z <- methods_text(list(e1))
  expect_true(length(z$paragraphs) >= 2)
  expect_length(z$conflicts, 0)
  expect_true(any(grepl("van de Pol", z$references)))
  z2 <- methods_text(list(e1, e2))
  expect_true(any(grepl("error family", z2$conflicts)))
})
