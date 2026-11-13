# 'Higher-order among term': 'linear'; 'same' (powers of the lifespan proxies and of mean age); 'consistent' (powers
# of the proxies, and the mean of each age term, which pairs exactly with the within-individual age terms). A reviewer
# found the fit recorded 'consistent' as 'linear', so the exported script, the provenance and the model descriptions
# all described a different model. These tests pin every place that must follow the option.

among_data <- function(seed = 3, n = 80) {
  set.seed(seed)
  do.call(rbind, lapply(seq_len(n), function(i) {
    ls <- sample(5:12, 1); age <- seq_len(ls); q <- (ls - 8) / 2
    data.frame(id = paste0("i", i), age = age, LS = ls,
               trait = 20 + 0.8 * q + 1.2 * age - 0.08 * age^2 + stats::rnorm(ls, 0, 0.8))
  }))
}
among_map <- function() {
  list(id = "id", age = "age", trait = "trait", alr = "__AUTO_LAST__", life = "LS", entry = "__AUTO_FIRST__",
       covars = character(0), cov_factor = character(0), cov_int = character(0), group = "", nested = TRUE,
       random = character(0), censor = "", censor_value = "", condition = "", trials = "", start_mode = "afr",
       start_age = NA_real_, age_round = NA_real_, cov_age = character(0))
}

test_that("the fit records the option that was fitted", {
  skip_if_not_installed("lme4")
  b <- standardise_data(among_data(), among_map())
  for (opt in c("linear", "same", "consistent")) {
    r <- fit_model_suite(b$data, b$meta, models = c("M1", "M2", "M3"), age_function = "Quadratic", among = opt)
    expect_identical(r$among, opt)
  }
  # with a linear ageing function the three options are the same model, recorded as linear
  r <- fit_model_suite(b$data, b$meta, models = c("M1", "M2"), age_function = "Linear", among = "consistent")
  expect_identical(r$among, "linear")
})

test_that("both export styles reproduce the option that was fitted", {
  skip_if_not_installed("lme4")
  b <- standardise_data(among_data(), among_map())
  con <- fit_model_suite(b$data, b$meta, models = c("M1", "M2", "M3"), age_function = "Quadratic", among = "consistent")
  expect_match(paste(engine_script_lines(con, b$meta, "data.csv"), collapse = "\n"), 'among = "consistent"', fixed = TRUE)
  script <- model_r_code(con, b$meta, "data.csv")
  expect_match(script, "dat[[paste0(v, 2)]] <- dat[[v]]^2", fixed = TRUE)       # the squared lifespan terms it needs
  same <- fit_model_suite(b$data, b$meta, models = c("M1", "M3"), age_function = "Quadratic", among = "same")
  expect_match(model_r_code(same, b$meta, "data.csv"), "dat$mean_f1_2 <- dat$mean_f1^2", fixed = TRUE)
})

test_that("Models 3 and 5 can be predicted under 'same polynomial order'", {
  skip_if_not_installed("lme4")
  b <- standardise_data(among_data(), among_map())
  r <- fit_model_suite(b$data, b$meta, models = c("M1", "M3"), age_function = "Quadratic", among = "same")
  fit3 <- r$fits[["M3"]]
  skip_if(is.null(fit3), "Model 3 did not fit on this sample")
  pc <- predict_population_curve(fit3, r, c(2, 4, 6))
  expect_true(nrow(pc) > 0 && all(is.finite(pc$fitted)))
})

test_that("extra terms added to a model follow the chosen option", {
  fs <- list(M1 = "f1 + f2")
  b <- c("f1", "f2")
  lin <- apply_extra_terms(fs, b, extra = list(M1 = c("ALR", "mean_age")), among = "linear")$M1
  con <- apply_extra_terms(fs, b, extra = list(M1 = c("ALR", "mean_age")), among = "consistent")$M1
  sam <- apply_extra_terms(fs, b, extra = list(M1 = c("ALR", "mean_age")), among = "same")$M1
  expect_false(grepl("ALR2", lin))
  expect_true(grepl("ALR2", con) && grepl("mean_f2", con) && !grepl("mean_f1_2", con))
  expect_true(grepl("ALR2", sam) && grepl("mean_f1_2", sam))
})

test_that("the model descriptions follow the chosen option", {
  row <- function(tab, m) tab$Fixed_effects[tab$Model == model_label(m)]
  con <- model_definition_table("Quadratic", among = "consistent")
  sam <- model_definition_table("Quadratic", among = "same")
  lin <- model_definition_table("Quadratic", among = "linear")
  expect_match(row(con, "M2"), "ALR\u00b2")
  expect_match(row(con, "M3"), "mean(age\u00b2)", fixed = TRUE)          # the mean of each age term
  expect_match(row(sam, "M3"), "mean(age)\u00b2", fixed = TRUE)          # powers of mean age
  expect_false(grepl("ALR\u00b2", row(lin, "M2"), fixed = TRUE))   # age\u00b2 is there; ALR\u00b2 must not be
})
