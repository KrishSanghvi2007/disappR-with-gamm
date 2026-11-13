# 0.24.4: no conflicts between the trait-specific box, mapped AFR/ALR columns and the AFE start of the missingness
# window, and the chosen AFR/ALR reach the models. One individual (P) is recorded at ages 1-6 but has trait values at
# ages 2-4 only; mapped columns give AFR 1 and ALR 5 (a sighting without the trait).

conflict_data <- function() data.frame(
  id = rep(c("P", "Q"), c(6, 4)), age = c(1:6, 1:4),
  y = c(NA, 5, 6, 7, NA, NA, 1, 2, 3, 4),
  AFRc = c(rep(1, 6), rep(1, 4)), ALRc = c(rep(5, 6), rep(4, 4)), stringsAsFactors = FALSE)
conflict_map <- function(afr_col, alr_col, ticked) list(
  id = "id", age = "age", trait = "y", alr = if (alr_col) "ALRc" else "__AUTO_LAST__", life = "",
  entry = if (afr_col) "AFRc" else "__AUTO_FIRST__", condition = "", covars = character(0), cov_factor = character(0),
  cov_int = character(0), group = "", nested = TRUE, random = character(0), censor = "", censor_value = "",
  start_mode = "afr", start_age = NA_real_, age_round = NA_real_, cov_age = character(0), trait_specific_ages = ticked)
# the rule: a mapped column wins; otherwise ticked = ages with a trait value (P: 2-4), unticked = any record (P: 1-6)
expected_afr <- function(afr_col, ticked) if (afr_col) 1 else if (ticked) 2 else 1
expected_alr <- function(alr_col, ticked) if (alr_col) 5 else if (ticked) 4 else 6

test_that("every combination of mapped columns and the box gives the expected AFR and ALR, in the data and the models", {
  for (afr_col in c(FALSE, TRUE)) for (alr_col in c(FALSE, TRUE)) for (ticked in c(FALSE, TRUE)) {
    lab <- sprintf("AFR column %s, ALR column %s, ticked %s", afr_col, alr_col, ticked)
    b <- standardise_data(conflict_data(), conflict_map(afr_col, alr_col, ticked))
    p <- b$data[b$data$id == "P", ]
    expect_equal(unique(p$entry), expected_afr(afr_col, ticked), info = lab)
    expect_equal(unique(p$alr), expected_alr(alr_col, ticked), info = lab)
    # the models' ALR and AFR proxies (unstandardised) are the same values
    pm <- prepare_model_data(b$data, "Linear", standardise = FALSE)$data
    expect_equal(unique(pm$ALR_raw[pm$id == "P"]), expected_alr(alr_col, ticked), info = lab)
    expect_equal(unique(pm$AFR_raw[pm$id == "P"]), expected_afr(afr_col, ticked), info = lab)
    # the missingness window: AFR mode opens at the AFR and closes at the ALR (never excluding a trait record)
    g <- build_missing_grid(b$data, 0, "afr", NA_real_)
    gp <- g$age[g$id == "P"]
    expect_equal(min(gp), min(expected_afr(afr_col, ticked), 2), info = lab)
    expect_equal(max(gp), max(expected_alr(alr_col, ticked), 4), info = lab)
    # AFE mode: the window opens at the earlier of the AFE and the AFR
    for (afe in c(1, 3)) {
      ga <- build_missing_grid(b$data, 0, "same", afe)
      expect_equal(min(ga$age[ga$id == "P"]), min(afe, expected_afr(afr_col, ticked), 2), info = paste(lab, "AFE", afe))
    }
  }
})

test_that("the box changes the fitted models only through AFR and ALR", {
  d <- conflict_data()
  d <- rbind(d, transform(d, id = paste0(id, "b")), transform(d, id = paste0(id, "c")))   # enough individuals to fit
  d$y[!is.na(d$y)] <- d$y[!is.na(d$y)] + seq_len(sum(!is.na(d$y))) %% 3
  fit <- function(ticked) {
    b <- standardise_data(d, conflict_map(FALSE, FALSE, ticked))
    fit_model_suite(b$data, b$meta, models = c("M1", "M2"), age_function = "Linear")
  }
  r_t <- fit(TRUE); r_u <- fit(FALSE)
  a1 <- function(r) r$aic$AIC[r$aic$Model == "Model 1"]
  expect_equal(a1(r_t), a1(r_u), tolerance = 1e-6)          # Model 1 has no ALR term: identical either way
})
