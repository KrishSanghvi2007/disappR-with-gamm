# The evidence summary grades what the saved results show. These tests pin the grading rules on constructed
# evidence, and check the two statistics that read the data directly against simulated cases where the answer is
# known. The rules were calibrated on simulated data before release; see NEWS for the scenarios.

entry <- function(ev) list(evidence = ev)
mv <- function(kind, slope = "none", p_level = 0.001, p_int = 0.001, shape_ok = TRUE)
  list(type = "models", kind = kind, supported = "Model 4", best = "Model 4", gap_m1 = 50, p_level = p_level,
       p_interaction = p_int, age_slope = -0.2, level_effect = 0.5, interaction = 0.1, random_slope = slope,
       age_function = "Linear", shape_ok = shape_ok, shape_supported = "Linear", appearance = FALSE)

test_that("every check saved and passing gives strong evidence, worded cautiously and neutrally", {
  g <- grade_evidence(list(
    entry(list(type = "visual", kind = "age_dependent", slope = 0.02, mean_gap = 0.1)),
    entry(mv("age_dependent")), entry(mv("age_dependent", slope = "uncorrelated")),
    entry(list(type = "permutation", pattern = "drops", model = "M4")),
    entry(list(type = "missingness", trait_p = 0.6, percent = 5))), trait = "body mass")
  expect_identical(g$level, "strong")
  expect_match(g$headline, "suggest")
  expect_match(g$finding, "body mass")
  expect_false(grepl("outperform|better|worse", g$finding))
  expect_length(g$pending, 0)
})

test_that("a check not yet saved caps the level at moderate and is listed as pending", {
  g <- grade_evidence(list(entry(list(type = "visual", kind = "age_dependent", slope = 0.02, mean_gap = 0.1)),
                           entry(mv("age_dependent"))))
  expect_identical(g$level, "moderate")
  expect_true(all(c("slopes", "permutation", "missingness") %in% g$pending))
  expect_false("terminal" %in% g$pending)
  expect_true(all(nzchar(PENDING_TEXT[g$pending])))
})

test_that("a failed random-slope check points to heterogeneity in ageing", {
  g <- grade_evidence(list(
    entry(list(type = "visual", kind = "none", slope = 0, mean_gap = 0)),
    entry(mv("age_dependent")), entry(mv("none", slope = "uncorrelated")),
    entry(list(type = "permutation", pattern = "no_drop", model = "M4"))))
  expect_identical(g$level, "weak")
  expect_match(g$explanation, "ageing at different rates")
})

test_that("a fit with random slopes that disagrees is sensitivity, not a contradiction between models", {
  g <- grade_evidence(list(entry(mv("age_dependent")), entry(mv("none", slope = "uncorrelated"))))
  expect_false(identical(g$level, "mixed"))
})

test_that("trait-dependent missingness caps the level at weak and is named", {
  g <- grade_evidence(list(
    entry(list(type = "visual", kind = "age_independent", slope = 0, mean_gap = 0.3)),
    entry(mv("age_independent", p_int = 0.5)),
    entry(list(type = "permutation", pattern = "drops", model = "M2")),
    entry(list(type = "missingness", trait_p = 0.001, percent = 30))))
  expect_identical(g$level, "weak")
  expect_match(g$explanation, "Missing records depend on the trait")
})

test_that("terminal effects are not an evidence line; the list ends with the permutation test and observation bias", {
  g <- grade_evidence(list(entry(list(type = "visual", kind = "age_dependent", slope = 0.02, mean_gap = 0.1)),
                           entry(mv("age_dependent")), entry(mv("age_dependent", slope = "uncorrelated")),
                           entry(list(type = "permutation", pattern = "drops", model = "M4")),
                           entry(list(type = "missingness", trait_p = 0.6, percent = 5))))
  expect_false("Terminal effects" %in% g$lines$Evidence)
  expect_identical(utils::tail(g$lines$Evidence, 2), c("Null-model bootstrap", "Observation bias"))
  expect_identical(g$level, "strong")
})

test_that("a misspecified ageing function is named when it is flagged", {
  g <- grade_evidence(list(entry(list(type = "visual", kind = "age_dependent", slope = 0.02, mean_gap = 0.1)),
                           entry(mv("age_dependent", shape_ok = FALSE))))
  expect_match(g$explanation, "ageing function")
})

test_that("nothing saved is reported as insufficient, not as absence of selection", {
  g <- grade_evidence(list(list(evidence = NULL)))
  expect_identical(g$level, "insufficient")
})

sim_sd <- function(kind, n = 220, seed = 1) {
  set.seed(seed)
  do.call(rbind, lapply(seq_len(n), function(i) {
    q <- stats::rnorm(1); ls <- max(4, round(14 + 4 * q)); age <- seq_len(ls)
    slope <- -0.2 + if (identical(kind, "rate")) 0.1 * q else 0
    y <- 10 + stats::rnorm(1) + slope * age + stats::rnorm(ls, 0, 0.7)
    if (identical(kind, "terminal")) y[ls] <- y[ls] - 2.5
    data.frame(id = paste0("i", i), age = age, trait = y, alr = ls)
  }))
}

test_that("the visual statistic finds rate-linked selection and not a null", {
  expect_identical(classify_visual_coef(sim_sd("rate"), B = 60)$kind, "age_dependent")
  expect_false(identical(classify_visual_coef(sim_sd("none", seed = 4), B = 60)$kind, "age_dependent"))
})



# ---- reviewer fixes (0.20.0) ----

test_that("the direction of ageing comes from the finding's own model, not the naive Model 1", {
  stub <- list(ok = TRUE,
    aic = data.frame(Model = c("Model 4", "Model 2", "Model 1"), Delta_AIC = c(0, 12, 40), AIC = c(100, 112, 140)),
    lrt = data.frame(Comparison = c("Model 1 vs Model 2", "Model 2 vs Model 4"), P_value = c(0.001, 0.001)),
    coefficients = data.frame(Model = c("Model 1", "Model 2", "Model 4", "Model 4"), Raw_term = c("f1", "f1", "f1", "f1:ALR"),
                              Estimate = c(0.4, -0.3, -0.5, 0.1), SE = 0.05, P_value = 0.01),
    random_slope = "none", age_function = "Linear", family = "gaussian", among = "linear", standardise = TRUE)
  ev <- models_evidence(stub, "age_dependent")
  expect_identical(ev$kind, "age_dependent")
  expect_equal(ev$age_slope, -0.5)          # Model 4's slope; the naive Model 1 slope has the opposite sign
})

test_that("the bootstrap counts only when run on the model the finding rests on", {
  base <- list(entry(list(type = "visual", kind = "age_dependent", slope = 0.02, mean_gap = 0.1)),
               entry(mv("age_dependent")), entry(mv("age_dependent", slope = "uncorrelated")),
               entry(list(type = "missingness", trait_p = 0.6, percent = 5)))
  wrong <- grade_evidence(c(base, list(entry(list(type = "permutation", pattern = "drops", model = "M2")))))
  right <- grade_evidence(c(base, list(entry(list(type = "permutation", pattern = "drops", model = "M4")))))
  expect_identical(wrong$lines$Status[wrong$lines$Evidence == "Null-model bootstrap"], "not saved")
  expect_true("permutation" %in% wrong$pending)
  expect_identical(right$lines$Status[right$lines$Evidence == "Null-model bootstrap"], "supports")
  expect_identical(right$level, "strong")
})

test_that("a test run with other data or settings does not count", {
  m <- utils::modifyList(mv("age_dependent"), list(data_sig = "A", settings_key = "Linear|gaussian|linear|TRUE"))
  p <- list(type = "permutation", pattern = "drops", model = "M4", data_sig = "B", settings_key = "Linear|gaussian|linear|TRUE")
  g <- grade_evidence(list(entry(m), entry(p)))
  expect_identical(g$lines$Status[g$lines$Evidence == "Null-model bootstrap"], "not saved")
})

test_that("refitting with a corrected ageing function supersedes the earlier comparison instead of making it 'mixed'", {
  old <- utils::modifyList(mv("age_dependent"), list(settings_key = "Linear|gaussian|linear|TRUE", age_function = "Linear"))
  new <- utils::modifyList(mv("none"), list(settings_key = "Quadratic|gaussian|linear|TRUE", age_function = "Quadratic"))
  g <- grade_evidence(list(entry(old), entry(new)))
  expect_false(identical(g$level, "mixed"))
  expect_true(any(grepl("set aside", g$cautions)))
})

test_that("a manual shape check on Model 1 is ignored for a finding about selection", {
  base <- list(entry(list(type = "visual", kind = "age_dependent", slope = 0.02, mean_gap = 0.1)), entry(mv("age_dependent")))
  on_m1 <- grade_evidence(c(base, list(entry(list(type = "shape", ok = FALSE, current = "Linear", model = "M1")))))
  on_m4 <- grade_evidence(c(base, list(entry(list(type = "shape", ok = FALSE, current = "Linear", model = "M4")))))
  expect_identical(on_m1$lines$Status[on_m1$lines$Evidence == "Ageing shape"], "supports")   # the automatic check stands
  expect_identical(on_m4$lines$Status[on_m4$lines$Evidence == "Ageing shape"], "caveat")
})

test_that("a shape check of a function other than the one fitted is ignored", {
  base <- list(entry(list(type = "visual", kind = "age_dependent", slope = 0.02, mean_gap = 0.1)), entry(mv("age_dependent")))
  g <- grade_evidence(c(base, list(entry(list(type = "shape", ok = FALSE, current = "Cubic", model = "M4")))))
  expect_identical(g$lines$Status[g$lines$Evidence == "Ageing shape"], "supports")
})

test_that("the visual check leaves no random seed behind when there was none", {
  d <- sim_sd("level")                      # the generator sets a seed of its own, so remove it afterwards
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  invisible(classify_visual_coef(d))
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})


# ---- 0.20.2 ----
test_that("the finding reads the long- and short-lived slopes when they are available", {
  m <- utils::modifyList(mv("age_dependent"), list(slope_long = -0.1, slope_short = -0.4, age_slope = 0.3))
  expect_match(grade_evidence(list(entry(m)), trait = "mass")$finding, "decline more slowly in mass")
  m2 <- utils::modifyList(mv("age_dependent"), list(slope_long = 0.1, slope_short = -0.2))
  expect_match(grade_evidence(list(entry(m2)), trait = "mass")$finding, "increase in mass with age, while shorter-lived individuals decline")
})

test_that("with the asymptotic exponential the direction follows the data, not the coefficient's sign", {
  skip_on_cran()
  set.seed(8)
  d <- do.call(rbind, lapply(seq_len(60), function(i) {
    q <- stats::rnorm(1); ls <- max(4, round(10 + 2 * q)); age <- seq_len(ls)
    data.frame(id = paste0("i", i), age = age, LS = ls,
               trait = 10 + 0.5 * q - (3 - 0.8 * q) * (1 - exp(-age / 3)) + stats::rnorm(ls, 0, 0.3))
  }))
  b <- standardise_data(d, list(id = "id", age = "age", trait = "trait", alr = "__AUTO_LAST__", life = "LS",
                                entry = "__AUTO_FIRST__", covars = character(0), cov_factor = character(0),
                                cov_int = character(0), group = "", nested = TRUE, random = character(0), censor = "",
                                censor_value = "", condition = "", trials = "", start_mode = "afr", start_age = NA_real_,
                                age_round = NA_real_, cov_age = character(0)))
  r <- fit_model_suite(b$data, b$meta, models = c("M1", "M2", "M4"), age_function = "Asymptotic exponential")
  skip_if_not(isTRUE(r$ok))
  ev <- models_evidence(r, "age_dependent")
  expect_lt(ev$age_slope, 0)                                   # the trait declines with age
  expect_gt(raw_coef(r, "M4", "f1")$est, 0)                    # although the basis coefficient is positive
})

test_that("a saved random-slope fit is the sensitivity check, not the finding", {
  g <- grade_evidence(list(entry(mv("age_dependent")), entry(mv("none", slope = "uncorrelated"))))
  expect_identical(g$kind, "age_dependent")
  expect_identical(g$lines$Status[g$lines$Evidence == "Random-slope sensitivity"], "contradicts")
})

test_that("a bootstrap with too few refitted datasets is a caveat, never support", {
  g <- grade_evidence(list(entry(list(type = "visual", kind = "age_dependent", slope = 0.02, mean_gap = 0.1)),
                           entry(mv("age_dependent")), entry(mv("age_dependent", slope = "uncorrelated")),
                           entry(list(type = "permutation", pattern = "too_few", model = "M4")),
                           entry(list(type = "missingness", trait_p = 0.6, percent = 5))))
  expect_identical(g$level, "moderate")
  expect_identical(g$lines$Status[g$lines$Evidence == "Null-model bootstrap"], "caveat")
})

# ---- 0.20.6: evidence for selective appearance, visual statistics in words, no lifespan-term line ----
test_that("the disappearance summary has no lifespan-term line and states the visual statistics", {
  vis <- list(type = "visual", process = "disappearance", proxy = "alr", kind = "age_dependent",
              slope = 0.004, mean_gap = 0.02, p_trend = 0.01, p_level = 0.2)
  g <- grade_evidence(list(entry(vis), entry(mv("age_dependent"))), trait = "mass")
  expect_false(any(grepl("term", g$lines$Evidence)))
  expect_match(g$lines$Note[g$lines$Evidence == "Visual pattern"], "change per unit age 0.004, p = 0.010", fixed = TRUE)
})

test_that("figures grouped by AFR are evidence for selective appearance, not disappearance", {
  app_vis <- list(type = "visual", process = "appearance", proxy = "entry", kind = "age_dependent",
                  slope = 0.01, mean_gap = -0.2, p_trend = 0.01, p_level = 0.03)
  m <- utils::modifyList(mv("none"), list(best = "Model 8", appearance_kind = "age_dependent", appearance_model = "Model 8",
                                          appearance_gap = 0))
  g <- grade_evidence(list(entry(app_vis), entry(m)))
  expect_identical(g$lines$Status[g$lines$Evidence == "Visual pattern"], "not saved")   # the AFR figure is not used here
  ga <- grade_appearance(list(entry(app_vis), entry(m)), trait = "mass")
  expect_identical(ga$kind, "age_dependent")
  expect_identical(ga$level, "moderate")                       # at most moderate: no slope or bootstrap check yet
  expect_match(ga$finding, "early- and late-entering")
  contra <- grade_appearance(list(entry(utils::modifyList(app_vis, list(kind = "none"))), entry(m)))
  expect_identical(contra$level, "weak")
  expect_null(grade_appearance(list(entry(mv("age_dependent")))))   # no Models 7-10, no AFR figure: nothing to say
})

test_that("the visual classifier can group by known lifespan or AFR", {
  d <- sim_sd("dependent", n = 150, seed = 5)
  d$life <- d$alr
  a <- classify_visual_coef(d, B = 30)
  b <- classify_visual_coef(d, B = 30, proxy = "life")
  expect_equal(a$p_trend, b$p_trend)
  expect_identical(b$proxy, "life")
  expect_identical(classify_visual_coef(transform(d, entry = alr), B = 30, proxy = "entry", process = "appearance")$process, "appearance")
})

# ---- 0.20.7: the gap figure as secondary evidence ----
test_that("the gap figure is a secondary line: it can flag a caveat but does not change the level", {
  base <- list(entry(list(type = "visual", kind = "age_dependent", slope = 0.02, mean_gap = 0.1)),
               entry(mv("age_dependent")), entry(mv("age_dependent", slope = "uncorrelated")),
               entry(list(type = "permutation", pattern = "drops", model = "M4")),
               entry(list(type = "missingness", trait_p = 0.6, percent = 5)))
  g0 <- grade_evidence(base)
  vis_gap <- list(type = "visual", kind = "age_dependent", slope = 0.02, mean_gap = 0.1,
                  gap = list(kind = "none", level = 0.01, p_level = 0.5, trend = 0.001, p_trend = 0.6))
  g1 <- grade_evidence(c(list(entry(vis_gap)), base[-1]))
  expect_identical(g1$level, g0$level)
  expect_identical(g1$lines$Status[g1$lines$Evidence == "Gap between groups (secondary)"], "caveat")
  g2 <- grade_evidence(c(list(entry(utils::modifyList(vis_gap, list(gap = list(kind = "age_dependent", level = 0.05,
                                   p_level = 0.01, trend = 0.01, p_trend = 0.001))))), base[-1]))
  expect_identical(g2$lines$Status[g2$lines$Evidence == "Gap between groups (secondary)"], "supports")
})

test_that("the gap statistics read a rising gap as age-dependent", {
  dz <- data.frame(age = rep(1:8, 2), difference = c(0.1 * (1:8), 0.1 * (1:8) + 0.01), w = 1)
  gs <- visual_gap_stats(dz)
  expect_identical(gs$kind, "age_dependent")
  expect_gt(gs$trend, 0)
  expect_null(visual_gap_stats(dz[1:2, ]))
})

test_that("headlines put the level and the process in capitals, and the gap caveat is a footnote", {
  g <- grade_evidence(list(entry(list(type = "visual", kind = "none", slope = 0, mean_gap = 0)), entry(mv("none"))))
  expect_match(g$headline, "NO SELECTIVE DISAPPEARANCE", fixed = TRUE)
  vis_gap <- list(type = "visual", kind = "age_dependent", slope = 0.02, mean_gap = 0.1,
                  gap = list(kind = "none", level = 0.01, p_level = 0.5, trend = 0.001, p_trend = 0.6))
  g2 <- grade_evidence(list(entry(vis_gap), entry(mv("age_dependent"))))
  row <- g2$lines[g2$lines$Evidence == "Gap between groups (secondary)", ]
  expect_match(row$Footnote, "^Secondary evidence")
  expect_false(grepl("Secondary evidence", row$Note))
})

# ---- 0.20.17: a tick for the model comparison only when one model clearly wins ----
test_that("the model comparison is a tick only for a clear winner; otherwise a caveat naming the supported set", {
  base <- list(entry(list(type = "visual", kind = "age_dependent", slope = 0.02, mean_gap = 0.1)),
               entry(mv("age_dependent")), entry(mv("age_dependent", slope = "uncorrelated")),
               entry(list(type = "permutation", pattern = "drops", model = "M4")),
               entry(list(type = "missingness", trait_p = 0.6, percent = 5)))
  g <- grade_evidence(base)
  expect_identical(g$lines$Status[g$lines$Evidence == "Model comparison"], "supports")
  amb <- utils::modifyList(mv("age_dependent"), list(supported = c("Model 4", "Model 5", "Model 2"),
                                                     supported_df = c(`Model 4` = 7, `Model 5` = 7, `Model 2` = 6)))
  g2 <- grade_evidence(c(base[1], list(entry(amb)), base[-(1:2)]))
  row <- g2$lines[g2$lines$Evidence == "Model comparison", ]
  expect_identical(row$Status, "caveat")
  expect_match(row$Note, "supported set is Model 4, Model 5, Model 2", fixed = TRUE)
  expect_match(row$Note, "Parsimony suggests interpreting the simplest, Model 2", fixed = TRUE)
  expect_false(identical(g2$level, "strong"))                  # no clear winner: at most moderate
})

# ---- 0.20.18: the evidence kind is the winning model's own structure, across Models 1-10 ----
test_that("each model's structure gives its own reading of the two processes", {
  expect_identical(model_process_kind(model_label("M1")), "none")
  expect_identical(model_process_kind(model_label("M2")), "age_independent")
  expect_identical(model_process_kind(model_label("M4")), "age_dependent")
  expect_identical(model_process_kind(model_label("M9")), "age_dependent")
  expect_identical(model_process_kind(model_label("M9"), "app"), "age_independent")
  expect_identical(model_process_kind(model_label("M10")), "age_independent")
  expect_identical(model_process_kind(model_label("M10"), "app"), "age_dependent")
  expect_identical(model_process_kind(model_label("M1"), "app"), "none")
  expect_true(is.na(model_process_kind("Model 42")))
})

test_that("a winning model outside Models 1, 2 and 4 sets the kind, and the model set is part of the settings key", {
  stub <- list(ok = TRUE,
    aic = data.frame(Model = c("Model 10", "Model 7", "Model 1"), Delta_AIC = c(0, 9, 30), AIC = c(100, 109, 130)),
    lrt = data.frame(Comparison = "Model 7 vs Model 10", P_value = 0.002),
    coefficients = data.frame(Model = "Model 10", Raw_term = "f1", Estimate = -0.4, SE = 0.05, P_value = 0.01),
    random_slope = "none", age_function = "Linear", family = "gaussian", among = "linear", standardise = TRUE)
  ev <- models_evidence(stub, NA_character_)
  expect_identical(ev$kind, "age_independent")        # Model 10: additive ALR
  expect_identical(ev$appearance_kind, "age_dependent")   # Model 10: AFR x age
  k1 <- evidence_settings_key(stub)
  stub2 <- stub
  stub2$aic <- stub$aic[1:2, , drop = FALSE]
  expect_false(identical(k1, evidence_settings_key(stub2)))
})
