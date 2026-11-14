# Step 7: the GAMM layer (R/gamm.R), fitted with mgcv

gamm_toy <- function(n_id = 120, seed = 3, two_groups = TRUE, sd_slope = 0) {
  set.seed(seed)
  do.call(rbind, lapply(seq_len(n_id), function(i) {
    ls <- sample(4:14, 1); a <- 1:ls
    sex <- if (two_groups) ifelse(i %% 2 == 0, "F", "M") else "F"
    peak <- if (sex == "F") 5 else 8
    data.frame(id = i, age = a, sex = sex, lifespan = ls,
               trait = 10 + 2 * sin(pi * pmin(a, 2 * peak) / (2 * peak)) - 0.08 * pmax(a - peak, 0)^2 + 0.1 * ls +
                 stats::rnorm(1, 0, 0.5) + stats::rnorm(1, 0, sd_slope) * a + stats::rnorm(ls, 0, 0.5))
  }))
}
gamm_prep <- function(d, life = "", covars = "sex", random = character(0)) standardise_data(d, list(id = "id", age = "age", trait = "trait",
  alr = "__AUTO_LAST__", life = life, entry = "__AUTO_FIRST__", covars = covars, cov_factor = intersect(covars, "sex"),
  cov_int = character(0), group = "", nested = TRUE, random = random, censor = "", censor_value = "",
  condition = "", trials = "", start_mode = "afr", start_age = NA_real_, age_round = NA_real_, cov_age = character(0)))
gamm_gd_of <- function(...) { b <- gamm_prep(...); gamm_data(b$data, b$meta) }

test_that("the formula follows the settings and always carries the group main effect", {
  gd <- data.frame(id = factor(rep(1:5, each = 2)), age = 1:10, trait = 1:10, ALR = rep(c(4, 6, 8, 9, 10), each = 2), LS = NA_real_, AFR = 1,
                   .terminal = factor("no", levels = c("no", "yes")), cv_sex = factor(rep(c("F", "M"), 5)))
  s <- utils::modifyList(gamm_defaults(), list(group = "cv_sex", proxy_form = "ti"))
  for (st in c("G", "GS", "GI", "S", "I")) expect_match(gamm_formula(utils::modifyList(s, list(structure = st)), gd), "trait ~ cv_sex +", fixed = TRUE)
  expect_match(gamm_formula(utils::modifyList(s, list(structure = "GS")), gd), "bs = \"sz\"", fixed = TRUE)
  expect_match(gamm_formula(utils::modifyList(s, list(structure = "S")), gd), "id = 1", fixed = TRUE)
  expect_match(gamm_formula(utils::modifyList(s, list(structure = "I")), gd), "by = cv_sex", fixed = TRUE)
  f <- gamm_formula(utils::modifyList(s, list(structure = "G")), gd)
  expect_match(f, "ti(age, ALR", fixed = TRUE)
  expect_match(f, "s(id, bs = \"re\")", fixed = TRUE)
  expect_equal(lengths(regmatches(f, gregexpr("cv_sex", f))), 1)            # the group is not entered twice
  expect_match(gamm_formula(utils::modifyList(s, list(individual = "rs")), gd), "s(id, .age_c, bs = \"re\")", fixed = TRUE)
  expect_match(gamm_formula(s, gd, age_poly = 2L), "poly(age, 2, raw = TRUE)", fixed = TRUE)
})

test_that("a GAMM fits, reports checks and finds the late decline", {
  skip_if_not_installed("mgcv")
  gd <- gamm_gd_of(gamm_toy(two_groups = FALSE), covars = character(0))
  r <- gamm_fit(gd, gamm_defaults())
  expect_true(r$ok)
  tb <- gamm_tables(r)
  expect_true(any(grepl("s(age)", tb$smooth$Smooth, fixed = TRUE)))
  expect_gt(tb$smooth$edf[tb$smooth$Smooth == "s(age)"], 2)
  dv <- gamm_derivative(r)
  expect_true(all(dv$lower_sim <= dv$lower + 1e-9) && all(dv$upper_sim >= dv$upper - 1e-9))   # simultaneous are wider
  pr <- gamm_periods(dv)
  expect_true(any(pr$Change == "decline" & pr$From > 5))
  po <- gamm_peak_onset(r, n_draws = 200)
  expect_equal(nrow(po), 1)
  expect_true(abs(as.numeric(po[["Peak age"]]) - 5) < 1.5)
  cv <- gamm_curve(r)
  expect_true(all(cv$lower <= cv$fit & cv$fit <= cv$upper))
  expect_s3_class(gamm_kcheck(r), "data.frame")
  expect_s3_class(gamm_vcomp(r), "data.frame")
  expect_true(any(grepl("library(mgcv)", gamm_code(r), fixed = TRUE)))
  expect_length(gamm_reading(r, dv), 3)
})

test_that("groups that age differently are told apart, and groups that differ only in level are not", {
  skip_if_not_installed("mgcv")
  gd <- gamm_gd_of(gamm_toy())
  s <- utils::modifyList(gamm_defaults(), list(group = "cv_sex", structure = "I"))
  cmp <- gamm_group_comparison(gd, s)
  expect_setequal(cmp$table$Model, c("G", "GS", "GI", "S", "I"))
  expect_false(identical(cmp$table$Model[[1]], "G"))                   # the two sexes peak at different ages
  expect_gt(nrow(cmp$differences), 0)
  # same shape, different level: G (with the group main effect) should be within 2 AIC of the best
  set.seed(9)
  d <- do.call(rbind, lapply(1:150, function(i) { a <- 1:sample(4:12, 1); sx <- ifelse(i %% 2, "F", "M")
    data.frame(id = i, age = a, sex = sx, trait = 3 + (sx == "M") * 1.5 + sin(a / 3) + rnorm(1, 0, .4) + rnorm(length(a), 0, .4)) }))
  cmp2 <- gamm_group_comparison(gamm_gd_of(d), utils::modifyList(gamm_defaults(), list(group = "cv_sex", proxy = "none", proxy_form = "none")))
  expect_lt(cmp2$table$dAIC[cmp2$table$Model == "G"], 2)
})

test_that("comparisons use the same records even when lifespan is unknown for some individuals", {
  skip_if_not_installed("mgcv")
  d <- gamm_toy(n_id = 80, two_groups = FALSE)
  d$lifespan[d$id %% 4 == 0] <- NA                                      # still alive
  gd <- gamm_gd_of(d, life = "lifespan", covars = character(0))
  pc <- gamm_proxy_comparison(gd, utils::modifyList(gamm_defaults(), list(proxy = "LS")))
  expect_true(pc$ok)
  tab <- pc$tables$LS
  expect_equal(nrow(tab), 4)
  expect_true(all(is.finite(tab$AIC)))
  expect_equal(length(unique(tab$Records)), 1)
  expect_equal(unique(tab$Records), sum(is.finite(gd$LS)))
  expect_true(pc$verdicts[["LS"]] %in% c("age_dependent", "age_independent", "none", "caution"))
  r <- gamm_fit(gd, utils::modifyList(gamm_defaults(), list(proxy = "LS")))
  expect_true(any(grepl("without a known lifespan", gamm_cautions(r, gd = gd))))
  pr <- gamm_parametric_reference(gd, gamm_defaults())
  expect_setequal(pr$Model, c("Linear age", "Quadratic age", "Cubic age", "Smooth of age"))
  expect_true(all(is.finite(pr$AIC)))
})

test_that("a covariate with one level, or constant random terms, do not break the fit", {
  skip_if_not_installed("mgcv")
  gd <- gamm_gd_of(gamm_toy(n_id = 60, two_groups = FALSE))       # sex is "F" for everyone
  r <- gamm_fit(gd, gamm_defaults())
  expect_true(r$ok)
  expect_true(any(grepl("do not vary", r$notes)))
})

test_that("every individual structure, the terminal term, AR1 and bam fit", {
  skip_if_not_installed("mgcv")
  gd <- gamm_gd_of(gamm_toy(n_id = 80, two_groups = FALSE, sd_slope = 0.1), life = "lifespan", covars = character(0))
  for (ind in c("rs", "fs", "none")) expect_true(gamm_fit(gd, utils::modifyList(gamm_defaults(), list(individual = ind)))$ok, info = ind)
  rt <- gamm_fit(gd, utils::modifyList(gamm_defaults(), list(terminal = TRUE)))
  expect_true(rt$ok); expect_match(rt$formula, ".terminal", fixed = TRUE)
  ra <- gamm_fit(gd, utils::modifyList(gamm_defaults(), list(ar1 = TRUE)))
  expect_true(ra$ok); expect_identical(ra$engine, "bam")
  rb <- gamm_fit(gd, utils::modifyList(gamm_defaults(), list(engine = "bam")))
  expect_true(rb$ok); expect_s3_class(gamm_curve(rb), "data.frame")
  rti <- gamm_fit(gd, utils::modifyList(gamm_defaults(), list(proxy_form = "ti")))
  expect_true(rti$ok)
  expect_equal(length(unique(gamm_curve(rti)$proxy)), 3)           # short-lived, median and long-lived curves
  nc <- gamm_naive_vs_corrected(gd, rti)
  expect_setequal(unique(nc$model), c("Without proxy terms (population curve)", "With proxy terms (within-individual curve)"))
})

test_that("non-Gaussian families fit and unsuitable data are refused with a message", {
  skip_if_not_installed("mgcv")
  set.seed(4)
  d <- do.call(rbind, lapply(1:80, function(i) { a <- 1:sample(3:10, 1); mu <- exp(1 + 0.3 * a - 0.03 * a^2 + rnorm(1, 0, .2))
    data.frame(id = i, age = a, trait = rpois(length(a), mu), surv = rbinom(length(a), 1, plogis(2 - 0.2 * a))) }))
  gd <- gamm_gd_of(d, covars = character(0))
  for (f in c("poisson", "nb", "tw", "ziP")) expect_true(gamm_fit(gd, utils::modifyList(gamm_defaults(), list(family = f)))$ok, info = f)
  qr <- gamm_quantile_residuals(gamm_fit(gd, utils::modifyList(gamm_defaults(), list(family = "poisson"))))
  expect_true(all(qr$u > 0 & qr$u < 1))
  d$trait <- d$surv
  gdb <- gamm_gd_of(d, covars = character(0))
  expect_true(gamm_fit(gdb, utils::modifyList(gamm_defaults(), list(family = "binomial")))$ok)
  expect_false(isTRUE(gamm_fit(gd, utils::modifyList(gamm_defaults(), list(family = "betar")))$ok))
  expect_false(isTRUE(gamm_fit(gd, utils::modifyList(gamm_defaults(), list(family = "binomial")))$ok))
  expect_false(isTRUE(gamm_fit(gd[1:10, ], gamm_defaults())$ok))
  expect_false(isTRUE(gamm_fit(gd, utils::modifyList(gamm_defaults(), list(proxy = "LS")))$ok))   # no lifespan mapped
})

test_that("the gamm4 engine fits what it can, and comparisons use one engine for every model", {
  skip_if_not_installed("mgcv"); skip_if_not_installed("gamm4")
  gd <- gamm_gd_of(gamm_toy(n_id = 80), life = "lifespan")
  r <- gamm_fit(gd, utils::modifyList(gamm_defaults(), list(engine = "gamm4", individual = "rs")))
  expect_true(r$ok); expect_identical(r$engine, "gamm4"); expect_false(is.null(r$mer))
  expect_true(is.finite(gamm_aic(r)))
  expect_s3_class(gamm_curve(r), "data.frame")
  expect_true(any(grepl("slope of age", gamm_vcomp(r)$Term)))
  expect_gt(nrow(gamm_individual_curves(r)), 0)
  expect_true(any(grepl("gamm4::gamm4", gamm_code(r), fixed = TRUE)))
  # ti() needs mgcv: the fit falls back to bam with a note
  rt <- gamm_fit(gd, utils::modifyList(gamm_defaults(), list(engine = "gamm4", proxy_form = "ti")))
  expect_identical(rt$engine, "bam"); expect_true(any(grepl("gamm4 cannot fit", rt$notes)))
  # with gamm4 the proxy comparison uses the varying-coefficient form, which gamm4 can fit, for every model
  pc <- gamm_proxy_comparison(gd, utils::modifyList(gamm_defaults(), list(engine = "gamm4")))
  expect_identical(pc$engine, "gamm4"); expect_true(pc$fast)
  expect_true(any(grepl("s(age, by = ALR)", pc$tables$ALR$Model, fixed = TRUE)))
  expect_true(all(is.finite(pc$tables$ALR$AIC)))
  # and the group comparison fits G, GI and I, and says why GS and S were not fitted
  gc <- gamm_group_comparison(gd, utils::modifyList(gamm_defaults(), list(engine = "gamm4", group = "cv_sex")))
  expect_true(gc$fast)
  expect_true(all(is.finite(gc$table$AIC[gc$table$Model %in% c("G", "GI", "I")])))
  expect_true(all(grepl("not fitted", gc$table$Status[gc$table$Model %in% c("GS", "S")])))
})

test_that("the varying-coefficient proxy form fits with mgcv and gamm4 and draws curves by proxy value", {
  skip_if_not_installed("mgcv")
  gd <- gamm_gd_of(gamm_toy(n_id = 80, two_groups = FALSE), covars = character(0))
  r <- gamm_fit(gd, utils::modifyList(gamm_defaults(), list(proxy_form = "vc")))
  expect_true(r$ok); expect_match(r$formula, "s(age, by = .ALR_c", fixed = TRUE)
  expect_equal(length(unique(gamm_curve(r)$proxy)), 3)
  expect_true(is.finite(r$centres$ALR))
  skip_if_not_installed("gamm4")
  r4 <- gamm_fit(gd, utils::modifyList(gamm_defaults(), list(proxy_form = "vc", engine = "gamm4")))
  expect_true(r4$ok); expect_identical(r4$engine, "gamm4")
  expect_equal(length(unique(gamm_curve(r4)$proxy)), 3)
})

test_that("an observation-level random effect fits with gamm4 and mgcv, and is left out of curves", {
  skip_if_not_installed("mgcv")
  set.seed(5)
  d <- do.call(rbind, lapply(1:80, function(i) { a <- 1:sample(3:9, 1)
    data.frame(id = i, age = a, trait = rpois(length(a), exp(1 + 0.4 * a - 0.04 * a^2 + rnorm(1, 0, .3) + rnorm(length(a), 0, .5)))) }))
  gd <- gamm_gd_of(d, covars = character(0))
  r <- gamm_fit(gd, utils::modifyList(gamm_defaults(), list(family = "poisson", olre = TRUE, engine = "gam")))
  expect_true(r$ok); expect_match(r$formula, "s(.obs, bs = \"re\")", fixed = TRUE)
  expect_true(any(grepl(".obs", gamm_random_smooths(r$fit)$labels, fixed = TRUE)))
  expect_true(all(is.finite(gamm_curve(r)$fit)))
  expect_gt(nrow(gamm_individual_curves(r)), 0)
  expect_true(any(grepl("gd$.obs <-", gamm_code(r), fixed = TRUE)))
  rg <- gamm_fit(gd, utils::modifyList(gamm_defaults(), list(family = "gaussian", olre = TRUE)))
  expect_false(grepl(".obs", rg$formula, fixed = TRUE)); expect_true(any(grepl("Poisson and binomial families only", rg$notes)))
  skip_if_not_installed("gamm4")
  r4 <- gamm_fit(gd, utils::modifyList(gamm_defaults(), list(family = "poisson", olre = TRUE)))
  expect_identical(r4$engine, "gamm4"); expect_match(r4$formula, "(1 | .obs)", fixed = TRUE)
})
