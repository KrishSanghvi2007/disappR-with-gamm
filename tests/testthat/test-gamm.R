# GAMM layer (0.23.0): an independent layer fitted with mgcv

gamm_toy <- function(n_id = 120, seed = 3, two_groups = TRUE) {
  set.seed(seed)
  do.call(rbind, lapply(seq_len(n_id), function(i) {
    ls <- sample(4:14, 1); a <- 1:ls
    sex <- if (two_groups) ifelse(i %% 2 == 0, "F", "M") else "F"
    peak <- if (sex == "F") 5 else 8
    data.frame(id = i, age = a, sex = sex,
               trait = 10 + 2 * sin(pi * pmin(a, 2 * peak) / (2 * peak)) - 0.08 * pmax(a - peak, 0)^2 + 0.1 * ls + stats::rnorm(1, 0, 0.5) + stats::rnorm(ls, 0, 0.5))
  }))
}
gamm_prep <- function(d) standardise_data(d, list(id = "id", age = "age", trait = "trait", alr = "__AUTO_LAST__", life = "", entry = "__AUTO_FIRST__",
  covars = "sex", cov_factor = "sex", cov_int = character(0), group = "", nested = TRUE, random = character(0), censor = "", censor_value = "",
  condition = "", trials = "", start_mode = "afr", start_age = NA_real_, age_round = NA_real_, cov_age = character(0)))

test_that("the formula follows the settings", {
  gd <- data.frame(id = factor(1:5), age = 1:5, trait = 1:5, ALR = 5, LS = NA_real_, AFR = 1, cv_sex = factor(c("F", "M", "F", "M", "F")))
  s <- utils::modifyList(gamm_defaults(), list(group = "cv_sex", structure = "GS", proxy_form = "ti"))
  f <- gamm_formula(s, gd)
  expect_match(f, "bs = \"fs\"", fixed = TRUE)
  expect_match(f, "ti(age, ALR", fixed = TRUE)
  expect_match(f, "s(id, bs = \"re\")", fixed = TRUE)
  expect_false(grepl("+ cv_sex", f, fixed = TRUE))                    # the fs smooth already carries each group
  expect_match(gamm_formula(utils::modifyList(s, list(structure = "I")), gd), "by = cv_sex", fixed = TRUE)
  expect_match(gamm_formula(s, gd, age_poly = 2L), "poly(age, 2, raw = TRUE)", fixed = TRUE)
})

test_that("a GAMM fits, reports checks and finds the late decline", {
  skip_if_not_installed("mgcv")
  b <- gamm_prep(gamm_toy())
  gd <- gamm_data(b$data, b$meta)
  r <- gamm_fit(gd, gamm_defaults())
  expect_true(isTRUE(r$ok))
  tb <- gamm_tables(r)
  expect_true(any(grepl("s(age", tb$smooth$Smooth, fixed = TRUE)))
  expect_gt(nrow(gamm_kcheck(r)), 0)
  cv <- gamm_curve(r)
  expect_true(all(cv$lower <= cv$fit & cv$fit <= cv$upper))
  dv <- gamm_derivative(r, n_sim = 500)
  expect_true(all(dv$lower_sim <= dv$lower + 1e-9))                     # simultaneous intervals are wider
  pr <- gamm_periods(dv)
  expect_true(any(pr$Change == "decline"))
  po <- gamm_peak_onset(r, n_draws = 200)
  expect_true(is.data.frame(po) && nrow(po) == 1)
  expect_true(is.character(gamm_code(r)))
  expect_true(isTRUE(tryCatch({ parse(text = gamm_code(r)); TRUE }, error = function(e) FALSE)))
})

test_that("groups that age differently are told apart", {
  skip_if_not_installed("mgcv")
  b <- gamm_prep(gamm_toy(n_id = 160))
  gd <- gamm_data(b$data, b$meta)
  s <- utils::modifyList(gamm_defaults(), list(group = "cv_sex", structure = "I"))
  r <- gamm_fit(gd, s)
  expect_true(isTRUE(r$ok))
  expect_gt(nrow(gamm_difference(r, n_sim = 300)), 0)
  cmp <- gamm_group_comparison(gd, s)
  expect_true(isTRUE(cmp$ok))
  expect_setequal(cmp$table$Model, c("G", "GS", "GI", "S", "I"))
  expect_false(identical(cmp$table$Model[[1]], "G"))                   # the two sexes peak at different ages
  expect_gt(nrow(cmp$differences), 0)
})

test_that("proxy and polynomial comparisons run", {
  skip_if_not_installed("mgcv")
  b <- gamm_prep(gamm_toy(n_id = 80, two_groups = FALSE))
  gd <- gamm_data(b$data, b$meta)
  pc <- gamm_proxy_comparison(gd, gamm_defaults())
  expect_equal(nrow(pc$table), 3)
  pr <- gamm_parametric_reference(gd, gamm_defaults())
  expect_setequal(pr$Model, c("Linear age", "Quadratic age", "Cubic age", "Smooth of age"))
})

test_that("unsuitable data are refused with a message", {
  skip_if_not_installed("mgcv")
  b <- gamm_prep(gamm_toy(n_id = 40, two_groups = FALSE))
  gd <- gamm_data(b$data, b$meta)
  expect_false(isTRUE(gamm_fit(gd, utils::modifyList(gamm_defaults(), list(family = "betar")))$ok))
  expect_false(isTRUE(gamm_fit(gd[1:10, ], gamm_defaults())$ok))
})
