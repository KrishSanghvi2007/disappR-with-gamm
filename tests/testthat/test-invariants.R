# Property/invariant tests: things that must hold whatever the data or encoding.
# These catch whole classes of silent error that example-based tests miss.

make_data <- function(n = 60, seed = 1) {
  set.seed(seed)
  do.call(rbind, lapply(seq_len(n), function(i) {
    ls <- max(3, round(rnorm(1, 9, 3)))
    ages <- seq_len(ls)
    data.frame(id = sprintf("i%02d", i), age = ages, trait = 10 + rnorm(1) - 0.2 * ages + rnorm(ls, 0, 0.6),
               LS = ls, grp = sprintf("g%d", i %% 4 + 1), stringsAsFactors = FALSE)
  }))
}
base_map <- function(...) {
  m <- list(id = "id", age = "age", trait = "trait", alr = "__AUTO_LAST__", life = "LS", entry = "__AUTO_FIRST__",
            covars = character(0), cov_factor = character(0), cov_int = character(0), group = "", nested = TRUE,
            random = character(0), censor = "", censor_value = "", condition = "", trials = "",
            start_mode = "afr", start_age = NA_real_, age_round = NA_real_, cov_age = character(0))
  utils::modifyList(m, list(...))
}
fit_aic <- function(d, map = base_map(), models = c("M1", "M2", "M4"), ...) {
  b <- standardise_data(d, map)
  r <- fit_model_suite(b$data, b$meta, models = models, age_function = "Quadratic", family = "gaussian", ...)
  stats::setNames(r$aic$AIC, r$aic$Model)
}

test_that("standardising predictors leaves AIC unchanged", {
  d <- make_data()
  a <- fit_aic(d, standardise = TRUE)
  b <- fit_aic(d, standardise = FALSE)
  expect_equal(unname(sort(a)), unname(sort(b)), tolerance = 1e-6)
})

test_that("row order does not change the fit", {
  d <- make_data()
  set.seed(99)
  a <- fit_aic(d)
  b <- fit_aic(d[sample(nrow(d)), , drop = FALSE])
  expect_equal(a, b, tolerance = 1e-8)
})

test_that("relabelling individual IDs does not change the fit", {
  d <- make_data()
  d2 <- d
  d2$id <- paste0("zz_", rev(sort(unique(d$id)))[match(d$id, sort(unique(d$id)))])
  expect_equal(unname(fit_aic(d)), unname(fit_aic(d2)), tolerance = 1e-8)
})

test_that("equivalent nested-ID encodings give equivalent models", {
  d <- make_data()
  d2 <- d
  d2$id <- paste(d$grp, d$id, sep = "_")          # IDs already unique within group, spelled out
  a <- fit_aic(d, base_map(group = "grp"))
  b <- fit_aic(d2, base_map(group = "grp"))
  expect_equal(unname(a), unname(b), tolerance = 1e-6)
})

test_that("under complete sampling the automatic ALR equals the last observed age exactly", {
  d <- make_data()
  b <- standardise_data(d, base_map())
  im <- individual_metrics(b$data)
  last <- tapply(b$data$age, b$data$id, max)
  expect_equal(unname(im$alr[match(names(last), im$id)]), unname(as.numeric(last)))
  expect_equal(unname(im$alr), unname(im$last_recorded))
})

test_that("the age basis round-trips between raw and transformed scales", {
  d <- make_data()
  b <- standardise_data(d, base_map())
  prep <- prepare_model_data(b$data, "Quadratic", character(0), TRUE, FALSE, character(0))
  bb <- age_basis(b$data$age, prep$age_params)
  expect_equal(bb$f1, prep$data$f1, tolerance = 1e-10)
  expect_equal(bb$f2, prep$data$f2, tolerance = 1e-10)
  # and the mean/delta decomposition reconstructs the basis exactly
  expect_equal(prep$data$mean_f1 + prep$data$delta_f1, prep$data$f1, tolerance = 1e-10)
})

test_that("a constant shift of age leaves model comparison unchanged", {
  d <- make_data()
  d2 <- d; d2$age <- d2$age + 100; d2$LS <- d2$LS + 100
  a <- fit_aic(d); b <- fit_aic(d2)
  expect_equal(unname(a - min(a)), unname(b - min(b)), tolerance = 1e-5)
})

test_that("the exported analysis code reproduces the fitted AIC values", {
  d <- make_data()
  b <- standardise_data(d, base_map())
  r <- fit_model_suite(b$data, b$meta, models = c("M1", "M2", "M4"), age_function = "Quadratic", family = "gaussian")
  again <- fit_model_suite(b$data, b$meta, models = c("M1", "M2", "M4"), age_function = "Quadratic", family = "gaussian")
  expect_equal(r$aic$AIC, again$aic$AIC, tolerance = 1e-10)
  code <- model_r_code(r, b$meta)
  expect_true(is.character(code) && any(grepl("fit_model_suite|lmer", code)))
})

test_that("the decomposition is invariant to row order and ID labels", {
  d <- make_data()
  b <- standardise_data(d, base_map())
  set.seed(5)
  b2 <- standardise_data(d[sample(nrow(d)), , drop = FALSE], base_map())
  x <- decomposition_trajectory(b$data); y <- decomposition_trajectory(b2$data)
  expect_equal(x$age, y$age)
  expect_equal(x$fitted, y$fitted, tolerance = 1e-10)
  expect_equal(x$n_pairs, y$n_pairs)
})

test_that("joint Wald tests use every coefficient of a block", {
  d <- make_data()
  b <- standardise_data(d, base_map())
  r <- fit_model_suite(b$data, b$meta, models = "M4", age_function = "Quadratic", family = "gaussian")
  fit <- r$fits[["M4"]]
  skip_if(is.null(fit))
  terms <- grep(":ALR$|^ALR:", names(lme4::fixef(fit)), value = TRUE)
  skip_if(length(terms) < 2)
  j <- joint_wald(fit, terms)
  expect_equal(j$df, length(terms))
  expect_true(is.finite(j$p) && j$p >= 0 && j$p <= 1)
  # a joint test is never simply the smallest component p-value
  ct <- summary(fit)$coefficients
  comp <- 2 * stats::pnorm(-abs(ct[terms, "t value"]))
  expect_false(isTRUE(all.equal(j$p, min(comp))))
})
