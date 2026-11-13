# Consistency tier (0.22.4): the numbers and words the app reports must match the truth under every scaling setting.
# Standardising age and the lifespan proxies changes the parameterisation, never the model, so everything reported in
# original units (predictions, predicted differences, peak ages, the ages quoted in the text) must be identical with
# and without standardisation, and must equal a calculation made by hand from the unstandardised coefficients.
# Mirrors Part C of inst/validation/audit_0_22_0.

cons_fits <- local({
  cache <- list()
  function(fun, std) {
    key <- paste(fun, std)
    if (is.null(cache[[key]])) {
      t <- toy_prepared(n_id = 120, seed = 21)
      cache[[key]] <<- fit_model_suite(t$prep$data, t$prep$meta, models = c("M1", "M2", "M4", "M5"), age_function = fun,
                                       family = "gaussian", standardise = std)
    }
    cache[[key]]
  }
})
CONS_FUNS <- c("Linear", "Quadratic", "Cubic", "Logarithmic", "Asymptotic exponential")

# numbers after "+" or the minus sign in "... at age X" phrases of the interpretation text
text_differences <- function(txt) {
  hit <- regmatches(txt, gregexpr("[+\u2212][0-9.]+ at age [0-9.]+", txt))[[1]]
  if (!length(hit)) return(data.frame(value = numeric(0), age = numeric(0)))
  val <- as.numeric(sub("^\u2212", "-", sub("^\\+", "", sub(" at age .*$", "", hit))))
  data.frame(value = val, age = as.numeric(sub("^.* at age ", "", hit)))
}

test_that("standardising changes the parameterisation, not the model", {
  for (fun in CONS_FUNS) {
    a <- cons_fits(fun, TRUE); b <- cons_fits(fun, FALSE)
    expect_true(isTRUE(a$ok) && isTRUE(b$ok), info = fun)
    ages <- smooth_prediction_ages(a$data$age, 25)
    for (m in c("M1", "M2", "M4")) {
      pa <- predict_population_curve(a$fits[[m]], a, ages); pb <- predict_population_curve(b$fits[[m]], b, ages)
      expect_equal(pa$fitted, pb$fitted, tolerance = 1e-4, info = paste(fun, m))
    }
    expect_equal(a$aic$AIC, b$aic$AIC, tolerance = 1e-5, info = fun)
  }
})

test_that("term descriptions follow the basis actually used", {
  for (fun in CONS_FUNS) for (std in c(TRUE, FALSE)) {
    r <- cons_fits(fun, std)
    raw <- r$coefficients$Raw_term[r$coefficients$Model == "Model 4"]
    sc <- term_scaling(raw, r)
    if (!std) {
      expect_false(any(grepl("centre", sc$Per)), info = paste(fun, "not standardised"))
      expect_true(all(sc$Scale_factor[raw != "(Intercept)"] == 1), info = fun)
    }
    if (std && fun %in% c("Quadratic", "Cubic")) expect_true(any(grepl("centre)^2", sc$Per, fixed = TRUE)), info = fun)
  }
})

test_that("estimates per original unit equal the unstandardised estimates for terms that centring does not change", {
  for (fun in c("Linear", "Quadratic", "Cubic")) {
    a <- cons_fits(fun, TRUE); b <- cons_fits(fun, FALSE)
    k <- length(a$basis)
    top <- paste0("f", k)
    for (term in c(top, "ALR")) {
      m <- if (identical(term, "ALR")) "Model 2" else "Model 1"
      ea <- a$coefficients[a$coefficients$Model == m & a$coefficients$Raw_term == term, ]
      eb <- b$coefficients[b$coefficients$Model == m & b$coefficients$Raw_term == term, ]
      conv <- ea$Estimate / term_scaling(term, a)$Scale_factor
      expect_equal(conv, eb$Estimate, tolerance = 1e-3, info = paste(fun, term))
    }
  }
})

test_that("predicted differences in the text equal a hand calculation and do not depend on standardisation", {
  for (fun in CONS_FUNS) {
    b <- cons_fits(fun, FALSE)
    bb <- lme4::fixef(b$fits$M4)
    ind <- b$data[!duplicated(b$data$id), , drop = FALSE]
    q <- as.numeric(stats::quantile(ind$ALR_raw[is.finite(ind$ALR_raw)], c(0.1, 0.9), names = FALSE, type = 7))
    inter <- function(fk) {
      nm <- names(bb)[vapply(strsplit(names(bb), ":", fixed = TRUE), function(p) setequal(p, c(fk, "ALR")), logical(1))]
      if (length(nm)) bb[[nm]] else 0
    }
    truth <- function(age) {
      basis <- age_basis(age, b$age_params)
      (q[2] - q[1]) * (bb[["ALR"]] + sum(vapply(names(basis), function(fk) inter(fk) * basis[[fk]][1], numeric(1))))
    }
    for (std in c(TRUE, FALSE)) {
      r <- cons_fits(fun, std)
      td <- text_differences(paste(interpret_model_terms(r, "M4"), collapse = " "))
      # the ages the text uses, unrounded, in the order it lists them
      ages <- unique(as.numeric(stats::quantile(r$data$age, c(0.1, 0.5, 0.9), names = FALSE, type = 7)))
      expect_equal(nrow(td), length(ages), info = paste(fun, std))
      for (i in seq_len(min(nrow(td), length(ages)))) {
        tr <- truth(ages[i])
        expect_lt(abs(td$value[i] - tr), 0.006 * max(1, abs(tr)) + 1e-3)
      }
    }
  }
})

test_that("the text names the age at which the main effect applies", {
  for (fun in c("Linear", "Quadratic", "Cubic")) {
    expect_match(paste(interpret_model_terms(cons_fits(fun, FALSE), "M4"), collapse = " "), "difference at age 0", info = fun)
    expect_false(grepl("not standardised", paste(interpret_model_terms(cons_fits(fun, TRUE), "M4"), collapse = " ")), info = fun)
  }
  ap <- cons_fits("Quadratic", TRUE)$age_params
  expect_equal(age_reference(ap)$age, ap$mean_age)
  expect_equal(age_reference(cons_fits("Quadratic", FALSE)$age_params)$age, 0)
  apl <- cons_fits("Logarithmic", FALSE)$age_params
  expect_equal(age_reference(apl)$age, if (isTRUE(apl$all_positive)) 1 else apl$min_age)
  expect_identical(age_reference(cons_fits("Asymptotic exponential", FALSE)$age_params)$age, Inf)
})

test_that("the mean-age term quotes real mean ages, younger group first, for every function", {
  for (fun in CONS_FUNS) for (std in c(TRUE, FALSE)) {
    r <- cons_fits(fun, std)
    txt <- paste(interpret_model_terms(r, "M5"), collapse = " ")
    # the text names the older group first ("between A and B" puts the higher group first), so each age is read
    # from its own label rather than from its position
    yo <- regmatches(txt, regexpr("younger ages \\(mean age \u2248 [0-9.]+", txt))
    ol <- regmatches(txt, regexpr("older ages \\(mean age \u2248 [0-9.]+", txt))
    if (!length(yo) || !length(ol)) next
    a <- as.numeric(sub("^.*\u2248 ", "", c(yo, ol)))
    expect_lt(a[1], a[2])
    expect_true(all(a >= min(r$data$age) & a <= max(r$data$age)), info = paste(fun, std))
  }
})

test_that("no non-zero coefficient is displayed as zero", {
  for (fun in c("Quadratic", "Cubic")) for (std in c(TRUE, FALSE)) {
    r <- cons_fits(fun, std)
    x <- r$coefficients[r$coefficients$Model == "Model 4", , drop = FALSE]
    shown <- suppressWarnings(as.numeric(coef_display(x, r)$Estimate))
    expect_false(any(shown == 0 & abs(x$Statistic) >= 0.1, na.rm = TRUE), info = paste(fun, std))
  }
  tab <- display_numbers(data.frame(a = c(0.0004, 0.5), b = c(1, 2)))
  expect_identical(tab$a, c("0.0004", "0.50"))
  expect_true(is.numeric(tab$b))
})

test_that("peak and onset ages do not depend on standardisation", {
  for (fun in c("Quadratic", "Cubic")) {
    a <- peak_onset_summary(cons_fits(fun, TRUE), "M2", n_draws = 0)
    b <- peak_onset_summary(cons_fits(fun, FALSE), "M2", n_draws = 0)
    step <- diff(range(cons_fits(fun, TRUE)$data$age)) / 199
    expect_lte(abs(a$turning$peak - b$turning$peak), step + 1e-9)
  }
})

test_that("the no-variance flag does not depend on the scale of a slope's covariate", {
  for (c0 in c(1, 1 / 15, 1 / 225)) {
    vc <- data.frame(Model = "Model 1", Group = c("id", "id.1"), Term = c("(Intercept)", "f2"), Type = "SD",
                     Estimate = c(0.3, 0.01 / c0), Variance = c(0.09, (0.01 / c0)^2), stringsAsFactors = FALSE)
    d <- data.frame(f2 = rep((1:15)^2 * c0, 20))
    expect_false(negligible_random_terms(vc, data = d)$flag[2])
  }
})
