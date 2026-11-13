# 0.22.3: model output readable and correctly described when age is not standardised

test_that("small coefficients are displayed with significant digits, not as 0.00", {
  expect_identical(sig_text(c(0.000463, 4.05, -0.0776, 0, NA)), c("0.000463", "4.05", "-0.0776", "0", "NA"))
  expect_match(sig_text(2.3e-6), "e-06$")
})

test_that("without standardisation the terms are described as age, age^2 and age^3, and the note says where the main effect sits", {
  t <- toy_prepared(n_id = 80, seed = 12)
  r <- fit_model_suite(t$prep$data, t$prep$meta, models = c("M1", "M4"), age_function = "Cubic", standardise = FALSE)
  expect_true(isTRUE(r$ok))
  sc <- term_scaling(r$coefficients$Raw_term[r$coefficients$Model == "Model 4"], r)
  raw <- r$coefficients$Raw_term[r$coefficients$Model == "Model 4"]
  expect_identical(sc$Per[raw == "f2"], "age^2")
  expect_identical(sc$Per[raw == "f3"], "age^3")
  expect_match(sc$Per[raw == "(Intercept)"], "age = 0")
  expect_false(any(grepl("centre)^", sc$Per, fixed = TRUE)))
  txt <- paste(interpret_model_terms(r, "M4"), collapse = " ")
  expect_match(txt, "not standardised")
  # with standardisation the descriptions keep the centre
  rs <- fit_model_suite(t$prep$data, t$prep$meta, models = c("M1", "M4"), age_function = "Cubic", standardise = TRUE)
  sc2 <- term_scaling(rs$coefficients$Raw_term[rs$coefficients$Model == "Model 4"], rs)
  expect_true(any(grepl("centre)^2", sc2$Per, fixed = TRUE)))
})

test_that("a random slope is judged by the variation it adds, not by its variance per unit", {
  vc <- data.frame(Model = "Model 1", Group = c("id", "id.1"), Term = c("(Intercept)", "f3"), Type = "SD",
                   Estimate = c(0.3, sqrt(1e-6)), Variance = c(0.09, 1e-6), stringsAsFactors = FALSE)
  d <- data.frame(f3 = (1:12)^3)
  expect_identical(negligible_random_terms(vc)$flag, c(FALSE, TRUE))                # per unit of age^3: looks negligible
  expect_identical(negligible_random_terms(vc, data = d)$flag, c(FALSE, FALSE))     # across the records: it is not
})
