test_that("Models 1-8 have the documented fixed effects (quadratic, linear among-individual terms)", {
  fs <- model_formula_strings(c("f1", "f2"), character(0), "linear", character(0), character(0))
  expect_identical(unname(fs[["M1"]]), "f1 + f2")
  expect_identical(unname(fs[["M2"]]), "f1 + f2 + ALR")
  expect_identical(unname(fs[["M3"]]), "mean_f1 + delta_f1 + delta_f2")
  expect_identical(unname(fs[["M4"]]), "f1 * ALR + f2 * ALR")
  expect_identical(unname(fs[["M5"]]), "mean_f1 * (delta_f1 + delta_f2)")
  expect_identical(unname(fs[["M6"]]), "f1 * LS + f2 * LS")
  expect_identical(unname(fs[["M7"]]), "f1 + f2 + ALR + AFR")
})

test_that("extra and built terms are added to the chosen model only", {
  b <- c("f1", "f2")
  fs <- apply_extra_terms(model_formula_strings(b, character(0), "linear", character(0), character(0)), b, list(M1 = "LS"), "linear")
  expect_match(fs[["M1"]], "LS", fixed = TRUE)
  expect_false(grepl("LS", fs[["M2"]], fixed = TRUE))
})

test_that("the random-effect string includes the individual intercept", {
  expect_match(random_term_string("f1", "none", FALSE, character(0)), "(1 | id)", fixed = TRUE)
})

test_that("a slope can be put on every age term (0.20.28)", {
  expect_identical(random_term_string(c("f1", "f2"), "correlated_all"), "(1 + f1 + f2 | id)")
  expect_identical(random_term_string(c("f1", "f2"), "uncorrelated_all"), "(1 | id) + (0 + f1 | id) + (0 + f2 | id)")
  expect_identical(random_term_string(c("f1", "f2"), "correlated"), "(1 + f1 | id)")   # unchanged
  expect_identical(random_term_string("f1", "correlated_all"), "(1 + f1 | id)")        # one-term function collapses
  expect_identical(normalise_slope("correlated_all"), "correlated_all")
  expect_identical(normalise_slope("nonsense"), "none")
  m <- list(map = list(id = "ID"), has_group = FALSE, has_group2 = FALSE, random_terms = character(0))
  expect_identical(random_display(m, "uncorrelated_all", "age + age\u00b2"), "(1 | ID) + (0 + age | ID) + (0 + age\u00b2 | ID)")
  expect_identical(random_display(m, "correlated", "age + age\u00b2"), "(1 + age | ID)")
})
