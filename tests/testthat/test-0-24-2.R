# 0.24.2: trait-specific automatic ALR and AFR (default) against dataset-wide ALR and AFR (bundled examples)

base_map <- function(trait, specific = TRUE, ...) {
  utils::modifyList(list(id = "id", age = "age", trait = trait, alr = "__AUTO_LAST__", life = "", entry = "__AUTO_FIRST__",
                         condition = "", covars = character(0), cov_factor = character(0), cov_int = character(0),
                         group = "", nested = TRUE, random = character(0), censor = "", censor_value = "",
                         start_mode = "afr", start_age = NA_real_, age_round = NA_real_, cov_age = character(0),
                         trait_specific_ages = specific), list(...))
}
two_traits <- function() data.frame(
  id = rep(c("P", "Q"), each = 5), age = rep(1:5, 2),
  A = c(NA, 5, 7, 6, NA, 4, 5, 6, 7, 8),           # P: trait A measured at ages 2-4
  B = c(3, 4, 0, NA, NA, 2, NA, 3, 4, NA),          # P: trait B at ages 1-3 (zero is a value)
  ALRcol = rep(c(9, 5), each = 5),
  stringsAsFactors = FALSE)
per_id <- function(b, v) tapply(b$data[[v]], b$data$id, function(x) x[[1]])

test_that("ticked: ALR and AFR are the last and first ages with a trait value; unticked: of any record", {
  d <- two_traits()
  a_t <- standardise_data(d, base_map("A", TRUE))
  a_u <- standardise_data(d, base_map("A", FALSE))
  expect_equal(unname(per_id(a_t, "alr")[["P"]]), 4)
  expect_equal(unname(per_id(a_t, "entry")[["P"]]), 2)
  expect_equal(unname(per_id(a_u, "alr")[["P"]]), 5)
  expect_equal(unname(per_id(a_u, "entry")[["P"]]), 1)
  expect_true(isTRUE(a_t$meta$trait_specific_ages))
  expect_false(isTRUE(a_u$meta$trait_specific_ages))
})

test_that("each trait gets its own ALR and AFR from the same file, and zero counts as a value", {
  d <- two_traits()
  b_t <- standardise_data(d, base_map("B", TRUE))
  expect_equal(unname(per_id(b_t, "alr")[["P"]]), 3)     # the zero at age 3 is a value
  expect_equal(unname(per_id(b_t, "entry")[["P"]]), 1)
  expect_equal(unname(per_id(b_t, "alr")[["Q"]]), 4)
  b_u <- standardise_data(d, base_map("B", FALSE))
  expect_equal(unname(per_id(b_u, "alr")[["P"]]), 5)
})

test_that("mean age uses only ages with a trait value, whichever option is chosen", {
  d <- two_traits()
  for (sp in c(TRUE, FALSE)) {
    im <- individual_metrics(standardise_data(d, base_map("A", sp))$data)
    expect_equal(im$mean_age[im$id == "P"], 3)
  }
})

test_that("mapped ALR and AFR columns are used as supplied under either option", {
  d <- two_traits()
  for (sp in c(TRUE, FALSE)) {
    b <- standardise_data(d, base_map("A", sp, alr = "ALRcol"))
    expect_equal(unname(per_id(b, "alr")[["P"]]), 9)
  }
})

test_that("'lifespan = last recorded age' follows the same option", {
  d <- two_traits()
  expect_equal(unname(per_id(standardise_data(d, base_map("A", TRUE, life = "__AUTO_LAST__")), "life")[["P"]]), 4)
  expect_equal(unname(per_id(standardise_data(d, base_map("A", FALSE, life = "__AUTO_LAST__")), "life")[["P"]]), 5)
})

test_that("bundled examples are dataset-wide; uploads and simulations are trait-specific", {
  expect_true(all(vapply(EXAMPLES, function(e) isFALSE(e$mapping$trait_specific_ages), logical(1))))
  sim <- simulate_toy_data(list(n_id = 40, seed = 3, afr_mode = "individual"))
  tm <- toy_mapping(sim)
  expect_true(isTRUE(tm$trait_specific_ages))
  expect_identical(tm$entry, "__AUTO_FIRST__")              # the observed AFR
  expect_true(isTRUE(guess_mapping(two_traits())$trait_specific_ages))
})

test_that("the missingness window runs from AFR to ALR, both included, under either option", {
  d <- two_traits()[1:5, ]
  g_t <- build_missing_grid(standardise_data(d, base_map("A", TRUE))$data)
  expect_equal(sort(g_t$age[g_t$id == "P"]), 2:4)
  expect_false(any(g_t$missing[g_t$id == "P"]))
  g_u <- build_missing_grid(standardise_data(d, base_map("A", FALSE))$data)
  expect_equal(sort(g_u$age[g_u$id == "P"]), 1:5)
  expect_equal(sum(g_u$missing[g_u$id == "P"]), 2)         # ages 1 and 5: recorded, trait not measured
  x <- grid_display(g_u, "P", life_known = FALSE)
  expect_identical(x$status[x$age == 1], "First record (AFR)")
  expect_identical(x$status[x$age == 5], "Last record (ALR)")
})

test_that("the exported script computes ALR and AFR the same way", {
  # enough individuals for the models to fit (two are too few, and the script needs a fitted suite)
  # (half the Q copies lose their age-5 record, so ALR varies among individuals under either option)
  d <- do.call(rbind, lapply(1:8, function(k) {
    x <- transform(two_traits(), id = paste0(id, k), A = A + k %% 3)
    if (k %% 2 == 0) x <- x[!(x$id == paste0("Q", k) & x$age == 5), ]
    x
  }))
  for (sp in c(TRUE, FALSE)) {
    b <- standardise_data(d, base_map("A", sp))
    r <- fit_model_suite(b$data, b$meta, models = c("M1", "M2"), age_function = "Linear")
    code <- paste(model_r_code(r, b$meta, "two_traits.csv"), collapse = "\n")
    expect_true(grepl(if (sp) "trait-specific ALR and AFR" else "dataset-wide ALR and AFR", code, fixed = TRUE))
  }
})

test_that("disappr_mapping() is trait-specific by default and can be switched", {
  expect_true(isTRUE(disappr_mapping(id = "id", age = "age", trait = "A")$trait_specific_ages))
  expect_false(isTRUE(disappr_mapping(id = "id", age = "age", trait = "A", trait_specific_ages = FALSE)$trait_specific_ages))
})
