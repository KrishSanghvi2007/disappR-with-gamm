test_that("the decomposition uses population occasions, not an individual's own consecutive records", {
  # A is recorded at 1, 2, 3; B at 1, 2, 5, 6 on a population grid of 1..6.
  d <- data.frame(id = c("A","A","A","B","B","B","B"), age = c(1,2,3,1,2,5,6),
                  trait = c(10,12,11,20,19,15,14), stringsAsFactors = FALSE)
  b <- standardise_data(d, list(id = "id", age = "age", trait = "trait", alr = "__AUTO_LAST__",
                                life = "", entry = "__AUTO_FIRST__", covars = character(0),
                                cov_factor = character(0), cov_int = character(0), group = "",
                                nested = TRUE, random = character(0), censor = "", censor_value = "",
                                condition = "", trials = "", start_mode = "afr", start_age = NA_real_,
                                age_round = NA_real_, cov_age = character(0)))
  de <- decomposition_trajectory(b$data)
  expect_true(nrow(de) > 0)
  expect_true("segment" %in% names(de))
  # B's 2 -> 5 gap must not become a transition: the chain breaks into two segments
  expect_equal(length(unique(de$segment)), 2L)
  expect_true(all(de$age %in% c(1, 2, 3, 5, 6)))
  # the link 2 -> 3 has one contributing individual (A only), 1 -> 2 has two
  expect_equal(de$n_pairs[de$age == 2 & de$segment == 1][[1]], 2L)
})

test_that("among-individual polynomial terms are powers of mean age, not the mean of age^2", {
  set.seed(1)
  d <- do.call(rbind, lapply(1:40, function(i) {
    ages <- 1:sample(4:9, 1)
    data.frame(id = paste0("i", i), age = ages, trait = 10 - 0.2 * ages + rnorm(length(ages)), stringsAsFactors = FALSE)
  }))
  b <- standardise_data(d, list(id = "id", age = "age", trait = "trait", alr = "__AUTO_LAST__",
                                life = "", entry = "__AUTO_FIRST__", covars = character(0),
                                cov_factor = character(0), cov_int = character(0), group = "",
                                nested = TRUE, random = character(0), censor = "", censor_value = "",
                                condition = "", trials = "", start_mode = "afr", start_age = NA_real_,
                                age_round = NA_real_, cov_age = character(0)))
  prep <- prepare_model_data(b$data, "Quadratic", character(0), TRUE, FALSE, character(0))
  dd <- prep$data
  expect_true(all(c("mean_f1_2", "mean_f1_3") %in% names(dd)))
  expect_equal(dd$mean_f1_2, dd$mean_f1^2)
  # and that this is genuinely different from the individual mean of age^2 (mean_f2)
  expect_false(isTRUE(all.equal(dd$mean_f1_2, dd$mean_f2)))
  fs <- model_formula_strings(prep$basis, among = "same")
  expect_true(grepl("mean_f1_2", fs[["M3"]], fixed = TRUE))
  expect_false(grepl("mean_f2", fs[["M3"]], fixed = TRUE))
})
