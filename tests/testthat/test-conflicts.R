# An individual has one ALR, one lifespan and one AFR. A reviewer found that conflicting values in different records
# were silently averaged. 0.20.1 kept averaging, with a warning; but the mean can be impossible (the Alpine swift's
# mean lifespan fell before its own last breeding record). Since 0.20.2 loading stops with a data-integrity error
# that names the individuals, and mapping$inconsistent = "exclude" drops them instead.

conflict_map <- function(...) {
  utils::modifyList(list(id = "id", age = "age", trait = "trait", alr = "__AUTO_LAST__", life = "LS", entry = "__AUTO_FIRST__",
       covars = character(0), cov_factor = character(0), cov_int = character(0), group = "", nested = TRUE,
       random = character(0), censor = "", censor_value = "", condition = "", trials = "", start_mode = "afr",
       start_age = NA_real_, age_round = NA_real_, cov_age = character(0)), list(...))
}

test_that("conflicting values within an individual are found, and consistent ones are not", {
  d <- data.frame(id = c("a", "a", "b", "b"), age = c(1, 2, 1, 2), trait = 1:4, alr = 2, life = c(3, 4, 5, 5), entry = 1)
  cf <- individual_value_conflicts(d)
  expect_identical(cf$life, "a")
  expect_null(cf$alr)
  expect_null(cf$entry)
})

test_that("conflicts stop loading with a data-integrity error that names the individuals and the column", {
  raw <- data.frame(id = rep(c("a", "b", "c"), each = 2), age = rep(1:2, 3), trait = c(5, 6, 5, 6, 5, 6), LS = c(3, 4, 5, 5, 4, NA))
  err <- tryCatch(standardise_data(raw, conflict_map()), disappr_integrity_error = function(e) e)
  expect_s3_class(err, "disappr_integrity_error")
  expect_identical(err$conflicts$life, "a")          # a blank value (c) is not a conflict
  expect_match(conditionMessage(err), "lifespan (column 'LS')", fixed = TRUE)
  expect_match(conditionMessage(err), "inconsistent = \"exclude\"", fixed = TRUE)
})

test_that("inconsistent = 'exclude' drops those individuals and the integrity check reports them", {
  raw <- data.frame(id = rep(c("a", "b", "c"), each = 2), age = rep(1:2, 3), trait = c(5, 6, 5, 6, 5, 6), LS = c(3, 4, 5, 5, 4, 4))
  b <- standardise_data(raw, conflict_map(inconsistent = "exclude"))
  expect_false("a" %in% b$data$id)
  expect_setequal(unique(b$data$id), c("b", "c"))
  expect_identical(b$meta$inconsistent_ids, "a")
  expect_identical(b$meta$n_inconsistent_rows, 2L)
  row <- data_integrity(b$data, b$meta)$table
  row <- row[row$Check == "One ALR, lifespan and AFR per individual", , drop = FALSE]
  expect_identical(row$Status, "Warning")
  expect_match(row$Result, "excluded")
})

test_that("the mean of conflicting values can be impossible: the swift's would fall before its last record", {
  f <- system.file("app", "data", "moullec_2023_alpine_swift_reproduction.csv", package = "disappR")
  if (!nzchar(f)) f <- file.path("..", "..", "inst", "app", "data", "moullec_2023_alpine_swift_reproduction.csv")
  skip_if_not(file.exists(f), "example data not found")
  d <- utils::read.csv(f, stringsAsFactors = FALSE)
  s <- d[d$ring == "Frontiers_000304", , drop = FALSE]
  expect_lt(mean(s$lifespan), max(s$age[is.finite(s$laying_date)]))
})

test_that("every bundled example loads, and only the documented individuals are excluded", {
  skip_if_not(nzchar(system.file("app", "data", package = "disappR")), "package data not installed")
  documented <- list(moullec_swift = "Frontiers_000304", allain = c("D037", "D114", "D165"))
  for (k in names(EXAMPLES)) {
    ex <- disappr_example(k)
    x <- disappr_prepare(ex$data, ex$mapping)
    expect_gt(nrow(x$data), 0)
    expect_setequal(x$meta$inconsistent_ids %||% character(0), documented[[k]] %||% character(0))
  }
})

test_that("the beetle example identifies every beetle uniquely", {
  f <- system.file("app", "data", "sanghvi_2022_beetle_female_fecundity.csv", package = "disappR")
  if (!nzchar(f)) f <- file.path("..", "..", "inst", "app", "data", "sanghvi_2022_beetle_female_fecundity.csv")
  skip_if_not(file.exists(f), "example data not found")
  d <- utils::read.csv(f, stringsAsFactors = FALSE)
  expect_true("individual" %in% names(d))
  expect_equal(length(individual_value_conflicts(data.frame(id = d$individual, life = d$Adult_lifespan))), 0)
})
