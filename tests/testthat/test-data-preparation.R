
# ---- 0.20.18: duplicates are only collapsed when the rows are identical ----
test_that("exact duplicates are separated from repeated measurements and named", {
  out <- data.frame(id = c("A", "A", "B", "B", "C"), age = c(1, 1, 2, 2, 3), trait = c(5, 5, 7, 9, 4),
                    cv_diet = c("x", "x", "x", "x", "y"), stringsAsFactors = FALSE)
  d <- duplicate_report(out)
  expect_identical(d$identical, "A\r1")
  expect_identical(d$differing, "B\r2")
  expect_identical(d$columns, "trait")
  expect_match(d$examples, "^B at age 2$")
  expect_identical(duplicate_report(out[5, , drop = FALSE])$identical, character(0))
})

test_that("individuals appearing under more than one group are named for the data checks", {
  id <- c("A", "A", "B", "B", "C", "C")
  grp <- c("G1", "G2", "G1", "G1", "G1", NA)
  r <- multi_group_report(id, grp)
  expect_identical(r$ids, c("A", "C"))
  expect_identical(r$n_gap, 1L)
  expect_identical(r$examples, c("A (G1, G2)", "C ((no group), G1)"))
  expect_identical(multi_group_report(character(0), character(0))$ids, character(0))
})

test_that("data without a grouping column still standardises (0.20.27 regression)", {
  d <- data.frame(id = rep(c("a", "b", "c"), each = 4), age = rep(1:4, 3), trait = rnorm(12), stringsAsFactors = FALSE)
  p <- standardise_data(d, list(id = "id", age = "age", trait = "trait"), "keep")
  expect_true(is.data.frame(p$data) && nrow(p$data) == 12)
  expect_identical(p$meta$n_multi_group, 0L)
  expect_identical(p$meta$multi_group_examples, character(0))
  expect_identical(p$meta$n_multi_group_gap, 0L)
  expect_true(is.data.frame(data_integrity(p$data, p$meta)$table))
})

test_that("numbers survive a comma decimal mark (0.21.12)", {
  op <- options(OutDec = ",")
  on.exit(options(op))
  expect_identical(safe_numeric(c(1.5, 2.25)), c(1.5, 2.25))
  expect_identical(safe_numeric(c("1.5", "2")), c(1.5, 2))
  expect_identical(names_num(table(c(0.5, 0.5, 1.5))), c(0.5, 1.5))
  d <- data.frame(id = rep(c("a", "b"), each = 3), age = rep(c(0.5, 1.5, 2.5), 2), trait = c(1.1, 2.2, 3.3, 1.4, 2.5, 3.6))
  p <- standardise_data(d, list(id = "id", age = "age", trait = "trait"), "keep")
  expect_true(all(is.finite(p$data$trait)) && all(is.finite(p$data$age)))
})

test_that("a mapped column missing from the data is a classed, readable error", {
  d <- data.frame(id = "a", age = 1, trait = 2)
  expect_error(standardise_data(d, list(id = "id", age = "AGE", trait = "trait"), "keep"), class = "disappr_input_error")
})
