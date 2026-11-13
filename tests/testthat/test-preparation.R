test_that("standardise_data returns model-ready data and metadata", {
  t <- toy_prepared()
  expect_s3_class(t$prep$data, "data.frame")
  expect_true(all(c("id", "age", "trait") %in% names(t$prep$data)))
  expect_true(is.list(t$prep$meta))
  expect_gt(length(unique(t$prep$data$id)), 50)
})

test_that("identifiers stay text: '001' and '1' remain different individuals", {
  f <- tempfile(fileext = ".csv")
  writeLines(c("ID,age,trait", "001,1,5", "001,2,6", "1,1,4", "1,2,5"), f)
  r <- read_user_csv(f, basename(f))
  expect_identical(sort(unique(r$data$ID)), c("001", "1"))
  p <- standardise_data(r$data, example_map(id = "ID", age = "age", trait = "trait"))
  expect_equal(length(unique(p$data$id)), 2)
})


test_that("covariate type warnings flag a mismatch but never block the mapping", {
  df <- data.frame(txt = rep(c("a", "b", "c"), 4), dose = rep(c(0.5, 1.5, 2.5), 4), grp = rep(1:3, 4), stringsAsFactors = FALSE)
  w <- covariate_type_warnings(df, num = "txt", fac = c("dose", "grp"))
  expect_true(any(grepl("'txt' is mapped as CONTINUOUS", w)))
  expect_true(any(grepl("'dose' is mapped as CATEGORICAL", w)))      # decimals: warned even with 3 levels
  expect_false(any(grepl("'grp'", w)))                                # whole-number group codes are fine
})
