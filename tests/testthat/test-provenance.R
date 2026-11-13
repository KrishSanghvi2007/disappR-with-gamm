test_that("every model comparison stores its provenance", {
  t <- toy_prepared(n_id = 60, seed = 10)
  r <- fit_model_suite(t$prep$data, t$prep$meta, c("M1", "M2"), "Quadratic", "gaussian")
  pv <- r$provenance
  expect_true(all(c("software", "data", "model", "transformations", "settings", "warnings") %in% names(pv)))
  expect_identical(nchar(pv$data$fingerprint_md5), 32L)
  expect_equal(as.integer(pv$data$rows), nrow(r$data))
  pl <- provenance_lines(pv)
  expect_gte(length(pl), 4)
  expect_match(pl[[1]], "Analysed data", fixed = TRUE)
  expect_true(any(grepl("Model settings", pl, fixed = TRUE)))
  expect_true(any(grepl("Software", pl, fixed = TRUE)))
})

test_that("optional packages and versions are reported", {
  st <- optional_package_status()
  expect_true(all(c("Package", "Installed", "Needed_for") %in% names(st)))
  expect_match(dependency_versions_line(), "disappR", fixed = TRUE)
})
