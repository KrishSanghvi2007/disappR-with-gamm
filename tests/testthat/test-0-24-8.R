# 0.24.8: the Modelling tab starts with random slopes only when the data support them well

test_that("well-supported slopes are the default; sparse data start with a random intercept", {
  # many individuals recorded at many ages: slopes (and, with four or more ages for most, every age term)
  rich <- data.frame(id = rep(sprintf("i%03d", 1:80), each = 6), age = rep(1:6, 80), trait = stats::rnorm(480))
  expect_true(default_random_structure(rich) %in% c("correlated", "correlated_all"))
  expect_identical(default_random_structure(rich), "correlated_all")
  # three ages for most individuals: a slope, but not every age term
  mid <- data.frame(id = rep(sprintf("m%03d", 1:80), each = 3), age = rep(1:3, 80), trait = stats::rnorm(240))
  expect_identical(default_random_structure(mid), "correlated")
  # one or two records per individual: a random intercept
  sparse <- data.frame(id = rep(sprintf("s%03d", 1:100), each = 2), age = rep(1:2, 100), trait = stats::rnorm(200))
  expect_identical(default_random_structure(sparse), "none")
  expect_identical(default_random_structure(NULL), "none")
})
