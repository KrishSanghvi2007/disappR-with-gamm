# The sampling grid must follow the ALR and AFR the models use; since 0.24.2 the window runs from AFR to ALR, both included. Field data often contain records without a trait
# value (a capture without a weighing): those still count as recorded ages for ALR, and individuals never measured
# for the trait enter no model. Checked against all bundled datasets and simulated designs before release.

grid_dat <- function() data.frame(
  id    = c("A", "A", "A", "B", "B", "C"),
  age   = c(1, 2, 3, 1, 4, 2),
  trait = c(5, 6, NA, 4, 3, NA),     # A: last capture not measured; C: never measured
  alr   = c(3, 3, 3, 6, 6, 2),       # B: mapped ALR later than its last record (seen alive, not measured)
  life  = NA_real_,
  entry = c(1, 1, 1, 1, 1, 2),
  stringsAsFactors = FALSE)

test_that("individuals never measured for the trait are left out of the grid and counted", {
  g <- build_missing_grid(grid_dat())
  expect_false("C" %in% g$id)
  expect_equal(attr(g, "n_no_trait"), 1)
})

test_that("the ALR marker sits on the ALR the models use, even when that record has no trait value", {
  g <- build_missing_grid(grid_dat())
  x <- grid_display(g, c("A", "B"), life_known = FALSE)
  a <- x[x$id == "A", ]
  expect_identical(a$status[a$age == 3], "Last record (ALR)")
  expect_false(any(a$status[a$age > 3] %in% c("Missed", "Observed")))
})

test_that("a mapped ALR later than the last record keeps those occasions expected, and nothing follows it", {
  g <- build_missing_grid(grid_dat())
  x <- grid_display(g, c("A", "B"), life_known = FALSE)
  b <- x[x$id == "B", ]
  expect_identical(b$status[b$age == 6], "Last record (ALR)")
  expect_identical(b$status[b$age == 5], "Missed")
  expect_identical(b$status[b$age == 4], "Observed")
  expect_false("Death (known LS)" %in% x$status)
})

test_that("the window opens at the AFR: a record without a trait value at the AFR counts as missed (0.24.2)", {
  # D was captured at age 1 without a trait value. Dataset-wide AFR (entry = 1): the window opens at 1, so age 1 is the
  # AFR and a missed occasion. Trait-specific AFR (entry = 2): the window opens at 2, the first trait value.
  wide <- data.frame(id = rep(c("D", "H"), each = 3), age = rep(c(1, 2, 3), 2), trait = c(NA, 5, 6, 4, 5, 6),
                     alr = 3, life = NA_real_, entry = 1)
  g <- build_missing_grid(wide)
  x <- grid_display(g, c("D", "H"), life_known = FALSE)
  expect_identical(x$status[x$id == "D" & x$age == 1], "First record (AFR)")
  expect_true(g$missing[g$id == "D" & g$age == 1])
  expect_identical(x$status[x$id == "D" & x$age == 2], "Observed")
  spec <- wide
  spec$entry[spec$id == "D"] <- 2
  g2 <- build_missing_grid(spec)
  x2 <- grid_display(g2, c("D", "H"), life_known = FALSE)
  expect_identical(x2$status[x2$id == "D" & x2$age == 1], "Not expected")
  expect_false(any(g2$id == "D" & g2$age == 1))
  expect_identical(x2$status[x2$id == "D" & x2$age == 2], "First record (AFR)")
})

test_that("with known lifespan nothing after the ALR is counted as missed", {
  d <- data.frame(id = rep(c("E", "F"), c(2, 4)), age = c(1, 2, 1, 2, 3, 4), trait = c(5, 6, 5, 6, 5, 4),
                  alr = rep(c(2, 4), c(2, 4)), life = rep(c(6, 4), c(2, 4)), entry = 1)
  g <- build_missing_grid(d)
  expect_false(any(g$id == "E" & g$age > 2))
  x <- grid_display(g, c("E", "F"), life_known = TRUE)
  expect_true(all(x$status[x$id == "E" & x$age > 2] == "Not expected"))
  expect_false(any(grepl("Death", x$status)))
})

test_that("with an age at first expression, occasions from that age to the first record count as missed", {
  d <- data.frame(id = "G", age = c(3, 4), trait = c(5, 6), alr = 4, life = NA_real_, entry = 3)
  x <- grid_display(build_missing_grid(d, start_mode = "same", start_age = 1), "G", life_known = FALSE)
  expect_identical(x$status[x$age == 1], "Missed")
  expect_identical(x$status[x$age == 2], "Missed")
  expect_identical(x$status[x$age == 3], "First record (AFR)")   # the AFR is marked inside the window (0.24.2)
})
