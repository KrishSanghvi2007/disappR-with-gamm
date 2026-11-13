# The interface must build. parse() does not catch errors that only occur when a call is evaluated - a named
# argument supplied twice, an argument a function does not take, an undefined helper - and each of those
# prevents the app from opening at all. This test evaluates ui.R so that such errors fail here instead.

test_that("the user interface builds without error", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("shinydashboard")
  app_dir <- system.file("app", package = "disappR")
  if (!nzchar(app_dir)) app_dir <- testthat::test_path("..", "..", "inst", "app")
  skip_if_not(file.exists(file.path(app_dir, "ui.R")), "app directory not found")
  owd <- setwd(app_dir)
  on.exit(setwd(owd), add = TRUE)
  env <- new.env(parent = globalenv())
  expect_no_error(sys.source("global.R", envir = env))
  expect_no_error(ui <- eval(parse("ui.R", keep.source = FALSE), envir = env))
})

test_that("no box is given the same argument twice", {
  app_dir <- system.file("app", package = "disappR")
  if (!nzchar(app_dir)) app_dir <- testthat::test_path("..", "..", "inst", "app")
  skip_if_not(file.exists(file.path(app_dir, "ui.R")), "app directory not found")
  src <- paste(readLines(file.path(app_dir, "ui.R"), warn = FALSE), collapse = "\n")
  starts <- gregexpr("box\\(", src)[[1]]
  for (st in starts[starts > 0]) {
    depth <- 0; i <- st + 3; args <- character(0); cur <- ""; inq <- ""
    repeat {
      ch <- substr(src, i, i)
      if (nzchar(inq)) { cur <- paste0(cur, ch); if (ch == inq) inq <- "" }
      else if (ch %in% c('"', "'")) { inq <- ch; cur <- paste0(cur, ch) }
      else if (ch %in% c("(", "[", "{")) { depth <- depth + 1; if (depth > 1) cur <- paste0(cur, ch) }
      else if (ch %in% c(")", "]", "}")) { depth <- depth - 1; if (depth == 0) break; cur <- paste0(cur, ch) }
      else if (ch == "," && depth == 1) { args <- c(args, cur); cur <- "" }
      else cur <- paste0(cur, ch)
      i <- i + 1
      if (i > nchar(src)) break
    }
    args <- c(args, cur)
    nm <- sub("^\\s*([A-Za-z_.][A-Za-z0-9_.]*)\\s*=.*$", "\\1", args[grepl("^\\s*[A-Za-z_.][A-Za-z0-9_.]*\\s*=[^=]", args)])
    expect_false(any(duplicated(nm)),
                 info = sprintf("box() at character %d repeats: %s", st, paste(unique(nm[duplicated(nm)]), collapse = ", ")))
  }
})

test_that("every engine file is loaded in both source and installed mode", {
  # A file in R/ that is missing from global.R's list is never sourced when the app runs from a checkout,
  # and one missing from DESCRIPTION's Collate is never installed. Either way its functions do not exist, and
  # because many handlers return NULL on error, the failure shows as an empty panel rather than a message.
  # effects.R was missing from both in 0.13.0 to 0.15.4.
  pkg_root <- testthat::test_path("..", "..")
  skip_if_not(dir.exists(file.path(pkg_root, "R")) && file.exists(file.path(pkg_root, "inst", "app", "global.R")), "package source not available")
  r_files <- basename(list.files(file.path(pkg_root, "R"), pattern = "[.]R$"))
  g <- paste(readLines(file.path(pkg_root, "inst", "app", "global.R"), warn = FALSE), collapse = "\n")
  listed <- regmatches(g, gregexpr('"[^"]+[.]R"', g))[[1]]
  listed <- gsub('"', "", listed)
  expect_setequal(setdiff(r_files, c("run_app.R", "api.R")), intersect(listed, r_files))
  desc <- read.dcf(file.path(pkg_root, "DESCRIPTION"))
  if ("Collate" %in% colnames(desc)) {
    coll <- gsub("'", "", strsplit(trimws(desc[1, "Collate"]), "[[:space:]]+")[[1]])
    expect_setequal(r_files, coll)
  }
})
