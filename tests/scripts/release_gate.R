# Release gate: all must pass before tagging a release. Run from the package root:
#   DISAPPR_GOLDEN_REF=/path/to/golden_previous_release.rds Rscript tests/scripts/release_gate.R
# 1. R CMD build + R CMD check (errors and warnings fail the gate; the placeholder maintainer must be replaced first)
# 2. every test tier (tests/scripts/run_all.R), including the golden comparison against the previous release
# 3. exported-code equivalence (part of tests/scripts/robustness_suite.R)
if (!file.exists("DESCRIPTION")) stop("Run from the package root (the folder containing DESCRIPTION).")
rbin <- file.path(R.home("bin"), "R")
# 0. Parse every shipped .R file. A file that does not parse cannot be caught by any later check,
# because the app never starts; this runs first and fails the gate immediately.
cat("== 0. parsing every shipped .R file ==\n")
r_files <- unique(c(list.files("R", pattern = "[.]R$", full.names = TRUE, recursive = TRUE),
                    list.files("inst", pattern = "[.]R$", full.names = TRUE, recursive = TRUE),
                    list.files("tests", pattern = "[.]R$", full.names = TRUE, recursive = TRUE)))
parse_bad <- character(0)
for (f in r_files) {
  ok <- tryCatch({ parse(f); TRUE }, error = function(e) { parse_bad <<- c(parse_bad, sprintf("%s: %s", f, conditionMessage(e))); FALSE })
}
if (length(parse_bad)) {
  cat("PARSE FAILURES:\n"); cat(paste0("  ", parse_bad, collapse = "\n"), "\n")
} else {
  cat(sprintf("  all %d R files parse\n", length(r_files)))
}
gate <- c(`parse all R files` = length(parse_bad) == 0)

# 0b. Build the user interface. parse() cannot catch errors that only appear when a call is evaluated -
# a named argument given twice, an argument a function does not accept, an undefined helper - and any of
# those stops the app opening. Evaluating ui.R builds the whole page object and surfaces them here.
cat("== 0b. building the user interface ==\n")
ui_ok <- tryCatch({
  env <- new.env(parent = globalenv())
  owd <- setwd(file.path("inst", "app")); on.exit(setwd(owd), add = TRUE)
  sys.source("global.R", envir = env)
  ui_obj <- sys.source("ui.R", envir = env, keep.source = FALSE)
  setwd(owd)
  TRUE
}, error = function(e) { cat("  UI BUILD FAILED:", conditionMessage(e), "\n"); FALSE })
if (ui_ok) cat("  the UI builds without error\n")
gate <- c(gate, `build the UI` = isTRUE(ui_ok))

gate <- c(gate)
desc <- read.dcf("DESCRIPTION")
if (grepl("anonymous@example.com|example[.]invalid|REPLACE", desc[1, "Authors@R"])) {
  cat("GATE: DESCRIPTION still has a placeholder maintainer email: replace it with the maintainer's real address.\n")
  gate["metadata"] <- FALSE
} else gate["metadata"] <- TRUE
if (!nzchar(Sys.getenv("DISAPPR_GOLDEN_REF"))) {
  cat("GATE: set DISAPPR_GOLDEN_REF to the previous release's golden capture (tests/golden/golden_capture.R).\n")
  gate["golden reference"] <- FALSE
} else gate["golden reference"] <- TRUE
build_dir <- tempfile("disappR_build"); dir.create(build_dir)
st <- suppressWarnings(system2(rbin, c("CMD", "build", shQuote(normalizePath("."))), stdout = TRUE, stderr = TRUE))
tarball <- list.files(".", pattern = "^disappR_.*[.]tar[.]gz$", full.names = TRUE)
if (!length(tarball)) {
  cat(tail(st, 5), sep = "\n"); gate["R CMD check"] <- FALSE
} else {
  file.rename(tarball[[1]], file.path(build_dir, basename(tarball[[1]])))
  ck <- suppressWarnings(system2(rbin, c("CMD", "check", "--no-manual", "--as-cran", shQuote(file.path(build_dir, basename(tarball[[1]])))),
                                 stdout = TRUE, stderr = TRUE))
  status_line <- grep("^Status:", ck, value = TRUE)
  cat(tail(ck, 15), sep = "\n")
  gate["R CMD check"] <- length(status_line) == 1 && !grepl("ERROR|WARNING", status_line)
}
gate["tests"] <- identical(suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), "tests/scripts/run_all.R")), 0L)
cat("\nRelease gate:", paste(names(gate), ifelse(gate, "pass", "FAIL"), sep = ": ", collapse = "; "), "\n")
if (!all(gate)) quit(status = 1)
cat("All gates passed.\n")
