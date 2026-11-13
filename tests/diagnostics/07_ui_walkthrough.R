# ---------------------------------------------------------------------------
# Diagnostic 7: headless walkthrough of the real interface (shinytest2)
#
#   install.packages("shinytest2")
#   Rscript tests/diagnostics/07_ui_walkthrough.R
#
# Drives an actual browser: loads every example, ticks every model, switches family,
# ageing function, random structure and scale, and visits every tab. This is what
# catches render-time errors that testServer cannot see.
# ---------------------------------------------------------------------------
# Safe to run from the R console as well as from Rscript: only a non-interactive session exits.
`%||%` <- function(a, b) if (is.null(a) || !length(a) || (length(a) == 1 && is.na(a))) b else a
finish <- function(fails, message_ok, message_bad) {
  cat("\n", if (fails) sprintf(message_bad, fails) else message_ok, sep = "")
  if (!interactive()) quit(status = if (fails) 1L else 0L)
  invisible(fails)
}

# shinytest2 calls testthat::skip_on_cran() when it starts an app; outside a test that skip is raised as the error
# "Reason: On CRAN" unless NOT_CRAN is set.
Sys.setenv(NOT_CRAN = "true")
library(shinytest2)
library(disappR)
chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
if (is.null(chrome) || !nzchar(chrome)) {
  mac <- "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
  if (file.exists(mac)) Sys.setenv(CHROMOTE_CHROME = mac) else {
    cat("Chrome (or Chromium) was not found, so the browser walkthrough cannot run.\n",
        "Install Google Chrome, or point to a Chromium-based browser with\n",
        "  Sys.setenv(CHROMOTE_CHROME = \"/path/to/browser\")\n", sep = "")
    if (!interactive()) quit(status = 2L) else stop("no Chrome found", call. = FALSE)
  }
}
app_dir <- system.file("app", package = "disappR")
if (!nzchar(app_dir)) app_dir <- "inst/app"

fails <- 0L
seen <- 0L
report <- function(app, label) {
  logs <- app$get_logs()
  msg <- as.character(logs$message %||% logs)
  new <- if (length(msg) > seen) msg[(seen + 1L):length(msg)] else character(0)
  seen <<- length(msg)
  # shinytest2's own notes about inputs that changed no output are timing, not errors
  new <- new[!grepl("did not update any output values|Unable to find input binding", new)]
  errs <- grep("Error|Warning in|object .* not found", new, value = TRUE)
  errs <- setdiff(errs, grep("^Warning in .*deprecat", errs, value = TRUE))
  if (length(errs)) {
    fails <<- fails + 1L
    cat("FAIL", label, "\n"); for (e in utils::head(errs, 5)) cat("     ", substr(e, 1, 140), "\n")
  } else cat("ok  ", label, "\n")
}

app <- AppDriver$new(app_dir, name = "disappR", height = 1000, width = 1500, load_timeout = 60000)
on.exit(app$stop(), add = TRUE)

for (key in names(disappR:::EXAMPLES)) {
  app$set_inputs(data_source = "example", example_id = key, wait_ = TRUE, timeout_ = 60000)
  app$wait_for_idle(timeout = 60000)
  report(app, paste("example:", key))
}

app$set_inputs(example_id = "fly")
app$wait_for_idle(timeout = 60000)
for (rs in c("none", "uncorrelated", "correlated", "uncorrelated_all", "correlated_all")) {
  app$set_inputs(random_structure = rs, wait_ = FALSE); app$wait_for_idle(timeout = 60000)
  report(app, paste("random structure:", rs))
}
for (fn in c("Linear", "Quadratic", "Cubic", "Logarithmic", "Asymptotic exponential")) {
  app$set_inputs(model_age_function = fn, wait_ = FALSE); app$wait_for_idle(timeout = 60000)
  report(app, paste("ageing function:", fn))
}
for (fam in c("gaussian", "poisson", "nbinom2", "zinb")) {
  app$set_inputs(model_family = fam, wait_ = FALSE); app$wait_for_idle(timeout = 60000)
  report(app, paste("family:", fam))
}
app$set_inputs(model_family = "zinb", wait_ = FALSE)
for (m in paste0("use_M", 1:10)) {
  do.call(app$set_inputs, c(stats::setNames(list(TRUE), m), list(wait_ = FALSE)))
}
app$wait_for_idle(timeout = 60000)
report(app, "all ten models ticked")
app$click("fit_models"); app$wait_for_idle(timeout = 300000)
report(app, "all ten models fitted (fly, zero-inflated negative binomial)")
for (tab in c("overview", "data", "visual", "sampling", "individual", "models", "summary")) {
  app$set_inputs(tabs = tab, wait_ = FALSE); app$wait_for_idle(timeout = 120000)
  report(app, paste("tab:", tab))
}
app$expect_screenshot()
finish(fails, "No errors logged anywhere in the walkthrough.\n", "%d step(s) logged an error.\n")
