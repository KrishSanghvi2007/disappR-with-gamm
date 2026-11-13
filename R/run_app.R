#' Launch the disappR workflow
#'
#' Starts the Shiny dashboard for diagnosing and modelling selective
#' disappearance and selective appearance in longitudinal ageing data.
#'
#' @param ... Arguments passed to [shiny::runApp()], such as `port`,
#'   `launch.browser` or `host`.
#'
#' @return Runs the Shiny application; does not return a value.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function(...) {
  op <- options(OutDec = ".")
  on.exit(options(op), add = TRUE)
  needed <- c("shiny", "shinydashboard", "ggplot2", "lme4")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Missing required package(s): ", paste(missing, collapse = ", "),
         ". Install them with install.packages() and try again.", call. = FALSE)
  }
  optional <- c(glmmTMB = "count, proportion and binary traits", lmerTest = "Gaussian p-values", nlme = "the exponential ageing function",
                DHARMa = "residual checks", performance = "model diagnostics", see = "diagnostic plots", codetools = "R code export",
                callr = "running step 4 in a separate R process")
  absent <- names(optional)[!vapply(names(optional), requireNamespace, logical(1), quietly = TRUE)]
  if (length(absent)) {
    message("Optional packages not installed (", paste(paste0(absent, ": ", optional[absent]), collapse = "; "),
            "). install.packages(c(", paste0('"', absent, '"', collapse = ", "), "))")
  }
  app_dir <- system.file("app", package = "disappR")
  if (!nzchar(app_dir)) stop("Could not find the app directory. Re-install disappR and try again.", call. = FALSE)
  # The upload limit (50 MB) is set in the app's global.R, the one place it is defined.
  shiny::runApp(app_dir, ...)
}
