# ====================================================================
# disappR - global definitions for the Shiny app
# ====================================================================
# The statistical engine lives in the package's R/ files (R/import.R, R/formulas.R, R/model-fit.R, ...), shared by
# the app, the R interface (disappr_prepare(), disappr_fit(), ...) and exported code. This file makes the same engine
# objects available to ui.R and server.R as when everything lived here:
#   * from a source checkout (shiny::runApp("inst/app"), launch_app.R, the tests) the engine files are sourced from
#     ../../R in the package's collation order;
#   * from an app folder bundled for deployment (inst/scripts/bundle_app.R) they are sourced from ./engine;
#   * from an installed package (run_app()) the objects of the package namespace are used.

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(ggplot2)
})

options(stringsAsFactors = FALSE)
options(shiny.maxRequestSize = 50 * 1024^2)   # uploads up to 50 MB: the one place this limit is set

DISAPPR_ENGINE_FILES <- c("utils.R", "provenance.R", "simulation.R", "import.R", "validation.R", "data-preparation.R", "formulas.R", "model-fit.R", "model-comparison.R", "diagnostics.R", "trajectories.R", "disappearance.R", "missingness.R", "interpretation.R", "effects.R", "evidence.R", "ui-helpers.R", "export.R")
.disappr_app_env <- environment()
.disappr_engine_dir <- if (all(file.exists(file.path("..", "..", "R", DISAPPR_ENGINE_FILES)))) {
  file.path("..", "..", "R")
} else if (all(file.exists(file.path("engine", DISAPPR_ENGINE_FILES)))) {
  "engine"
} else ""
if (nzchar(.disappr_engine_dir)) {
  for (.disappr_file in DISAPPR_ENGINE_FILES) {
    source(file.path(.disappr_engine_dir, .disappr_file), local = .disappr_app_env, encoding = "UTF-8")
    if (identical(.disappr_file, "provenance.R")) disappr_runtime_init(.disappr_app_env)
  }
} else {
  if (!requireNamespace("disappR", quietly = TRUE)) {
    stop("The disappR engine was not found: run the app from the package folder, install the package, or bundle the app ",
         "for deployment with inst/scripts/bundle_app.R.", call. = FALSE)
  }
  # An installed copy is replaced on disk by install.packages(), but an R session that had loaded the old version
  # keeps the old engine in memory: the new app would then call functions that do not exist yet. Stop and say so.
  .disappr_disk <- tryCatch(unname(read.dcf(file.path("..", "DESCRIPTION"), fields = "Version")[1, 1]), error = function(e) NA_character_)
  .disappr_loaded <- tryCatch(as.character(getNamespaceVersion("disappR")), error = function(e) NA_character_)
  if (!is.na(.disappr_disk) && !is.na(.disappr_loaded) && !identical(.disappr_disk, .disappr_loaded)) {
    stop(sprintf(paste0("disappR %s is installed, but this R session still has version %s loaded, so the app and its engine ",
                        "would not match. Restart R (RStudio: Session > Restart R), then run disappR::run_app() again."),
                 .disappr_disk, .disappr_loaded), call. = FALSE)
  }
  .disappr_ns <- asNamespace("disappR")
  for (.disappr_name in ls(.disappr_ns)) assign(.disappr_name, get(.disappr_name, envir = .disappr_ns), envir = .disappr_app_env)
}

if (any(!OPTIONAL_STATUS$Installed)) {
  message("disappR ", DISAPPR_VERSION, ": optional packages not installed: ",
          paste(OPTIONAL_STATUS$Package[!OPTIONAL_STATUS$Installed], collapse = ", "), " (the Start page lists what each adds).")
}


# Say which copy is running: with several unzipped copies on one computer it is easy to open an old one.
try(message("disappR ", if (exists("DISAPPR_VERSION")) DISAPPR_VERSION else "(version unknown)",
            " starting from ", normalizePath(file.path("..", ".."), mustWork = FALSE)), silent = TRUE)
