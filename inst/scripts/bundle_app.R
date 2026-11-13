# Bundle the Shiny app with its engine for deployment where the package is not installed (for example shinyapps.io or
# Posit Connect): copies inst/app and the engine files from R/ into one self-contained folder, disappR_app/.
# Run from the package root:  source("inst/scripts/bundle_app.R")   then deploy the disappR_app folder.
local({
  if (!file.exists("DESCRIPTION")) stop("Run this from the disappR package root (the folder containing DESCRIPTION).")
  target <- "disappR_app"
  if (dir.exists(target)) unlink(target, recursive = TRUE)
  dir.create(target)
  file.copy(list.files(file.path("inst", "app"), full.names = TRUE), target, recursive = TRUE)
  dir.create(file.path(target, "engine"))
  engine <- setdiff(list.files("R", pattern = "[.]R$"), c("api.R", "run_app.R"))
  file.copy(file.path("R", engine), file.path(target, "engine"))
  message("Bundled app written to ", normalizePath(target), " (", length(engine), " engine files): deploy that folder.")
})
