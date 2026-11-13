# Run the app without installing disappR. Source this file from the package root (the folder with DESCRIPTION),
# or run shiny::runApp("path/to/disappR/inst/app") from anywhere. Missing packages are installed first.
needed <- c("shiny", "shinydashboard", "ggplot2", "lme4")
optional <- c("glmmTMB", "lmerTest", "nlme", "DHARMa", "performance", "see", "codetools", "callr")   # every optional feature
missing <- c(needed, optional)[!vapply(c(needed, optional), requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)
shiny::runApp(file.path("inst", "app"), launch.browser = TRUE)
