# Held-out validation with real datasets (see inst/validation/heldout/README.md). Run from the package root:
#   Rscript tests/scripts/heldout_validation.R
if (!file.exists("DESCRIPTION")) stop("Run from the package root (the folder containing DESCRIPTION).")
suppressPackageStartupMessages(library(shiny))
root <- getwd()
source(file.path("inst", "validation", "heldout", "registry.R"))
if (!length(HELDOUT)) {
  cat("No held-out datasets registered yet: add them to inst/validation/heldout/registry.R.\n")
  quit(status = 0)
}
owd <- setwd(file.path("inst", "app"))
source("global.R")
setwd(owd)
out <- list()
for (key in names(HELDOUT)) {
  h <- HELDOUT[[key]]
  path <- if (file.exists(h$file)) h$file else file.path(root, h$file)
  raw <- read_user_csv(path, basename(path))$data
  map <- utils::modifyList(example_map(id = h$mapping$id, age = h$mapping$age, trait = h$mapping$trait), h$mapping)
  b <- standardise_data(raw, map)
  r <- fit_model_suite(b$data, b$meta, h$models, h$age_function %||% "Quadratic", h$family %||% "gaussian",
                       standardise = isTRUE(h$standardise %||% TRUE))
  if (!isTRUE(r$ok)) {
    out[[key]] <- data.frame(dataset = key, model = NA, term = NA, estimate = NA, published = NA, difference = NA, result = paste("not fitted:", r$message))
    next
  }
  ct <- r$coefficients
  for (m in h$models) {
    if (is.null(r$fits[[m]])) next
    for (i in seq_len(nrow(h$published))) {
      est <- ct$Estimate[ct$Model == model_label(m) & ct$Raw_term == h$published$term[[i]]]
      est <- if (length(est)) est[[1]] else NA_real_
      dif <- est - h$published$estimate[[i]]
      out[[paste(key, m, i)]] <- data.frame(dataset = key, model = m, term = h$published$term[[i]], estimate = est,
                                            published = h$published$estimate[[i]], difference = dif,
                                            result = if (is.finite(dif) && abs(dif) <= h$published$tolerance[[i]]) "within tolerance" else "outside tolerance")
    }
  }
}
res <- do.call(rbind, out)
print(res, row.names = FALSE)
utils::write.csv(res, file.path("inst", "validation", "heldout", "heldout_results.csv"), row.names = FALSE)
