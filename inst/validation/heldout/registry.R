# Held-out datasets: one list entry per dataset, filled in BEFORE running tests/scripts/heldout_validation.R.
# `mapping` uses the same roles as disappr_mapping(); `published` lists the terms to compare (as named in the
# coefficient table's Raw_term column), with the published estimate and the tolerated absolute difference.
HELDOUT <- list(
  # example_entry = list(
  #   file = "path/to/data.csv",
  #   citation = "Author et al. Year. Journal. doi:...",
  #   mapping = list(id = "ID", age = "age", trait = "trait", alr = "__AUTO_LAST__"),
  #   models = c("M1", "M2"), age_function = "Linear", family = "gaussian", standardise = FALSE,
  #   published = data.frame(term = c("f1", "ALR"), estimate = c(-0.06, 0.02), tolerance = c(0.01, 0.01))
  # )
)
