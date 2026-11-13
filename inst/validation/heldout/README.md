# Held-out validation with real data

Datasets used while developing disappR (the bundled examples) cannot show how it performs on data it has never
seen. This folder is for genuinely held-out datasets: published longitudinal ageing data that were not used to
build, tune or check the app.

Protocol:

1. Choose datasets whose published analysis can be expressed with disappR's models (family, ageing function,
   covariates, random effects).
2. Before running disappR, record in `registry.R` the column mapping, the settings that match the paper, and the
   published estimates to compare (term, estimate, tolerance). Do not change these after seeing the results.
3. Run `Rscript tests/scripts/heldout_validation.R` from the package root. It fits each entry with the disappR
   engine, compares the estimates with the published values and writes `heldout_results.csv` here.
4. Report every dataset, including those where disappR and the paper disagree.

The independent simulation study (`../VALIDATION.md`) already includes held-out simulated scenarios.
