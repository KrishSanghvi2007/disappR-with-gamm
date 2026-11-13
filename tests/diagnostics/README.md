# Out-of-app diagnostics

Checks that the test tiers in `tests/scripts/` do not cover: what happens when a suggested package is
missing, whether the exported script reproduces the app, how the importer behaves on mutated files,
whether results depend on locale, whether the engine is deterministic and scales, package-level checks,
and a walkthrough of the real interface.

**These are terminal commands, not R console commands**, and they must be run from the package *source*
directory (the folder holding DESCRIPTION), with disappR installed:

    cd ~/Desktop/disappR
    Rscript tests/diagnostics/run_diagnostics.R             # everything except the browser walkthrough
    Rscript tests/diagnostics/run_diagnostics.R --with-ui   # including it

From the R console instead:

    setwd("~/Desktop/disappR")
    source("tests/diagnostics/run_diagnostics.R")
    run_diagnostics()                    # or run_diagnostics(with_ui = TRUE)
    run_diagnostics(only = "01")         # one script

Or one at a time from a terminal:

| Script | What it proves | Needs |
|---|---|---|
| `01_suggests_matrix.R` | a missing suggested package degrades instead of erroring | callr |
| `02_export_reproduction.R` | the exported script gives the app's own AICs | callr |
| `03_import_fuzz.R` | no mutation of a good file produces an uncaught error | - |
| `04_locale_encoding.R` | dropdown order and numbers are identical in every locale | callr, the locales installed |
| `05_determinism_scale.R` | same input gives identical output; 10,000 individuals still fit | digest |
| `06_package_checks.R` | unbound variables, lints, spelling, URLs, data files, checksums | codetools, lintr, spelling, urlchecker |
| `07_ui_walkthrough.R` | every example, model, family, function, structure and tab renders without an error | shinytest2 |

Each script exits non-zero on failure, so they can go straight into CI. `run_diagnostics.R` prints a summary
and exits non-zero if any script failed.

`devtools::check(args = "--as-cran", vignettes = TRUE)` is separate: run it before a release.
