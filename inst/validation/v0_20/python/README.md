# Python checks for disappR 0.20.x

**Evidence type: independent implementation.** These scripts re-implement the calculations in Python (numpy/scipy),
using the mixed-model emulator in `../../extended/`. They do not execute the R package: agreement supports the
methods, not the R code, which is checked by `tests/testthat/`.

| Script | Output | What it checks | Run time |
|---|---|---|---|
| `bootstrap_calibration_vv.py` | `bootstrap_calibration_summary.csv`, `_replicates.csv` | False-positive rate and power of the null-model bootstrap (the 0.20.x null and a correlated-slope null) against the permutation test, in six scenarios | about 7 minutes; resumable (`python3 bootstrap_calibration_vv.py 60 39 [first] [last]`) |
| `stats_checks.py` | `stats_checks.md` | QAICc against Poisson AICc and a correct-likelihood benchmark in step 4; the direction rule for every ageing basis; when standardising age changes AIC | about 2 minutes |

Results are summarised in `../../VALIDATION.md` and NEWS.md.
