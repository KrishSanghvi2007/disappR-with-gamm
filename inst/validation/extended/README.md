# Extended independent validation (disappR 0.11.2)

The original independent study (`../independent_python/`) covered Gaussian and Poisson Models 1-6 with a random
intercept. This extension covers the combinations that were previously validated only inside R, chosen because
they are the ones most likely to mislead a user: binomial and binary traits, the appearance models, nested random
effects, covariates, random slopes, a misspecified ageing function, and the grid decomposition.

**Evidence type: independent implementation.** Scenarios are generated and fitted by a second implementation in
Python (`harness.py` + `emu.py`: `prepare()` and the model formulas transcribed from `R/data-preparation.R` and
`R/formulas.R`; maximum-likelihood LMM; Gauss-Hermite quadrature for Poisson and binomial GLMMs, checked against
brute-force integration). The R package is not executed, so agreement supports the specification rather than the
R code; the R test suite (`tests/testthat/`) checks the code.

## Protocol

Every scenario is run over **20 seeds**. A scenario is judged on the whole set, not on a single run:

* **Expected-set win share** - the proportion of seeds whose lowest-AIC model is in the expected set;
* **Median dAIC behind winner** - the median, over seeds, of how far the best expected-set model sits behind the
  overall winner. Models within 2 AIC are not distinguished by the data, so a seed whose winner lies outside the
  expected set by less than 2 AIC is a tie, not a failure.

A scenario passes when the expected set wins at least 70% of seeds, or when it is within 2 AIC of the winner on
the median seed. Two scenarios are intentional demonstrations of false-positive mechanisms; they are labelled
DOCUMENTED with the rate at which a selection model wins, and are not graded. The expected sets treat Model 3
(mean age) as an age-independent correction alongside Models 2 and 7, and Models 4, 5, 6, 8, 9 and 10 as the
age-dependent family.

## Results

| Area | Scenario | Expected set | Expected-set win share | Median dAIC behind winner | Winners over 20 seeds | Verdict |
|---|---|---|---|---|---|---|
| count | Poisson, SD=rate | M10/M4/M5/M6/M8/M9 | 100% | 0.0 | M4:20 | PASS |
| count | Poisson, SD=none | M1 | 75% | 0.0 | M1:15, M4:3, M2:2 | PASS |
| count | Poisson, SD=level | M1/M2/M3/M7 | 90% | 0.0 | M2:18, M4:2 | PASS |
| binomial | binary 0/1, SD=none | M1 | 75% | 0.0 | M1:15, M2:4, M4:1 | PASS |
| binomial | proportion, 8 trials, SD=level | M1/M2/M3/M7 | 80% | 0.0 | M2:16, M4:4 | PASS |
| binomial | proportion, 8 trials, SD=none | M1 | 80% | 0.0 | M1:16, M4:3, M2:1 | PASS |
| binomial | binary 0/1, SD=rate | M10/M4/M5/M6/M8/M9 | 100% | 0.0 | M4:20 | PASS |
| binomial | binary 0/1, SD=level | M1/M2/M3/M7 | 75% | 0.0 | M2:15, M4:5 | PASS |
| binomial | proportion, 8 trials, SD=rate | M10/M4/M5/M6/M8/M9 | 100% | 0.0 | M4:20 | PASS |
| appearance | variable AFR, nothing linked to the trait | M1/M2/M3/M7 | 80% | 0.0 | M1:13, M2:3, M10:2, M4:1, M9:1 | PASS |
| appearance | selective appearance only | M10/M7/M8/M9 | 100% | 0.0 | M7:16, M10:3, M9:1 | PASS |
| random effects | nested: individual within group, rate-linked SD | M10/M4/M5/M6/M8/M9 | 100% | 0.0 | M4:20 | PASS |
| covariates | factor covariate with an age interaction, rate-linked SD | M10/M4/M5/M6/M8/M9 | 100% | 0.0 | M4:20 | PASS |
| random slopes | rate-linked SD, random slopes fitted | M10/M4/M5/M6/M8/M9 | 100% | 0.0 | M4:20 | PASS |
| random slopes | slope variation (SD 0.12), no selection, slopes FITTED | M1/M2/M3/M7 | 100% | 0.0 | M1:15, M2:5 | PASS |
| random slopes | slope variation (SD 0.12), no selection, slopes NOT fitted | M1/M2/M3/M7 | 40% | 0.9 | M4:12, M1:6, M2:2 | DOCUMENTED: 60% of seeds favour a selection model |
| misspecification | quadratic data fitted with a QUADRATIC function, no selection | M1 | 75% | 0.0 | M1:15, M4:3, M2:2 | PASS |
| misspecification | quadratic data fitted with a LINEAR ageing function, no selection | M1 | 0% | 1167.5 | M4:18, M5:2 | DOCUMENTED: 100% of seeds favour a selection model |
| decomposition | rate-linked SD | biased |  |  | slope -0.139 +/- 0.027 (truth -0.250) | PASS |
| decomposition | no SD | unbiased |  |  | slope -0.252 +/- 0.020 (truth -0.250) | PASS |
| decomposition | level-linked SD | unbiased |  |  | slope -0.252 +/- 0.020 (truth -0.250) | PASS |

## What the two documented rows show

* **Random slopes omitted.** With among-individual variation in ageing rate that is unrelated to lifespan and no
  selective disappearance at all, Model 4 wins 60% of seeds when random slopes are left out of the model, and
  0% when they are fitted. Among-individual variation in ageing rate is a false-positive mechanism in its own
  right, independent of selection; this reproduces the warning in the accompanying manuscript.
* **Misspecified ageing function.** Quadratic data fitted with a linear ageing function makes an interaction model
  win 100% of seeds with a median gap of 1,167 AIC, again with no selection simulated; with the correct function
  Model 1 wins 75% and the rest are ties. This is the mechanism the app's ageing-function check (Modelling tab)
  exists to catch.

## Still not independently validated

Zero-inflated families, beta-binomial, the estimated-rate exponential ageing function, crossed (as opposed to
nested) random effects, and DHARMa-based residual checks are exercised by the R test suite but not by a second
implementation.

## Reproducing

```
cd inst/validation/extended
python3 run_extended.py                 # 20 seeds per scenario, ~5 minutes, rewrites results_extended.csv
python3 run_extended.py --seeds 10      # quicker
python3 run_extended.py --only binomial # rerun one area and keep the others
```
Requires Python 3 with numpy, pandas and scipy. All five files in this folder are needed: `run_extended.py`,
`harness.py`, `emu.py`, `grid_decomposition.py` and `results_extended.csv`.
