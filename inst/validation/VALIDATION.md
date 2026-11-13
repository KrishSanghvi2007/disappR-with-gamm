# Independent simulation verification and validation (disappR 0.9.7)

> **Historical study.** This is the 0.9.7 validation, kept unchanged so its numbers can be reproduced. It covers random
> intercepts, Models 1-6 and Gaussian and Poisson traits only. Later checks: the null-model bootstrap calibration
> (`v0_20/python/bootstrap_calibration_summary.md`), the held-out scenarios below, and the 0.21.0 pre-submission audit
> (`audit_0_21_0/README.md`), which covers random slopes for every age term and the binomial traits. The families and
> age-dependent variance added in 0.22.0 are covered by the unit tests (`tests/testthat/test-0-22.R`), not by a
> simulation study.

**Evidence type: independent implementation.** The study re-implements, in Python (numpy/scipy), the app's
simulation design (`simulate_toy_data`), its Models 1–6 (quadratic ageing, standardised age, linear among-individual
terms, random intercept for individual, maximum likelihood), its population-trajectory predictions (proxies held at
their individual-level means) and its decomposition (0.9.6 algorithm). It does **not** execute the R app. Agreement
here supports the methods and their specification; `tests/smoke_test.R` checks the app's own fits in R.

## Design

* 50 scenarios, 4720 simulated datasets (100 per Gaussian scenario, 30 per Poisson scenario).
* Core grid (N = 300 individuals, strong selection): selection **none / age-independent (level) / age-dependent
  (rate) / both** × sampling **complete / missing at random (75% kept) / missing when old / missing when young /
  30% right-censored / irregular ages (±0.35 time steps)**.
* Additional blocks: N = 100; subtle selection; high individual slope variance; Poisson counts; negative
  selection direction; linear ageing; short lifespans (mean 8).
* Truth: the latent average within-individual trajectory (population mean coefficients); for counts, the
  trajectory of a typical individual on the response scale.

**Metrics.** Mean absolute percentage deviation from the truth over ages with at least 10 individuals (as in
the manuscript); share of replicates in which each of Models 1–5 had the lowest AIC (Model 6, which needs known
lifespan, is a benchmark); coverage of 95% confidence intervals of the predicted trajectory at an early, a
middle and a late age; share of replicates in which the Model 4 Wald test of the ALR × age terms (2 df) gave
p < 0.05; for the decomposition, deviation from its own estimand (the latent survivor-restricted
within-individual trajectory) as a verification check; failure and singular-fit rates.

## Verification

* The fast profiled random-intercept ML fit used here matches a general dense mixed-model fit to within
  2×10⁻⁸ (AIC and coefficients) on every model.
* No replicate failed and no fit was singular (0 errors in 4720 datasets).
* The decomposition stays within 0.3–1.5% of its survivor-restricted estimand on regular schedules (complete,
  missing at random, missing when old, censored). It drifts 3.7–4.0% on irregular ages (the common-grid
  approximation) and 5.6–9.3% when young ages are missing (early links rest on few individuals).

## Main findings (suggestive; see limitations)

1. **Without age-dependent selection** every model recovers the true trajectory within about 0.4–0.7% in the
   core grid (1.3–1.8% with high slope variance); observed means are 2–7% off under age-independent selection.
2. **With age-dependent selection** Models 1–3 deviate by about 8–9% and observed means by 13–21%; Model 4 and
   the known-lifespan Model 6 stay within about 1%, Model 5 within about 2.5–3%, and the decomposition deviates
   4–11%. Missing old ages shrink every deviation, as fewer late records are informative.
3. **AIC and accuracy can diverge.** With 30% censoring, Model 4 (ALR) drifts to 3.8–4.0% while AIC still favours
   it; with irregular ages, AIC favours Model 5 in about three-quarters of replicates although Model 4 tracks the
   truth more closely.
4. **Random-intercept models appear over-confident.** Confidence intervals cover the truth in 75–94% of cases
   without selection (nominal 95%). The Model 4 ALR × age test gives p < 0.05 in up to 35% of datasets with no
   age-dependent selection, and in 83–88% when individuals differ strongly in slopes, where AIC also favours
   Model 4 in most replicates. This suggests that evidence for age-dependent selective disappearance from
   random-intercept fits alone is weak unless random slopes are also considered; random slopes (available in
   the app) were not part of this study.
5. **Counts (Poisson).** Model 4 tracks the typical-individual trajectory within 1.5–3.8%; Models 1–2 deviate
   by about 41–43% under age-dependent selection. Observed means and the decomposition estimate population-mean
   counts, a different quantity, so their large deviations from the typical-individual truth are expected.

## Core grid (N = 300, strong selection): mean absolute % deviation from the true trajectory

*The four censored rows were regenerated in 0.20.2. The censoring step iterated over a Python set, whose order
follows the per-process hash seed, so those cells could not be reproduced from their seeds; it now uses a fixed order.
The other 46 cells reproduce the committed replicates to within 10⁻¹¹ (checked in 0.20.2).*

| Block | Selection | Sampling | Observed | Decomp. | M1 | M2 | M3 | M4 | M5 | M6 | Lowest AIC (share of replicates) | M4 CI coverage | M4 ALR×age test p<0.05 | Decomp. vs its survivor target |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| core | none | complete | 0.7 | 0.5 | 0.4 | 0.4 | 0.4 | 0.4 | 0.4 | 0.4 | M1 54%, M4 33%, M2 11% | 90.7% | 30.0% | 0.3 |
| core | none | mcar | 0.8 | 0.7 | 0.4 | 0.4 | 0.4 | 0.5 | 0.4 | 0.5 | M1 52%, M4 36%, M2 11% | 90.7% | 30.0% | 0.6 |
| core | none | mwo | 0.6 | 0.7 | 0.4 | 0.4 | 0.4 | 0.4 | 0.4 | 0.4 | M1 69%, M2 11%, M5 7%, M3 7%, M4 6% | 91.3% | 7.0% | 0.6 |
| core | none | mwy | 0.9 | 5.3 | 0.4 | 0.4 | 0.4 | 0.4 | 0.4 | 0.4 | M1 60%, M4 25%, M2 15% | 93.0% | 20.0% | 5.6 |
| core | none | censored | 0.7 | 0.5 | 0.4 | 0.4 | 0.4 | 0.5 | 0.5 | 0.4 | M1 52%, M4 35%, M2 13% | 91.3% | 29.0% | 0.3 |
| core | none | irregular | 0.7 | 3.6 | 0.4 | 0.4 | 0.4 | 0.4 | 0.4 | 0.4 | M1 58%, M4 32%, M2 9% | 94.3% | 28.0% | 3.8 |
| core | independent | complete | 6.8 | 0.7 | 0.6 | 0.6 | 0.6 | 0.6 | 0.6 | 0.6 | M2 57%, M4 42% | 81.0% | 30.0% | 0.3 |
| core | independent | mcar | 6.3 | 1.1 | 0.6 | 0.6 | 0.6 | 0.7 | 0.7 | 0.7 | M2 68%, M4 32% | 74.7% | 21.0% | 0.9 |
| core | independent | mwo | 1.8 | 0.8 | 0.6 | 0.6 | 0.6 | 0.6 | 0.6 | 0.6 | M2 69%, M3 18%, M4 8%, M5 5% | 92.0% | 3.0% | 0.5 |
| core | independent | mwy | 7.0 | 8.6 | 0.6 | 0.6 | 0.6 | 0.6 | 0.6 | 0.6 | M2 67%, M4 30% | 81.0% | 18.0% | 8.9 |
| core | independent | censored | 6.6 | 0.7 | 0.6 | 0.6 | 0.6 | 0.7 | 0.7 | 0.6 | M3 63%, M5 37% | 88.7% | 28.0% | 0.3 |
| core | independent | irregular | 7.0 | 3.5 | 0.6 | 0.6 | 0.6 | 0.6 | 0.6 | 0.6 | M2 55%, M4 42% | 78.0% | 24.0% | 3.7 |
| core | dependent | complete | 14.1 | 4.3 | 8.8 | 8.8 | 8.8 | 0.7 | 2.5 | 0.7 | M4 95%, M5 5% | 74.0% | 100.0% | 0.3 |
| core | dependent | mcar | 12.9 | 4.0 | 8.2 | 8.2 | 8.1 | 0.7 | 3.1 | 0.7 | M4 100% | 76.3% | 100.0% | 0.7 |
| core | dependent | mwo | 2.3 | 0.9 | 1.6 | 1.6 | 1.6 | 0.7 | 1.0 | 0.6 | M4 96% | 78.0% | 100.0% | 0.5 |
| core | dependent | mwy | 14.3 | 7.7 | 7.8 | 7.7 | 7.7 | 0.9 | 2.9 | 1.0 | M4 100% | 65.0% | 100.0% | 5.9 |
| core | dependent | censored | 13.2 | 4.3 | 8.5 | 8.4 | 8.4 | 4.0 | 3.0 | 0.7 | M4 100% | 32.0% | 100.0% | 0.5 |
| core | dependent | irregular | 14.1 | 8.2 | 8.9 | 8.8 | 8.8 | 0.8 | 2.6 | 0.8 | M5 75%, M4 25% | 74.0% | 100.0% | 4.0 |
| core | both | complete | 21.2 | 4.5 | 9.1 | 9.0 | 9.0 | 1.2 | 2.5 | 1.2 | M4 94%, M5 6% | 51.0% | 100.0% | 0.3 |
| core | both | mcar | 19.2 | 4.3 | 8.3 | 8.3 | 8.3 | 1.1 | 3.2 | 1.0 | M4 100% | 58.3% | 100.0% | 0.9 |
| core | both | mwo | 4.2 | 1.3 | 1.8 | 1.8 | 1.8 | 1.1 | 1.3 | 0.9 | M4 92%, M5 8% | 79.0% | 100.0% | 0.5 |
| core | both | mwy | 21.2 | 10.6 | 8.1 | 8.0 | 8.0 | 1.3 | 3.2 | 1.3 | M4 100% | 52.0% | 100.0% | 9.3 |
| core | both | censored | 19.1 | 4.3 | 8.2 | 8.1 | 8.1 | 3.8 | 2.9 | 1.0 | M4 100% | 35.0% | 100.0% | 0.6 |
| core | both | irregular | 20.7 | 8.1 | 8.9 | 8.8 | 8.8 | 1.1 | 2.6 | 1.1 | M5 73%, M4 27% | 58.0% | 100.0% | 3.9 |

## Additional blocks

| Block | Selection | Sampling | Observed | Decomp. | M1 | M2 | M3 | M4 | M5 | M6 | Lowest AIC (share of replicates) | M4 CI coverage | M4 ALR×age test p<0.05 | Decomp. vs its survivor target |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| N=100 | none | complete | 0.9 | 0.7 | 0.6 | 0.6 | 0.6 | 0.6 | 0.6 | 0.6 | M1 58%, M4 24%, M2 9%, M5 5% | 93.7% | 23.0% | 0.4 |
| N=100 | none | mcar | 1.0 | 1.3 | 0.6 | 0.6 | 0.6 | 0.6 | 0.6 | 0.6 | M1 54%, M4 31%, M2 8% | 95.7% | 24.0% | 1.2 |
| subtle selection | none | complete | 0.6 | 0.5 | 0.4 | 0.4 | 0.4 | 0.4 | 0.4 | 0.4 | M1 54%, M4 43% | 95.7% | 35.0% | 0.3 |
| high slope variance | none | complete | 2.1 | 1.1 | 1.4 | 1.3 | 1.3 | 1.8 | 1.7 | 1.8 | M4 87%, M1 10% | 76.3% | 88.0% | 0.3 |
| Poisson counts | none | complete | 5.7 | 9.1 | 1.3 | 1.3 | – | 1.5 | – | – | M1 57%, M4 23%, M2 20% | – | – | 1.5 |
| N=100 | independent | complete | 5.1 | 1.0 | 0.9 | 0.9 | 0.9 | 1.0 | 1.0 | 1.0 | M2 53%, M4 38%, M3 6% | 83.0% | 30.0% | 0.4 |
| N=100 | independent | mcar | 4.6 | 1.5 | 1.0 | 1.0 | 1.0 | 1.0 | 1.0 | 1.0 | M2 67%, M4 27% | 80.7% | 16.0% | 1.2 |
| subtle selection | independent | complete | 2.5 | 0.5 | 0.5 | 0.5 | 0.5 | 0.5 | 0.5 | 0.5 | M2 63%, M4 32% | 91.3% | 28.0% | 0.3 |
| high slope variance | independent | complete | 7.0 | 1.3 | 1.5 | 1.5 | 1.5 | 1.8 | 1.7 | 1.8 | M4 79%, M2 13%, M5 7% | 69.0% | 83.0% | 0.3 |
| Poisson counts | independent | complete | 22.8 | 68.3 | 1.6 | 1.5 | – | 1.9 | – | – | M2 67%, M4 33% | – | – | 1.5 |
| N=100 | dependent | complete | 9.2 | 2.9 | 6.0 | 6.0 | 6.0 | 1.0 | 2.4 | 1.0 | M4 86%, M5 14% | 80.0% | 100.0% | 0.4 |
| N=100 | dependent | mcar | 8.7 | 3.0 | 5.9 | 5.8 | 5.8 | 1.3 | 3.0 | 1.3 | M4 100% | 68.0% | 100.0% | 1.0 |
| subtle selection | dependent | complete | 5.0 | 1.7 | 3.2 | 3.1 | 3.1 | 0.4 | 0.9 | 0.4 | M4 88%, M5 12% | 93.7% | 100.0% | 0.3 |
| high slope variance | dependent | complete | 14.6 | 4.5 | 9.3 | 9.2 | 9.2 | 1.9 | 3.1 | 1.9 | M4 93%, M5 7% | 70.7% | 100.0% | 0.3 |
| Poisson counts | dependent | complete | 98.3 | 148.9 | 41.4 | 41.1 | – | 3.3 | – | – | M4 100% | – | – | 1.5 |
| N=100 | both | complete | 14.6 | 3.5 | 6.6 | 6.6 | 6.5 | 1.8 | 2.9 | 1.8 | M4 81%, M5 19% | 52.3% | 100.0% | 0.4 |
| N=100 | both | mcar | 12.8 | 3.5 | 5.9 | 5.8 | 5.8 | 1.9 | 3.0 | 1.9 | M4 100% | 54.3% | 100.0% | 1.3 |
| subtle selection | both | complete | 7.4 | 1.6 | 3.2 | 3.2 | 3.2 | 0.6 | 0.9 | 0.6 | M4 80%, M5 20% | 82.0% | 100.0% | 0.3 |
| high slope variance | both | complete | 21.5 | 4.9 | 9.6 | 9.4 | 9.4 | 2.2 | 3.4 | 2.2 | M4 85%, M5 15% | 65.3% | 100.0% | 0.3 |
| Poisson counts | both | complete | 161.2 | 254.3 | 43.2 | 42.9 | – | 3.8 | – | – | M4 97% | – | – | 1.3 |
| negative direction | dependent | complete | 14.2 | 4.4 | 8.9 | 8.9 | 8.9 | 0.8 | 2.5 | 0.8 | M4 100% | 68.0% | 100.0% | 0.3 |
| negative direction | both | complete | 20.7 | 4.3 | 8.9 | 8.8 | 8.8 | 1.1 | 2.5 | 1.1 | M4 100% | 59.0% | 100.0% | 0.3 |
| linear ageing | none | complete | 1.1 | 0.7 | 0.5 | 0.5 | 0.5 | 0.6 | 0.6 | 0.6 | M1 69%, M4 10%, M5 8%, M3 7%, M2 6% | 93.0% | 16.0% | 0.4 |
| short lifespans (mean 8) | none | complete | 0.7 | 0.5 | 0.4 | 0.4 | 0.4 | 0.5 | 0.5 | 0.5 | M1 69%, M4 18%, M2 13% | 89.0% | 13.0% | 0.3 |
| linear ageing | both | complete | 36.1 | 7.6 | 15.4 | 15.4 | 15.3 | 1.9 | 3.7 | 1.9 | M4 100% | 53.0% | 100.0% | 0.4 |
| short lifespans (mean 8) | both | complete | 22.0 | 5.0 | 9.4 | 9.3 | 9.3 | 1.3 | 2.5 | 1.3 | M4 93%, M5 7% | 49.7% | 100.0% | 0.3 |

## Limitations

* Independent implementation: the R app was not executed; lme4 and glmmTMB fits should match these maximum
  likelihood fits for the same specification, which `tests/smoke_test.R` checks for selected cases.
* Random intercepts only; random slopes, negative binomial, zero-inflated and binomial families, Models 7–10
  (selective appearance), covariates and nesting were outside this study.
* Coverage was assessed at three ages; truths for counts are conditional (typical individual).

## Reproduce

```
cd inst/validation/independent_python
python3 run_vv.py core && python3 run_vv.py extra && python3 summarise_vv.py
```
Replicate-level results: `results/vv_replicates.csv.gz`; scenario summaries: `results/vv_summary.csv`.

## Held-out validation (0.9.13)

Ten simulated scenarios that were not used while developing or validating disappR were run afterwards: new ageing
shapes (steeper, later and intermediate peaks), sample sizes from 150 to 500, mean lifespans from 6 to 25, new
combinations of selection, sampling and slope variance, and new random seeds (60 datasets each; 40 for counts). Four
expectations were fixed beforehand from the development results:

* **E1** without age-dependent selection, every model stays within 2% of the true trajectory;
* **E2** with age-dependent selection, Model 4 stays within 3% (6% for counts) and closer than Model 1;
* **E3** with age-dependent selection and complete or randomly missing records, Model 4 or 5 has the lowest AIC in at
  least 80% of datasets;
* **E4** on regular schedules, the decomposition stays within 2% of its survivor-restricted target.

Nine of the ten scenarios met every expectation. The exception: with 30% of individuals censored and only 150
individuals, Model 4 deviated 4.4% from the true trajectory (expected within 3%), although it stayed much closer than
Model 1 (8.4%). This repeats the development finding that censoring makes ALR a poorer proxy for lifespan; the app's
interpretation notes already caution about censored data.

Mean absolute % deviation from the true trajectory:

| Scenario | Model 1 | Model 4 | Decomposition vs its target | Expectations |
|---|---|---|---|---|
| steep peak, N 200, lifespan 12, age-dependent SD, complete | 8.1% | 0.8% | 0.3% | E2 age-dependent selection: Model 4 within 3% and closer than Model 1: PASS; E3 Models 4 or 5 have the lowest AIC in at least 80% of datasets: PASS; E4 decomposition within 2% of its survivor-restricted target: PASS |
| steep peak, N 200, lifespan 12, no SD, MCAR | 0.4% | 0.5% | 0.7% | E1 no age-dependent selection: every model within 2%: PASS; E4 decomposition within 2% of its survivor-restricted target: PASS |
| late peak, N 500, lifespan 25, both SD, missing when old | 2.3% | 1.1% | 0.6% | E2 age-dependent selection: Model 4 within 3% and closer than Model 1: PASS; E4 decomposition within 2% of its survivor-restricted target: PASS |
| late peak, N 150, age-dependent SD, censored | 8.4% | 4.4% | 0.6% | E2 age-dependent selection: Model 4 within 3% and closer than Model 1: FAIL |
| mid peak, lifespan 16, age-independent SD, irregular ages | 0.6% | 0.6% | 4.7% | E1 no age-dependent selection: every model within 2%: PASS |
| mid peak, lifespan 16, both SD, missing when young | 9.4% | 1.3% | 9.0% | E2 age-dependent selection: Model 4 within 3% and closer than Model 1: PASS |
| subtle SD, high slope variance, age-dependent, MCAR | 2.9% | 1.7% | 0.8% | E2 age-dependent selection: Model 4 within 3% and closer than Model 1: PASS; E3 Models 4 or 5 have the lowest AIC in at least 80% of datasets: PASS; E4 decomposition within 2% of its survivor-restricted target: PASS |
| negative direction, age-independent SD, censored | 0.6% | 0.7% | 0.4% | E1 no age-dependent selection: every model within 2%: PASS |
| Poisson counts, age-dependent SD, MCAR | 37.5% | 3.5% | 4.7% | E2 age-dependent selection: Model 4 within 6% and closer than Model 1: PASS |
| linear senescence, N 400, lifespan 6, both SD, complete | 22.8% | 1.9% | 0.4% | E2 age-dependent selection: Model 4 within 3% and closer than Model 1: PASS; E3 Models 4 or 5 have the lowest AIC in at least 80% of datasets: PASS; E4 decomposition within 2% of its survivor-restricted target: PASS |

Scenario definitions and per-scenario metrics: `independent_python/heldout_vv.py` and `results/heldout_summary.csv`.
A protocol and runner for genuinely held-out real datasets are in `heldout/`.



## disappR 0.20.0: step 4, the evidence summary and the null-model bootstrap

The validations above predate three parts of the app, which a reviewer rightly noted were therefore unvalidated.
Versioned scripts now cover them, using the app's own functions on simulated data with known answers:

| Script | What it checks | Expected result |
|---|---|---|
| `v0_20/step4_reconstruction.R` | Step 4's reconstruction of the average trajectory from individual fits, for continuous and count traits, three ageing forms, with and without age-dependent selection and missing records | Continuous traits within about 1-2% of the truth; Poisson counts within about 2-9% for linear and quadratic forms; cubic counts noisy unless only well-sampled individuals are fitted |
| `v0_20/grader_calibration.R` | The evidence summary, fed by every line of evidence as a user would save it | No selection: "consistent with none"; age-independent and age-dependent selection: "strong" with the right kind; trait-dependent missingness: not strong, observation bias flagged |
| `v0_20/bootstrap_calibration.R` | False-positive rate of the null-model bootstrap without selection - including a misspecified ageing function and individuals ageing at different rates, which defeated the permutation test - and its power with selection | Close to 5% in every null case; high power with selection |

Run all three from the package root with `Rscript inst/validation/v0_20/run_all.R` (tens of minutes; set
`DISAPPR_VALIDATION_SEEDS` to change the number of simulated datasets). Results are written to
`inst/validation/results/v0_20_*.csv`.

**Status: the R scripts are written but have not yet been run in R.** Their expected results come from independent
Python implementations (step 4 and the grader). The results files should be generated, inspected and committed.

**Measured in Python (0.20.2)**, `v0_20/python/`, with the emulator of `extended/` (not the R package):

| Scenario (60 datasets each) | AIC > 2 | LRT | Permutation (≤ 0.19.6) | Bootstrap, 0.20.x null | Bootstrap, correlated-slope null |
|---|---|---|---|---|---|
| clean null | 0.05 | 0.05 | 0.017 | 0.000 | 0.017 |
| misspecified ageing function, no selection | 1.00 | 1.00 | 1.000 | 0.000 | 0.033 |
| heterogeneous ageing rates, no selection | 0.25 | 0.25 | 0.050 | 0.050 | 0.017 |
| rate-linked selection (strength 1) | 1.00 | 1.00 | 1.000 | 1.000 | 1.000 |
| rate-linked selection (strength 0.5) | 1.00 | 1.00 | 1.000 | 0.933 | 0.933 |
| misspecified function and heterogeneous rates, no selection | 1.00 | 1.00 | 0.967 | 0.017 | 0.017 |

A valid test with 39 null draws rejects in about 2.5% of datasets. The null used by `bootstrap_test()` (a cubic curve
and uncorrelated random slopes when the analysis has a random intercept) held its size in all four scenarios without
selection, including the misspecified ageing function that defeated the permutation test, and kept 93-100% power.
`stats_checks.md` in the same folder shows that step 4's QAICc chose the same function as the correct likelihood in
every scenario, that the coefficient-sign rule for the finding's direction fails only for the asymptotic exponential,
and that standardising age changes the AIC only with uncorrelated random slopes (median 31 units).

## Pre-submission audit (0.21.0)

`audit_0_21_0/` holds the static, scope, numerical and empirical checks run before submission, with the Python scripts behind them. It complements the R test suite; `tests/scripts/run_all.R` remains the release gate.
