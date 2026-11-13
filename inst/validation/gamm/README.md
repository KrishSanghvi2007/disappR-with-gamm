# Validation of the GAMM step (step 7, disappR 0.25.0)

Every result here comes from the real engines: mgcv (or gamm4) for the GAMMs and lme4 for the GLMMs. The 0.23 layer
was validated on a Python re-implementation of a GAMM. That port modelled age-dependent selective disappearance
differently (`s(age, by = ALR)`) from the R code (`ti(age, ALR)`), so its results did not validate the app.

| File | Script | What it checks |
|---|---|---|
| `replication_results.txt` | `tests/scripts/gamm_replication.R` | Two published GAMM analyses, from the authors' data and model code, against the app's engine |
| `gamm_vs_glmm_results.txt`, `gamm_vs_glmm_replicates.csv` | `tests/scripts/gamm_vs_glmm.R` | GAMMs against the parametric models of step 5 on 240 simulated datasets with a known answer |
| `gamm_vs_glmm_empirical.txt` | the same, `DISAPPR_GVG_REPS=0` | The same comparison on the bundled Gaussian examples |
| `stress_results.txt` | `tests/scripts/gamm_stress.R` | Every bundled example, every out-of-sample dataset in `tests/stress_data`, and 31 awkward datasets |

## Replication (bundled examples `tsui_mass` and `brown_haematocrit`)

### Tsui et al. 2026, *Evol. Lett.*: Seychelles warbler body mass

Run on the authors' data and `model1.R`.

| Check | Paper | Authors' code, refitted | disappR |
|---|---|---|---|
| Records / birds | — | 2,560 / 1,172 | 2,559 / 1,172 |
| Age smooth | ageing in body mass | edf 3.05, F = 4.46, p = 0.002 | edf 3.03, F = 4.50, p = 0.002 |
| Lifespan, per year | linear term | 0.0252 (SE 0.0055) | 0.0244 (SE 0.0055) |
| Sex curves | G / GS / S chosen by AIC | S best; GS +4.5, G +4.3 | S best; G +5.7 by ML |
| Onset of decline | 4.7 years (limits 1–8.5) | — | final decline from 4.9 years (95% limits 3.5–14.8) |
| Age × lifespan | reported as significant for body mass | `ti` p = 0.056 | not supported (ΔAIC +8.5); additive lifespan best |

- **Records:** one record differs because its observer is blank. disappR treats a blank as missing; the authors'
  `factor()` kept it as a level.
- **Onset of decline:** with pointwise intervals, decline is significant from 8.6 to 12.2 years. With simultaneous
  intervals no period is significant.
- **Age × lifespan:** the only result that does not match the paper. Both the authors' formula refitted here and the
  app find it weak or absent. It may come from a different specification in the authors' AIC scripts, which we did
  not run.

### Brown et al. 2021, *Ecol. Evol.*: Seychelles warbler haematocrit

| Check | Paper (gamm4) | Authors' specification refitted | disappR |
|---|---|---|---|
| Records / birds | 1,379 / 730 | 1,379 / 730 | 1,379 / 730 |
| Age smooth | edf 7.805, F = 16.73, p < .001 | gamm4 edf 7.81, F = 17.5; mgcv edf 7.81, F = 15.5 | edf 7.81, F = 15.5 |
| Peak | about 1.5 years | — | 1.62 years (95% limits 1.44–1.88) |
| Selective disappearance | none detected | — | ALR p = 0.45: none |

## GAMMs against GLMMs (simulations)

The design has four true ageing shapes, three kinds of selective disappearance and 20 replicates each. Each approach
fits its version of Models 1, 2 and 4 by ML. "Age-dependent declared" means the age-dependent model is more than
2 AIC better than the additive one.

| Approach | Falsely age-dependent (no true age-dependence) | Power (true age-dependence) | Mean shape error |
|---|---|---|---|
| GAMM | 11% | 100% | 0.34 |
| GLMM, function chosen by AIC | 30% | 100% | 0.37 |
| GLMM, quadratic | 75% | 100% | 0.41 |

The GAMM is the most reliable of the three. Its false-positive rate (11%) is still above 5%, as expected for a
2-AIC rule on a term with several effective degrees of freedom. The 0.23 audit's 3% came from the Python port. A
stricter rule (for example ΔAIC > 4) would lower it.

## Performance

These figures are for the great tit example: 7,126 records from 4,848 birds.

- **mgcv:** every individual's random effect is a dense column. A `bam` fit took 202 s and 5.6 GB, and `summary()`
  with random-effect tests another 151 s.
- **gamm4:** lme4's sparse random effects fit the same model in 97 s and 1.4 GB. The app uses gamm4 automatically
  above 1,500 individuals when the model allows it.
- **Summaries:** `summary()` now skips mgcv's random-effect tests. They are slow, and they failed on some ML fits.
  Variance components are reported instead.

All timings are from a 4-core container with OpenBLAS. R's reference BLAS, the default on Windows, is several
times slower for these models.
