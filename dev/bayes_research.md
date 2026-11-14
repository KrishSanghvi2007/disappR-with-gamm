# Step 8 (Bayesian, brms): research notes and draft plan

Branch `bayes-ageing`, forked from the GAMM branch (`claude/loving-cori-xmw0mp`). Nothing in the app has changed yet.

## 1. Do ageing studies use Bayesian mixed models?

Sample: 62 open-access eco-evo papers (Europe PMC full text, 2014-2026) with senescence, ageing or selective
disappearance in the title. Counts are regex hits anywhere in the text, references included, so they over-count;
treat them as roughly +/- 10 percentage points.

| Tool family | Share of 62 papers |
|---|---|
| Any Bayesian tool | 18% |
| brms / Stan / rstanarm | 10% |
| MCMCglmm | 3% |
| JAGS / INLA | 5% |
| lme4 / glmmTMB / nlme | 34% |

- brms/Stan use clusters in 2022-2026. MCMCglmm is older and tied to animal models (random regression, heritability
  of ageing).
- What Bayesian fits were used for: credible intervals on derived quantities and variance components; animal models;
  robustness checks beside a frequentist analysis; survival and capture-recapture models that mixed-model software
  cannot fit.
- No paper read used LOO/WAIC or a prior predictive check. Priors were often default or reported without justification.
- No verified brms analysis was found from the flagship long-term studies (Soay sheep, red deer, Seychelles warbler,
  great tit, meerkats). These remain mostly frequentist.

Examples (methods read):

- Badas et al. 2026, Behav Ecol, 10.1093/beheco/arag024: brms ordinal models as a robustness check of senescence.
- Feder et al. 2025, R Soc Open Sci, 10.1098/rsos.241210: brms binomial and rstanarm survival models in primates.
- Mohring et al. 2026, Ecol Lett, 10.1111/ele.70384: brms, weakly informative normal priors, albatross trade-offs.
- Brengdahl et al. 2020, BMC Biol, 10.1186/s12915-020-00858-5: rstanarm GLMMs of age-specific mutation effects.
- Chik et al. 2025, Evol Lett, 10.1093/evlett/qrae055: MCMCglmm telomere ageing, prior sensitivity.
- Power et al. 2025, PNAS, 10.1073/pnas.2515125122: MCMCglmm, within/among-individual telomere change and selective
  disappearance; the closest match to disappR's design.
- Herdegen-Radwan & Raubic 2026, Behav Ecol, 10.1093/beheco/arag020: MCMCglmm, age change and selective disappearance.
- Fay et al. 2020, Ecol Evol, 10.1002/ece3.6281: JAGS, senescence in survival but not reproduction.

## 2. Methods literature the step should follow

- Burkner 2017 (J Stat Softw 10.18637/jss.v080.i01), 2018 (R J 10.32614/RJ-2018-017): brms syntax and families.
- Gelman et al. 2020, Bayesian Workflow (arXiv 2011.01808): prior check, fit, diagnose, posterior check, compare.
- Gabry et al. 2019 (JRSS A 10.1111/rssa.12378): prior/posterior predictive and diagnostic plots.
- Vehtari et al. 2021 (Bayesian Anal 10.1214/20-BA1221): rank-normalised R-hat < 1.01, bulk and tail ESS.
- Vehtari et al. 2017 (Stat Comput 10.1007/s11222-016-9696-4): PSIS-LOO, elpd differences with SE, Pareto k.
- Lemoine 2019 (Oikos 10.1111/oik.05985); Banner et al. 2020 (MEE 10.1111/2041-210X.13407): weakly informative
  priors, and report and justify them.
- Gelman 2006 (Bayesian Anal 10.1214/06-BA117A): priors on random-effect SDs.
- Kruschke 2021 (Nat Hum Behav 10.1038/s41562-021-01177-7): BARG reporting checklist.
- van de Pol & Verhulst 2006 (Am Nat 10.1086/503331): within/between age decomposition, already in step 5.

## 3. Existing tools

shinybrms (Weber 2022, R J) is a general brms front-end; shinystan browses fitted models; JASP has slow Bayesian
mixed models aimed at psychology. None is aimed at longitudinal ageing data, prior predictive trajectories, or
selective disappearance. (Moderate confidence: limited searching.)

## 4. Benchmark in this container (4 cores, OpenBLAS, brms 2.20.4, rstan 2.32.5)

6,086 records, 300 individuals, 4 chains x 2000 iterations, default priors.

| Model | Time |
|---|---|
| Gaussian `trait ~ age + age^2 + ALR + (1 | id)`, compile and sample | 116 s (about 90 s compiling) |
| Same model, `update()` with new data, no recompile | 21 s |
| Poisson with random slope `(1 + age | id)` | 179 s |

Estimates matched the lme4 fit closely (age 6.33, age^2 -1.61, ALR 7.93), but maximum R-hat was 1.05 (> 1.01).
Defaults alone are not enough: the step needs a convergence panel and better default priors.

## 5. Family coverage (step 5 families -> brms)

Native in brms: gaussian, Gamma, lognormal, Beta, poisson, negbinomial (nbinom2), zero_inflated_poisson,
zero_inflated_negbinomial (zinb), binomial/bernoulli, beta_binomial; dispersion models through distributional
formulas (`sigma ~`, `shape ~`, `phi ~`). Not native: nbinom1 and zinb1 (need a custom family; leave out at first).

## 6. Verdict

Worthwhile as an optional, clearly guided step, not as a core step. Its value is in teaching the workflow
(priors -> prior check -> fit -> diagnostics -> posterior check -> interpretation -> sensitivity) and in credible
intervals for derived quantities (peak age, onset, the within-individual ageing rate, selective-disappearance terms),
side by side with step 5. Main costs: the C++ toolchain (Rtools / Xcode) and run times of minutes.

## 7. Agreed plan (14 Nov 2026)

Decisions: brms with a compiler check (not rstanarm); refit the step-5 model chosen by the user, preselecting the
best-ranked one, with an optional comparison set later.

- **Phase 1 (done on this branch):** set-up check; model choice; priors with instant prior predictive curves;
  draft/full fit in a callr process with a cached compiled model; convergence traffic lights and traces; posterior
  against step 5, curve, peak and onset; posterior predictive checks; code, methods, saves. Gaussian, Poisson and
  binomial families. Files: `R/bayes.R`, `inst/app/bayes_ui.R`, `inst/app/bayes_server.R`,
  `tests/testthat/test-bayes.R`, `tests/scripts/bayes_app_test.R`.
- **Phase 2:** the other families (Gamma, lognormal, beta, nbinom2, ZIP, ZINB, beta-binomial; nbinom1 needs a custom
  family), age-dependent variance (distributional `sigma ~`), an optional set of models compared by LOO (elpd with SE,
  Pareto k), a prior sensitivity refit (narrow and wide side by side).
- **Phase 3:** validation (credible-interval coverage on simulations with a known answer; agreement with step 5 on the
  bundled examples; stress tests), documentation and R CMD check.
