# Binomial traits: weights, repeated records and selective disappearance (0.20.21)

`binomial_checks.py` fits binomial GLMs written directly (IRLS), outside R, to check the behaviour the app relies on
for proportion traits. Cluster-robust standard errors are used for the interaction term. Output:

```
weights: slope with trials                                            mean +0.354 (true +0.350), SD 0.028, mean SE 0.034
weights: slope without trials                                         mean +0.354, SD 0.044, mean SE 0.109
weights: 95% CI coverage                                              with trials 0.99; without 1.00
weights: equal trials, same estimate                                  with +0.3284 vs without +0.3284 (difference 0.0000)
two records at one age: keep both                                     mean slope +0.348 (true +0.350), SD 0.028
two records at one age: pool successes and trials                     mean slope +0.348, SD 0.028
two records at one age: average the proportions                       mean slope +0.347 (bias -0.003, SD 0.047)
selective disappearance (none): within-individual age slope           naive +0.187, additive ALR -0.299, ALR interaction -0.299 (true -0.300)
selective disappearance (none): ALR x age term                        mean +0.000 (true 0.000), detected in 5% of runs
selective disappearance (age_dependent): within-individual age slope  naive +0.394, additive ALR -0.171, ALR interaction -0.249 (true -0.300)
selective disappearance (age_dependent): ALR x age term               mean +0.193 (true +0.180), detected in 100% of runs
overdispersion: beta-binomial data read as binomial                   Pearson chi2/df = 3.83; clean binomial data = 1.02
```

Readings:

* **Map the number of trials.** Ignoring it leaves the estimate roughly unbiased but throws away the information in
  the denominators: the standard error is about three times too wide and the spread of estimates grows. With equal
  trials for every record the two agree exactly.
* **Two records at one age.** Keeping both rows and pooling successes and trials both recover the slope with the same
  precision. Averaging the proportions is not far off on average here but is markedly noisier, and it builds a row
  (a mean proportion beside an averaged number of trials) that was never observed - the app never does it.
* **Selective disappearance.** With a binomial trait the naive model reverses the sign of the within-individual age
  slope (+0.19 against a true -0.30). Adding ALR recovers it (-0.30). Under age-dependent selection the ALR x age term
  is recovered (+0.19 against +0.18) and detected in every run, with a 5% false-positive rate when it is absent.
* **Overdispersion.** Beta-binomial data read as binomial give a Pearson chi-square per degree of freedom near 4,
  which is what the beta-binomial family in the app is for.
