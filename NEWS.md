# disappR 0.24.9

* Test fix: a unit test still expected the old wording of the random-term advice ("without random slopes"); it now
  expects the 0.24.7 wording ("without this term"). Every other message the unit tests expect was checked against the
  source. No change to the app or engine.

# disappR 0.24.8

* **Random slopes by default when the data support them well.** For simulated and uploaded data, the Modelling tab now
  starts with a correlated random intercept and slope when the data support slopes well (at least half the
  individuals, and at least 30, recorded at three or more ages, with more observations than individual-level random
  effects), and with a slope for every age term when most individuals (at least 30) are also recorded at four or more
  ages with a median of four or more records. Otherwise it starts with a random intercept. Bundled examples keep the
  random structure of their published analysis. The note under the random-effect choice states the default for the
  data (`default_random_structure()`, `test-0-24-8.R`).

# disappR 0.24.7

Interface clean-up: less text on screen, more behind info buttons and folded sections.

* **Bundled examples** open with a short summary (species, trait, default model, covariates and what they mean, main
  result, other traits); the full note is folded under "More details". AIC values and weights were removed from the notes.
* **Data tab.** Removed the pre-filled-mapping, binomial-weights and "AFR enters the dataset" notes; the rounding
  explanation moved to an info button, which now says that records rounded to the same age are kept, not averaged;
  clearer exclusion label.
* **Page 2.** Shorter trait-scale, terminal-change and hazard-test texts; the bins sentence moved to the info button.
* **Page 3.** The proxy-choice card and the missing-cell counts are gone; the settings box is "Missingness settings".
  The sampling grid gives an individual with a single occasion its own colour and legend entry ("AFR = ALR").
* **Page 4.** The data-support box is gone (it is on the Modelling tab); "Suggested function" wording; the function
  comparison is "How do different functions fit the population-level data?", with its own shorter help.
* **Page 5.**
  - The suggested starting analysis and its "Apply these settings" button are removed.
  - The random-effect caution appears once, under the random-effect choice.
  - The fitted formula of every supported model is listed.
  - "Is the variance in the data changing across age?" replaces "Residual variance / dispersion".
  - Shorter random-term, Hessian, caution, standardising and misspecified-shape texts; "Further" in the guide.
  - New plot of the predicted trajectories of stored models, for the stored models you tick.
* Audit Part M (38 checks); every earlier part re-run.

# disappR 0.24.6

* Test fix: the app server test listed five outputs that no longer exist (`package_status`, `coverage_table`,
  `b2_settings`, `summary_ui`, `pred_summary_table`). Since 0.23.2 the test fails on any problem, so these stale names
  failed it. They are removed, and audit Part H now checks that every output the test renders is defined by the
  server. No change to the app or engine.

# disappR 0.24.5

* Test fix: the unit test checking that the exported script computes ALR and AFR the same way used two individuals,
  too few to fit the models, so the script could not be generated. It now uses 16 individuals whose ALR varies under
  either option. No change to the app or engine.

# disappR 0.24.4

* **AFE and the trait-specific box no longer conflict.** With an age at first expression (AFE) set on the Sampling
  tab, the missingness window opened at the earlier of the AFE and the individual's first record of any kind, ignoring
  the box and a mapped AFR column. It now opens at the earlier of the AFE and the AFR the models use. Unchanged when
  unticked with automatic AFR (every bundled example).
* **Mapped columns and the box.** A note under the box now says when a mapped ALR or AFR column means the box
  changes only one of them, or neither. The automatic ALR, AFR and lifespan choices name the box.
* The R snapshot tier (tier 9) counts missed occasions from AFR, as the app does by default.
* **Audit Part L** (19 checks) and `test-0-24-4.R`: every combination of AFR automatic or mapped, ALR automatic or
  mapped, box ticked or not, and AFE absent, early or late gives the specified AFR, ALR and window, and the models
  use the same AFR and ALR.

# disappR 0.24.3

* **Data checks follow the trait-specific option.** "Mapped ALR vs last age" compares a mapped ALR with the last age
  with a trait value when ticked (the last record of any kind when unticked), and "LS - ALR" uses the ALR the models
  use when ticked. "LS earlier than the last record" still uses any record, since no individual is seen after death.
* The sampling grid's ALR order breaks ties by the same AFR as the blue tile.
* The app server test toggles the trait-specific box and redraws the grid, the proxy panels and the data checks
  under each option, and checks the benchmark note on a ticked bundled example.
* **Audit Part K** (17 checks): odd missingness x trait x age scenarios with known lifespan and ALR automatic, mapped
  as the last sighting, or mapped inconsistently. Every invariant of the window and the proxies holds.

# disappR 0.24.2

* **Trait-specific ALR and AFR (default).** When ALR and AFR are not mapped as columns, they are the last and first
  ages at which the trait has a value (zero is a value, NA is not), the same records mean age uses. A tick box on the
  Data tab, with a help panel, switches to the last and first ages of any record ("dataset-wide"). Before, the
  dataset-wide definition was the only one.
  - Bundled examples open unticked, so their documented model results are unchanged; a note appears if it is ticked.
  - Uploads, simulations and `disappr_mapping()` in R open ticked. Mapped ALR and AFR columns are used as supplied.
  - "Lifespan = last recorded age" follows the same choice; the disappearance hazard and life table still use each
    individual's last record of any kind.
  - The choice is part of the data fingerprint, the exported R script and the methods draft.
* **Simulations use the observed AFR** (the first observed record), like ALR; the true AFR stays in the data.
* **Missingness window from AFR to ALR, both included.** It opened at the first trait record and closed at the later of
  the ALR and the last record of any kind. Records without a trait value inside the window count as missed, so the
  missingness figures of six bundled examples rise by 0.6 to 3.4 percentage points. The AFR is now a blue tile on the
  sampling grid (an individual seen once gets the ALR tile with a blue outline), and "order by AFR" sorts on it.
* **Audit Part J** (33 checks) and R tier 9 (`trait_ages_snapshot.R`) compare the two options on every example, on
  files with several traits and on simulations; `j_snapshot_before_after.md` and a code diff record 0.24.1 against
  0.24.2.

# disappR 0.24.1

From the first R run of 0.24.0 (R CMD check: 0 errors, 0 warnings, 0 notes; unit tests 1896 passed, 1 failed).

* **The app server unit test** failed to find `tests/scripts/app_test.R`: its path was relative to the tests folder
  but the script was started from the package root. Both paths are now absolute.
* **Peak and onset intervals** no longer call `lme4::nobars()`, which lme4 has moved to the reformulas package and
  now warns about; the fixed-effect formula is taken with `formula(fit, fixed.only = TRUE)` for lme4 and glmmTMB fits
  alike. Results are unchanged.

# disappR 0.24.0

A release of everything up to 0.23.2 except the GAMM layer, to be tested on its own before the GAMMs return.

* **GAMM layer held back.** Step 7 (GAMMs), its engine (`R/gamm.R`), help panels, methods paragraph, saved-result
  types, unit tests (`test-gamm.R`), the R tier comparing GAMMs with GLMMs (`gamm_vs_glmm.R`) and the mgcv and gamm4
  suggestions are removed. Everything else, including every fix in 0.23.1 and 0.23.2, is unchanged.
* **Audit Part I** checks the removal against 0.23.2: nothing of the GAMM layer is left, every removed block was GAMM
  code, and every other engine file is byte-identical. Part G keeps only its checks of the code review's fixes.

# disappR 0.23.2

* **Continuous integration.** GitHub Actions runs:
  - R CMD check on macOS, Windows and Linux for every push and pull request;
  - the app server test and the unit tests from the source tree, on the same triggers;
  - test coverage;
  - every script tier, weekly and on demand.

  The app server test now fails with an error when an output fails (before, it only printed the failure), covers the
  GAMM outputs, and runs inside the unit tests.
* **Individual frailty in the disappearance hazard.** The trait-hazard models and the life-table test of a constant
  hazard now include a random intercept for individual when it can be estimated, and otherwise fall back to the
  previous model; the app says which was fitted. Audit Part H found the previous model was already well calibrated for
  one-event-per-individual data, so results change little.
* **Models 3 and 5 wording.** The help now states that age² is centred on each individual's own mean of age²,
  matching the revised manuscript. It also explains how this differs from Fay et al.'s (2022) equations 3 and 4.
* Audit Part H: continuous integration, the frailty models, and the preprint's, the revised manuscript's and the
  app's Models 3 and 5 compared on the preprint's simulation design.

# disappR 0.23.1

Fixes from the external code review of 0.21.16, with Part G of the audit.

* **Guidance that users act on.**
  - The random-slope warning read the requested random structure, so it vanished when 'Automatic' resolved to an
    intercept only (the false-positive case); it now reads the fitted structure.
  - The suggested starting analysis offered Model 6 when lifespan was only the last-record proxy; it now requires a
    known lifespan.
* **Messages and records.**
  - The bootstrap caution printed the number of datasets requested instead of the number that refitted.
  - The reproducibility checksum hashed the installed copy of an example rather than the file the app loaded.
  - There were two upload limits (20 and 200 MB); there is now one, 50 MB, set in global.R.
  - The model script was written in the native encoding; it is now UTF-8.
* **Smaller fixes.**
  - The random-slope support check now covers the "every age term" structures.
  - Messages no longer say duplicates were averaged: `dup_action = "mean"` only collapses identical rows.
  - The mean-age note no longer fires for Model 6.
  - A layout test no longer errors outside a source checkout.
* **Housekeeping.**
  - Unused server code removed, including a latent error.
  - Automatic ageing-function and error-family checks are cached by data and settings, so refitting the same models
    no longer repeats them.
  - Exported scripts read uploads through the public `disappr_read()`.
  - Model names come from one source (Model 3 was "Mean-centring" in one table).
  - Stale "moved verbatim" headers and `.Rhistory` removed.
* **Audit Part G: GAMMs against GLMMs** on 880 simulated datasets and 8 examples. With no selective disappearance,
  a quadratic GLMM declared age-dependent selection in 67% of datasets, a GLMM with the function chosen by AIC in
  25%, and the GAMM in 3%. All three detected real age-dependent selection every time, and the GAMM recovered the
  trajectory shape best on average. The same comparison through the real engines is the new R tier 9,
  `tests/scripts/gamm_vs_glmm.R`.

# disappR 0.23.0

* **GAMM layer (step 7, optional).** Generalized additive mixed models of ageing, fitted with mgcv (or gamm4), on
  simulated, example or uploaded data. It is an independent layer: it reads the current data and changes nothing in
  steps 1-6 (Part F of the audit checks this line by line).
  - **Model:** error family (Gaussian, Gaussian with a variance smooth in age, Gamma, beta, Poisson, negative
    binomial, zero-inflated Poisson, Tweedie, binomial); basis (tp, ts, cr, cs, ps, bs, ad); k; penalty order;
    hand-placed knots (cr, cs); shrinkage selection; REML or ML; engine gam, bam or gamm4.
  - **Individual variation:** random intercept, intercept and slope, or a random smooth per individual (fs); mapped
    random intercepts and covariates.
  - **Selective disappearance and appearance:** ALR or lifespan, and AFR, each linear, smooth or age-dependent
    through ti(age, proxy); compared by ML.
  - **Groups:** models G, GS, GI, S and I (Pedersen et al. 2019) compared by ML AIC; ordered-factor difference
    smooths with tests; pairwise difference curves with pointwise and simultaneous intervals; a likelihood-ratio test
    of one smooth against one per group.
  - **Ageing:** the first derivative of the population curve with pointwise and simultaneous 95% intervals, periods
    of significant decline and increase, and peak and onset with limits from 1,000 draws.
  - **Checks:** smooth and parametric tables, basis-dimension (k) check, concurvity, residual plots, random-effect
    SDs, individual fitted curves, a comparison with linear, quadratic and cubic age, automatic cautions and the R
    code of the fit.
  - Results save to the summary, and the methods draft describes them.

# disappR 0.22.8

* `inst/app/data/PROVENANCE.md` lists each bundled dataset's paper DOI, data DOI and licence. Ten of the 13 files come
  from Dryad (CC0). Still open: the fly data (licence and DOI), the great tit data (no public archive found;
  permission from the authors needed) and the source of the swift file (the article's supplement, CC BY 4.0, to
  confirm).

# disappR 0.22.7

* **Wording audit** (Part D of `inst/validation/audit_0_22_0/`). Every help panel now says what the result is, how
  to read it and what to watch for, in sentences under 45 words.
  - Corrected: the population-level function comparison uses maximum likelihood, not restricted maximum likelihood;
    binomial families are fitted with glmmTMB, not lme4; effect sizes are on the link scale.
  - Removed: outdated statements about random slopes and the random-term flag, an option name that no longer exists,
    and references to tabs and settings that do not exist. The trait-scale caution now appears once, instead of in
    five panels.
  - Most "not available" messages now say what to do.
  - The glossary adds AIC/ΔAIC, random slope, error family, sampling interval, SD/SE/CI and MCAR.
* **Complex-scenario audit** (Part E) and a new R test tier, `tests/scripts/complex_scenarios.R` (tier 8 of
  `run_all.R`): monthly ages, staggered cohorts, mixed sampling frequency, study windows with calendar trends,
  AFR-lifespan confounding, unstandardised cubics on days, tiny data, the family screen, and beta proportions. Two
  traps are now named in the model-support help: a calendar-year trend within a study window mimics selective
  disappearance (add year as a random intercept); and with AFR correlated with lifespan, Model 10 can report false
  selective appearance (compare with Model 8).
* An empty decomposition (for example with cohorts sampled in alternate years) is now explained under the prediction
  figure instead of silently not drawn.

# disappR 0.22.6

* **Error-family warning without running the check.** After each fit, the best-supported model is refitted with each
  family of the trait's kind (zero-inflated families only when at least 10% of values are zero; skipped above 40,000
  rows), and an instant screen of the fitted model flags overdispersion, excess zeros, right-skewed residuals of a
  positive trait, or a count or binary trait fitted as Gaussian. Both report in the sensitivity banner, which names
  its source; the manual check is used instead when it matches the current fit.
* **Shorter wording.** The banner, the help panels and messages added since 0.21.16, the peak sentences, the store,
  random-effect and sampling-interval notes, and the methods draft and its conflicts were tightened and checked for
  accuracy. The methods draft describes peak ages only when the peak table is saved.

# disappR 0.22.5

* `tests/testthat/test-consistency.R`: the mean-age test read the two quoted ages by position, but the text names the
  older group first ("the gap between individuals ... older ages and individuals ... younger ages"), so it failed for
  every function although the ages were right. Each age is now read from its own label. No change to the app.

# disappR 0.22.4

A consistency audit (Part C of `inst/validation/audit_0_22_0/`, and the test tier `tests/testthat/test-consistency.R`)
checks every number and description the app reports against an independent truth, for all five ageing functions, with
and without standardisation. It found and fixed:

* **The mean-age term quoted ages that were not ages.** For the logarithmic and asymptotic exponential functions the
  text back-transformed the mean ageing term with the polynomial formula, printing a log- or exponential-scale value as
  "mean age" (6-9 years off), and for the asymptotic exponential, whose term falls with age, the "younger" group was
  the older one. The text now quotes the real mean ages of the individuals used, younger group first.
* **The reference of main effects named for each function.** When age is not standardised, main effects apply at age 0
  for the polynomials, at age 1 (or the youngest age) for the logarithm, and at no age for the asymptotic exponential;
  0.22.3 said age 0 for all. Standardised non-polynomial functions now name the centre with its age, and the
  mean-age interactions name each individual's own mean age.
* **Every table rounded small values to 0.00**, not only the coefficients: all tables now pass through a guard that
  shows values below 0.005 with three significant digits and leaves other columns as they were.
* The logarithmic term is described as log(age - youngest age + 1) when ages include 0.

# disappR 0.22.3

Model output when age is not standardised (reported on a cubic Model 10 with 'Standardise' off). The estimates and the
predicted ratios were correct, but the table and the text could not be read against each other:

* **Coefficients shown as 0.00.** The tables rounded every number to two decimals, so the age^2 and age^3 interactions
  of an unstandardised cubic (for example 0.0063 and -0.00023) appeared as 0.01 and -0.00. Coefficient and
  random-effect tables now show three significant digits.
* **Wrong term descriptions.** Without standardisation the age terms are age, age^2 and age^3 themselves, but they were
  described as "(age - centre)^2", and the intercept as "the value at the centre of every scaled variable". They now
  read "age^2", "age^3" and "value at age = 0 with every proxy at 0".
* **Wrong reading of the main effect.** With interactions, the main-effect coefficient is the difference at the
  reference point of age. Unstandardised, that is age 0, outside the sampled ages, yet the text said "at the mean age
  the two groups differ little" (in the reported fit the ratio at age 0 was x1.30 while at the median age it was x0.48).
  The text now says the main effect sits at age 0, that its test says nothing about the sampled ages, and gives the
  predicted difference at the median age. With standardised age the reference is named as the mean age (polynomial
  functions) or the centre of the age scale (the others).
* **Random slopes flagged as "explains no variance" by their scale.** A slope's variance is per unit of its covariate,
  so slopes on unstandardised age^2 and age^3 looked negligible however much they varied. Slopes are now judged by the
  variation they add across the records (variance x mean of the covariate squared).
* The random-effects table names the slope groups by individual rather than "id.1", "id.2", and the second and third
  ageing terms as age^2 and age^3.

# disappR 0.22.2

* **The null-model bootstrap carries the variance model.** When 'Residual variance / dispersion' is set to change with
  age, the null model is fitted with the same dispersion formula, its simulated data vary with age in the same way,
  and every refit uses it too. The 0.22.0 audit found that a variance rising with age, left unmodelled, triples the rate
  at which Model 4 is wrongly preferred (7% to 21%); a null simulated with a constant variance could not reproduce that.
  The result note, the help panel and the methods draft state the variance model used. With a constant variance
  (the default) the bootstrap is unchanged.
* **Family labels.** The binomial and beta-binomial families are labelled "binomial (logit link)" and "beta-binomial
  (logit link)" in tables, the stored-model list and the error-family check; the methods draft names families without
  the link, which it states separately.
* `tests/scripts/run_all.R` can be sourced from an R console: a failed tier stops with an error instead of quitting R.

# disappR 0.22.1

Fixes from the 0.22.0 audit (`inst/validation/audit_0_22_0/`).

* **Sampling interval and the decomposition.** Setting the interval to the value the app infers now gives exactly the
  automatic decomposition; before, the set-interval path re-gridded the records, which dropped 175 records of the
  chipmunk example. A set interval that differs from the inferred one now anchors its grid on the most common phase of
  the ages rather than on the youngest age, and draws each occasion at the mean of its records' real ages: a design
  with an offset first occasion (the bee example: day 1, then weekly from day 7) was drawn one day late (8, 15, 22...).
* **Share of wins.** An individual whose lowest AICc is shared by several functions (identical fits, for example two
  two-parameter functions on two distinct ages) now gives each tied function an equal fraction, so the shares sum to
  100%; before, each tied function counted a full win and the shares could sum to 160%.
* The 0.22.0 audit itself: `inst/validation/audit_0_22_0/` (README, captured output and the Python used). Unit tests
  added for both fixes.

# disappR 0.22.0

* **Sampling interval: inferred or set.** A tick box on the Missingness tab ('Infer the sampling interval
  automatically') can be unticked to enter the interval between occasions. The interval is used throughout the app: the
  sampling grid and missingness, the decomposition, the disappearance diagnostics (A4-A7), the integrity table, the
  bins and the prediction ages. With a set interval the decomposition places records on that grid and leaves out
  records more than a quarter of an interval off it, so an extra mid-year record in annual sampling no longer breaks the
  chain of annual links (before, such data were read as half-yearly sampling: missingness roughly doubled and the
  decomposition kept a handful of individuals per link). Exported scripts carry the interval.
* **Current data** box in the sidebar: for simulations, the sample size, average number of time steps (mean lifespan),
  functional form, selective disappearance and appearance, strength, sampling design and seed; for examples and
  uploads, the dataset and its size; always the sampling interval and whether it was inferred or set.
* **Compare models across fits.** A box beside the model comparison stores any fitted model, so models fitted with
  different ageing functions, random-effect structures, families, variance models or extra terms can be compared by
  AIC (for example a quadratic Model 4 with random slopes against a logarithmic Model 10). Stored models are grouped into
  comparison sets that share the data, the analysed records and the kind of likelihood (continuous density or discrete
  probability mass); ΔAIC is computed within a set only, and the reason sets differ is stated. No Akaike weights.
* **Gamma, lognormal and beta families** (glmmTMB): Gamma and lognormal (log link) for strictly positive traits, beta
  (logit link) for continuous proportions strictly between 0 and 1. Unsuitable values are refused with a message. The
  error-family check now compares the continuous families with each other (Gaussian, Gamma, lognormal, beta), as it
  compares the count families; a continuous family is never compared with a count family.
* **Residual variance or dispersion that changes with age** (Advanced options): constant (default), log-linear in age,
  or in every term of the ageing function, through glmmTMB's dispersion formula. Gaussian models with a changing
  variance are fitted with glmmTMB (Wald z-tests). Families without a dispersion parameter (Poisson, binomial,
  zero-inflated Poisson) ignore the option and say so. The equation panels, exported code, provenance and the
  random-effects table (log-dispersion coefficients) follow. The null-model bootstrap is unchanged and always fits a
  homogeneous variance; its help panel says so.
* **Share of wins** in the individual-level function comparison (step 4): Share_best is the percentage of the common
  individuals for which each function has the lowest AICc.
* **Peak age and onset of senescence for every fitted model** (Model output tab): read from each model's predicted
  population trajectory on a fine age grid, with 95% confidence limits from 1,000 draws of the fixed effects; a table
  and one sentence per model, saved to the summary on request.
* **Random effects against lifespan proxies** (Model output tab): each individual's predicted level and, with random
  slopes, ageing rate and curvature, correlated with its ALR, AFR and known lifespan (covariance, r with 95% CI, and a
  faceted figure). With Model 1 this reads Cov(LS, β₀) and Cov(LS, β₁) directly; the help panel states the
  BLUP caveats (Hadfield et al. 2010).
* **Auto-written methods** (Summary tab): a draft methods section, with references, built from the saved results the
  user ticks, using the data and settings stored with each result when it was saved. Conflicts are flagged: results
  from different data or sampling intervals, model results saved with different families, ageing functions, random
  effects, among-individual terms, variance models or standardisation, step-2 figures with different bins, and
  disagreements with the individual-level function comparison or the error-family check.
* Layout: the sensitivity banner sits above the AIC plot and table, which are side by side again; the prediction
  toggles are in two rows.
* Documentation: REPLICATION.md revised (the removed male-weight example dropped, the 0.21.0 audit's findings added
  to the tern, turtle, beetle and fly rows, and rows for the marmot, swift and Apollo examples); VALIDATION.md marked as
  the historical 0.9.7 study with pointers to the later checks. New unit tests in `tests/testthat/test-0-22.R`.

# disappR 0.21.16

* `individual_metrics()` guards its character summaries (condition label, group) as it already guarded the numeric
  ones, and the missingness grid does the same for the entry and lifespan columns: a frame without those columns gives
  NA per individual instead of an error. Only hand-built frames lack them; the app always creates them.

# disappR 0.21.15

* **The step-4 function ranking no longer collapses when one function cannot be scored.** AICc needs more records per
  individual than a function has parameters plus one; a cubic with six records per individual has none, and because
  an individual only counted when every function had a finite AICc, one such function emptied the comparison and the
  ranking silently fell back to adjusted R-squared. A function whose AICc is incomputable for most individuals is now
  left out of the AICc comparison, listed last, and named in a caution beside the table. The remaining functions are
  ranked by mean dAICc over the individuals they share, as the table always claimed. The default ageing function, the
  shape headline and the common-set warning read the same table, so they follow.
* **Population curves for rank-adjusted count models.** When glmmTMB drops collinear columns (AFR cubed with a cubic
  ageing function), its `predict()` refuses new data. The curve is now computed from the retained fixed effects, a
  dropped column counting as a zero coefficient; a test checks this equals `predict()` wherever `predict()` works.
  Fits, AICs and coefficients are unchanged.

# disappR 0.21.14

* **Population curves for binomial and beta-binomial fits with trials.** The trials were passed to glmmTMB as a
  vector, so predicting at new ages failed with "variable lengths differ (found for '(weights)')" and the trajectory
  figure stayed empty. They are now a column of the model data. The fits, AICs and coefficients are unchanged.
* **Population curves for the non-linear exponential.** `predict()` re-evaluates the nlme call, which named local
  objects (`fx`, the random and grouping structure, the starting values) that do not exist outside the fitting
  function. The values themselves are now stored in the call. The fits are unchanged.
* `diagnose_failures.R` also reports the rank-deficient cubic count models (Models 8 to 10).

# disappR 0.21.13

* Diagnostics only; the package is unchanged. The walkthrough uses the app's real input ids (`model_family`, the
  sidebar's tab names), fits all ten models before visiting the tabs, and reads only the log lines each step added,
  so one error no longer marks every later step as failed. `diagnose_failures.R` evaluates the test file's data
  builders inside the package namespace, as testthat does. The package check ignores names the package declares
  with `utils::globalVariables()`, which codetools does not see when called directly.

# disappR 0.21.12

* **Fixed silent data loss under a comma decimal mark.** With `options(OutDec = ",")`, which some European R
  set-ups use, `as.character()` writes 1.5 as "1,5". Numeric columns were converted through text, so every non-integer
  age or trait value became NA and was dropped without warning; integers survived, so the data looked partly intact.
  Numeric columns now stay numeric, numbers read back from table names are parsed with the decimal mark accounted for,
  and the app sets `OutDec = "."` for its own session. Found by the locale diagnostic.
* The missing-column message is a classed condition (`disappr_input_error`), like the integrity error.
* Diagnostics: the fuzzer counts a classed rejection as a clean rejection; the package check fails only on problems
  that can break at run time (unused locals and lints are reported, not failed); the walkthrough finds Chrome on a Mac
  or says clearly that it needs it; `diagnose_failures.R` loads the combination data builders from the test file.

# disappR 0.21.11

* The UI walkthrough sets `NOT_CRAN`, without which shinytest2 stops with "Reason: On CRAN"; the runner sets it for
  every script. `diagnose_failures.R` now ships in `tests/diagnostics/`.

# disappR 0.21.10

* **A mapped column that is not in the data now gives a clear message** ("Mapped column not found in the data: X")
  instead of "arguments imply differing number of rows". Found by the import fuzzer; it affects the API, not the app,
  where columns are chosen from the file.
* **R CMD check notes cleared:** the Shiny and shinydashboard helpers the UI functions use are imported in NAMESPACE,
  the two aesthetic column names are declared as global variables, and the long line in the API example is split.
* **Diagnostic scripts fixed** (mine, not the package's): the suggested-package matrix now inlines the hidden
  package name into its tracer, the export check runs the script in a directory holding the file it reads, and the
  locale check reports why a fit failed instead of failing on the comparison.

# disappR 0.21.9

* **Fixes from the first full R test run.**
  - `individual_metrics()` no longer fails when the data frame carries no condition column: a missing column gives
    NA per individual instead of an error in `tapply()` (six failures in the missingness grid tests).
  - `prediction_contrast()` returns an empty frame when two predictions share no ages, rather than failing on the
    assignment.
  - `proxy_pair_plot()` qualifies its ggplot2 calls, so it works when the package is used without ggplot2 attached.
    It was the only plotting function left unqualified.
  - `wrap_label()` lets a line fill the requested width; `strwrap()` breaks strictly below it.
  - A failed population prediction now carries the error message and the newdata columns as attributes, so the
    reason can be seen instead of an empty data frame.
  - The provenance test checks the lines that must be present rather than a fixed count.

# disappR 0.21.8

* The diagnostics can be run from the R console as well as from a terminal: they no longer quit an interactive
  session, and `source("tests/diagnostics/run_diagnostics.R")` defines `run_diagnostics()`. The walkthrough no
  longer needs rlang's injection syntax.

# disappR 0.21.7

* **Out-of-app diagnostics** in `tests/diagnostics/`: suggested-package matrix, exported-script reproduction,
  importer fuzzing, locale and encoding, determinism and scale, package-level checks, and a shinytest2 walkthrough
  of the interface. `Rscript tests/diagnostics/run_diagnostics.R` runs them all, each in its own process.

# disappR 0.21.6

* **Packaging fix:** nine directories in the source tree (including R/, man/, inst/ and tests/) were shipped without
  write permission, inherited from a read-only copy. R CMD build copies permissions into its temporary tree, then
  cannot create inst/doc there, which failed as "more 'from' files than 'to' files" while creating vignettes and left
  the cleanup unable to remove anything. All directories are now writable.

# disappR 0.21.5

* The mean of individual curves (population average) is off by default on the average-trajectory figure; tick it to
  draw it alongside the typical individual.

# disappR 0.21.4

* The published-dataset dropdown is ordered by first-author surname, as in the manuscript's reference list.

# disappR 0.21.3

* **Sensitivity checks appear in a fixed order:** random effects, then the error family, then the ageing function,
  then anything else. The error-family flag fires only after that check has been run, and is guarded against a
  missing model label or family.

# disappR 0.21.2

* **The sensitivity check flags the error family.** When the error-family check has been run and another family fits
  the same model more than 2 AIC better, the box says so and notes that the wrong error distribution inflates
  interaction terms.
* Documentation no longer carries release-status banners; the README points at `tests/scripts/run_all.R` instead.

# disappR 0.21.1

* **Pre-submission audit shipped** in `inst/validation/audit_0_21_0/`: the static results, the scope scan, the
  numerical checks and the Python scripts behind them, so a reader can reproduce or challenge any of it. Summary:
  structure and wiring clean, no unassigned-object bugs left, 38 of 38 logic checks pass, the decomposition
  reproduces Supplementary S1 from the app's own algorithm, all 50 validation cells reproduce exactly, and eleven of
  thirteen empirical examples reproduce their paper's direction under the default settings.
* **Two documentation gaps closed:** the methods vignette describes slopes for every age term and why a slope on age
  alone leaves curvature uncovered; the workflow vignette's multi-group wording is corrected.

# disappR 0.21.0

* **Less text, in the places it was repeated.**
  - The random-effects setting now reports only how many individuals support a random term; the caution about
    interaction models absorbing heterogeneity lives in the sensitivity check alone.
  - The sensitivity check's wording: these data give weak/limited/good support for fitting random slopes; random
    slopes account for heterogeneity in ageing, and leaving them out can inflate the fit of interaction models,
    because the interaction recovers some of that heterogeneity; refit with slopes, or with a slope for every age
    term, to check that the interaction's advantage is not a false positive.
  - The supported-set box gives the set, the most parsimonious model in it and the fitted equation. The Model 4
    versus Model 5 line and the 'not distinguished by these data' paragraph are gone.
  - The truth-comparison box keeps the relativised deviation formula; the explanation moved into its help panel.
  - The ageing-function note under the selector no longer repeats the sensitivity check.
  - Internal consistency is folded away until clicked.
  - Saved model comparison: no unlabelled formula line, no restatement of which model has the lowest AIC.
  - Saved coefficients: the fitted equation with random terms and covariates, the family and scale, then one line
    naming each selection term's support and what it implies.
* **The null-model bootstrap** speaks of a lifespan-proxy term (ALR, lifespan or mean age, whichever the model
  uses), not of a lifespan term, since most datasets have no lifespan column.
* **Coefficient interpretation** now reads the interaction and the main effect together: when the interaction is
  supported it says whether the groups also differ at the mean age, or whether the association appears only through
  its change with age.

# disappR 0.20.28

* **A random slope for every age term.** Two structures join the dropdown: 'Uncorrelated intercept and every age
  term' and 'Correlated intercept and every age term'. With a quadratic or cubic function they put a slope on each
  age term, which is what covers among-individual differences in curvature; with a one-term function they collapse to
  the existing slope structures. The formula, the equation panel, the settings summary and the exported R code all
  follow, and the non-linear suite maps them onto its two parameters.
* **Caution added** where the app explains the random effects: a random slope covers only the age terms it is fitted
  to, individuals can also differ in curvature, and leaving that unmodelled inflates the fit of interaction models.
* Tests for the new formula strings and their display, including the one-term collapse.

# disappR 0.20.27

* **Fixed a crash introduced in 0.20.19.** `standardise_data()` built its multi-group report only when a grouping
  column was mapped, but read it whichever way, so any dataset without a group stopped with "object 'multi_group'
  not found": the simulator, uploaded files and every example except the two with a mapped group (fly, beetle). The
  report is now created before the branch. A regression test standardises data with no grouping column and checks
  the data-integrity table builds.

# disappR 0.20.26

* The model-comparison box is now titled 'Which selective processes best explain the data?', and its help says what
  Models 1 to 10 are for: comparing selective disappearance against selective appearance, each carried either by a
  covariate term (ALR, AFR or lifespan) or by the centred among-individual term, and each as an age-independent or an
  age-dependent effect, with any mapped covariates entering every model additively.

# disappR 0.20.25

* **Examples carry the random-effect structure their paper used.** An example can now set the random structure, and
  loading one applies it (examples without it return to a random intercept). The Clouded Apollo and the seed beetle
  open with a correlated random slope, as their papers fitted. The tern immunity example keeps a random intercept:
  re-reading its AIC table, random intercepts fitted both traits better (dAIC 4.0 for the titre, 2.6 for
  haptoglobin), and an independent refit here reproduces that gap of 4.0, so the earlier note was corrected.
* **Documentation caught up with the last dozen versions.** The models-and-methods vignette lists Models 9 and 10,
  states the shared-row rule and where individual means come from, and mentions the random-slope options. The
  workflow vignette describes the observed-mean overlay, the points and population-average toggles, the trait-scale
  advice, the duplicate rule and the multi-group flag. The development vignette documents test tier 7 and the
  separate R process for step 4.

# disappR 0.20.24

* **Study descriptions checked against the papers.** Twelve of the thirteen examples were compared with their source
  paper (the fly, tern immunity, marmot, swift, Apollo, tern navigation, beetle, chipmunk supplement, great tit,
  turtle, both Soay files and the bee). Corrections:
  - Fly: the published selective-disappearance term is the daughters' lifespan, not ALR, entered additively beside a
    quadratic age term; the note now says so and points to Model 6.
  - Tern immunity: the paper's second model set (age, age at first measurement, age at last measurement) is Model 7
    here, and that is where its conclusion about selective appearance and disappearance comes from, so **Model 7 is
    now in the example's model set**; random slopes fitted the titre better.
  - Alpine swift: its disappearance term is lifespan and its appearance term is AFR, both additive.
  - Clouded Apollo: random-slope models fitted better than random-intercept ones.
  - Tern navigation: splitting age into among- and within-individual components is Model 3 here.
  - Chipmunk: the supplementary tables give the best female model as age + age2 + season + AFR + season x age +
    season x age2 + AFR x age + AFR x age2 + lifespan, which is Model 10 with lifespan in place of ALR.
  - Great tit: the paper fitted ALR, ALR2 and ALR x age with terminal effects (Model 4 with a quadratic proxy), and
    modelled recruits as counts, so the family must be switched to Poisson for the count traits.
  - Painted turtle: the paper fitted no lifespan or ALR term at all, so the published specification is Model 1 with a
    covariate and every other model there goes beyond it.
  - Marmot, both Soay files, the beetle and the bee needed no change; their notes already matched their papers.

# disappR 0.20.23

* **Trait against age by lifespan bin:** the observed mean of the trait at each age, across every individual, is
  drawn as grey open circles sized by the number of individuals, on the same scale as the bins (raw or
  log(trait + 1)), so the bin trajectories can be read against the data behind them. 'Show observed means' switches
  them off; the circle size means the same as on the bin points.

# disappR 0.20.22

* **Average within-individual trajectory reaches further.** Its drawn range now covers the ages held by at least five
  individuals, not ten (`A3_MIN_IND_PER_AGE`). The threshold sets the range only: the curve is the fitted function at
  the mean coefficients, so values at ages already covered are unchanged, while the tails now extend into thinner
  ages (with a wider band). The figure caption and the help say five.
* **Missingness help.** The sampling-grid 'More' text now says that the manuscript uses 'missingness' both for
  occasions missed between AFR and ALR and for those between ALR and true lifespan, that only the first is
  calculable when lifespan is latent, and that the detection rate reported here is therefore an upper bound.
* The decomposition is unchanged: after a break the chain restarts, anchored on the survivor-restricted mean of the
  individuals recorded at both ages of the resumed link.

# disappR 0.20.21

* **IDs in more than one group are flagged while mapping.** A red note sits directly under the 'Nested' toggle,
  before anything is fitted: how many IDs are involved, the first few by name, what nesting does to them, the crossed
  alternative (untick nesting and add the grouping column under 'Additional random intercepts'), and a pointer to the
  data integrity checks.
* **Binomial traits in the sweep.** `tests/scripts/sweep_0_20.R` now simulates proportion traits under no selection,
  age-independent and age-dependent selective disappearance, with a second record at the same age for some
  individuals. It checks that the trials column is used as weights, that repeated records are kept rather than
  merged, that the binomial and beta-binomial suites fit with finite estimates, that the expected model wins, that
  the ALR x age term has the simulated sign, and that dropping the trials column widens the ALR standard error.
* **Binomial validation notes** (`inst/validation/v0_20/python/binomial_checks.py` and `.md`): outside R, ignoring
  the trials leaves the estimate unbiased but inflates its standard error about threefold; keeping both records at an
  age and pooling successes and trials are equally precise while averaging proportions is noisier; the naive model
  reverses the sign of the age slope (+0.19 against a true -0.30) and adding ALR recovers it; the ALR x age term is
  recovered (+0.19 against +0.18) and detected in every run, with a 5% false-positive rate when absent; and
  beta-binomial data read as binomial give a Pearson chi-square per degree of freedom near 4.

# disappR 0.20.20

* **New test tier: `tests/scripts/sweep_0_20.R`** (tier 7 of `run_all.R`), covering what changed in 0.20.9-0.20.19:
  the Trajectories page with sparse individuals and count traits on both scales (the crash case), the clamping of
  extreme curves, duplicates and IDs in two groups in the data checks, per-model row sets and Model 6's availability,
  the evidence summary across Models 1-10, the likelihood-ratio pairs, random terms that explain no variance, the
  visual gap statistics, and every bundled example (models for four of them; set DISAPPR_SWEEP_FULL=1 for all).
* **App server test extended** with the controls added since 0.20.9: the gap figure's points toggle, the
  average-trajectory toggle, the trait-scale advice, the log scale for individual fits, the function-comparison
  button, duplicate collapsing, Models 7-10, and the new warning and flag outputs. It also checks that no model is
  recorded as using more rows than it can and that every evidence line carries a known status.

# disappR 0.20.19

* **Individuals in more than one group are named.** The default is unchanged: with nesting the individual is
  group/ID, so an ID appearing under two groups is analysed as two individuals. The data check 'IDs linked to >1
  higher-level group' now names them and the groups they appear under, says what that means for their random
  intercept, ALR, AFR and mean age, and points out the alternative when the same animal really moved between groups
  (map the grouping column as an additional crossed random term instead of a nesting level). IDs missing a group on
  some rows are counted too, because they split the same way.
* Test: the naming of individuals spread across groups, including rows with no group.

# disappR 0.20.18

* **Model comparison warnings (no change to the statistics).** All selected models are still fitted to the rows every
  one of them can use, because AIC and likelihood-ratio tests only compare models fitted to the same data. The
  Modelling tab now says when that costs a model rows, naming each model's own usable rows and individuals, why rows
  were dropped and what can be done instead. It also warns when extra terms were added to some models only (AIC
  differences then mix two changes), and states that mean age and the within/between split are computed from all rows
  available to Models 3, 5 and 6, not only the shared ones. Model 6 still requires a mapped lifespan column.
* **Evidence summary reads all ten models.** The kind of selective disappearance and appearance now comes from the
  winning model's own structure (proxy absent, additive, or interacting with age), for any of Models 1-10, instead of
  only Models 1, 2 and 4. The naive model being within 2 AIC of the best still gives the cautious reading, 'none'.
* **Likelihood-ratio pairs.** The interpretation of a chosen model now quotes the comparison that tests that term in
  that model: for Model 8, Model 10 vs 8 for the ALR x age term and Model 9 vs 8 for the AFR x age term.
* **Saved model sets.** The settings key includes the set of models compared, so saving Models 1-4 and later Models
  1-10 on the same data is no longer read as two conflicting findings.
* **Duplicate records.** Nothing is averaged. 'Collapse exact duplicates' (off by default) collapses only rows that
  agree in every mapped column - the same record entered twice. Rows that differ in anything, including the trait, are
  two measurements and are always kept. The data checks name the individuals and ages, say which columns differ, and
  a help panel beside the setting explains the choice.
* **Version provenance** asks the loaded package first and no longer falls back to an invented '0.9.7'.
* DESCRIPTION: 13 empirical examples from 12 published studies (was 'ten'). Non-linear help text: age is fitted on the
  standardised scale, and the claim about the logit link is gone. The dead 20 MB upload block is removed. The README
  status line now points at the test suite.

# disappR 0.20.17


* **Evidence summary, model comparison:** a tick only when one model clearly wins (no other model within 2 AIC of the
  best); the note then names the next model and its ΔAIC (and Model 1's). When several models are within 2 AIC, the
  line is a caveat ('!') that lists the supported set and says that parsimony suggests interpreting the simplest (the
  fewest parameters, named), and the overall level is at most 'moderate'. The same rule applies to the selective-
  appearance summary.
* Test: a clear winner gives a tick; a supported set gives a caveat, names the simplest model and caps the level.

# disappR 0.20.16


* **Average within-individual trajectory:** 'Show the mean of individual curves (population average)' switches that
  curve and its band on or off (on by default), leaving the typical individual (mean of coefficients) and, for
  simulated data, the truth. The axis then fits the curves shown.

# disappR 0.20.15


* **Average within-individual trajectory:** the help says how the mean of coefficients (typical individual) and the
  mean of individual curves (population average) differ and how to read a large gap between them. Its 'More' text and
  the figure's subtitle said the two coincide for every polynomial; that holds on the raw scale only - fitted on the
  log scale, every function is drawn as exp(...), so the two differ. Both now say so.

# disappR 0.20.14


* **Scale of the individual fits explained.** A help panel next to 'Scale of the individual fits' says what the raw
  and log scales are for (continuous traits and counts), how each is fitted, and why a Linear function fitted on the
  log scale is drawn as a curve: log(expected count) = b0 + b1 age, so the count itself is exp(b0 + b1 age). The
  individual-fits figure also says so in its subtitle when the fits are on the log scale.
* An individual whose records are all equal now gets a readable axis around its value (the axis repeated the same
  number).

# disappR 0.20.13


* **Trajectories crash found and fixed.** The console log of a crash showed the fits finishing (0.09 s) and the app
  dying while the average trajectory was drawn, with individuals of about three records each. Curves extrapolated to all
  ages - and, on the log scale, exponentiated (0.20.5 capped them below infinity, which kept them in the figure) - can
  be astronomically large. coord_cartesian() hid them, but the graphics device still received their coordinates and
  can crash on them, taking the app with it. Every line and band of the average-trajectory figure is now clamped near
  the visible window before drawing; each individual's curve is kept near its own records; and the optional
  reconstruction on the model-prediction figure leaves out parts far beyond the models' predictions.
* Test: the clamping.

# disappR 0.20.12


* **Step 4 is isolated from the rest of the app.** With the callr package installed, the individual fits run in a
  separate R process. Whatever goes wrong there - an error, a hang, or a failure that stops R - ends only that
  process: the page says why, and every other page keeps working. A job running longer than 180 seconds is stopped.
  The progress bar still follows the individuals fitted. The comparison fits all the functions not yet fitted in one
  job. Without callr the fits run in the app's own process as before, and the page says how to isolate them.
* callr is suggested, installed by launch_app.R and listed in the README's optional packages.
* Test: several functions fitted in one call, with progress across all of them.

# disappR 0.20.11


* **Gap between lifespan groups (Visual diagnosis):** 'Show individual points' hides the points (one difference per
  age and bin pair) to show only the trend lines and their bands. On by default; the weighting still applies to the
  lines.

# disappR 0.20.10


* **Random effects table:** a random term that explains no variance is flagged ('explains no variance'), with advice
  below the table. A term is flagged when it explains less than 1% of the total variance (random terms plus residual)
  in Gaussian models, or when its standard deviation is below 0.05 on the log or logit scale in count and binomial
  models, which have no residual variance on that scale. The advice: 'Random term X explains no variance, try
  refitting without that term'; for random slopes, refit without random slopes; the individual random intercept is
  kept (the repeated records of each individual need it), so its message says what a zero variance means instead.

# disappR 0.20.9


* **Trajectories plot again.** The page-wide busy lock added in 0.20.7 kept the Trajectories page from finishing
  ('Working, please wait', then 'Still working', and no figure). 0.20.6, without the lock, plotted. The lock is
  removed, and the two figures whose height follows the data (individual fits, sampling grid) are drawn exactly as in
  0.20.6: the fixed-height containers of 0.20.8 rested on a mistaken diagnosis and are reverted.
* **Rapid clicks:** instead of locking the page, a button is disabled for 1.5 seconds after it is clicked, so
  repeated clicks on it cannot queue the same work several times. Nothing else on the page is affected.

# disappR 0.20.8


* **Trajectories hang fixed at its cause.** The individual-fits figure (and the sampling grid) had an automatic height.
  A scrollbar appearing or disappearing changed the page width, which redrew the figure, which changed the page height
  again, so the app could stay busy for good; 0.20.7's busy lock then held the page. Both figures now sit in a
  container of fixed height. The busy lock also lifts itself after 8 seconds ('Still working' is shown instead), so a
  long calculation can never lock the page.
* **Legends:** the sampling-grid legend wraps its labels into two columns; the average-trajectory and model-prediction
  legends wrap more.
* **New help panels:** nested likelihood-ratio tests, and fitting status (convergence, singular fits and a Hessian
  that is not positive definite).
* **Wording:** the AIC help recommends the simpler model when ΔAIC < 2 (parsimony); the bootstrap names Model 1 the
  more parsimonious interpretation; the sensitivity banner suggests refitting with random slopes where the data
  allow; the internal-consistency note on random slopes; the distribution help says it shows the observed data
  (after missingness, for simulated data); the Advanced box is 'How large is the selective process's influence, and is
  it stronger than chance alone would produce?'.
* **Evidence summary:** levels and processes in capitals (for example 'STRONG evidence ... AGE-DEPENDENT SELECTIVE
  DISAPPEARANCE'); the gap line's caveat is a footnote in small text; the random-slope line explains what a preserved
  interaction means. The HTML and text reports now include the selective-appearance summary and the footnotes.

# disappR 0.20.7


* **Evidence summary:** the gap figure's reading is a secondary line, 'Gap between groups (secondary)', with its
  average gap and its change per unit age (weighted least squares, inverse-variance weights). It supports the finding
  when it agrees and is a caveat when it does not, but never changes the level: its p-values are optimistic because
  points share individuals, and group gaps drift with age as each group loses its shortest-lived members first.
  Shown for selective disappearance and, from AFR groups, for selective appearance.
* **Trait scale advice (Visual diagnosis):** a box in the bins settings says whether the trait looks continuous or
  like counts and whether the chosen scale suits it (raw for continuous traits, log(trait + 1) for counts), in red when
  it does not: on the wrong scale, differences between groups can shrink, vanish or reverse. The cautions of each
  trait-based figure say the same (the disappearance hazard does not depend on the trait scale).
* **Saved sampling grid:** only the share of expected occasions missed between each individual's first and last
  recorded age.
* **Legends:** the average-trajectory and model-prediction legends wrap their labels and use several rows.
* **Model output table:** 'Per' is renamed 'Term description' and shown second, after the term.
* **Busy lock:** while R works for more than a moment, the page ignores clicks and changes and shows 'Working, please
  wait', so a burst of clicks cannot queue up work faster than the app can do it.
* Tests: the gap line and its statistics, and the label wrapping.

# disappR 0.20.6


* **Mismatched engine stopped with an explanation.** Installing a new version while the old one is loaded in the R
  session left the old engine in memory, so the new Trajectories page called functions that did not exist yet
  ('unused argument (progress = pr)'). The app now compares the installed version with the one loaded and, if they
  differ, stops with: restart R, then run disappR::run_app() again.
* **Evidence summary, in two parts:** selective disappearance (figures grouped by LS or ALR; Models 1-6) and selective
  appearance (figures grouped by AFR; Models 7-10). Each visual line now states both statistics behind it: the average
  coefficient of the trait on the grouping variable across ages (a difference from zero) and its change per unit age,
  with bootstrap p-values over individuals. A figure's evidence uses its own grouping variable and trait scale (known
  lifespan when it is the grouping, AFR for appearance). The selective-appearance summary is at most 'moderate': the
  random-slope and null-model checks are run for disappearance only. The separate lifespan-term line (Model 2 vs 4) is
  removed.
* **Model comparison summary:** the model, its formula and settings, then only which model has the lowest AIC and the
  ΔAIC of the next best; the LRT sentences and the Model 4 vs 5 discussion are gone (the saved table has them).
* **Model output table:** age terms and lifespan-proxy terms (ALR, AFR, LS, mean age; additive and interactive) in bold.
* **Missingness:** the saved sampling grid reports the share of expected occasions missed between each individual's
  first record (AFR, or AFE if set) and last record (ALR); the grid's legend wraps within its box.
* **Wording:** step-4 help (the scale and minimum-records notes moved to 'More'; how to read the fits; the average
  trajectory; the function comparison; the coefficient summary), the Modelling goal (Models 7-10), the standardisation
  note, and the evidence help.
* Tests: the appearance summary, the visual statistics, grouping by LS or AFR, and the bold terms.

# disappR 0.20.5


* **Trajectories (step 4) no longer does work you did not ask for.** The six-function comparison runs only when you
  press 'Compare ageing functions across individuals', and opening the Modelling tab no longer starts it (the
  Modelling tab uses the comparison if you ran it). Each function's individual fits are cached and reused, including
  inside the comparison, so no function is fitted twice for the same data and settings.
* **Progress and timings.** The progress bar moves with the individuals fitted (up to 100 updates per function), and
  the R console logs when the tab opens, how long each function took and when each figure is drawn, so a slow step
  can be told from a stalled one.
* **Cap.** With more than 2,000 individuals, step 4 uses an evenly spaced subset of 2,000 and says so on the page;
  the models use everyone.
* **Overflow guard.** Exponential curves are capped below the largest finite number, and an individual whose curve
  is still not finite is left out of the mean of functions, so extreme fits cannot produce Inf or NaN.
* **Logo.** The header image is now `www/disappR_logo.png`, requested with the version number, so a browser cannot
  keep showing a cached copy of the old logo.
* **Examples:** 13 datasets from 12 published studies (the Soay sheep study provides two), stated as such on the
  start page, in the help and in the README.
* Tests for the progress callback, the reuse of fits and the overflow guard.

# disappR 0.20.4


* **Gap between lifespan groups:** the optional weights are now inverse-variance weights (the toggle is renamed
  'Inverse-variance weights'). Each difference is weighted by 1 / (SE² of one bin mean + SE² of the other); a bin
  whose individuals share one value uses the pooled within-bin variance. Points are sized by weight. Replaces log2 N.
* **Does the trait track lifespan, and does that change with age?** The separate change-in-slope figure is removed:
  a line fitted through a few changes, with a very wide band, contradicted what the slopes show. Its statistics are
  printed in the caption of the remaining figure: the slope in each age bin, its change from the previous bin, and the
  trend of the slope across age (weighted by the inverse of each slope's variance) with its p-value. The figure spans
  the full width; its help text is revised.
* **Copy buttons** say 'Copied' or 'Could not copy'. When the browser blocks the clipboard (for example in the RStudio
  viewer) they try the older copy method before reporting failure.
* **Age at first trait expression (AFE):** a missing, negative or impossible value now gives a message on the
  Missingness tab instead of an error.
* **Covariate types:** a categorical covariate with non-whole numbers is always flagged (before, only with more than
  five values). The messages say the mapping can be kept; nothing is blocked.
* **Tests:** inverse-variance weights and covariate warnings. The app test read five outputs that no longer exist
  (a2_slope_table, a2_trend_note, a4_status, definition_table, varcomp_table), so it could not pass; they are removed,
  and the static audit now checks that every output a test reads exists.

# disappR 0.20.3

Wording and logo only: no calculation changed.

* New logo in the app header.
* Start page: the purpose, the four gaps, the three ways to start and the six step descriptions reworded. The workflow
  help now explains how to use tabs 1-6 and 'Save to summary' (help entries can open with a lead paragraph).
* Data page: the start instructions, the statement on the bundled examples, the AFR note, and the descriptions of the
  fly, Alpine swift, seed beetle, chipmunk, great tit, painted turtle and Soay sheep examples revised. For the swift,
  chipmunk and beetle examples, the note on excluded individuals or the derived identifier now appears above the data
  integrity checks instead of in the description.
* Visual diagnosis, Missingness and proxies, Trajectories: guidance, titles and 'Look for' text revised; the caution
  on log weights in the lifespan-association help removed.

# disappR 0.20.2

(below). Run `tests/scripts/release_gate.R`, commit the golden reference it writes, and fix what fails before tagging.

## Data integrity: conflicting values stop loading again (reverses 0.20.1)
* 0.20.1 averaged an individual's conflicting ALR, lifespan or AFR values, with a warning. The mean can be
  impossible. The Alpine swift Frontiers_000304 has lifespan 2, 3 and 4 in its three records (the column repeats its
  age) and laid eggs at age 4, so its averaged lifespan of 3 fell before its own last breeding record - a value the
  app's own check calls impossible. Chipmunk D037 had young at age 7 but was given an AFR of 8.5.
* Loading now stops with a data-integrity error (class `disappr_integrity_error`) naming the individuals and the
  column. `inconsistent = "exclude"` drops them instead (in the app, the checkbox under the lifespan column), and
  the integrity table reports them.
* The swift and chipmunk examples exclude those four individuals, with a note. The stress-test count file's `ls`
  column (age + 1 in every row) is no longer mapped as a lifespan, and a new check confirms that mapping it stops
  loading.
* The beetle example keeps its derived identifier. Its nested Family/ID already separated the same 637 beetles with
  no conflicts; the derived column protects analyses run without nesting.

## Evidence summary
* The finding's direction came from the sign of the first age coefficient. The asymptotic-exponential basis,
  exp(-z_age), falls with age, so for that function a declining trait was described as increasing. The direction now
  comes from the finding model's predictions: the slopes of a long- and a short-lived individual (90th and 10th
  percentiles of the proxy) between the 25th and 75th age percentiles, on the link scale. The sentence also handles
  long-lived individuals improving while short-lived ones decline.
* The finding comes from the latest comparison fitted with a random intercept, for the latest analysis. A later fit
  with random slopes is the sensitivity check. Before, a later random-slope fit replaced the finding, so a failed
  sensitivity check was never seen, and the existing test "a failed random-slope check points to heterogeneity in
  ageing" could not pass.

## Null-model bootstrap
* The default is now 39 simulated datasets (was 20). With 20, one failed refit made p < 0.05 impossible (the smallest
  p is 1/(n + 1) for n refitted datasets), so a strong result read as "partial". Fewer than 19 refitted datasets now
  read as "too few", graded as a caveat.
* The test records the settings of its own fit. It recorded the among option requested, while fits record "linear"
  whenever the ageing function is linear, so a valid test with a linear function and a higher-order among option was
  always set aside.
* Calibrated in the Python emulator (`inst/validation/v0_20/python/`; 39 null draws, so a valid test rejects about
  2.5% of the time):

| Scenario (60 datasets each) | AIC > 2 | LRT | Permutation (≤ 0.19.6) | Bootstrap, 0.20.x null | Bootstrap, correlated-slope null |
|---|---|---|---|---|---|
| clean null | 0.05 | 0.05 | 0.017 | 0.000 | 0.017 |
| misspecified ageing function, no selection | 1.00 | 1.00 | 1.000 | 0.000 | 0.033 |
| heterogeneous ageing rates, no selection | 0.25 | 0.25 | 0.050 | 0.050 | 0.017 |
| rate-linked selection (strength 1) | 1.00 | 1.00 | 1.000 | 1.000 | 1.000 |
| rate-linked selection (strength 0.5) | 1.00 | 1.00 | 1.000 | 0.933 | 0.933 |
| misspecified function and heterogeneous rates, no selection | 1.00 | 1.00 | 0.967 | 0.017 | 0.017 |

## Smaller corrections
* Effect sizes name their scale ("log expected trait per unit age", "log-odds of the trait ...") instead of "trait
  per unit age" for count and binomial models. Unstandardised fits note that slopes refer to age 0.
* DESCRIPTION imports grDevices and graphics, which the package code uses (an R CMD check warning).
* `launch_app.R` installs, and `run_app()` reports, every optional package a feature uses: nlme, performance, see and
  codetools were missing from both.
* Info panels give the bootstrap's default and minimum, and say which saved comparison the finding comes from.
* README rewritten as an overview: the project's purpose (in the words of the app's start page), how to launch the
  app, and its features grouped by aim (load and check data, diagnose, model, summarise, learn with simulations).

## Tests
* `test-conflicts.R` rewritten for error-by-default and explicit exclusion.
* `test-combinations.R` gains 16 cases: nbinom1, zip, zinb, zinb1, binomial with trials, beta-binomial, binary
  traits, and the non-linear exponential. Every exported script must now parse.
* New cases in `test-evidence.R` (direction with the asymptotic exponential, the random-slope rule, too few
  refits) and `test-bootstrap.R` (default, too few, settings key).

## Checks run without R
* **Syntax.** A parser that follows R's grammar (`tools/static_audit/`). It passes 64 self-test cases, accepts all
  66 files of 0.19.6, and finds no syntax error in the 75 R and R Markdown files of 0.20.2.
* **Calls.** Every call is matched against the called function's formals, as R matches arguments. The checker is
  shown to catch unused, surplus, partial and empty arguments. It finds none; every unresolved name is accounted for
  (testServer reactives, base functions, validation helpers).
* **App wiring.** Every input the server reads is created, every output shown is defined, every help panel exists,
  and every example's mapped columns exist in its CSV.
* **Validation reruns.** The independent study's four censored cells could not be reproduced from their seeds: the
  censoring step iterated over a Python set, whose order follows the per-process hash seed. Fixed; their 400
  replicates were regenerated and VALIDATION.md updated. The other 46 cells reproduce the committed replicates to
  within 1e-11. `summarise_vv.py` read a file name that is not shipped; fixed.
* **Statistics** (`inst/validation/v0_20/python/stats_checks.md`):
  - Step 4's QAICc chose the same function as the correct likelihood in every scenario tested. The plain Poisson
    AICc used before 0.20.0 chose a more complex function in 98% (negative binomial) and 84% (zero-inflated) of
    datasets where the correct likelihood chose the simpler one.
  - The coefficient-sign rule for the direction is wrong only for the asymptotic exponential, and the
    prediction-based rule is right for every function.
  - Standardising age leaves AIC unchanged to within 3e-7 with random intercepts or correlated slopes, and changes it
    by a median of 31 units with uncorrelated slopes.

# disappR 0.20.1

* Conflicting ALR, lifespan or AFR values within an individual are averaged again, as before 0.20.0, but no longer
  silently: the data integrity check now gives a warning that names the individuals and says their mean is used.
  In the bundled examples this applies to one Alpine swift and three chipmunks. The beetle example keeps its derived
  identifier: those were different beetles sharing an ID, not one beetle with conflicting values.
* The evidence summary's info panel described the null test as a permutation test. It now describes the null-model
  bootstrap, and which results count: the bootstrap only when run on the model the finding rests on with the same
  data and settings, a manual ageing-function check only for the function fitted and not when run on Model 1, and
  results from an earlier analysis with other settings set aside.

# disappR 0.20.0

Answers a code review. Every point was checked against the code and found to be real.

## Evidence summary
* The finding's direction ("longer-lived individuals decline faster...") came from Model 1's ageing slope, which is
  biased by selective disappearance and can have the opposite sign to within-individual ageing. It now comes from the
  finding's own model: Model 4 for age-dependent, Model 2 for age-independent.
* Every saved result records the data and settings it came from, and the grader combines only results from the same
  analysis. A comparison refitted with a corrected ageing function now supersedes the earlier one (noted in the
  cautions) instead of producing "Mixed"; differences in random structure remain the sensitivity check.
* The null test counts only when it was run on the finding's model - an interaction model for an age-dependent
  finding, an additive one for age-independent - with the same data and settings. A test of Model 2 no longer
  supports an age-dependent finding from Model 4.
* The manual ageing-function check undid the 0.18.0 fix: it defaulted to Model 1, and a saved result overrode the
  automatic check, so a correct function could be flagged. It now starts on the best-supported model once models are
  fitted, records the model it used, is ignored when run on Model 1 for a finding about selection, and is compared
  with the function actually fitted rather than the dropdown.
* The visual check left a fixed random seed behind when none existed, making later random draws repeat. Fixed.

## A null-model bootstrap replaces the permutation test
* Shuffling lifespans between individuals created records after an individual's last record or death, so anything
  tied to the observation window beat every shuffle - which is why a misspecified function and terminal decline
  came out significant without selection. Those were failures of the null, not alternatives to rule out.
* bootstrap_test() fits a model with no lifespan term - a cubic ageing function, individual differences in level and
  rate of ageing, and the analysis's covariates and family - simulates new traits from it keeping every individual's
  real ages, ALR and AFR, and refits. Same cost; the observation windows stay real. Models 3 and 5 can now be tested.
  Not yet available for binomial traits with trials or the non-linear exponential. permutation_test() remains in
  the package for reference.

## Step 4
* The individual fits depended on which individuals were drawn, so changing or reshuffling the drawn set refitted
  every individual - on large data, repeated full refits behind a frozen progress bar, the likely cause of the
  reported step-4 crash. Every individual is now fitted once; the drawn curves are rebuilt from the stored
  coefficients (individual_curves()).
* Counts modelled as negative binomial or zero-inflated are compared by QAICc: the Poisson likelihood with the
  dispersion estimated once from the most flexible function (Burnham and Anderson 2002). The table says so.

## Data integrity
* Conflicting ALR, lifespan or AFR values within an individual were averaged. Such individuals are now excluded and
  reported as an error. This found real conflicts in three bundled datasets: one Alpine swift (lifespans 2, 3 and 4),
  three chipmunks (AFR 7 and 10), and the seed beetles - where a character lost when the file was re-encoded meant
  15 IDs each held more than one beetle. The beetle example now uses a documented derived identifier (Block, Family
  and ID): 637 beetles instead of 619, with no original value changed.

## Smaller corrections
* The standardisation option no longer claims "AIC unchanged": that is untrue with uncorrelated random slopes. A note
  explains the exception. The README's "Models 1-8" now reads 1-10.

## Testing and validation
* Combinations, not only components: a pairwise design in which every pair of settings - ageing function, among
  terms, random structure, standardisation, family - appears together (15 cases for all 100 pairs, instead of 270).
  Each case fits all ten models and checks the recorded settings, that every fitted model predicts, that the exported
  script creates every variable its formulas use, and that the engine script asks for the same settings.
* New tests for every fix above: 99 test blocks in 26 files.
* Versioned validation scripts for step 4, the evidence summary and the bootstrap (inst/validation/v0_20). They are
  written but not yet run in R; VALIDATION.md states the expected results and that the results are pending.

# disappR 0.19.7

## Reviewer: the 'consistent' higher-order decomposition was recorded as 'linear'
* Confirmed and fixed. The models were fitted with the option chosen, but fit_model_suite() recorded 'consistent' as
  'linear' in the fit object (it knew only 'same'). Everything that reads that record then described a different
  model: the exported disappR script re-fitted the linear version (no squared lifespan terms in Models 2, 4 and 6-10,
  no mean of age squared in Models 3 and 5), silently reproducing different AICs and estimates; and the provenance in
  the reproducibility bundle recorded 'linear'. The fit now records the option it used.
* The same origin - code written when 'same' was the only higher-order option - caused four further faults, all
  fixed:
  - extra terms added to a model (and their preview) used linear lifespan and mean-age terms under 'consistent',
    giving a model inconsistent with the option chosen;
  - the model descriptions showed consistent models as linear, and described 'same' models with the consistent
    mean-age terms; each option is now described with the terms it actually uses;
  - the settings summary on the Modelling tab labelled 'consistent' as linear;
  - the numerical-stability warning (shown only with standardisation off) did not count 'consistent'.
* A related fault found while checking the export, in the 'same' option: Models 3 and 5 use powers of mean age,
  which neither the app's predictions nor the exported lme4 script created. Inside the app those models could not be
  predicted and silently dropped out of the prediction plot and the model-term interpretation; the exported script
  stopped with "object 'mean_f1_2' not found". Both now create them. The standalone lme4 script also creates the
  squared lifespan terms for 'consistent', as it did for 'same'.
* test-among.R: the fit records each option; both export styles reproduce the option fitted; Models 3 and 5 predict
  under 'same'; extra terms and model descriptions follow the option.

# disappR 0.19.6

## Reviewer: unguarded log ratio in the model-term interpretation
* interpret_model_terms() describes each lifespan term by contrasting predictions for individuals with a high
  and a low value of the proxy. For count and binomial models it used log(hi / lo) with no guard. Predictions
  are on the response scale, so they are positive in principle, and no error could occur - but one can round to
  exactly zero (an extreme extrapolation, or a degenerate zero-inflation part). A zero numerator then printed as
  "x0 (100% lower)", and the description of how the effect changes with age was silently dropped.
* A second fault in the same place: the prediction function drops any age whose prediction failed, but the two
  sides were compared by position. If one side lost an age, R silently recycled the shorter vector and compared
  different ages - for example the long-lived prediction at age 9 against the short-lived one at age 5, a ratio of
  1.125 instead of 1.5.
* Both fixed in a new, separately tested helper, prediction_contrast(): the two sides are matched on age; a ratio
  is taken only from two positive predictions, and any other age is reported as not estimable; the text says so
  when the change across ages cannot be described.
* Swept for the same pattern elsewhere. Every other log() and division is already guarded: the logarithmic age
  basis shifts ages to at least 1, starting values use only positive traits, the proxy's scale falls back to 1
  when its SD is not positive, and the relativised deviation D excludes ages where the truth is within 5% of zero.
* test-interpretation.R: matching on age, a zero prediction flagged without warnings, Gaussian differences with
  negative predictions, and no shared ages.

# disappR 0.19.5

## Step 4: failures are shown, never left blank
* A report that step 4 crashed and plotted nothing. Tracing the step-4 code found no fault that could be
  confirmed without running R, so this release makes any failure there visible instead of silent:
  - the individual fits and the function comparison capture an error with its message; every step-4 output
    then shows "The individual fits could not be computed. Error: ..." in place of a blank plot, and the message
    is printed to the R console;
  - only the fitting itself is inside the error handler, so the app's own messages (for example, asking for the
    columns to be mapped) still appear as before;
  - a progress bar shows while step 4 fits every individual, so a long fit is not mistaken for a crash;
  - the predictions overlay on the Modelling tab and the step-4 save button handle a failed fit safely.
* Also checked: on the zero-heavy fly fecundity data, log-scale fits succeed for 99% of individuals with a linear
  function, 66% with a quadratic and 87% with a cubic; fits fail where an individual's counts fall to zero and
  the log-scale curve diverges. Choosing 'Raw trait' as the scale of the individual fits includes them.

# disappR 0.19.4

## Sampling grid: missingness only between the first observation and the ALR
* Missed occasions are now counted only inside each individual's window, from its first observation of the trait
  to its ALR. The first observation is observed by definition, and nothing after the ALR is counted, with or
  without a known lifespan. With 'Age at first trait expression' the window opens at the chosen age instead.
* Why: in the Alpine swift data, AFR is mapped to age at first reproduction, which for 28 birds came before their
  first laying-date record, so occasions before the first observation were shown as missed; and lifespan is known,
  so for 61 birds the rows ran past the ALR to death, counting those occasions as missed as well.
* The death marker is gone from the grid; with known lifespan, how far ALR falls short of lifespan is summarised by
  r(ALR, LS) in the proxy guidance instead. The legend reads "Missed (between the first observation and the ALR)"
  and "Outside the window (before the first observation or after the ALR)".
* Checked against all 13 bundled datasets (the fly data under both start options) and nine simulated designs
  (27 datasets), with rules that every individual must satisfy - including that the first observation is never
  missed and that nothing after the ALR is expected: zero violations.
* The summary text, the grid and start-option info panels and the note under the start setting all state the
  same definition. test-grid.R gains three tests: a capture without a trait value before the first observation
  is outside the window; with known lifespan nothing after the ALR is counted; the age-at-first-expression option
  still counts from the chosen age.

## Checked: fitting the ageing function through f1 and f2
* Models use f1 = (age - mean) / SD with f2 = f1^2 and f3 = f1^3, and standardised lifespan, so Model 4 is
  trait ~ (f1 + f2) * ALR. Fitted side by side with trait ~ (age + I(age^2)) * ALR on simulated data, both gave
  identical maximum-likelihood AIC and identical predicted trajectories, with a random intercept and with
  correlated random slopes. The only difference is the scale and meaning of the coefficients: lower-order terms are
  effects at the mean age and mean lifespan rather than at age 0 and lifespan 0. Uncorrelated random slopes are the
  one structure that is not invariant (2.7 AIC apart in the test), because assuming zero correlation depends on
  where age = 0 lies; the app centres age, so the assumption applies at the mean age. No change needed.

# disappR 0.19.3

## Sampling grid: fixed for field data
* Two faults, both introduced in 0.18.2 and visible in the Clouded Apollo data, where 715 of 3,888 captures have
  no body mass:
  - the ALR marker sat on each individual's last capture WITH a trait value, not on its ALR - the last recorded
    age, with or without a trait value, which is the ALR the models use. For 107 butterflies caught again after
    their last weighing, the captures after the marker showed as missed. The marker now sits on the models' ALR;
  - individuals never measured for the trait (125 butterflies) were drawn as rows of missed cells with no ALR
    marker. They enter no model, so they are now left out of the grid and its statistics, and counted in a note
    under the grid.
* Ordering by ALR or AFR uses the values the models use (for simulated data, taken after missingness, as before).
  Individuals with no ALR value are placed at the top.
* A mapped ALR later than the last record (seen alive, not measured) now keeps those occasions expected, and the
  window ends at the ALR when lifespan is unknown.
* The note under the grid reports individuals whose recorded ages run past their ALR value - an inconsistency in
  the data as supplied (14 females in the great tit data) - rather than leaving it unexplained.
* Checked against every bundled dataset (13) and nine simulated sampling designs (27 datasets), with a rule set every
  individual must satisfy: each measured record shown as observed; exactly one ALR marker, at the models' ALR;
  nothing expected after the ALR without known lifespan; with known lifespan, only missed occasions between ALR and
  death and nothing after death; nothing expected before the window opens; no never-measured individual drawn.
  Zero violations. The exported R script orders the grid the same way.
* test-grid.R pins the three behaviours with a small dataset built to trigger each.

# disappR 0.19.2

## Evidence summary
* Terminal effects are no longer an evidence line: the lines run from the visual pattern to the permutation test,
  followed by observation bias when missingness results are saved. Removed with it: its effect on the level, its
  explanation, its entry in the list of checks to complete, the record saved with the terminal-investment figure,
  and the screening function behind it. The terminal-investment figure itself stays in step 2, and the permutation
  test still names terminal decline as an alternative explanation.
* Re-checked on simulated data: no selection gives "consistent with none"; age-independent and age-dependent
  selection give "strong"; heterogeneity in ageing gives "none"; trait-dependent missingness flags observation bias.
  A terminal decline without selection now reads "moderate", with the ageing function named as the issue: the drop
  at death makes a curved function fit better, which the shape check detects.

## Final audit
* 44 files (engine, app, tests, launcher) free of unbalanced brackets, trailing commas, missing commas, adjacent
  strings and duplicated arguments. Every call to the package's own functions resolves, and every named argument
  matches its function's definition. Every output, input, save button and info panel is wired; every engine file is
  loaded both ways. None of the failure classes met earlier in this project recur, and no removed feature is still
  referenced. Tests: 20 files, 67 test blocks, all calling functions that exist.

# disappR 0.19.1

## Quantile bins by default
* The bin boundaries for the lifespan-group figures now default to quantiles, so each bin holds about the same
  number of individuals; equal width remains available. On simulated lifespans, equal-width bins left the oldest
  group with 12 of 300 individuals (4 bins) or 3 (6 bins), and put 248 of 300 into one bin when most individuals
  die young; quantile bins kept every group between about 35 and 100 individuals.
* The figure, the exported R script and the saved-result description all use the same default.
* The info panel explains the default, and notes that when many individuals share the same lifespan some
  quantile boundaries coincide and fewer bins are formed than requested.

# disappR 0.19.0

## Step 4 fits count traits on the log scale
* Checked: the simulator puts each ageing function on the log scale for count traits, as the count models do, and
  on the raw scale for continuous traits. That is correct and unchanged - a raw-scale form would make every count
  model misspecified, and could give negative expected counts.
* Fixed: step 4 fitted every individual by least squares on the raw trait, whatever the trait type. For a count
  trait it therefore fitted a raw quadratic to an exponentiated one. On simulated count data the reconstruction was
  off by 34.5% on average with no selection, and went negative at age 25; with age-dependent selection and missing
  records, by 8-9%. Count traits are now fitted on the log scale - a Poisson fit for each individual - which gives
  5.4%, 3.0% (random missingness) and 1.5% (missing in old age) on the same data. Continuous traits are fitted as
  before, and remain within 0.1-1.3% of the truth, including with age-dependent selection and missingness.
* On the log scale the two reconstructions differ, so both are drawn: the curve of the mean coefficients (the
  typical individual, which the simulated truth shows) and the mean of the individual curves (the population
  average). The coefficient band is built on the log scale, so it stays positive.
* The functions compared at the individual level are all fitted by Poisson likelihood for counts, so their AICc
  values remain comparable; on the log scale the exponential a*exp(b*age) is the linear function.
* "Scale of the individual fits": Automatic (log for counts, decided from the trait as the suggested error family
  is), Raw trait, or Log.

## A minimum number of records per individual that works
* The step-4 setting for a minimum number of records did nothing: its value was fixed at 1 and ignored. It is now
  a real control. For a cubic on a noisy count trait, individuals with few records dominate the average: fitting
  only individuals with 10 or more records cut the error from 21.7% to 1.9%. Raising it also fits longer-lived
  individuals, which the step's survivor-bias check reports, so it is best kept for higher-order functions on noisy
  traits.

## Tests
* test-individual-scale.R: the log-scale fit recovers a log-quadratic truth and stays positive, beats a raw-scale
  fit on counts, leaves the raw-scale path unchanged, and respects the minimum number of records.

# disappR 0.18.6

## Which copy is running
* The version now shows in the app's header, and the R console prints the version and folder at start-up.
  Unzipping a new build next to an old one makes macOS create "disappR 2", "disappR 3" and so on, so a launcher with
  a fixed path can keep opening an old copy - with no way to tell from inside the app until now.
* inst/scripts/launch_newest.R finds every copy in Downloads, Desktop and Documents, lists them with their versions,
  and opens the newest.

# disappR 0.18.5

## The evidence summary is now findable
* It had its own output but no box: a card inside the saved-results box, rendered only once something had been
  saved, so on a fresh session there was nothing on the page to find. The Summary page is now three titled boxes,
  top to bottom: "Saved results: manage and export" (controls, downloads and the caution about what is counted),
  "Evidence summary: what do the saved results suggest?", and "Saved results (these are exported)".
* The evidence summary is always shown. Before anything is saved it says so and lists where to start.
* If the summary cannot be built, it now shows the error message instead of disappearing silently.
* The box has its own info panel, including which saved results feed which line of evidence.
* The Summary guidance points to the evidence summary; it previously referred to the "overall reading of the
  evidence", which was replaced in 0.18.0.
* A test checks the summary is visible, and says how to start, before anything is saved.

# disappR 0.18.4

Pre-release audit. One defect found and fixed; no other changes.

## Fixed
* The data-integrity check for infinite values looked for a column called `lifespan`, but the standardised data
  holds lifespan as `life`, and it did not check `alr` at all. An infinite value in a mapped lifespan or ALR column
  was therefore never flagged, and would have surfaced later as a model failure naming something else. It now
  checks age, trait, life, alr and entry. A sweep found no other place that confuses the data's `life` with the
  per-individual summary's `lifespan`.

## Audit
* Static: 50 files free of unbalanced brackets, trailing commas, missing commas, adjacent strings and duplicated
  arguments; 388 calls to the package's own functions all resolve; 193 calls with named arguments all match the
  function's definition; every UI output, input, save button and info panel is wired; every engine file is loaded
  both ways; none of the failure classes met earlier in this project recur.
* Statistical, on simulated data with a known answer (25 scenarios, 50 datasets, 150 fits): continuous traits with
  linear, quadratic, cubic, logarithmic and asymptotic-exponential ageing; count traits with the same five shapes;
  no, level-linked, rate-linked and combined selection; complete sampling, random missingness, missingness in old
  age and trait-dependent missingness; small and large individual variation. With real selection, AIC selected the
  correct model in every dataset, by margins of 300 to 6,500 AIC. Where it chose an extra lifespan term without
  selection, the margin was at most 2.7 AIC - a near-tie the app reports as such. The correct model recovered the
  true trajectory within 0.3-1.7% for continuous traits and 1.0-3.3% for counts, except counts with large
  individual variation in curve shape (19.9%, the log-scale effect documented in 0.18.2). Trait-dependent
  missingness without selection produced a spurious lifespan term, as expected; the missingness diagnostics and
  the evidence summary's observation-bias line exist to catch it.
* Tests: 19 files, 61 test blocks; every function and constant they use is defined.

# disappR 0.18.3

## True trajectory after a new simulation
* Checked end to end: the true trajectory is read from the simulated data itself, which is regenerated whenever a
  simulation is applied, and nothing caches it. A new test drives the app's server through Cubic, then Quadratic,
  then a Linear count simulation (shiny::testServer) and checks that the truth - its form, its label and the curve
  itself - follows each one, and that changing a setting without applying it leaves the simulation in use.
* Why it could look stale: with the default shapes the quadratic and cubic truths are both humps (for a
  continuous trait, peaks of 56.5 at age 15 and 60.0 at age 20), and for count traits the simulated form acts on
  the log scale, so a linear count trajectory is a curved exponential decline. Changing a setting on the Data tab
  also has no effect until 'Simulate & use this dataset' is pressed, and the reminder appeared only on that tab.
* The truth line now names its simulation - for example "True (simulated Quadratic)", or "True (simulated Linear
  on the log scale)" for counts - in the average-trajectory plot and the model-predictions plot.
* When the Data tab's simulation settings differ from the simulation in use, a note above both plots says so and
  names the simulation still shown.

## Fixed
* The simulation the app starts with used seed 1 while the Data tab displayed 42 (since 0.16.1, when only the
  displayed default was changed). The start-up settings now use 42, so the opening dataset matches the screen and
  the 'settings changed' reminder no longer appears before anything has been changed. A test checks this.

# disappR 0.18.2

## Cubic count simulations: checked, no mathematical or coding error
Model predictions for cubic count traits can miss the simulated truth even with the correct model. This was
traced to its cause with a series of controlled simulations (Poisson GLMMs fitted by the Laplace approximation,
as lme4 does by default; each result the mean of two datasets unless noted).
* Control - no individual variation in the shape of ageing, no selection: Model 1 recovers the truth within 0.6%
  on average, including the steep late decline, and recovers the random-intercept SD (0.251; true 0.25).
* The ageing basis is rebuilt for prediction from the fitting data's stored centring and scale, as raw powers of
  standardised age (no poly()), and population predictions hold lifespan at the mean across individuals - the
  typical individual the truth describes.
* Cause: the simulator gives each individual its own deviation on every cubic coefficient; the models give each
  individual its own intercept, and optionally its own age slope, but not its own curvature. On a log scale,
  averaging individual curves that bend differently gives more than the typical individual's curve, by up to
  exp(variance / 2). With 'Small' individual differences the correct model stays within 2-3% of the truth; with
  'Large' it overshoots at old ages (1.35 times the truth at age 20, 7 times at age 30, with selection).
* The same settings with a continuous trait - identity link, where averaging individual curves gives exactly the
  typical curve - recover the truth within 1.9% at every age. Quadratic and logarithmic count traits recover it
  within 1.2-1.3%.
* Residual offsets of a few percent in individual datasets came from the sample's mean lifespan differing from the
  population's; against the sample's own typical individual, Model 4 is within 0-2%.
* The deviation note under the model predictions and the predictions info panel now explain this for count traits.

## Sampling grid
* Ordering by ALR or AFR now uses the last or first observed record in the grid itself, after missingness, so the
  order always matches what is drawn, whatever columns were mapped.
* The last observed record is marked on its own. When lifespan is known, each row continues to death, marked
  separately, with the missed occasions in between; previously the single end marker sat at death, so rows sorted
  by observed ALR looked unsorted under missingness. The exported R script draws the grid the same way.
* Both the sampling-grid and proxy-guidance info notes say that ordering uses the observed records.

## Change figure
* The look-for is restored to the wording approved in 0.16.1.

# disappR 0.18.1

## Weighting by sample size
* "Points weighted (log2 N)" tick boxes on the gap figure ("How big is the gap between lifespan groups at each
  age?") and the change figure ("Does the lifespan association change across age?"). Each point is weighted by
  log2 of the individuals behind it - the two lifespan bins at that age, or the two age bins being compared - in
  the trend lines, their confidence bands and the pooled line, and point size shows the weight. Off by default.
* Any base of logarithm gives the same fitted trend (proportional weights give the same least-squares line), so
  the label says log2 and the result is identical to log10.
* Log weights are mild. In simulation, the oldest comparison rested on 71 individuals against 591 for the
  youngest, yet carried two-thirds of its weight. Weighting nudged detection of age-dependent selection from 53%
  to 55% and false detections under no selection from 1.7% to 1.3%. The info panels say so.

## Fixed: how the change figure is read
* The change figure plots the change in the regression slope of trait on lifespan between consecutive age bins.
  Under age-dependent selective disappearance the lifespan coefficient rises steadily with age, so each change is
  about the same positive amount: the changes sit away from zero but do not trend. In simulation, the slope of the
  line through the changes detected rate-linked selection 4% of the time, the same as with none; whether the
  changes sat away from zero detected it 53-55% of the time, against 1-2% with none. The look-for and info panel,
  written in 0.16.1, invited reading the slope. They now say to read where the line sits, not its slope, and that
  a sloping line means the rate of change is itself speeding up or slowing down. The y-axis now reads "Change in
  regression slope from the previous age bin", and the panel has its own info text.

## Other
* The number of bins defaults to half the average number of time steps per individual, within the range the
  data allow. It is reset when a new dataset is loaded, not when other settings change.
* The disappearance-diagnostics box in Advanced diagnoses is removed, with its output.
* When N_common is low, the individual-level comparison now suggests comparing the mean adjusted R-squared
  rather than delta AICc, in both its info panel and its warning.
* test-weights.R: log2 weights, base invariance, pooled sample sizes carried by the bin differences, and weighted
  trends and bands.

# disappR 0.18.0

## Evidence summary
* The Summary page grades the saved evidence. It sits after the saved-results controls and caution, and before
  the list of saved results that is exported, and it opens the exported report.
* It states a finding in neutral, cautious language - for example "the saved evidence suggests that
  longer-lived individuals decline more slowly in body mass with age than shorter-lived individuals" - never
  "outperform", "better" or "worse".
* Each line of evidence is marked as supporting, a caveat, contradicting, or not yet saved: the visual pattern,
  the model comparison, the lifespan term, the ageing shape, random-slope sensitivity, the permutation test,
  terminal effects and observation bias. Only the checks relevant to the finding are shown.
* An overall level - strong, moderate, weak, mixed, consistent with none, or not enough saved evidence. It is
  strong only when every relevant check has been saved and passed; a check not yet saved caps it at moderate;
  figures contradicting the models, a failed random-slope or permutation check, or missing records that depend on
  the trait cap it at weak. A failed check also changes the explanation offered.
* "Complete the following checks for more robust evidence" lists exactly what is still to run, as numbered
  instructions saying where to find each one.
* It reads only what you saved. Each relevant saved result carries a small structured record of its statistics,
  written when it is saved, so the grading never depends on parsing display text.
* Replaces the previous summary box.

## Calibration against known answers
The grader was run on simulated data spanning selection types, ageing shapes, error families, sample sizes and
series lengths. Final results: no selection gave "consistent with none" in 5 of 6 datasets (the sixth "mixed");
age-independent selection gave "strong" in 3 of 4 (the fourth "moderate"); age-dependent selection "strong" in
4 of 4; heterogeneity in ageing with no selection "none" in 2 of 2; a misspecified ageing function "weak", naming
the function, in 2 of 2, and "none" once the right function was fitted; curved ageing with real selection
"strong" in 2 of 2; terminal decline flagged and named in 2 of 2; trait-dependent missingness flagged in 2 of 2,
capping a spurious finding at "weak"; count data with real selection "strong" in 2 of 2; a short, small study with
real selection "weak" or "mixed", never "strong".

Calibration found six flaws, all fixed before release:
* When Models 2 and 4 were both within 2 AIC, the interaction was chosen, so age-independent selection was read as
  age-dependent. The finding now prefers the simpler model unless the Model 2 vs 4 test says otherwise.
* Lifespan-binned trajectories converge with age under purely age-independent selection, because each bin loses
  its own shortest-lived members. Wide age bins also confound age with lifespan. The visual line now uses the
  coefficient of the trait on lifespan at each exact age - which selection on lifespan leaves unbiased - with a
  bootstrap over individuals, since the same individuals contribute at many ages.
* The terminal screen confused the trend selection itself creates with a change at death. It now compares, within
  each individual, the departure of the last record against that of the penultimate record, and requires the
  change to sit at the last record, so smooth curvature does not trigger it.
* The automatic ageing-function check refitted Model 1, which omits lifespan: under age-dependent selection the
  population curve bends even when every individual ages linearly, so a correct function was flagged. It now
  refits the best-supported model. This also changes the caution under the model comparison.
* High observation bias lowered the level by one step instead of capping it at weak as intended.
* When terminal decline and the ageing shape were both flagged, the shape was named; terminal decline, the more
  specific diagnosis, is now named first.

## Tests
* test-evidence.R: nine tests pinning each grading rule on constructed evidence, and checking the visual and
  terminal statistics against simulated data with known answers.

# disappR 0.17.2

## Ageing-function caution
* The caution under the model comparison no longer tells you to refit. It reports that, when Model 1 is fitted
  with each ageing function under the same settings, the function in use has less support than others, says
  that this model-level comparison is more reliable than the individual fits in step 4 (which rest on few
  records per individual), and directs you to the Ageing-function check on the Checks tab to compare
  functional forms for your models.
* The same point, in a line or two, wherever the individual fits drive the choice of function: the suggested
  starting analysis, "Why these settings?", the best-supported shape in step 4, and the info panels of the
  step-4 comparison and the Ageing-function check. When the two disagree, rely on the model-level comparison.

## Saved results
* Saving exactly the same analysis again no longer adds a second copy. A save is a duplicate only if its content
  and every analysis setting in force match an existing one - button clicks and navigation are ignored, and a
  setting that changes only the figure still makes a new result. A message says when a save was skipped.
* Trait against lifespan no longer saves the per-bin coefficient table, and the model predictions no longer
  save the deviation-from-truth table.
* The model comparison reports its model settings - among-individual terms, standardisation, zero-inflation and
  the models compared - instead of software, platform and data provenance, and no longer ends with the
  statement about automated rules.

# disappR 0.17.1

## Permutation test: results you can read
* The result now says in plain language what happened and what it means. It compares the model's advantage over
  Model 1 with the real lifespans against its advantage when lifespan is shuffled between individuals, and
  gives one of four readings:
  - drops when lifespan is shuffled: the advantage depends on which individual had which lifespan, as
    selective disappearance predicts. Trust it, provided the ageing function is right and there is no terminal
    decline - both named as the explanations still to rule out;
  - shrinks, but not clearly: treat the lifespan effect as uncertain;
  - does not drop: the advantage does not depend on real lifespans, so it is not evidence of selection. For an
    interaction model the likely cause is heterogeneity in ageing, and the reading says to fit random slopes;
  - no advantage to begin with: Model 1 is the safer reading.
  Each reading answers "What this suggests", "Should you trust the model?" and "Other explanations to rule out".
  The same reading is used in the saved result, the exported report and the summary box, and the saved result
  now includes the null-distribution figure.
* Checked on simulated data where the answer is known: no selection gave "no advantage" in 3 of 3; level- and
  rate-linked selection gave "drops" in 3 of 3 each; heterogeneity in ageing with no selection never gave
  "drops" - in one dataset where Model 4 beat Model 1 by 17 AIC, the reading was "not clearly".

## Fixed
* Models 3 and 5 could be chosen for the permutation test but cannot be tested by it: their among-individual term
  is each individual's mean age, computed from its own records, which shuffling lifespan does not change. Every
  shuffle reproduced the observed fit, so the test returned p = 1 even with genuine selective disappearance in
  the data - the opposite of the truth. They are no longer offered, the function refuses them with the reason,
  and the info panel explains it.
* The info panel no longer says models using known lifespan cannot be tested; lifespan is shuffled like ALR.

## Tests
* test-effects.R adds checks that Models 3 and 5 are refused and that each of the four readings is returned for
  the right pattern, with the right alternatives named.

# disappR 0.17.0

## Summary tab
* One summary box, folded by default. Opened, it states the data analysed (for simulated data, exactly what was
  simulated, once), which saved results are consistent with which reading, where they disagree and the likely
  reasons, the best-supported models and the permutation result, and the cautions not already covered. It
  replaces the separate summary and cautions boxes and the overall-reading card.
* Disagreements are specific. When the models point to an age-dependent process that the figures do not show,
  it names the likely causes - heterogeneity in ageing absorbed by the interaction (flagging when random slopes
  were not fitted), a misspecified ageing function, trait-dependent missingness, terminal decline - and says to
  refit with random slopes and run the permutation test. It also explains disagreements the other way, and
  among the visual diagnostics or among saved models.
* A caution box under the saved-result controls: the summary reads every saved result, so remove exploratory or
  unwanted analyses, and every saved model is used, so remove the ones you do not want interpreted.
* The automatic overview is removed.

## Saved results and the report
* Listed and exported in the order they appear in the app: by step, then top to bottom and left to right within
  a step. The order is derived from where each save button sits, so it follows the layout if that changes.
* No date and time on any saved result; the time stays in the selection list only. The data line is given once
  at the top rather than under every result, and the generic "What this means" line is gone throughout.
* Model comparison: fit status is a column of the model-support table; the separate status and random-effect
  variance tables are no longer saved.
* Bin differences and the change in the lifespan coefficient across age bins save their figure only.
* Trait against lifespan no longer repeats its settings.
* Coefficients state the exact fitted equation of that model and which model had the lowest AIC.

## Permutation test
* Uses a fixed random seed of its own, so a rerun gives the same p-value, and restores the session's
  random-number stream afterwards, so nothing else random in the app is shifted.
* The threshold note was wrong: with n permutations the smallest p-value is 1/(n + 1), so p < 0.05 needs at least
  20 permutations, not 19.
* Checked against simulated data where the answer is known. With no selection it was significant in 0 of 5;
  with level-linked and rate-linked selection in every dataset. With strong heterogeneity in ageing and no
  selection, AIC favoured Model 4 in 3 of 4 datasets and the permutation test in none. But it gave a false
  positive in 3 of 3 datasets when the ageing function was misspecified, and in none once the right function
  was fitted; and it was significant in 4 of 4 with a terminal decline, which is a genuine lifespan link. The
  info panel now states both limits and their reason.
* A new test checks the permutation is reproducible and leaves the session's random stream untouched.

## Other
* The sidebar paragraph under the steps is removed.

# disappR 0.16.2

## Fixed (permutation test)
* The permutation test failed on every run with "length = 20 in coercion to logical(1)". A vector holding one
  value per permutation sat on one side of `&&`, which R 4.3 and later reject. The line computed a variable
  that was never used; it is removed. The failure came after every permutation had been fitted, so each run
  cost the full fitting time before erroring.
* Known lifespan was never permuted: the code looked for a column called `lifespan`, but the prepared data
  stores it as `life`. ALR, LS and AFR now move together as one set per individual.
* Asking for 5 permutations ran 10, because the count was silently raised to a minimum of 10. The number you
  enter is now used as given (1 to 500). When it is below 19 the result says so: with n permutations the
  smallest possible p-value is 1/(n + 1), so fewer than 19 cannot reach p < 0.05.

## Hardened
* Two lookups on the Summary page returned every matching row; a repeated comparison would have handed a
  vector to `&&` in the same way. They now take the first match.

## New tests
* test-effects.R runs permutation_test() and selection_effect_sizes() end to end on simulated data, checks the
  p-value lies in (0, 1] and respects the 1/(n + 1) floor, and checks that the permuted null actually differs
  from the observed data. Static checks passed this code; only running it exposes this class of error.

# disappR 0.16.1

Completes the outstanding list from 0.15.5 and 0.16.0.

## Start page and data
* The workflow box now spans the same width as Purpose. Purpose gains the "gaps in ageing research" lead-in
  and a closing paragraph on disappR as a first port of call for longitudinal data.
* The Analyse card and data-source panel describe any longitudinal CSV rather than one row per individual per
  age; repeated measures at the same age are handled by the random intercept.
* The active-simulation box now only summarises the data you generated, with no expectations.

## Missingness
* Detection probability has its own "i": its value depends on whether missingness is counted from first
  record or from first trait expression.
* The expected/observed table beside the missingness-by-age figure is removed, since the figure shows it.
* Question headings are now the same size as other box titles.

## Trajectories
* Step-4 guidance rewritten. The best-supported-shape statement moved from the top of the page into the
  function-comparison box as its lead line; that box loses its subtitle.
* "Which function best describes individual-level data?" gains a caution that a misspecified function can lead
  to incorrect conclusions.
* The "use this ageing function" control is larger, darker and outlined, so it is hard to miss.
* The coefficient summary spells out every fitted function and what each coefficient means. The asymptotic
  exponential is described as fitted: b0 + b1 exp(-z) on standardised age, with the rate fixed by the spread
  of ages rather than estimated.
* The population-function box loses its explanatory paragraph and settings echo.
* The mean-coefficient panel is reworded around what it approximates.

## Visual diagnosis
* The gap figure and the trait-against-lifespan figure each gain a "Show bin legend" toggle; the gap figure
  gains a look-for.
* The change-from-previous-bin column is now a figure: the change in the trait-lifespan coefficient between
  consecutive age bins, with a regression line and confidence band and its own look-for. The full table is
  still saved with the panel.

## Modelling
* Suggested starting analysis shows the choices; "Why these settings?" now explains each one - why random
  slopes matter, what each lifespan proxy measures and how missingness affects it, when the appearance models
  apply - rather than repeating the visible lines.
* The before-fitting box is rewritten as plain sentences.
* Model output: random effects carry their own "i", explaining how to judge whether a random term explains
  anything; scaling constants fold below them; the separate random-effect variance box is removed as a
  duplicate; the internal footnotes are gone. Likelihood-ratio tests fold on the Fit panel.
* DHARMa and performance panels explain what a good diagnostic looks like and what a violation looks like,
  with specific readings of each plot in "More info".

# disappR 0.16.0

## Citations
* Below the two app citations, a fold lists all twelve empirical source papers in full with DOIs, each verified
  against the publisher, PubMed or the paper itself: Allain et al. 2024 (Oikos), Bichet et al. 2022 (J Anim Ecol),
  Bichet et al. 2022 (Ecol Evol), Bouwhuis et al. 2009 (Proc R Soc B), McKenna-Ell et al. 2023 (Biol Lett),
  Moullec et al. 2023 (Front Ecol Evol), Pasztor et al. 2022 (Ecol Evol), Sanghvi et al. 2022 (Evolution),
  Sanghvi et al. 2025 (Am Nat), Szejner-Sigal et al. 2025 (Proc R Soc B), Warner et al. 2016 (PNAS) and Wynn et
  al. 2025 (J Anim Ecol).

## Figures
* Every in-plot title, subtitle and caption is removed, at the shared theme so that no figure is missed. The
  box heading poses the question and the info panel explains the figure.
* Legends are larger and bold throughout.
* The reconstruction from individual fits no longer asks which curve to draw: one curve for functions linear in
  their parameters, where the two reconstructions coincide, and both for the non-linear exponential.

## Summary tab
* Each block leads with one plain overall statement, phrased as what the analysis suggests, with the supporting
  evidence in a "More details" fold. The guidance box now sits above them.

## Other
* Modelling: the guidance box sits above the suggested analysis and adds viewing predictions and output,
  checking model assumptions, and a "Further" step on the permutation test. Diagnostics (DHARMa, performance)
  are back above the error-family and ageing-function checks - a regression from 0.14.0 - with question
  headings. Model tabs are larger and outlined. The AIC look-for is removed.
* Headings: "What ageing trajectory does each model and method predict?", "What does each term in the model
  predict, and what is the estimated age effect?", "Which function best describes individual-level data?".
* Look-fors on the visual tab now distinguish the three readings: no selective process, age-independent, and
  age-dependent. The predictions look-for is rewritten.
* Purpose paragraph extended; simulation seed defaults to 42; the step-4 index label is shortened to fit.

## Fixed during this build
* A reordering of the Checks panel left two rows with no comma between them, which would have stopped the app
  opening. Found and fixed before release. The audit now checks for missing commas between sibling arguments,
  alongside brackets, trailing commas and duplicated arguments.

# disappR 0.15.5

## Fixed (critical)
* Effect sizes produced nothing and the permutation test reported "could not find function". effects.R, added in
  0.13.0, was never loaded: the app sources engine files from a fixed list in global.R, and the installed
  package includes only the files named in DESCRIPTION's Collate field. It was in neither. Both lists now include
  it, and a new test fails if any file in R/ is missing from either, so a new engine file cannot be left out
  silently again. The effect-size failure was invisible because its handler returned NULL on error, so the
  panel simply stayed empty.

## Start page
* "Citation" is now "Cite as". Step cards 2 and 5 reworded; guide-box headings now read only "What to do here".

## Wording and layout
* The Primary/Sensitivity/Supporting/Technical legend text is removed; the colours and classification stay.
  "Terms used in this app" is now "Glossary of terms".
* Guide-box footnotes removed from the data, visual-diagnosis and missingness tabs; the "Start here" divider note
  removed; "Sensitivity checks" renamed "Advanced diagnoses".
* Headings: "Do long- and short-lived individuals differ in their phenotype and how it ages?", "Is there
  terminal investment?", "How likely is an individual to die at a specific age?". Missingness tab retitled to
  ask which among-individual term best accounts for selective processes.
* Info panels rewritten as requested: data source (describes any longitudinal CSV, not one row per individual
  per age; no cautions), workflow (no cautions), bins (states that they stratify the diagnostic plots),
  terminal investment (the resemblance to selective disappearance), missingness by age and proxy agreement
  (no cautions), ageing-function check (clearer caution), permutation test (rewritten around the two reasons a
  model can win), predictions (Model 1 described as the negative control), effect sizes ("accounts for").
* The effect-size and permutation panel is no longer folded, since it is already its own sub-page, and the
  permutation test now defaults to 20 runs. The Modelling description cites "our manuscript (Sanghvi et al.
  2026)".

# disappR 0.15.4

## Fixed (release blocker)
* The app would not open: "formal argument 'collapsible' matched by multiple actual arguments". Two boxes - the
  data preview and the individual coefficient summary - carried collapsible = TRUE twice, left over from the
  0.14.0 evidence-hierarchy change, which appended the argument to boxes that already had it. Both are fixed.

## How this is now caught
* parse() cannot detect this class of error: a repeated argument, an argument a function does not accept, or an
  undefined helper only fails when the call is evaluated. The release gate gains step 0b, which builds the
  whole user interface, and a new test (test-ui-builds.R) does the same and also checks every box() for
  repeated arguments.
* Swept afterwards: 10,217 function calls across the package, none with a duplicated named argument; every
  box, tabBox, tabPanel, radioButtons, checkboxInput, selectInput, numericInput, actionButton and plotOutput
  call checked against the arguments those functions accept, none unrecognised.

# disappR 0.15.3

## Among-individual terms: a consistent higher-order option
* The control is renamed "Higher-order among term" and now offers three settings, with an info panel saying
  when each applies. It remains in Advanced options, and the default is unchanged.
* "Linear (default, as in the manuscript)" - mean age, ALR, LS and AFR each enter once. This matches the
  manuscript's Models 3 and 5 exactly and is what almost every analysis should use.
* "Higher order, consistent decomposition" - NEW. The centring models take the individual mean of age^2
  (mean_f2), which pairs exactly with the within-individual term delta_f2 = age^2 - mean(age^2), so among plus
  within reconstructs age^2 term for term. mean_f2 was computed but unreachable from any formula before this.
* "Higher order, powers of the mean" - the former "Same polynomial order", unchanged in behaviour. It squares
  the mean age, which mirrors ALR + ALR^2 and is fine for the proxy models, but in the centring models it does
  not pair with delta_f2: the two differ by Var_i(age).

## What the change does and does not affect
Verified by simulation: delta terms sum to zero within every individual, so their cross-product with any
among-individual term is exactly zero (correlations of order 1e-17). The within-individual ageing estimates are
therefore identical under all three settings - spread across options 2e-16 for the linear term and 1e-15 for the
quadratic. The setting changes the among-individual coefficients and the model fit, not the estimated ageing
trajectory. Across 15 runs spanning no, level-linked and rate-linked disappearance, the consistent decomposition
had the lowest AIC every time.

# disappR 0.15.2

## Fixed
* README said 0.9.13, sixteen releases out of date. It now matches DESCRIPTION.
* methods::is() was called at three sites while `methods` appeared in neither Imports nor Suggests, which
  R CMD check flags. Replaced with base `inherits()`, which does the same job here and adds no dependency.
* The golden regression test wrote its reference on first run and then skipped, so a shipped archive without a
  committed reference certified whatever the code happened to do on the user's machine. It now writes the
  reference and FAILS, with a message saying to inspect and commit it. The header explains why the reference
  is not shipped: it has to come from the same R and package versions the maintainer tests against.

## Renamed
* "Population-level trajectory" was inaccurate: random effects are excluded, so the curve is conditional on a
  zero random effect rather than a population mean, and for count families it omits the marginal correction.
  The figure keeps its question heading and now carries "Model predictions trajectory" as its technical name,
  in the box, the info panel, the saved-result label and the exported script comments.

# disappR 0.15.1

Info panels rewritten as a plain statement plus a "More info" fold. This replaces the mechanical split
attempted earlier: the visible text is newly written, not the first half of the old text.

* Each panel now states what the output is, how to read it, and the cautions that matter - in plain technical
  language, a few sentences each. "More info" holds the full original text: mechanics, formulae, edge cases and
  the detailed caveats. Nothing is deleted.
* Cautions stay visible. The false positives this app exists to guard against - omitted random slopes,
  trait-dependent missingness, terminal decline, misspecified ageing functions - are stated in the visible text
  with the reason, and explained technically in the fold.
* 30 of the 48 panels are rewritten, covering every panel a user meets in the main workflow: the visual
  diagnostics, the sensitivity figures, missingness and proxies, individual and population trajectories, model
  settings, model support, predictions, the permutation test, effect sizes, decomposition, reconstruction and
  the family and ageing-function checks. In these, visible text is about 7,900 characters against 18,500 folded.
* info_entry() gains a fifth field for the technical text; panels without one render exactly as before.

# disappR 0.15.0

## The result, stated once
* The Summary tab now opens with "Summary of results based on the analysis workflow": whether the saved
  diagnostics agree and on what, the best-supported model and the set within 2 AIC, the R2 of its fixed
  effects, the share of expected occasions missing, the permutation-test outcome if you ran one, and a flag
  when an interaction model is supported without individual ageing slopes. Every line names the evidence it
  rests on, and the block says plainly that these are suggestions to weigh, not conclusions.
* Below it, "Things to be cautious about" collects what could undermine the reading: evidence pointing
  different ways, saved results that carried their own caution, interaction models without random slopes, low
  R2, heavy missingness, two models within 2 AIC, few individuals, a permutation test that did not separate
  the result from the null, and too few saved results to compose a summary from.
* Both are built only from what you saved and what you fitted. Nothing is interpreted for you earlier in the
  workflow.

## Model fit reported alongside AIC
* The model comparison table now carries Nakagawa R2 for every model: "R2 fixed" (fixed effects alone) and
  "R2 total" (fixed plus random). A model can win on AIC while explaining very little, and now you can see it.

## Interface
* The progress strip is removed.
* One semantic palette. Red now means a genuine caution and nothing else; primary boxes carry the single dark
  header, and supporting and sensitivity boxes use quiet neutrals so that an optional diagnostic can no longer
  look more important than a result.
* Eighteen box headings are now questions: "Is age at last record a reliable stand-in for lifespan?", "What
  shape best describes ageing?", "Does the lifespan association change across age?", "Do long- and short-lived
  individuals follow different trajectories?", "Do survivors differ from those that disappear?", "Which models
  do the data support?", and so on.

Still to come: splitting each info panel into a short default and a "More info" fold, and culling the
overlapping help mechanisms.

# disappR 0.14.1

* Every figure is larger. Heights rise by 30-40% across the app: the trajectory figure from 430 to 560, model
  predictions 430 to 580, the population ageing function 460 to 600, the bin differences and trait-against-
  lifespan figures 400 to 520, the reconstruction 380 to 520, terminal trajectories 360 to 480, the hazard 340
  to 460, and the rest in proportion. The two auto-sized figures grow with them: the sampling heatmap now
  spans 600 to 1700 pixels rather than 460 to 1400, and each row of individual fits gets 210 pixels rather
  than 170.
* No other changes.

# disappR 0.14.0

Reorganised so that the page shows what matters first. No analysis, model, default or statistic changes.

## Fixed
* Six trailing commas before a closing bracket, which R reads as an empty argument. One of these (in the
  visual-diagnosis layout) predated this release.

## A question at the top of every page
* Each numbered tab now opens with the question it answers: what data do I have and are the columns mapped
  correctly; is there visual evidence for selective disappearance and of what type; is my data complete and how
  does that affect which term I use; how do traits change at the individual level; what model best describes the
  data and why; what did I find and do the pieces agree.
* The visual-diagnosis text now starts from the biological question - do older individuals differ because
  individuals change, or because different individuals remain - and names the terms second.
* New collapsed glossary on the Data tab: selective disappearance and appearance, AFR, ALR, LS, the
  among-individual term, within-individual change.

## Hierarchy of evidence
* Four levels, with a key shown once: Primary (read now), Sensitivity (could overturn it), Supporting
  (corroboration), Technical (diagnostics and code). Sensitivity sits above Supporting because omitted random
  slopes and misspecified ageing functions are the documented routes to a false positive.
* Visual diagnosis: the trajectory and trait-against-lifespan figures are Primary; the bin differences and the
  coefficient table are Supporting; terminal decline, selection differentials and the disappearance hazard are
  Sensitivity, collapsed, under a divider that says they can overturn the conclusion above.
* Missingness: the sampling grid and the proxy-agreement figure are Primary; missingness against other variables
  is Sensitivity.
* Trajectories: the function-support box is Primary, everything else Supporting or Technical.

## Answer first
* The trajectories tab opens with the ageing shape that has most support, the functions within 2 AICc, and a
  note when that rests on a thin common subset.
* The suggested starting analysis gains a "Why these settings?" disclosure giving the evidence behind each
  choice, so the machinery is available without being in the way.

## Modelling tab split
* Five panels instead of one long page: Fit (settings, support, predictions), Model output (coefficients,
  variance components), Checks (error family, ageing function, DHARMa, performance), Advanced (effect sizes,
  permutation test) and Code. Output identifiers are unchanged.

# disappR 0.13.0

New advanced box on the Modelling tab, below the model predictions and above the diagnostics, collapsed by
default. It holds two things AIC cannot give you.

## Permutation test against a null of no selection
* Permutes the lifespan proxy across individuals and refits, leaving every record, age and per-individual
  sample size where it was. Only the link between an individual's proxy value and its own records is broken,
  so the null keeps the ageing pattern, the repeated-measures structure and the missingness intact.
* Reports the observed AIC advantage, the 95th percentile of the permuted null, a one-sided p-value, and a
  histogram of the null with the observed value marked. Model and number of permutations are selectable.
* Validated against simulated data where the answer is known: real age-dependent selection gives observed
  1155.8 against a null 95th percentile of 35.8 (p = 0.024); no selection with no slope variation gives
  p = 0.902; and the case AIC gets wrong - no selection at all, but among-individual slope variation with
  random slopes omitted - gives an observed advantage of 16.9 against a null 95th percentile of 23.1
  (p = 0.098), so the test correctly refuses to call it selection.

## Effect sizes
* Three quantities on the trait's own scale, with 95% intervals: how far the ageing slope moves when the
  proxy is added (absolute and as a percentage of the uncorrected slope), how much trait one extra unit of
  ALR buys, and the difference in ageing rate between long- and short-lived individuals, taken as the
  ALR x age coefficient over the 10th-to-90th-percentile spread of ALR.
* Both carry their own info panel explaining what the quantity is, why it matters and where it misleads, and
  both save to the summary.

New file R/effects.R holds selection_effect_sizes() and permutation_test(). Neither is exported, matching the
package convention that only the disappr_* API and run_app() are.

# disappR 0.12.12

Completes the outstanding items from the previous two rounds.

## Text
* Bin settings now suggest how many bins to use: enough to resolve finer differences, not so many that a bin
  holds too few individuals, with time steps divided by three as a starting point.
* The AFR/ALR panel now states that missingness alters both proxies - a missed first occasion pushes AFR later,
  a missed final occasion pulls ALR earlier - so neither is observed cleanly under incomplete sampling.
* The before-fitting box now says the formulae listed there are general and do not expand the chosen ageing
  function, and points to each model's own "i" for the exact formula.
* The function-comparison table carries a bold disclaimer that the comparison is inaccurate with small samples,
  few sampling time steps or high missingness.
* The trait-against-lifespan footnote now reads "across age bins" rather than "across age panels".

## New panels and controls
* Separate "i" panels for Decomposition and for Reconstruction from individual fits, each beside its own option
  on the prediction figure. Both give the purpose, the arithmetic and the biological reading: the decomposition
  chains mean within-individual change between adjacent occasions over individuals recorded at both, assuming no
  functional form; the reconstruction averages each individual's fitted function across the whole age range and
  so assumes individuals are immortal.
* Bin legend can be switched off on the trajectory figure, which otherwise swamps the plot when bins are many.
* "Use this ageing function for the mixed models" now sits below the data-support readout in a bordered box with
  a dark, full-width button, so it is hard to miss.

## Model definitions
* Each model's purpose ("Negative control: no selective disappearance term", "Age-dependent selective
  disappearance through ALR x ageing terms", and so on) now appears as the first attribute in that model's own
  "i" panel, beside the lifespan proxy and the two selection attributes. The duplicate column is removed from
  the definitions table, which was itself removed in 0.12.10.

# disappR 0.12.11

* The individual-level function comparison now flags an unreliable AICc comparison. When the set of individuals
  for which every function is estimable (N_common) falls 20% or more below the total considered, a red-flagged
  caution appears under the table giving the percentage and both counts, and stating that the comparison uses
  only the common set and should not be used to choose an ageing function.
* The matching info panel explains why: functions needing more records can only be fitted to the individuals
  that have enough data, so their AICc rests on an easier subset; the dAICc column therefore uses only the
  common set, and if that is substantially smaller than the sample, the comparison is inaccurate.

Threshold behaviour, checked against simulated data with 300 individuals: mean 15 records per individual gives a
1.7% drop (no warning), mean 8 gives 12.3% (no warning), mean 5 gives 37.7% (warning), mean 3 gives 91.7%
(warning) - so the flag fires in the short-lived systems where the comparison actually misleads.

# disappR 0.12.10

## Fixed
* Simulated body mass can no longer go negative; it is floored at a small positive fraction of the mean.
* An info panel with nothing under Cautions no longer shows an empty "Cautions" heading.

## Checked, not a bug
* Fitting AFR and ALR together in different age-dependent and age-independent forms was tested against
  simulated truth across five combinations. Model 9 (ALR x age + AFR) wins when disappearance is age-dependent
  and appearance is not; Model 8 wins only when appearance acts on the ageing rate as well; Model 2 and Model 4
  win in the level-only and rate cases. The formulas are correct.
* The weak visual signal when a large age-independent effect is present is a scaling matter, not a maths error:
  with a strong level effect the bins separate widely at every age, so the extra divergence is a smaller share
  of the gap (slope of the gap falls from 0.147 to 0.062) while remaining real. The consecutive-bin difference
  figure beside it quantifies exactly this.

## Interface
* "Settings for the lifespan-group figures" is now "Set the number of bins and the selection term of interest".
* The disappearance-diagnostics blurb under Additional checks is removed.
* Trajectory interpretation now opens with the manuscript's manifestations of selective disappearance.
* Individual-level function comparison warns that with few sampling intervals the best-fitting function is often
  not the generating equation, so the AIC values are suggestive rather than true.
* Accuracy against the simulated truth now defines what "truth" means: the average within-individual trajectory,
  latent to the researcher but inferable from cohort-level data.
* The ageing-function check is rewritten as a single paragraph with the actual term expansions.
* Suggested starting analysis: the "Recommended starting point" badge and the "does not fit anything" footnote
  are gone, replaced by a caution that the recommendation rests on function selection, missingness and data
  structure; the error-family line now says to choose your own if it looks wrong.
* The Modelling step description now lists what to choose first and what to explore afterwards.
* The sensitivity warning ends with the check to run rather than the simulation percentages.
* "Model predictions as lines, not functional smooths".
* Model definitions box removed, and Model output now spans the row; the predicted-trait-at-representative-ages
  table is removed, since the prediction figure already shows it.
* Each empirical example note drops the shared closing paragraph; one general note now sits below the example
  selector explaining that defaults match the published specification, that the app generally reproduces the
  published results, and that the data are here for exploring alternatives rather than for reanalysis.

# disappR 0.12.9

## Fixed
* The `is.character(txt) is not TRUE` error is explained and guarded. `validate()` and `need()` were called
  unqualified, so whenever jsonlite was attached ahead of shiny on the search path, `jsonlite::validate(txt)`
  answered instead of `shiny::validate()`, and its `stopifnot(is.character(txt))` failed on a shiny validation
  object. Every call is now `shiny::validate(shiny::need(...))` (58 of them), which is also why the error struck
  data loading, upload, the variable distribution and the integrity checks alike: they all route through
  `validate()`.
* Removed the stray empty "Optional packages" panel beside the visual workflow, and the now-unused
  `package_status` output behind it.

## Interface
* Visual diagnosis is now paired side by side: trajectories within bins beside the consecutive-bin differences,
  and trait against lifespan beside its regression-coefficient table.
* Trajectory interpretation reads "consistent with and indicative of"; its cautions now describe thin bins at
  older lifespans, the risk of false negatives, and that non-linear trajectories are not plotted. The
  bins-are-formed footnote is gone.
* Terminal trajectories: distinct investment in the phenotype as residual reproductive value changes, "might
  indicate" rather than "means", the note-below-the-figure sentence and the cautions removed.
* Selection differentials: interpretation rewritten around comparing survivors with those that die, whether the
  interval overlaps zero, and what a change with age indicates; cautions removed.
* Disappearance hazard: the dashed line is now explained as the overall age-independent hazard, with what points
  above and below it mean and what a rising or falling series implies for where selection acts.
* Missingness against other variables: interpretation rewritten around judging whether missingness looks random;
  cautions reduced to the point that these are associations, not quantitative tests.
* Individual fit settings now open with "How does each individual age?" and explain the reconstruction.
* Individual-level function comparison cautions rewritten around biological plausibility, data requirements, the
  same-functional-form assumption, and AIC as suggestion rather than test.
* Mean-coefficient panel opens by saying it averages individual trajectories, states that the reconstruction
  assumes immortality and extends functions beyond death, and points to the model-prediction comparison.
* Population-level ageing function now names the fitting method and drops the Model 1 reference.
* Model settings open with "Start simple", and the cautions explain that interaction models can fit better
  without age-dependent selection when random slopes are absent.
* DHARMa residual checks and the performance diagnostics now sit above the error-family and ageing-function
  checks.

No change to any model, formula, default or statistic.

# disappR 0.12.7

Interface and wording. No change to any model, formula, default or statistic.

## Data tab
* Cautions removed from the data source, map columns, subset and integrity panels.
* Map columns no longer warns about continuous vs categorical placement; its footnote is now just the
  definitions of AFR, ALR, LS, nesting, additional random intercepts and censoring.
* The simulator no longer pre-empts the result: the "Model 1 is expected...", "Model 1 can prefer...",
  "These expectations were verified..." and complete-sampling lines are gone, as is the random-effect note,
  which now belongs on the Modelling tab.

## Visual diagnosis
* The step description now covers individuals entering late as well as dying early, and points to the AFR
  grouping for selective appearance. Interpretation reads "consistent with and indicative of".
* "The single most informative figure in the app" removed; the settings note now reads "These settings apply
  to both figures below"; the redundant "Useful, but not required before modelling" note is gone.
* The standalone "What you should see" box is removed.

## Missingness and proxies
* The step description now explains that the among-individual term carries the correction for selective
  processes: with complete sampling and no AFR variation, mean age and LS are perfect proxies of lifespan, but
  because mean age follows from sampling design and missingness, it becomes a worse proxy than ALR when AFR
  varies or data go missing.
* The proxy figure is retitled "How well do different measures of among-individual and selective processes
  correlate?".

## Individual and population trajectories
* Retitled "Which ageing function has most support at the individual level?".

## Modelling
* The step description now states what the tab does: fit mixed-effects models following Models 1-6 of the
  manuscript plus models for selective appearance, set the error family, random terms and covariates, and
  compare AICs, predicted trajectories, model output and diagnostics.
* New "Random slopes of age" info panel: without random slopes the interaction models can fit better simply
  because the interaction terms recover among-individual heterogeneity in ageing, so refit with slopes before
  reading an age-dependent result.
* Model support cautions now cover trait-dependent missingness and omitted random slopes as routes to a false
  positive, and say plainly not to use AIC as evidence for a process. The dropped-rows note explains it applies
  to AIC comparison and only bites if a covariate is added to one model and not the others.
* Centring models are described as assuming ageing is absolute and depends on chronological age, so the
  among-individual term is transformed before centring (Fay et al. 2022).
* The fitting packages are named: lme4, glmmTMB, lmerTest, DHARMa and performance.
* "Population-level ageing trajectories" is now "Comparison of model predictions for age effects";
  "Coefficients" is now "Model output"; the model-definitions column "Question" is now "Assumption about the
  data-generating process" (renamed at display, so the stored table is unchanged).

# disappR 0.12.6

## Fixed (critical)
* Nothing would load in 0.12.2 to 0.12.5. The importer rewritten in 0.12.2 declared the decoded file text as
  "bytes" (Encoding(txt) <- "bytes"). R refuses to pass byte-declared strings to strsplit() and most other
  string functions, and iconv() returns that declaration unchanged when the input is already valid UTF-8 -
  which every bundled example and almost every upload is. The result was an error on loading any example or
  CSV, and the same error wherever the data were then used: the preview, the variable distributions and the
  integrity checks. The declaration is removed, the decoded text is returned as a plain character string, and
  a direct-file read is now used as a fallback whenever the decoded text is unusable, so a fault in decoding
  can never again stop a readable file from loading.

## Interface
* The Purpose page now opens by separating the two processes from the data pattern: ecologists want the latent
  average within-individual ageing pattern but only have population-level data on individuals followed over
  time, those data are usually incomplete, and when phenotypes are associated with entry into or removal from
  the sample - selective appearance and selective disappearance - the analyses can be biased. The four points
  that follow are shortened.
* The Quick start box is removed: its two buttons duplicated the Learn and Explore cards. The optional-package
  readout it contained is kept.
* Collapse controls (the + and - in box headers, and the Advanced options disclosure) are now bordered, so they
  are visible against the header.
* Learn now describes generating a variety of datasets with control over data structure, missingness, the
  organism's biology, sample size, and the type and magnitude of the selective processes; the line about being
  a first-time user is removed. Explore now says it opens one of thirteen published datasets and sets the
  default options that most closely reproduce the published analysis.

No change to any model, formula, default or statistic.

# disappR 0.12.5

Three new empirical examples, taking the bundled set from ten to thirteen. No change to any model, formula,
default or statistic for the existing examples, which reproduce their previous results exactly.

* **Alpine marmot immunity** (Bichet et al. 2022, Ecology and Evolution). 173 records from 53 marmots, with age at
  last observation supplied by the authors. Opens on lymphocyte counts with a Poisson family, a linear ageing
  function and Models 1, 2 and 7 - the published specification, which replaces the average/delta age split with
  actual age, age at access to dominance and age at last observation. Fitting it here returns an ALR coefficient
  of +0.35 against the published +0.27, and +0.02 for AFR against their -0.01. Four further traits (neutrophils,
  monocytes, eosinophils, log leukocyte concentration) are in the file.
* **Alpine swift reproduction** (Moullec et al. 2023, Frontiers in Ecology and Evolution). 2,087 bird-years from
  545 swifts, with age at first reproduction and lifespan supplied. Opens on laying date, Gaussian, quadratic,
  with Models 1, 2, 4, 6, 7 and 8. The note states plainly that the quadratic ageing function is the closest this
  app can come to the threshold models the paper fitted, and that their reported onset age has no equivalent here.
  Three count traits (clutch size, brood size at hatching and at fledging) are also in the file.
* **Clouded Apollo butterfly body size** (Pasztor et al. 2022, Ecology and Evolution). 3,888 records from 1,315
  butterflies across seven flight periods, merged from the three archived files. Opens on log body mass, Gaussian,
  quadratic, with wing length, sex and day of first capture as covariates. Every analysis column is DERIVED from
  measurement dates rather than archived; the app note and data/PROVENANCE.md both say so and give each definition.
  Fitting the paper's best-supported model reproduces all ten coefficients to within about one published standard
  error. Age is counted from first capture, so every individual starts at age 0 and the appearance models are
  correctly unavailable.
* data/PROVENANCE.md documents all three, including a table of every derived butterfly column and the
  reconstruction checks against the paper's reported sample sizes.
* Removed a stale PROVENANCE row for a beetle male-weight file that is not shipped.

# disappR 0.12.4

* Embedded NUL bytes are removed before a file is read, and the import note says how many were dropped. The
  0.12.2 rewrite muffled every read warning, including R's "embedded nul", so a file could be truncated with no
  message; a NUL now leaves the data identical to the clean file instead.
* Saved results carry an explicit verdict where one exists, instead of relying on keyword matching. The model
  comparison and the population trajectories take theirs from the supported set (an interaction model supported
  without Model 1 reads as age-dependent; an additive proxy model as age-independent; Model 1 supported as
  none), and the trajectory-by-bin, coefficient-across-age-bin, trait-before-death and disappearance-hazard
  panels take theirs from their own fitted trend lines. Every panel previously classified as "context", so the
  report always read "Insufficient"; it now reaches Concordant, Mixed or Fragile as intended.
* The random-slope sensitivity warning now fires whenever slopes were omitted and an interaction model is in
  the supported set, not only when the data support for slopes is "good", with wording graded by that support.
  It cites the measured effect on the bundled seed-beetle data: a null created by permuting lifespan across
  individuals still favoured Model 4 by about 69 AIC without slopes and by 1 AIC with them.
* New integrity row for infinite values, naming the columns and counts. Nothing is blocked - such a column can
  still be mapped, including as age - but the row explains that infinite values are not measurements, that they
  pull means, ranges, bins and standardisation to infinity, and that a fit will usually fail with a message
  pointing elsewhere.

No change to any model, formula, default or statistic: the ten bundled datasets, the simulation grid and the
complex scenarios reproduce their previous values exactly.

# disappR 0.12.3

* New tests/testthat/test-golden.R: golden-output tests that pin model AICs, coefficients, the decomposition,
  individual metrics and the age basis for a fixed dataset, generated by R on first run and compared thereafter;
  a check that the exported script names every fitted model and that refits reproduce; and a smoke test for the
  package-level plotting helpers. Regenerating the reference is deliberate, not a way to silence a failure.
* No change to any analysis, model, default or statistic.

# disappR 0.12.2

## Fixed
* Release blocker. The ageing-function table built a column with a backtick-quoted name containing a unicode
  escape, the only such name in the package; the name is now set after the data frame is built. A recursive
  parse() of every shipped .R file in R/, inst/ and tests/ is now step 0 of the release gate, so a file that
  does not parse fails immediately instead of reaching a user.
* CSV import is no longer at the mercy of the session locale. The importer now reads the file's bytes, strips a
  UTF-8 byte-order mark, detects UTF-16 and says so plainly, converts from UTF-8, latin1 or windows-1252 to
  UTF-8 itself, and only then parses from text. Previously it passed fileEncoding to read.csv, whose success
  depends on the platform and locale and which leaves invalid bytes in the returned strings, where later calls
  such as trimws() fail. The note now reports when a file was converted from another encoding. All 26
  adversarial fixtures in tests/stress_data now parse, including latin1_semicolon.csv (latin1 with a degree
  sign in a header) and messy_semicolon_decimal_comma.csv (BOM, semicolons and decimal commas), and all ten
  bundled datasets are unchanged.

## Sensitivity checks are now automatic
* The two mechanisms that manufacture selective disappearance are checked as soon as models are fitted and
  shown above the model table, not behind a button. A misspecified ageing function made an interaction model
  win 100% of simulated replicates with no selection (median gap 1,167 AIC); omitted random slopes did so in
  60%. The banner fires when the ageing function in use is not within 2 AIC of the best-supported shape, or
  when the data support individual slopes, slopes were not fitted, and an interaction model is in the supported
  set. On datasets above 40,000 rows the function check is not run automatically and the banner says so.

No change to any model, formula, default, threshold or statistic: the ten bundled datasets, the simulation grid
and the complex scenarios all reproduce their previous values exactly.

# disappR 0.12.1

Language only. No analysis, model, default, threshold or caveat changes, and every statistical file in R/ is
byte-identical to 0.12.0. The ten bundled datasets reproduce their previous results exactly.

* The ten empirical example notes are rewritten tighter, from about 19,900 to 11,900 characters, with every
  fact, caveat and pointer kept. The closing paragraph shared by all ten (defaults follow the published fit,
  are editable, usually reproduce the pattern, may differ through subsetting, unspecified terms, random-effect
  structure or software, and are exploratory rather than exact re-runs) is now a third shorter.
* The six longest "i" panels are condensed: mean-coefficient and mean-function trajectories, population-level
  trajectories, data source and simulation, model settings, trait before death, and model support. Each keeps
  every mechanism, number and warning - including the 1450-AIC terminal-decline result, the Jensen's-inequality
  distinction between the two reconstructions, and the reason models can share a trajectory while differing by
  hundreds of AIC.
* Minor phrase simplification elsewhere ("in order to", "note that") where it left meaning untouched.

# disappR 0.12.0

Guidance layer. This release adds signposting only: no analysis, model, default, formula or statistic changes.
Every file in R/ except ui-helpers.R is byte-identical to 0.11.2, and the ten bundled datasets, the simulation
grid and the complex scenarios reproduce their previous results exactly.

* "What to do here" panel at the top of each numbered tab: the goal of the tab, what to open first, what follows,
  a step counter and Previous/Next links.
* Progress strip under the header showing which of the six steps are done, which you are on, and how many results
  have been saved to the report.
* Three journeys on the start page - Learn (simulated data with a known answer), Explore (a published empirical
  example) and Analyse (upload my data) - each setting the data source and moving to the Data tab.
* "Look for:" lines under the main figures and the model table, saying what to read off them, including the
  reminder to read the supported set rather than the first row.
* "Start here" and "Additional checks" dividers on the visual diagnosis tab, so that six figures do not read as
  six compulsory tasks.
* Advanced controls (among-individual polynomial order, standardisation, invalid Hessians) collapsed behind an
  "Advanced options" disclosure on the Modelling tab. Nothing is removed and no default changes.
* "Suggested starting analysis" panel on the Modelling tab, derived from the trait distribution, the data support
  for random slopes and the mapped columns. It states its reasoning, applies nothing until the button is pressed,
  and fits nothing.
* Plain-language questions above technical panel names ("Which measure of lifespan should I use?", "Which ageing
  function fits best?"), with the terminology kept beneath.
* Consistent status labels (Recommended starting point, Optional sensitivity check, Required, Caution, Not
  available with these data) so recommendations are not mistaken for requirements.

# disappR 0.11.2

* Uploads up to 20 MB: `run_app()` and the app's `global.R` both set `shiny.maxRequestSize` (Shiny's default is 5 MB).
* The extended independent validation is now reproducible from the package: the missing `emu.py` module is
  shipped, and `run_extended.py` regenerates `results_extended.csv` exactly. Every scenario is run over 20 seeds
  and judged on the expected-set win share and the median dAIC behind the winner, rather than on a single run.
  19 of 19 graded scenarios pass; the two intentional false-positive demonstrations (random slopes omitted,
  misspecified ageing function) are labelled DOCUMENTED with their rates and are not graded. The selective-
  appearance scenario now links entry age to trait quality (the previous generator linked it to lifespan, which
  is a different thing); the AFR models recover it in 20 of 20 seeds.

# disappR 0.11.1

## Inference
* Multi-degree-of-freedom effects are now tested jointly. A polynomial interaction is several coefficients, and
  comparing the smallest of their Wald p-values with 0.05 is not a 0.05 test of the interaction (with three
  coefficients the true error rate is roughly 12-14%). The interpretation now uses the nested likelihood-ratio
  test where one exists and a joint Wald chi-square over the whole coefficient block otherwise, reporting the
  statistic, its degrees of freedom and p. A single small component coefficient can no longer override a
  non-significant joint test, which was possible for main effects before.
* The model panel now leads with the supported set (every model within 2 AIC) instead of the lowest-AIC model.
  Where several models are within 2 AIC the text says plainly that the data do not distinguish them.
* New "Ageing-function check" box beside the error-family check: refits one selected model with each ageing
  function on the same rows and reports AIC, dAIC, df and fit status side by side, with a warning when the
  function currently in use is not among the best-supported shapes. Misspecifying the ageing function is the
  largest false-positive mechanism found in testing, so this is a direct check on it.
* The individual-slope support numbers are presented as rules of thumb, not thresholds: nothing is gated on
  them and the text says so.

## Interpretation and reporting
* Every saved result now carries a line saying what it means for selective disappearance and ageing, and a
  coarse verdict code (age-dependent, age-independent, none, context, caution).
* The summary tab and both reports open with an overall reading of the evidence - Concordant, Mixed, Fragile or
  Insufficient - describing whether the saved results agree, with the standing reminder that selective
  disappearance is an observed lifespan-trait association rather than a demonstrated mechanism.
* The trials mapping explains why the number of trials matters: 3/5 and 600/1000 are both 0.6 but carry very
  different information, and without the column every proportion is weighted equally.

## Reproducibility
* Provenance records now capture platform and OS, locale, BLAS/LAPACK, more package versions, the input file
  name and its md5, the number of rows read and the complete column mapping.
* New "Reproducibility bundle (JSON)" download on the summary tab containing the whole record.

## Testing
* New property/invariant test file: standardisation leaves AIC unchanged; row order and ID relabelling are
  no-ops; equivalent nested-ID encodings agree; complete sampling makes the automatic ALR exactly the last
  observed age; the age basis round-trips and mean + delta reconstructs it; a constant age shift leaves model
  comparison unchanged; repeated fits reproduce; the decomposition is invariant to row order; joint Wald tests
  use every coefficient of a block and are never the smallest component p-value.
* Independent Python validation extended to Poisson, binary, proportions with trials, Models 7-10, nested
  random effects, covariates, random slopes and the grid decomposition (inst/validation/extended/): 19 of 21
  single-run checks pass, and both single-seed failures pass on replication with a median gap of 0.0 AIC.
  Recorded there: with individual slope variation unrelated to lifespan and no selection at all, Model 4 wins 5
  of 10 replicates when random slopes are NOT fitted (gap up to 26.6 AIC), and Model 1 wins once they are.

# disappR 0.9.16

* The placeholder maintainer address is replaced: E. R. Ivimey-Cook is listed as maintainer
  (E.Ivimey-Cook@uea.ac.uk), with Krish Sanghvi and E. R. Ivimey-Cook both as authors. The release gate's
  placeholder-email check now passes.

# disappR 0.9.15

* Documentation only: the comment in R/formulas.R describing the among-individual terms still referred to
  mean_f2 (the individual mean of age squared) for the centring models; it now names mean_f1_2, the square of
  the individual mean age, matching the code since 0.9.14. No behaviour changes.

# disappR 0.9.14

## Corrected mathematics
* The "same polynomial order as the ageing function" option now gives the centring models a polynomial of MEAN AGE.
  It previously used mean_f2, the individual mean of age squared, which is not the square of the individual mean age:
  mean(age^2) = mean(age)^2 + the within-individual variance of age. The among-individual terms are now mean_f1,
  mean_f1_2 and mean_f1_3 (powers of the individual mean), mirroring ALR + ALR^2 for the other proxies; the
  within-individual terms are unchanged. The "i" panel states the distinction.
* The decomposition is now defined on the POPULATION sampling grid rather than on each individual's own consecutive
  records. Occasions are the distinct sampled ages (ages within a quarter of a step share an occasion); two occasions
  are linked when they are adjacent and about one sampling step apart; an individual contributes to a link only when
  it was recorded at both of its occasions. An animal seen at ages 1, 2, 5 and 6 on a grid of 1..6 therefore
  contributes to 1->2 and 5->6 and to nothing else, instead of 2->5 counting as a transition. Where the grid breaks
  the curve is returned in separate segments, which are drawn unjoined, and the caution says why.
* The great tit example opened laying date (a date, ranging over April days) with a Poisson family left over from the
  recruits default; it is now Gaussian.

# disappR 0.9.13

* Authors: Krish Sanghvi (maintainer) and E. R. Ivimey-Cook; `citation("disappR")` gives the package and preprint
  references. The maintainer's email is still a placeholder, which the release gate reports.
* Structured fit diagnostics: every model gets convergence, singular fit, Hessian, rank-deficiency and AIC checks read
  from the fitted object itself, shown in the fitting-status table beside the status they imply. The reported
  Valid/Caution/Failed status is unchanged.
* Random-effect checks before fitting: repeated records per individual, support for random slopes, levels and records
  per level of every grouping or random term; cautions appear in the pre-fit summary.
* Predictions first: the coefficients panel opens with the trait each model predicts at representative ages, since
  raw polynomial, standardised or interaction coefficients are hard to read on their own.
* Held-out validation: ten simulated scenarios never used in development, tested against expectations fixed
  beforehand (nine of ten met all of them; see VALIDATION.md), and a protocol and runner for held-out real datasets.
* Documentation: the README is split into five vignettes (getting started, workflow, models and methods, validation and
  replication, development and testing).
* Golden comparisons report new result fields as notes; a missing field still fails.

# disappR 0.9.12

* Unit tests: `tests/testthat/` covers formulas, data preparation, model fitting and its guards, trajectories, the
  decomposition, simulations, provenance, exported code and the R interface; R CMD check runs them.
* The longer test scripts moved to `tests/scripts/` so that R CMD check does not run them; `tests/scripts/run_all.R`
  runs every tier (unit tests first) and `tests/scripts/release_gate.R` the release checks.
* Continuous integration: R CMD check on Linux, Windows and macOS, and the full test tiers, on every push.
* `disappr_example()` and exported scripts for bundled examples find the example data wherever R is running.
* Package metadata for R CMD check: base packages used are declared, codetools and testthat (edition 3) are suggested,
  the licence names its holder, and non-package files are excluded from the build.

# disappR 0.9.11

* server.R is orchestration only: its 18 pure helpers (population predictions, result tables, the analysis signature,
  term-builder vocabulary, proxy plots, model-check drawing, script helpers and interface helpers) moved unchanged
  into the engine files.
* Provenance: every model comparison stores a provenance record - fingerprints of the analysed rows and individuals,
  formulas, family, random effects, zero-inflation, transformations (standardisation constants, age rounding, subset,
  duplicates), settings, warnings and software versions. Saved model results, reports and the exported script include
  it; `disappr_provenance()` returns it.
* Exported script: with the same disappR version installed it reads, prepares and fits the data with the app's own
  engine and settings; otherwise it runs the standalone lme4/glmmTMB reconstruction as before.
* R interface: `disappr_read()`, `disappr_example()` and `disappr_provenance()`.
* Testing: `tests/run_all.R` runs the test tiers in order; `tests/release_gate.R` adds R CMD check and the golden
  comparison with the previous release.

# disappR 0.9.10

* Architecture: the statistical engine moved out of `inst/app/global.R` into `R/` (import, validation, data
  preparation, formulas, model fitting, model comparison, diagnostics, trajectories, disappearance, missingness,
  simulation, interpretation, provenance, export, plus shared helpers and the app's help texts). Every definition was
  moved verbatim, so results, features and the interface are unchanged; `global.R` now only loads packages, sets
  options and loads the engine (from `R/` in a source checkout, from `engine/` in a deployment bundle, or from the
  installed package).
* Runtime values (optional packages, their versions and the disappR version) are set when the engine loads, so an
  installed package reflects the packages installed now.
* R interface: `disappr_mapping()`, `disappr_prepare()`, `disappr_fit()`, `disappr_compare()`, `disappr_predict()`,
  `disappr_diagnose()`, `disappr_interpret()`, `disappr_code()` and `disappr_simulate()` call the same engine as the app.
* `inst/scripts/bundle_app.R` writes a self-contained app folder for deployment without installing the package.
* `tests/golden/` captures and compares results between two versions (formulas, rows, coefficients, log-likelihood,
  AIC, predictions, validity, notes, decomposition, exported code).

# disappR 0.9.9

* Population-trajectory figure: a 'Model predictions as lines (observed ages)' tick box draws each model's
  prediction at the observed ages (or sampling occasions), joined by straight lines with a point at each age.
  Unticked, the models are drawn as smooth curves, now always evaluated on a fine age grid (previously, data with
  30 or fewer distinct ages were drawn through those ages only). Predictions themselves are unchanged.

# disappR 0.9.8

* Stale results: the count-family comparison and the performance-check note clear when data or settings change.
* Population curves no longer depend on row order: time-varying numeric covariates are held at the mean of the
  individuals' own means, and factor combinations are weighted by all records (each individual counting once).
* Guards before fitting: a trait with a single value (one outcome class, all-zero counts) and data with one record
  per individual stop with a plain message; a random intercept with one level per record is omitted in Gaussian
  models and noted as an observation-level random effect in count and binomial models.
* Redundant terms are named before and after fitting (for example, a covariate that duplicates ALR or age).
* Exported code: an 'Analysis data (CSV)' download holds the rows and variables fitted; the script uses it when
  present for exact reproduction, otherwise it rebuilds them from the raw file.
* The simulator restores R's random-number state after simulating.
* Unstandardised cubic, same-order or quadratic-interaction fits carry a numerical-stability note.
* Interpretation: a random-slope check is suggested when age-dependent terms are favoured with random intercepts
  only; cautions for censored individuals and irregular ages; ALR is described as the last record in the data.
* Individual fits: fitted and not-fitted individuals are compared (ALR, trait mean, records).
* Pre-fit summary: formulas, family and link, random effects, zero-inflation and standardisation before fitting.
* `tests/robustness_suite.R` (not part of the app): metamorphic, oracle, export round-trip, adversarial and
  reactive-state tests.

# disappR 0.9.7

* Wording: interpretation, help and simulation-expectation texts are suggestive and indicative, not causal or
  prescriptive ("lowest AIC", not "best"; "is expected to", not "should").
* AIC: the headline lists every model within 2 AIC of the lowest as having comparable support.
* P-values: a "P type" column states whether each is a Satterthwaite t-test (lmerTest) or an asymptotic Wald z-test.
* Rows before fitting: the Modelling tab shows how many records and individuals the selected models share and why
  the rest would be dropped (a dry run of the fitting code); after fitting, individuals lost entirely are reported.
* Optional packages: the Start page lists which are installed (with versions) and what each adds; missing ones are
  also reported in the console at start-up.
* Versions: exported code records the disappR, R and model-package versions; `inst/scripts/lock_dependencies.R`
  writes `dependency_versions.csv` and, with renv, `renv.lock`. Stale version references were corrected (README,
  DESCRIPTION, exported code, HTML report).
* Validation: `inst/validation/` holds an independent simulation verification and validation study (VALIDATION.md).

# disappR 0.9.6

* Start page: a short Purpose section above the workflow sets out why the app exists: selective disappearance and
  appearance are pervasive, the methods that handle them are recent and were built for simple cases, some are biased
  under missing data or under age-dependent selection (which is rarely tested), and there is little guidance on which
  model to fit.
* Decomposition on any sampling schedule. It always uses two successive sampling occasions and only the individuals
  sampled at both (survivor-restricted). On a common schedule the result is unchanged (identical to 0.9.5 for every
  bundled dataset except the chipmunk data, whose ages are irregular). On an irregular schedule the records are now
  placed on a common grid of occasions, spaced by the median interval between an individual's successive records,
  before the chain is built, and a caution below the population-trajectory figure says the result is approximate.
  Schedules such as "age 1, then every third age" are now decomposed through every occasion instead of stopping after
  the first two.
* The exported model script uses the app's own decomposition functions, so its figure matches the app.

# disappR 0.9.5

* Three new empirical examples, each pre-set to the published model: Soay sheep breeding probability and offspring
  first-winter survival (binomial) and Soay sheep offspring birth weight (Gaussian) from McKenna-Ell et al. 2023,
  Biology Letters, with age, age at last observation, bred as a yearling and early-life recruitment (each interacting
  with age; lamb capture age, sex and twin status for weight) and random intercepts for female, year and cohort; and
  leafcutting-bee locomotor activity from Szejner-Sigal et al. 2025, Proc. R. Soc. B, with a quadratic age function,
  lifespan added to Model 1 and a random intercept per bee, opening on females as in the paper.
* Examples can open with a data subset and with extra model terms; both are reset when another example is chosen.
* An example now keeps the ageing function of its published model when the Modelling tab opens; before, the tab
  replaced it with the function that fitted individuals best (the tab's own default for simulated and uploaded data).
* Decomposition: consecutive records are linked when their interval rounds to one sampling step, instead of only
  when it equals the step exactly. Schedules with an unequal first interval (day 1, then weekly on days 7, 14, 21, as
  in the bee data) are now decomposed through every occasion rather than stopping after the first two ages. Regular
  schedules give exactly the same result as before, and a missed occasion still breaks the chain. The exported
  figure code uses the same rule.
* Sampling step: a smallest common interval that is close to but not a divisor of a clearly dominant interval (6 and
  7 days in the bee data) is treated as an unequal first interval, and the dominant interval becomes the step, so the
  sampling grid follows the weekly schedule. No other bundled or test dataset changes step.
* The CONTINUOUS-box note about few distinct whole-number values no longer fires for counts from zero (0, 1, 2, 3).

# disappR 0.9.4

* Binomial models now fit binary (0/1) traits. Without a mapped trials column, the internal trials column (all
  missing) was still passed to glmmTMB as prior weights, so every row was dropped and the fit stopped with
  "contrasts can be applied only to factors with 2 or more levels". Weights are now used only when every analysed
  row has a positive number of trials. The exported model script wrote the negative binomial family for binomial
  fits and omitted the weights; it now writes binomial() or betabinomial() with weights = .trials.
* The number of binomial trials ("Weights") is chosen on the Modelling tab below the error family and is shown only
  for the binomial and beta-binomial families; a note pops up when one of them is selected. Changing it no longer
  resets the family and the model selection.
* New error family: zero-inflated negative binomial with linear variance (nbinom1), next to the nbinom2 version, in
  the models, the error-family check, the model equations and the exported code.
* Three-level nesting: an optional "next level up" grouping column, e.g. individuals within fathers within families,
  (1 | family) + (1 | family:father) + (1 | family:father:ID), in every model (including the non-linear exponential
  models), the equations, the variance table, the figures' panels and the exported code, with an integrity row for
  group labels reused across top-level groups.
* Term builder: the wrench on each model row adds a term built from up to three chosen terms (age, ALR, AFR, LS,
  mean age or any covariate) joined by + or x, including three-way interactions; interactions with age carry an
  over-fitting caution.
* Simulator: each ageing form offers biologically motivated shapes (slow, fast or no senescence; early or late peak;
  U-shape; rapid or gradual decline or growth that levels off; late-life increase). The default shape of each form
  is unchanged, so earlier simulations are reproduced exactly.
* Data tab: CONTINUOUS and CATEGORICAL covariate boxes that warn immediately when a column looks as if it belongs in
  the other box; a note and a check that age must be numeric; the upload note states that blank cells and NA are
  both read as missing.
* Figures: 95% confidence bands on the bin-difference trend lines and on the lines of the trait-against-lifespan
  figure; solid black regression lines on the ALR, mean age, LS and AFR agreement plots; a 'Panels by' menu
  (categorical variables) in the trajectory figure; optional jittered individual records in the population-level
  trajectory plot.
* The references to cite are shown in the sidebar.

# disappR 0.9.3

* Age at first trait expression (AFE) now lives only where it applies: the "Count missed occasions from" control
  on the Sampling and missingness tab, with AFR the default and a numeric AFE entry appearing only when AFE is
  chosen. Its "i" panel states that the setting changes the missingness figures on that tab and nothing else.
* The Data tab's AFR control is back to two options, automatic from each individual's first record (recommended)
  or a mapped column, and says plainly that AFR is what every model and statistic uses.
* The 0.9.2 option that set AFR equal to AFE is withdrawn, together with the constant-AFR path it used in
  standardise_data, so no entered age can reach the models. AFR, the ALR and mean-age proxies, the appearance
  models and every other statistic are unchanged by this release.

# disappR 0.9.2

* The Data tab now separates two ages explicitly. "Age at first trait expression (AFE)" is an optional single
  age entered as a number: the age from which the trait exists and could have been measured, even if recording
  began later. "Age at first record (AFR)" is when the individual enters the data and offers three options -
  automatic from the first record, the same as AFE (which requires AFE to be filled and makes AFR constant, so
  Models 7-10 become unavailable), or a column, whose dropdown appears only when that option is chosen. The
  column-based expression input added in 0.9.0 is withdrawn.
* The AFR-ALR agreement is now shown as a plot (with the 1:1 line and a fitted line) beside its table.
* The start page and the data-source panel no longer refer to "the fruit-fly data"; they refer to the published
  empirical datasets. The count-as-Gaussian note beneath the model table is shortened to the essential check.
* The predictions panel now explains why every model can return almost the same trajectory while their AIC values
  differ by hundreds of units: under age-independent selective disappearance the proxy is held at its mean, so
  proxy terms contribute nothing at that value and the age slope is unbiased in all models; the curves separate
  only when disappearance is age-dependent. Verified by simulation.

# disappR 0.9.1

* Fixes a parse error introduced in 0.9.0: the rewritten empirical-example notes were missing the comma between
  each example's own text and the shared closing paragraph inside paste(), so sourcing global.R failed. All seven
  notes are corrected.

# disappR 0.9.0

* Age at first observation is now separated from age at first trait expression. The Data tab maps entry into the
  dataset (AFR, used in Models 7-10) and, optionally, an earlier age at which the trait could first have been
  recorded. The distinction changes the missingness window: with entry only it runs from AFR to ALR, so earlier
  occasions are "not yet expressing"; with an expression age it opens there, so occasions between expression and
  entry count as missed. A new "i" panel explains the two and warns that the resulting missingness estimates are
  not comparable.
* New diagnostic on the missingness tab: the correlation between AFR and ALR, with the mean and SD of the
  observation window, and an "i" panel on how to read it (design-driven versus biological association, and what
  it implies for ALR and mean age as proxies).
* New "i" panel on the among-individual polynomial order, with a worked example (a quadratic ageing function
  gives ALR and ALR^2 plus their interactions with each age term) and the case for using it: a linear ALR term
  cannot represent a non-monotonic relation between lifespan and the trait, and will report no selective
  disappearance when the pattern is U-shaped or humped.
* Individual-fit panels: the function comparison and the mean-coefficient and mean-function reconstructions now
  state that they are unreliable under high missingness, few time steps, few records per individual or small
  samples, and that the dAICc ranking is suggestive rather than prescriptive. A warning now appears beside the
  random-slope advice whenever the data give weak support for individual slopes.
* The integer-data caution is shortened to the essential check: integer data treated as Gaussian, confirm the
  trait is continuous rather than a count or 0/1 outcome.
* The great tit example opens on laying date (LD), with the paper's other traits available from the trait
  dropdown. The seed-beetle male weight example has been removed; the female fecundity example remains.

# disappR 0.8.8

* The empirical-example notes are now qualitative. Each states what the paper did, what kind of model it fitted
  and what it generally found, then says that the app's defaults are set close to the published fit for that
  trait, can be changed, and generally reproduce the published pattern but may differ because of subsetting,
  model terms or covariates that were not fully specified, random-effect structures, or software and estimation.
  Specific published coefficients, AIC gaps and provenance wording have been removed from the notes; the file
  histories remain in data/PROVENANCE.md.
* The start page and the "Data source and simulation" panel now say that several empirical examples are
  available, from laboratory systems (fruit flies, seed beetles) and wild populations (terns, turtles, great
  tits, chipmunks), and the panel's cautions carry the same reproducibility caveat.
* New cautions on the individual-fit panels: the mean-coefficient and mean-function reconstructions can depart
  from the true latent trajectory for reasons other than selective disappearance - extrapolation beyond each
  individual's own observed ages, near-saturated fits when records are few, and, for counts, averaging on the
  response scale while the truth is a curve on the link scale - and these grow under missingness. Users are
  asked to read them alongside the population-level model predictions on the Modelling tab. The function
  comparison panel now notes that with few occasions per individual the simplest function usually wins on AICc
  (a quadratic needs at least six records for AICc to be defined), which reflects what is estimable rather than
  what generated the data.

# disappR 0.8.7

* The great tit example now ships the paper's full sample, 7,341 breeding attempts by 4,935 females, in place of
  the 7,126-row subset that omitted 215 attempts without a lay date. The replication improves: the
  individual-level peak is 2.80 years against the published 2.80 (was 2.85), the cross-sectional peak 3.48
  against 3.45, ALR +0.062 against +0.04, the post-peak slope -0.105 against -0.11, and clutch, brood and
  fledgling coefficients +0.499/-0.058, +0.594/-0.096 and +0.637/-0.109 against +0.52/-0.06, +0.62/-0.10 and
  +0.63/-0.11. The note records the counts, the lay-date missingness and the 361 females whose supplied ALR
  differs from their last recorded age.
* Built on 0.8.5, whose REPLICATION.md refits each example under its own default settings including the extra
  random intercepts. Carried over from the 0.8.6 branch: the repaired turtle sentence and a pointer from the
  empirical-example panel to the comparison table.

# disappR 0.8.5

* New REPLICATION.md compares, example by example, what each paper reported with what the app returns on the
  bundled data under the pre-filled settings, and says plainly which comparisons are quantitative, which are
  qualitative and which could not be verified offline (the fly and beetle-fecundity count families).
* Two figures quoted earlier were corrected by refitting with the examples' own default settings: the turtle
  hatchlings-alive comparison is weak (Model 4 leads Model 1 by 2.3 AIC, not 37.8, once the year random intercept
  is included) and the great tit recruit ranking under Gaussian settings gives Model 1 +6.1 rather than +13.
* Start-up checks: all three files balance, no empty arguments, every symbol used in ui.R is defined before the
  server starts, all eight example files exist with resolvable mappings, valid families, model ids and ageing
  functions, no duplicate or orphan input/output ids, and every info panel key referenced in the UI is defined.

# disappR 0.8.4

* Restores three details to the example notes: the tern stopover-threshold sensitivity (the age signature holds
  at 50 and 200 km but disappears with no filter, so the conclusion is conditional on it) and why 24 birds are
  lost; the evidence for harmonising the tern sex codes (nineteen individuals appear under both codings, and 1
  never coincides with f nor 2 with m); and the names of the restored columns, Wt_D0 in the beetle files and the
  nest microhabitat variables in the turtle file.

# disappR 0.8.3

* Merges the regenerated datasets and provenance record with the corrections from 0.8.1/0.8.2 that the previous
  build did not carry: the painted-turtle years and usable-record counts, the chipmunk sexes and the Model 6
  row restriction, the great tit sample description, and the app test's data source.
* The tern note now gives the published track and bird totals against the bundled ones, and states that the paper
  applied FLightR particle filtering before computing deflection, so the derived series reproduces the age
  signature but not every coefficient.
* The empirical-example panel points to data/PROVENANCE.md.

# disappR 0.8.2

* Data provenance audit of the bundled examples. Every file is now a verbatim copy of its archived original with
  all columns retained (earlier builds dropped a few unused columns from the beetle and turtle files, including
  the beetles emergence weight Wt_D0 and the turtle nest microhabitat variables, and renamed the turtle columns).
  The turtle example mapping uses the original column names again.
* The tern navigation example, the only derived dataset, now defines the autumn goal as the individuals mean
  wintering position rather than the tracks, which is what the paper describes: the among- and within-individual
  age effects move from -0.64 / +1.58 to -0.77 / +0.90 against the published -0.870 / +0.851. The file now also
  carries move_bearing, goal_bearing, goal_lat, goal_lon, step_km, the migration dates and sex_raw so that the
  derivation can be audited, and data/PROVENANCE.md documents every transformation.

# disappR 0.8.1

* Eighth empirical example: Bouwhuis et al. 2009, Proc. R. Soc. B - great tit (recruit production), the
  49-year Wytham dataset. Age at last reproduction is supplied in the data, so the example also demonstrates a
  mapped ALR that deliberately disagrees with the last recorded age.
* The example reproduces the published analysis closely: cross-sectional peak 3.43 years against 3.45, moving
  to 2.85 against 2.80 once ALR is fitted; ALR +0.06 against +0.04; post-peak slope -0.110 against -0.11;
  clutch size, brood size and fledgling numbers +0.48/-0.056, +0.60/-0.097 and +0.65/-0.112 against
  +0.52/-0.06, +0.62/-0.10 and +0.63/-0.11, with no ALR effect on any of the three.

# disappR 0.8.0

## Empirical examples
* The "Fruit-fly fecundity" data source becomes **Empirical examples (published data)**, a drop-down of seven
  published datasets listed as Surname et al., year, journal, species:
  - Sanghvi et al. 2025, American Naturalist - Drosophila melanogaster (daily fecundity)
  - Bichet et al. 2022, Journal of Animal Ecology - common tern (immune parameters)
  - Wynn et al. 2025, Journal of Animal Ecology - common tern (navigational efficiency)
  - Sanghvi et al. 2022, Evolution - seed beetle (male weight)
  - Sanghvi et al. 2022, Evolution - seed beetle (female fecundity)
  - Allain et al. 2023, Oikos - eastern chipmunk (reproduction)
  - Warner et al. 2016, PNAS - painted turtle (reproduction)
* Each example ships with the published data in inst/app/data, a pre-filled column mapping (covariates,
  interactions, nesting, extra random intercepts, censoring) and pre-filled model settings - error family,
  ageing function and the selected models - chosen to reproduce the published analysis. A note under the
  drop-down states what the paper found and what the app returns.
* Derived variables are included in the bundled files so that nothing has to be computed by hand: age in the
  Bichet data (year of sampling minus year of birth) and instantaneous deflection in the Wynn data (the angle
  between the movement bearing and the bearing to the goal, computed from the archived geolocator fixes, with
  steps under 100 km removed).
* New helpers: EXAMPLES, EXAMPLE_CHOICES, example_map() and load_example_file(). FLY_MAPPING and
  load_fly_example() are retained.

# disappR 0.7.6

* The two exponential functions are renamed so that they are not confused. The linear-in-coefficients basis
  (intercept + beta * exp(-age)) is now "Asymptotic exponential"; the model with an estimated rate is now
  "Exponential (a * exp(b * age))". Internal keys, the simulator, the exported R code and the tests follow the
  new names.
* Each model's "i" panel now states what the age terms are for the chosen function (for example
  "f1 = exp(-age), centred and scaled"), so models that share the formula trait ~ f1 + ... but differ in the
  age term are no longer ambiguous.
* Tests assert that the two exponential functions give different equations, coefficient names and fitting calls
  for every model where both are defined, that Models 3 and 5 remain unavailable for the estimated-rate model,
  and that each ageing function builds a different age term.

# disappR 0.7.5

* Caveats only, no change to any calculation. Added to the "i" panels: terminal effects as a rival explanation
  for the model ranking (trait before death); clustering of repeated records in the missingness and proxy-trend
  tests; hazards not accounting for frailty; saturated individual fits (quadratic on 3 records, cubic on 4);
  the two exponential parameterisations not being comparable; standardised coefficients depending on the sample's
  age range; likelihood-ratio p-values being approximate; model ranking as relative support; and multiple testing
  with no correction.

# disappR 0.7.4

* Trait-dependent missingness is now flagged as a rival explanation for the model ranking: the caveat in the
  "Missingness against other variables" help entry says that it produces the same evidence as age-dependent
  selective disappearance, and a warning appears under that plot whenever the prior-trait (or condition)
  association is significant.

# disappR 0.7.3

* AFR with fewer than two values is now a Warning in the integrity table (it names Models 7-10 and
  explains that they would repeat Models 1-4 because their AFR terms are dropped as rank deficient),
  and the same warning appears with the model results.
* Removed the calendar-year age warning added in 0.7.1.
* Binomial families: rows without a positive number of trials are dropped from the analysed rows and
  reported, instead of reaching glmmTMB as zero or negative weights.

# disappR 0.7.2

## Binomial families
* New error families: binomial (binary 0/1, or a proportion of successes) and beta-binomial for
  overdispersed proportions, both through glmmTMB with a logit link.
* New optional "Number of binomial trials" column on the Data tab. For proportion traits the trials
  are passed to glmmTMB as prior weights, reported in the integrity table, and rows without a
  positive number of trials are dropped from binomial models.
* suggest_family() now recognises binary traits (suggests binomial) and proportions between 0 and 1
  (suggests binomial and asks for a trials column); the Data tab reports the trait kind.
* Model equations, the exported R code, the error-family check and the performance checks all follow
  the binomial families (zero-inflation checks are limited to the count families).
* Guidance added: fitting binary or proportion traits as Gaussian can favour the interaction models
  even with no selective disappearance, because their variance changes with the predicted mean.

# disappR 0.7.1

## Fixes from the empirical stress test
* Fixed a stray empty argument in ui.R (`),,` before the sampling section code box) that stopped the
  app from starting.
* Binary (0/1) traits are recognised as binary rather than counts: the Data tab says so, Gaussian is
  suggested as a linear-probability approximation, and the text points to binomial() in the exported code.
* Underdispersed counts (clutch and litter sizes, breeding success) no longer get a Poisson suggestion:
  below a Pearson dispersion of 0.7 the suggestion is Gaussian, with the reason given.
* New integrity warning when ages look like calendar years (all ages between 1800 and 2100), the most
  common mapping mistake in datasets with no age column.
* The zero ratio in the trait-distribution line is capped (">999x") instead of printing huge numbers.
* The duplicate ID x age advice now covers IDs reused in different groups: map the grouping column and
  keep nesting instead of averaging.
* When a count family is refused because the trait is not integer, the message says so if averaging
  duplicates caused it.

## Guidance added
* Trajectories within bins: non-monotonic bin means mean the lifespan effect is not linear, so ALR^2 is
  needed (Models 2 and 4 use a linear ALR term and will find nothing).
* Model support: trait-dependent missingness and terminal declines both reproduce the ranking expected
  from selective disappearance, with pointers to the missingness drivers and the trait-before-death figure.
* Trait before death: a terminal change alone can make Models 4 and 5 win with no ageing at all.
* Sampling summary: individuals on genuinely different schedules appear as heavy missingness, which is
  study design rather than failed detection.

# disappR 0.7.0

## Stability, exact model equations and section scripts (latest round)
* Fixed the start-up crash "attempt to set an attribute on NULL": standardise_data() called setNames() on the
  (NULL) names of an empty covariate list whenever no covariates were mapped.
* Uploads: any error while reading a file, or a malformed result, is reported as a message before the data are used.
* Every observer runs inside an error guard, so an unexpected error is shown as a notification (and in the R
  console) instead of disconnecting the session.
* Each model's "i" shows the exact model: the fixed effects fully expanded as R expands them (A * B = A + B + A:B,
  one coefficient per term and per level of a categorical covariate), random effects and their distributions,
  the error family and zero-inflation part, and the R syntax as written, fully expanded and as fitted.
* Population-level trajectories: unticking every model no longer redraws all models (which made ticking one look
  like the others disappearing).
* Data, Visual diagnosis, Missingness and proxies, and Individual and population trajectories each end with a
  collapsible "R code for this section" box (copy or download) that reproduces the section with the current
  settings, including the app's own helper functions.

## Final polish
* Individual fits need fewer records: 2 for a linear fit, 3 for quadratic, logarithmic and exponential fits, 4 for a
  cubic fit (fits through every point carry no AICc and are left out of the function comparison).
* Workflow codes (A1-A7, B1-B5) removed from all titles, labels and help text.
* Removed the "Lifespan beyond the last record" plot.
* performance checks can no longer stop the app: each check and its formatting runs with its own error handler
  and time limit, glmmTMB models use lighter check_model() panels, and failures are reported as messages.
* The sampling grid shows up to 500 individuals (slider), with the plot height adjusted to the number shown.

## Layout and model specification
* Disappearance diagnostics merged into Visual diagnosis; one shared settings row for A1-A2 and figure-specific
  settings (bin boundaries and SE, trend lines, jittered points, survivors comparison) directly above each figure.
* A1 bin differences always compare consecutive bins; every bin x age point and A2 age bin needs at least 3
  individuals (the minimum-individuals setting was removed).
* Removed: the trait-now disappearance model plot, the A5 relative-to-age-mean option, the A7 hazard table and the
  within-model ageing-function comparison.
* Data: continuous and categorical fixed-effect covariates are mapped separately; interactions between a covariate
  and age or between two covariates are added to every model.
* Modelling: full-width settings with one row per model (tick box, "i" with the model's meaning and exact
  specification under the current settings, and a menu of terms added to that model only); model support below.
* Individual fits: "Use this ageing function for the mixed models" sets the Modelling tab's function; the
  population-level function comparison includes the non-linear exponential (Gaussian traits).
* Observed means in the population trajectory plots are drawn with a dark outline.

## Navigation and export
* Browser tab title; tabs named for what they do (Data, Visual diagnosis, Disappearance diagnostics, Missingness and
  proxies, Individual and population trajectories, Modelling, Summary and report); Start here describes each tab.
* Visual diagnosis uses one grouping variable (ALR, mean age, LS or AFR) and one number of bins for both figures.
* The population-level ageing-function comparison moved into 'Individual and population trajectories'.
* The modelling ageing function defaults to the function with the lowest mean ΔAICc across individual fits (with a
  note that simpler functions may be preferable).
* Data tab: distribution and preview before the integrity checks.
* Exported R code now also reproduces the subset, censoring and the trajectory figure (model predictions,
  observed means and the decomposition); fixed the non-linear script builder, which kept stale model lines.

## Modelling, diagnostics and interpretation
* New tab A4-A7 (disappearance diagnostics): A4 discrete-time hazard of disappearing before the next occasion
  from the current trait (optionally its change and a trait x age term); A5 trait before death by lifespan
  tercile (optionally relative to the mean at the same age); A6 selection differentials by age; A7 life table
  with the age-independent and age-specific hazard.
* Models 9 (ALR x age + AFR) and 10 (ALR + AFR x age), so appearance and disappearance can differ in age
  dependence; extra terms (ALR, AFR, LS, mean age, additively or x age, and covariates) can be added to chosen
  models; likelihood-ratio tests only for genuinely nested pairs.
* Random effects: intercept, uncorrelated or correlated intercept and slope, or automatic choice from the data
  support; polynomial among-individual terms (optional); covariate x age interactions chosen on the Data tab;
  zero inflation depending on age, ALR, AFR and covariates.
* Non-linear exponential ageing a * exp(b * age) fitted with nlme (Gaussian traits); the linear-in-parameters
  exponential basis is now exp(-z), and the simulated exponential form declines steeply early and slowly late.
* Fits classified Valid / Caution / Failed; Failed fits (e.g. non-positive-definite Hessian) are excluded from
  ranking, tests, prediction plots and automated interpretation unless included explicitly.
* Coefficients with scaling constants and estimates per original unit, random-effect variances, and a cautious
  plain-language interpretation of selection terms; performance::check_model() and related checks (fail-safe).
* Predictions: choose which models to draw, overlay the A3 reconstruction, and show curves by factor level.
* Internal consistency checks (AIC vs recovery of the simulated truth, AIC vs likelihood-ratio tests, best-fit
  validity, random-slope support, A2 trend) and a more cautious automatic summary.
* Data: optional subset by a categorical variable; AFR defined as age at first observation; lifespan margin
  removed; clearer A1-A2 colours; A1 difference trend as one pooled line or one per bin pair; A2 points optional;
  a prominent caveat when sampling is irregular; A3 data-support metrics; uploads up to 200 MB.

## Robustness (stress-tested on 22 out-of-sample datasets)
* Sampling step inferred from the most common interval between an individual's consecutive records, so
  staggered cohorts, floating-point ages and occasional extra records no longer shrink it; irregular ages use
  the median interval and are flagged. Expected occasions are anchored on each individual's first record.
  The grid is capped at 300,000 cells (coarsening reported) and the heatmap at 60,000 tiles. Results on
  regular schedules (including the fruit-fly data) are unchanged.
* Decomposition built from same-individual records one sampling step apart, following one schedule.
* Optional "Round ages to multiples of" for irregular ages (also applied in the exported R script).
* Uploads: automatic separator (comma, semicolon, tab, pipe) and decimal-comma detection, byte-order mark,
  common NA codes, trimmed white space, IDs kept as text ("007" != "7"), repaired blank or duplicated
  column names, latin1 fallback, clear messages for Excel workbooks, header-only files and dates in the age column.
* Integrity checks for irregular or offset schedules, -99/-999/-9999 codes, ages <= 0, extreme values,
  covariates without variation, with many levels or missing values, and empty columns.
* Models: covariates without variation are omitted with a note instead of stopping every model; ageing
  functions that need more distinct ages than the data have are refused with a message; covariate and
  random-effect names that collide after make.names are kept apart; glmmTMB adjusts rank-deficient designs;
  plain-language hints for unidentifiable random slopes. Individual fits use lm.fit, and the A3 function
  comparison uses at most 2,000 individuals.
* New `tests/stress_test.R` with the datasets in `tests/stress_data`.

## Latest revisions
* Simulator: five ageing forms (linear, quadratic, cubic, logarithmic, exponential, using the same bases as
  the models). Selective disappearance and selective appearance are each set as none / age-independent /
  age-dependent / both, with a positive or negative direction (replacing the intercept/slope/shape selectors).
  Individual differences in ageing rates (small/large) and mean lifespan (3-30 occasions); diet covariate that
  lowers the trait without affecting lifespan; AFR and ALR uncorrelated by construction.
* A1-A2: facet (panel) by any covariate or treatment with 2-8 levels; number of bins from 3 to the number of
  distinct ages (A1: values of the grouping variable) observed in at least the minimum number of individuals,
  counting single-record individuals; one bin per value when bins >= distinct values. A1 bin differences use
  age on the x-axis with a least-squares trend line per pair. A2 adds a table of the regression coefficient in
  each consecutive age bin (95% CI, r, change from the previous bin) and a weighted trend across age.
* A3: mean of coefficients and mean of individual functions (identical for functions linear in their
  parameters; formulas in the help), each with a 95% band; adds a non-linear individual function
  a * exp(b * age); curves restricted to ages observed in >= 10 individuals; minimum residual df fixed at 1.
* Sampling grid: observed / missed / death or ALR / not expected, ordered by ALR, AFR, mean trait, ID or at random.
* Models: random-effect variances, SDs, correlations and dispersion shown with the coefficients (and for all
  models); B2 function comparison with selectable functions and distinct colours; new within-model comparison
  of ageing functions for any of Models 1-8.
* "i" help for every section (what it does, how to read it, cautions) and "Save to summary" buttons; the
  Summary tab lists saved results and exports them as an HTML report (with plots) or a text report.

## Earlier revisions
* A2 now plots the trait against LS (or ALR / mean age / AFR) within age bins, with a linear fit per age bin.
* New A1 panel: signed difference in mean trait between proxy bins at each age (successive bins or all pairs).
  The A2 interpretation box was removed; bins are numbered in the A1 legend.
* Teaching data: AFR is fixed (everyone starts at age 1) or variable. Variable AFR is either random or
  selective appearance (positive or negative). MCAR samples ~75% of occasions (the paper simulation keeps the
  Methods' 0.5). Trait- and condition-dependent missingness were rescaled to ~25-30% missing.
* Data: age at first trait expression is either each individual's AFR or a common age for everyone, so that
  occasions between that age and the first record count as missing. Adds a censoring indicator (LS set to
  missing for censored individuals; fly data: `Censored = 0`), additional crossed random intercepts `+ (1 | X)`,
  and a distribution plot for any variable.
* Sampling: ALR vs mean age, ALR vs LS and mean age vs LS as separate labelled panels (ALR vs mean age only
  when LS is unknown).
* A3: free x and y scales per individual.

Rebuilt after an out-of-sample check on the fruit-fly fecundity data (Sanghvi et al. 2025, Am. Nat.).

## Why the rebuild: the fly data exposed a modelling problem
* 0.6.x fitted Gaussian LMMs only. On the fly counts (30% zeros, strongly overdispersed) a Gaussian
  Model 1-5 comparison favours **Model 5** (Model 4 is about 19 AIC worse). The manuscript's zero-inflated
  negative binomial favours **Model 4** (dAIC of Models 1, 2, 3, 5 = 38.4, 24.1, 28.0, 2.7). An independent
  re-implementation of the ZINB GLMM reproduced these values (38.3, 24.0, 27.2, 2.6). The error family
  changes the conclusion, so it is now a first-class choice.

## Modelling (B1-B5)
* Error families: Gaussian (lme4), Poisson, negative binomial (nbinom2, nbinom1), zero-inflated Poisson
  and zero-inflated NB (glmmTMB), with constant or age-dependent zero inflation.
* Fixed-effect covariates, and a higher-level random effect that can be nested (individuals identified by
  group + ID, i.e. `(1 | group/ID)`) or crossed.
* Age and proxies standardised before fitting (a linear reparameterisation: likelihood and AIC are
  unchanged, but raw age x ALR interactions no longer produce NaN/non-positive-definite Hessians).
* All models are fitted on common complete-case rows. Model 6 is left unselected by default when LS is
  missing for some individuals, and Models 7-8 when AFR is (nearly) constant, so Models 1-5 are not
  silently truncated.
* Nested likelihood-ratio tests (1 vs 2, 2 vs 4, 3 vs 5, 2 vs 7, 4 vs 8, 7 vs 8), Akaike weights,
  convergence/singularity/boundary notes.
* Population predictions follow the manuscript: random effects excluded, proxies held at individual-level
  means, factor covariates averaged over individuals, response scale.
* Error-family check (AIC across count families), DHARMa residual checks, and a generated R script that
  reproduces the comparison outside the app.
* Fitting runs on a button press, with a staleness guard when data or settings change.

## Diagnostics
* New data-integrity table: duplicate ID x age records, IDs linked to more than one group, mapped ALR
  vs last record, missing LS, LS earlier than the last record, LS - ALR gap, off-grid ages, records per
  individual, AFR variation, and a suggested family (Poisson GLM Pearson dispersion and zero excess).
* A1 bins are formed among individuals (not rows), with equal-width (as in the manuscript) or quantile
  boundaries and a minimum number of individuals per bin x age point.
* A2 (new): proxy-trait slope or correlation at each age with 95% CIs and an inverse-variance weighted
  test for change with age. It replaces the nlme "simple cross-check" models.
* A3: minimum residual df per individual (exact fits no longer report R2 = 1), a warning when fitted
  individuals are longer-lived than the population (survivor bias), and a choice of which individuals
  to draw (instead of "0 = all").
* Sampling: expected windows now end at the last occasion at or before LS (floor, not rounding), plus an
  optional lifespan margin. B3 guidance is based on detection probability and proxy correlations (the
  3/p - 1 threshold was removed). New panels: ALR / mean age vs LS, LS - ALR gap histogram, and
  coverage by age.
* Decomposition: starts at the first two consecutive ages at which individuals were sampled at both.
  The starting value is the mean trait at the first of these ages of those individuals. The 0.6.1
  all-individuals anchor and the anchor selector were removed.

## Teaching simulations
* Traits: continuous body mass (Gaussian), fecundity counts (Poisson, negative binomial, ZINB; log link),
  or the exact manuscript parameterisation.
* "Dramatic" strength (|rho| = 0.85 through a latent-quality factor) makes patterns obvious.
  "Moderate" uses |rho| = 0.5.
* Independent sign choices for lifespan correlations with intercept, slope and shape.
* Sampling options: complete, MCAR, MWO, MWY, trait-dependent and condition-dependent.
* Optional selective appearance, a selective treatment covariate, and 25 families (nested random effect).
* Each simulation states what the user should see. Model predictions and the decomposition are scored
  against the true trajectory (Methods Eq. 11).

## Empirical example
* The fruit-fly dataset is bundled, with mapping, covariates, nesting and family pre-filled.

## Other
* Removed: nlme dependency, the generic example CSV, the proxy-by-age-class plot.
* Tests: `tests/smoke_test.R` (all analysis functions, plus the fly replication checks) and
  `tests/app_test.R` (drives the real server with `shiny::testServer()`).

# disappR 0.6.1

## Fixes that stopped the app running
* `server.R`: `download_example` content handler contained `;` inside an argument list (parse error).
* Package layout: app files moved to `inst/app/` (logo to `inst/app/www/`); `run_app()` calls `shiny::runApp()`.

## Bugs
* Long model formulas were rebuilt via `deparse()`, which splits at ~60 characters (cubic Model 8 always failed).
* AFR variance check could return `NA` inside `if()`.
* Individual trajectories: "0 = all individuals" drew no curves; empty fits crashed the facet plot.
* Individual dAICc taken over different candidate sets per individual.
* Log basis choice fixed once per dataset.
* Missingness "After last observation" relied on an unmapped ALR column.
* Column auto-detection mangled upper-case names.
* Prediction transform parameters and within-individual centring corrected.
* Decomposition anchor changed to all individuals at the anchor age (replaced in 0.7.0).
* Font Awesome 4 icon names replaced.

## Improvements
* Model fitting status table; quantile bins for continuous proxies; corrected the claim that Models 4-6 are
  equivalent under complete sampling (Models 4 and 6 are; Model 5 is not in the same design space).
