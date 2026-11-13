# Audit of disappR 0.22.0 (fixes released as 0.22.1; Part C added in 0.22.4, Parts D and E in 0.22.7)

This folder holds the audit of the 0.22.0 release. Part A re-checks everything that existed before 0.22.0, looking for
breakage, crashes and inaccuracy. Part B stress-tests the 0.22.0 additions with data built to exercise them. It follows
the 0.21.0 audit (`../audit_0_21_0/`) and re-runs its scripts.

**How it was done.** R was not available in the audit environment, so nothing here executes the R code. The audit
combines three kinds of evidence:

- **Static analysis** of the R source: `tools/static_audit/`, plus the scans in `scripts/audit_a_existing.py`.
- **Line-by-line Python ports** of the engine functions touched in 0.22.0 (`scripts/port022.py`). Each port asserts
  the R source's thresholds and branches, so a change in R that is not mirrored here stops the audit rather than
  passing silently.
- **Small maximum-likelihood mixed models** (`scripts/lmm022.py`), used to check the statistics on simulated and
  bundled data. The fitter is itself checked in B3 (its likelihood ratios follow the expected chi-square distribution)
  and in A8, where this audit's decomposition port agrees with the 0.21.0 audit's independently written port to 2e-13.

**The release gate is still the R suite:** `Rscript tests/scripts/run_all.R`. It runs the unit tests (including
`tests/testthat/test-0-22.R`), the app-server tests, the golden reference and `R CMD check`.

**Files and how to re-run them.**

| File | What it holds |
|---|---|
| `a_existing.txt` | Part A output (26 checks passed, 0 failures; lists 120 change hunks) |
| `b_new.txt` | Part B output (118 checks passed, 0 failures) |
| `c_consistency.txt` | Part C output: numbers and words against the truth under every scaling (18 checks passed, 0 failures) |
| `d_wording.txt` | Part D output: wording and instructions (7 checks passed, 0 failures) |
| `e_complex.txt` | Part E output: complicated data structures (21 checks passed, 0 failures) |
| `h_audit.txt`, `h_models35.csv` | Part H output: continuous integration, frailty in the hazard models, Models 3 and 5 specifications (13 checks passed, 0 failures) |
| `g_fixes.txt` | Part G output: the code review's fixes (16 checks passed, 0 failures) |
| `m_text_cleanup.txt` | Part M output: interface clean-up, and (0.24.8) the default random structure (39 checks passed, 0 failures) |
| `l_conflicts.txt` | Part L output: tick-box conflicts with mapped columns and AFE, and propagation to the models (19 checks passed, 0 failures) |
| `k_odd_scenarios.txt`, `k_scenarios.csv` | Part K output: odd missingness x trait x age scenarios with LS and ALR given (17 checks passed, 0 failures) |
| `j_trait_ages.txt`, `j_snapshot_before_after.md`, `j_examples.csv`, `j_example_models.csv`, `j_simulations.csv`, `j_code_diff_0_24_1_to_0_24_4.patch` | Part J output: trait-specific ALR/AFR and the missingness window (33 checks passed, 0 failures) |
| `i_gamm_removed.txt` | Part I output: the GAMM layer removed cleanly for 0.24.0 (7 checks passed, 0 failures) |
| `static.txt` | The static audit |
| `rerun_0_21_0.txt` | The 0.21.0 audit scripts re-run against this code |
| `scripts/` | The Python used |

To re-run: `cd scripts; OLD=<0.21.16 tree> python3 audit_a_existing.py; python3 audit_b_new.py; python3 audit_c_consistency.py; python3 audit_d_wording.py; python3 audit_e_complex.py`. Set `QUICK=1` for
fewer replicates.

## Part C: do the reported numbers and words match the truth under every scaling? (0.22.4, `c_consistency.txt`)

Added after a user found the coefficient table and the interpretation text of an unstandardised cubic model
disagreeing (0.22.3). Every quantity the app reports depends on how age and the lifespan proxies are scaled: the
coefficients, their conversion "per original unit", the term descriptions, the reference age of main effects, the
predicted differences in the text, the random-term flags and the ages quoted. `scripts/audit_c_consistency.py` ports
those pieces from the R source and checks each against an independent truth. The runs cover four datasets (two
simulated, one with ages from 0; the great tit and turtle examples), all five ageing functions, eight models (1-7
and 10), with and without standardisation. The same checks run in R as `tests/testthat/test-consistency.R`.

| Check | Truth it is compared with | Result |
|---|---|---|
| C1 Term description x scale factor reproduces the model column | The described quantity computed from the raw data | Exact (largest error 6e-16) |
| C2 Estimate / scale factor | Refit of the model in the described units | Agrees (2e-6) |
| C3 Standardising changes the parameterisation, not the model | Fitted values with and without standardisation | Identical (2e-7 SD) |
| C4 Predicted differences quoted in the text | The same differences without standardisation, and a hand calculation from the coefficients | Identical (3e-7) |
| C5 The reference age named for a main effect | The predicted difference at that age | Exact (5e-15) |
| C6 Joint Wald test of the interaction block | The same test without standardisation | Identical (5e-7); the main-effect test is not, as it should not be |
| C7 No displayed number rounds to zero | The value itself | Two-decimal display hid 11 of 128 coefficients; none now |
| C8 Ages quoted for the mean-age term | The real mean ages of the individuals used | Fixed: see below |
| C9 "Explains no variance" flag | The same slope with its covariate rescaled | Scale-free now; the old rule was not |
| C10 Peak and onset ages | The same with and without standardisation | Identical |
| C11 Tables and scaling constants | The data's means and SDs; every table through the rounding guard | Pass |

**Faults found and fixed in 0.22.4.**

1. **The mean-age term quoted ages that were not ages (C8).** For the logarithmic and asymptotic exponential
   functions, the text converted the mean ageing term back to age with the polynomial formula. It therefore printed a
   log-scale or exponential-scale value as "mean age ≈ −0.77" (off by 6-9 years).
2. **Younger and older were swapped (C8).** For the asymptotic exponential, whose term falls with age, the
   "younger" group was the older one.

   The text now quotes the real mean age of the individuals used and orders the groups by it.
3. **The main-effect reference was named for the wrong function (C5).** 0.22.3 said "age 0" for every
   unstandardised function. That was right only for the polynomials: the logarithm's term is 0 at age 1 (or at the
   youngest age when ages include 0), and the asymptotic exponential's never is. Standardised non-polynomial functions
   now name the centre with its age. The M5 main effect is named as each individual's own mean age, because its
   deviations are centred within individuals whatever the scaling.
4. **Every table rounded small numbers to 0.00 (C7, C11).** Not only the coefficient table: every table did. All
   tables now pass through a guard that keeps values below 0.005 readable and leaves other columns unchanged.
5. **The log term's label was wrong when ages include 0 (C11).** It is now described as log(age − youngest age + 1).

Parts A and B were re-run on 0.22.4 and still pass (0 failures).

## Part D: wording (0.22.7, `d_wording.txt`)

The question behind Part D: could a user who reads only the boxes, labels and help panels tell what to do, what to
run and how to read each result? `scripts/audit_d_wording.py` checks every user-facing string, and a read-through of
all 60 help panels followed.

| Check | Before | After |
|---|---|---|
| D1 Boxes with a help panel | 38 of 44 | 39 of 44; the rest are the purpose, glossary, preview, saved-results and effect-size/bootstrap containers, whose parts have their own help |
| D2 Panels saying what it is, how to use or read it, and what to watch | 6 incomplete | all |
| D3 Buttons, tabs and settings named in texts exist under that name | 24 unresolved; 2 wrong | all exist |
| D4 Abbreviations defined | glossary of 7 terms | glossary of 13 terms, adding AIC/ΔAIC, random slope, error family, sampling interval, SD/SE/CI and MCAR; IQR, GLMM and QQ spelt out |
| D5 Long sentences repeated across panels | 1, in 5 panels | none |
| D6 "Not available" messages that say what to do | 15 of 39 | 36 of 39; the other 3 state a data limit with no user action |
| D7 Help sentences over 45 words | 23 | none |
| D8 Result panels that say how to read the result | 6 without | all |

**Errors found and corrected.**

- "Restricted maximum likelihood" for the population-level function comparison: every model comparison uses maximum
  likelihood.
- Binomial families listed as fitted with lme4: every family except the Gaussian uses glmmTMB.
- Effect sizes described as "on the trait's own scale" in one paragraph and "on the fitted link scale" in the next.
  They are on the link scale.
- Outdated statements that random slopes cover only the first age term, and that "Random slope" is a tick box.
- The old rule for flagging random terms, replaced in 0.22.3.
- A "Same polynomial order" option that no longer exists (the options are "Higher order, …").
- References to a "Models tab", a "Sampling tab" and a "lifespan margin" setting, none of which exist.
- A figure caution about the trait scale repeated in five panels. It now appears once, in the figure settings.

**Clarity added.** An empty decomposition used to disappear from the prediction figure without explanation; the note
under the figure now says why (see E2). The model-support help names two traps found in Part E.

Two help panels (`definitions`, `quickstart`) are not linked from any box; they are kept for the R interface.

## Part E: complicated data structures (0.22.7, `e_complex.txt`; in R: `tests/scripts/complex_scenarios.R`)

Scenarios not tested before, run on the ports here and through the real engine in the new R tier 8.

| Scenario | Result |
|---|---|
| E1 Monthly ages with the interval typed as 0.0833 | Same 120 links, same missingness (9.9%), no record dropped; the rounding drifts by 0.05 of an interval per 10 years and would drop records only after about 52 years |
| E2 Staggered biennial cohorts (odd and even years) | Interval (2) and missingness (0%) correct. The decomposition is empty: it links neighbouring occasions, which no individual shares. **Now explained in the app**; a design limitation, not a fault |
| E3 Annual sampling to age 5, then biennial | Planned skips count as missed (22%) because they are measured against the annual interval; the help now says so |
| E4 Study window (calendar years 15-35), left truncation and censoring, no selection | No trend: false Model 2 preference 2%. A trend of −0.08 per calendar year: **Model 2 wrongly wins in 100%, Model 4 in 99%**. With year in the model: 2%. **Trap: add year as a random intercept**; now in the help |
| E5 AFR correlated with lifespan (r = −0.54), no effect of AFR itself | Selection on age at death: Model 10's AFR × age is "significant" in **100%**, Model 8's in 8%. **Trap: compare with Model 8**; now in the help. Selection on adult lifespan (ALR − AFR): AFR × age carries it even in Model 8, because lifespan counted from entry depends on AFR. An "appearance" term then reflects lifespan from entry, not an effect of entry age |
| E6 Unstandardised cubic on ages in days | Design condition number 1e10, yet the fit matches the standardised one to 2e-9 here. Coefficients such as −1.5e-10 were displayed as 0.00 before 0.22.3 |
| E7 Tiny data (12 individuals, 8 with one record) | Every function returns without error; a quadratic on two distinct ages is rank-deficient and handled as a Caution fit |
| E8 The instant error-family screen | Poisson data: 0% false alarms. Negative binomial (θ = 2): overdispersion flagged 100%. 25% extra zeros: flagged 100%. Skew screen: 0% on Gaussian residuals, 100% on lognormal σ = 0.5, about a third on mild skew (σ = 0.3), where the automatic AIC comparison decides |
| E9 Beta-distributed proportions | Beta preferred over Gaussian by more than 2 AIC in 100% of datasets; exact 0 or 1 refused with a message |

The R tier adds E10: M4 fitted with a random intercept, with random slopes, and with a logarithmic function falls in
one stored-model comparison set. It marks statistical expectations that can miss by chance on a single dataset as NOTE
rather than FAIL.

## Part H: continuous integration, frailty in the hazard models, and Models 3 and 5 (0.23.2, `h_audit.txt`, `h_models35.csv`)

**H1 Continuous integration (9 checks pass; since 0.24.6 also that every output the app test renders exists).** Three GitHub Actions workflows:

- **R CMD check** on macOS, Windows and Linux (current and previous R) for every push and pull request. A
  separate job on the same triggers runs the app server test and the unit tests from the source tree.
- **Test coverage** on every push; the upload to Codecov cannot fail the build.
- **All script tiers** weekly and on demand.

The app server test now exits with an error when an output fails (before, it only printed the failure) and runs
inside the unit tests (`test-app-server.R`), where it is skipped on CRAN and outside a source
tree.

**H2 Individual frailty in the discrete-time hazard models.** Both the trait-hazard models and the life-table test now
fit a random intercept for individual when it can be estimated, and fall back to the GLM, with a note, when it cannot.
In 150 simulated datasets per condition (300 individuals, one terminal event each, frailty SD 1):

| | GLM | GLMM with frailty |
|---|---|---|
| Type I error of the trait effect | 7.3% | 7.3% |
| Power (log-odds 0.25 per trait SD) | 98.0% | 97.3% |

The frailty SD was recovered (median 1.04) but estimated at zero in 16% of datasets. With one terminal event per
individual, the discrete-time likelihood already factorises over occasions (Allison 1982), so the plain GLM was not
anticonservative. The change is correct but changes little; its main effect is to say which model was fitted.

**H3 Models 3 and 5: preprint, revised manuscript and app.** On the preprint's own design (quadratic ageing; lifespan
correlated with intercept, slope, shape or all three; complete sampling and 50% missing at random; 30 replicates),
three versions of the within-individual quadratic term were compared:

- **"mean of squares":** age² minus the individual's mean of age². This is the revised manuscript (Table 1) and the app.
- **"square of mean":** age² minus the square of the individual's mean age. This is Fay et al. 2022, eq. 3, and one
  reading of the preprint's notation.
- **"squared deviation":** (age − mean age)². This is Fay et al. 2022, eq. 4.

Results:

- **The app matches the revised manuscript.** The app's standardised Model 5 is the revised manuscript's Model 5
  (identical AIC, difference 5e-8), so the app's scaling changes nothing.
- **"Mean of squares" and "square of mean" are nearly the same model.** Trajectories within 0.36 percentage points;
  AIC within about 2 in most scenarios and at most 10.7. They differ only by the variance of each individual's sampled
  ages, entered with the quadratic coefficient.
- **In Model 5,** "squared deviation" fits worse in every scenario (ΔAIC 9-74).
- **In Model 3 under age-dependent selection,** "squared deviation" fits far better (ΔAIC 149-2190) and its trajectory
  is closer to the truth. The reason: (age − mean age)² contains −2 × age × mean age, an age-by-mean-age interaction,
  so it partly models the age-dependent selective disappearance that Model 3 otherwise omits. It is a different model,
  not a better Model 3.

Parts A-G were re-run on 0.23.2 and pass. Part A's regression check now ignores comments and accepts the frailty
test added to `life_table()`.

## Part M: interface clean-up (0.24.7, `m_text_cleanup.txt`)

38 checks, all passing: 16 requested removals are gone (among them the suggested starting analysis and its Apply
button, the proxy-choice card, the data-support box on the individual-trajectories page, the duplicated random-effect
warning, the frailty clause and the AIC values in the example notes), 19 revised texts are in place, all 13 bundled
examples have a short summary with the full note folded under "More details", and the new plot of the stored models'
predicted trajectories is placed and rendered by the app test. Parts A-E, G, H and J-L were re-run on 0.24.7 and pass.
In 0.24.8 one more check covers the default random structure: slopes when the data support them well (every age
term when most individuals have four or more ages), a random intercept otherwise, and the published structure for
bundled examples.

## Part L: tick-box conflicts and propagation to the models (0.24.4, `l_conflicts.txt`)

**L1 precedence and wording (6 checks).** A mapped ALR or AFR column always wins over the box; the note under the box
now says when a mapped column means the box changes only one of them, or neither. The automatic ALR, AFR and
lifespan choices name the box.

**Conflict found and fixed.** With an age at first expression (AFE) set on the Sampling tab, the window opened at the
earlier of the AFE and the individual's *first record of any kind*, ignoring both the box and a mapped AFR. It now
opens at the earlier of the AFE and the AFR the models use. Unticked with automatic AFR (every bundled example) the
result is unchanged.

**L2 propagation (9 checks).** The models' ALR and AFR proxies, Model 6's automatic lifespan, the evidence and effect
summaries, the proxy panels, the data checks, the grid, the exported script and the methods draft all read the same
prepared AFR/ALR. The box is in the data fingerprint and the fingerprint also sums the prepared ALR and AFR, so
fitted results are shown only for the setting they were fitted under: changing the box forces a refit.

**L3 (4 checks).** A line-by-line port of the R grid code matches the specification in all 1440 combinations of 60
random datasets x AFR automatic or mapped x ALR automatic or mapped x ticked or not x no AFE, an early AFE or a late
AFE, including the review's case (AFR column + ticked + AFE). The R counterpart is `tests/testthat/test-0-24-4.R`,
which also checks the models' ALR and AFR proxies for every combination.

## Part K: odd missingness x trait x age scenarios, and what the tick option reaches (0.24.3, `k_odd_scenarios.txt`)

**K1 (10 checks).** The tick option reaches the missingness grid (AFR to ALR), the AFR-ALR and ALR-mean age
correlations, the proxy tables and, since 0.24.3, the data checks ("mapped ALR vs last age" and "LS - ALR"); mean age
is always from the ages with a trait value. By design it does not reach the disappearance hazard, the life table or
the "LS earlier than last record" check, which use the last record of any kind. The app server test now toggles the
box and redraws the grid, the proxies and the checks under each option.

**K2 (7 checks; 75 datasets x 2 traits x 3 ALR mappings x 2 options).** LS known; sightings every year (85%);
trait A measured at 70% of sightings with low values missed more often when old (missing not at random x age);
trait B measured only from age 2 to 7; ALR automatic, mapped as the last sighting, or mapped with 5% inconsistent
values (earlier than the last trait value). Every invariant holds on every dataset: every observed trait record lies
inside its window; the window ends at the automatic ALR; automatic ALR never exceeds LS and is never later ticked
than unticked; a mapped ALR is the same under both options; an inconsistent mapped ALR keeps observed records inside.

**Findings.**
- A trait sampled over part of life only (trait B) makes the ticked ALR a poor lifespan proxy (r with LS 0.51-0.53,
  against 0.99 unticked), while ALR and mean age agree more (r 0.81-0.83 against 0.43-0.44). Age-independent
  selective disappearance was still detected in 100% of datasets under both options.
- Missingness not at random (trait A) produces false age-dependent calls even with known lifespan (16% with no
  selection, 32% with age-independent selection): no proxy choice removes this. The ticked ALR adds a little
  (24% and 44%); unticked matches known lifespan (16% and 28%).
- Missingness is roughly halved ticked (19-33% against 56-62% unticked), because unticked counts sightings without a
  trait value as missed occasions.
- With ALR mapped as a column, the option changes only AFR, the window start and the missingness figure.

## Part J: trait-specific ALR/AFR and the missingness window (0.24.2, `j_trait_ages.txt`, `j_snapshot_before_after.md`)

**What changed.** When ALR and AFR are not mapped as columns they are now, by default, the last and first ages at
which the trait has a value (zero is a value, NA is not), the same records mean age uses. A tick box on the Data tab
("Trait-specific ALR/AFR (default)") switches to the last and first ages of any record. Bundled examples open
unticked, so their documented model results are unchanged; uploads, simulations and `disappr_mapping()` open ticked.
Simulations now use the observed AFR (first observed record). The missingness window runs from AFR to ALR, both
included, and the AFR is shown as a blue tile.

**Checks (33, all passing).**
- J1 (15): the rule, the defaults, the tick box and help, the fingerprint, the exported script, the methods draft,
  the window, the AFR tile and order; the hazard and life table unchanged.
- J2 (11): your worked example (ticked AFR 2, ALR 4; unticked AFR 1, ALR 5), a second trait in the same file, zeros,
  an individual never measured, mapped columns, gaps and a single record, by an independent implementation that
  the R unit tests (`test-0-24-2.R`) mirror.
- J3-J5 (7): all 13 examples processed; files with several traits; simulations.

**Findings.**
- **Bundled examples.** Models unchanged unticked. Ticking changes ALR/AFR only in Apollo (107 ALR, 300 AFR),
  turtle (36, 40), tern immunity (9, 25), great tit (AFR only, 137) and marmot (1 AFR); the refitted best models do
  not change (Apollo: Model 5 best either way; turtle and great tit: Model 4; tern: Models 1, 7, 3 and 4 within 1 AIC).
- **Missingness figures of six examples change even unticked,** because the window now opens at the AFR and
  closes at the ALR: tern immunity 32.9% to 35.6%, swift 9.6% to 10.8%, Apollo 65.7% to 69.1%, chipmunk 61.2% to
  62.3%, great tit 10.3% to 10.9%, turtle 27.0% to 28.8%. Records without a trait value inside the window now count
  as missed, as the unticked definition implies.
- **Several traits in one file.** Unticked, every trait shares one ALR per individual; ticked, each trait gets its
  own (great tit ACW: 147 of 4499 females have an earlier ALR than the file-wide one).
- **Simulations with records kept when the trait is missing (450 datasets).** The file-wide ALR is always the closer
  lifespan proxy (r with lifespan 0.99-1.00 against 0.77-0.98 trait-specific; mean age 0.75-0.95), because
  sightings continue after the trait stops being measured. Yet model conclusions barely differ: age-dependent
  selective disappearance was detected in 100% of datasets under both options, false alarms differed by at most
  7 percentage points, and trajectory error was equal. Missingness is roughly halved ticked (records without the
  trait are outside the window). With no records lacking the trait (the app's own simulator) both options are
  identical.

**Snapshot.** `j_snapshot_before_after.md` compares 0.24.1 with 0.24.2 (unticked and ticked) for every example,
the multi-trait files and the simulations; `j_code_diff_0_24_1_to_0_24_4.patch` holds every code change. The R
counterpart is tier 9, `tests/scripts/trait_ages_snapshot.R`, which refits the documented models under both options.

## Part I: 0.24.0 without the GAMM layer (`i_gamm_removed.txt`)

The GAMM layer (0.23.0 to 0.23.2) is held back from 0.24.0 so that the rest of the app can be released and tested
on its own first. Its code, tests, help and audits (the former Parts F and G2-G3) are kept outside the package for
reintroduction. Part I checks the removal against 0.23.2 (7 checks, all passing):

- **No residue:** no GAMM function, input, output, help key, package or file is left in the package, app, tests or
  workflows. Only `R/gamm.R`, `tests/testthat/test-gamm.R` and `tests/scripts/gamm_vs_glmm.R` were removed, and no
  file was added.
- **Removal only:** every removed block contains GAMM code. The only lines that changed are lists that lost their
  GAMM items, the last menu item losing its trailing comma, one reflowed workflow comment and the version number.
- **The rest of the engine is untouched:** every R/ file other than `export.R` (the GAMM methods paragraph) and
  `ui-helpers.R` (the GAMM help panels) is byte-identical to 0.23.2, and all 282 engine functions outside `gamm.R`
  are still defined.

Parts A to E, G and H were re-run on 0.24.0 and pass; the static audit passes (157 inputs read, 133 outputs,
60 help entries).

## Part G: the code review's fixes (0.23.1, `g_fixes.txt`)

**G1 Fixes.** Each of the review's ten correctness findings is fixed, and the fix is checked in the source (16 checks,
all passing):

1. The random-slope warning reads the fitted structure.
2. The suggested analysis offers Model 6 only with a known lifespan.
3. The bootstrap caution prints the number of datasets that refitted.
4. The checksum is of the file the app loaded.
5. There is one 50 MB upload limit, set in one place.
6. The random-slope support check covers the "every age term" structures.
7. No message says duplicates were averaged.
8. The mean-age note covers Models 3 and 5 only.
9. The model script is written as UTF-8.
10. The layout test skips without the app sources.

Also: unused server code removed; automatic checks cached by signature; exported scripts use the public
`disappr_read()`; model names come from one source; stale file headers and `.Rhistory` removed.

## Changes after this audit (0.22.2)

The two open items on the bootstrap and the family labels were resolved in 0.22.2. The null-model bootstrap now
carries the analysis's variance model to the null model and to every refit, and binomial and beta-binomial fits now
have proper labels. `scripts/audit_b_new.py` checks the new bootstrap behaviour, so re-running it tests the shipped
code. The open items below refer to 0.22.1.

## Faults found and fixed in 0.22.1

1. **Setting the sampling interval could move or change the decomposition.** The set-interval path anchored its grid on
   the youngest age and drew occasions at grid ages. This caused two errors:
   - In the bee example (day 1, then weekly from day 7), setting the interval moved every occasion one day late
     (8, 15, 22 and so on).
   - In the chipmunk example, setting the interval to exactly the value the app infers dropped 175 records.

   Now:
   - An interval equal to the inferred one gives exactly the automatic decomposition; this holds for all 13 examples.
   - A different interval anchors its grid on the most common phase of the ages within the interval.
   - Every occasion is drawn at the mean of its records' real ages.
2. **Share of wins double-counted ties.** Each function tied for the lowest AICc counted a full win, so the shares
   could sum to 160% (the tern navigation example). Tied functions now share the win equally, and the shares sum to
   100%.

Both fixes are mirrored in the ports and covered by new unit tests.

## Part A: what existed before 0.22.0 (`a_existing.txt`)

| Check | Result |
|---|---|
| A1 Parser self-test, R syntax, calls against formals | 87 files, 0 syntax errors, 0 argument errors |
| A1 App wiring, help panels, examples, stress data | 157 inputs, 133 outputs, 60 help panels, 13 examples, all resolved; no loading conflicts |
| A2 Observers wrapped in the error guard | 59 of 64 |
| A3 Column and box widths per layout row | none wider than 12 |
| A4 Every function called in `server.R` is defined | yes; the unresolved names are base, shiny or ggplot2 functions, or function-valued arguments |
| A5 Every saved title has a place in the summary order | yes |
| A6 Changes against 0.21.16 | 120 hunks, classified below |
| A7 Every example loads with its mapping; grid and decomposition run | 13 of 13 |
| A8 Two independent decomposition ports agree | 300 points, largest difference 2.3e-13 |

**Unguarded observers (A2).** The five observers without the error guard all predate 0.22.0 and cannot fail in
practice. They clear a cache, log a tab change, and switch the data source from the three journey buttons.

**A6 regression.** Every hunk falls into one of these groups:

- New arguments with defaults that keep the old behaviour. These are `step` (defaulting to `NULL` or
  `meta$age_step`, which is `NULL` unless the user sets an interval) and `disp = "constant"`.
- `infer_age_step()` replaced by `resolve_age_step()`, which returns the same value when no interval is set.
- Family additions: new switch branches, labels, and validation that runs only for the new families.
- New server blocks and new UI boxes.
- Layout and wording.

The script proves three regression points directly:

- `population_newdata()` is a verbatim move of the old prediction-grid code.
- In the seven functions that consume the sampling interval, the only changed lines are the signature and the interval
  lookup.
- The default model path still calls `lmer` exactly as before, which is what the golden reference tests.

Behaviour changes to pre-existing features, all intended:

- The error-family check now runs for continuous traits and compares the continuous families.
- Beta-binomial fits now list their dispersion parameter among the random-effect components.
- Gaussian fits with an age-dependent variance use glmmTMB (Wald z-tests).
- The step-4 comparison table gains the Share_best column.

**A7 examples, under the defaults (ports):**

| Example | Inferred interval | Missing occasions | Decomposition segments |
|---|---|---|---|
| Fly | 14 | 0.1% | 1 |
| Tern immunity | 1 | 32.9% | 1 |
| Marmot | 1 | 5.6% | 1 |
| Swift | 1 | 9.6% | 1 |
| Apollo | 1 | 65.7% | 3 |
| Tern navigation | 1 | 1.1% | 1 |
| Beetle | 1 | 0.2% | 1 |
| Chipmunk | 3 | 61.2% | 3 |
| Great tit | 1 | 5.0% | 1 |
| Turtle | 1 | 27.0% | 1 |
| Sheep breeding | 1 | 0.0% | 1 |
| Sheep weight | 1 | 9.4% | 1 |
| Bee | 7 | 1.0% | 1 |

The chipmunk's interval of 3 comes from seasonal sessions with irregular gaps (3, 7, 8 and 15 units). Its 61%
missingness and three segments are the automatic reading of an irregular design. A user who knows the true interval can
now set it.

**The 0.21.0 scripts re-run (`rerun_0_21_0.txt`):**

- Reproduced exactly:
  - The logic checks (38 of 38).
  - The binomial checks, identical to the 0.21.0 capture.
  - The curvature controls, identical.
- Re-ran with a different output: the decomposition truth check. Its output format has changed since `numeric2.txt` was
  captured, so its numbers are not compared line by line. Its readings are consistent with that capture: the
  decomposition is within 1.0-1.7% of the truth in every scenario, and the observed trajectory deviates most under
  intercept and combined selection.
- Problems with the 0.21.0 scripts themselves, none of them faults in the app:
  - `null_rates.py` cannot run, because it reads `null_randomslope.py`, which was never shipped.
  - The `emp_*.py` scripts use a relative path that only works from a different folder layout. They were run here with
    the path corrected.
  - `emp_defaults.py` reports "no fit" for the turtle example. Its Python port treats plastron length as categorical
    because 103 entries read "UNK". The app declares the covariate continuous (`cov_factor = character(0)`), so those
    entries become missing values and the model fits. A7 confirms the example loads.
  - `emp_crossed.py` was ended during its great tit refit; the examples before it completed.

## Part B: the 0.22.0 changes (`b_new.txt`)

**B1 Sampling interval.** Results from designs built to break the inferred interval:

| Design | Interval | Step used | Missing | Segments | Largest link (individuals) | Records off the grid |
|---|---|---|---|---|---|---|
| Annual, 20% with an extra mid-year record | inferred | 0.5 | 43.8% | 2 | 20 | 0 |
| | set to 1 | 1 | 0.0% | 1 | 300 | 61 |
| True half-yearly design | inferred and set agree | 0.5 | 0.0% | 1 | 300 | 0 |
| Biennial with some annual extras | inferred and set agree | 2 | 0.0% | 1 | 300 | 0 |
| Irregular, continuous ages | inferred | 0.996 | 6.4% | 0 | 0 | 0 |
| | set to 1 | 1 | 6.5% | 1 | 88 | 861 |

The first rows are the fault the setting was added to fix. The irregular design shows its other use: with continuous
ages no two records share an occasion, so the automatic decomposition is empty. Setting an interval gives a usable
decomposition, and the caution states how many records were left out.

Other results:

- An interval of 0.001 is coarsened, so the grid stays within its 300,000-cell cap.
- An interval longer than the age range returns an empty result without error.
- Zero, negative, missing and `NaN` entries fall back to the inferred interval.
- The server passes the interval to every grid, decomposition and prediction-age call, and meta carries it to the
  disappearance, life-table, terminal and integrity functions.
- The two remaining `infer_age_step()` calls in the server are the note and the prefill, which must show the inferred
  value.

**B2 Families.**

- Gamma, lognormal and beta are handled explicitly in fitting, in labels, in exported code and in the equation panel.
- Which examples could use them:
  - Gamma and lognormal: all six positive Gaussian traits (the tern immunity, tern navigation, swift, great tit,
    turtle and sheep weight examples).
  - Beta: none of the examples.
- AIC comparisons of densities of the same values pick the true family. On lognormal data, lognormal scores 11,430
  against Gamma 11,557 and Gaussian 13,335. On Gamma data, Gamma scores 11,264 against lognormal 11,438. This supports
  comparing the continuous families in the error-family check and in the stored-model sets.

**B3 Age-dependent variance.** Checks of the code:

- The dispersion formulas are correct.
- Families without a dispersion parameter ignore the option with a note.
- The non-linear exponential refuses the option.
- `bootstrap_test()` is untouched and always fits a homogeneous variance.

The fitter's calibration: across 300 null datasets, twice the Model 4 log-likelihood gain follows a chi-square with 3
df (KS p = 0.85; ΔAIC > 2 in 6.0% of datasets against 4.6% expected).

Simulations, 150 replicates of 150 individuals each:

| Scenario | Variance model | Model 4 preferred over Model 1 (ΔAIC > 2) | Age-dependent variance preferred |
|---|---|---|---|
| Constant variance, no selection | constant | 7% | |
| | age-dependent | 7% | 7% |
| Variance rising with age, no selection | constant | **21%** | |
| | age-dependent | **7%** | 100% |
| Variance rising, level-linked selection | either | 100% | 100% |

A residual variance that rises with age triples the rate at which Model 4 is wrongly preferred when there is no
selective disappearance. Modelling that variance brings the rate back to the nominal level. The option therefore
matters for the selective-disappearance reading, not only for residual fit.

One consequence to decide on: the null-model bootstrap always simulates and fits a homogeneous variance. With a
variance that rises with age, its null distribution is narrower than the data's, so it may be anticonservative. This was
not tested here and nothing was changed.

**B4 Stored models.**

- Sets split by data, analysed rows and likelihood type, and ΔAIC is computed within a set only.
- There are no Akaike weights.
- More than 26 sets are labelled (Z, then S27).
- A missing AIC is kept but not compared.
- The row key ignores row order and changes when one value changes or one record is dropped.
- The stored key includes every specification field, so the same model is never stored twice.
- On the great tit data, a quadratic Model 4 and a logarithmic Model 4 fitted to the same 7,126 records share a set.
  They differ by 9.6 AIC, which is the cross-function comparison the store was built for.

**B5 Share of wins.**

- Ties split the win.
- An individual missing any comparable function leaves the common set.
- A function estimable for fewer than half the individuals is excluded, and its share is reported as missing.
- On the seven Gaussian examples, fitted per individual with the polynomial and logarithmic functions, the shares sum
  to 100% in every case.

**B6 Peak and onset.**

- The turning-point rules hold on constructed curves:
  - quadratic: peak equals onset;
  - rise, plateau and late decline: onset at the late decline;
  - monotone decline: onset at or before the first age;
  - monotone rise: no final decline;
  - U-shape: an interior trough;
  - flat: no peak.
- Coverage: across 200 simulated datasets with a true peak of 5.0, the 95% limits from 1,000 draws of the fixed
  effects covered it 95.5% of the time (mean width 0.25).
- Great tit recruits, with the paper's covariates and an individual random intercept: the peak moves from 3.59 to 2.66
  once ALR is added. The published values are 3.45 and 2.80; REPLICATION.md reports 3.48 and 2.80 for the app with
  year and area random effects.

**B7 Random effects against lifespan.** Across 100 replicates each:

| Scenario | Model 1 r(random intercept, lifespan) | Share with p < 0.05 | Model 2 r |
|---|---|---|---|
| No selective disappearance | −0.010 | 5% | 0.000 |
| Level-linked selection | +0.470 | 100% | 0.000 |

The Model 2 column confirms the panel's note: once ALR is in the model, the random effects carry no association left
to show. On six Gaussian examples, Model 1 random intercepts against ALR give |r| ≤ 0.06, all non-significant.

**B8 Methods writer.** Checked by cross-reference, since the text itself was not generated:

- Every title prefix the writer looks for occurs among the server's saves.
- Every cited reference key is defined.
- `methods_context()` supplies every data, model and bootstrap field the writer reads.

**B9 Current-data box.** Every simulation setting the box reads has a default, every sampling design offered in the
simulator has a label, and the box is placed in the sidebar.

**B10 Wiring.** All 13 new inputs are created, all 16 new outputs are defined and placed, and all seven new observers
are guarded.

**B11 Unit-test expectations.** The step, decomposition and grid expectations of `test-0-22.R` hold for 20 seeds of
the test's design.

## Not covered, and open items

- **The R code was not executed.** In particular, the glmmTMB fits of the Gamma, lognormal and beta families and of the
  dispersion formula, the draws on the link scale for count and binomial families, and the rendering of the new panels
  are checked only through wiring, ports and likelihood logic. Run `Rscript tests/scripts/run_all.R` before release.
- **The random-effect covariance port covers random intercepts only.** Ageing-rate and curvature effects need random
  slopes, which the port does not fit.
- **Bootstrap under an age-dependent variance.** See B3; the bootstrap was left unchanged by decision.
- **Cosmetic.** `family_label()` returns the code itself for `binomial` and `betabinomial`, so those appear as written
  in tables and in the methods draft. This predates 0.22.0.
- **Data licences.** `inst/app/data/PROVENANCE.md` still lacks a licence and DOI for most datasets.
