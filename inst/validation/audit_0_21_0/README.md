# Pre-submission audit, disappR 0.21.0

This folder holds an independent audit: a static analysis of the package and a set of Python re-implementations that
check the statistics the app relies on, written without reference to the R implementation so that agreement between
the two means something. The package's own test suite (`tests/scripts/run_all.R`) covers the R code itself; the two
are complementary.

Captured output: `static.txt` (structure), `condscan.txt` (scope), `numeric1.txt` and `numeric2.txt` (statistics).
`scripts/` holds the Python used, so any result here can be reproduced or challenged.

## 1. Code and structure (`static.txt`)

| Check | Result |
|---|---|
| Parser self-test, R and Rmd syntax | 77 files, 0 errors, 0 lints |
| Calls against formals (unused, misnamed, missing arguments) | 0 errors, 0 warnings, 0 notes |
| App wiring: inputs read, outputs defined and placed, help panels, example columns | 144 inputs, 117 outputs, 54 panels, 13 examples, all resolved |
| Duplicate input/output ids; ids created in both ui.R and server.R | none |
| Individual-level conflicts in the bundled examples and stress data | none that would stop loading |
| Non-ASCII characters in `R/`; leftover `browser()`/`debug()` | none |
| Exported functions without an Rd page; Rd usage against formals | none; 11 usages match |
| Version in DESCRIPTION, NEWS and README | agree (0.21.0) |
| Assets referenced by the UI present in `www/` | yes |

## 2. Scope and conditional bugs (`condscan.txt`)

A scan of every `reactive`/`render*` block for names read but never assigned there: the failure mode that produced
the `multi_group` crash in 0.20.19. 160 blocks scanned; 64 candidates, all of them argument names (`drop = FALSE`,
`colour =`, `tables =`) or list fields, verified by hand. No unassigned reads remain. The engine keeps a regression
test that standardises data with no grouping column.

## 3. Pure helpers (`numeric1.txt`)

38 of 38 logic checks pass, covering every helper added since 0.20.9: curve clamping, the gap statistics, the
no-variance flags, bold terms, duplicate classification, multi-group naming, the supported-set reading, each model's
structural meaning, and the likelihood-ratio pair each term needs. Thresholds and regexes are read out of the R
source, so the port cannot drift from the code.

## 4. Statistics against the manuscript (`numeric2.txt`)

The app's decomposition algorithm, ported to Python and run on the manuscript's generating process (25 replicates,
ages with at least 20 survivors):

| Scenario | Observed vs true, age 25 / 35 | Decomposition vs true, 25 / 35 | Eq. 9 fit | Segments |
|---|---|---|---|---|
| No selective disappearance | +0.1% / +3.1% | −0.2% / −0.3% | 0.24% | 1.0 |
| Intercept +0.5 | +7.0% / +23.6% | −0.1% / −0.3% | 0.30% | 1.0 |
| Slope +0.5 | +3.2% / +14.6% | +0.7% / +3.1% | 0.25% | 1.0 |
| Shape −0.5 | −2.8% / −19.5% | −0.9% / −6.5% | 0.33% | 1.0 |
| Intercept +0.5, 50% MCAR | +7.0% / +22.2% | +0.2% / −0.1% | 0.36% | 1.6 |

This is Supplementary S1 reproduced from the app's own algorithm: the decomposition is exact under no selection and
under intercept-only selection, and biased only through slope or shape, growing with age (Eq. 15). Equation 9
predicts the observed deviation to within 0.36% of the mean trait throughout.

Independent validation: all 50 stored cells re-run and reproduce exactly.

## 5. Binomial traits (`numeric1.txt`)

Mapping the number of trials matters for precision, not bias: without it the standard error is about three times too
wide (0.109 against an actual spread of 0.044), and with equal trials the two agree to four decimals. Keeping two
records at one age and pooling successes and trials are equally precise; averaging the proportions is nearly twice as
noisy. With a proportion trait the naive model reverses the sign of the age slope (+0.19 against a true −0.30) and
adding ALR recovers it; the ALR × age term is recovered (+0.19 against +0.18), detected in every run, with a 5%
false-positive rate when absent. Beta-binomial data read as binomial give Pearson chi-square per degree of freedom
of 3.8 against 1.0 for clean data.

## 6. Random slopes and curvature (`scripts/curvature_controls.py`, `scripts/null_rates.py`)

The reason the app gained a slope for every age term in 0.20.28. With no selective disappearance but individuals
differing in curvature, the interaction model wins under a random intercept (mean AIC gap +18) and still wins under
a slope on age alone (+6 across 15 replicates); with a slope on every age term the advantage becomes −3.9 and Model 1
wins five times in six. Where there is no curvature heterogeneity the extra terms cost about 0.4 AIC and change
nothing. With real age-dependent selection, Model 4 still wins every replicate, the gap halving from 45 to 23 AIC:
the check removes the artefact without removing the finding.

## 7. Empirical examples against their papers (`scripts/emp_*.py`)

Twelve of the thirteen examples were read against their source paper; the chipmunk is documented from its
supplementary model tables. Every example loads, maps and fits under its default settings, and eleven of thirteen
reproduce the paper's direction. The two that do not are proxy substitutions, not faults: the swift's appearance
signal needs its supplied age at first reproduction rather than a derived one, and the beetle's ALR carries almost
nothing while its lifespan does.

Refitting with every random intercept the mapping specifies (individual plus year, track, cohort or area) changed two
readings, both towards the paper: the tern immunity ALR term falls from 2.2 to 1.5 standard errors, matching its
reported absence of selective disappearance, and the turtle's ALR × age falls from 3.4 to 1.4. Anything the app
reports uses the full random structure; the reduced fits in these scripts do not.

## 8. Text, flow and documentation

A sweep for sentences over 70 characters appearing more than once found 19, all deliberate: widget text repeated per
model, the trait-scale caution placed in each figure's help by design, and strings used in alternative branches of
one function. The 0.21.0 release removed the repetition that mattered: the random-slope caution now appears only in
the sensitivity check, the supported-set box no longer restates the ranking, the truth box keeps only its formula,
and the saved results carry the equation and a one-line reading rather than a paragraph.

Vignettes were checked against current behaviour, and two gaps closed in this pass: the methods vignette now
describes slopes for every age term and why a slope on age alone leaves curvature uncovered, and the workflow
vignette's wording for the multi-group flag was corrected.

## Scope

This audit checks structure, scope, the statistics and the empirical defaults. It does not replace the R test suite:
the unit tests, the app-server tests, the seven script tiers including the 0.20.9 to 0.20.19 sweep, `R CMD check` and
the golden reference are run with `Rscript tests/scripts/run_all.R`, whose output is the release gate.
