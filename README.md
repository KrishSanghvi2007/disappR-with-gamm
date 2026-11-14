<p align="center">
  <img src="inst/app/www/disappR_logo.png" width = "200"/>
</p>

<div align="center">
 <h1>disappR</h1>
</div>

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/license-MIT-green)](LICENSE.md)
<!-- badges: end -->

disappR is an R package providing a Shiny app for **diagnosing and modelling
selective disappearance and selective appearance** in longitudinal ageing data.

Most ageing studies want the average within-individual ageing pattern, but what
they actually have is population-level data on individuals followed over time,
and that data is almost never complete. Individuals die, emigrate or are simply
missed, and others only enter the study part-way through life. If the
individuals that leave early or arrive late differ in the trait being measured,
the population-level pattern stops reflecting how individuals age. Standard
analyses can then show senescence that isn't there, or hide senescence that is.

disappR works through this in the order you would want to: it first shows
whether selective disappearance or appearance is visible in your data, then
checks how complete your sampling is and what shape ageing takes, and only then
fits and compares a set of ten mixed models built to separate the two. Every
panel explains what it shows and how to read it, and everything you run can be
exported as R code.

## Features

- **Visual diagnosis before modelling.** Trait trajectories of lifespan and
  age-at-entry groups, how far apart those groups are at each age, the trait
  before death, selection differentials by age and the disappearance hazard.
- **Sampling and missingness checks.** When each individual was actually seen,
  how much is missing and at which ages, whether missingness tracks the trait,
  and which lifespan proxy to use.
- **Individual and population trajectories.** An ageing function fitted to each
  individual, the average within-individual trajectory rebuilt from those fits,
  and a comparison of ageing functions at both levels.
- **Ten comparative mixed models**, from a naive model to models for
  age-independent and age-dependent selective disappearance and appearance,
  with Gaussian, Gamma, lognormal, beta, Poisson, negative binomial,
  zero-inflated, binomial and beta-binomial families.
- **Checks on the fitted models.** Likelihood-ratio tests, residual diagnostics
  (DHARMa and performance), error-family and ageing-function checks, effect
  sizes and a null-model bootstrap for the lifespan effect.
- **Teaching simulations.** Generate data with a known true trajectory, choosing
  the ageing shape, the sampling design and the type and strength of selection,
  and see how close each model gets to the truth.
- **18 published datasets** from 16 studies of laboratory and wild populations,
  each set up to follow its paper (the great tit data open on laying date).
- **Reproducible output.** Downloadable R code for every section, HTML and text
  reports, the analysis data, and a reproducibility bundle of every setting used.
- **Runs locally.** Your data never leave your machine.

## Step 7: GAMMs (smooth models of ageing)

An optional seventh step fits generalized additive mixed models (GAMMs, with
`mgcv`) to the same data. The ageing trajectory is a smooth curve whose shape the
data choose, rather than a function you pick. It reads the data mapped on the Data
tab and changes nothing in steps 1-6.

- **Model:**
  - a smooth of age;
  - individual random intercepts, slopes or smooths;
  - mapped random intercepts and covariates;
  - ALR, lifespan and AFR as linear, smooth or age-dependent (`ti(age, proxy)`) terms;
  - an optional terminal-record effect;
  - nine error families, with `gam` or `bam` chosen by data size.
- **Results:**
  - the population curve with simultaneous intervals;
  - where the trait changes significantly with age, from the derivative;
  - peak and onset of decline;
  - how far the proxy terms move the curve;
  - Pedersen et al.'s (2019) group structures for sex or treatment;
  - basis, concurvity and residual checks;
  - R code to reproduce the fit.
- **Examples:** Tsui et al. (2026) and Brown et al. (2021) open with their
  published GAMM settings. `inst/validation/gamm/` replicates both from the
  authors' code.

GAMMs with many individuals are much faster with an optimised BLAS (for example
OpenBLAS on Linux, or the Accelerate BLAS that R for macOS can use). The app
switches to `mgcv::bam` automatically for large data.

## Installation

disappR is not on CRAN. Install the development version from GitHub:

```r
install.packages("pak")
pak::pak("EIvimeyCook/disappR")
```

The app needs `shiny`, `shinydashboard`, `ggplot2` and `lme4`, which are
installed automatically. The packages below are optional. Without them the app
still runs with fewer analyses, and the *Start here* page lists any that are
missing, apart from `callr`, which `run_app()` reports in the R console.

```r
install.packages(c("glmmTMB", "lmerTest", "nlme", "DHARMa", "performance", "see", "codetools", "callr", "mgcv"))
```

| Package | Enables |
| :------ | :------ |
| `glmmTMB` | All non-Gaussian families: count, proportion, binary, Gamma, lognormal and beta |
| `lmerTest` | p-values for Gaussian models |
| `nlme` | The non-linear exponential ageing function |
| `DHARMa` | Simulation-based residual checks |
| `performance`, `see` | Model diagnostics and their plots |
| `codetools` | Exported R code |
| `callr` | Runs the trajectory fits and GAMMs in a separate R process, so a failure there cannot stop the app |
| `mgcv` | Step 7: GAMMs |

## Usage

Video walkthrough [here](https://osf.io/kevnm/files/m6utp).

```r
library(disappR)
run_app()
```

This opens the app in your browser or the RStudio viewer.

If you would rather not install the package, you can run the app straight from a
downloaded copy of the repository:

```r
shiny::runApp("path/to/disappR/inst/app")
```

## Basic workflow

The app is laid out as six numbered steps in the left-hand sidebar, plus an
optional seventh (GAMMs; see above). You can move between them freely, but each
builds on the one before.

**Start here.** An overview of the problem and the workflow. Under *Choose how
to start*, the three buttons take you to the Data step with simulated data (where
the answer is known), the published examples, or the upload option selected.
This page also lists which optional packages are installed (apart from `callr`).

1. **Data** — choose a data source: simulated teaching data, one of the 13
   published datasets, or your own CSV (one row per individual per sampling
   occasion). Then map your columns: individual ID, age and trait are required,
   and lifespan, age at first observation, covariates, groups and censoring are
   optional (see [Preparing your data](#preparing-your-data)). The *Data
   integrity checks* panel flags anything that looks wrong, such as repeated
   records, IDs shared between groups, or an individual whose lifespan differs
   between its rows. You can also subset the data and look at the distribution
   of any variable.
2. **Visual diagnosis** — does the phenotype of long- and short-lived (or early-
   and late-entering) individuals differ, and does that difference change with
   age? A constant gap between groups points to age-independent selection; a gap
   that widens or narrows with age points to age-dependent selection. Further
   panels look at the trait just before death, whether survivors differ from
   those that disappear at each age, and the age-specific disappearance hazard.
3. **Missingness and proxies** — a grid of when each individual was actually
   seen, how much is missing at each age, and whether what is missing is related
   to the trait. This step also compares age at last record (ALR) and individual
   mean age as proxies for lifespan, which tells you which of the models in step
   5 to trust.
4. **Trajectories** — fits linear, quadratic, cubic, logarithmic, asymptotic and
   exponential functions to each individual, rebuilds the average
   within-individual trajectory from those fits, and compares the functions at
   the individual and population level. Use the best-supported function as the
   ageing function in step 5.
5. **Modelling** — choose the error family, random effects (intercepts, or
   correlated or uncorrelated random slopes), the ageing function and any
   covariates, tick the models you want, and press *Fit models*. The results
   show which selective processes the data support, likelihood-ratio tests
   between nested models, coefficients in original units, predicted population
   trajectories, and a set of checks on the fits.
6. **Summary and report** — every result you saved with a *Save to summary*
   button is collected here, along with an evidence summary that grades what
   the saved results suggest about selective disappearance and appearance. Export
   it as an HTML report (with plots) or a text report, alongside the R script,
   the analysis data and a reproducibility bundle.

> **Tip.** Start with the simulated data before loading your own. The simulated
> datasets make the patterns deliberately obvious, and on the Modelling tab the
> app scores each model against the true trajectory, so you can see what each
> diagnostic looks like when you already know the answer.

> **Tip.** Click the ⓘ next to any panel title for what it shows, how to read it
> and what to watch out for. The ⓘ next to each model in the model settings gives
> its exact specification.

## Preparing your data

Data should be a CSV with **one row per individual per sampling occasion**.

| Column | Required? | Notes |
| :----- | :-------- | :---- |
| Individual ID | Yes | Can be nested within a group (for example family), and within a second level above that |
| Age | Yes | Numeric, in any unit; can be rounded to a set resolution |
| Trait | Yes | Continuous, count, proportion or binary (0/1) |
| Age at last record (ALR) | No | If not mapped, the oldest age at which the individual has a trait value (untick *Trait-specific ALR/AFR* to use any record) |
| Known lifespan (LS) | No | Map only if lifespan is truly known; enables Model 6 |
| Age at first observation (AFR) | No | If not mapped, the youngest age at which the individual has a trait value (same option); enables Models 7–10, which need AFR to vary among individuals |
| Censoring | No | Marks individuals still alive at the end of the study |
| Covariates | No | Continuous or categorical, with optional interactions with age or with each other |
| Group / random effects | No | Grouping variables and additional random intercepts |
| Number of trials | No | For proportion traits with the binomial families (chosen on the Modelling tab) |

Each individual must have a single ALR, lifespan and AFR. If a mapped column
gives an individual different values in different rows, loading stops and names
the individuals involved; you can then fix the data or tick the box to exclude
them. Repeated records of the same individual at the same age are kept by
default. You can choose to collapse exact duplicates (rows identical in every
mapped column); rows that differ in anything are always kept, and nothing is
averaged.

## The models

With `b(age)` the chosen ageing function (for example age + age²), `mean(age)`
an individual's mean age and `Δb(age)` its within-individual deviation:

| Model | Fixed effects | What it tests |
| :---- | :------------ | :------------ |
| 1 | b(age) | Naive model: selection not accounted for |
| 2 | b(age) + ALR | Age-independent selective disappearance |
| 3 | mean(age) + Δb(age) | Age-independent selective disappearance, by mean-age centring |
| 4 | b(age) × ALR | Age-dependent selective disappearance (ALR) |
| 5 | mean(age) × Δb(age) | Age-dependent selective disappearance (mean age) |
| 6 | b(age) × LS | Positive control when lifespan is known |
| 7 | b(age) + ALR + AFR | Age-independent disappearance and appearance |
| 8 | b(age) × ALR + b(age) × AFR | Age-dependent disappearance and appearance |
| 9 | b(age) × ALR + AFR | Age-dependent disappearance, age-independent appearance |
| 10 | b(age) + ALR + b(age) × AFR | Age-independent disappearance, age-dependent appearance |

Mapped covariates enter every model, and all models are fitted by maximum
likelihood to the same rows, so their AICs can be compared directly. The
*models-and-methods* vignette gives the full specification of each.

## Teaching simulations

The simulator generates longitudinal data with a known true ageing trajectory.
You can set the trait and its distribution (body mass, or fecundity as Poisson,
negative binomial or zero-inflated counts), the ageing form and a biological
scenario within it (for example slow or fast senescence, an early or late peak,
or terminal investment), mean lifespan, individual variation in ageing rates,
the type and direction of selective disappearance and appearance, the sampling
design (including several kinds of missingness), a diet covariate, families as
a nested random effect, the number of individuals and the seed. Each simulation
lists what you should expect to see, and the simulated data can be downloaded
as a CSV.

## Example data

Thirteen published datasets are bundled. Each opens with settings that follow
its paper as closely as the app allows, except the great tit data, which open on
laying date (the trait analysed in Sanghvi et al. 2026) rather than the traits in
Bouwhuis et al. 2009:

- Sanghvi et al. 2025, *American Naturalist* — *Drosophila melanogaster* daily fecundity
- Sanghvi et al. 2022, *Evolution* — seed beetle female fecundity
- Bichet et al. 2022, *Journal of Animal Ecology* — common tern immune parameters
- Bichet et al. 2022, *Ecology and Evolution* — Alpine marmot immune parameters
- Wynn et al. 2025, *Journal of Animal Ecology* — common tern navigational efficiency
- Moullec et al. 2023, *Frontiers in Ecology and Evolution* — Alpine swift reproduction
- Pásztor et al. 2022, *Ecology and Evolution* — clouded Apollo butterfly body size
- Allain et al. 2023, *Oikos* — eastern chipmunk reproduction
- Bouwhuis et al. 2009, *Proc. R. Soc. B* — great tit laying date (opens here), recruit production, clutch size, brood size and fledglings
- Warner et al. 2016, *PNAS* — painted turtle reproduction
- McKenna-Ell et al. 2023, *Biology Letters* — Soay sheep breeding probability and offspring survival
- McKenna-Ell et al. 2023, *Biology Letters* — Soay sheep offspring birth weight
- Szejner-Sigal et al. 2025, *Proc. R. Soc. B* — alfalfa leafcutting bee locomotor activity

Sources and licence details for each dataset are listed in
[`inst/app/data/PROVENANCE.md`](inst/app/data/PROVENANCE.md).

## Scripted use

The functions behind the app can also be used directly in R scripts:

```r
library(disappR)

sim <- disappr_simulate(n_id = 150, seed = 1, sd_type = "both")
map <- disappr_mapping(id = "ID", age = "age", trait = "body_mass", life = "lifespan")
x   <- disappr_prepare(sim, map)
fit <- disappr_fit(x, models = c("M1", "M2", "M4"), age_function = "Quadratic")

disappr_compare(fit)          # AIC table
disappr_predict(fit, "M4")    # predicted population trajectory
cat(disappr_code(fit, x))     # R code that reproduces the analysis
```

The exported script reads `your_data.csv` by default, so write your data to that
file or pass a name as the third argument. Its figure section expects at least
one mapped covariate; without one, run the model-fitting part of the script on
its own.

| Function | Purpose |
| :------- | :------ |
| `disappr_read()` | Read a CSV or other delimited file |
| `disappr_example()` | Load a bundled dataset with its published settings |
| `disappr_simulate()` | Simulate data with a known trajectory |
| `disappr_mapping()` | Say which column holds which variable |
| `disappr_prepare()` | Check and standardise the data |
| `disappr_fit()` | Fit and compare Models 1–10 |
| `disappr_compare()` | The AIC table |
| `disappr_predict()` | Predicted population trajectory for one model |
| `disappr_diagnose()` | Fit validity, dropped rows and notes |
| `disappr_interpret()` | Interpretation notes |
| `disappr_code()` | A standalone R script for the analysis |
| `disappr_provenance()` | Every setting and package version used |

`?disappr_api` documents the arguments of each.

## Getting help

- **Inside the app**, every panel has an ⓘ button explaining what it shows and
  how to read it.
- **The video walkthrough** on [OSF](https://osf.io/kevnm/files/m6utp) shows the
  app in use.
- **The vignettes** cover the bigger picture: an overview of the package, a
  tab-by-tab walkthrough of the workflow, the models and methods, how the app
  was validated against independent code and the published analyses, and the
  test suite. `pak` does not build vignettes, so to use
  `browseVignettes("disappR")` install with:

  ```r
  install.packages(c("remotes", "knitr", "rmarkdown"))
  remotes::install_github("EIvimeyCook/disappR", build_vignettes = TRUE)
  ```

  This needs pandoc, which comes with RStudio. The sources can also be read in
  the [`vignettes/`](vignettes) folder on GitHub.

## Tips and limitations

- An association between missingness and an observed variable cannot show
  whether data are missing completely at random, at random, or not at random.
- AIC measures relative support among the models fitted, not causation. Check
  the model comparison against the visual diagnosis and the residual checks
  before drawing conclusions.
- Mean age is a poor proxy for lifespan when sampling is incomplete. Step 3 tells
  you whether this applies to your data.
- The evidence summary on step 6 is deliberately cautious and only uses results
  you have saved.

## Bug reports and contributions

Please file issues and feature requests at
<https://github.com/EIvimeyCook/disappR/issues>. Pull requests are welcome.

## Related tools

- [**READMEBuilder**](https://github.com/EIvimeyCook/READMEBuilder) — document
  your finished analysis for archiving
- [**DCQC**](https://github.com/SORTEE/DCQC) — data and code quality control
  for ecology and evolutionary biology

## Citation

If disappR helps with your work, please cite:

> Sanghvi, K., & Ivimey-Cook, E. R. (2026). *disappR: a shiny app to model
> ageing and selective [dis]appearance.* R package.
> <https://github.com/EIvimeyCook/disappR>

and the paper describing the methods:

> Sanghvi, K., Ivimey-Cook, E. R., Bouwhuis, S., Sepil, I., & van de Pol, M.
> (2026). Biases in common methods used to quantify ageing and selective
> disappearance: a guide. *Methods in Ecology and Evolution*.

Data, code and supplementary material for the paper are on
[OSF](https://osf.io/kevnm/). `citation("disappR")` gives both references, and a
machine-readable [`CITATION.cff`](CITATION.cff) is included, so GitHub's "Cite
this repository" button gives formatted APA and BibTeX.

## Contact

Edward R. Ivimey-Cook — <e.ivimeycook@gmail.com> —
[ORCID 0000-0003-4910-0443](https://orcid.org/0000-0003-4910-0443)

## License

The disappR code is released under the [MIT License](LICENSE.md). The bundled
datasets are not covered by this licence: they are redistributed under the terms
of their sources, listed in
[`inst/app/data/PROVENANCE.md`](inst/app/data/PROVENANCE.md). Cite the original
paper and dataset when you use one.
