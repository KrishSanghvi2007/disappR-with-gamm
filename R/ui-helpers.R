# disappR engine - User-interface helpers and help text for the Shiny app ('i' help entries, info titles, save buttons).
# Engine code shared by the app and the R interface; changes that alter results must pass the golden tests.

#' @importFrom shiny actionButton actionLink column downloadButton fluidRow icon showNotification verbatimTextOutput
#' @importFrom htmltools tags div p h5 strong tagList span
#' @importFrom shinydashboard box
#' @noRd
NULL

optional_packages_ui <- function(st = OPTIONAL_STATUS) {
  tags$table(class = "table table-condensed optional-pkgs", tags$tbody(lapply(seq_len(nrow(st)), function(i) {
    tags$tr(tags$td(strong(st$Package[[i]])),
            tags$td(if (isTRUE(st$Installed[[i]])) span(class = "pkg-ok", paste("\u2713", st$Version[[i]])) else span(class = "pkg-missing", "\u2717 not installed")),
            tags$td(st$Needed_for[[i]]))
  })))
}

# UI helpers shared by ui.R and server.R (objects defined in ui.R are not visible to the server)
sign_choices <- c("\u2212 (negative)" = "-1", "0 (none)" = "0", "+ (positive)" = "1")

box_note <- function(...) div(class = "small-note", ...)

# Columns usable for subsetting: text columns with 2-50 levels or numeric columns with 2-10 distinct values.
subset_candidates <- function(df) {
  if (is.null(df) || !ncol(df)) return(character(0))
  names(df)[vapply(df, function(x) {
    v <- x[!is.na(x)]
    if (!length(v)) return(FALSE)
    nu <- length(unique(v))
    num <- safe_numeric(v)
    if (mean(is.finite(num)) >= 0.95) nu >= 2 && nu <= 10 else nu >= 2 && nu <= 50
  }, logical(1))]
}

# ---------------------------------------------------------------------------
# "i" help content: what each section does, how to use and read it, and cautions
# ---------------------------------------------------------------------------
# An info panel. what/how/caution are the plain statement a user needs to use and read the output;
# `more` carries the mechanics, formulae and edge cases, shown only when the reader opens "More info".
info_entry <- function(title, what, how, caution, more = "")
  list(title = title, what = what, how = how, caution = caution, more = more)

INFO <- list(
  overview = c(info_entry("The workflow", "", "", ""),
    list(lead = "Follow tabs 1\u20136 to diagnose the dataset and interpret and quantify the pattern of ageing and selective processes. Click on SAVE TO SUMMARY for every result that you are happy with and wish to include in your manuscript: these are collected at the end in an HTML report, along with the associated R code to reproduce them.")),
  quickstart = info_entry("Quick start",
    "Loads a simulated teaching dataset (where the answer is known) or one of the bundled published datasets.",
    "Simulated data show textbook patterns with a known truth; the fly data are a real reanalysis with count data, covariates, nesting and censoring. Saved results are collected on the 'Summary and report' tab.",
    "Teaching simulations use deliberately strong effects."),
  data_source = info_entry("Data source and simulation",
    "Loads simulated data with a known answer, one of 13 empirical examples from 12 published studies, or your own CSV (one row per individual \u00d7 age).",
    "With the simulator, change one setting at a time, press 'Simulate & use this dataset', and see how the diagnostics and model rankings respond. Examples open with settings close to their published analysis.",
    "For your own CSV, age must be numeric; missing values can be blank or NA.",
    more = "The examples cover laboratory systems (fruit-fly and seed-beetle fecundity, leafcutting-bee activity) and wild populations (tern immunity and navigation, painted turtle reproduction, great tit recruitment, chipmunk reproduction, Soay sheep reproduction and weight, marmot immunity, swift reproduction, Apollo body mass). The simulator draws trajectories from an ageing form (linear, quadratic, cubic, logarithmic, exponential) and a shape within it (slow, fast or no senescence; an early or late peak; decline then improvement; a change that levels off). Selective disappearance can be absent, age-independent (lifespan linked to the individual's level), age-dependent (linked to its rate of ageing) or both, positive or negative; with individual AFR, selective appearance is set the same way. Mean lifespan sets how many occasions each individual supplies. Diet lowers the trait; families add a nested random effect. Use 'Subset the data' to analyse one group."),
  afr_expression = info_entry("Counting missed occasions: from AFR or from AFE",
    "Sets where each individual's expected-occasion window opens. 'Age at first record (AFR)' opens each individual's window at its AFR, the one the models use, and counts the AFR itself. 'Age at first trait expression (AFE)' is the age from which the trait exists and could in principle have been measured, entered as one age that applies to every individual: body size is expressed from birth even if recording begins at first breeding. The window then opens at the earlier of the AFE and the individual's AFR.",
    "Use AFR when the trait genuinely begins when the individual enters the data (a first clutch, for example). Use AFE when individuals could have been measured earlier than they were, and give the age at which the trait starts.",
    "This setting changes the missingness figures only. It affects this tab's grid, missed-occasion counts and coverage; it does not enter any model, proxy or statistic anywhere in the app, all of which use AFR. Counting from AFE usually raises the estimated missingness, sometimes substantially, and changes its age profile at young ages, so the two definitions are not comparable: say which one you used when reporting."),
  afr_alr = info_entry("Agreement between AFR and ALR",
    "Correlation between each individual's age at first observation and its age at last record, with the mean and SD of the observation window (ALR \u2212 AFR). The figure shows the dashed 1:1 line and the linear regression of ALR on AFR (solid black, with its 95% band).",
    "Under a fixed entry age, AFR does not vary and the correlation is undefined. A correlation near zero means entry and exit are independent, so ALR reflects lifespan rather than when an individual was first seen. A strong positive correlation means individuals that enter late also leave late, so the two proxies carry overlapping information and AFR terms (Models 7-10) and ALR terms (Models 2, 4) compete for the same variance.",
    "Missingness alters both AFR and ALR across individuals: a missed first occasion pushes AFR later, a missed final occasion pulls ALR earlier, so neither proxy is observed cleanly when sampling is incomplete. A high correlation can arise from biology (late starters live longer) or purely from the study design (a short study window forces late entrants to have late exits), and this diagnostic cannot separate the two. With few individuals the correlation is unstable. Mean age is affected by both ends of the window, so a variable AFR degrades it as a proxy of lifespan more than it degrades ALR."),
  among_order = info_entry("Higher-order among term",
    "Controls how the among-individual terms enter when the ageing function is quadratic or cubic. Linear is the default and matches the manuscript: mean age, ALR, LS and AFR each enter once.",
    "Use a higher-order term when you expect the among-individual effect itself to be curved \u2014 for example when mid-lived individuals differ from both the shortest- and the longest-lived, which a single linear term cannot show. 'Consistent decomposition' is the option to use for the centring models: it adds the individual mean of age\u00b2, which pairs exactly with the within-individual \u0394(age\u00b2) term so that the two reconstruct age\u00b2.",
    "'Powers of the mean' squares the mean age instead. That mirrors ALR + ALR\u00b2 and is fine for the ALR, LS and AFR models, but in the centring models it does not pair with \u0394(age\u00b2): the two differ by the within-individual spread of sampled ages, which is larger in individuals observed longer. Higher-order terms also cost degrees of freedom and need enough distinct ages per individual to be estimable.",
    more = "For a quadratic basis the age term splits as age\u00b2 = mean_i(age\u00b2) + [age\u00b2 \u2212 mean_i(age\u00b2)]. The second part is the within-individual term the app always uses (delta_f2). 'Consistent decomposition' supplies the first part, so among + within reconstructs age\u00b2 exactly. 'Powers of the mean' supplies (mean age)\u00b2 instead, and mean_i(age\u00b2) \u2212 (mean age)\u00b2 = Var_i(age), so a term correlated with how long each individual was observed is left out of the model. The two options coincide only when every individual is observed at exactly the same set of ages, when Var_i(age) is constant and the intercept absorbs it. That is rarer than it sounds: even with complete sampling from age one, individuals differ in lifespan, so Var_i(age) still varies and correlates about 0.98 with age at last record."),
  duplicates = info_entry("Repeated ID \u00d7 age records",
    "What to do when an individual appears more than once at the same age. 'Keep all' (default) keeps every row; 'Collapse exact duplicates' keeps one of the rows that agree in every mapped column.",
    "Rows that differ in anything (trait, trials, a covariate, the group) are separate measurements and are always kept. The data checks list them and the columns that differ: fix a typo in the ID or age, map the grouping column if IDs are reused across groups, or combine the rows yourself.",
    "Nothing is averaged for you: averaging would invent values, for example a mean proportion beside a number of trials it never came from."),
  mapping = info_entry("Map columns",
    "Tells the app which columns are the individual, the age and the trait, and optionally lifespan, entry age, covariates and extra random effects.",
    "The three required columns must be different. Age must be numeric. Map lifespan when you know it: it makes Model 6 available as a benchmark.",
    "Getting this wrong invalidates everything downstream. Non-numeric values in a continuous covariate are treated as missing and drop those rows.",
    more = "Tells the app which columns hold ID, age and trait, plus optional ALR, lifespan, AFR (age at first observation), continuous and categorical fixed-effect covariates, their interactions, grouping, additional random intercepts, censoring and an optional age resolution. Uploaded columns keep their names. Continuous covariates (e.g. temperature) enter the models as linear effects; categorical covariates (e.g. treatment, sex, diet) enter as factors with one coefficient per level. Interactions can be a covariate \u00d7 age (the covariate changes the shape of the ageing trajectory, e.g. diet \u00d7 (age + age\u00b2)) or between two covariates (e.g. diet \u00d7 sex, added to every model with both main effects). Covariates and interactions are used in the mixed models on tab 5 (Modelling). Map lifespan (LS) only if it is truly known; use nesting when IDs repeat across groups; map a censoring column for individuals alive at the end of the study. With a 'next level up' column the random effects have three levels, e.g. individuals within fathers within families: (1 | family) + (1 | family:father) + (1 | family:father:ID)."),
  trait_ages = info_entry("Trait-specific ALR/AFR (default)",
    "When ALR and AFR are not supplied as columns, they are calculated for each individual as the last and first ages at which the trait has a value (zero is a value; NA is not). Unticked, they are the last and first ages of any record, with or without a trait value.",
    "Example: ages 1\u20135 with trait values NA, 5, 7, 6, NA give AFR 2 and ALR 4 when ticked, and AFR 1 and ALR 5 when unticked. Unticked assumes that an individual with a record at an age was alive then, even if the trait was not recorded.",
    "Bundled examples open unticked, because their documented results use every record; ticking the box can change them. Mean age is always calculated from the ages with a trait value.",
    more = "Missingness is counted from AFR to ALR, both included, so a record without a trait value inside that window counts as a missed occasion. ALR or AFR columns chosen in the mapping are used as supplied, whichever option is chosen. The disappearance hazard and the life table always use each individual's last record of any kind."),
  age_round = info_entry("Round ages",
    "Places every record on the nearest multiple of the value entered, for example 1 (year) or 7 (days), so that irregular ages (from dates, for instance) share a common sampling schedule. Leave it blank when individuals were sampled on a schedule.",
    "Enter the sampling resolution. Rounding happens before anything else, so ALR, AFR, mean age, the sampling grid and every model use the rounded ages.",
    "Records that round to the same age for one individual are not averaged: they stay separate records, which every model uses (as repeated measures at that age), while the decomposition, the age-specific means and the sampling grid use the individual's mean at that age. The data checks count these repeats; 'Repeated ID \u00d7 age records' decides whether exact duplicates are collapsed.",
    more = "Coarse rounding merges nearby ages and can hide within-individual change between them; very fine rounding leaves irregular ages and makes the sampling grid count many unobservable occasions as missed."),
  subset = info_entry("Subset the data",
    "Restricts every analysis to the rows whose value of one categorical variable (a text column with up to 50 levels, or a numeric column with up to 10 distinct values) is among the chosen levels.",
    "Pick the variable, then tick the levels to keep (for example females only, or one treatment). Leave the variable empty to use all rows. The subset applies to the integrity checks, all diagnostics, individual fits and models, and to the exported R code only through the data you supply.",
    "A subset changes every result, including the model ranking: report which subset you analysed."),
  integrity = info_entry("Data integrity checks",
    "Checks the data for problems that change results, from duplicate records to ages that look like calendar years.",
    "Fix every 'Warning' row (in the data or the mapping) before modelling; 'Note' rows are information. The suggested error family is a starting point.",
    "These checks cannot find every recording error. Infinite values are kept but distort every summary they enter.",
    more = "Checked: duplicate records, IDs in several groups, missing or impossible lifespans, censoring and the sampling schedule (step, off-schedule rows, staggered or irregular schedules). Also: few records per individual, AFR variation and its correlation with ALR, missing-value codes (-99, -999, -9999), ages at or below zero, problem covariates, empty columns, extreme trait values and the trait distribution. The sampling step is the most common interval between an individual's consecutive records."),
  distribution = info_entry("Distribution of a variable",
    "Histogram (numbers) or bar chart (categories) of any column, per row or per individual.",
    "Look for skew, zeros, outliers and unexpected categories before choosing an error family or covariates.",
    "It shows the data as loaded (after any subset on the Data tab): the observed records only. For simulated data these are the records left after the simulated missingness has removed missed occasions, with no records after death, not the complete simulated population. 'Per individual' averages the raw ID column, so individuals nested in groups with repeated IDs are merged here."),
  visual_settings = info_entry("Set the number of bins and the selection term of interest",
    "The bins set here determine how the plots that diagnose selective processes are stratified. Choose the grouping variable, the number of bins, the panels and the trait scale.",
    "Choose ALR, mean age or LS for selective disappearance, AFR for selective appearance. By default the number of bins is half the average number of time steps per individual, within what the data allow. Fewer bins put more individuals behind each point and give a steadier pattern; more bins resolve it more finely, with fewer individuals per point.",
    "Every bin, bin \u00d7 age point and age bin needs at least three individuals, so sparse bins are not drawn; with no more distinct values than bins, each value is its own bin. Use the trait scale suggested above the figures (raw for continuous traits, log(trait + 1) for counts): on the wrong scale, group differences in every figure below can shrink or reverse.",
    more = "The variable that groups individuals (ALR, mean age, LS or AFR), the number of bins, panels and the trait scale, applied to both lifespan-group figures. Settings that affect a single figure sit just above that figure. The grouping variable forms the bins of the trajectory plot and the x-axis of the lifespan plot; the number of bins sets the number of groups in the trajectory plot and of age bins in the lifespan plot. Choose ALR, mean age or LS to diagnose selective disappearance and AFR to diagnose selective appearance. Every bin \u00d7 age point in the trajectory plot and every age bin in the lifespan plot needs at least 3 individuals, so sparse bins are not drawn. With no more distinct values than bins, each value is its own bin, and the same bin boundaries are used in every panel. Increase the number of bins to resolve finer differences, but not so far that a bin holds too few individuals: a useful starting point is the number of time steps divided by three. Few bins hide patterns, many bins give noisy points. Facets split the sample, so each panel has fewer individuals. A time-varying covariate is summarised by each individual's most common value. On the log scale, differences are ratios."),
  a1 = info_entry("Trajectories within bins",
    "Groups individuals by lifespan proxy (ALR, LS, mean age or AFR) and plots the mean trait against age for each group. Grey circles are the mean of all individuals at each age, sized by sample size.",
    "Overlapping bands: no selective disappearance. Parallel, offset bands: age-independent selection. Bands that diverge or converge with age: age-dependent selection. Use AFR groups for selective appearance, and compare panels to see whether the pattern differs between groups.",
    "Few individuals contribute at old ages, so late points are imprecise, and bands can overlap by chance while selection is present. Non-linear lifespan effects are not shown.",
    more = "Each individual is counted once; points need at least 3 individuals. Bins are quantiles by default (about equal numbers per bin; tied lifespans can merge bins) or equal width (sparse outer bins). Error bars are \u00b11 SE. The manuscript describes several forms selective disappearance can take (selection gradients that change with age, early- versus late-life trade-offs, pace-of-life differences, a weakening of selection, terminal investment, condition-dependent survival costs); these figures let you assess them qualitatively. The figure beside this one quantifies the bin differences."),
  a1_diff = info_entry("How big is the gap between lifespan groups at each age?",
    "Plots the gap between neighbouring lifespan groups at each age (higher bin minus the next lower bin), with least-squares trend lines and 95% bands.",
    "Differences near zero with flat lines: no selection. Flat lines away from zero: age-independent selection. Rising or falling lines: age-dependent selection. Tick 'Inverse-variance weights' to let precise differences count more; one line per bin pair shows whether particular bins drive the pattern.",
    "Each point rests on two bins, so it is noisier than the trajectory figure, and late ages rest on few individuals.",
    more = "A point appears only where both bins have at least 3 individuals. Without weights, each plotted difference counts as one point, so slopes and bands are descriptive; a line through two points has no band. The saved table reports each line's slope per unit age."),
  a2 = info_entry("Does the trait track lifespan, and does that change with age?",
    "Plots the trait against the lifespan proxy separately within each age bin.",
    "A slope that is similar in every age bin indicates an age-independent association. A slope that changes across age bins indicates an age-dependent one. The caption under the figure gives the slope in each age bin, its change from the previous bin, and whether the slope changes with age (a trend weighted by the inverse variance of each slope, with its p-value).",
    "For count traits the slope can change through scale alone: compare with the log scale before concluding.",
    more = "Each individual's mean trait within an age bin, plotted against its lifespan (or ALR / AFR), with a straight-line fit and its 95% confidence band per age bin; age bins need at least 3 individuals. The caption lists the regression coefficient (slope) in each age bin and its change from the previous bin, and tests the trend of the coefficient across age by weighted least squares, each coefficient weighted by the inverse of its squared standard error. Saving the figure also stores each coefficient's 95% CI, the correlation r and its p-value. Coefficients near zero in every bin: no selection. Similar non-zero coefficients in every bin: age-independent selection. Coefficients that change across consecutive age bins: age-dependent selection. Untick 'Show individual points' above the figure to see only the fitted lines, which makes differences in slope between age bins easier to judge. Older age bins contain only long-lived individuals, so their lifespan range is narrow and coefficients are imprecise (wide CIs). When count means and variances fall with age, coefficients can change through scale alone: compare with the log scale or r. The age-specific coefficients share individuals, so the trend across ages treats correlated estimates as independent."),
  a5_terminal = info_entry("Do individuals change in their final occasions?",
    "Aligns individuals on occasions before death (or their last record) instead of age, and plots the mean trait for the shortest-, middle- and longest-lived thirds.",
    "A fall (terminal decline) or rise (terminal investment) in the final occasions shows change linked to approaching death, separate from ageing. A similar change in every lifespan group suggests age-independent selective disappearance; a larger one in one group suggests age dependence.",
    "Terminal investment can look like selective disappearance: judge which is biologically plausible, using the other diagnostics and the models.",
    more = "0 is the last occasion. Short-lived individuals reach their final occasions young and long-lived ones old, so the same terminal change removes different individuals at different ages."),
  a6_selection = info_entry("Do survivors differ from those that disappear?",
    "At each age, the difference in mean trait between individuals that survive to their next occasion and all individuals present (or those that disappear), in within-age SD units.",
    "A point whose interval excludes zero suggests selective disappearance at that age; a trend across age suggests its strength depends on age.",
    "This describes selection on the observed trait, not its cause. Ages with few survivors or disappearances are dropped.",
    more = "Intervals are 95%; the trend across age is weighted by precision."),
  a7_hazard = info_entry("Disappearance hazard",
    "Shows the probability of disappearing before the next occasion at each age, against the overall age-independent rate (dashed line).",
    "Points above the line mark ages where individuals disappear faster than average, points below where they disappear more slowly. A rising series indicates actuarial senescence, so later ages are thinned most and selection acts hardest there. A falling series means early losses dominate.",
    "With unknown lifespan, disappearance includes missed detection and emigration. Old ages rest on few individuals, so intervals are wide.",
    more = "At each age, the probability of disappearing before the next occasion (with 95% Wilson intervals, point size = individuals at risk), the overall, age-independent hazard (dashed line: all disappearances / all individual-occasions at risk), and a likelihood-ratio test of a constant hazard against age-specific hazards. The test is fitted on individual occasions with an individual random intercept (frailty) when that can be estimated. The dashed line is the overall hazard: the share of all individual-occasions that ended in disappearance, ignoring age. Points above it mark ages where individuals disappear faster than average, points below it ages where they disappear more slowly. A flat series along the dashed line is consistent with and indicative of a chance of disappearing that does not change with age. A series rising across age is consistent with and indicative of actuarial senescence, and means later ages are thinned most, so any selective disappearance acts hardest there. A falling series means the opposite: early losses dominate, and the survivors reaching old age are already a filtered set. With unknown lifespan, disappearances include missed detections and emigration, and individuals alive at the study end must be marked as censored. Hazards at old ages rest on few individuals (wide intervals). The hazard is fitted with ordinary binomial GLMs on individual \u00d7 occasion records: the discrete-time likelihood is standard, but unmodelled differences in frailty between individuals are not accounted for."),
  sampling_guidance = info_entry("Missingness settings",
    "Sets how missed occasions are counted and how the sampling grid is drawn. Missed occasions are counted only from each individual's AFR to its ALR, both included, or from the age at first expression when that option is chosen.",
    "Read the detection and proxy figures above alongside the proxy-agreement plots. In the sampling grid, ordering by ALR or AFR uses the ALR and AFR the models use, which for simulated data are taken after missingness.",
    "In the manuscript we use 'missingness' for two things: occasions missed between AFR and ALR, and the occasions between ALR and true lifespan, at which an individual was alive but no longer recorded. Only the first is calculable from data alone, because the steps between ALR and LS are latent whenever lifespan is unknown. This grid therefore measures missingness between each individual's first and last record, so occasions missed after the last record are invisible to it and the detection rate reported here is an upper bound. The missingness grid assumes regular, scheduled sampling occasions: it is most reliable when every individual is due to be sampled at a common interval. With irregular ages (for example ages calculated from dates) expected occasions are approximate, so missingness percentages, patterns and drivers should be read qualitatively; round ages to a sampling resolution on the Data tab. Without known lifespan, occasions missed after the last record are invisible, so detection is overestimated. If individuals genuinely follow different schedules (for example some sampled every year and others every third year), the common step is the shortest one and the less frequently sampled individuals appear here as heavily missing: that is study design, not failed detection."),
  heatmap = info_entry("Sampling grid",
    "Draws one row per individual showing the ages at which it was recorded, so the sampling design is visible directly.",
    "Look for individuals with gaps, late entry or early exit, and for whole occasions missing across the population. Ordering by ALR or AFR uses the values the models use (see 'Trait-specific ALR/AFR' on the Data tab, or the mapped columns), so the latest ALRs sit at the bottom. Each row runs from the AFR (blue) to the ALR (orange; purple when they are the same occasion), both counted: nothing before the AFR or after the ALR is counted as missed, whether or not lifespan is known. Individuals never measured for the trait enter no model and are not shown.",
    "With many individuals only a sample of rows is drawn.",
    more = "One row per individual and one column per expected occasion: observed, missed (expected but not recorded), the AFR (blue), the ALR (orange) and, for an individual recorded on a single occasion, AFR = ALR (purple). White cells lie outside the window: before the AFR or after the ALR. Each individual's schedule starts from its own first record (or the common start age), so staggered cohorts are not given spurious missed occasions. Scattered red cells: random gaps. Red concentrated at young or old ages: age-dependent sampling. Order rows by ALR, AFR, mean trait, ID or at random to see whether gaps line up with lifespan, entry age or trait values. Up to 500 individuals (set with 'Individuals to show in the grid') and 60,000 cells are drawn, evenly spaced in the chosen order. With irregular ages the occasions are approximate; round ages on the Data tab. Very fine or irregular ages are coarsened so that the grid stays below 300,000 cells, which is reported on this tab."),
  missing_by_age = info_entry("How much is missing, and where?",
    "Shows how many individuals are present at each age and the share of expected occasions with a trait record.",
    "Missingness rising with age suggests 'missing when old'; high early missingness suggests 'missing when young'. Where few individuals remain, conclusions about those ages are weak.",
    "Percentages at the oldest ages rest on few individuals. Missed occasions are counted against the sampling interval (step 3 settings)."),
  missing_vs_var = info_entry("Is what is missing related to the trait or the individual?",
    "Tests whether missing records are related to age, lifespan, entry age, condition or the previous trait value.",
    "Use this to judge whether missingness looks random, or whether older, younger, longer-lived or higher-trait individuals go missing more often.",
    "These are associations, not tests of the type of missingness. Missingness that depends on the trait produces the same evidence as age-dependent selective disappearance, and no model on these data can separate the two.",
    more = "Missed-occasion rate against an individual-level variable, and logistic regressions of missingness on age, LS, AFR, the previous trait value and condition. Use this to judge whether missingness might be random, or whether the pattern is indicative of non-random missingness \u2014 older, younger, longer-lived or higher-trait individuals going missing more often. These are only associations, so use them as suggestions rather than as anything definite. Do not use visual diagnostics of missingness as quantitative tests of the type of missingness."),
  proxy_agreement = info_entry("ALR, mean age and lifespan",
    "Compares the lifespan proxies against each other, and against known lifespan where you have it.",
    "A proxy that tracks lifespan closely corrects better for which individuals remain. Under incomplete sampling or variable entry age, age at last record tracks lifespan better than mean age.",
    "ALR equals lifespan only when individuals are followed until death; emigration, missed detection and censoring make it shorter.",
    more = "ALR is the age at an individual's last record in the data. It equals lifespan only when individuals are followed until death: emigration, missed detection and censoring also end the records, so ALR is a proxy of lifespan. Pairwise agreement between ALR, mean sampled age and known lifespan (one point per individual, with r, a dotted 1:1 line and a solid black linear regression line). High r(ALR, LS) supports ALR-based models (2, 4); high r(mean age, LS) supports centring models (3, 5). Under complete sampling ALR and mean age are nearly interchangeable. The regression line shows how one measure scales with the other: departures from the 1:1 line reveal systematic under- or overestimation, for example ALR falling increasingly short of lifespan when final occasions are missed. Missed final occasions make ALR underestimate lifespan; intermittent gaps shift mean age. With unknown lifespan only ALR vs mean age is shown."),
  a3_settings = info_entry("Which function best describes individual-level data?",
    "Explores which ageing function might best describe the average trajectory of the individuals in your data, by fitting the same function to each individual in turn.",
    "Fit a function, press 'Compare ageing functions across individuals' to compare the functions in the table, then send the one you choose to the modelling step.",
    "A misspecified ageing function can lead to incorrect conclusions about selective disappearance. Individuals need more records than the function has parameters.",
    more = "Scale: count traits are fitted on the log scale (a Poisson fit for each individual), the scale on which they are modelled and simulated; 'Automatic' decides from the trait. Minimum records: with noisy traits and higher-order functions, individuals with few records dominate the average, so raise it to fit only well-sampled individuals - at the cost of fitting longer-lived individuals, which the survivor-bias check below reports. How does each individual age? This fits each individual\u0027s longitudinal data with the same ageing function, for example logarithmic or quadratic. You can then compare, below, how much of the variation in the individual data each functional form explains, and judge at the individual level which function each individual might be ageing along. Averaging the fitted functions, or averaging their coefficients, reconstructs the approximate parametric latent function that represents the average within-individual trajectory. Compare functions in the table. 'Use this ageing function for the mixed models' makes the selected function the ageing function on the Modelling tab (otherwise, once you have run the comparison, the Modelling tab starts with the function with the lowest mean \u0394AICc across individuals). 'Exponential (a \u00b7 exp(b \u00b7 age))' is fitted by non-linear least squares and is the only function that is non-linear in its parameters. Draw the longest- or shortest-lived individuals to see how fits change with the number of records. Individuals need at least 2 records at different ages for a linear fit, 3 for quadratic, logarithmic and exponential fits, and 4 for a cubic fit. With exactly as many records as parameters the curve passes through every point, so it carries no residual information (no AICc) and is left out of the function comparison. Non-linear fits that do not converge are dropped."),
  a3_scale = info_entry("Scale of the individual fits",
    "Sets the scale on which each individual's ageing function is fitted. 'Raw trait' fits the function to the trait as recorded, by least squares: for continuous traits, for example body mass. 'Log (counts)' fits it to the log of the expected count, with a Poisson likelihood for each individual: for counts, for example fecundity. 'Automatic' chooses the log scale for counts and the raw scale otherwise.",
    "The curves are always drawn in the trait's own units, so on the log scale the chosen function is straight or curved on the log scale, not in the figure. A Linear function on the log scale is log(expected count) = b0 + b1\u00b7age, so the expected count is exp(b0 + b1\u00b7age): a curve that is always positive and changes by the same percentage per unit age. It is not a quadratic; a Quadratic on the log scale is exp(b0 + b1\u00b7age + b2\u00b7age\u00b2). Use the log scale for counts - they cannot be negative, their variance grows with the mean, and the mixed models for counts use a log link, so the individual fits then match the scale of the models. Use the raw scale for continuous traits.",
    "Counts fitted on the raw scale can be given negative expected values, and individuals with high counts dominate. The log scale needs non-negative traits; with negative values the fits use the raw scale. The shape you choose describes the trait on the scale it is fitted on, so compare functions on the same scale as the mixed models you will fit."),
  a3_fits = info_entry("Individual trajectories and data support",
    "Fits each individual's own ageing curve with the chosen function and draws a sample of them over their records.",
    "Look for systematic misfit (for example curvature a straight line misses) and whether it differs between short- and long-lived individuals. Use the function comparison beside this figure to choose another shape.",
    "A visual check, not a test. Individuals with few records are fitted loosely or not at all, so the sample shown favours well-sampled individuals.",
    more = "The support metrics summarise the information for individual slopes: individuals, observations, observations per individual (median and interquartile range), the share with at least 2, 3 and 4 distinct ages, and the median age span. A line through 2 points, a quadratic through 3 or a cubic through 4 fits exactly and says nothing about the true shape; separable slopes need at least 3 points and a non-exact quadratic 4. If few individuals have 3 or more points, individual slopes and random slopes (Modelling tab) are poorly estimated."),
  a3_mean = info_entry("What is the average within-individual trajectory?",
    "Averages the fitted individual trajectories across all sampled ages: an approximation of the average within-individual ageing trajectory, free of who survives.",
    "Read it as the trajectory the models try to recover and compare it with the model predictions (Modelling tab). Two curves are drawn: the mean of the coefficients (the typical individual) and the mean of the individual curves (the population average). They coincide for functions linear in their coefficients on the scale drawn; on the log scale the population average lies above. A large gap means the individual curves vary a lot, often a flexible function fitted to few records: raise the minimum records or choose a simpler function.",
    "It treats individuals as immortal: each curve is extended to the oldest age with at least five individuals. It is unreliable with high missingness, few records per individual, small samples, or when individuals follow different shapes.",
    more = "Mean of coefficients: f(age; \u03b8\u0304) with \u03b8\u0304 = (1/N) \u03a3\u1d62 \u03b8\u1d62. Mean of functions: f\u0304(age) = (1/N) \u03a3\u1d62 f(age; \u03b8\u1d62). They differ when f is non-linear in \u03b8 (Jensen's inequality), including every function drawn as exp(f) on the log scale. Bands: 95% intervals from SD\u1d62[f(age; \u03b8\u1d62)]/\u221aN (functions) and the delta method with Cov(\u03b8\u1d62)/N (coefficients). The mean of coefficients compares with a mixed model's fixed-effect curve on the link scale, and is what the simulated truth shows; the mean of functions compares with observed means when there is no selection. Treat it as a suggestion of the latent trajectory, not an estimate."),
  a3_compare = info_entry("Individual-level function comparison",
    "Compares how well each ageing function fits individual trajectories: mean adjusted R\u00b2, mean \u0394AICc across individuals, and Share_best, the percentage of individuals for which each function has the lowest AICc (ties split).",
    "The lowest mean \u0394AICc and the largest Share_best point to the function with most support in individuals; a difference of a few AICc is not decisive. Check the choice with the ageing-function check (Checks tab), which is more reliable.",
    "Unreliable with few records per individual, few occasions, high missingness or small samples. \u0394AICc and Share_best use only the individuals every function can be fitted to (N_common): if that set is much smaller than your sample, do not use them, and prefer adjusted R\u00b2.",
    more = "The comparison describes which function is estimable from these individuals, not which generated the data: with few points per individual the best-fitting function is often not the generating one, even in simulations. It assumes every individual follows the same functional form; that may not hold if ageing is stochastic. A function with statistical support can still be biologically implausible."),
  a3_coefs = info_entry("Individual coefficients",
    "Summarises the coefficients estimated for each individual under the chosen ageing function: their mean (the coefficient averaged across individuals), variability (heterogeneity among individuals in that coefficient) and range across individuals.",
    "The functions fitted are: Linear, b0 + b1\u00b7age, where b0 is the intercept and b1 the linear rate of change with age. Quadratic, b0 + b1\u00b7age + b2\u00b7age\u00b2, where b2 is the curvature: negative for a rise-then-fall, positive for a fall-then-rise. Cubic, b0 + b1\u00b7age + b2\u00b7age\u00b2 + b3\u00b7age\u00b3, where b3 allows a second change of direction. Logarithmic, b0 + b1\u00b7log(age), where b1 is the change per proportional increase in age, so change slows as age rises. Asymptotic exponential, b0 + b1\u00b7exp(\u2212z), where z is age standardised to mean zero and unit standard deviation. b0 is the level approached at old age and b1 sets how far from that level the trait starts; the rate of approach is fixed by the spread of ages, not estimated.",
    "Wide spread in a coefficient means individuals differ in that aspect of ageing. Coefficients are estimated on standardised age unless standardisation is switched off, so compare them within one analysis rather than across datasets. For count traits fitted on the log scale, the coefficients are on the log scale: exp(b0) is the expected count at age zero, and each coefficient changes the log of the expected count."),
  b2 = info_entry("How do different functions fit the population-level data?",
    "Fits each ageing function to the whole population as a mixed model (Model 1, with your family, covariates and random effects) and compares AICs and curves.",
    "The lowest AIC is the best-supported shape of the population trajectory; within 2 AIC, shapes are equally supported. Compare with the individual-level comparison, which describes individuals.",
    "This fit has no lifespan or AFR term, so the preferred shape can change once proxies are added. Cubic curves extrapolate poorly.",
    more = "Fitted by maximum likelihood (the non-linear exponential by non-linear least squares); points are observed means. With count families the exponential a \u00b7 exp(b \u00b7 age) is the Linear function on the log scale and is left out. 'Asymptotic exponential' is an intercept plus \u03b2 \u00b7 exp(\u2212standardised age): linear in its coefficients, flattening to a plateau at a rate fixed by the age scale, unlike 'Exponential (a \u00b7 exp(b \u00b7 age))', whose rate is estimated."),
  model_settings = info_entry("Model settings",
    "Sets the error family, ageing function, random effects and the models to compare.",
    "Start simple: pick the family that suits the trait, keep the random intercept, add random slopes to test whether individuals age at different rates, press 'Fit models', then compare AICs.",
    "Without random slopes, interaction models can fit better with no age-dependent selection, because the interaction absorbs individual differences in ageing: if one wins, refit with random slopes. AIC compares models only within one family and random structure (use 'Compare models across fits' for others).",
    more = "Each model row has a tick box, an 'i' with what it tests and its exact formula, and a menu to add terms to that model alone. The wrench builds terms from up to three components (age, ALR, AFR, LS, mean age, a covariate) joined by + or \u00d7. Binomial families show a 'Weights' menu for the number of trials. Random effects: '(1 | ID)'; uncorrelated '(1 | ID) + (0 + age | ID)'; correlated '(1 + age | ID)'; one slope per age term; 'Automatic' follows the data support shown below the menu. Among-individual terms enter linearly by default; the two 'Higher order' options add their squares and cubes. The non-linear exponential fits trait = a \u00b7 exp(b \u00b7 z_age) with nlme (Gaussian only). Built interactions with age multiply the ageing terms, so they can over-fit or fail to converge: compare them with simpler models. Including invalid fits is for inspection only."),
  lrt = info_entry("Nested likelihood-ratio tests",
    "Compares nested models: pairs in which one model is a subset of the other, so the larger model contains fixed-effect terms that the smaller one does not. This assesses whether those extra fixed-effect terms are significant in explaining the data.",
    "If the smaller model is no different from the larger one (a non-significant test), the additional fixed-effect terms do not explain the data, and the simpler model is usually preferred (parsimony).",
    "Only nested pairs can be compared this way; models that are not nested are compared by AIC. A likelihood-ratio test compares two models, not the whole set, and repeated tests inflate the chance of a false positive."),
  fit_status = info_entry("Fitting status, convergence and dropped rows",
    "Shows for each model whether the fit converged, whether it is singular, whether its Hessian is positive definite, and which rows were dropped.",
    "Converged: estimates can be read. Singular: a random-effect variance is at zero or a correlation at \u00b11, so the random structure is more complex than the data support; fixed effects are usually usable, but a simpler structure is better. Hessian not positive definite: a parameter cannot be identified (too many parameters, near-collinear terms, a variance at zero), so standard errors and p-values cannot be trusted.",
    "'Caution' fits can be compared, but check them first; fits with an invalid Hessian are excluded from the ranking. Rows missing any model variable are dropped from every model, so all models use the same data."),
  model_support = info_entry("Which selective processes best explain the data?",
    "Ranks the fitted models by AIC and marks those within 2 AIC of the best, which the data do not distinguish; R\u00b2 shows the variance each explains. Models 1 to 10 compare selective processes: disappearance and appearance, through ALR, AFR, lifespan or mean age, age-independent or age-dependent. Mapped covariates enter every model.",
    "Read the supported set, not the top row, and check each model's fit status. Model 2 vs 4 tests age-dependent disappearance through ALR, 3 vs 5 through mean age; 2 vs 7 and 4 vs 8 test selective appearance. Models 9 and 10 let the two differ in age dependence (9: ALR \u00d7 age + AFR; 10: ALR + AFR \u00d7 age). Among similar models (\u0394AIC < 2), interpret the simpler one.",
    "AIC ranks models; it does not establish a process. Trait-dependent missingness, omitted random slopes, a misspecified ageing function, a wrong error family and terminal decline can all make a lifespan model win: read the sensitivity banner, the coefficients and the diagnostics before concluding.",
    more = "Gaussian models are fitted with lme4 (glmmTMB when the variance changes with age), all other families with glmmTMB; Gaussian p-values use lmerTest. Each fit is Valid, Caution (boundary or singular fit, gradient warnings, dropped rank-deficient terms) or Failed (no fit, invalid Hessian or non-finite AIC); failed fits are left out of the ranking, tests, predictions and interpretation unless 'Include fits with invalid Hessians' is ticked. Likelihood-ratio tests appear only for nested pairs and are approximate for mixed models. In Models 3 and 5 each age term is centred on the individual's own mean of that term (\u0394age\u00b2 = age\u00b2 minus the individual's mean of age\u00b2), so within-individual terms average zero for every individual. Centring age\u00b2 on the squared mean age instead (Fay et al. 2022, eq. 3) changes fits little; squaring centred age (their eq. 4) biases ageing estimates. Models 3 and 5 can win with no selection at all when missingness depends on the trait: read this with the missingness drivers (step 3). Two further traps (0.22.7 audit): a calendar-year trend within a study window makes ALR models win without selection (add year as a random intercept); and when AFR is correlated with lifespan, Model 10 can report selective appearance that is disappearance (compare with Model 8)."),
  consistency = info_entry("Internal consistency of the evidence",
    "Cross-checks the ranking against other evidence: likelihood-ratio tests against AIC, the best model's age dependence against the lifespan-trait slopes across age bins, 'Caution' fits, missing random slopes and, for simulations, recovery of the true trajectory.",
    "Agreement strengthens a conclusion. When checks disagree, report it and prefer the more conservative reading. AIC measures fit to the sampled records; recovering the latent trajectory is a different goal, and the two can favour different models.",
    "The checks are heuristic: no flag is not proof that the evidence is coherent."),
  random_slopes = info_entry("Random slopes of age",
    "Lets individuals differ in how fast they age, not only in their average level, by adding a random slope of age within individual.",
    "Fit a random slope whenever the data support it, then compare the model ranking with and without it.",
    "Without random slopes the interaction models can fit better simply because individuals differ in ageing rate. To avoid reading that as age-dependent selective disappearance, refit with slopes and check the interaction p-values and the visual diagnostics.",
    more = "Lets individuals differ not only in their average level but in how fast they age, by adding a random slope of age within individual. Fit a random slope whenever the data support it, then compare the model ranking with and without it. Without random slopes, the interaction models can still fit better simply because the interaction terms recover among-individual heterogeneity in ageing. To avoid reading that as age-dependent selective disappearance, refit with random slopes and check whether the result holds."),
  varcomp = info_entry("Random effects",
    "How much individuals (and grouping levels) differ beyond the fixed effects: the variance and SD of each random term.",
    "Compare each variance with the residual. A large intercept variance: individuals differ consistently in level. A slope variance clearly above zero: individuals age at different rates, the case in which omitting slopes can favour interaction models. A term flagged 'explains no variance' can usually be dropped.",
    "Variances are on the link scale (log or logit for counts and proportions).",
    more = "A term is flagged when it explains less than 1% of the total variance (Gaussian models) or its SD is below 0.05 on the link scale (other families). A random slope is judged by the variation it adds across the records, so its scale does not matter. The individual random intercept is always kept, because repeated records need it."),
  store_pred = info_entry("Predicted trajectories of the stored models",
    "Draws the predicted population trajectory of each stored model you tick, so fits with different settings can be compared by eye.",
    "Tick the stored models to draw. Each curve is drawn as it was predicted when the model was stored, with random effects at zero, covariates at their means and proxies at their individual-level means.",
    "Curves from other data or data settings, or from a different error family, are drawn on their own response scale: compare their shapes, not their AICs.",
    more = "Only models stored from 0.24.7 onwards carry a predicted trajectory."),
  b2_curves = info_entry("Fitted population trajectories",
    "Trajectories of each function drawn with random effects set to zero (a typical individual), numeric covariates at their mean over individuals, and ALR, LS, AFR and mean-age terms at their individual-level means.",
    "Factor covariates are averaged over the level combinations in the data, weighted by individuals, or split by 'Show predictions by'. Curves are on the response scale, including zero inflation.",
    "Points are observed means, which include the effect of any selective disappearance."),
  predictions = info_entry("Model predictions trajectory",
    "Draws each model's predicted population trajectory on the trait scale, with optional overlays: observed means, individual records, the decomposition and the reconstruction from individual fits.",
    "Compare the corrected models with Model 1, which ignores selective processes. A difference in level indicates an age-independent association; a difference that widens or narrows with age, an age-dependent one. In simulations, models that account for selection should track the truth.",
    "Under age-independent selection all models give almost the same curve even when AICs differ greatly, because the proxy is held at its mean; curves separate only under age-dependent selection. Late ages rest on few individuals. For counts, the curve is the typical individual.",
    more = "Predictions set random effects to zero (a typical individual), numeric covariates to their mean over individuals, and ALR, LS, AFR and mean-age terms to their individual-level means. Factor covariates are averaged over observed level combinations, weighted by individuals (or split by 'Show predictions by'). The scale is the response scale, including zero inflation. The decomposition (Rebke et al. 2010) chains the mean change between successive occasions of individuals recorded at both; it is biased under age-dependent selection, as are observed means. Caution fits are dashed; failed fits are not drawn unless included."),
  decomposition = info_entry("Decomposition (Rebke et al. 2010)",
    "Reconstructs the population trajectory from within-individual change alone, using at each step only the individuals measured at both ages.",
    "The gap between this and the observed means is the contribution of selective disappearance. Nothing is fitted, so no ageing function is assumed.",
    "Unbiased when disappearance is age-independent, biased when the rate of ageing itself is linked to lifespan. Late steps rest on few pairs, and a break in the sampling grid splits the curve into segments.",
    more = "A non-parametric reconstruction of the population trajectory from within-individual change alone. Occasions are the distinct sampled ages; between each pair of adjacent occasions the app takes the mean change in the trait over only those individuals recorded at both, then chains those increments from a starting level. In symbols: the value at occasion t+1 is the value at t plus the mean of (trait at t+1 \u2212 trait at t) over individuals present at both. Nothing is fitted, so no functional form is assumed. Compare it against the observed means. Observed means mix within-individual change with the changing composition of survivors; the decomposition removes the composition change, because every increment comes from individuals measured twice. A gap between the two is the contribution of selective disappearance, and the decomposition is what the population trajectory would look like if no one had left. It is unbiased when disappearance is age-independent, and biased when the rate of ageing itself is linked to lifespan, because then the individuals contributing each increment are themselves a selected set. On an irregular schedule records are first placed on a common grid, and a break in the grid splits the curve into segments that are drawn unjoined. Late increments rest on few pairs."),
  reconstruction = info_entry("Reconstruction from individual fits",
    "Averages each individual's fitted ageing function (step 4) across the whole age range: the parametric counterpart of the decomposition.",
    "Read it as the average within-individual trajectory with mortality set aside: it is not shifted by who survives, so a gap from the observed means suggests selective disappearance.",
    "It treats individuals as immortal and assumes they share one functional form; unreliable with few records per individual, high missingness or small samples."),
  effect_sizes = info_entry("Effect sizes",
    "How far the ageing slope moves when the lifespan proxy is added, how much the trait changes per unit of ALR, and the difference in ageing rate between long- and short-lived individuals.",
    "A large change in slope means the correction matters for the ageing estimate, whichever model won; report it with the ranking (for example 'correcting for selective disappearance steepened the decline by 34%').",
    "All on the fitted link scale (log or logit for counts and proportions). The interval on the change in slope treats the two fits as independent, so it is conservatively wide; the percentage is unstable when the uncorrected slope is near zero.",
    more = "Change in slope: Model 1 against Models 2, 4 or 6, absolute and as a percentage. Rate difference: the ALR \u00d7 age coefficient times the spread of ALR between its 10th and 90th percentiles. All with 95% intervals."),
  permutation = info_entry("Null-model bootstrap against data without a lifespan effect",
    "Fits a model with no lifespan term - a flexible (cubic) ageing function, individual differences in level and in rate of ageing, and your covariates and family - and simulates new trait values from it, keeping every individual's real ages, ALR, AFR and lifespan. The chosen model and Model 1 are refitted to each simulated dataset, and the model's AIC advantage in your data is compared with the advantages in the simulations.",
    "A small p-value means data without any link between lifespan and the trait rarely give an advantage as large as yours, so the lifespan term captures a real association. Shuffling lifespans between individuals, as a permutation test does, would create records after an individual's last record or death, so anything tied to the observation window - a misspecified ageing function, a change just before death - would beat every shuffle. Simulating the trait keeps the real observation windows. The test counts in the evidence summary only when it is run on the model the finding rests on, with the same data and settings.",
    "The default is 39 simulated datasets. The smallest possible p-value is 1 / (n + 1) for n refitted datasets, so at least 19 must be refitted to reach p < 0.05; 39 leaves room for a few failed refits. A change in the trait just before death is still a link to lifespan, so check 'Is there terminal investment?' in step 2 when the result is significant.",
    more = paste("Null: no link between the trait and the lifespan proxy, in level or rate, given the observed sampling. It is always compared with Model 1, so it tests any proxy association, not age-dependence specifically.",
                 "The null uses a cubic only with at least six distinct ages (otherwise your function), and your residual-variance model, so a spread that grows with age cannot by itself make the proxy look significant. Missingness is taken as observed: trait-dependent missingness is not reproduced.")),
  evidence = info_entry("Evidence summary",
    "Grades the evidence in the results you saved, separately for selective disappearance and selective appearance.",
    "Save results from steps 2 to 5 and the summary updates as you save or remove them. The level is strong only when every check relevant to the finding has been saved and has passed.",
    "It reads only saved results, so an exploratory result saved by mistake is counted. It grades the evidence for a pattern; it does not establish the mechanism behind it.",
    more = "Which saved results feed which line. Visual pattern: the trajectory, gap or trait-against-lifespan figures in step 2. Model comparison and the lifespan term: the latest model comparison in step 5 fitted with a random intercept only (or, if none was saved, the latest comparison). Random-slope sensitivity: a model comparison of the same analysis saved after refitting with random slopes; if it disagrees, the evidence is weakened rather than replaced. The finding's direction is read from the predictions of the model it rests on. Ageing shape: the Ageing-function check, or the automatic check saved with the model comparison; a manual check counts only for the function actually fitted, and not when it was run on Model 1 for a finding about selection. Null-model bootstrap: step 5, Advanced, run on the model the finding rests on (for example Model 4 for an age-dependent finding) with the same data and settings. Results from an earlier analysis with other settings are set aside. Observation bias: the missingness results in step 3."),
  detection = info_entry("Detection probability",
    "The share of expected occasions at which an individual carrying a record was actually recorded.",
    "Read it with the sampling window in mind: its value depends on whether missingness is counted only between an individual's first and last record, or from the age at which the trait is first expressed.",
    "Low detection makes ALR underestimate lifespan, which weakens ALR-based corrections."),
  accuracy = info_entry("How close is each model to the known truth?",
    "The true trajectory here is what ecologists want to estimate from longitudinal data: the average within-individual trajectory, latent to the researcher but inferable from cohort-level data. Relativised deviation D = 100 \u00d7 (estimate \u2212 truth) / truth (Methods Eq. 11) for each model, observed means and the decomposition.",
    "Smaller mean |D| is better.",
    "Simulations only; restricted to ages with at least 10 observed individuals."),
  coefficients = info_entry("Coefficients, scaling and interpretation",
    "Fixed-effect estimates, standard errors and tests for the selected model, with the scaling constant behind each coefficient, the estimate per original unit, the random-effect variances, and a plain-language reading of the selective disappearance or appearance terms. Scaling: with standardisation, age = (age \u2212 centre) / SD and ALR, LS and AFR are standardised across individuals; a term's scale factor is the product of the SDs of its components (for example SD\u00b2 for age\u00b2, SD_age \u00d7 SD_ALR for age \u00d7 ALR). Estimate per original unit = estimate / scale factor: the coefficient per unit of the centred original variables (for example per year of ALR, or per (age \u2212 centre)\u00b2). Numeric covariates are not standardised. Count models are on the log scale.",
    "The interpretation compares predicted trajectories for individuals at the 10th and 90th percentiles of ALR (or LS, AFR, mean age) at a young, middle and old age. If only the additive term is supported, longer- and shorter-lived individuals differ by a similar amount at all ages (age-independent selective disappearance). If the interaction is supported, the gap changes with age (age-dependent), for example longer-lived individuals having higher trait values mainly at older ages.",
    "Standardisation aids numerical stability; the per-unit column translates coefficients back to biological units. P-values are Satterthwaite t-tests for Gaussian fits when lmerTest is installed and asymptotic Wald z-tests otherwise (see the 'P type' column); likelihood-ratio tests are used where models are nested. The interpretation is an association conditional on the model, not proof of selection: check it against the visual diagnosis plots, residual diagnostics and alternative random-effect structures. Standardised coefficients are expressed on the age scale of the data analysed, so they change if the sample or its age range changes."),
  definitions = info_entry("Model definitions",
    "Fixed effects and the question addressed by each ticked model, with the chosen covariates and interactions.",
    "Models 3 and 5 separate within- and among-individual age effects; Model 6 needs known lifespan; Models 7\u201310 add AFR (age at first observation). Click the 'i' next to a model in the settings for its exact specification, including terms added to that model.",
    "Covariates enter every model; interactions chosen on the Data tab are added to every model; terms added in a model's menu apply only to that model."),
  function_check = info_entry("Ageing-function check",
    "Refits a model with each ageing function in turn and compares AICs, to test whether another shape fits better.",
    "The lowest AIC is the best-supported function for that model; within 2 AIC, functions are equally supported. Refit with it if yours is behind. When this disagrees with step 4, rely on this check: it compares functions within the same model, using all the data.",
    "A misspecified function can make interaction models look supported without selective disappearance, because the interaction absorbs the wrong shape.",
    more = "The same comparison runs automatically after each fit on the best-supported model and reports in the sensitivity banner. The age terms change with the function and the rest of the model follows: for Model 4, a quadratic fits age + age\u00b2 + ALR + age \u00d7 ALR + age\u00b2 \u00d7 ALR, and a logarithmic fits log(age) + ALR + log(age) \u00d7 ALR. For counts the truth is the typical individual's curve, so observed means differ from it even without selection."),
  family_check = info_entry("Error-family check",
    "Refits a model with each family of the trait's kind (count families; binomial families; or Gaussian, Gamma, lognormal and beta) and compares AIC.",
    "Choose the lowest AIC, then check the residuals. Zero inflation is worth testing when zeros are common.",
    "Families are compared only within a kind: continuous families never against count families. Gamma and lognormal need positive values, beta values strictly between 0 and 1.",
    more = "The same comparison runs automatically after each fit, on the best-supported model, and an instant screen of the fitted model (overdispersion, excess zeros, skew, a count trait fitted as Gaussian) runs with it; both report in the sensitivity banner. Zero-inflated families enter the automatic check only when at least 10% of values are zero."),
  dharma = info_entry("Are my data overdispersed or underdispersed?",
    "Simulation-based residual checks for the fitted model: whether residuals are uniform, whether the data are more or less variable than the error family expects, and whether there are more zeros than expected.",
    "A good model gives a QQ (quantile-quantile) plot whose points follow the diagonal and a residual-versus-fitted plot with no pattern, and non-significant uniformity, dispersion and zero-inflation tests. Points bending away from the diagonal, or a fan or curve in the residuals, indicate a problem.",
    "In large datasets trivial deviations become statistically significant, so judge the plots rather than the p-values alone.",
    more = "Reading the plots in more detail. Overdispersion shows as a dispersion ratio well above 1 and residuals spread wider than the simulated ones: move from Poisson to negative binomial. Underdispersion shows as a ratio well below 1 and residuals bunched towards the middle: common with bounded counts, and it makes intervals too wide rather than too narrow. Excess zeros show as a zero-inflation test with a ratio above 1: try a zero-inflated family. A curved band in the residual-versus-fitted plot means the ageing function or a covariate is misspecified, not the error family. Outliers flagged in red are observations more extreme than every simulation; check them for recording errors before removing anything. Simulation-based residual tests for uniformity, dispersion and zero inflation, with diagnostic plots. Small p-values indicate that the error distribution or model structure fits poorly. In large datasets trivial deviations become significant: judge the plots."),
  performance = info_entry("Does my model violate assumptions such as homoscedasticity or normality of residuals?",
    "Assumption checks for the selected model from the performance package: homogeneity of variance, normality of residuals and of random effects, influential observations, collinearity and, for counts, overdispersion and zero inflation.",
    "A good model shows a flat, even band in the homogeneity panel, points close to the line in the normality panels, no points beyond the influence contours, and low collinearity. A funnel in the homogeneity panel, strong curvature in the normality panels, or isolated points outside the contours indicate a violation.",
    "Mixed models are fairly robust to mild departures from normality. Heteroscedasticity and influential points matter more, because they distort both the estimates and their standard errors.",
    more = "Reading the panels in more detail. Homogeneity of variance: a funnel widening with fitted values suggests a count or log-scale trait modelled as Gaussian - try a count family or a log transform. Normality of residuals: heavy tails or an S-shape suggest the wrong family or outlying individuals. Normality of random effects: a skewed distribution suggests a subgroup of individuals behaving differently, worth modelling as a covariate. Influential observations: points outside the Cook's distance contours shift the estimates on their own; refit without them to see whether conclusions change. Collinearity: a variance inflation factor above about 5 to 10 means two predictors carry overlapping information, which is expected between age and its polynomial terms and between ALR and mean age, and inflates their standard errors. Singular fits mean a random-effect variance is estimated at zero: simplify the random structure. Checks from the performance package for the selected model: Nakagawa R\u00b2, intraclass correlation, singularity, convergence, collinearity and, for counts, overdispersion and zero inflation, plus the performance::check_model() diagnostic panels (needs the see package). Use these alongside the DHARMa residual checks: singular fits suggest simplifying the random effects; overdispersion suggests a negative binomial family; a zero-inflation ratio well below 1 suggests a zero-inflated family. Not every check supports every model type (for example some zero-inflated glmmTMB models): unavailable checks are reported rather than stopping the app. Polynomial and interaction terms are collinear by construction. check_model() can be slow on large datasets."),
  rcode = info_entry("Reproducible R code",
    "A script that reproduces the model comparison with lme4 or glmmTMB.",
    "Run it on your data file to reproduce or extend the analysis.",
    "It assumes the same column names and settings as the app."),
  saved = info_entry("Saved results",
    "The results you saved from other tabs. They make up the exported HTML report (with plots) or text report.",
    "Remove items you no longer need before exporting.",
    "Saved items are snapshots: they do not update when the data or settings change."),
  age_step = info_entry("Sampling interval",
    "The interval between sampling occasions, in age units. It sets the expected occasions (grid and missingness), the links of the decomposition, the 'next occasion' of the disappearance plots and the prediction ages.",
    "Keep 'infer' ticked for regular designs. Enter the interval when the inferred one is wrong, for example annual sampling with some mid-year records, which is inferred as 0.5.",
    "Too small an interval inflates missingness and fragments the decomposition; too large merges occasions. With a set interval, the decomposition leaves out records off its grid and reports how many."),
  disp = info_entry("Is the variance in the data changing across age?",
    "Lets the residual variance (Gaussian) or dispersion (Gamma, lognormal, beta, negative binomial, beta-binomial) change with age on the log scale, through glmmTMB's dispersion formula.",
    "Use it when the trait's spread changes with age. An unmodelled variance that rises with age can make interaction models look supported; compare constant and age-dependent variance in 'Compare models across fits'.",
    "Ignored for Poisson, binomial and zero-inflated Poisson, which have no dispersion parameter. Gaussian fits then use glmmTMB, with Wald z-tests. The bootstrap uses the same variance model."),
  model_store = info_entry("Compare models across fits",
    "Stores fitted models so that models from different fits (ageing function, random effects, family, variance model, extra terms) can be compared by AIC.",
    "Tick models, press 'Store selected models', change the settings, refit and store more.",
    "AIC compares only models fitted by maximum likelihood to the same records of the same response. Stored models are grouped into sets that share data, records and likelihood type (continuous or discrete); \u0394AIC is computed within a set. Variances estimated at zero make comparisons of random structures conservative."),
  blup_cov = info_entry("Do individuals' model-predicted random effects covary with lifespan, ALR or AFR?",
    "Correlates each individual's predicted random effects (level; with random slopes, ageing rate and curvature) with its ALR, AFR and known lifespan. From Model 1 this estimates Cov(LS, \u03b2\u2080) and Cov(LS, \u03b2\u2081), the covariances behind selective disappearance.",
    "A level association suggests age-independent selective disappearance (appearance, for AFR); a rate association suggests an age-dependent one. From a model that already includes a proxy, only the remaining association is shown.",
    "Descriptive only: random effects are shrunk towards zero and their uncertainty is ignored, so p-values are anticonservative (Hadfield et al. 2010). Rate and curvature need random slopes. The level is at the age where every age term is 0 (the mean age for standardised polynomials, age 0 for unstandardised ones). Non-Gaussian effects are on the link scale."),
  peak_onset = info_entry("Peak age and onset of senescence",
    "For each model, the age at which the predicted population trajectory (random effects at zero, proxies at their means) peaks, and the age at which its final decline to the oldest sampled age begins.",
    "Compare models: correcting for selective disappearance can move the peak. For a quadratic, the onset equals the peak.",
    "95% limits come from 1,000 draws of the fixed effects, so they cover curve uncertainty only. A maximum at the edge of the sampled ages is not a peak. If lower values mean better performance, read the lowest point instead."),
  methods_text = info_entry("Auto-written methods",
    "Drafts a methods section, with references, from the saved results you tick, using the data and settings in force when each was saved.",
    "Tick results. Resolve any listed conflicts (different data or model settings) by choosing the primary analysis, then edit the draft.",
    "A draft: check every statement before use."),
  overview_auto = info_entry("Automatic overview",
    "An automatic summary of the current data and settings.",
    "Use it as a checklist of where to look, not as a conclusion. Save it to include it in the report.",
    "It is generated from rules, so it can miss what a careful look at the figures would catch.")
)


# Collapsible box with the R code that reproduces a section (the minus sign hides it)
section_code_box <- function(section) {
  fluidRow(box(width = 12, title = "R code for this section", status = "info", solidHeader = TRUE, collapsible = TRUE,
    p(class = "small-note", "A script that reproduces this section with the current settings. The app's own helper functions are included, so it runs on its own (packages: ggplot2; lme4, and glmmTMB or nlme where models are fitted). If you uploaded a file, put it in R's working directory."),
    div(class = "code-toolbar",
        tags$button(type = "button", class = "btn btn-default btn-sm", onclick = sprintf("disapprCopy('code_%s_text', this)", section), icon("copy"), " Copy"),
        downloadButton(paste0("download_code_", section), "Download .R", class = "btn-default btn-sm")),
    div(class = "section-code", verbatimTextOutput(paste0("code_", section, "_text")))))
}

info_title <- function(title, key) {
  tags$span(title, actionLink(paste0("info_", key), label = "", icon = icon("circle-info"),
                              class = "info-link", title = "What is this, and how do I read it?"))
}

info_body <- function(e) {
  has <- function(x) !is.null(x) && length(x) && any(nzchar(trimws(as.character(x))))
  tagList(
    if (has(e$lead)) p(e$lead) else NULL,
    if (has(e$what)) tagList(h5(strong("What it does")), p(e$what)) else NULL,
    if (has(e$how)) tagList(h5(strong("How to use and interpret it")), p(e$how)) else NULL,
    if (has(e$caution)) tagList(h5(strong("Cautions")), p(e$caution)) else NULL,
    if (has(e$more)) tags$details(class = "info-more", tags$summary("More info"),
                                  div(class = "info-more-body", p(e$more))) else NULL
  )
}

# ---------------------------------------------------------------------------
# Guidance layer. These helpers add signposting only: what this tab is for, what to open first,
# what to look at in a figure, and which controls are recommended rather than required. They do not
# change any analysis, setting or default.
# ---------------------------------------------------------------------------

# The six numbered steps of the workflow, used by the progress strip and the Previous/Next buttons.
WORKFLOW_STEPS <- data.frame(
  tab = c("data", "visual", "sampling", "individual", "models", "summary"),
  label = c("Data", "Visual diagnosis", "Missingness and proxies", "Trajectories", "Modelling", "Report"),
  stringsAsFactors = FALSE
)

# "What to do here" panel: the goal of a tab, what to open first, what follows, and where to go next.
guide_box <- function(tab, goal, start_with, then = NULL, tip = NULL, further = NULL) {
  i <- match(tab, WORKFLOW_STEPS$tab)
  nxt <- if (!is.na(i) && i < nrow(WORKFLOW_STEPS)) WORKFLOW_STEPS$tab[[i + 1]] else NULL
  prv <- if (!is.na(i) && i > 1) WORKFLOW_STEPS$tab[[i - 1]] else NULL
  div(class = "guide-box",
      div(class = "guide-head", "What to do here"),
      div(class = "guide-goal", goal),
      div(class = "guide-step", tags$span(class = "guide-lab", "Start with:"), start_with),
      if (!is.null(then)) div(class = "guide-step", tags$span(class = "guide-lab", "Then:"), then) else NULL,
      if (!is.null(further)) div(class = "guide-step", tags$span(class = "guide-lab", "Further:"), further) else NULL,
      if (!is.null(tip)) div(class = "guide-tip", tip) else NULL,
      div(class = "guide-nav",
          if (!is.null(prv)) actionLink(paste0("go_prev_", tab), paste0("\u2190 ", WORKFLOW_STEPS$label[[i - 1]]), class = "guide-link") else NULL,
          if (!is.null(nxt)) actionLink(paste0("go_next_", tab), paste0(WORKFLOW_STEPS$label[[i + 1]], " \u2192"), class = "guide-link guide-link-next") else NULL))
}

# The page's question, shown above the guidance panel.
page_title <- function(txt) div(class = "page-title", txt)

# What the four box colours mean. Shown once, on the first numbered tab.
evidence_key <- function() {
  div(class = "evidence-key",
      tags$span(class = "ek ek-primary", "Primary"), " read this now \u00b7 ",
      tags$span(class = "ek ek-sens", "Sensitivity"), " could overturn it \u00b7 ",
      tags$span(class = "ek ek-support", "Supporting"), " corroboration \u00b7 ",
      tags$span(class = "ek ek-tech", "Technical"), " diagnostics and code")
}

# One sentence under an output telling the reader what to look at.
look_for <- function(...) div(class = "look-for", tags$span(class = "look-lab", "Look for:"), ...)

# A short, consistent status label so that recommendations are not mistaken for requirements.
badge <- function(kind) {
  txt <- switch(kind,
    recommended = "Recommended starting point",
    optional = "Optional sensitivity check",
    required = "Required for this analysis",
    caution = "Caution",
    unavailable = "Not available with these data",
    kind)
  tags$span(class = paste0("dbadge dbadge-", kind), txt)
}

# A plain-language question above the technical name of a panel, with the "i" help kept intact.
plain_title <- function(question, technical, key) {
  tags$span(class = "plain-title",
            tags$span(class = "plain-q", question),
            tags$span(class = "plain-t", info_title(technical, key)))
}

# Divider between the outputs most users need and the deeper checks.
section_divider <- function(label, note = NULL) {
  div(class = "section-divider", tags$span(class = "section-divider-label", label),
      if (!is.null(note)) tags$span(class = "section-divider-note", note) else NULL)
}

save_button <- function(id, label = "Save to summary") {
  actionButton(id, label, icon = icon("bookmark"), class = "btn-default btn-sm save-btn")
}

# ---- Moved unchanged from inst/app/server.R (0.9.11): pure helpers that use no reactive state ----

# Errors inside observers would otherwise end the session (the page turns grey and stops responding).
# Every observer runs inside this guard: the error is reported on screen and in the R console instead.
disappr_guard <- function(where, expr) {
  tryCatch(expr,
           shiny.silent.error = function(e) invisible(NULL),
           error = function(e) {
             msg <- paste0("Something went wrong (", where, "): ", conditionMessage(e))
             message("disappR: ", msg)
             try(showNotification(msg, type = "error", duration = 20), silent = TRUE)
             invisible(NULL)
           })
}

num_input <- function(x, default) {
  v <- suppressWarnings(as.numeric(x))
  if (length(v) != 1 || !is.finite(v)) default else v
}

safe_get <- function(expr) tryCatch(expr, error = function(e) NULL)

metric_card <- function(label, value, sub = NULL, width = 3) {
  column(width, div(class = "metric-card", div(class = "metric-label", label), div(class = "metric-value", value),
                    if (!is.null(sub)) div(class = "metric-sub", sub)))
}
