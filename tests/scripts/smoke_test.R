# disappR headless smoke test
# Run from the package root:   Rscript tests/scripts/smoke_test.R
# Exercises every analysis function on simulated data (all trait types, sampling schemes and
# options) and on the bundled fruit-fly data, and checks the fly reanalysis against the
# manuscript. Failures are collected and printed at the end rather than stopping the run.

app_dir <- file.path("inst", "app")
if (!dir.exists(app_dir)) stop("Run this script from the package root (the folder containing DESCRIPTION).")
old_wd <- setwd(app_dir)
on.exit(setwd(old_wd), add = TRUE)
source("global.R")

fails <- list()
notes <- character(0)
check <- function(label, expr) {
  out <- tryCatch(expr, error = function(e) {
    fails[[label]] <<- conditionMessage(e)
    NULL
  })
  out
}
expect <- function(label, cond) {
  ok <- isTRUE(cond)
  notes <<- c(notes, sprintf("[%s] %s", if (ok) "PASS" else "CHECK", label))
  invisible(ok)
}

run_pipeline <- function(raw, map, family, tag, fit_models = TRUE, models = MODEL_IDS) {
  b <- check(paste(tag, "standardise"), standardise_data(raw, map))
  if (is.null(b)) return(NULL)
  d <- b$data; m <- b$meta
  check(paste(tag, "integrity"), data_integrity(d, m))
  g <- check(paste(tag, "grid"), build_missing_grid(d, margin = 0, start_mode = m$map$start_mode %||% "afr",
                                                   start_age = m$map$start_age %||% NA_real_))
  check(paste(tag, "grid afr start"), build_missing_grid(d, margin = 1, start_mode = "afr"))
  if (!is.null(g)) {
    check(paste(tag, "missingness"), missingness_summary(d, g, 0.05, isTRUE(m$has_life) && !isTRUE(m$life_auto)))
    for (v in c("Age", "ALR", "LS", "Condition", "Trait")) check(paste(tag, "miss_by", v), missingness_by_variable(d, g, v))
    check(paste(tag, "coverage"), coverage_by_age(g))
  }
  im <- individual_metrics(d)
  for (target in c("disappearance", "appearance")) {
    for (px in proxy_choices(im, m, target)) {
      pv <- proxy_values(im, px)
      s <- check(paste(tag, "A1", px), binned_trajectory(d, pv, 4, "equal", 3))
      check(paste(tag, "A1q", px), binned_trajectory(d, pv, 3, "quantile", 3))
      if (!is.null(s)) {
        check(paste(tag, "A1 diff", px), bin_differences(s, "successive"))
        check(paste(tag, "A1 diff all", px), bin_differences(s, "all"))
      }
      check(paste(tag, "A2 age bins", px), trait_by_age_bins(d, pv, 4))
      z8 <- check(paste(tag, "A2 age bins 8", px), trait_by_age_bins(d, pv, 8))
      if (!is.null(z8) && nrow(z8)) check(paste(tag, "A2 coefficient table", px), a2_slope_trend(a2_bin_slopes(z8)))
      if (!is.null(s) && nrow(s)) check(paste(tag, "A1 trends", px), bin_difference_trends(bin_differences(s, "all")))
      check(paste(tag, "A2 slopes", px), proxy_slopes_by_age(d, pv, "r"))
    }
  }
  for (fn in A3_FUNCTIONS) check(paste(tag, "A3", fn), fit_individual_function(d, fn, 1, unique(d$id)[1:5]))
  check(paste(tag, "A3 compare"), compare_individual_functions(d, 1))
  check(paste(tag, "decomposition"), decomposition_trajectory(d))
  check(paste(tag, "random display"), random_display(m, TRUE))
  res <- NULL
  if (fit_models) {
    av <- model_availability(d, m)
    sel <- intersect(models, MODEL_IDS[av$ok])
    res <- check(paste(tag, "models"), fit_model_suite(d, m, sel, "Quadratic", family, FALSE, TRUE, "~1"))
    if (!is.null(res) && isTRUE(res$ok)) {
      ages <- prediction_ages(res$data$age)
      for (mm in names(res$fits)) check(paste(tag, "predict", mm), predict_population_curve(res$fits[[mm]], res, ages))
      code <- check(paste(tag, "code"), model_r_code(res, m, "x.csv"))
      if (!is.null(code)) check(paste(tag, "code parses"), parse(text = code))
      check(paste(tag, "definitions"), model_definition_table("Quadratic", m$covars, res$random, m$cov_labels))
      expect(paste(tag, "random-effect table has variances"), is.data.frame(res$varcomp) && "Variance" %in% names(res$varcomp))
    } else if (!is.null(res)) {
      fails[[paste(tag, "models not ok")]] <<- paste(res$message, paste(unlist(res$status), collapse = " | "))
    }
  }
  list(data = d, meta = m, res = res)
}

# ---------------------------------------------------------------------------
# 1. Simulated data: every trait type, ageing form and option
# ---------------------------------------------------------------------------
has_tmb <- HAS_GLMMTMB
for (tr in TOY_TRAITS) {
  for (opt in c("plain", "all_options")) {
    for (mi in c("complete", "mcar")) {
      cfg <- list(trait = tr, missingness = mi, n_id = 150, seed = 11,
                  afr_mode = if (opt == "all_options") "individual" else "same",
                  sa_type = if (opt == "all_options") "both" else "none",
                  diet = opt == "all_options", groups = opt == "all_options")
      raw <- check(paste(tr, opt, mi, "simulate"), simulate_toy_data(cfg))
      if (is.null(raw)) next
      fam <- toy_family(tr)
      fit <- identical(fam, "gaussian") || has_tmb
      out <- run_pipeline(raw, toy_mapping(raw), fam, paste(tr, opt, mi), fit_models = fit)
      if (!is.null(out$res) && isTRUE(out$res$ok)) {
        cat(sprintf("%-11s %-12s %-9s %-8s best = %s\n", tr, opt, mi, fam, out$res$aic$Model[[1]]))
      }
    }
  }
}
for (mi in c("mwo", "mwy", "trait", "condition")) {
  raw <- check(paste("mass", mi, "simulate"), simulate_toy_data(list(trait = "mass", missingness = mi, n_id = 150, seed = 5)))
  if (!is.null(raw)) run_pipeline(raw, toy_mapping(raw), "gaussian", paste("mass", mi), models = c("M1", "M2", "M4", "M5"))
}
for (fm in AGE_FUNCTIONS) {
  for (sdt in c("none", "independent", "dependent", "both")) {
    for (tr in c("mass", "count_pois")) {
      raw <- check(paste("form", fm, sdt, tr), simulate_toy_data(list(trait = tr, form = fm, sd_type = sdt, sd_dir = -1,
                                                                        mean_ls = 8, n_id = 120, seed = 21)))
      if (!is.null(raw)) {
        expect(sprintf("form %s / %s / %s: finite trait values and a finite true curve", fm, sdt, tr),
               all(is.finite(raw[[3]])) && all(is.finite(toy_true_curve(attr(raw, "truth"), sort(unique(raw$age))))))
      }
    }
  }
  raw <- simulate_toy_data(list(trait = "mass", form = fm, sd_type = "dependent", n_id = 150, seed = 4))
  run_pipeline(raw, toy_mapping(raw), "gaussian", paste("form pipeline", fm), models = c("M1", "M4"))
}

# Simulator checks
raw <- simulate_toy_data(list(trait = "mass", missingness = "mcar", n_id = 400, seed = 2))
b <- standardise_data(raw, toy_mapping(raw))
g <- build_missing_grid(b$data, 0, b$meta$map$start_mode, b$meta$map$start_age)
expect(sprintf("MCAR toy: about 25%% of expected occasions missing (got %.1f%%)", 100 * mean(g$missing)), abs(mean(g$missing) - 0.25) < 0.03)
for (sat in c("none", "independent", "both")) {
  for (dr in c(1, -1)) {
    raw <- simulate_toy_data(list(trait = "mass", sd_type = "none", afr_mode = "individual", sa_type = sat, sa_dir = dr, n_id = 400, seed = 3))
    d0 <- standardise_data(raw, toy_mapping(raw))$data
    im <- individual_metrics(d0)
    span <- max(d0$entry)                                  # every included individual is alive and sampled at this age
    at <- d0[d0$age == span, , drop = FALSE]
    r_tr <- cor_safe(im$entry[match(at$id, im$id)], at$trait)
    r_al <- cor_safe(im$entry, im$alr)
    expect(sprintf("AFR %s (direction %d): r(AFR, trait at age %d) = %.2f has the expected sign; r(AFR, ALR) = %.2f near 0",
                   sat, dr, span, r_tr, r_al),
           length(unique(im$entry)) > 3 && abs(r_al) < 0.15 && (if (sat == "none") abs(r_tr) < 0.2 else sign(r_tr) == dr))
  }
}
raw <- simulate_toy_data(list(trait = "mass", afr_mode = "same", n_id = 100, seed = 3))
expect("same AFR: no AFR column and everyone starts at age 1", !"AFR" %in% names(raw) && min(raw$age) == 1)
for (L in c(3, 10, 30)) {
  raw <- simulate_toy_data(list(trait = "mass", mean_ls = L, n_id = 300, seed = 6))
  recs <- as.numeric(table(raw$ID))
  expect(sprintf("mean lifespan %d: about %d records per individual (got %.1f)", L, L, mean(recs)), abs(mean(recs) - L) < 0.15 * L + 0.6)
}
raw <- simulate_toy_data(list(trait = "mass", diet = TRUE, sd_type = "none", n_id = 600, seed = 7))
ind <- raw[!duplicated(raw$ID), ]
r_dl <- cor_safe(as.numeric(ind$diet == "Poor"), as.numeric(ind$lifespan))
expect(sprintf("diet affects the trait only: r(diet, lifespan) = %.2f", r_dl), abs(r_dl) < 0.1)

# A1-A2 helpers with known answers
set.seed(8)
kd <- do.call(rbind, lapply(1:240, function(i) {
  ls <- c(4, 8, 12)[(i %% 3) + 1]
  a <- 1:ls
  data.frame(id = paste0("k", i), age = a, trait = 10 + (1 + 0.5 * ls / 4) * a - ifelse(i %% 2 == 1, 3, 0),
             alr = ls, life = ls, entry = 1, condition = NA_real_, condition_label = NA_character_, group = NA_character_,
             diet = ifelse(i %% 2 == 1, "Poor", "Standard"), stringsAsFactors = FALSE)
}))
kim <- individual_metrics(kd)
kfac <- stats::setNames(paste0("diet: ", kd$diet[!duplicated(kd$id)]), kd$id[!duplicated(kd$id)])
expect("max_bins_for counts distinct ALR values with enough individuals (3 values -> 3)", max_bins_for(kim$alr, 3, 1) == 3)
expect("max_bins_for: 12 sampling ages with >= 3 individuals", max_bins_for(id_age_means(kd)$age, 3, 1) == 12)
ks <- binned_trajectory(kd, proxy_values(kim, "ALR"), 3, "equal", 3, facet = kfac)
kdz <- bin_differences(ks, "successive")
ktr <- bin_difference_trends(kdz)
expect("difference trends: slope gap 0.5 per age in both diet panels", nrow(ktr) == 4 && all(abs(ktr$Slope_per_age - 0.5) < 1e-8))
kz <- trait_by_age_bins(kd, proxy_values(kim, "LS"), 12, facet = kfac)
ktab <- a2_bin_slopes(kz)
expect("A2 coefficient table: one row per panel x age bin with finite coefficients where 3+ lifespans overlap",
       nrow(ktab) > 0 && all(c("Coefficient", "Change_from_previous") %in% names(ktab)))
check("A2 coefficient trend", a2_slope_trend(ktab))

# A3: both reconstructions
for (fn in A3_FUNCTIONS) {
  z <- check(paste("A3 reconstructions", fn), fit_individual_function(kd, fn, 1, unique(kd$id)[1:3]))
  if (!is.null(z) && nrow(z$mean_curve)) {
    gap <- max(abs(z$mean_curve$fitted - z$mean_curve$fitted_fun), na.rm = TRUE)
    if (identical(fn, A3_NONLINEAR)) {
      notes <- c(notes, sprintf("[INFO] A3 %s: max gap between reconstructions = %.3g", fn, gap))
    } else {
      expect(sprintf("A3 %s (linear in parameters): mean of coefficients equals mean of functions (max gap %.2g)", fn, gap), gap < 1e-6)
    }
  }
}

# ---------------------------------------------------------------------------
# New modelling options and disappearance diagnostics (this round)
# ---------------------------------------------------------------------------
raw <- simulate_toy_data(list(trait = "mass", sd_type = "dependent", afr_mode = "individual", sa_type = "independent", diet = TRUE, n_id = 250, seed = 31))
mp <- utils::modifyList(toy_mapping(raw), list(cov_age = "diet"))
bb <- standardise_data(raw, mp)
d <- bb$data
m <- bb$meta
expect("covariate x age: diet recorded as interacting with age", identical(m$cov_age, m$covars[m$cov_labels == "diet"]))
fs <- model_formula_strings(c("f1", "f2"), m$covars, "same", m$cov_age)
expect("formulas: Models 9 and 10 exist and polynomial among terms use ALR2", all(c("M9", "M10") %in% names(fs)) && grepl("ALR2", fs[["M4"]]))
expect("formulas: covariate x age terms added to every model", all(grepl(paste0(m$cov_age, ":("), fs, fixed = TRUE)))
ex <- apply_extra_terms(fs, c("f1", "f2"), list(models = c("M5", "M6"), terms = c("ALR", "AFR_x_age")), "linear")
expect("extra terms: ALR added to Models 5 and 6 only", grepl("ALR", ex[["M5"]]) && grepl("AFR", ex[["M6"]]) && identical(ex[["M1"]], fs[["M1"]]))
expect("nesting: Model 4 nested in Model 9, Model 5 not nested in Model 4",
       formula_nested(model_formula_strings(c("f1", "f2"))[["M4"]], model_formula_strings(c("f1", "f2"))[["M9"]]) &&
       !formula_nested(model_formula_strings(c("f1", "f2"))[["M5"]], model_formula_strings(c("f1", "f2"))[["M4"]]))
sup <- individual_data_support(d)
adv <- random_slope_advice(sup)
expect(sprintf("random-slope advice returns a level (%s)", adv$level), adv$level %in% c("none", "limited", "good"))
for (rs in c("none", "uncorrelated", "correlated", "auto")) {
  res <- check(paste("random structure", rs), fit_model_suite(d, m, c("M1", "M4", "M9", "M10"), "Quadratic", "gaussian", rs, TRUE, "~1",
                                                          among = if (identical(rs, "auto")) "same" else "linear",
                                                          extra = list(models = "M4", terms = "AFR")))
  if (!is.null(res) && isTRUE(res$ok)) {
    expect(paste("random structure", rs, "-> fit validity recorded for every model"), all(names(res$fits) %in% names(res$validity)))
    expect(paste("random structure", rs, "-> eligible models ranked first"), isTRUE(res$aic$Eligible[[1]]))
    best <- sub("^Model ", "M", res$aic$Model[[1]])
    check(paste("interpretation", rs), interpret_model_terms(res, best))
    check(paste("scaling", rs), term_scaling(res$coefficients$Raw_term, res))
    check(paste("predictions by diet", rs), predict_population_curve(res$fits[[best]], res, prediction_ages(res$data$age), by = m$covars[[1]]))
    check(paste("consistency", rs), consistency_notes(res, NULL, NULL))
    check(paste("R code", rs), parse(text = model_r_code(res, m, "toy.csv")))
    if (identical(rs, "none")) check("performance checks", run_performance(res$fits[[best]], "gaussian"))
  }
}
if (requireNamespace("nlme", quietly = TRUE)) {
  rawe <- simulate_toy_data(list(trait = "mass", form = "Asymptotic exponential", sd_type = "dependent", n_id = 150, seed = 32))
  be <- standardise_data(rawe, toy_mapping(rawe))
  nl <- check("non-linear exponential models (nlme)", fit_model_suite(be$data, be$meta, c("M1", "M2", "M4"), A3_NONLINEAR, "gaussian"))
  if (!is.null(nl)) {
    expect(paste("non-linear exponential: fits or reports why not -", nl$message), isTRUE(nl$ok) || nzchar(nl$message))
    if (isTRUE(nl$ok)) {
      b1 <- sub("^Model ", "M", nl$aic$Model[[1]])
      check("non-linear predictions", predict_population_curve(nl$fits[[b1]], nl, prediction_ages(nl$data$age)))
      check("non-linear interpretation", interpret_model_terms(nl, b1))
      check("non-linear R code parses", parse(text = model_r_code(nl, be$meta, "toy.csv")))
    }
  }
}
expect("exponential simulator declines steeply early and slowly late",
       with(list(tc = attr(simulate_toy_data(list(trait = "mass", form = "Asymptotic exponential", n_id = 60, seed = 3)), "truth")),
            { v <- toy_true_curve(tc, c(1, 6, 11, 21, 31)); diff(v)[1] < diff(v)[2] && all(diff(v) < 0) }))
# disappearance diagnostics on simulated data with censoring
rawc <- simulate_toy_data(list(trait = "mass", sd_type = "both", n_id = 300, seed = 33))
rawc$alive <- ifelse(rawc$lifespan >= stats::quantile(rawc$lifespan, 0.9), "yes", "no")
bc <- standardise_data(rawc, utils::modifyList(toy_mapping(rawc), list(censor = "alive", censor_value = "yes")))
ia <- check("A4 disappearance data", disappearance_data(bc$data, bc$meta))
if (!is.null(ia)) {
  expect("A4: censored individuals have no final disappearance", !any(ia$event[ia$id %in% bc$meta$censored_ids] == 1, na.rm = TRUE))
  dm <- check("A4 hazard models", disappearance_models(ia))
  if (!is.null(dm) && isTRUE(dm$ok)) expect("A4: positive selective disappearance gives odds ratio < 1 for the trait", dm$table$Odds_ratio[dm$table$Model == "Trait now"][[1]] < 1)
  check("A6 selection differentials", selection_differentials(ia))
}
lt <- check("A7 life table", life_table(bc$data, bc$meta))
if (!is.null(lt) && isTRUE(lt$ok)) expect("A7: hazard between 0 and 1", all(lt$table$Hazard >= 0 & lt$table$Hazard <= 1))
tdat <- check("A5 terminal data", terminal_data(bc$data, bc$meta))
if (!is.null(tdat) && nrow(tdat)) expect("A5: detrended values present", "resid" %in% names(tdat))
expect("subset candidates include diet", "diet" %in% subset_candidates(raw))
# individual fits: minimum records per function (Linear 2; Quadratic and Logarithmic 3; Cubic 4)
two <- c(1, 2); three <- c(1, 2, 3)
expect("linear fit on 2 records", !is.null(individual_fit_one(two, c(3, 5), make_age_params(1:10, "Linear", FALSE))))
expect("logarithmic fit needs 3 records", is.null(individual_fit_one(two, c(3, 5), make_age_params(1:10, "Logarithmic", FALSE))) &&
         !is.null(individual_fit_one(three, c(3, 5, 4), make_age_params(1:10, "Logarithmic", FALSE))))
expect("quadratic fit on 3 records", !is.null(individual_fit_one(three, c(3, 5, 4), make_age_params(1:10, "Quadratic", FALSE))))
expect("cubic fit needs 4 records", is.null(individual_fit_one(three, c(3, 5, 4), make_age_params(1:10, "Cubic", FALSE))) &&
         !is.null(individual_fit_one(1:4, c(3, 5, 4, 6), make_age_params(1:10, "Cubic", FALSE))))
# performance checks report problems instead of stopping the app
simp <- simulate_toy_data(list(n_id = 60, seed = 5))
bp <- standardise_data(simp, toy_mapping(simp))
rp <- check("model for performance checks", fit_model_suite(bp$data, bp$meta, "M1", "Linear", "gaussian"))
if (!is.null(rp) && isTRUE(rp$ok)) {
  pz <- check("performance checks return a result", run_performance(rp$fits[["M1"]], "gaussian", time_limit = 60))
  if (!is.null(pz)) expect("performance result is a list with a status", is.list(pz) && !is.null(pz$ok))
}
bad <- check("performance checks on an unusable object do not stop", run_performance(list(), "zinb", time_limit = 5))
if (!is.null(bad)) expect("unusable object: result returned, no error", is.list(bad))
# regression test: with no covariates mapped, standardise_data() used to fail ("attempt to set an attribute on NULL")
raw0 <- simulate_toy_data(list(n_id = 40, seed = 9))
b0 <- check("standardise_data without covariates", standardise_data(raw0, utils::modifyList(toy_mapping(raw0),
                list(covars = character(0), cov_factor = character(0), cov_int = "x|||age"))))
if (!is.null(b0)) expect("no covariates: empty covariate metadata", length(b0$meta$covars) == 0 && length(b0$meta$cov_pairs) == 0)
# exact model equations: every model (Gaussian), and interaction models under count families and random slopes
lv3 <- list()
for (cv in b3$meta$covars[b3$meta$cov_types[b3$meta$covars] == "categorical"]) lv3[[cv]] <- sort(unique(as.character(stats::na.omit(b3$data[[cv]]))))
eq_settings <- function(fam, rs) list(family = fam, age_function = "Quadratic", among = "linear", random_slope = rs, zi = "~ f1 + ALR", extra = list(M5 = "ALR"))
for (mid in MODEL_IDS) {
  eq <- check(paste("equation", mid), model_equation(mid, eq_settings("gaussian", "none"), b3$meta, lv3, b3$data))
  if (!is.null(eq)) expect(paste("equation", mid, "has coefficients and R syntax"), isTRUE(eq$available) && nrow(eq$coefficients) >= 2 && nzchar(eq$r_call))
}
for (fam in c("poisson", "nbinom2", "zinb")) for (rs in c("uncorrelated", "correlated", "auto")) {
  eq <- check(paste("equation M8", fam, rs), model_equation("M8", eq_settings(fam, rs), b3$meta, lv3, b3$data))
  if (!is.null(eq)) expect(paste("equation M8", fam, rs, "parses as R"), !inherits(tryCatch(parse(text = eq$r_expanded), error = function(e) e), "error"))
}
eqn <- check("equation for the non-linear exponential", model_equation("M4", utils::modifyList(eq_settings("gaussian", "correlated"), list(age_function = A3_NONLINEAR)), b3$meta, lv3, b3$data))
if (!is.null(eqn)) expect("non-linear equation has level and rate coefficients", isTRUE(eqn$available) && any(grepl("^a\\.", eqn$coefficients$R_term)) && any(grepl("^b\\.", eqn$coefficients$R_term)))
# the two exponential functions must be different models, with different equations and availability
eq_asym <- model_equation("M4", utils::modifyList(eq_settings("gaussian", "none"), list(age_function = "Asymptotic exponential")), b3$meta, lv3, b3$data)
eq_expo <- model_equation("M4", utils::modifyList(eq_settings("gaussian", "none"), list(age_function = A3_NONLINEAR)), b3$meta, lv3, b3$data)
expect("the two exponential functions give different equations",
       !identical(eq_asym$equation, eq_expo$equation) && !identical(eq_asym$r_call, eq_expo$r_call))
expect("asymptotic exponential states its basis", grepl("exp", eq_asym$basis %||% "", fixed = TRUE) && grepl("f1", eq_asym$basis %||% "", fixed = TRUE))
expect("exponential model states that a and b are estimated", grepl("estimated", eq_expo$basis %||% "", fixed = TRUE))
for (mid in c("M3", "M5")) {
  eq_c <- model_equation(mid, utils::modifyList(eq_settings("gaussian", "none"), list(age_function = A3_NONLINEAR)), b3$meta, lv3, b3$data)
  expect(paste("centring model", mid, "is unavailable for the exponential model"), isFALSE(eq_c$available))
}
for (mid in setdiff(MODEL_IDS, c("M3", "M5"))) {
  ea <- model_equation(mid, utils::modifyList(eq_settings("gaussian", "none"), list(age_function = "Asymptotic exponential")), b3$meta, lv3, b3$data)
  ee <- model_equation(mid, utils::modifyList(eq_settings("gaussian", "none"), list(age_function = A3_NONLINEAR)), b3$meta, lv3, b3$data)
  expect(paste("model", mid, "differs between the two exponential functions"),
         isTRUE(ea$available) && isTRUE(ee$available) && !identical(ea$r_call, ee$r_call) &&
           !identical(ea$coefficients$R_term, ee$coefficients$R_term))
}
# the ageing functions must give different data columns (f1), not just different labels
ages_chk <- c(1, 2, 4, 8, 16)
bas <- lapply(AGE_FUNCTIONS, function(fn) as.numeric(age_basis(ages_chk, make_age_params(ages_chk, fn, FALSE))[, 1]))
expect("each ageing function builds a different age term",
       length(unique(lapply(bas, function(v) round(v / max(abs(v)), 6)))) == length(AGE_FUNCTIONS))
eq4 <- model_equation("M4", eq_settings("gaussian", "none"), b3$meta, lv3, b3$data)
expect("Model 4 is fully expanded (main effects and interactions each with a coefficient)",
       all(c("f1", "f2", "ALR", "f1:ALR", "f2:ALR") %in% eq4$coefficients$R_term))
if (requireNamespace("codetools", quietly = TRUE)) {
  defs <- check("helper functions for the section scripts", app_code_definitions(app_code_closure(c("standardise_data", "binned_trajectory", "life_table"))))
  if (!is.null(defs)) expect("exported helper functions parse", !inherits(tryCatch(parse(text = paste(defs, collapse = "\n")), error = function(e) e), "error"))
}
# covariate types, interactions, per-model extra terms and the per-model help
mp2 <- utils::modifyList(toy_mapping(raw), list(cov_factor = "diet", cov_int = c("diet|||age")))
b2m <- standardise_data(raw, mp2)$meta
expect("categorical covariate typed as categorical", identical(unname(b2m$cov_types[[b2m$covars[[1]]]]), "categorical"))
expect("diet x age interaction recorded", identical(b2m$cov_age, b2m$covars[[1]]))
raw$temp <- round(stats::rnorm(nrow(raw), 20, 2), 1)
mp3 <- utils::modifyList(toy_mapping(raw), list(covars = c("diet", "temp"), cov_factor = "diet", cov_int = "diet|||temp"))
b3 <- standardise_data(raw, mp3)
expect("continuous covariate stays numeric", is.numeric(b3$data[[b3$meta$covars[b3$meta$cov_labels == "temp"]]]))
expect("covariate pair recorded as an interaction", length(b3$meta$cov_pairs) == 1)
fs3 <- model_formula_strings(c("f1", "f2"), b3$meta$covars, "linear", character(0), b3$meta$cov_pairs)
expect("covariate pair added to every model", all(grepl(b3$meta$cov_pairs, fs3, fixed = TRUE)))
ex3 <- clean_extra_terms(list(M5 = c("ALR", "not_a_term"), M6 = "AFR_x_age"), b3$meta$covars)
expect("per-model extra terms cleaned", identical(ex3, list(M5 = "ALR", M6 = "AFR_x_age")))
r3 <- check("models with a covariate pair and per-model extra terms", fit_model_suite(b3$data, b3$meta, c("M1", "M4", "M5"), "Quadratic", "gaussian",
                                                                                    extra = list(M5 = "ALR")))
if (!is.null(r3) && isTRUE(r3$ok)) {
  expect("Model 5 has the added ALR term", grepl("ALR", r3$formulas[["M5"]]) && !grepl("ALR", r3$formulas[["M1"]]))
  check("R code with typed covariates parses", parse(text = model_r_code(r3, b3$meta, "toy.csv")))
}
for (mid in MODEL_IDS) {
  z <- check(paste("model help content", mid), model_info_content(mid, list(family = "gaussian", age_function = "Quadratic", among = "linear",
                                                                           random_slope = "none", extra = list(M5 = "ALR")), b3$meta))
  if (!is.null(z)) expect(paste("model help for", mid, "has a specification"), nzchar(z$spec) && nzchar(z$meaning))
}

# ---------------------------------------------------------------------------
# Break tests: small awkward datasets (irregular ages, nesting, censoring, gaps, ties)
# ---------------------------------------------------------------------------
base_map <- function(...) utils::modifyList(list(id = "ID", age = "age", trait = "y", alr = "__AUTO_LAST__", life = "LS",
                                                 entry = "__AUTO_FIRST__", condition = "", covars = character(0), group = "",
                                                 nested = TRUE, random = character(0), censor = "", censor_value = "",
                                                 start_mode = "afr", start_age = NA_real_), list(...))
set.seed(99)
bt <- list()
bt$half_years <- data.frame(ID = rep(paste0("H", 1:8), each = 4), age = rep(c(0, 0.5, 1.5, 2), 8), y = rnorm(32), LS = NA)
bt$off_grid <- data.frame(ID = rep(paste0("O", 1:6), each = 4), age = rep(c(1, 2.5, 4, 6.2), 6), y = rnorm(24), LS = NA)
bt$nested_ids <- data.frame(fam = rep(c("A", "B", "C"), each = 15), ID = rep(rep(as.character(1:3), each = 5), 3),
                            age = rep(1:5, 9), y = rnorm(45), LS = 5, year = rep(c("y1", "y2", "y3"), 15))
bt$censored <- data.frame(ID = rep(paste0("C", 1:8), each = 4), age = rep(1:4, 8), y = rpois(32, 3), LS = 10,
                          Censored = rep(c(1, 1, 1, 1, 1, 1, 0, 0), each = 4))
bt$gaps_and_na <- data.frame(ID = c("a", "a", "a", "b", "c", "c", "c", "d", "d", "d", "e", "e"),
                             age = c(1, 2, 2, 1, 1, 2, 3, 1, 3, 4, 2, 4), y = c(1, NA, 5, 2, 3, 4, NA, 2, 3, 1, 6, 7),
                             LS = c(2, 2, 2, 1, 3, 3, 3, 4, 4, 4, 1, 1))
bt$late_starters <- data.frame(ID = rep(paste0("Y", 1:6), each = 4), age = rep(3:6, 6), y = rnorm(24), LS = 6)
bt$constant_alr <- data.frame(ID = rep(paste0("K", 1:5), each = 3), age = rep(1:3, 5), y = rnorm(15), LS = 3)
bt$tied_alr <- do.call(rbind, lapply(1:50, function(i) {
  ls <- if (i <= 40) 5 else 6
  data.frame(ID = paste0("T", i), age = 1:ls, y = 1:ls + rnorm(ls), LS = ls)
}))
bt$numeric_ids_text_ages <- data.frame(ID = rep(101:110, each = 3), age = as.character(rep(c(10, 20, 30), 10)),
                                       y = rnorm(30), LS = NA)
for (nm in names(bt)) {
  df <- bt[[nm]]
  mp <- switch(nm,
    nested_ids = base_map(group = "fam", random = "year"),
    censored = base_map(censor = "Censored", censor_value = "0", start_mode = "same", start_age = 1),
    late_starters = base_map(start_mode = "same", start_age = 1),
    base_map())
  b <- check(paste("break", nm, "standardise"), standardise_data(df, mp))
  if (is.null(b)) next
  d <- b$data
  check(paste("break", nm, "integrity"), data_integrity(d, b$meta))
  g <- check(paste("break", nm, "grid"), build_missing_grid(d, 0, mp$start_mode, mp$start_age))
  if (!is.null(g)) check(paste("break", nm, "missingness"), missingness_summary(d, g, 0.05, TRUE))
  im <- individual_metrics(d)
  for (px in c("ALR", "Mean age")) {
    pv <- proxy_values(im, px)
    s <- check(paste("break", nm, "A1", px), binned_trajectory(d, pv, 4, "quantile", 1))
    if (!is.null(s)) check(paste("break", nm, "diff", px), bin_differences(s, "all"))
    check(paste("break", nm, "A2", px), trait_by_age_bins(d, pv, 3))
  }
  check(paste("break", nm, "A3"), fit_individual_function(d, "Quadratic", 0, unique(d$id)[1:2]))
  check(paste("break", nm, "decomposition"), decomposition_trajectory(d))
}
bn <- standardise_data(bt$nested_ids, base_map(group = "fam", random = "year"))
expect("break nested: 9 distinct individuals from repeated IDs", length(unique(bn$data$id)) == 9)
bc <- standardise_data(bt$censored, base_map(censor = "Censored", censor_value = "0"))
expect("break censored: 2 censored individuals have LS set to NA", bc$meta$n_censored == 2 && sum(!is.finite(individual_metrics(bc$data)$lifespan)) == 2)
bl <- standardise_data(bt$late_starters, base_map(start_mode = "same", start_age = 1))
gl <- build_missing_grid(bl$data, 0, "same", 1)
expect("break late starters: ages 1-2 counted as missing (2 of 6 occasions)", isTRUE(all.equal(mean(gl$missing), 2 / 6)))
bh <- standardise_data(bt$half_years, base_map(life = ""))
gh <- build_missing_grid(bh$data)
expect("break half-year ages: missing age 1.0 is 'between records' for all 8", sum(gh$age == 1 & gh$pattern == "Missed: between records") == 8)
if (TRUE) {
  gm <- check("break nested + extra random intercept model",
              fit_model_suite(bn$data, bn$meta, c("M1", "M2"), "Linear", "gaussian"))
  if (!is.null(gm)) expect("break nested model: random structure includes (1 | re_year)", grepl("re_year", gm$random %||% ""))
}

# Expected teaching patterns (dramatic, age-dependent selection, complete sampling)
raw <- simulate_toy_data(list(trait = "mass", sd_type = "both", sd_dir = 1, n_id = 300, seed = 1))
out <- run_pipeline(raw, toy_mapping(raw), "gaussian", "teaching check", models = c("M1", "M2", "M3", "M4", "M5", "M6"))
im <- individual_metrics(out$data)
a2 <- proxy_slopes_by_age(out$data, proxy_values(im, "ALR"), "slope")
expect("dramatic toy: A2 slope changes with age (trend p < 0.05)", is.finite(a2$trend_p) && a2$trend_p < 0.05)
if (!is.null(out$res) && isTRUE(out$res$ok)) {
  expect("dramatic toy: an interaction model (4, 5 or 6) is best", out$res$aic$Model[[1]] %in% c("Model 4", "Model 5", "Model 6"))
  expect("dramatic toy: Model 1 has the highest AIC", tail(out$res$aic$Model, 1) == "Model 1")
  fcmp <- check("within-model function comparison (Model 4)",
                compare_population_functions(out$data, out$meta, functions = c("Linear", "Quadratic", "Cubic"), model = "M4"))
  if (!is.null(fcmp)) expect("within-model function comparison: the simulated quadratic is within 2 AIC of the best",
                             isTRUE(fcmp$table$Delta_AIC[fcmp$table$Function == "Quadratic"] < 2))
}

# ---------------------------------------------------------------------------
# 2. Fruit-fly data: integrity diagnostics and the manuscript's reanalysis
# ---------------------------------------------------------------------------
fly <- load_fly_example()
if (is.null(fly)) {
  fails[["fly data"]] <- "data/fly_fecundity.csv not found"
} else {
  b <- standardise_data(fly, FLY_MAPPING)
  integ <- data_integrity(b$data, b$meta)
  print(integ$table[, c("Check", "Result", "Status")], row.names = FALSE)
  expect("fly: 5 duplicate ID x age rows", b$meta$n_dup == 5)
  expect("fly: 1 ID linked to two fathers", b$meta$n_multi_group == 1)
  expect("fly: 4 individuals with LS < last record", length(integ$bad_ls_ids) == 4)
  expect("fly: zero-inflated NB suggested", identical(integ$family$family, "zinb"))
  av <- model_availability(b$data, b$meta)
  expect("fly: default models are 1-5", identical(MODEL_IDS[av$default], paste0("M", 1:5)))

  g_fit <- check("fly gaussian", fit_model_suite(b$data, b$meta, paste0("M", 1:5), "Quadratic", "gaussian"))
  if (!is.null(g_fit) && isTRUE(g_fit$ok)) {
    cat("\nFly data, Gaussian:\n"); print(g_fit$aic[, c("Model", "AIC", "Delta_AIC", "df")], row.names = FALSE)
    expect("fly Gaussian: Model 5 best (Python replication: Model 4 is ~19 AIC worse)", g_fit$aic$Model[[1]] == "Model 5")
  }
  if (has_tmb) {
    z_fit <- check("fly zinb", fit_model_suite(b$data, b$meta, paste0("M", 1:5), "Quadratic", "zinb"))
    if (!is.null(z_fit) && isTRUE(z_fit$ok)) {
      cat("\nFly data, zero-inflated negative binomial:\n"); print(z_fit$aic[, c("Model", "AIC", "Delta_AIC", "df")], row.names = FALSE)
      print(unlist(z_fit$status))
      dA <- stats::setNames(z_fit$aic$Delta_AIC, z_fit$aic$Model)
      expect("fly ZINB: Model 4 best (manuscript)", z_fit$aic$Model[[1]] == "Model 4")
      expect("fly ZINB: dAIC Model 1 about 38.4", abs(dA[["Model 1"]] - 38.4) < 3)
      expect("fly ZINB: dAIC Model 2 about 24.1", abs(dA[["Model 2"]] - 24.1) < 3)
      expect("fly ZINB: dAIC Model 3 about 28.0", abs(dA[["Model 3"]] - 28.0) < 3)
      expect("fly ZINB: dAIC Model 5 about 2.7", abs(dA[["Model 5"]] - 2.7) < 2)
      fc <- check("fly family check", compare_families(b$data, b$meta, "M4", families = c("poisson", "nbinom2", "zinb")))
      if (!is.null(fc) && isTRUE(fc$ok)) print(fc$table, row.names = FALSE)
    }
  } else {
    notes <- c(notes, "[SKIP] glmmTMB not installed: fly ZINB replication not run")
  }
}

# ---- 0.9.4: binary binomial without trials, zinb1, three-level nesting, built terms, simulator shapes ----
local({
  set.seed(94)
  n_id <- 150
  fam_i <- sprintf("F%02d", sample(1:15, n_id, replace = TRUE))
  sire_i <- sprintf("S%d", sample(1:3, n_id, replace = TRUE))   # sire labels restart within each family
  ls_i <- sample(3:8, n_id, replace = TRUE)
  raw <- do.call(rbind, lapply(seq_len(n_id), function(i) {
    data.frame(ID = sprintf("I%02d", i %% 50), family = fam_i[i], sire = sire_i[i], age = seq_len(ls_i[i]),
               lifespan = ls_i[i], treat = if (i %% 2) "a" else "b", stringsAsFactors = FALSE)
  }))
  raw$y <- stats::rbinom(nrow(raw), 1, stats::plogis(0.5 - 0.3 * raw$age + 0.2 * raw$lifespan))
  raw$cnt <- ifelse(stats::runif(nrow(raw)) < 0.3, 0L, stats::rnbinom(nrow(raw), size = 2, mu = exp(2 - 0.15 * raw$age)))
  map <- list(id = "ID", age = "age", trait = "y", alr = "__AUTO_LAST__", life = "lifespan", entry = "__AUTO_FIRST__",
              condition = "", covars = "treat", cov_factor = "treat", cov_int = character(0), group = "sire", group2 = "family",
              nested = TRUE, random = character(0), censor = "", censor_value = "", age_round = NA_real_, cov_age = character(0))
  b <- check("0.9.4 three-level standardise", standardise_data(raw, map))
  if (is.null(b)) return(invisible(NULL))
  n_true <- nrow(unique(raw[, c("family", "sire", "ID")]))
  expect("0.9.4 three-level nesting: second level detected", isTRUE(b$meta$has_group2))
  expect("0.9.4 three-level nesting: one individual per family/sire/ID", length(unique(b$data$id)) == n_true)
  expect("0.9.4 individual_ids() matches standardise_data()", setequal(unique(individual_ids(raw, map)), unique(b$data$id)))
  expect("0.9.4 random-effect string has three levels",
         identical(random_term_string("f1", "none", TRUE, character(0), TRUE), "(1 | group2) + (1 | group) + (1 | id)"))
  expect("0.9.4 binary trait suggested as binomial", identical(suggest_family(b$data$trait, b$data$age)$family, "binomial"))
  fs <- apply_extra_terms(model_formula_strings(c("f1", "f2")), c("f1", "f2"),
                          list(M1 = "term:ALR*AFR*age", M5 = "term:cv_treat+ALR"), "linear")
  expect("0.9.4 built three-way term", identical(fs[["M1"]], "f1 + f2 + ALR * AFR * (f1 + f2)"))
  expect("0.9.4 built additive terms", identical(fs[["M5"]], "mean_f1 * (delta_f1 + delta_f2) + cv_treat + ALR"))
  ce <- clean_extra_terms(list(M1 = c("term:ALR*cv_treat", "term:bad*age", "term:ALR*AFR*LS*age")), "cv_treat")
  expect("0.9.4 built terms validated", identical(ce, list(M1 = "term:ALR*cv_treat")))
  w <- covariate_type_warnings(data.frame(a = rep(c("x", "y"), 5), b = seq(0.5, 5, by = 0.5), stringsAsFactors = FALSE), num = "a", fac = "b")
  expect("0.9.4 covariate box warnings", length(w) == 2)
  bd <- lm_band(1:10, (1:10) + stats::rnorm(10))
  expect("0.9.4 confidence band", nrow(bd) == 50 && all(bd$lo <= bd$fit & bd$fit <= bd$hi))
  expect("0.9.4 default shape reproduces the original simulation",
         identical(simulate_toy_data(list(seed = 3, n_id = 60)), simulate_toy_data(list(seed = 3, n_id = 60, shape = "default"))))
  for (fm in names(TOY_SHAPES)) for (sh in names(TOY_SHAPES[[fm]])) for (tr in c("mass", "count_nb")) {
    z <- check(paste("0.9.4 shape", fm, sh, tr), simulate_toy_data(list(form = fm, shape = sh, trait = tr, n_id = 60, seed = 2)))
    if (!is.null(z)) expect(paste("0.9.4 shape", fm, sh, tr, "has data"), nrow(z) > 100 && all(is.finite(z[[3]])))
  }
  if (HAS_GLMMTMB) {
    rb <- check("0.9.4 binary binomial fit", fit_model_suite(b$data, b$meta, c("M1", "M2", "M4"), "Linear", "binomial"))
    expect("0.9.4 binary 0/1 trait fits with the binomial family", isTRUE(rb$ok))
    expect("0.9.4 three-level random effects in the fitted models", isTRUE(grepl("(1 | group2) + (1 | group)", rb$random, fixed = TRUE)))
    b2 <- check("0.9.4 counts standardise", standardise_data(raw, utils::modifyList(map, list(trait = "cnt"))))
    if (!is.null(b2)) {
      rz <- check("0.9.4 zinb1 fit", fit_model_suite(b2$data, b2$meta, c("M1", "M2"), "Linear", "zinb1"))
      expect("0.9.4 zero-inflated nbinom1 fits", isTRUE(rz$ok))
    }
  } else {
    notes <<- c(notes, "[SKIP] glmmTMB not installed: 0.9.4 binomial and zinb1 fits not run")
  }
})

# ---- 0.9.5: Soay sheep and leafcutting-bee examples, and the decomposition on an unequal first interval ----
local({
  for (k in c("mckennaell_breeding", "mckennaell_weight", "szejnersigal_activity")) {
    ex <- EXAMPLES[[k]]
    raw <- check(paste("0.9.5 load", k), load_example_file(ex$file))
    expect(paste("0.9.5", k, "mapping resolves"), is.data.frame(raw) &&
             all(c(ex$mapping$id, ex$mapping$age, ex$mapping$trait, ex$mapping$covars, ex$mapping$random) %in% names(raw)))
  }
  coef_of <- function(res, pattern) {
    ct <- res$coefficients
    ct$Estimate[ct$Model == "Model 2" & grepl(pattern, ct$Raw_term)][1]
  }
  wt <- load_example_file(EXAMPLES$mckennaell_weight$file)
  bw <- check("0.9.5 sheep weight standardise", standardise_data(wt, EXAMPLES$mckennaell_weight$mapping))
  if (!is.null(bw)) {
    rw <- check("0.9.5 sheep weight fit", fit_model_suite(bw$data, bw$meta, "M2", "Linear", "gaussian", standardise = FALSE))
    expect("0.9.5 sheep weight fits", isTRUE(rw$ok))
    if (isTRUE(rw$ok)) {
      expect("0.9.5 sheep weight: age -0.061", isTRUE(abs(coef_of(rw, "^f1$") + 0.061) < 0.005))
      expect("0.9.5 sheep weight: age at last observation +0.016", isTRUE(abs(coef_of(rw, "^ALR$") - 0.016) < 0.005))
      expect("0.9.5 sheep weight: twin -0.826", isTRUE(abs(coef_of(rw, "TwinStatus2$") + 0.826) < 0.01))
      expect("0.9.5 sheep weight: capture age +0.112", isTRUE(abs(coef_of(rw, "CaptureAge$") - 0.112) < 0.005))
    }
  }
  if (HAS_GLMMTMB) {
    fe <- load_example_file(EXAMPLES$mckennaell_breeding$file)
    bf <- check("0.9.5 sheep breeding standardise", standardise_data(fe, EXAMPLES$mckennaell_breeding$mapping))
    if (!is.null(bf)) {
      rf <- check("0.9.5 sheep breeding fit", fit_model_suite(bf$data, bf$meta, "M2", "Linear", "binomial", standardise = FALSE))
      expect("0.9.5 sheep breeding probability fits (binary 0/1, binomial)", isTRUE(rf$ok))
      if (isTRUE(rf$ok)) {
        expect("0.9.5 sheep breeding: age -0.487", isTRUE(abs(coef_of(rf, "^f1$") + 0.487) < 0.05))
        expect("0.9.5 sheep breeding: age at last observation +0.188", isTRUE(abs(coef_of(rf, "^ALR$") - 0.188) < 0.03))
        expect("0.9.5 sheep breeding: age x bred as a yearling -0.210", isTRUE(abs(coef_of(rf, "BredYearling1:f1") + 0.210) < 0.03))
      }
    }
    bs <- check("0.9.5 sheep survival standardise",
                standardise_data(fe, utils::modifyList(EXAMPLES$mckennaell_breeding$mapping, list(trait = "OffspringRecruitment"))))
    if (!is.null(bs)) {
      rs <- check("0.9.5 sheep survival fit", fit_model_suite(bs$data, bs$meta, "M2", "Linear", "binomial", standardise = FALSE))
      expect("0.9.5 sheep offspring survival fits (binomial)", isTRUE(rs$ok))
      if (isTRUE(rs$ok)) {
        expect("0.9.5 sheep survival: n = 2573", identical(as.integer(rs$aic$N[[1]]), 2573L))
        expect("0.9.5 sheep survival: age -0.232", isTRUE(abs(coef_of(rs, "^f1$") + 0.232) < 0.05))
        expect("0.9.5 sheep survival: early-life recruitment +0.869", isTRUE(abs(coef_of(rs, "EarlyLifeRec$") - 0.869) < 0.1))
      }
    }
  } else {
    notes <<- c(notes, "[SKIP] glmmTMB not installed: 0.9.5 Soay sheep binomial replications not run")
  }
  be <- load_example_file(EXAMPLES$szejnersigal_activity$file)
  if (is.data.frame(be)) {
    bb <- check("0.9.5 bee standardise", standardise_data(be[be$sex == "f", , drop = FALSE], EXAMPLES$szejnersigal_activity$mapping))
    if (!is.null(bb)) {
      expect("0.9.5 bee sampling step is weekly (7 days)", isTRUE(abs(infer_age_step(bb$data$age, bb$data$id) - 7) < 1e-8))
      dc <- check("0.9.5 bee decomposition", decomposition_trajectory(bb$data))
      expect("0.9.5 bee decomposition runs through the weekly occasions, not only days 1 and 7", is.data.frame(dc) && nrow(dc) >= 7)
      rb <- check("0.9.5 bee fit", fit_model_suite(bb$data, bb$meta, c("M1", "M2", "M6"), "Quadratic", "gaussian",
                                                   extra = EXAMPLES$szejnersigal_activity$extra))
      expect("0.9.5 bee models fit", isTRUE(rb$ok))
      if (isTRUE(rb$ok) && !is.null(rb$fits[["M1"]])) {
        cv <- predict_population_curve(rb$fits[["M1"]], rb, 1:56)
        pk <- cv$age[which.max(cv$fitted)]
        expect("0.9.5 bee female activity peaks in mid-life (about 26 days)", length(pk) == 1 && pk >= 20 && pk <= 32)
      }
    }
  }
  fl <- load_example_file("sanghvi_2022_beetle_female_fecundity.csv")
  if (is.data.frame(fl)) {
    bl <- standardise_data(fl, EXAMPLES$sanghvi_female$mapping)
    dl <- decomposition_trajectory(bl$data)
    expect("0.9.5 decomposition on a regular daily schedule links every consecutive day", nrow(dl) >= 5 && all(abs(diff(dl$age) - 1) < 1e-8))
  }
})

# ---- 0.9.6: decomposition on regular, missing, staggered-entry, unequal and irregular schedules ----
local({
  sc <- list(complete = list(sd_type = "none"), mcar = list(sd_type = "none", missingness = "mcar"),
             missing_when_old = list(sd_type = "none", missingness = "mwo"), individual_afr = list(sd_type = "none", afr_mode = "individual"))
  for (nm in names(sc)) {
    raw <- simulate_toy_data(utils::modifyList(list(n_id = 400, seed = 5), sc[[nm]]))
    b <- standardise_data(raw, toy_mapping(raw))
    de <- check(paste("0.9.6 decomposition", nm), decomposition_trajectory(b$data))
    if (is.data.frame(de) && nrow(de)) {
      obs <- observed_trajectory(b$data)
      ok <- de$age %in% obs$age[obs$n >= 20]
      tr <- toy_true_curve(attr(raw, "truth"), de$age)
      D <- 100 * mean(abs(de$fitted[ok] - tr[ok]) / abs(tr[ok]))
      expect(sprintf("0.9.6 decomposition tracks the truth without selective disappearance (%s): mean |D| = %.1f%%", nm, D), is.finite(D) && D < 5)
      expect(sprintf("0.9.6 decomposition starts at the first age (%s)", nm), isTRUE(de$age[[1]] <= min(b$data$age) + 1e-8))
      expect(sprintf("0.9.6 no caution on a regular schedule (%s)", nm), is.null(attr(de, "caution")))
    }
  }
  raw <- simulate_toy_data(list(n_id = 400, seed = 6, sd_type = "none"))
  set.seed(1)
  jit <- raw
  jit$age <- jit$age + stats::runif(nrow(jit), -0.35, 0.35)
  bj <- standardise_data(jit, toy_mapping(jit))
  dj <- check("0.9.6 decomposition, irregular ages", decomposition_trajectory(bj$data))
  expect("0.9.6 irregular ages: the decomposition runs across the ages and carries a caution",
         is.data.frame(dj) && nrow(dj) >= 20 && length(attr(dj, "caution")) == 1)
  off <- raw[raw$age == 1 | raw$age %% 3 == 0, , drop = FALSE]
  bo <- standardise_data(off, toy_mapping(off))
  dof <- check("0.9.6 decomposition, age 1 then every third age", decomposition_trajectory(bo$data))
  expect("0.9.6 unequal first interval: the decomposition runs past the first two occasions", is.data.frame(dof) && nrow(dof) >= 8)
  one <- raw[!duplicated(raw$ID), , drop = FALSE]
  bs <- standardise_data(one, toy_mapping(one))
  ds <- check("0.9.6 decomposition, one record per individual", decomposition_trajectory(bs$data))
  expect("0.9.6 one record per individual: no decomposition, no error", is.data.frame(ds) && !nrow(ds))
  code <- check("0.9.6 exported model script", {
    r <- fit_model_suite(b$data, b$meta, c("M1", "M2"), "Quadratic", "gaussian")
    if (isTRUE(r$ok)) model_r_code(r, b$meta) else ""
  })
  expect("0.9.6 exported model script defines and calls the app's decomposition",
         is.character(code) && grepl("decomposition_trajectory <- function", code, fixed = TRUE) &&
           grepl("decomposition_on_grid <- function", code, fixed = TRUE) && grepl("decomp <- decomposition_trajectory(dat)", code, fixed = TRUE))
  if (is.character(code) && nzchar(code)) {
    pe <- tryCatch({ parse(text = code); TRUE }, error = function(e) conditionMessage(e))
    expect("0.9.6 exported model script parses", isTRUE(pe))
  }
})

# ---- 0.9.7: version stamp, optional packages, P type, rows before fitting ----
local({
  expect("0.9.7 version constant is set", is.character(DISAPPR_VERSION) && length(DISAPPR_VERSION) == 1 && nzchar(DISAPPR_VERSION))
  st <- check("0.9.7 optional package status", optional_package_status())
  expect("0.9.7 optional package status lists packages and what they add",
         is.data.frame(st) && all(c("Package", "Installed", "Needed_for") %in% names(st)) && nrow(st) >= 5)
  raw <- simulate_toy_data(list(n_id = 150, seed = 3))
  b <- standardise_data(raw, toy_mapping(raw))
  pv <- check("0.9.7 rows before fitting (dry run)", fit_model_suite(b$data, b$meta, c("M1", "M2", "M6"), "Quadratic", "gaussian", dry_run = TRUE))
  expect("0.9.7 dry run reports shared rows and individuals without fitting",
         isTRUE(pv$ok) && isTRUE(pv$dry_run) && is.null(pv$fits) && pv$n_rows_used <= pv$n_rows && pv$n_ids_used <= pv$n_ids)
  r <- check("0.9.7 Gaussian fit", fit_model_suite(b$data, b$meta, c("M1", "M2"), "Quadratic", "gaussian"))
  if (isTRUE(r$ok)) {
    expect("0.9.7 coefficient table states the p-value type",
           "P_method" %in% names(r$coefficients) && all(r$coefficients$P_method %in% c("t, Satterthwaite df", "Wald z, asymptotic")))
    expect("0.9.7 individuals lost entirely are reported", is.numeric(r$n_ids_dropped) && r$n_ids_dropped >= 0)
    code <- check("0.9.7 exported code", model_r_code(r, b$meta))
    expect("0.9.7 exported code records the disappR version", is.character(code) && grepl(paste0("disappR ", DISAPPR_VERSION), code, fixed = TRUE))
  }
})

# ---- 0.9.8: data guards, duplicate terms, row-order-free curves, analysis data, RNG, notes ----
local({
  set.seed(99); before <- .Random.seed
  raw <- simulate_toy_data(list(n_id = 80, seed = 2, sd_type = "dependent"))
  expect("0.9.8 simulating leaves R's random-number stream unchanged", identical(before, .Random.seed))
  m <- toy_mapping(raw)
  cst <- raw; cst[[m$trait]] <- 5
  bc <- standardise_data(cst, toy_mapping(cst))
  rc <- check("0.9.8 constant trait", fit_model_suite(bc$data, bc$meta, c("M1", "M2"), "Quadratic", "gaussian"))
  expect("0.9.8 a constant trait stops with a message", is.list(rc) && !isTRUE(rc$ok) && grepl("single value", rc$message %||% ""))
  one <- do.call(rbind, lapply(split(raw, raw$ID), function(x) x[nrow(x), , drop = FALSE]))
  bo <- standardise_data(one, toy_mapping(one))
  ro <- check("0.9.8 one record per individual", fit_model_suite(bo$data, bo$meta, c("M1", "M2"), "Linear", "gaussian"))
  expect("0.9.8 one record per individual stops with a message", is.list(ro) && !isTRUE(ro$ok) && grepl("single record", ro$message %||% ""))
  dup <- raw; dup$alr_copy <- stats::ave(dup$age, dup$ID, FUN = max)
  md <- toy_mapping(dup); md$covars <- "alr_copy"
  bd <- standardise_data(dup, md)
  pv <- check("0.9.8 dry run with a covariate duplicating ALR", fit_model_suite(bd$data, bd$meta, c("M1", "M2", "M4"), "Quadratic", "gaussian", dry_run = TRUE))
  expect("0.9.8 a covariate duplicating ALR is named before fitting", isTRUE(pv$ok) && any(grepl("ALR", pv$alias_notes)))
  expect("0.9.8 the pre-fit summary carries formulas, family and random effects",
         isTRUE(pv$ok) && all(c("formulas", "random", "family", "standardise") %in% names(pv)) && length(pv$formulas) == 3)
  set.seed(7)
  cv <- raw; cv$temp <- stats::rnorm(nrow(cv), 20, 3); cv$season <- sample(c("dry", "wet"), nrow(cv), replace = TRUE)
  mc <- toy_mapping(cv); mc$covars <- c("temp", "season"); mc$cov_factor <- "season"
  b1 <- standardise_data(cv, mc); sh <- cv[sample(nrow(cv)), , drop = FALSE]; b2 <- standardise_data(sh, mc)
  r1 <- check("0.9.8 fit (original order)", fit_model_suite(b1$data, b1$meta, "M2", "Quadratic", "gaussian"))
  r2 <- check("0.9.8 fit (shuffled rows)", fit_model_suite(b2$data, b2$meta, "M2", "Quadratic", "gaussian"))
  if (isTRUE(r1$ok) && isTRUE(r2$ok)) {
    ages <- seq(2, 18, length.out = 9)
    p1 <- predict_population_curve(r1$fits$M2, r1, ages); p2 <- predict_population_curve(r2$fits$M2, r2, ages)
    expect("0.9.8 population curves do not depend on row order (time-varying covariates)",
           nrow(p1) == length(ages) && isTRUE(max(abs(p1$fitted - p2$fitted)) < 1e-6))
    ad <- analysis_data_export(r1)
    expect("0.9.8 analysis data export returns the fitted rows", is.data.frame(ad) && nrow(ad) == nrow(r1$data))
    code <- model_r_code(r1, b1$meta)
    expect("0.9.8 exported code can use the app's analysis data", grepl("use_app_data <- file.exists(app_data_file)", code, fixed = TRUE))
    pe <- tryCatch({ parse(text = code); TRUE }, error = function(e) conditionMessage(e))
    expect("0.9.8 exported code parses", isTRUE(pe))
  }
  bi <- standardise_data(raw, m)
  ri <- check("0.9.8 random-intercept fit", fit_model_suite(bi$data, bi$meta, c("M2", "M4"), "Quadratic", "gaussian"))
  if (isTRUE(ri$ok) && identical(ri$aic$Model[[1]], "Model 4")) {
    expect("0.9.8 age-dependent terms favoured with random intercepts only prompts a random-slope check",
           any(grepl("random intercept", consistency_notes(ri), ignore.case = TRUE)))
  }
  rr <- raw; rr$rowid <- as.character(seq_len(nrow(rr)))
  mr <- toy_mapping(rr); mr$random <- "rowid"
  br <- standardise_data(rr, mr)
  pr <- check("0.9.8 random term with one level per record", fit_model_suite(br$data, br$meta, c("M1", "M2"), "Quadratic", "gaussian", dry_run = TRUE))
  expect("0.9.8 a per-record random intercept is omitted in Gaussian models", isTRUE(pr$ok) && any(grepl("one level per record", pr$drop_by)))
})

# ---- 0.9.9: predictions as lines at the observed ages ----
expect("0.9.9 prediction ages for the lines style are the distinct observed ages", identical(observed_prediction_ages(c(3, 1, 2, 2, NA)), c(1, 2, 3)))
expect("0.9.9 smooth curves use a fine age grid", length(smooth_prediction_ages(c(5, 6, 7, 8))) == 100 && identical(range(smooth_prediction_ages(c(5, 8))), c(5, 8)))
expect("0.9.9 continuous ages without IDs fall back to the smooth grid",
       identical(observed_prediction_ages(seq(0, 10, length.out = 500)), prediction_ages(seq(0, 10, length.out = 500))))
local({
  set.seed(3)
  ids <- rep(sprintf("i%02d", 1:40), each = 8)
  ages <- rep(1:8, 40) + stats::runif(320, -0.3, 0.3)
  oa <- observed_prediction_ages(ages, ids)
  expect("0.9.9 irregular ages are drawn at the sampling occasions", length(oa) >= 6 && length(oa) <= 10 && all(diff(oa) > 0))
})

# ---- 0.9.11: provenance, exported code on the shared engine, server helpers in the engine ----
local({
  raw <- simulate_toy_data(list(n_id = 90, seed = 4))
  b <- standardise_data(raw, toy_mapping(raw))
  r <- check("0.9.11 fit with provenance", fit_model_suite(b$data, b$meta, c("M1", "M2", "M4"), "Quadratic", "gaussian"))
  if (isTRUE(r$ok)) {
    pv <- r$provenance
    expect("0.9.11 every model comparison stores its provenance",
           is.list(pv) && all(c("software", "data", "model", "transformations", "settings", "warnings") %in% names(pv)))
    expect("0.9.11 provenance fingerprints the analysed rows", is.character(pv$data$fingerprint_md5) && nchar(pv$data$fingerprint_md5) == 32 &&
             identical(as.integer(pv$data$rows), nrow(r$data)))
    expect("0.9.11 provenance lines are readable", length(provenance_lines(pv)) == 4)
    code <- model_r_code(r, b$meta, "disappR_simulated.csv", source_type = "toy")
    expect("0.9.11 exported code runs the shared engine when the same version is installed",
           grepl("use_engine <- requireNamespace(\"disappR\"", code, fixed = TRUE) && grepl("disappr_fit(x, models = ", code, fixed = TRUE))
    pe <- tryCatch({ parse(text = code); TRUE }, error = function(e) conditionMessage(e))
    expect("0.9.11 exported code parses", isTRUE(pe))
    expect("0.9.11 the R interface returns the provenance", identical(disappr_provenance(r), pv))
  }
  expect("0.9.11 former server helpers live in the engine", all(vapply(c("pred_curves_for", "coef_display", "aic_display", "lrt_display", "settings_sig",
                                                                          "extra_term_choices", "builder_tokens", "a3_min_df"), exists, logical(1))))
})

cat("\n", paste(notes, collapse = "\n"), "\n", sep = "")
if (length(fails)) {
  cat("\nFAILURES (", length(fails), "):\n", sep = "")
  for (nm in names(fails)) cat(" -", nm, ":", fails[[nm]], "\n")
} else {
  cat("\nNo errors.\n")
}
