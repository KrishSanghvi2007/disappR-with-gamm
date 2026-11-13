# disappR stress test on out-of-sample data
# Run from the package root:   Rscript tests/scripts/stress_test.R
# Every dataset in tests/stress_data goes through file reading, cleaning, integrity checks, the sampling
# grid, A1-A3, the decomposition, ageing-function comparisons, Models 1-8 (Gaussian and, with glmmTMB,
# count families), predictions and the exported R code; five uploads are also driven through the real
# server with shiny::testServer(). Errors are collected per dataset and step; targeted expectations are
# reported as PASS / CHECK. A 6,000-individual dataset is generated for timings.

root <- getwd()
app_dir <- file.path(root, "inst", "app")
data_dir <- normalizePath(file.path(root, "tests", "stress_data"), mustWork = FALSE)
if (!dir.exists(app_dir) || !dir.exists(data_dir)) stop("Run this script from the package root (the folder containing DESCRIPTION).")
setwd(app_dir)
on.exit(setwd(root), add = TRUE)
source("global.R")

fails <- list()
notes <- character(0)
infos <- character(0)
step <- function(ds, what, expr) {
  tryCatch(expr, error = function(e) {
    fails[[paste(ds, "|", what)]] <<- conditionMessage(e)
    NULL
  })
}
expect <- function(label, cond) {
  ok <- isTRUE(cond)
  notes <<- c(notes, sprintf("[%s] %s", if (ok) "PASS" else "CHECK", label))
  invisible(ok)
}
info <- function(ds, txt) infos <<- c(infos, paste0(ds, ": ", txt))

mk_map <- function(...) {
  utils::modifyList(list(id = "", age = "", trait = "", alr = "__AUTO_LAST__", life = "", entry = "__AUTO_FIRST__",
                         condition = "", covars = character(0), group = "", nested = TRUE, random = character(0),
                         censor = "", censor_value = "", start_mode = "afr", start_age = NA_real_, age_round = NA_real_),
                    list(...))
}
deg <- "\u00b0"
specs <- list(
  wild_bird_annual = list(file = "wild_bird_annual.csv", family = "poisson", facet = "sex",
    map = mk_map(id = "ring", age = "age", trait = "clutch_size", life = "lifespan", censor = "alive_at_end", censor_value = "yes",
                 covars = "habitat", random = c("cohort", "observer"))),
  mammal_irregular_days = list(file = "mammal_irregular_days.csv", family = "gaussian", facet = "sex", rounded = 30,
    map = mk_map(id = "animal_id", age = "age_days", trait = "mass_kg", life = "death_age_days", censor = "censored", censor_value = "1")),
  ages_from_dates = list(file = "ages_from_dates_decimal_years.csv", family = "gaussian", rounded = 1,
    map = mk_map(id = "ID", age = "age_years", trait = "weight")),
  staggered_cohorts = list(file = "staggered_cohorts_lab.csv", family = "zinb", facet = "treatment",
    map = mk_map(id = "fly", age = "age", trait = "offspring", life = "LS", group = "vial", covars = "treatment", random = "block")),
  few_individuals = list(file = "few_individuals.csv", family = "gaussian",
    map = mk_map(id = "id", age = "age", trait = "trait", life = "ls")),
  single_records = list(file = "single_records_heavy.csv", family = "gaussian", random_slope = TRUE,
    map = mk_map(id = "ID", age = "Age", trait = "Size", life = "Lifespan")),
  messy_csv = list(file = "messy_semicolon_decimal_comma.csv", family = "gaussian", facet = "2nd brood",
    map = mk_map(id = "Individual ID", age = "Age (years)", trait = "Body mass (g)", life = "Lifespan", covars = c(paste0("Temp ", deg, "C"), "2nd brood"))),
  covariate_edges = list(file = "covariate_edge_cases.csv", family = "gaussian", facet = "is_breeder",
    map = mk_map(id = "id", age = "age", trait = "trait", life = "LS", covars = c("site", "a b", "a.b", "rare_level", "temperature", "is_breeder"))),
  age_edges = list(file = "age_edge_cases.csv", family = "gaussian",
    map = mk_map(id = "ID", age = "age", trait = "y", life = "LS", entry = "AFR")),
  # 'ls' in this file is age + 1 on every row, not a lifespan: it is not mapped (see the integrity check at the end)
  count_edges = list(file = "count_edge_cases.csv", family = "nbinom2",
    map = mk_map(id = "id", age = "age", trait = "eggs")),
  two_ages = list(file = "two_ages_only.csv", family = "gaussian", map = mk_map(id = "id", age = "age", trait = "trait", life = "LS")),
  one_age = list(file = "single_age_cross_sectional.csv", family = "gaussian", map = mk_map(id = "id", age = "age", trait = "trait", life = "LS")),
  grouping_edges = list(file = "grouping_timevarying_edge.csv", family = "gaussian", facet = "breeding",
    map = mk_map(id = "id", age = "age", trait = "y", life = "LS", group = "group", censor = "still_alive", censor_value = "TRUE")),
  latin1 = list(file = "latin1_semicolon.csv", family = "gaussian", map = mk_map(id = "ID", age = "age", trait = "mass", covars = paste0("Temp ", deg, "C"))),
  tab_quotes = list(file = "tab_with_commas_in_quotes.tsv", family = "gaussian", map = mk_map(id = "id", age = "age", trait = "mass", random = "observer")),
  leading_zero_ids = list(file = "leading_zero_ids.csv", family = "gaussian", map = mk_map(id = "id", age = "age", trait = "y")),
  blank_headers = list(file = "blank_duplicate_headers.csv", family = "gaussian", map = mk_map(id = "id", age = "age", trait = "y.1")),
  inf_huge = list(file = "inf_and_huge_traits.csv", family = "gaussian", map = mk_map(id = "id", age = "age", trait = "y")),
  all_na_covariate = list(file = "all_na_covariate.csv", family = "gaussian", map = mk_map(id = "id", age = "age", trait = "y", covars = "cov")),
  dates_as_ages = list(file = "dates_in_age_column.csv", expect_stop = TRUE, map = mk_map(id = "id", age = "date", trait = "y")),
  header_only = list(file = "header_only.csv", expect_unreadable = TRUE),
  excel_renamed = list(file = "excel_renamed.csv", expect_unreadable = TRUE),
  a4_selection_censored = list(file = "a4_selection_censored.csv", family = "gaussian", extra_models = TRUE,
    map = mk_map(id = "id", age = "age", trait = "trait", life = "lifespan", censor = "censored", censor_value = "1")),
  a4_age_dependent = list(file = "a4_age_dependent_selection.csv", family = "gaussian",
    map = mk_map(id = "id", age = "age", trait = "trait", life = "lifespan", censor = "censored", censor_value = "1")),
  terminal_decline = list(file = "terminal_decline.csv", family = "gaussian",
    map = mk_map(id = "id", age = "age", trait = "trait", life = "lifespan", censor = "censored", censor_value = "1")),
  rising_hazard = list(file = "rising_hazard.csv", family = "gaussian",
    map = mk_map(id = "id", age = "age", trait = "trait", life = "lifespan", censor = "censored", censor_value = "1"))
)

facet_for <- function(df, sp) {
  if (is.null(sp$facet) || !sp$facet %in% names(df)) return(NULL)
  ids <- trimws(as.character(df[[sp$map$id]]))
  if (nzchar(sp$map$group)) {
    gg <- trimws(as.character(df[[sp$map$group]]))
    ids <- ifelse(is.na(gg) | !nzchar(gg), ids, paste(gg, ids, sep = "/"))
  }
  lv <- as.character(df[[sp$facet]])
  ok <- !is.na(ids) & !is.na(lv) & nzchar(lv)
  per <- tapply(lv[ok], ids[ok], mode_or_na)
  stats::setNames(paste0(sp$facet, ": ", as.character(per)), names(per))
}

run_dataset <- function(ds, sp, map_override = NULL) {
  label <- if (is.null(map_override)) ds else paste0(ds, " (ages rounded to ", map_override$age_round, ")")
  rd <- step(label, "read", read_user_csv(file.path(data_dir, sp$file), sp$file))
  if (is.null(rd)) return(invisible(NULL))
  if (isTRUE(sp$expect_unreadable)) {
    expect(sprintf("%s: rejected with a clear message ('%s')", label, rd$note), is.null(rd$data) || nrow(rd$data) < 10 || ncol(rd$data) < 3)
    return(invisible(NULL))
  }
  df <- rd$data
  info(label, rd$note)
  mp <- if (is.null(map_override)) sp$map else map_override
  b <- step(label, "standardise", standardise_data(df, mp, "keep"))
  if (is.null(b)) return(invisible(NULL))
  d <- b$data
  m <- b$meta
  usable <- nrow(d) >= 10 && length(unique(d$id)) >= 3 && sum(is.finite(d$trait)) >= 10
  if (isTRUE(sp$expect_stop)) {
    expect(sprintf("%s: the app stops with a validation message (%d usable rows)", label, nrow(d)), !usable)
    return(invisible(NULL))
  }
  if (!usable) {
    fails[[paste(label, "| usable data")]] <<- sprintf("%d rows, %d individuals, %d numeric traits", nrow(d), length(unique(d$id)), sum(is.finite(d$trait)))
    return(invisible(NULL))
  }
  integ <- step(label, "integrity", data_integrity(d, m))
  t_grid <- system.time(g <- step(label, "grid (AFR start)", build_missing_grid(d, 0, "afr")))[["elapsed"]]
  step(label, "grid (common start)", build_missing_grid(d, 0, "same", min(d$age)))
  step(label, "grid (margin 1)", build_missing_grid(d, 1, "afr"))
  ms <- NULL
  if (!is.null(g) && nrow(g)) {
    disp <- step(label, "grid display", grid_display(g, utils::head(unique(g$id), 150)))
    ms <- step(label, "missingness summary", missingness_summary(d, g, 0.05, isTRUE(m$has_life) && !isTRUE(m$life_auto)))
    for (v in c("Age", "ALR", "LS", "Condition", "Trait")) step(label, paste("missingness by", v), missingness_by_variable(d, g, v))
    step(label, "coverage by age", coverage_by_age(g))
    info(label, sprintf("grid %s cells in %.1fs, %.1f%% missing, step %s%s", format(nrow(g), big.mark = ","), t_grid,
                        100 * mean(g$missing), format_num(g$step[[1]]), if (!is.null(disp)) sprintf(", %d display tiles", nrow(disp)) else ""))
  }
  im <- individual_metrics(d)
  fct <- facet_for(df, sp)
  st <- infer_age_step(d$age, d$id)
  for (target in c("disappearance", "appearance")) {
    for (px in proxy_choices(im, m, target)) {
      pv <- proxy_values(im, px)
      nb <- max_bins_for(pv, 3, st)
      for (bins in unique(c(3, nb))) {
        s <- step(label, paste("A1", px, bins, "bins"), binned_trajectory(d, pv, bins, "equal", 3, facet = fct))
        if (!is.null(s) && nrow(s)) {
          dz <- step(label, paste("A1 differences", px), bin_differences(s, "all"))
          if (!is.null(dz)) step(label, paste("A1 trends", px), bin_difference_trends(dz))
        }
      }
      na2 <- max_bins_for(id_age_means(d)$age, 3, st)
      z <- step(label, paste("A2", px), trait_by_age_bins(d, pv, na2, facet = fct))
      if (!is.null(z) && nrow(z)) {
        tab <- step(label, paste("A2 coefficient table", px), a2_bin_slopes(z))
        if (!is.null(tab)) step(label, paste("A2 trend", px), a2_slope_trend(tab))
      }
      step(label, paste("A2 correlations by age", px), proxy_slopes_by_age(d, pv, "r"))
    }
  }
  for (fn in c("Quadratic", A3_NONLINEAR)) step(label, paste("A3", fn), fit_individual_function(d, fn, 1, utils::head(unique(d$id), 5)))
  t_a3 <- system.time(step(label, "A3 function comparison", compare_individual_functions(d, 1)))[["elapsed"]]
  dec <- step(label, "decomposition", decomposition_trajectory(d))
  ia4 <- step(label, "A4 disappearance data", disappearance_data(d, m))
  a4 <- if (!is.null(ia4)) step(label, "A4 hazard models", disappearance_models(ia4)) else NULL
  a6 <- if (!is.null(ia4)) step(label, "A6 selection differentials", selection_differentials(ia4)) else NULL
  a5 <- step(label, "A5 terminal data", terminal_data(d, m))
  a7 <- step(label, "A7 life table", life_table(d, m))
  step(label, "support metrics", individual_data_support(d))
  fams <- unique(c("gaussian", if (!identical(sp$family, "gaussian") && HAS_GLMMTMB) sp$family))
  fits <- list()
  for (fam in fams) {
    for (fn in if (identical(fam, "gaussian")) c("Quadratic", "Linear") else "Quadratic") {
      av <- model_availability(d, m)
      t_fit <- system.time(res <- step(label, paste("models", fam, fn),
        fit_model_suite(d, m, MODEL_IDS[av$ok], fn, fam, if (isTRUE(sp$random_slope)) "auto" else "none", TRUE, "~1",
                        extra = if (isTRUE(sp$extra_models)) list(M5 = "ALR", M6 = c("ALR", "AFR_x_age")) else NULL)))[["elapsed"]]
      if (is.null(res)) next
      fits[[paste(fam, fn)]] <- res
      if (isTRUE(res$ok)) {
        ages <- prediction_ages(res$data$age)
        for (mm in names(res$fits)) step(label, paste("predict", fam, fn, mm), predict_population_curve(res$fits[[mm]], res, ages))
        bm <- sub("^Model ", "M", res$aic$Model[[1]])
        step(label, paste("interpretation", fam, fn), interpret_model_terms(res, bm))
        step(label, paste("scaling", fam, fn), term_scaling(res$coefficients$Raw_term, res))
        step(label, paste("consistency", fam, fn), consistency_notes(res, NULL, NULL))
        if (identical(fam, "gaussian") && identical(fn, "Quadratic")) step(label, "performance", run_performance(res$fits[[bm]], fam))
        code <- step(label, paste("R code", fam, fn), model_r_code(res, m, sp$file))
        if (!is.null(code)) step(label, paste("R code parses", fam, fn), parse(text = code))
        info(label, sprintf("%s %s: %d models in %.1fs, best %s; dropped: %s; status: %s", fam, fn, length(res$fits), t_fit,
                            res$aic$Model[[1]], if (length(res$drop_by)) paste(res$drop_by, collapse = "; ") else "none",
                            paste(unique(unlist(res$status)), collapse = " | ")))
      } else {
        info(label, sprintf("%s %s not fitted: %s %s", fam, fn, res$message, paste(unlist(res$status), collapse = " | ")))
      }
    }
    step(label, paste("B2 function comparison", fam), compare_population_functions(d, m, fam, functions = c("Linear", "Quadratic"), model = "M1"))
  }
  if (HAS_DHARMA && !is.null(fits[["gaussian Quadratic"]]) && isTRUE(fits[["gaussian Quadratic"]]$ok) && ds %in% c("wild_bird_annual", "covariate_edges")) {
    r <- fits[["gaussian Quadratic"]]
    step(label, "DHARMa", run_dharma(r$fits[[1]], "gaussian"))
  }
  info(label, sprintf("A3 comparison %.1fs", t_a3))
  list(rd = rd, d = d, m = m, integ = integ, g = g, ms = ms, dec = dec, fits = fits, a4 = a4, a5 = a5, a6 = a6, a7 = a7)
}

out <- list()
for (ds in names(specs)) {
  cat("Stress dataset:", ds, "\n")
  out[[ds]] <- run_dataset(ds, specs[[ds]])
  if (!is.null(specs[[ds]]$rounded)) {
    mp <- utils::modifyList(specs[[ds]]$map, list(age_round = specs[[ds]]$rounded))
    out[[paste0(ds, "_rounded")]] <- run_dataset(ds, specs[[ds]], mp)
  }
}

# ---------------------------------------------------------------------------
# Targeted expectations
# ---------------------------------------------------------------------------
has_check <- function(o, pattern, status = NULL) {
  tab <- o$integ$table
  if (is.null(tab)) return(FALSE)
  hit <- grepl(pattern, tab$Check) | grepl(pattern, tab$Result) | grepl(pattern, tab$Advice)
  if (!is.null(status)) hit <- hit & tab$Status %in% status
  any(hit)
}
o <- out$mammal_irregular_days
if (!is.null(o)) {
  expect("irregular days: grid stays below 300,000 cells", nrow(o$g) <= 300000)
  expect("irregular days: integrity warns that ages are irregular", has_check(o, "irregular", "Warning"))
}
o <- out$mammal_irregular_days_rounded
if (!is.null(o)) expect("irregular days rounded to 30: grid step is 30 and the decomposition has several points",
                        isTRUE(abs(o$g$step[[1]] - 30) < 1e-8) && isTRUE(nrow(o$dec) >= 4))
o <- out$ages_from_dates
if (!is.null(o)) expect("decimal-year ages: grid below 300,000 cells and irregularity flagged", nrow(o$g) <= 300000 && has_check(o, "irregular", "Warning"))
o <- out$staggered_cohorts
if (!is.null(o)) {
  expect(sprintf("staggered cohorts: step 14 (got %s)", format_num(o$g$step[[1]])), isTRUE(abs(o$g$step[[1]] - 14) < 1e-8))
  expect(sprintf("staggered cohorts: no spurious missed occasions (%.1f%% missing)", 100 * mean(o$g$missing)), mean(o$g$missing) < 0.05)
  expect("staggered cohorts: offset schedules reported", has_check(o, "offset schedules"))
  expect("staggered cohorts: decomposition follows a cohort schedule (>= 4 points)", isTRUE(nrow(o$dec) >= 4))
}
o <- out$messy_csv
if (!is.null(o)) {
  expect("messy CSV: semicolons and decimal commas detected", grepl("semicolon", o$rd$note) && grepl("decimal commas", o$rd$note))
  expect("messy CSV: 6 columns with numeric body mass", ncol(o$rd$data) == 6 && sum(is.finite(o$d$trait)) > 600)
  expect("messy CSV: -999 codes flagged", has_check(o, "missing-value codes", "Warning"))
  expect("messy CSV: trailing spaces removed from IDs (120 individuals)", length(unique(o$d$id)) == 120)
}
o <- out$covariate_edges
if (!is.null(o)) {
  r <- o$fits[["gaussian Quadratic"]]
  expect("covariate edges: models fit despite a constant covariate", isTRUE(r$ok))
  expect("covariate edges: constant covariate omitted with a note", any(grepl("site", r$drop_by)))
  expect("covariate edges: 'a b' and 'a.b' get separate internal names", length(unique(o$m$covars)) == length(o$m$covars) && length(o$m$covars) == 6)
  expect("covariate edges: numeric temperature stays numeric", is.numeric(o$d[[o$m$covars[o$m$cov_labels == "temperature"]]]))
}
o <- out$two_ages
if (!is.null(o)) {
  expect("two ages: quadratic refused with a distinct-ages message", !isTRUE(o$fits[["gaussian Quadratic"]]$ok) && grepl("distinct ages", o$fits[["gaussian Quadratic"]]$message))
  expect("two ages: linear models fit", isTRUE(o$fits[["gaussian Linear"]]$ok))
}
o <- out$one_age
if (!is.null(o)) expect("one age: linear refused with a distinct-ages message", grepl("distinct ages", o$fits[["gaussian Linear"]]$message %||% ""))
o <- out$leading_zero_ids
if (!is.null(o)) expect("leading-zero IDs: 007, 07 and 7 are three different individuals (7 in total)", length(unique(o$d$id)) == 7)
o <- out$blank_headers
if (!is.null(o)) expect("blank/duplicate headers: columns repaired (id, age, y, y.1)", identical(names(o$rd$data), c("id", "age", "y", "y.1")))
o <- out$inf_huge
if (!is.null(o)) expect("Inf and 1e300 traits: Inf treated as missing and extreme values noted", sum(!is.finite(o$d$trait)) >= 5 && has_check(o, "Extreme trait values"))
o <- out$all_na_covariate
if (!is.null(o)) expect("all-NA covariate: skipped and reported, models still fit", identical(o$m$skipped_columns, "cov") && isTRUE(o$fits[["gaussian Quadratic"]]$ok))
o <- out$latin1
if (!is.null(o)) expect("latin1 file: degree sign read correctly in the header", paste0("Temp ", deg, "C") %in% names(o$rd$data))
o <- out$tab_quotes
if (!is.null(o)) expect("tab-separated with quoted commas: 4 columns, observer 'Smith, J.'", ncol(o$rd$data) == 4 && all(o$rd$data$observer == "Smith, J."))
o <- out$wild_bird_annual
if (!is.null(o)) expect("wild birds: censored birds have unknown lifespan", isTRUE(o$m$n_censored > 0))
o <- out$a4_selection_censored
if (!is.null(o) && isTRUE(o$a4$ok)) {
  t1 <- o$a4$table[o$a4$table$Model == "Trait now", ]
  expect(sprintf("A4 trait-dependent mortality: odds ratio %.2f < 1 and p < 0.05 (simulated 0.45)", t1$Odds_ratio[[1]]), t1$Odds_ratio[[1]] < 0.8 && t1$P[[1]] < 0.05)
}
o <- out$a4_age_dependent
if (!is.null(o) && isTRUE(o$a4$ok)) expect(sprintf("A4 age-dependent selection detected (trait x age p = %s)", format_p(o$a4$p_age_dependence)), o$a4$p_age_dependence < 0.05)
o <- out$terminal_decline
if (!is.null(o) && is.data.frame(o$a5) && nrow(o$a5)) {
  ch <- vapply(split(o$a5, o$a5$end_bin), function(x) if (any(x$before == 0) && any(x$before >= 2)) x$resid[x$before == 0][[1]] - mean(x$resid[x$before >= 2]) else NA_real_, numeric(1))
  expect(sprintf("A5 terminal decline of -5 recovered in each lifespan group (%s)", paste(round(ch, 1), collapse = ", ")), all(is.finite(ch)) && all(ch < -2.5))
}
o <- out$rising_hazard
if (!is.null(o) && isTRUE(o$a7$ok)) expect(sprintf("A7 rising hazard: constant-hazard test rejected (p = %s)", format_p(o$a7$p_constant)), o$a7$p_constant < 0.05)
o <- out$terminal_decline
if (!is.null(o) && isTRUE(o$a7$ok)) expect(sprintf("A7 constant simulated hazard: overall hazard %.1f%% (simulated 10%%)", 100 * o$a7$overall), abs(o$a7$overall - 0.1) < 0.03)

# ---------------------------------------------------------------------------
# Scale: 6,000 individuals
# ---------------------------------------------------------------------------
set.seed(11)
big <- do.call(rbind, lapply(seq_len(6000), function(i) {
  ls <- sample(2:14, 1)
  a <- seq_len(ls)
  a <- a[stats::runif(ls) < 0.6]
  if (!length(a)) a <- 1
  data.frame(id = paste0("P", i), age = a, y = 20 + a - 0.05 * a^2 + stats::rnorm(length(a)), ls = ls)
}))
bmap <- mk_map(id = "id", age = "age", trait = "y", life = "ls")
tb <- system.time(bb <- standardise_data(big, bmap))[["elapsed"]]
tg <- system.time(gb <- build_missing_grid(bb$data))[["elapsed"]]
ti <- system.time(integrity_big <- data_integrity(bb$data, bb$meta))[["elapsed"]]
tm <- system.time(ms_big <- missingness_summary(bb$data, gb, 0.05, TRUE))[["elapsed"]]
ta <- system.time(a3_big <- compare_individual_functions(bb$data, 1))[["elapsed"]]
tf <- system.time(fit_big <- fit_model_suite(bb$data, bb$meta, paste0("M", 1:5), "Quadratic", "gaussian"))[["elapsed"]]
cat(sprintf("\nScale (%s rows, 6000 individuals): standardise %.1fs, grid %.1fs (%s cells), integrity %.1fs, missingness %.1fs, A3 comparison %.1fs, Models 1-5 %.1fs\n",
            format(nrow(bb$data), big.mark = ","), tb, tg, format(nrow(gb), big.mark = ","), ti, tm, ta, tf))
expect("scale: Models 1-5 fit on 6,000 individuals", isTRUE(fit_big$ok))
expect("scale: A3 comparison limited to 2,000 individuals", isTRUE(all(a3_big$N_considered <= 2000)))

# ---------------------------------------------------------------------------
# Server: uploads driven through the real app
# ---------------------------------------------------------------------------
if (requireNamespace("shiny", quietly = TRUE)) {
  app <- shiny::shinyAppDir(normalizePath("."))
  problems <- character(0)
  touch <- function(output_env, names, ds) {
    for (nm in names) {
      res <- tryCatch({
        output_env[[nm]]
        "ok"
      }, shiny.silent.error = function(e) "ok", error = function(e) conditionMessage(e))
      if (!identical(res, "ok")) problems <<- c(problems, paste0(ds, " | ", nm, ": ", res))
    }
  }
  outs <- c("mapping_ui", "data_metrics", "integrity_table", "integrity_extra", "dist_var_ui", "dist_plot", "data_preview",
            "visual_proxy_ui", "facet_ui", "a1_plot", "bin_diff_plot", "a2_plot", "sampling_metrics", "sampling_guidance", "heatmap", "missing_by_age", "coverage_table", "missing_vs_var", "drivers_table",
            "proxy_plots_ui", "proxy_alr_mean", "a3_metrics", "a3_plot", "a3_mean_plot", "a3_compare_table", "a3_coef_table",
            "model_fit_note", "aic_plot", "aic_table", "lrt_table", "status_table", "pred_plot", "coef_model_ui",
            "coef_table", "coef_re_table", "code_ui", "summary_ui")
  base_inputs <- list(n_bins = 4, bin_method = "equal", show_se = FALSE,
                      trait_scale = "raw", heat_order = "alr",
                      miss_var = "ALR", dist_unit = "row", a3_function = "Quadratic", a3_recon = "both", a3_n = 10,
                      a3_order = "random", zi_formula = "~1", model_age_function = "Quadratic", random_slope = FALSE,
                      standardise = TRUE, show_observed = TRUE, show_decomp = TRUE, dup_action = "keep")
  for (ds in c("wild_bird_annual", "messy_csv", "staggered_cohorts", "covariate_edges", "mammal_irregular_days")) {
    sp <- specs[[ds]]
    mp <- sp$map
    res <- tryCatch(shiny::testServer(app, {
      do.call(session$setInputs, base_inputs)
      session$setInputs(data_source = "upload",
                        data_file = list(name = sp$file, datapath = file.path(data_dir, sp$file), size = 1, type = "text/csv"))
      session$setInputs(col_id = mp$id, col_age = mp$age, col_trait = mp$trait, col_alr = mp$alr, col_life = mp$life,
                        start_mode = mp$start_mode, col_entry = mp$entry, col_condition = "",
                        col_cov_num = NULL, col_cov_fac = if (length(mp$covars)) mp$covars else NULL, col_cov_int = NULL, col_group = mp$group, group_nested = TRUE,
                        col_random = if (length(mp$random)) mp$random else NULL, col_censor = mp$censor,
                        censor_value = mp$censor_value, age_round = NA, dist_var = mp$trait, facet_var = sp$facet %||% "",
                        visual_proxy = "ALR", model_family = "gaussian", use_M1 = TRUE, use_M2 = TRUE, use_M3 = TRUE, use_M4 = TRUE, use_M5 = TRUE, use_M6 = FALSE, use_M7 = FALSE, use_M8 = FALSE, use_M9 = FALSE, use_M10 = FALSE)
      session$setInputs(fit_models = 1)
      touch(output, outs, ds)
      for (ho in c("afr", "trait")) {
        session$setInputs(heat_order = ho)
        touch(output, "heatmap", ds)
      }
      session$setInputs(save_integrity = 1, save_models = 1, save_heatmap = 1)
      rep <- html_report(saved_results(), ds)
      if (!length(rep)) problems <<- c(problems, paste(ds, "| empty HTML report"))
    }), error = function(e) conditionMessage(e))
    if (is.character(res) && length(res) == 1) problems <- c(problems, paste(ds, "| server:", res))
  }
  if (length(problems)) {
    for (pr in problems) fails[[paste("server", pr)]] <- pr
  } else {
    notes <- c(notes, "[PASS] server: all outputs rendered for five uploaded stress datasets")
  }
}

# 0.20.2: a column that repeats each record's age is not a lifespan. Mapping count_edge_cases.csv's 'ls' as lifespan
# must stop with a data-integrity error naming the individuals, not average it into impossible values.
local({
  rd <- tryCatch(read_user_csv(file.path(data_dir, "count_edge_cases.csv"), "count_edge_cases.csv"), error = function(e) NULL)
  if (is.null(rd) || is.null(rd$data)) return(invisible(NULL))
  err <- tryCatch(standardise_data(rd$data, mk_map(id = "id", age = "age", trait = "eggs", life = "ls")),
                  disappr_integrity_error = function(e) e, error = function(e) NULL)
  expect("count_edge_cases: mapping 'ls' (age + 1) as lifespan stops with a data-integrity error",
         inherits(err, "disappr_integrity_error") && length(err$conflicts$life) > 100)
})

cat("\nDETAILS\n", paste("-", infos, collapse = "\n"), "\n", sep = "")
cat("\nEXPECTATIONS\n", paste(notes, collapse = "\n"), "\n", sep = "")
if (length(fails)) {
  cat("\nFAILURES (", length(fails), "):\n", sep = "")
  for (nm in names(fails)) cat(" -", nm, ":", fails[[nm]], "\n")
} else {
  cat("\nNo errors.\n")
}

