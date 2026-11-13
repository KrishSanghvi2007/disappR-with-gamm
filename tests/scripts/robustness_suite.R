# disappR robustness suite (reviewer tests): metamorphic invariants, lme4 oracle, exported-code round trip,
# adversarial data (controlled failure) and reactive-state (stale result) tests.
# Not part of the app. Run from the package root:  Rscript tests/scripts/robustness_suite.R
# Optional large-data timings:  DISAPPR_BIG=1 Rscript tests/scripts/robustness_suite.R
suppressPackageStartupMessages(library(shiny))
app_dir <- normalizePath(file.path("inst", "app"), mustWork = TRUE)
old_wd <- setwd(app_dir)
source("global.R")
set.seed(20260919)

results <- data.frame(Test = character(0), Result = character(0), Detail = character(0), stringsAsFactors = FALSE)
record <- function(test, ok, detail = "") {
  res <- if (identical(ok, NA)) "INFO" else if (isTRUE(ok)) "PASS" else "FAIL"
  results[nrow(results) + 1, ] <<- list(test, res, as.character(detail))
  cat(sprintf("[%s] %s%s\n", res, test, if (nzchar(detail)) paste0(" - ", detail) else ""))
}
safely <- function(expr) tryCatch(expr, error = function(e) structure(list(message = conditionMessage(e)), class = "suite_error"))
is_err <- function(x) inherits(x, "suite_error")

# ---- helpers: fit through the app's own functions and extract AICs and population curves
fit_on <- function(raw, map, models = c("M1", "M2", "M3", "M4", "M5"), fun = "Quadratic", family = "gaussian",
                   standardise = TRUE, ages = NULL) {
  b <- standardise_data(raw, map)
  r <- fit_model_suite(b$data, b$meta, models, fun, family, standardise = standardise)
  if (!isTRUE(r$ok)) return(list(ok = FALSE, message = r$message))
  aic <- stats::setNames(r$aic$AIC, sub("^Model ", "M", r$aic$Model))
  if (is.null(ages)) ages <- seq(min(b$data$age, na.rm = TRUE), max(b$data$age, na.rm = TRUE), length.out = 15)
  pred <- lapply(names(aic), function(m) {
    pc <- tryCatch(predict_population_curve(r$fits[[m]], r, ages), error = function(e) NULL)
    if (is.data.frame(pc) && "fitted" %in% names(pc)) pc$fitted else NULL
  })
  list(ok = TRUE, aic = aic, pred = stats::setNames(pred, names(aic)), n = r$aic$N[[1]], res = r, b = b, ages = ages)
}
aic_gap <- function(a, b) if (!setequal(names(a), names(b))) Inf else max(abs(a - b[names(a)]))
daic_gap <- function(a, b) if (!setequal(names(a), names(b))) Inf else max(abs((a - min(a)) - (b[names(a)] - min(b))))
pred_gap <- function(p, q, scale = 1, shift = 0) {
  g <- vapply(names(p), function(m) {
    if (is.null(p[[m]]) || is.null(q[[m]]) || length(p[[m]]) != length(q[[m]])) return(NA_real_)
    max(abs(p[[m]] * scale + shift - q[[m]])) / max(max(abs(q[[m]])), 1e-12)
  }, numeric(1))
  if (all(is.na(g))) NA_real_ else max(g, na.rm = TRUE)
}
load_ex <- function(key) {
  ex <- EXAMPLES[[key]]
  raw <- load_example_file(ex$file)
  if (!is.null(ex$subset)) raw <- raw[!is.na(raw[[ex$subset$var]]) & as.character(raw[[ex$subset$var]]) %in% ex$subset$levels, , drop = FALSE]
  list(raw = raw, map = ex$mapping)
}

# ---- 1. metamorphic invariants on empirical data (Gaussian fits so that the invariances are exact)
for (key in c("mckennaell_weight", "bouwhuis", "szejnersigal_activity", "sanghvi_female")) {
  ex <- safely(load_ex(key))
  if (is_err(ex)) { record(paste("metamorphic", key, "load"), FALSE, ex$message); next }
  raw <- ex$raw; map <- ex$map
  base <- safely(fit_on(raw, map))
  if (is_err(base) || !isTRUE(base$ok)) { record(paste("metamorphic", key, "baseline fit"), FALSE, if (is_err(base)) base$message else base$message); next }
  try_t <- function(label, raw2, map2 = map, scale = 1, shift = 0, daic = FALSE, standardise = TRUE, ages = base$ages) {
    x <- safely(fit_on(raw2, map2, standardise = standardise, ages = ages))
    if (is_err(x) || !isTRUE(x$ok)) return(record(sprintf("metamorphic %s: %s", key, label), FALSE, if (is_err(x)) x$message else x$message))
    ga <- if (daic) daic_gap(base$aic, x$aic) else aic_gap(base$aic, x$aic)
    gp <- pred_gap(base$pred, x$pred, scale, shift)
    record(sprintf("metamorphic %s: %s", key, label), ga < 1e-3 && (is.na(gp) || gp < 1e-4),
           sprintf("max AIC change %.2g; max relative change in population curve %.2g", ga, gp))
  }
  try_t("rows shuffled", raw[sample(nrow(raw)), , drop = FALSE])
  ids <- unique(as.character(raw[[map$id]]))
  new_ids <- stats::setNames(sprintf("%05d", sample(length(ids))), ids)
  r2 <- raw; r2[[map$id]] <- unname(new_ids[as.character(r2[[map$id]])])
  try_t("IDs renamed (leading zeros)", r2)
  r3 <- raw; r3$unused_number <- stats::rnorm(nrow(r3)); r3$unused_text <- "x"
  try_t("unused columns added", r3)
  try_t("standardisation off", raw, standardise = FALSE)
  r4 <- raw
  for (col in unique(c(map$age, if (!startsWith(map$alr, "__") && nzchar(map$alr)) map$alr, if (nzchar(map$life)) map$life,
                       if (!startsWith(map$entry, "__") && nzchar(map$entry)) map$entry))) r4[[col]] <- suppressWarnings(as.numeric(r4[[col]])) * 365
  try_t("ages in days instead of years", r4, ages = base$ages * 365)
  r5 <- raw; r5[[map$trait]] <- suppressWarnings(as.numeric(r5[[map$trait]])) + 100
  try_t("trait + 100", r5, shift = 100)
  r6 <- raw; r6[[map$trait]] <- suppressWarnings(as.numeric(r6[[map$trait]])) * 10
  try_t("trait x 10 (delta AIC)", r6, scale = 10, daic = TRUE)
  facs <- intersect(map$cov_factor, map$covars)
  if (length(facs)) {
    r7 <- raw
    for (f in facs) {
      lv <- sort(unique(as.character(r7[[f]][!is.na(r7[[f]])])))
      ren <- stats::setNames(paste0(letters[26:(27 - length(lv))], "_", lv), lv)
      r7[[f]] <- ifelse(is.na(r7[[f]]), NA, unname(ren[as.character(r7[[f]])]))
    }
    try_t("factor level order reversed", r7)
  }
}

# ---- 2. oracle: the app's Models 1-6 against hand-written lme4 formulas on raw age
if (requireNamespace("lme4", quietly = TRUE)) {
  raw <- simulate_toy_data(list(n_id = 150, seed = 11))
  b <- standardise_data(raw, toy_mapping(raw))
  r <- safely(fit_model_suite(b$data, b$meta, c("M1", "M2", "M3", "M4", "M5", "M6"), "Quadratic", "gaussian"))
  if (is_err(r) || !isTRUE(r$ok)) {
    record("oracle: app fit", FALSE, if (is_err(r)) r$message else r$message)
  } else {
    d <- b$data[is.finite(b$data$trait) & is.finite(b$data$age), , drop = FALSE]
    d$A <- d$age; d$A2 <- d$age^2; d$ALRr <- d$alr; d$LSr <- d$life
    d$mA <- stats::ave(d$A, d$id); d$dA <- d$A - d$mA; d$dA2 <- d$A2 - stats::ave(d$A2, d$id)
    ref <- list(M1 = trait ~ A + A2 + (1 | id), M2 = trait ~ A + A2 + ALRr + (1 | id), M3 = trait ~ mA + dA + dA2 + (1 | id),
                M4 = trait ~ (A + A2) * ALRr + (1 | id), M5 = trait ~ mA * (dA + dA2) + (1 | id), M6 = trait ~ (A + A2) * LSr + (1 | id))
    app_ll <- stats::setNames(r$aic$logLik, sub("^Model ", "M", r$aic$Model))
    for (m in names(ref)) {
      f <- safely(lme4::lmer(ref[[m]], data = d, REML = FALSE))
      if (is_err(f)) { record(paste("oracle", m), FALSE, f$message); next }
      gap <- abs(as.numeric(stats::logLik(f)) - app_ll[[m]])
      record(paste("oracle", m, "log-likelihood equals hand-written lme4 fit"), gap < 1e-3 && stats::nobs(f) == r$aic$N[[1]],
             sprintf("|difference| = %.2g; N app %d, reference %d", gap, r$aic$N[[1]], stats::nobs(f)))
    }
  }
}

# ---- 3. exported code round trip in a fresh R process
local({
  raw <- simulate_toy_data(list(n_id = 120, seed = 5))
  b <- standardise_data(raw, toy_mapping(raw))
  r <- safely(fit_model_suite(b$data, b$meta, c("M1", "M2", "M4", "M5"), "Quadratic", "gaussian"))
  if (is_err(r) || !isTRUE(r$ok)) return(record("export round trip: app fit", FALSE, if (is_err(r)) r$message else r$message))
  csv <- tempfile(fileext = ".csv"); out <- tempfile(fileext = ".rds"); script <- tempfile(fileext = ".R")
  utils::write.csv(raw, csv, row.names = FALSE)
  code <- model_r_code(r, b$meta, source_label = gsub("\\\\", "/", csv))
  writeLines(c(code, sprintf('saveRDS(vapply(fits, function(f) as.numeric(AIC(f)), numeric(1)), "%s")', gsub("\\\\", "/", out))), script)
  log <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), shQuote(script), stdout = TRUE, stderr = TRUE))
  if (!file.exists(out)) return(record("export round trip: script runs in a fresh R session", FALSE, paste(utils::tail(log, 3), collapse = " | ")))
  ea <- readRDS(out); ra <- stats::setNames(r$aic$AIC, sub("^Model ", "M", r$aic$Model))
  nm <- intersect(names(ea), names(ra))
  gap <- if (length(nm)) max(abs(ea[nm] - ra[nm])) else if (length(ea) == length(ra)) max(abs(sort(ea) - sort(ra))) else Inf
  record("export round trip: exported script reproduces the app's AICs", gap < 1e-3, sprintf("max |AIC difference| %.2g over %d models", gap, length(ea)))
  # exact reproduction from the app's analysis data (the 'Analysis data (CSV)' download)
  wd2 <- tempfile("roundtrip"); dir.create(wd2)
  utils::write.csv(analysis_data_export(r), file.path(wd2, "disappR_analysis_data.csv"), row.names = FALSE)
  out2 <- file.path(wd2, "aic.rds"); script2 <- file.path(wd2, "script.R")
  writeLines(c(code, sprintf('saveRDS(vapply(fits, function(f) as.numeric(AIC(f)), numeric(1)), "%s")', gsub("\\\\", "/", out2))), script2)
  owd <- setwd(wd2)
  log2 <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), shQuote(script2), stdout = TRUE, stderr = TRUE))
  setwd(owd)
  if (!file.exists(out2)) return(record("export round trip with the app's analysis data", FALSE, paste(utils::tail(log2, 3), collapse = " | ")))
  ea2 <- readRDS(out2); nm2 <- intersect(names(ea2), names(ra))
  gap2 <- if (length(nm2)) max(abs(ea2[nm2] - ra[nm2])) else if (length(ea2) == length(ra)) max(abs(sort(ea2) - sort(ra))) else Inf
  record("export round trip with the app's analysis data (exact)", gap2 < 1e-6, sprintf("max |AIC difference| %.2g", gap2))
})

# ---- 4. adversarial data: failures must be controlled (no escaped R error, a message or a status)
adv_base <- function(n = 60, cross = FALSE, seed = 1) {
  set.seed(seed)
  do.call(rbind, lapply(seq_len(n), function(i) {
    ages <- if (cross) sample(1:8, 1) else seq_len(sample(3:8, 1))
    data.frame(ID = sprintf("%03d", i), age = ages, trait = 10 + stats::rnorm(1) - 0.2 * ages + stats::rnorm(length(ages), 0, 0.5),
               LS = max(ages) + i %% 3, x1 = stats::rnorm(length(ages)), grp = "G1", stringsAsFactors = FALSE)
  }))
}
adv_map <- function(...) utils::modifyList(example_map(id = "ID", age = "age", trait = "trait", life = "LS"), list(...))
adversarial <- function(label, d, map, family = "gaussian", models = c("M1", "M2", "M4")) {
  if (!identical(family, "gaussian") && !HAS_GLMMTMB) return(record(paste("adversarial:", label), NA, "glmmTMB not installed"))
  b <- safely(standardise_data(d, map))
  if (is_err(b)) return(record(paste("adversarial:", label), FALSE, paste("standardise_data error:", b$message)))
  r <- safely(fit_model_suite(b$data, b$meta, models, "Linear", family))
  if (is_err(r)) return(record(paste("adversarial:", label), FALSE, paste("escaped error:", r$message)))
  st <- if (length(r$validity)) paste(names(r$validity), unlist(r$validity), sep = "=", collapse = ", ") else ""
  record(paste("adversarial:", label), TRUE, paste0(if (isTRUE(r$ok)) "fitted" else "not fitted", "; ", substr(r$message %||% "", 1, 90), if (nzchar(st)) paste0("; ", st) else ""))
}
d <- adv_base(); d$trait <- as.numeric(d$age > 4); adversarial("binary, perfect separation by age", d, adv_map(), "binomial")
d <- adv_base(); d$trait <- 0; adversarial("binary, one outcome class only", d, adv_map(), "binomial")
d <- adv_base(); d$trait <- 0; d$trait[1] <- 1; adversarial("binary, a single success", d, adv_map(), "binomial")
d <- adv_base(); d$trait <- 0; adversarial("counts, all zero", d, adv_map(), "poisson")
d <- adv_base(); d$trait <- 5; adversarial("Gaussian, constant trait", d, adv_map())
d <- adv_base(); d$x2 <- 2 * d$x1; adversarial("perfectly correlated covariates", d, adv_map(covars = c("x1", "x2")))
d <- adv_base(); d$agecov <- d$age; adversarial("covariate identical to age", d, adv_map(covars = "agecov"))
d <- adv_base(); d$alrcov <- stats::ave(d$age, d$ID, FUN = max); adversarial("covariate identical to ALR", d, adv_map(covars = "alrcov"))
d <- adv_base(); d$site <- ifelse(d$x1 > 0, "A", "B"); d$x1[d$site == "B"] <- NA
adversarial("factor left with one level after complete cases", d, adv_map(covars = c("x1", "site"), cov_factor = "site"))
d <- adv_base(); adversarial("random term with one level", d, adv_map(random = "grp"))
d <- adv_base(); d$rowid <- as.character(seq_len(nrow(d))); adversarial("random term with one level per record", d, adv_map(random = "rowid"))
d <- adv_base(cross = TRUE); adversarial("one record per individual", d, adv_map())
d <- adv_base(n = 5); adversarial("five individuals", d, adv_map())
d <- adv_base(); d$age <- 3; adversarial("a single age", d, adv_map())
d <- adv_base(); d$trait[seq(1, nrow(d), 2)] <- NA; d$x1[seq(2, nrow(d), 2)] <- NA; adversarial("complete cases leave nothing", d, adv_map(covars = "x1"))

# ---- 5. reactive state: results must not outlive the data or settings they came from
local({
  app <- shiny::shinyAppDir(app_dir)
  count_trait <- unname(TOY_TRAITS)[grepl("count", unname(TOY_TRAITS))][[1]]
  tm <- toy_mapping(simulate_toy_data(list(trait = count_trait, n_id = 40, seed = 1)))
  out_state <- function(output, nm) tryCatch({ output[[nm]]; "ok" }, error = function(e) paste0(class(e)[[1]], ": ", conditionMessage(e)))
  fam <- if (HAS_GLMMTMB) "poisson" else "gaussian"
  res <- safely(shiny::testServer(app, {
    session$setInputs(n_bins = 4, bin_method = "equal", show_se = TRUE, diff_lines = "pairs", a2_points = TRUE, trait_scale = "raw",
                      heat_order = "alr", heat_n = 500, a6_compare = "population", subset_var = "", miss_var = "ALR", dist_unit = "row",
                      a3_function = "Quadratic", a3_recon = "both", a3_n = 10, a3_order = "random", facet_var = "",
                      b2_functions = c("Linear", "Quadratic"), zi_vars = NULL, model_age_function = "Quadratic", random_structure = "none",
                      among_order = "linear", include_invalid = FALSE, standardise = TRUE, show_observed = TRUE, show_decomp = TRUE,
                      show_a3 = FALSE, pred_by = "", family_check_model = "M4")
    session$setInputs(data_source = "toy", toy_trait = count_trait, toy_form = "Quadratic", toy_strength = "dramatic", toy_sd_type = "both",
                      toy_sd_dir = "1", toy_rate_var = "low", toy_mean_ls = 12, toy_missingness = "complete", toy_afr_mode = "same",
                      toy_sa_type = "none", toy_sa_dir = "1", toy_diet = FALSE, toy_groups = FALSE, toy_n = 120, toy_seed = 3, dup_action = "keep")
    session$setInputs(toy_commit = 1)
    session$setInputs(col_id = tm$id, col_age = tm$age, col_trait = tm$trait, col_alr = "__AUTO_LAST__", col_entry = "__AUTO_FIRST__",
                      col_life = tm$life %||% "", col_cov_num = NULL, col_cov_fac = NULL, col_cov_int = NULL, col_group = "", group_nested = TRUE,
                      col_condition = "", col_random = NULL, col_censor = "", start_mode = "afr")
    session$setInputs(model_family = fam, use_M1 = TRUE, use_M2 = TRUE, use_M3 = FALSE, use_M4 = TRUE, use_M5 = FALSE, use_M6 = FALSE,
                      use_M7 = FALSE, use_M8 = FALSE, use_M9 = FALSE, use_M10 = FALSE)
    session$setInputs(fit_models = 1)
    record("state: AIC table after fitting", identical(out_state(output, "aic_table"), "ok"), out_state(output, "aic_table"))
    if (HAS_GLMMTMB) session$setInputs(run_family_check = 1)
    fam_before <- out_state(output, "family_table")
    session$setInputs(toy_seed = 4)
    session$setInputs(toy_commit = 2)
    s <- out_state(output, "aic_table")
    record("state: AIC table invalidated after new data", !identical(s, "ok") && grepl("changed since the last fit", s), s)
    if (HAS_GLMMTMB && identical(fam_before, "ok")) {
      s <- out_state(output, "family_table")
      record("state: family comparison invalidated after new data", !identical(s, "ok"), if (identical(s, "ok")) "still shows the comparison for the previous data" else s)
    }
    session$setInputs(fit_models = 2)
    session$setInputs(model_family = "gaussian")
    s <- out_state(output, "aic_table")
    record("state: AIC table invalidated after a family change", !identical(s, "ok"), s)
    session$setInputs(fit_models = 3)
    session$setInputs(use_M4 = FALSE)
    s <- out_state(output, "aic_table")
    record("state: AIC table invalidated after deselecting a model", !identical(s, "ok"), s)
    session$setInputs(fit_models = 4)
    session$setInputs(fit_models = 5)
    record("state: repeated clicks leave a valid fit", identical(out_state(output, "aic_table"), "ok"), out_state(output, "aic_table"))
  }))
  if (is_err(res)) record("state tests ran", FALSE, res$message)
})

# ---- 6. optional: large data timings (DISAPPR_BIG=1)
if (identical(Sys.getenv("DISAPPR_BIG"), "1")) {
  one <- simulate_toy_data(list(n_id = 500, seed = 9))
  for (n_copies in c(20, 50, 100)) {
    big <- do.call(rbind, lapply(seq_len(n_copies), function(k) { x <- one; x$ID <- paste0(x$ID, "_", k); x }))
    t1 <- system.time(b <- standardise_data(big, toy_mapping(big)))[["elapsed"]]
    t2 <- system.time(pv <- fit_model_suite(b$data, b$meta, c("M1", "M2"), "Quadratic", "gaussian", dry_run = TRUE))[["elapsed"]]
    t3 <- system.time(r <- fit_model_suite(b$data, b$meta, c("M1", "M2"), "Quadratic", "gaussian"))[["elapsed"]]
    t4 <- system.time(de <- decomposition_trajectory(b$data))[["elapsed"]]
    record(sprintf("large data: %d individuals, %d rows", length(unique(b$data$id)), nrow(b$data)), NA,
           sprintf("standardise %.1fs; row check %.1fs; fit M1-M2 %.1fs (ok %s); decomposition %.1fs", t1, t2, t3, isTRUE(r$ok), t4))
  }
}

setwd(old_wd)
cat("\n", sum(results$Result == "PASS"), " passed, ", sum(results$Result == "FAIL"), " failed, ", sum(results$Result == "INFO"), " info\n", sep = "")
if (any(results$Result == "FAIL")) print(results[results$Result == "FAIL", ], row.names = FALSE)
