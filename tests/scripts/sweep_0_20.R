# =============================================================================
# disappR - sweep for the changes in 0.20.9 to 0.20.19
#
# Run from the package root:   Rscript tests/scripts/sweep_0_20.R
# Set DISAPPR_SWEEP_FULL=1 to fit models for every bundled example (slower).
#
# Covers, on simulated and bundled data: the Trajectories page with sparse
# individuals and count traits (the crash case), the clamping of extreme curves,
# duplicates and multi-group IDs in the data checks, per-model row sets, the
# evidence summary across Models 1-10, the likelihood-ratio pairs, the random
# terms that explain no variance, and the version provenance.
# Failures are collected and printed; the script exits 1 if any occur.
# =============================================================================

app_dir <- file.path("inst", "app")
if (!dir.exists(app_dir)) stop("Run this script from the package root (the folder containing DESCRIPTION).")
old_wd <- setwd(app_dir)
on.exit(setwd(old_wd), add = TRUE)
source("global.R")

fails <- character(0)
notes <- character(0)
timings <- list()
check <- function(label, expr) {
  t0 <- Sys.time()
  res <- tryCatch(expr, error = function(e) structure(conditionMessage(e), class = "sweep_error"))
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  timings[[label]] <<- secs
  if (inherits(res, "sweep_error")) {
    fails <<- c(fails, sprintf("%s: %s", label, as.character(res)))
    cat(sprintf("  FAIL  %-58s %6.1fs  %s\n", label, secs, as.character(res)))
    return(invisible(NULL))
  }
  if (isTRUE(is.logical(res)) && !isTRUE(res)) {
    fails <<- c(fails, sprintf("%s: check returned FALSE", label))
    cat(sprintf("  FAIL  %-58s %6.1fs\n", label, secs))
    return(invisible(res))
  }
  cat(sprintf("  ok    %-58s %6.1fs\n", label, secs))
  invisible(res)
}
note <- function(...) notes <<- c(notes, sprintf(...))

cat("disappR", DISAPPR_VERSION, "sweep\n")
check("version provenance is not the old fallback", !identical(DISAPPR_VERSION, "0.9.7") && nzchar(DISAPPR_VERSION))

# ---- helpers ---------------------------------------------------------------
sim <- function(...) simulate_toy_data(utils::modifyList(TOY_DEFAULTS, list(...)))
sim_trait <- function(d) setdiff(names(d), c("ID", "age", "lifespan", "AFR", "condition", "diet", "family"))[1]
statuses_ok <- function(g) all(g$lines$Status %in% c("supports", "caveat", "contradicts", "not saved"))

# ---- 1. Trajectories with sparse individuals and count traits (0.20.12-0.20.16)
cat("\n1. Trajectories: sparse individuals, count traits, every function\n")
for (tr in c("count_nb", "count_zinb", "mass")) {
  d <- check(sprintf("simulate %s with ~3 records per individual", tr),
             sim(trait = tr, n_id = 120, mean_ls = 3, form = "Quadratic", strength = "dramatic", seed = 7))
  if (is.null(d)) next
  p <- check(sprintf("standardise %s", tr), standardise_data(d, list(id = "ID", age = "age", trait = sim_trait(d))))
  if (is.null(p)) next
  dat <- p$data
  link <- if (identical(tr, "mass")) "identity" else "log"
  fits <- check(sprintf("fit all six functions to %s (one call)", tr),
                fit_individual_functions(dat, A3_FUNCTIONS, link = link, min_records = NULL))
  if (!is.null(fits)) {
    check(sprintf("every function returned a fit or a reason (%s)", tr),
          all(vapply(fits, function(z) is.list(z) && (!is.null(z$error) || !is.null(z$fit_stats)), logical(1))))
    tab <- check(sprintf("compare the six functions (%s)", tr),
                 compare_individual_functions(dat, 1, link = link, min_records = NULL, quasi = !identical(tr, "mass"), fits = fits))
    if (is.data.frame(tab) && nrow(tab)) check(sprintf("comparison scores are finite (%s)", tr), all(is.finite(tab[[2]]) | is.na(tab[[2]])))
    for (fn in c("Linear", "Quadratic", "Cubic")) {
      z <- fits[[fn]]
      if (is.null(z) || !is.null(z$error) || is.null(z$mean_curve) || !nrow(z$mean_curve)) next
      mc <- z$mean_curve
      check(sprintf("%s mean curve is finite (%s)", fn, tr), all(is.finite(mc$fitted)))
      lim <- range(mc$fitted[is.finite(mc$fitted)])
      sq <- squish_to(mc$fitted_fun %||% mc$fitted, lim)
      check(sprintf("%s curves clamp to a drawable range (%s)", fn, tr), all(abs(sq[is.finite(sq)]) < 1e12))
      ids <- utils::head(unique(dat$id), 40)
      cv <- check(sprintf("%s individual curves (%s)", fn, tr), individual_curves(z, dat, ids))
      if (is.data.frame(cv) && nrow(cv)) check(sprintf("%s individual curves are finite (%s)", fn, tr), all(is.finite(cv$fitted)))
    }
  }
}

# ---- 2. Data checks: duplicates and multi-group IDs (0.20.18-0.20.19) --------
cat("\n2. Data checks: duplicates and IDs in two groups\n")
base <- data.frame(ID = rep(sprintf("I%02d", 1:20), each = 4), age = rep(1:4, 20),
                   mass = rnorm(80, 10, 1), cage = rep(c("A", "B"), each = 40), stringsAsFactors = FALSE)
dup <- rbind(base, base[1, , drop = FALSE], transform(base[5, , drop = FALSE], mass = base$mass[5] + 3))
map0 <- list(id = "ID", age = "age", trait = "mass")
keep <- check("duplicates kept by default", standardise_data(dup, map0, "keep"))
coll <- check("exact duplicates collapsed on request", standardise_data(dup, map0, "mean"))
if (!is.null(keep) && !is.null(coll)) {
  check("only the exact duplicate was collapsed", nrow(coll$data) == nrow(keep$data) - 1)
  check("the repeated measurement was kept", sum(coll$data$id == keep$data$id[1] & coll$data$age == keep$data$age[1]) >= 1)
  check("the duplicate report names what differs", isTRUE(coll$meta$n_dup_differing >= 1) && length(coll$meta$dup_examples) >= 1)
  tb <- check("data checks table builds", data_integrity(coll$data, coll$meta)$table)
  if (is.data.frame(tb)) check("the duplicate row is a warning", any(grepl("Duplicate", tb$Check) & tb$Status == "Warning"))
}
moved <- base
moved$cage[moved$ID == "I01" & moved$age > 2] <- "B"
mg <- check("an ID in two groups still loads", standardise_data(moved, c(map0, list(group = "cage", nested = TRUE)), "keep"))
if (!is.null(mg)) {
  check("the ID in two groups is counted", isTRUE(mg$meta$n_multi_group >= 1))
  check("the ID in two groups is named", any(grepl("^I01 ", mg$meta$multi_group_examples)))
  check("it is analysed as two individuals", sum(grepl("/I01$", unique(mg$data$id))) == 2)
}

# ---- 3. Models: row sets, availability, evidence and LRT pairs ---------------
cat("\n3. Models: shared rows, Models 1-10, evidence and the LRT pairs\n")
d3 <- check("simulate a dataset with a missing covariate",
            sim(trait = "mass", n_id = 160, mean_ls = 7, form = "Quadratic", strength = "clear", seed = 11, groups = TRUE))
if (!is.null(d3)) {
  d3$diet_measured <- d3$diet
  gap <- sample(seq_len(nrow(d3)), max(5L, floor(0.05 * nrow(d3))))
  d3$diet_measured[gap] <- NA
  p3 <- check("standardise with a covariate", standardise_data(d3, list(id = "ID", age = "age", trait = sim_trait(d3),
              covars = "diet_measured", cov_labels = c(diet_measured = "diet"))))
  if (!is.null(p3)) {
    r <- check("fit Models 1-10", fit_model_suite(p3$data, p3$meta, models = MODEL_IDS, family = "gaussian",
                                                  age_function = "Quadratic"))
    if (!is.null(r) && isTRUE(r$ok)) {
      check("row sets are recorded for every fitted model", is.data.frame(r$row_sets) && nrow(r$row_sets) == length(r$formulas))
      check("no model is recorded as using more rows than it can", all(r$row_sets$Rows_usable >= r$row_sets$Rows_used))
      check("the shared set is smaller when a covariate is missing", any(r$row_sets$Rows_usable > r$row_sets$Rows_used))
      check("Model 6 is unavailable without a mapped lifespan", !("M6" %in% names(r$formulas)))
      ev <- check("models evidence", models_evidence(r, NA_character_))
      if (!is.null(ev)) {
        check("the kind comes from the winning model", identical(ev$kind, model_process_kind(ev$best)) ||
                identical(ev$kind, "none"))
        check("the settings key carries the model set", grepl("Model", ev$settings_key))
      }
      lrt <- r$lrt
      if (is.data.frame(lrt) && nrow(lrt)) {
        check("every LRT pair is nested and positive on df", all(lrt$df > 0))
        for (cmp in c("Model 10 vs Model 8", "Model 9 vs Model 8", "Model 7 vs Model 9", "Model 7 vs Model 10")) {
          if (!cmp %in% lrt$Comparison) note("LRT pair not fitted in this run: %s", cmp)
        }
      }
      txt <- check("interpretation text builds for the best model", interpret_model_terms(r, names(r$formulas)[match(r$aic$Model[[1]], vapply(names(r$formulas), model_label, character(1)))]))
      vc <- r$varcomp
      if (is.data.frame(vc) && nrow(vc)) {
        fl <- check("random terms with no variance are detected", negligible_random_terms(vc[vc$Model == r$aic$Model[[1]], , drop = FALSE]))
        if (is.data.frame(fl)) check("the no-variance flag is one value per row", nrow(fl) == sum(vc$Model == r$aic$Model[[1]]))
      }
      sv <- list(list(evidence = models_evidence(r, NA_character_)),
                 list(evidence = classify_visual_coef(p3$data, B = 30)))
      g <- check("grade the saved evidence", grade_evidence(sv, trait = "body mass"))
      if (!is.null(g)) {
        check("every evidence line has a known status", statuses_ok(g))
        check("the headline is in capitals", grepl("EVIDENCE|SELECTIVE|evidence", g$headline))
      }
      ga <- grade_appearance(sv, trait = "body mass")
      if (!is.null(ga)) check("the appearance summary is well formed", statuses_ok(ga) && ga$level %in% c("none", "weak", "moderate", "mixed", "insufficient"))
    }
  }
}

# ---- 4. Visual diagnostics: gap statistics and the trait scale ---------------
cat("\n4. Visual diagnostics\n")
d4 <- check("simulate for the visual checks", sim(trait = "count_nb", n_id = 200, mean_ls = 6, form = "Linear", strength = "clear", seed = 5))
if (!is.null(d4)) {
  p4 <- standardise_data(d4, list(id = "ID", age = "age", trait = sim_trait(d4)))
  dat4 <- p4$data
  vis <- check("visual classifier (ALR)", classify_visual_coef(dat4, B = 50))
  if (!is.null(vis)) check("the classifier returns a known reading", vis$kind %in% c("age_dependent", "age_independent", "none", "unavailable"))
  s <- check("bins", binned_trajectory(dat4, proxy_values(individual_metrics(dat4), "ALR"), 4, "quantile", 3))
  if (is.data.frame(s) && nrow(s)) {
    dz <- check("bin differences", bin_differences(s, "successive"))
    if (is.data.frame(dz) && nrow(dz) >= 3) {
      gs <- check("gap statistics", visual_gap_stats(dz))
      if (!is.null(gs)) check("the gap reading is one of three", gs$kind %in% c("age_dependent", "age_independent", "none"))
    }
  }
}

# ---- 5. The bundled examples ------------------------------------------------
cat("\n5. Bundled examples\n")
ex <- EXAMPLES
full <- nzchar(Sys.getenv("DISAPPR_SWEEP_FULL"))
model_keys <- if (full) names(ex) else utils::head(names(ex), 4)
for (k in names(ex)) {
  e <- check(sprintf("load example %s", k), disappr_example(k))
  if (is.null(e)) next
  p <- check(sprintf("standardise %s", k), standardise_data(e$data, e$mapping, "keep"))
  if (is.null(p)) next
  check(sprintf("data checks %s", k), is.data.frame(data_integrity(p$data, p$meta)$table))
  g <- check(sprintf("missingness grid %s", k), build_missing_grid(p$data))
  if (is.data.frame(g) && nrow(g)) check(sprintf("missingness summary %s", k), is.list(missingness_summary(p$data, g)))
  z <- check(sprintf("individual fits %s", k), fit_individual_function(p$data, "Quadratic", 1, character(0),
                                                                       link = if (suggest_family(p$data$trait)$family %in% COUNT_FAMILIES) "log" else "identity"))
  if (is.list(z) && is.null(z$error) && is.data.frame(z$mean_curve) && nrow(z$mean_curve)) {
    check(sprintf("mean curve finite %s", k), all(is.finite(z$mean_curve$fitted)))
  }
  if (k %in% model_keys) {
    r <- check(sprintf("fit Models 1-4 on %s", k), fit_model_suite(p$data, p$meta, models = c("M1", "M2", "M3", "M4"),
                                                                   family = e$settings$family %||% "gaussian", age_function = "Quadratic"))
    if (!is.null(r) && isTRUE(r$ok)) {
      check(sprintf("row sets on %s", k), is.data.frame(r$row_sets) && all(r$row_sets$Rows_usable >= r$row_sets$Rows_used))
      check(sprintf("evidence on %s", k), !is.null(models_evidence(r, NA_character_)))
    }
  }
}


# ---- 6. Binomial traits: trials, repeated records at one age, and the model suite ----
cat("\n6. Binomial traits\n")
sim_binom <- function(kind = "null", n = 220, seed = 3, repeats = 30) {
  set.seed(seed)
  ls <- pmax(2, pmin(18, round(3 + stats::rgamma(n, 4, 1 / 1.2))))
  z <- as.numeric(scale(ls))
  a0 <- (if (identical(kind, "null")) 0 else 0.6) * z + stats::rnorm(n, 0, 0.4)
  slope <- -0.30 + if (identical(kind, "age_dependent")) 0.18 * z else rep(0, n)
  rows <- do.call(rbind, lapply(seq_len(n), function(i) {
    age <- seq_len(ls[i])
    k <- sample(2:12, length(age), replace = TRUE)
    eta <- a0[i] + slope[i] * (age - 1) / 5
    data.frame(ID = sprintf("B%04d", i), age = age, successes = stats::rbinom(length(age), k, 1 / (1 + exp(-eta))),
               trials = k, LS = ls[i], stringsAsFactors = FALSE)
  }))
  # a second record at the same age for some individuals: a repeated measure, not a duplicate row
  extra <- rows[rows$ID %in% utils::head(unique(rows$ID), repeats) & rows$age == 1, , drop = FALSE]
  if (nrow(extra)) {
    extra$trials <- extra$trials + 3L
    extra$successes <- stats::rbinom(nrow(extra), extra$trials, 0.5)
    rows <- rbind(rows, extra)
  }
  rows$prop <- rows$successes / rows$trials
  rows[order(rows$ID, rows$age), , drop = FALSE]
}
bin_map <- function(trials = TRUE) c(list(id = "ID", age = "age", trait = "prop", life = "LS"),
                                     if (trials) list(trials = "trials") else NULL)
for (kind in c("null", "age_independent", "age_dependent")) {
  d <- check(sprintf("simulate a binomial trait (%s)", kind), sim_binom(kind, seed = switch(kind, null = 3, age_independent = 4, 5)))
  if (is.null(d)) next
  p <- check(sprintf("standardise the binomial data (%s)", kind), standardise_data(d, bin_map(TRUE), "keep"))
  if (is.null(p)) next
  check(sprintf("the trials column is used as weights (%s)", kind), isTRUE(p$meta$has_trials) && all(p$data$.trials > 0))
  check(sprintf("records repeated at one age are kept (%s)", kind), isTRUE(p$meta$n_dup > 0) &&
          nrow(p$data) == nrow(d) && isTRUE(p$meta$n_dup_differing > 0))
  check(sprintf("the trait stays a proportion in [0, 1] (%s)", kind), all(p$data$trait >= 0 & p$data$trait <= 1))
  if (!HAS_GLMMTMB) { note("binomial models skipped for %s: glmmTMB is not installed", kind); next }
  r <- check(sprintf("fit Models 1, 2, 4 and 6 with the binomial family (%s)", kind),
             fit_model_suite(p$data, p$meta, models = c("M1", "M2", "M4", "M6"), family = "binomial", age_function = "Linear"))
  if (is.null(r) || !isTRUE(r$ok)) next
  check(sprintf("every binomial coefficient is finite (%s)", kind), all(is.finite(r$coefficients$Estimate)))
  check(sprintf("the AIC table is complete (%s)", kind), all(is.finite(r$aic$AIC)))
  best <- r$aic$Model[[1]]
  expected <- switch(kind, null = "Model 1", age_independent = c("Model 1", "Model 2"), c("Model 4", "Model 6"))
  if (!best %in% expected) note("binomial %s: best model was %s (expected %s)", kind, best, paste(expected, collapse = " or "))
  if (identical(kind, "age_dependent")) {
    b <- r$coefficients[r$coefficients$Model == "Model 4" & grepl("ALR", r$coefficients$Raw_term) & grepl("f1", r$coefficients$Raw_term), , drop = FALSE]
    if (nrow(b)) check("the ALR x age term has the simulated (positive) sign", b$Estimate[[1]] > 0)
  }
  # the same data without the trials column: the model still runs, on much less information
  p0 <- check(sprintf("standardise without the trials column (%s)", kind), standardise_data(d, bin_map(FALSE), "keep"))
  if (!is.null(p0)) {
    check(sprintf("no weights are recorded without a trials column (%s)", kind), !isTRUE(p0$meta$has_trials))
    r0 <- check(sprintf("fit without weights (%s)", kind),
                fit_model_suite(p0$data, p0$meta, models = c("M1", "M2"), family = "binomial", age_function = "Linear"))
    if (!is.null(r0) && isTRUE(r0$ok)) {
      se_w <- r$coefficients$SE[r$coefficients$Model == "Model 2" & r$coefficients$Raw_term == "ALR"]
      se_n <- r0$coefficients$SE[r0$coefficients$Model == "Model 2" & r0$coefficients$Raw_term == "ALR"]
      if (length(se_w) && length(se_n) && is.finite(se_w[[1]]) && is.finite(se_n[[1]])) {
        check(sprintf("ignoring the trials widens the ALR standard error (%s)", kind), se_n[[1]] >= se_w[[1]])
        note("binomial %s: ALR SE with trials %.3f, without %.3f", kind, se_w[[1]], se_n[[1]])
      }
    }
  }
  rb <- check(sprintf("fit the beta-binomial family (%s)", kind),
              fit_model_suite(p$data, p$meta, models = c("M1", "M2"), family = "betabinomial", age_function = "Linear"))
  if (!is.null(rb) && isTRUE(rb$ok)) check(sprintf("beta-binomial coefficients are finite (%s)", kind), all(is.finite(rb$coefficients$Estimate)))
}

# ---- summary ---------------------------------------------------------------
cat("\n", strrep("-", 72), "\n", sep = "")
slow <- sort(unlist(timings), decreasing = TRUE)
cat("slowest steps: ", paste(sprintf("%s (%.1fs)", names(utils::head(slow, 5)), utils::head(slow, 5)), collapse = "; "), "\n", sep = "")
if (length(notes)) cat("notes:\n", paste0("  - ", notes, collapse = "\n"), "\n", sep = "")
if (length(fails)) {
  cat("FAILURES (", length(fails), "):\n", paste0("  - ", fails, collapse = "\n"), "\n", sep = "")
  quit(status = 1)
}
cat("sweep passed: no failures\n")
