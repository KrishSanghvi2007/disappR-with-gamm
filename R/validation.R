# disappR engine - Validation: data-integrity checks, covariate and age-type messages, sampling-schedule and duplicate-term checks.
# Engine code shared by the app and the R interface; changes that alter results must pass the golden tests.

# Checks shown under the covariate boxes of the Data tab when a column seems to be in the wrong box: text in a
# CONTINUOUS covariate (values that are not numbers become missing), or a CATEGORICAL covariate that looks
# continuous (numeric with decimals or many distinct values, so each value would become its own level).
covariate_type_warnings <- function(df, num = character(0), fac = character(0)) {
  num <- intersect(num %||% character(0), names(df))
  fac <- intersect(fac %||% character(0), names(df))
  msgs <- character(0)
  for (nm in intersect(num, fac)) {
    msgs <- c(msgs, sprintf("Warning: '%s' is in both boxes and is used as CATEGORICAL: remove it from one of them.", nm))
  }
  for (nm in setdiff(num, fac)) {
    ch <- trimws(as.character(df[[nm]]))
    present <- !is.na(ch) & nzchar(ch)
    if (!any(present)) next
    v <- suppressWarnings(as.numeric(ch[present]))
    bad <- !is.finite(v)
    ex <- paste0("'", utils::head(unique(ch[present][bad]), 3), "'", collapse = ", ")
    if (mean(bad) > 0.2) {
      msgs <- c(msgs, sprintf("Warning: '%s' is mapped as CONTINUOUS but %.0f%% of its values are not numbers (e.g. %s), so it looks CATEGORICAL. As a continuous covariate those values are treated as missing: move it to the CATEGORICAL box.",
                              nm, 100 * mean(bad), ex))
    } else if (any(bad)) {
      msgs <- c(msgs, sprintf("Note: %d values of the CONTINUOUS covariate '%s' are not numbers (e.g. %s) and are treated as missing.", sum(bad), nm, ex))
    } else {
      u <- sort(unique(v))
      # (whole numbers counting from zero, such as numbers of offspring, are left alone: they are counts, not codes)
      if (length(u) >= 3 && length(u) <= 5 && all(abs(u - round(u)) < 1e-8) && !(u[[1]] == 0 && all(diff(u) == 1))) {
        msgs <- c(msgs, sprintf("Note: the CONTINUOUS covariate '%s' has only %d distinct whole-number values (%s). If these are codes for groups (e.g. treatments or blocks), move it to the CATEGORICAL box.",
                                nm, length(u), paste(format(u, trim = TRUE), collapse = ", ")))
      }
    }
  }
  for (nm in setdiff(fac, num)) {
    ch <- trimws(as.character(df[[nm]]))
    present <- !is.na(ch) & nzchar(ch)
    if (!any(present)) next
    v <- suppressWarnings(as.numeric(ch[present]))
    if (mean(is.finite(v)) < 0.95) next
    u <- unique(v[is.finite(v)])
    decimals <- any(abs(u - round(u)) > 1e-8)
    if (decimals || length(u) > 20) {
      msgs <- c(msgs, sprintf("Warning: '%s' is mapped as CATEGORICAL but looks CONTINUOUS (%d distinct numeric values%s): each value becomes its own level. Move it to the CONTINUOUS box, or keep it here if the numbers are group codes.",
                              nm, length(u), if (decimals) ", with decimals" else ""))
    }
  }
  msgs
}

# Message for the column mapped as age: age must be numeric; values that are not numbers are dropped.
age_type_message <- function(x, col = "age") {
  ch <- trimws(as.character(x))
  present <- !is.na(ch) & nzchar(ch)
  if (!any(present)) return(sprintf("Warning: '%s' has no values.", col))
  v <- suppressWarnings(as.numeric(ch[present]))
  bad <- !is.finite(v)
  if (!any(bad)) return("")
  ex <- paste0("'", utils::head(unique(ch[present][bad]), 3), "'", collapse = ", ")
  sprintf("Warning: %d of %d values in '%s' are not numbers (e.g. %s). Age must be numeric, so these rows are dropped.%s",
          sum(bad), sum(present), col, ex,
          if (mean(bad) > 0.5) " The column looks categorical: map a numeric age column instead (convert dates or age classes to numbers first)." else "")
}

data_integrity <- function(dat, meta, step = meta$age_step) {
  rows <- list()
  add <- function(check, result, status, advice) {
    rows[[length(rows) + 1]] <<- data.frame(Check = check, Result = result, Status = status, Advice = advice,
                                            stringsAsFactors = FALSE)
  }
  if (!nrow(dat)) return(list(table = data.frame(), family = suggest_family(numeric(0))))
  im <- individual_metrics(dat)
  step <- resolve_age_step(dat$age, dat$id, step)
  schedule <- list(irregular = FALSE, n_schedules = 1, step = step, off_share = 0)
  add("Rows used", sprintf("%d of %d rows", meta$n_raw - meta$n_dropped, meta$n_raw),
      if (meta$n_dropped > 0) "Note" else "OK",
      if (meta$n_dropped > 0) "Rows without an ID or a numeric age were removed." else "")
  n_na <- sum(!is.finite(dat$trait))
  add("Missing trait values", sprintf("%d rows", n_na), if (n_na > 0) "Note" else "OK",
      if (n_na > 0) "Rows without a trait value count as recorded ages (for ALR/AFR) but are not modelled." else "")
  # Infinite values: nothing is blocked, but they distort every summary they enter and can make a fit fail
  # with a message that points somewhere else entirely, so they are flagged prominently here.
  inf_cols <- list()
  # the standardised data holds lifespan as `life` (individual_metrics() calls it `lifespan`) and ALR as `alr`
  for (nm in intersect(c("age", "trait", "life", "alr", "entry"), names(dat))) {
    v <- suppressWarnings(as.numeric(dat[[nm]]))
    k <- sum(is.infinite(v))
    if (k > 0) inf_cols[[nm]] <- k
  }
  n_inf <- sum(unlist(inf_cols))
  add("Infinite values (Inf, -Inf)",
      if (n_inf > 0) paste(sprintf("%d in %s", unlist(inf_cols), names(inf_cols)), collapse = "; ") else "none",
      if (n_inf > 0) "Warning" else "OK",
      if (n_inf > 0) paste0("Infinite values are kept and can still be mapped, including as age, but they are not ",
                            "real measurements: they pull means, ranges, bins and standardisation to infinity, and a ",
                            "model fitted with them usually fails with an error that names something else. They ",
                            "normally come from a division by zero or a spreadsheet overflow upstream. Check those ",
                            "rows and either correct them or mark them as missing before modelling.") else "")
  dup_detail <- if (isTRUE(meta$n_dup > 0)) paste0(
    sprintf("%d individual-age pair(s) repeat every mapped column (the same record entered twice); %d differ in %s.", meta$n_dup_identical %||% 0L,
            meta$n_dup_differing %||% 0L,
            if (length(meta$dup_columns)) paste(vapply(meta$dup_columns, display_term, character(1)), collapse = ", ") else "no column"),
    if (length(meta$dup_examples)) paste0(" For example: ", paste(meta$dup_examples, collapse = "; "), ".") else "",
    " Rows that differ are two measurements and are always kept; decide yourself whether to combine them.") else ""
  add("Duplicate ID \u00d7 age records", sprintf("%d rows", meta$n_dup), if (meta$n_dup > 0) "Warning" else "OK",
      if (meta$n_dup > 0) paste0(dup_detail, " ", if (identical(meta$dup_action, "mean")) "Exact duplicates were collapsed to one row." else "None were collapsed.", " (setting: ", meta$dup_action,
                                 "). Check for ID or age typos; choose 'average duplicates' if they are technical replicates. If the same ID is reused in different groups (families, vials, sites), do not average: map the grouping column and keep nesting, so that group + ID identifies the individual.") else "")
  n_single <- sum(im$n_trait == 1)
  add("Individuals with one trait record", sprintf("%d of %d (%.0f%%)", n_single, nrow(im), 100 * n_single / nrow(im)),
      if (n_single / nrow(im) > 0.25) "Note" else "OK",
      "They inform among-individual terms but contribute no within-individual change or decomposition pairs.")
  cf <- meta$conflicts %||% list()
  n_cf <- length(unique(unlist(cf, use.names = FALSE)))
  cf_txt <- if (n_cf) paste(vapply(names(cf), function(v) {
    ids <- cf[[v]]
    sprintf("%s differs within %d individual%s (%s%s)", switch(v, alr = "ALR", life = "lifespan", entry = "AFR", v), length(ids),
            if (length(ids) == 1) "" else "s", paste(utils::head(ids, 3), collapse = ", "), if (length(ids) > 3) ", ..." else "")
  }, character(1)), collapse = "; ") else "none"
  add("One ALR, lifespan and AFR per individual", if (n_cf) paste0(n_cf, " excluded: ", cf_txt) else "consistent",
      if (n_cf) "Warning" else "OK",
      if (n_cf) "These individuals had different values in different records (a column that repeats the age, or two individuals sharing an ID) and were excluded, as the mapping asked. Correct the data to include them." else "")
  # the last record the automatic ALR would use: with a trait value when trait-specific (0.24.2), else any record
  trait_specific <- !isFALSE(meta$trait_specific_ages)
  last_ref <- if (trait_specific && !is.null(im$last_observed)) im$last_observed else im$last_recorded
  if (isTRUE(meta$alr_mapped)) {
    mism <- sum(abs(im$alr - last_ref) > 1e-8, na.rm = TRUE)
    add(if (trait_specific) "Mapped ALR vs last age with a trait value" else "Mapped ALR vs last recorded age",
        sprintf("%d individuals differ", mism), if (mism > 0) "Warning" else "OK",
        if (mism > 0) paste0("ALR should equal the last ", if (trait_specific) "age with a trait value" else "recorded age",
                             "; check the column or use automatic calculation.") else "")
  }
  if (isTRUE(meta$has_censor)) {
    add("Censored individuals", sprintf("%d individuals (%s = %s)", meta$n_censored, meta$map$censor, meta$map$censor_value),
        if (meta$n_censored > 0) "Note" else "OK",
        "LS is set to missing for censored individuals: their recorded lifespan is not an age at death.")
  }
  if (isTRUE(meta$has_life) && !isTRUE(meta$life_auto)) {
    n_ls_na <- sum(!is.finite(im$lifespan))
    n_ls_bad <- sum(is.finite(im$lifespan) & im$lifespan < im$last_recorded - 1e-8)
    add("Lifespan (LS) missing", sprintf("%d individuals", n_ls_na), if (n_ls_na > 0) "Note" else "OK",
        if (n_ls_na > 0) "Model 6 drops these individuals; by default Model 6 is then left unselected so Models 1-5 use all data." else "")
    add("LS earlier than last recorded age", sprintf("%d individuals", n_ls_bad), if (n_ls_bad > 0) "Warning" else "OK",
        if (n_ls_bad > 0) "Impossible values: check lifespan or age records for these IDs (listed below)." else "")
    if (is.finite(step)) {
      # LS minus the ALR the models use when trait-specific (0.24.2); dataset-wide, minus the last record of any kind
      gap <- (im$lifespan - (if (trait_specific) im$alr else im$last_recorded)) / step
      gap <- gap[is.finite(gap)]
      if (length(gap)) {
        add("LS \u2212 ALR (time steps)", sprintf("median %.2f; %.0f%% \u2265 1 step", stats::median(gap), 100 * mean(gap >= 1 - 1e-8)),
            if (mean(gap >= 1 - 1e-8) > 0.1) "Note" else "OK",
            "Gaps of \u2265 1 step mean an individual was alive at a sampling occasion without a record (terminal missingness), or that ages and LS use different day conventions. Check that ages and lifespans use the same convention (Data tab).")
      }
    }
  }
  if (isTRUE(meta$has_group)) {
    nbad <- meta$n_multi_group %||% 0L
    add("IDs linked to >1 higher-level group", sprintf("%d IDs", nbad), if (nbad > 0) "Warning" else "OK",
        if (nbad > 0) paste0(
          if (length(meta$multi_group_examples)) paste0(paste(meta$multi_group_examples, collapse = "; "), ". ") else "",
          if (isTRUE(meta$nested)) paste0(
            "With nesting the individual is group/ID, so each of these is analysed as several individuals, each with its own random intercept, ALR, AFR and mean age. ",
            "That is right when IDs are reused between groups (a typo in the ID or group column would look the same). ",
            "If the same animal really moved between groups, map the grouping column as an additional (crossed) random term instead of a nesting level, so the animal stays one individual. ",
            if (isTRUE(meta$n_multi_group_gap > 0)) sprintf("%d of them have no group on some rows, which splits them the same way. ", meta$n_multi_group_gap) else "")
          else "Without nesting these rows share one individual.") else "Nested: individuals are identified by group + ID.")
  }
  if (isTRUE(meta$has_group2)) {
    nb2 <- meta$n_multi_group2 %||% 0L
    add("Groups linked to >1 top-level group", sprintf("%d groups", nb2), if (nb2 > 0) (if (isTRUE(meta$nested)) "Note" else "Warning") else "OK",
        if (nb2 > 0) paste("The same group label occurs in more than one top-level group.", if (isTRUE(meta$nested)) "With nesting, each top-level group/group combination is a separate group (e.g. father 1 of family A and father 1 of family B are different fathers)." else "Without nesting these rows share one group-level random intercept.") else "Three levels: individuals within groups within top-level groups.")
  }
  if (is.finite(step)) {
    fa <- stats::ave(dat$age, dat$id, FUN = min)
    rel <- (dat$age - fa) / step
    off <- abs(rel - round(rel)) > 0.01
    first_ages <- fa[!duplicated(dat$id)]
    phase <- round(((first_ages - min(dat$age)) / step) %% 1, 2)
    phase[phase >= 1] <- 0
    n_sched <- length(unique(phase))
    irregular <- mean(off) > 0.2
    schedule <- list(irregular = irregular, n_schedules = n_sched, step = step, off_share = mean(off))
    add("Sampling schedule",
        sprintf("%d distinct ages; step = %s; %d off-schedule rows%s", length(unique(dat$age)), format_num(step), sum(off),
                if (!irregular && n_sched > 1) sprintf("; %d offset schedules", n_sched) else ""),
        if (irregular) "Warning" else if (sum(off) > 0 || n_sched > 1) "Note" else "OK",
        if (irregular) {
          sprintf("Ages are irregular (not on a common sampling schedule), so the missingness grid, per-age means and the decomposition are approximate. Consider 'Round ages to multiples of' on the Data tab (e.g. %s).", format_num(signif(step, 2)))
        } else if (n_sched > 1) {
          "Individuals follow schedules offset from each other (e.g. staggered cohorts): expected occasions are anchored on each individual's first record."
        } else if (sum(off) > 0) {
          "Some ages are not on the schedule: they are assigned to the nearest expected occasion."
        } else "")
  }
  if (length(meta$skipped_columns)) {
    add("Covariates or random effects without values", paste(meta$skipped_columns, collapse = ", "), "Warning",
        "These columns contain no values and were not used.")
  }
  tv <- dat$trait[is.finite(dat$trait)]
  if (length(tv) >= 10) {
    spread <- stats::IQR(tv)
    if (!isTRUE(spread > 0)) spread <- stats::sd(tv)
    if (isTRUE(spread > 0)) {
      n_ext <- sum(abs(tv - stats::median(tv)) > 20 * spread)
      if (n_ext > 0) {
        add("Extreme trait values", sprintf("%d values more than 20 interquartile ranges from the median (maximum %s)", n_ext, format_num(max(abs(tv)))),
            "Note", "Check whether these are genuine (heavy-tailed counts) or data-entry errors or unit changes: a few extreme values can dominate models and plots.")
      }
    }
  }
  n_code <- sum(dat$trait %in% c(-99, -999, -9999)) + sum(dat$age %in% c(-99, -999, -9999)) + sum(dat$life %in% c(-99, -999, -9999))
  if (n_code > 0) {
    add("Possible missing-value codes", sprintf("%d values equal to -99, -999 or -9999", n_code), "Warning",
        "Recode missing-value codes as blank or NA before uploading; otherwise they are analysed as real values.")
  }
  if (any(dat$age <= 0)) {
    add("Ages at or below zero", sprintf("%d rows", sum(dat$age <= 0)), "Note",
        "Allowed. The logarithmic function then uses log(age \u2212 minimum age + 1).")
  }
  for (cv in meta$covars %||% character(0)) {
    x <- dat[[cv]]
    lab <- meta$cov_labels[[cv]]
    if (is.numeric(x)) {
      if (!isTRUE(stats::sd(x, na.rm = TRUE) > 0)) add(paste("Covariate", lab), "no variation", "Warning", "Omitted from the models.")
    } else {
      nl <- length(unique(x[!is.na(x)]))
      if (nl < 2) {
        add(paste("Covariate", lab), sprintf("%d level", nl), "Warning", "A factor needs at least two levels: omitted from the models.")
      } else if (nl > 50) {
        add(paste("Covariate", lab), sprintf("%d levels", nl), "Note", "Many levels: consider a random intercept instead of a fixed effect (slow fits, many parameters).")
      }
    }
    if (any(is.na(x))) add(paste("Covariate", lab, "missing"), sprintf("%d rows", sum(is.na(x))), "Note", "Rows with a missing covariate are dropped from every model.")
  }
  if (isTRUE(meta$has_trials)) {
    nb <- meta$n_bad_trials %||% 0L
    add("Number of binomial trials", sprintf("%s: %d rows without a positive number of trials", meta$trials_col, nb),
        if (nb > 0) "Warning" else "OK",
        if (nb > 0) "Rows without a positive number of trials are dropped from binomial models." else "Used as prior weights in the binomial families.")
  }
  add("Records per individual", sprintf("median %s (range %d\u2013%d)", format_num(stats::median(im$n_trait)), min(im$n_trait), max(im$n_trait)),
      if (stats::median(im$n_trait) < 4) "Note" else "OK",
      if (stats::median(im$n_trait) < 4) "Few records per individual: individual parametric fits will be restricted to long-lived individuals." else "")
  n_afr <- length(unique(im$entry[is.finite(im$entry)]))
  add("AFR variation", sprintf("%d distinct values", n_afr), if (n_afr < 2) "Warning" else "OK",
      if (n_afr < 2) "Fewer than two AFR values: selective appearance cannot be assessed and Models 7-10 are unavailable. Fitted anyway they would repeat Models 1-4, because their AFR terms are dropped as rank deficient." else "")
  if (n_afr >= 2) {
    r_aa <- cor_safe(im$entry, im$alr)
    add("AFR\u2013ALR correlation", if (is.finite(r_aa)) sprintf("r = %.2f", r_aa) else "NA",
        if (isTRUE(abs(r_aa) > 0.3)) "Note" else "OK",
        "Strongly correlated AFR and ALR make appearance (AFR) and disappearance (ALR) terms in Models 7-8 hard to separate.")
  }
  fam <- suggest_family(dat$trait, dat$age)
  add("Trait distribution", fam$text, if (fam$family != "gaussian") "Warning" else "OK",
      if (fam$family != "gaussian") "Model rankings can change with the error family: choose it on the Model comparison tab." else "")
  bad_ids <- if (isTRUE(meta$has_life) && !isTRUE(meta$life_auto)) im$id[is.finite(im$lifespan) & im$lifespan < im$last_recorded - 1e-8] else character(0)
  list(table = do.call(rbind, rows), family = fam, bad_ls_ids = bad_ids, schedule = schedule)
}

# Terms that exactly duplicate other terms (or are constant) in the analysed rows. They would be dropped as rank
# deficient when fitting; naming them shows, for example, that a covariate duplicates ALR or age.
duplicate_term_notes <- function(dd, formulas) {
  hits <- list()
  for (m in names(formulas)) {
    X <- tryCatch(stats::model.matrix(stats::as.formula(paste("~", formulas[[m]])), data = dd), error = function(e) NULL)
    if (is.null(X) || ncol(X) < 2) next
    sc <- sqrt(colSums(X^2))
    sc[!is.finite(sc) | sc == 0] <- 1
    qx <- qr(sweep(X, 2, sc, "/"), tol = 1e-7)
    if (qx$rank >= ncol(X)) next
    kept <- qx$pivot[seq_len(qx$rank)]
    for (j in qx$pivot[(qx$rank + 1):ncol(X)]) {
      cf <- tryCatch(qr.coef(qr(X[, kept, drop = FALSE]), X[, j]), error = function(e) NULL)
      big <- if (is.null(cf)) logical(0) else is.finite(cf) & abs(cf) > 1e-6 * max(1, max(abs(cf), na.rm = TRUE))
      partners <- setdiff(colnames(X)[kept][big], "(Intercept)")
      what <- if (!length(partners)) "is constant in the analysed rows" else if (length(partners) <= 3) {
        paste("duplicates", paste(display_term(partners), collapse = " + "))
      } else "is an exact combination of other terms"
      key <- paste(display_term(colnames(X)[j]), what)
      hits[[key]] <- c(hits[[key]], model_label(m))
    }
  }
  if (!length(hits)) return(character(0))
  vapply(names(hits), function(k) sprintf("%s (%s)", k, paste(unique(hits[[k]]), collapse = ", ")), character(1), USE.NAMES = FALSE)
}

# TRUE when more than a fifth of the records are not a whole number of sampling steps after the individual's
# first record (the integrity table's 'Sampling schedule' rule).
schedule_irregular <- function(dat, step = NULL) {
  ok <- is.finite(dat$age)
  if (sum(ok) < 3) return(FALSE)
  a <- dat$age[ok]
  id <- as.character(dat$id[ok])
  step <- resolve_age_step(a, id, step)
  if (!is.finite(step) || step <= 0) return(FALSE)
  rel <- (a - stats::ave(a, id, FUN = min)) / step
  mean(abs(rel - round(rel)) > 0.01) > 0.2
}
