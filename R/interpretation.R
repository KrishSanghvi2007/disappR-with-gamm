# disappR engine - Interpretation: evidence summaries and cautious interpretation rules built on the fitted results.
# Engine code shared by the app and the R interface; changes that alter results must pass the golden tests.

a2_interpretation <- function(a2, proxy, alpha = 0.05) {
  if (!nrow(a2$table)) return("Not estimable: need at least 5 individuals with trait data and proxy variation at two or more ages.")
  lab <- if (identical(a2$metric, "r")) "correlation" else "regression slope"
  main <- if (is.finite(a2$mean_p) && a2$mean_p < alpha) {
    sprintf("The %s between %s and the trait is on average %s (weighted mean p = %s), consistent with an age-independent component of selection.",
            lab, proxy, if (a2$mean_est > 0) "positive" else "negative", format_p(a2$mean_p))
  } else {
    sprintf("No consistent average %s between %s and the trait (p = %s).", lab, proxy, format_p(a2$mean_p))
  }
  trend <- if (is.finite(a2$trend_p) && a2$trend_p < alpha) {
    sprintf("The %s changes systematically with age (weighted trend p = %s): consistent with age-dependent selective disappearance/appearance.",
            lab, format_p(a2$trend_p))
  } else if (is.finite(a2$trend_p)) {
    sprintf("No systematic change of the %s with age (trend p = %s).", lab, format_p(a2$trend_p))
  } else {
    "Too few ages to test for a trend with age."
  }
  paste(main, trend)
}

# ---------------------------------------------------------------------------
# Plain-language interpretation of the selective disappearance / appearance terms of one model
# ---------------------------------------------------------------------------
# Uses Wald p-values of the terms, the nested likelihood-ratio test where available, and predicted
# population-level contrasts between individuals at the 10th and 90th percentiles of the proxy at a
# young, middle and old age (10th, 50th, 90th percentiles of the analysed ages).
# Joint (multi-degree-of-freedom) Wald test of a block of coefficients: chi2 = b' V^-1 b on
# length(b) degrees of freedom. A polynomial interaction is several coefficients, and testing the
# smallest of their individual p-values against 0.05 is not a 0.05 test of the interaction: with three
# coefficients the true error rate is roughly 12-14%. The nested likelihood-ratio test is preferred
# where one exists; this is the fallback when it does not.
joint_wald <- function(fit, terms) {
  if (is.null(fit) || !length(terms)) return(list(chisq = NA_real_, df = NA_integer_, p = NA_real_))
  b <- tryCatch({
    if (inherits(fit, "glmmTMB")) glmmTMB::fixef(fit)$cond else lme4::fixef(fit)
  }, error = function(e) NULL)
  V <- tryCatch({
    v <- stats::vcov(fit)
    if (inherits(fit, "glmmTMB")) as.matrix(v$cond) else as.matrix(v)
  }, error = function(e) NULL)
  if (is.null(b) || is.null(V)) return(list(chisq = NA_real_, df = NA_integer_, p = NA_real_))
  keep <- intersect(terms, names(b))
  keep <- intersect(keep, rownames(V))
  if (!length(keep)) return(list(chisq = NA_real_, df = NA_integer_, p = NA_real_))
  bb <- b[keep]
  VV <- V[keep, keep, drop = FALSE]
  q <- tryCatch(as.numeric(t(bb) %*% solve(VV) %*% bb), error = function(e) NA_real_)
  if (!is.finite(q) || q < 0) return(list(chisq = NA_real_, df = length(keep), p = NA_real_))
  list(chisq = q, df = length(keep), p = stats::pchisq(q, df = length(keep), lower.tail = FALSE))
}

# Contrast two population predictions (for individuals with a high and a low value of a lifespan proxy).
# The predictions are matched on age: predict_population_curve() drops any age whose prediction failed, so the two
# sides need not have the same ages, and matching by position would compare different ages. For Gaussian models the
# contrast is a difference; for count and binomial models it is a log ratio. Their response-scale predictions are
# positive in principle but can round to exactly zero (an extreme extrapolation, or a degenerate zero-inflation
# part), which would give 0, Inf or NaN: such ages are flagged as unusable rather than reported.
# The age at which every ageing term is zero (0.22.4): where a model's main-effect coefficients are evaluated when
# they also enter interactions with the ageing terms. Standardised: the mean age for the polynomials, and the age
# whose log or exponential term equals its mean for the other functions. Not standardised: age 0 for the
# polynomials, age 1 (or the youngest age) for the logarithm, and no age at all for the asymptotic exponential,
# whose term exp(-z) is never 0.
age_reference <- function(ap) {
  poly <- isTRUE(ap$fun %in% c("Linear", "Quadratic", "Cubic"))
  if (isTRUE(ap$standardise)) {
    a <- if (poly) ap$mean_age else if (identical(ap$fun, "Logarithmic")) {
      if (isTRUE(ap$all_positive)) exp(ap$mean_log) else exp(ap$mean_log) + ap$min_age - 1
    } else ap$mean_age - ap$sd_age * log(ap$mean_exp)
    lab <- if (poly) paste0("the mean age (", format_num(a), ")") else paste0("the centre of the age scale (age \u2248 ", format_num(a), ")")
  } else {
    a <- if (poly) 0 else if (identical(ap$fun, "Logarithmic")) { if (isTRUE(ap$all_positive)) 1 else ap$min_age } else Inf
    lab <- if (poly) "age 0" else if (identical(ap$fun, "Logarithmic")) {
      if (isTRUE(ap$all_positive)) "age 1, where log(age) is 0" else paste0("the youngest age (", format_num(a), "), where the log term is 0")
    } else "the limit where exp(\u2212z_age) is 0, which no age reaches"
  }
  list(age = a, label = lab)
}

prediction_contrast <- function(lo, hi, ratio) {
  pr <- merge(data.frame(age = lo$age, lo = lo$fitted), data.frame(age = hi$age, hi = hi$fitted), by = "age")
  pr <- pr[order(pr$age), , drop = FALSE]
  # no shared ages: return the empty frame with its columns, rather than failing on the assignment below
  if (!nrow(pr)) return(data.frame(age = numeric(0), lo = numeric(0), hi = numeric(0),
                                   eff = numeric(0), usable = logical(0)))
  usable <- is.finite(pr$lo) & is.finite(pr$hi)
  if (isTRUE(ratio)) usable <- usable & pr$lo > 0 & pr$hi > 0
  pr$eff <- NA_real_
  if (any(usable)) pr$eff[usable] <- if (isTRUE(ratio)) log(pr$hi[usable]) - log(pr$lo[usable]) else pr$hi[usable] - pr$lo[usable]
  pr$usable <- usable
  rownames(pr) <- NULL
  pr
}

interpret_model_terms <- function(res, m) {
  fit <- res$fits[[m]]
  if (is.null(fit)) return(character(0))
  ct <- res$coefficients[res$coefficients$Model == model_label(m), , drop = FALSE]
  if (!nrow(ct)) return("No coefficient table for this model.")
  # which selection terms does this model contain (standard models or extra terms)?
  term_parts <- strsplit(ct$Raw_term, ":", fixed = TRUE)
  has_term <- function(v) any(vapply(term_parts, function(pp) any(pp %in% c(v, paste0(v, 2:3), paste0("a.", v), paste0("b.", v))), logical(1)))
  has_mean <- any(grepl("^mean_f[1-3]$", ct$Raw_term) | grepl("(^|:)mean_f[1-3](:|$)", ct$Raw_term))
  if (!has_term("ALR") && !has_term("LS") && !has_term("AFR") && !has_mean) {
    return(paste0(model_label(m), " has no selective disappearance or appearance term: its trajectory describes the sampled records and is biased if individuals that disappear early differ from the others."))
  }
  count <- !identical(res$family, "gaussian")
  d <- res$data
  ind <- d[!duplicated(d$id), , drop = FALSE]
  ages <- as.numeric(stats::quantile(d$age, c(0.1, 0.5, 0.9), names = FALSE, type = 7))
  ages <- unique(ages)
  lrt_p <- function(cmp) {
    l <- res$lrt
    if (is.data.frame(l) && nrow(l) && cmp %in% l$Comparison) l$P_value[l$Comparison == cmp][[1]] else NA_real_
  }
  fmt_diff <- function(hi, lo) {
    if (count) {
      if (!(is.finite(hi) && is.finite(lo) && hi > 0 && lo > 0)) return("not estimable (a prediction rounds to zero)")
      r <- hi / lo
      sprintf("\u00d7%s (%s)", format_num(r), if (r >= 1) sprintf("%.0f%% higher", 100 * (r - 1)) else sprintf("%.0f%% lower", 100 * (1 - r)))
    } else {
      sprintf("%s%s", if (hi - lo >= 0) "+" else "\u2212", format_num(abs(hi - lo)))
    }
  }
  contrast <- function(label, var, hold_lo, hold_hi, lo_txt, hi_txt, main_terms, inter_terms, p_main_lrt, p_inter_lrt, lrt_names,
                       ref_mode = "age") {
    # Joint tests, not the smallest component p-value: LRT where a nested comparison exists,
    # otherwise a multi-df Wald chi-square over the whole block of coefficients.
    jw_main <- joint_wald(fit, main_terms)
    jw_int <- joint_wald(fit, inter_terms)
    p_main <- jw_main$p
    p_int <- jw_int$p
    lo <- predict_population_curve(fit, res, ages, hold = hold_lo)
    hi <- predict_population_curve(fit, res, ages, hold = hold_hi)
    if (!nrow(lo) || !nrow(hi)) return(paste0(label, ": predictions could not be computed."))
    pc <- prediction_contrast(lo, hi, ratio = count)
    if (!nrow(pc)) return(paste0(label, ": predictions could not be computed."))
    # Where the main-effect coefficient is evaluated (0.22.3): with standardised age, at the centre of the age scale
    # (the mean age for the polynomial functions); without, at age 0, which is usually outside the sampled ages, so
    # the main-effect test then says nothing about the sampled ages and the predicted difference at the median age is
    # reported instead.
    ap <- res$age_params
    scaled_age <- isTRUE(ap$standardise)
    ref <- age_reference(ap)
    ref_age <- ref$label
    ref_inside <- is.finite(ref$age) && ref$age >= min(d$age, na.rm = TRUE) && ref$age <= max(d$age, na.rm = TRUE)
    if (identical(ref_mode, "own")) {
      # the mean-age interactions use within-individual deviations, which are zero at each individual's own mean,
      # whether or not age is standardised
      scaled_age <- TRUE
      ref_age <- "each individual's own mean age"
    }
    mid <- pc[which.min(abs(pc$age - stats::median(ages))), , drop = FALSE]
    unscaled_note <- paste0(" Age is not standardised, so the main effect is the difference at ", ref_age,
                            if (ref_inside) "" else " (outside the sampled ages; its test says nothing about them)",
                            "; at the median age (", format_num(mid$age[[1]]), ") the ", if (count) "ratio" else "difference",
                            " is ", fmt_diff(mid$hi[[1]], mid$lo[[1]]), ".")
    at <- paste(vapply(seq_len(nrow(pc)), function(i) paste0(fmt_diff(pc$hi[i], pc$lo[i]), " at age ", format_num(pc$age[i])), character(1)), collapse = ", ")
    eff <- pc$eff
    ok <- all(pc$usable)
    shape <- if (!ok) {
      "how the difference changes with age cannot be described, because a prediction is zero or missing at some ages"
    } else if (length(eff) < 2) "" else if (any(eff > 0) && any(eff < 0)) {
      "the direction of the difference reverses across ages"
    } else if (abs(eff[length(eff)]) > 1.5 * abs(eff[1]) && abs(eff[length(eff)]) > 0) {
      "the difference is larger at older ages"
    } else if (abs(eff[1]) > 1.5 * abs(eff[length(eff)]) && abs(eff[1]) > 0) {
      "the difference is larger at younger ages"
    } else {
      "the difference is similar in size across ages"
    }
    # One test decides each verdict: the LRT when it exists, otherwise the joint Wald test. A single
    # small component coefficient never overrides a non-significant joint test.
    sig_int <- if (is.finite(p_inter_lrt)) p_inter_lrt < 0.05 else isTRUE(is.finite(p_int) && p_int < 0.05)
    sig_main <- if (is.finite(p_main_lrt)) p_main_lrt < 0.05 else isTRUE(is.finite(p_main) && p_main < 0.05)
    jw_txt <- function(j, lab) {
      if (!is.finite(j$p)) return(NULL)
      sprintf("%s joint Wald \u03c7\u00b2 = %s on %d df, p = %s", lab, format_num(j$chisq), as.integer(j$df), format_p(j$p))
    }
    evidence <- paste0(
      if (length(main_terms)) paste0(jw_txt(jw_main, "main-effect term(s)"), collapse = "") else NULL,
      if (length(inter_terms)) sprintf("%s%s", if (length(main_terms)) "; " else "", paste0(jw_txt(jw_int, "interaction term(s)"), collapse = "")) else NULL,
      if (is.finite(p_main_lrt)) sprintf("; LRT %s p = %s", lrt_names[[1]], format_p(p_main_lrt)) else "",
      if (is.finite(p_inter_lrt)) sprintf("; LRT %s p = %s", lrt_names[[2]], format_p(p_inter_lrt)) else "")
    what <- paste0("Predicted ", if (count) "ratio" else "difference", " between ", hi_txt, " and ", lo_txt, ": ", at,
                   if (nzchar(shape)) paste0("; ", shape) else "", ".")
    verdict <- if (length(inter_terms) && sig_int) {
      # the interaction decides the reading; the main effect says whether the difference is also there at the mean age
      main_note <- if (!length(main_terms)) "" else if (!scaled_age) unscaled_note
      else if (sig_main)
        paste0(" The main effect is also supported: the groups already differ at ", ref_age, ".")
      else
        paste0(" The main effect is not supported: at ", ref_age, " the groups differ little, and the association appears through its change with age.")
      paste0(label, ": consistent with an age-dependent pattern \u2014 the gap between ", hi_txt, " and ", lo_txt,
             " changes with age (", evidence, ").", main_note)
    } else if (sig_main) {
      paste0(label, ": consistent with an age-independent pattern \u2014 ", hi_txt, " differ from ", lo_txt, " by a similar amount at all ages",
             if (length(inter_terms)) " (the interaction is not clearly supported)" else "", " (", evidence, ").",
             if (length(inter_terms) && !scaled_age) unscaled_note else "")
    } else {
      paste0(label, ": no clear evidence for this term in this model (", evidence, "). With few long-lived individuals, a real effect can go undetected.",
             if (length(inter_terms) && !scaled_age) unscaled_note else "")
    }
    c(verdict, what)
  }
  pct <- function(x) as.numeric(stats::quantile(x[is.finite(x)], c(0.1, 0.9), names = FALSE, type = 7))
  proxy_part <- function(v, label_hi, label_lo, lrt_main, lrt_inter, lrt_names) {
    raw <- ind[[paste0(v, "_raw")]]
    if (is.null(raw) || sum(is.finite(raw)) < 3) return(character(0))
    q <- pct(raw)
    cs <- res$proxy_params[[v]]
    z <- (q - cs[["centre"]]) / cs[["scale"]]
    main <- intersect(c(v, paste0(v, 2:3), paste0("a.", v)), ct$Raw_term)
    inter <- ct$Raw_term[vapply(strsplit(ct$Raw_term, ":", fixed = TRUE), function(pp) {
      length(pp) > 1 && any(pp %in% c(v, paste0(v, 2:3))) && any(grepl("^f[1-3]$", pp))
    }, logical(1)) | ct$Raw_term == paste0("b.", v)]
    hl <- stats::setNames(list(z[1]), v)
    hh <- stats::setNames(list(z[2]), v)
    contrast(paste(v, "term"), v, hl, hh,
             paste0(label_lo, " (", v, " = ", format_num(q[1]), ")"), paste0(label_hi, " (", v, " = ", format_num(q[2]), ")"),
             main, inter, lrt_main, lrt_inter, lrt_names)
  }
  out <- character(0)
  if (has_term("ALR")) {
    out <- c(out, proxy_part("ALR", "individuals with a late last record (longer-lived)", "individuals with an early last record (shorter-lived)",
                             if (m %in% c("M2", "M4", "M7", "M8", "M9", "M10")) lrt_p("Model 1 vs Model 2") else NA_real_,
                             if (identical(m, "M4")) lrt_p("Model 2 vs Model 4") else if (identical(m, "M8")) lrt_p("Model 10 vs Model 8")
                             else if (identical(m, "M9")) lrt_p("Model 7 vs Model 9") else NA_real_,
                             c("Model 1 vs 2", switch(m, M8 = "Model 10 vs 8", M9 = "Model 7 vs 9", "Model 2 vs 4"))))
  }
  if (has_term("LS")) {
    out <- c(out, proxy_part("LS", "longer-lived individuals", "shorter-lived individuals", NA_real_, NA_real_, c("", "")))
  }
  if (has_term("AFR")) {
    out <- c(out, proxy_part("AFR", "individuals first observed later", "individuals first observed earlier",
                             if (m %in% c("M7", "M8", "M9", "M10")) lrt_p("Model 2 vs Model 7") else NA_real_,
                             if (identical(m, "M8")) lrt_p("Model 9 vs Model 8") else if (identical(m, "M10")) lrt_p("Model 7 vs Model 10") else NA_real_,
                             c("Model 2 vs 7", switch(m, M8 = "Model 9 vs 8", "Model 7 vs 10"))))
  }
  if (has_mean && !is.null(ind[["mean_f1"]])) {
    mf <- ind[["mean_f1"]]
    q <- pct(mf)
    near <- function(target) which.min(abs(mf - target))
    hold_for <- function(target) {
      j <- near(target)
      stats::setNames(lapply(res$basis, function(nm) ind[[paste0("mean_", nm)]][[j]]), paste0("mean_", res$basis))
    }
    main <- ct$Raw_term[grepl("^mean_f[1-3]$", ct$Raw_term)]
    inter <- ct$Raw_term[grepl("mean_f[1-3]", ct$Raw_term) & grepl("delta_f[1-3]", ct$Raw_term)]
    # the mean age reported is the real mean age of the individual whose mean age term is used (0.22.4): back-
    # transforming mean_f1 was right only for the standardised polynomials and gave a log or exponential-scale value,
    # labelled as an age, for the logarithmic and asymptotic exponential functions
    real_mean_age <- function(target) {
      j <- near(target)
      mean(d$age[as.character(d$id) == as.character(ind$id[[j]])], na.rm = TRUE)
    }
    # the asymptotic exponential term exp(-z) falls with age, so its low percentile is the older group: order the two
    # groups by their real mean ages so that 'younger' is younger for every function
    if (isTRUE(real_mean_age(q[1]) > real_mean_age(q[2]))) q <- rev(q)
    out <- c(out, contrast("Mean-age term", "mean_f1", hold_for(q[1]), hold_for(q[2]),
                           paste0("individuals whose records centre on younger ages (mean age \u2248 ", format_num(real_mean_age(q[1])), ")"),
                           paste0("individuals whose records centre on older ages (mean age \u2248 ", format_num(real_mean_age(q[2])), ")"),
                           main, inter, NA_real_, if (identical(m, "M5")) lrt_p("Model 3 vs Model 5") else NA_real_, c("", "Model 3 vs 5"),
                           ref_mode = "own"))
  }
  v <- res$validity[[m]] %||% "Valid"
  c(out,
    if (!identical(v, "Valid")) paste0("This fit is classified '", v, "': treat these estimates with caution.") else NULL,
    "Caveats: these are associations conditional on the model. ALR and mean age are proxies for lifespan that also depend on sampling, so conclusions should agree with the visual diagnosis plots, the residual checks and alternative random-effect structures and error families.")
}

# ---------------------------------------------------------------------------
# Internal consistency of the model evidence
# ---------------------------------------------------------------------------
consistency_notes <- function(r, deviation = NULL, a2_trend = NULL) {
  out <- character(0)
  if (is.null(r) || !isTRUE(r$ok)) return(out)
  aic <- r$aic[r$aic$Eligible, , drop = FALSE]
  if (!nrow(aic)) return(out)
  best <- aic$Model[[1]]
  if (is.data.frame(deviation) && nrow(deviation)) {
    dm <- deviation[grepl("^Model", deviation$Method), , drop = FALSE]
    if (nrow(dm) >= 2 && best %in% dm$Method) {
      dbest <- dm$Method[which.min(dm$Mean_abs_D)]
      gap <- dm$Mean_abs_D[dm$Method == best] - min(dm$Mean_abs_D)
      if (!identical(dbest, best) && gap > max(2, 0.25 * min(dm$Mean_abs_D))) {
        out <- c(out, sprintf("AIC favours %s, but %s recovers the simulated population trajectory most closely (mean |D| %.1f%% vs %.1f%%). AIC rewards fit to the sampled records (including random effects, zero inflation and the error family), whereas deviation measures how well the population-level curve matches the simulated typical individual. They can disagree when late ages are sparsely sampled (e.g. missing when old or few occasions), when a model fits the observed range well but extrapolates poorly, when holding ALR or AFR at their means describes a different 'average' individual than the simulation, or when selective appearance and disappearance are confounded. Report both and favour conclusions that agree across AIC, likelihood-ratio tests and the visual diagnostics.",
                              best, dbest, dm$Mean_abs_D[dm$Method == best], min(dm$Mean_abs_D)))
      }
    }
  }
  l <- r$lrt
  if (is.data.frame(l) && nrow(l)) {
    p24 <- if ("Model 2 vs Model 4" %in% l$Comparison) l$P_value[l$Comparison == "Model 2 vs Model 4"][[1]] else NA_real_
    if (is.finite(p24)) {
      if (best %in% c("Model 4", "Model 8", "Model 9") && p24 >= 0.05) {
        out <- c(out, sprintf("%s has the lowest AIC, but the likelihood-ratio test of the ALR \u00d7 age terms (Model 2 vs 4) is not significant (p = %s): AIC penalises extra parameters less than a 5%% test, so the evidence for age-dependent selective disappearance is weak.", best, format_p(p24)))
      }
      if (best %in% c("Model 1", "Model 2") && p24 < 0.05) {
        out <- c(out, sprintf("%s has the lowest AIC, but the likelihood-ratio test of the ALR \u00d7 age terms is significant (p = %s): the two criteria disagree, and both are worth reporting.", best, format_p(p24)))
      }
    }
  }
  best_id <- sub("^Model ", "M", best)
  if (identical(r$validity[[best_id]], "Caution")) {
    out <- c(out, sprintf("The lowest-AIC model (%s) is classified 'Caution' (%s): check whether a simpler random-effect structure or family gives the same conclusion.",
                          best, sub("^Caution: ", "", r$status[[best_id]] %||% "")))
  }
  inter_best <- best %in% c("Model 4", "Model 5", "Model 6", "Model 8", "Model 9", "Model 10")
  if (inter_best && identical(r$random_structure %||% "none", "none") && identical(r$random_advice$level %||% "none", "good")) {
    out <- c(out, sprintf("%s (with age-dependent terms) is favoured here while only random intercepts are fitted, although this might change if random slopes are fitted too. Individual differences in ageing rates can make interaction models fit better even in the absence of selective disappearance, through heterogeneity in ageing. The data support random slopes: refitting with a random slope may help to distinguish the two interpretations.", best))
  } else if (inter_best && identical(r$random_structure %||% "none", "none")) {
    out <- c(out, sprintf("%s (with age-dependent terms) is favoured with random intercepts only. In the app's validation simulations, random-intercept fits indicated age-dependent selection that was absent in up to about a third of datasets, and more often when individuals differed in ageing rates; a refit with a random slope, where the data allow it, would help to judge this.", best))
  }
  if (is.data.frame(a2_trend) && nrow(a2_trend) == 1 && is.finite(a2_trend$P[[1]])) {
    inter <- best %in% c("Model 4", "Model 5", "Model 6", "Model 8", "Model 9", "Model 10")
    if (inter && a2_trend$P[[1]] >= 0.05) {
      out <- c(out, sprintf("The lowest-AIC model has age-dependent terms, but the lifespan\u2013trait coefficient does not change clearly across age bins (p = %s): the visual diagnosis plots and the data support at old ages may help to judge this.", format_p(a2_trend$P[[1]])))
    } else if (!inter && best %in% c("Model 1", "Model 2", "Model 3", "Model 7") && a2_trend$P[[1]] < 0.05) {
      out <- c(out, sprintf("The lowest-AIC model has no age-dependent terms, but the lifespan\u2013trait coefficient changes across age bins (p = %s): the visual diagnostic and the model comparison disagree.", format_p(a2_trend$P[[1]])))
    }
  }
  if (isTRUE(r$has_censor) && best %in% c("Model 2", "Model 4", "Model 7", "Model 8", "Model 9", "Model 10")) {
    out <- c(out, "Some individuals are censored, so their ALR understates lifespan. In the app's validation simulations with censoring, AIC still favoured the ALR model (Model 4) although it tracked the trajectory less closely than the mean-age model (Model 5); comparing the two is advisable.")
  }
  if (isTRUE(r$schedule_irregular) && best %in% c("Model 3", "Model 5")) {
    out <- c(out, "Ages are not on a common sampling schedule. In the app's validation simulations with irregular ages, AIC tended to favour the mean-age model (Model 5) although the ALR model (Model 4) tracked the trajectory more closely; comparing the two is advisable.")
  }
  out
}


# ---------------------------------------------------------------------------
# What a saved result means for selective disappearance and ageing.
#
# Every saved result carries a short label so that a reader of the report knows what the panel was
# evidence ABOUT, and a verdict code so that the report can describe whether the pieces of evidence
# agree. The codes are deliberately coarse:
#   age_dependent   the panel points to a lifespan-trait association that changes with age
#   age_independent the panel points to an association that does not change with age
#   none            the panel shows no clear association
#   context         the panel describes the data or the fit, not the selective-disappearance question
#   caution         the panel flags something that undermines any reading of the others
saved_meaning <- function(section, title, text = character(0), verdict = NA_character_) {
  tt <- tolower(paste(c(title, text), collapse = " "))
  guess <- function() {
    if (grepl("no clear evidence|no selective|not clearly supported", tt)) return("none")
    if (grepl("age-dependent|changes with age|larger at older|interaction", tt)) return("age_dependent")
    if (grepl("age-independent|similar amount at all ages|similar in size across ages", tt)) return("age_independent")
    if (grepl("caution|warning|too few|imprecise|unreliable|failed|singular", tt)) return("caution")
    "context"
  }
  v <- if (!is.na(verdict)) verdict else guess()
  lab <- switch(v,
    age_dependent = "Points to AGE-DEPENDENT selective disappearance: the lifespan-trait association changes with age, so the uncorrected ageing trajectory is biased in shape and level.",
    age_independent = "Points to AGE-INDEPENDENT selective disappearance: longer- and shorter-lived individuals differ by a similar amount at all ages, so the shape of the ageing trajectory is largely unaffected.",
    none = "No clear selective disappearance in this panel: the ageing trajectory here is not obviously distorted by which individuals remain. Absence of evidence is not evidence of absence.",
    caution = "Caution for the ageing interpretation: this panel flags a limitation that weakens any conclusion drawn from the others.",
    "Context for the ageing analysis: describes the data, sampling or model fit rather than selective disappearance itself.")
  list(verdict = v, meaning = lab)
}

# Agreement between the saved results. Not a quality score: a description of whether the evidence lines up.
concordance_state <- function(saved) {
  if (!length(saved)) return(list(state = "Insufficient", text = "Nothing has been saved yet, so there is no evidence to compare."))
  v <- vapply(saved, function(x) x$verdict %||% "context", character(1))
  subst <- v[v %in% c("age_dependent", "age_independent", "none")]
  cautions <- sum(v == "caution")
  n_kinds <- length(unique(subst))
  if (length(subst) < 2) {
    return(list(state = "Insufficient",
                text = sprintf("Fewer than two saved results speak to selective disappearance (%d of %d). Save the model comparison and at least one visual diagnostic before drawing a conclusion.",
                               length(subst), length(v))))
  }
  if (cautions > 0 && n_kinds == 1) {
    return(list(state = "Fragile",
                text = sprintf("The %d saved results that address selective disappearance agree (%s), but %d saved result(s) flag a limitation that could change the reading. Treat the conclusion as provisional and check the sensitivity of the result to family, random slopes and proxy choice.",
                               length(subst), unique(subst), cautions)))
  }
  if (n_kinds == 1) {
    return(list(state = "Concordant",
                text = sprintf("All %d saved results that address selective disappearance point the same way (%s). Independent lines of evidence agreeing is the strongest case this app can make, though it still describes a pattern rather than a mechanism.",
                               length(subst), unique(subst))))
  }
  list(state = "Mixed",
       text = sprintf("The saved results disagree: %s. Model comparison, visual diagnostics and the decomposition need not agree, and a disagreement is informative - it usually means the effect is weak, confined to part of the age range, or sensitive to a modelling choice. Report the disagreement rather than the most favourable panel.",
                      paste(sprintf("%d say %s", as.integer(table(subst)), names(table(subst))), collapse = "; ")))
}

# One-line reading of the selection terms for the saved results: which are supported, and what that implies.
term_verdict_line <- function(res, m) {
  fit <- res$fits[[m]]
  ct <- res$coefficients[res$coefficients$Model == model_label(m), , drop = FALSE]
  if (is.null(fit) || !nrow(ct)) return(character(0))
  parts <- strsplit(ct$Raw_term, ":", fixed = TRUE)
  p_of <- function(terms) if (!length(terms)) NA_real_ else tryCatch(joint_wald(fit, terms)$p, error = function(e) NA_real_)
  block <- function(v) {
    main <- intersect(c(v, paste0(v, 2:3), paste0("a.", v)), ct$Raw_term)
    inter <- ct$Raw_term[vapply(parts, function(pp) length(pp) > 1 && any(pp %in% c(v, paste0(v, 2:3))) &&
                                  any(grepl("^(f[1-3]|delta_f[1-3])$", pp)), logical(1))]
    list(main = p_of(main), inter = p_of(inter), has_main = length(main) > 0, has_inter = length(inter) > 0)
  }
  lab <- function(v) switch(v, ALR = "ALR", LS = "lifespan", AFR = "AFR", mean_f1 = "mean age", v)
  sig <- function(p) isTRUE(is.finite(p) && p < 0.05)
  bits <- character(0)
  kind <- function(b) if (sig(b$inter)) "age-dependent" else if (sig(b$main)) "age-independent" else "none"
  dis <- "none"; app <- "none"
  for (v in c("ALR", "LS", "mean_f1", "AFR")) {
    b <- block(v)
    if (!b$has_main && !b$has_inter) next
    if (b$has_main) bits <- c(bits, sprintf("%s %s (p = %s)", lab(v), if (sig(b$main)) "supported" else "not supported", format_p(b$main)))
    if (b$has_inter) bits <- c(bits, sprintf("%s \u00d7 age %s (p = %s)", lab(v), if (sig(b$inter)) "supported" else "not supported", format_p(b$inter)))
    k <- kind(b)
    if (identical(v, "AFR")) { if (!identical(k, "none")) app <- k } else if (!identical(k, "none") && identical(dis, "none")) dis <- k
  }
  if (!length(bits)) return(paste0(model_label(m), " has no selective disappearance or appearance term."))
  verdict <- paste0(switch(dis, none = "no selective disappearance", paste(dis, "selective disappearance")), " and ",
                    switch(app, none = "no selective appearance", paste(app, "selective appearance")))
  paste0(paste(bits, collapse = "; "), ". Consistent with ", verdict, ", from ", model_label(m), " as specified above.")
}
