# disappR engine - Model comparison: coefficient and variance tables, scaling, likelihood-ratio tests, family and function comparisons.
# Engine code shared by the app and the R interface; changes that alter results must pass the golden tests.

coef_table <- function(fit, label) {
  if (inherits(fit, "nlme")) {
    tt <- tryCatch(summary(fit)$tTable, error = function(e) NULL)
    if (is.null(tt)) return(NULL)
    return(data.frame(Model = label, Term = display_term(rownames(tt)), Raw_term = rownames(tt), Estimate = tt[, "Value"],
                      SE = tt[, "Std.Error"], Statistic = tt[, "t-value"], P_value = tt[, "p-value"],
                      P_method = "t (nlme)", stringsAsFactors = FALSE, row.names = NULL))
  }
  cf <- tryCatch({
    s <- summary(fit)$coefficients
    if (is.list(s) && !is.matrix(s)) s <- s$cond
    s
  }, error = function(e) NULL)
  if (is.null(cf) || !is.matrix(cf) || !"Estimate" %in% colnames(cf)) return(NULL)
  cn <- colnames(cf)
  stat_col <- grep("value$", cn, value = TRUE)
  p_col <- grep("^Pr\\(", cn, value = TRUE)
  est <- cf[, "Estimate"]
  se <- cf[, "Std. Error"]
  stat <- if (length(stat_col)) cf[, stat_col[[1]]] else est / se
  p <- if (length(p_col)) cf[, p_col[[1]]] else 2 * stats::pnorm(abs(stat), lower.tail = FALSE)
  # lmerTest reports t-tests with Satterthwaite df; glmmTMB and the normal approximation give asymptotic Wald z-tests
  p_method <- if (length(p_col) && grepl("|t|", p_col[[1]], fixed = TRUE)) "t, Satterthwaite df" else "Wald z, asymptotic"
  data.frame(Model = label, Term = display_term(rownames(cf)), Raw_term = rownames(cf), Estimate = est, SE = se, Statistic = stat,
             P_value = p, P_method = p_method, stringsAsFactors = FALSE, row.names = NULL)
}

# Scaling constants behind each coefficient. A term's scale factor is the product of the scales of its
# standardised components (age term powers, ALR/LS/AFR powers); dividing the estimate (and SE) by it gives
# the coefficient per original unit of the centred variables, e.g. per year of ALR, or per (age - mean age)^2.
# Numeric covariates are not standardised; factor contrasts have no scale.
scaling_constants <- function(res) {
  ap <- res$age_params
  pp <- res$proxy_params
  age_var <- switch(ap$fun, Logarithmic = if (isFALSE(ap$all_positive)) "log(age \u2212 youngest age + 1)" else "log(age)",
                    "Asymptotic exponential" = "exp(\u2212z_age)", "age")
  age_centre <- if (!isTRUE(ap$standardise)) 0 else switch(ap$fun, Logarithmic = ap$mean_log, "Asymptotic exponential" = ap$mean_exp, ap$mean_age)
  age_scale <- if (!isTRUE(ap$standardise)) 1 else switch(ap$fun, Logarithmic = ap$sd_log, "Asymptotic exponential" = ap$sd_exp, ap$sd_age)
  out <- data.frame(Variable = age_var, Centre = age_centre, Scale = age_scale, stringsAsFactors = FALSE)
  for (v in c("ALR", "LS", "AFR")) {
    if (!is.null(pp[[v]])) out <- rbind(out, data.frame(Variable = v, Centre = pp[[v]][["centre"]], Scale = pp[[v]][["scale"]], stringsAsFactors = FALSE))
  }
  out
}

term_scaling <- function(raw_terms, res) {
  sc <- scaling_constants(res)
  age_scale <- sc$Scale[[1]]
  age_var <- sc$Variable[[1]]
  poly <- res$age_params$fun %in% c("Linear", "Quadratic", "Cubic")
  scaled <- isTRUE(res$age_params$standardise)
  # Without standardisation the age terms are age, age^2 and age^3 themselves (0.22.3: the descriptions said
  # "(age - centre)^2"), and the intercept is the value at age 0 with every proxy at 0.
  icpt <- if (scaled) "value at the centre of every scaled variable" else
    paste0("value at ", age_var, " = 0 with every proxy at 0 (not standardised, so usually outside the sampled ages)")
  one <- function(raw) {
    if (identical(raw, "(Intercept)")) return(c(scale = NA_real_, unit = icpt))
    s <- 1
    units <- character(0)
    if (grepl("^[ab]\\.", raw)) {
      rate <- startsWith(raw, "b.")
      raw <- sub("^[ab]\\.", "", raw)
      if (rate) {
        s <- s * age_scale
        units <- c(units, "rate per age unit")
      }
      if (identical(raw, "(Intercept)")) return(c(scale = if (rate) s else NA_real_, unit = if (rate) "rate per age unit (in the exponent)" else if (scaled) "level a at the centre of every scaled variable" else "level a at age 0"))
    }
    parts <- strsplit(raw, ":", fixed = TRUE)[[1]]
    for (p in parts) {
      base <- sub("^(mean_|delta_)", "", p)
      if (grepl("^f[1-3]$", base)) {
        pw <- if (poly) as.integer(substring(base, 2)) else 1L
        s <- s * age_scale^pw
        units <- c(units, paste0(if (grepl("^mean_", p)) "individual mean " else if (grepl("^delta_", p)) "within-individual deviation of " else "",
                                 if (pw > 1) { if (scaled) paste0("(", age_var, " \u2212 centre)^", pw) else paste0(age_var, "^", pw) } else paste(age_var, "unit")))
      } else if (grepl("^(ALR|LS|AFR)[23]?$", p)) {
        v <- sub("[23]$", "", p)
        pw <- if (grepl("[23]$", p)) as.integer(substring(p, nchar(p))) else 1L
        s <- s * sc$Scale[sc$Variable == v][[1]]
        if (pw > 1) s <- s * sc$Scale[sc$Variable == v][[1]]^(pw - 1)
        v_scaled <- isTRUE(sc$Centre[sc$Variable == v][[1]] != 0 || sc$Scale[sc$Variable == v][[1]] != 1)
        units <- c(units, if (pw > 1) { if (v_scaled) paste0("(", v, " \u2212 centre)^", pw) else paste0(v, "^", pw) } else paste(v, "unit"))
      } else {
        units <- c(units, paste0(display_term(p), " (not scaled)"))
      }
    }
    c(scale = s, unit = paste(units, collapse = " \u00d7 "))
  }
  m <- vapply(raw_terms, one, character(2))
  data.frame(Scale_factor = suppressWarnings(as.numeric(m["scale", ])), Per = m["unit", ], stringsAsFactors = FALSE, row.names = NULL)
}

# Random-effect variances, standard deviations and correlations of a fitted lme4 or glmmTMB model,
# plus the residual SD (Gaussian) or the family dispersion parameter (negative binomial).
# Random terms that explain (next to) no variance. With a residual variance (Gaussian models) a term is flagged when it
# explains less than 1% of the total variance (random terms plus residual). Count and binomial models have no residual
# variance on the link scale, so there a term is flagged when its standard deviation is below 0.05 on the log or logit
# scale (individuals or levels differing by about 5% or less). One row per row of `vc` (one model's components).
negligible_random_terms <- function(vc, share_min = 0.01, sd_min = 0.05, data = NULL) {
  n <- if (is.data.frame(vc)) nrow(vc) else 0L
  out <- data.frame(flag = rep(FALSE, n), share = rep(NA_real_, n), sd = rep(NA_real_, n))
  if (!n || !all(c("Type", "Group", "Variance") %in% names(vc))) return(out)
  sd_rows <- vc$Type == "SD" & vc$Group != "Residual" & is.finite(vc$Variance)
  if (!any(sd_rows)) return(out)
  # A random slope's variance is per unit of its covariate, so on its own it depends on the covariate's scale: a slope
  # on unstandardised age^3 has a tiny variance per unit yet can matter a great deal. With the model data supplied
  # (0.22.3), each slope is judged by the variance it adds across the records: its variance x the mean of the
  # covariate squared. Intercepts are unchanged.
  eff <- vc$Variance
  if (is.data.frame(data) && "Term" %in% names(vc)) {
    for (i in which(sd_rows)) {
      tm <- vc$Term[[i]]
      if (!identical(tm, "(Intercept)") && tm %in% names(data) && is.numeric(data[[tm]])) {
        ms <- mean(data[[tm]]^2, na.rm = TRUE)
        if (is.finite(ms)) eff[[i]] <- vc$Variance[[i]] * ms
      }
    }
  }
  out$sd[sd_rows] <- sqrt(pmax(eff[sd_rows], 0))
  res <- vc$Type == "SD" & vc$Group == "Residual" & is.finite(vc$Variance)
  if (any(res)) {
    tot <- sum(eff[sd_rows | res])
    if (is.finite(tot) && tot > 0) {
      out$share[sd_rows] <- eff[sd_rows] / tot
      out$flag[sd_rows] <- out$share[sd_rows] < share_min
    }
  } else {
    out$flag[sd_rows] <- out$sd[sd_rows] < sd_min
  }
  out
}

# The flag's message. The individual random intercept stays in every model (the repeated records of an individual
# need it), so for that term the message says what a zero variance means instead of suggesting to drop it.
random_term_flag_text <- function(group, term, is_id, share = NA_real_, sd = NA_real_) {
  slope <- !identical(term, "(Intercept)")
  how <- if (is.finite(share)) sprintf("%.1f%% of the variance", 100 * share) else
    sprintf("SD %s on the link scale%s", format_num(sd), if (slope) " across the records" else "")
  if (is_id && !slope) {
    return(sprintf("Random term %s (intercept) explains no variance (%s): records of the same individual are no more alike than records of different individuals. It stays in the models, which need it for the repeated records of each individual.", group, how))
  }
  if (slope) {
    return(sprintf("Random term (%s) explains no variance (%s): individuals do not detectably differ in this term. Try refitting without this term.", term, how))
  }
  sprintf("Random term %s explains no variance (%s), try refitting without that term.", group, how)
}

variance_components <- function(fit, label) {
  tryCatch({
    if (inherits(fit, "nlme")) {
      vm <- unclass(nlme::VarCorr(fit))
      rn <- rownames(vm)
      grp <- "id"
      rows <- list()
      prev_term <- NULL
      for (i in seq_len(nrow(vm))) {
        if (grepl(" =$", rn[i])) {
          grp <- sub(" =$", "", trimws(rn[i]))
          next
        }
        sdv <- suppressWarnings(as.numeric(vm[i, "StdDev"]))
        if (!is.finite(sdv)) next
        g <- if (identical(trimws(rn[i]), "Residual")) "Residual" else grp
        rows[[length(rows) + 1]] <- data.frame(Model = label, Group = g, Term = trimws(rn[i]), Type = "SD", Estimate = sdv,
                                               Variance = suppressWarnings(as.numeric(vm[i, "Variance"])), stringsAsFactors = FALSE)
        if ("Corr" %in% colnames(vm)) {
          cr <- suppressWarnings(as.numeric(vm[i, "Corr"]))
          if (is.finite(cr) && !is.null(prev_term)) {
            rows[[length(rows) + 1]] <- data.frame(Model = label, Group = g, Term = paste(prev_term, "~", trimws(rn[i])), Type = "Correlation",
                                                   Estimate = cr, Variance = NA_real_, stringsAsFactors = FALSE)
          }
        }
        prev_term <- trimws(rn[i])
      }
      return(if (length(rows)) do.call(rbind, rows) else NULL)
    }
    if (inherits(fit, "merMod")) {
      vc <- as.data.frame(lme4::VarCorr(fit))
      is_sd <- is.na(vc$var2)
      data.frame(Model = label, Group = vc$grp,
                 Term = ifelse(is.na(vc$var1), "Residual", ifelse(is_sd, vc$var1, paste(vc$var1, "~", vc$var2))),
                 Type = ifelse(is_sd, "SD", "Correlation"), Estimate = vc$sdcor,
                 Variance = ifelse(is_sd, vc$vcov, NA_real_), stringsAsFactors = FALSE)
    } else {
      vc <- summary(fit)$varcor$cond
      rows <- list()
      for (g in names(vc)) {
        sds <- attr(vc[[g]], "stddev")
        rows[[length(rows) + 1]] <- data.frame(Model = label, Group = g, Term = names(sds), Type = "SD",
                                               Estimate = as.numeric(sds), Variance = as.numeric(sds)^2,
                                               stringsAsFactors = FALSE)
        cr <- attr(vc[[g]], "correlation")
        if (!is.null(cr) && length(sds) > 1) {
          ij <- which(upper.tri(cr), arr.ind = TRUE)
          rows[[length(rows) + 1]] <- data.frame(Model = label, Group = g,
                                                 Term = paste(rownames(cr)[ij[, 1]], "~", colnames(cr)[ij[, 2]]),
                                                 Type = "Correlation", Estimate = as.numeric(cr[ij]), Variance = NA_real_,
                                                 stringsAsFactors = FALSE)
        }
      }
      fam <- tryCatch(stats::family(fit)$family, error = function(e) "")
      # an age-dependent dispersion model (0.22.0): its coefficients on the log scale replace the single parameter
      bd <- tryCatch(glmmTMB::fixef(fit)$disp, error = function(e) NULL)
      if (length(bd) > 1) {
        rows[[length(rows) + 1]] <- data.frame(Model = label, Group = "Dispersion model", Term = paste("log-dispersion:", names(bd)),
                                               Type = "Coefficient", Estimate = as.numeric(bd), Variance = NA_real_,
                                               stringsAsFactors = FALSE)
      } else {
        disp <- tryCatch(if (grepl("nbinom|Gamma|gamma|lognormal|beta|gaussian", fam)) stats::sigma(fit) else NA_real_,
                         error = function(e) NA_real_)
        if (length(disp) == 1 && is.finite(disp)) {
          if (identical(fam, "gaussian")) {
            rows[[length(rows) + 1]] <- data.frame(Model = label, Group = "Residual", Term = "Residual", Type = "SD",
                                                   Estimate = disp, Variance = disp^2, stringsAsFactors = FALSE)
          } else {
            rows[[length(rows) + 1]] <- data.frame(Model = label, Group = "Family", Term = "Dispersion parameter",
                                                   Type = "Dispersion", Estimate = disp, Variance = NA_real_,
                                                   stringsAsFactors = FALSE)
          }
        }
      }
      if (length(rows)) do.call(rbind, rows) else NULL
    }
  }, error = function(e) NULL)
}

lrt_table <- function(fits, validity = list(), include_invalid = FALSE, formulas = NULL) {
  pairs <- list(
    c("M1", "M2", "Age-independent selective disappearance (ALR main effect)"),
    c("M2", "M4", "Age-dependent selective disappearance (ALR \u00d7 ageing terms)"),
    c("M3", "M5", "Age-dependent selective disappearance (mean age \u00d7 \u0394age terms)"),
    c("M2", "M7", "Age-independent selective appearance (AFR main effect, given ALR)"),
    c("M4", "M8", "Age-dependent selective appearance (AFR \u00d7 ageing terms, given ALR \u00d7 ageing)"),
    c("M7", "M8", "Age-dependent disappearance + appearance vs additive"),
    c("M7", "M9", "Age-dependent disappearance (ALR \u00d7 age), given additive AFR"),
    c("M7", "M10", "Age-dependent appearance (AFR \u00d7 age), given additive ALR"),
    c("M4", "M9", "Age-independent appearance (AFR main effect), given ALR \u00d7 age"),
    c("M9", "M8", "Age-dependent appearance (AFR \u00d7 age), given ALR \u00d7 age"),
    c("M10", "M8", "Age-dependent disappearance (ALR \u00d7 age), given AFR \u00d7 age")
  )
  rows <- lapply(pairs, function(pr) {
    a <- fits[[pr[1]]]; b <- fits[[pr[2]]]
    if (is.null(a) || is.null(b)) return(NULL)
    va <- validity[[pr[1]]] %||% "Valid"
    vb <- validity[[pr[2]]] %||% "Valid"
    if (!isTRUE(include_invalid) && (identical(va, "Failed") || identical(vb, "Failed"))) return(NULL)
    if (!is.null(formulas) && !is.null(formulas[[pr[1]]]) && !is.null(formulas[[pr[2]]]) &&
        !formula_nested(formulas[[pr[1]]], formulas[[pr[2]]])) return(NULL)
    la <- stats::logLik(a); lb <- stats::logLik(b)
    ddf <- attr(lb, "df") - attr(la, "df")
    if (ddf <= 0) return(NULL)
    chi <- max(0, 2 * (as.numeric(lb) - as.numeric(la)))
    data.frame(Comparison = paste(model_label(pr[1]), "vs", model_label(pr[2])), Tests = pr[3],
               Chisq = chi, df = ddf, P_value = stats::pchisq(chi, ddf, lower.tail = FALSE),
               Fits = if (identical(va, "Valid") && identical(vb, "Valid")) "Valid" else paste(va, "/", vb), stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# B1: error-family check. AICs are comparable among count families (all are
# probability mass functions for the same counts) but NOT with a Gaussian density.
# ---------------------------------------------------------------------------
compare_families <- function(dat, meta, model = "M4", age_function = "Quadratic", random_slope = FALSE,
                             standardise = TRUE, zi_str = "~1", families = COUNT_FAMILIES, progress = NULL,
                             among = "linear", include_invalid = FALSE, disp = "constant") {
  if (!HAS_GLMMTMB) return(list(ok = FALSE, message = "Install glmmTMB to compare error families."))
  v <- dat$trait[is.finite(dat$trait)]
  binom_set <- all(families %in% BINOMIAL_FAMILIES)
  cont_set <- all(families %in% DENSITY_FAMILIES)
  skipped <- character(0)
  if (cont_set) {
    # continuous families: each needs its own support (Gamma and lognormal > 0, beta strictly within 0-1)
    need_pos <- intersect(families, c("gamma", "lognormal"))
    if (length(need_pos) && any(v <= 0)) { skipped <- c(skipped, need_pos); families <- setdiff(families, need_pos) }
    if ("beta" %in% families && any(v <= 0 | v >= 1)) { skipped <- c(skipped, "beta"); families <- setdiff(families, "beta") }
    if (!length(families)) return(list(ok = FALSE, message = "None of the continuous families applies to these trait values."))
  } else if (binom_set) {
    if (any(v < 0) || any(v > 1)) return(list(ok = FALSE, message = "The trait is not binary or a proportion, so the binomial families do not apply."))
  } else if (any(v < 0) || any(abs(v - round(v)) > 1e-8)) {
    return(list(ok = FALSE, message = "The trait is not a non-negative integer count, so count families do not apply (use Gaussian)."))
  }
  rows <- list()
  for (i in seq_along(families)) {
    fam <- families[[i]]
    if (is.function(progress)) progress(i, length(families), family_label(fam))
    res <- fit_model_suite(dat, meta, models = model, age_function = age_function, family = fam,
                           random_slope = random_slope, standardise = standardise, zi_str = zi_str,
                           among = among, include_invalid = include_invalid, disp = disp)
    if (!isTRUE(res$ok) || !nrow(res$aic)) {
      rows[[fam]] <- data.frame(Family = family_label(fam), AIC = NA_real_, df = NA_real_, N = NA_real_, Fit = res$validity[[model]] %||% "Failed",
                                Status = paste(unlist(res$status), res$message, collapse = " "), stringsAsFactors = FALSE)
    } else {
      rows[[fam]] <- data.frame(Family = family_label(fam), AIC = res$aic$AIC[[1]], df = res$aic$df[[1]], N = res$aic$N[[1]], Fit = res$aic$Fit[[1]],
                                Status = unlist(res$status)[[1]], stringsAsFactors = FALSE)
    }
  }
  tab <- do.call(rbind, rows)
  if (any(is.finite(tab$AIC))) tab$Delta_AIC <- tab$AIC - min(tab$AIC, na.rm = TRUE) else tab$Delta_AIC <- NA_real_
  tab <- tab[order(tab$AIC, na.last = TRUE), c("Family", "AIC", "Delta_AIC", "df", "N", "Fit", "Status"), drop = FALSE]
  rownames(tab) <- NULL
  list(ok = TRUE, table = tab, model = model,
       skipped = if (length(skipped)) paste0(paste(vapply(skipped, family_label, character(1)), collapse = ", "),
                                             " not compared: values outside the family's range") else "")
}

# ---------------------------------------------------------------------------
# B2: comparison of ageing functions for one fixed-effect structure (Model 1 on the B2 tab, any
# model in the within-model check), with the chosen family, covariates and random effects.
# All functions use the same rows, so AICs are comparable.
# ---------------------------------------------------------------------------
compare_population_functions <- function(dat, meta, family = "gaussian", random_slope = FALSE, standardise = TRUE,
                                         zi_str = "~1", progress = NULL, functions = AGE_FUNCTIONS, model = "M1",
                                         among = "linear", include_invalid = FALSE, disp = "constant") {
  functions <- intersect(functions, MODEL_FUNCTIONS)
  rows <- list()
  curves <- list()
  for (i in seq_along(functions)) {
    fn <- functions[[i]]
    if (is.function(progress)) progress(i, length(functions), fn)
    res <- fit_model_suite(dat, meta, models = model, age_function = fn, family = family,
                           random_slope = random_slope, standardise = standardise, zi_str = zi_str,
                           among = among, include_invalid = include_invalid,
                           disp = if (identical(fn, A3_NONLINEAR)) "constant" else disp)
    if (!isTRUE(res$ok) || !nrow(res$aic)) {
      rows[[fn]] <- data.frame(Function = fn, AIC = NA_real_, df = NA_real_, N = NA_real_, Fit = res$validity[[model]] %||% "Failed",
                               Status = paste(unlist(res$status), res$message, collapse = " "), stringsAsFactors = FALSE)
      next
    }
    rows[[fn]] <- data.frame(Function = fn, AIC = res$aic$AIC[[1]], df = res$aic$df[[1]], N = res$aic$N[[1]], Fit = res$aic$Fit[[1]],
                             Status = unlist(res$status)[[1]], stringsAsFactors = FALSE)
    cv <- predict_population_curve(res$fits[[model]], res, prediction_ages(res$data$age))
    if (nrow(cv)) {
      cv$Function <- fn
      curves[[fn]] <- cv
    }
  }
  if (!length(rows)) return(list(table = data.frame(), curves = data.frame(), model = model, functions = functions))
  tab <- do.call(rbind, rows)
  tab$Delta_AIC <- if (any(is.finite(tab$AIC))) tab$AIC - min(tab$AIC, na.rm = TRUE) else NA_real_
  tab <- tab[order(tab$AIC, na.last = TRUE), c("Function", "AIC", "Delta_AIC", "df", "N", "Fit", "Status"), drop = FALSE]
  rownames(tab) <- NULL
  list(table = tab, curves = if (length(curves)) do.call(rbind, curves) else data.frame(), model = model, functions = functions)
}

# ---- Moved unchanged from inst/app/server.R (0.9.11): pure helpers that use no reactive state ----

# Terms made only of age terms and lifespan proxies (ALR, AFR, LS, mean age), additive or interactive: shown in bold in
# the model output table. Covariates and their interactions with age are not.
key_age_proxy_terms <- function(raw) {
  part_ok <- function(p) grepl("^(f[0-9]+|delta_f[0-9]+|mean_f[0-9]+(_[0-9]+)?|ALR[0-9]*|AFR[0-9]*|LS[0-9]*)$", p)
  vapply(strsplit(as.character(raw), ":", fixed = TRUE), function(ps) length(ps) > 0 && all(part_ok(ps)), logical(1))
}

coef_display <- function(x, r) {
  sc <- tryCatch(term_scaling(x$Raw_term, r), error = function(e) data.frame(Scale_factor = rep(NA_real_, nrow(x)), Per = ""))
  # significant digits as text, so that small coefficients are not displayed as 0.00 (0.22.3)
  data.frame(Term = x$Term, Estimate = sig_text(x$Estimate, 3), SE = sig_text(x$SE, 3),
             Statistic = ifelse(is.finite(x$Statistic), sprintf("%.2f", x$Statistic), "NA"),
             P = vapply(x$P_value, format_p, character(1)), `P type` = if (is.null(x$P_method)) "" else x$P_method,
             `Scale factor` = sig_text(sc$Scale_factor, 4), `Estimate per original unit` = sig_text(x$Estimate / sc$Scale_factor, 3),
             `SE per original unit` = sig_text(x$SE / sc$Scale_factor, 3), Per = sc$Per, check.names = FALSE, stringsAsFactors = FALSE)
}

fmt_ci <- function(lo, hi) {
  ifelse(is.finite(lo) & is.finite(hi), paste0(vapply(lo, format_num, character(1)), " to ", vapply(hi, format_num, character(1))), "NA")
}

fmt_r <- function(r) if (length(r) == 1 && is.finite(r)) sprintf("%.2f", r) else "NA"

aic_display <- function(aic) {
  out <- data.frame(Model = aic$Model, AIC = round(aic$AIC, 1), dAIC = round(aic$Delta_AIC, 1), Weight = round(aic$Weight, 3),
                    df = aic$df, logLik = round(aic$logLik, 1), N = aic$N)
  if ("R2_marginal" %in% names(aic)) out[["R2 fixed"]] <- round(aic$R2_marginal, 3)
  if ("R2_conditional" %in% names(aic)) out[["R2 total"]] <- round(aic$R2_conditional, 3)
  out
}

lrt_display <- function(l) {
  if (is.null(l) || !nrow(l)) return(data.frame())
  l$Chisq <- round(l$Chisq, 2)
  l$P_value <- vapply(l$P_value, format_p, character(1))
  l
}

# ---------------------------------------------------------------------------
# Stored models: any fitted model can be stored and compared with models from other fits (0.22.0) - another
# ageing function, random-effect structure, family, variance model or set of extra terms. AIC compares models fitted
# by maximum likelihood to the same response values, so stored models are grouped into comparison sets that share the
# data, the analysed rows and the kind of likelihood (a density for continuous traits, a probability mass for counts
# and binomial traits). Delta AIC is computed within a set only.
# ---------------------------------------------------------------------------
rows_key <- function(d, family = "gaussian") {
  if (!is.data.frame(d) || !nrow(d) || !all(c("id", "age", "trait") %in% names(d))) return(NA_character_)
  x <- data.frame(id = as.character(d$id), age = signif(as.numeric(d$age), 10), trait = signif(as.numeric(d$trait), 10),
                  stringsAsFactors = FALSE)
  if (isTRUE(family %in% BINOMIAL_FAMILIES) && ".trials" %in% names(d)) x$trials <- signif(as.numeric(d$.trials), 10)
  x <- x[order(x$id, x$age, x$trait), , drop = FALSE]
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  tryCatch({
    utils::write.csv(x, f, row.names = FALSE)
    unname(tools::md5sum(f))
  }, error = function(e) paste(nrow(x), signif(sum(x$age), 12), signif(sum(x$trait), 12)))
}

stored_model_entry <- function(res, m, data_sig = "") {
  if (is.null(res) || is.null(res$aic) || !nrow(res$aic)) return(NULL)
  a <- res$aic[res$aic$Model == model_label(m), , drop = FALSE]
  if (!nrow(a) || !is.finite(a$AIC[[1]])) return(NULL)
  disp <- res$disp %||% "constant"
  spec <- paste(c(res$age_function %||% "", family_label(res$family %||% "gaussian"),
                  if (nzchar(res$random %||% "")) display_term(res$random) else NULL,
                  if (!identical(res$among %||% "linear", "linear")) paste("among-individual terms:", res$among) else NULL,
                  if (!identical(disp, "constant")) "age-dependent variance" else NULL,
                  if (length(res$extra[[m]])) paste("extra terms:", paste(display_term(res$extra[[m]]), collapse = ", ")) else NULL),
                collapse = "; ")
  rk <- rows_key(res$data, res$family)
  list(key = paste(m, res$age_function, res$family, res$random, res$among, disp, res$zi %||% "", isTRUE(res$standardise),
                   res$formulas[[m]] %||% "", data_sig, rk, sep = "|"),
       model = m, label = model_label(m), spec = spec, formula = res$formulas[[m]] %||% "", random = res$random %||% "",
       age_function = res$age_function %||% "", family = res$family %||% "gaussian",
       family_class = family_density_class(res$family %||% "gaussian"), among = res$among %||% "linear", disp = disp,
       standardise = isTRUE(res$standardise), AIC = as.numeric(a$AIC[[1]]), logLik = as.numeric(a$logLik[[1]] %||% NA_real_),
       df = as.numeric(a$df[[1]]), N = as.numeric(a$N[[1]]), n_ind = length(unique(res$data$id)), fit = as.character(a$Fit[[1]]),
       data_sig = data_sig, rows = rk, time = format(Sys.time(), "%H:%M:%S"))
}

stored_set_labels <- function(store) {
  grp <- vapply(store, function(e) paste(e$data_sig, e$rows, e$family_class, sep = "|"), character(1))
  k <- match(grp, unique(grp))
  vapply(k, function(i) if (i <= 26) LETTERS[[i]] else paste0("S", i), character(1))
}

stored_comparison_table <- function(store) {
  if (!length(store)) return(data.frame())
  set <- stored_set_labels(store)
  aic <- vapply(store, function(e) as.numeric(e$AIC), numeric(1))
  d <- rep(NA_real_, length(aic))
  for (g in unique(set)) {
    k <- which(set == g & is.finite(aic))
    if (length(k)) d[k] <- aic[k] - min(aic[k])
  }
  out <- data.frame(ID = vapply(store, function(e) e$id %||% "", character(1)), Set = set,
                    Model = vapply(store, function(e) e$label, character(1)),
                    Specification = vapply(store, function(e) e$spec, character(1)),
                    N = vapply(store, function(e) e$N, numeric(1)), df = vapply(store, function(e) e$df, numeric(1)),
                    AIC = round(aic, 1), dAIC = round(d, 1),
                    Fit = vapply(store, function(e) e$fit %||% "", character(1)), stringsAsFactors = FALSE)
  out <- out[order(out$Set, out$dAIC, out$AIC), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Why the comparison sets differ from the first set: different data, different analysed rows, or a different kind
# of likelihood. One sentence per extra set.
stored_set_notes <- function(store) {
  if (length(store) < 2) return(character(0))
  set <- stored_set_labels(store)
  first <- store[[which(set == set[[1]])[[1]]]]
  out <- character(0)
  for (g in setdiff(unique(set), set[[1]])) {
    e <- store[[which(set == g)[[1]]]]
    why <- c(if (!identical(e$data_sig, first$data_sig)) "other data or data settings",
             if (identical(e$data_sig, first$data_sig) && !identical(e$rows, first$rows))
               sprintf("other records (%s vs %s)", format_num(e$N), format_num(first$N)),
             if (!identical(e$family_class, first$family_class))
               sprintf("%s likelihood (%s)", if (identical(e$family_class, "continuous")) "continuous" else "discrete", family_label(e$family)))
    if (!length(why)) why <- "another comparison basis"
    out <- c(out, sprintf("Set %s: %s; its AIC cannot be compared with set %s.", g, paste(why, collapse = " and "), set[[1]]))
  }
  out
}

# ---------------------------------------------------------------------------
# Among-individual covariance between the lifespan proxies and the trait (0.22.0): each individual's predicted random
# effects (BLUPs: level and, with random slopes, ageing rate) against its ALR, AFR and known lifespan. With Model 1,
# which carries no proxy, this reads Cov(LS, beta_0) and Cov(LS, beta_1) of the manuscript directly; with a model that
# already includes a proxy, it shows only the association left after that term.
# ---------------------------------------------------------------------------
blup_component_label <- function(x) {
  lab <- c(`(Intercept)` = "Level (intercept)", f1 = "Ageing rate (first age term)",
           f2 = "Curvature (second age term)", f3 = "Third age term")
  out <- unname(lab[x])
  out[is.na(out)] <- x[is.na(out)]
  out
}

blup_proxy_covariance <- function(res, m) {
  fit <- res$fits[[m]]
  if (is.null(fit)) return(list(ok = FALSE, message = "This model was not fitted."))
  re <- tryCatch({
    if (inherits(fit, "merMod")) lme4::ranef(fit)
    else if (inherits(fit, "glmmTMB")) glmmTMB::ranef(fit)$cond
    else NULL
  }, error = function(e) NULL)
  if (is.null(re) || !length(re)) {
    return(list(ok = FALSE, message = "Individual random effects are not available for this model (the non-linear exponential model is not supported here)."))
  }
  parts <- re[grepl("^id(\\.[0-9]+)?$", names(re))]
  if (!length(parts)) return(list(ok = FALSE, message = "The model has no individual random effects."))
  blup <- NULL
  for (p in parts) {
    p <- as.data.frame(p, check.names = FALSE, stringsAsFactors = FALSE)
    p$id <- rownames(p)
    blup <- if (is.null(blup)) p else merge(blup, p, by = "id", all = TRUE)
  }
  comps <- setdiff(names(blup), "id")
  d <- res$data
  ind <- d[!duplicated(d$id), , drop = FALSE]
  mi <- match(blup$id, as.character(ind$id))
  proxies <- c(ALR = "ALR_raw", AFR = "AFR_raw", LS = "LS_raw")
  rows <- list(); pts <- list()
  for (pv in names(proxies)) {
    col <- proxies[[pv]]
    if (!col %in% names(ind)) next
    x <- as.numeric(ind[[col]][mi])
    if (sum(is.finite(x)) < 5 || !isTRUE(stats::sd(x, na.rm = TRUE) > 0)) next
    for (cp in comps) {
      y <- as.numeric(blup[[cp]])
      ok <- is.finite(x) & is.finite(y)
      n <- sum(ok)
      if (n < 5 || !isTRUE(stats::sd(y[ok]) > 0)) next
      r <- stats::cor(x[ok], y[ok])
      z <- atanh(max(min(r, 0.999999), -0.999999))
      se <- if (n > 3) 1 / sqrt(n - 3) else NA_real_
      pval <- tryCatch(stats::cor.test(x[ok], y[ok])$p.value, error = function(e) NA_real_)
      rows[[length(rows) + 1]] <- data.frame(Component = blup_component_label(cp), Proxy = pv, N = n,
                                             Covariance = stats::cov(x[ok], y[ok]), r = r,
                                             Lower_95 = tanh(z - 1.96 * se), Upper_95 = tanh(z + 1.96 * se), P = pval,
                                             stringsAsFactors = FALSE)
      pts[[length(pts) + 1]] <- data.frame(Component = blup_component_label(cp), Proxy = pv, proxy = x[ok], blup = y[ok],
                                           stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) return(list(ok = FALSE, message = "Too few individuals with a proxy value and a random effect (at least 5 are needed)."))
  terms <- unlist(strsplit(res$formulas[[m]] %||% "", "[^A-Za-z0-9_.]+"))
  in_model <- names(proxies)[vapply(names(proxies), function(pv) any(terms %in% c(pv, paste0(pv, 2:3))), logical(1))]
  if (any(grepl("^mean_f", terms))) in_model <- c(in_model, "mean age")
  list(ok = TRUE, table = do.call(rbind, rows), points = do.call(rbind, pts), components = blup_component_label(comps),
       in_model = in_model, model = m, family = res$family, slopes = length(comps) > 1)
}
