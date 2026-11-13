# disappR engine - Diagnostics: starting error family, performance checks and simulation-based (DHARMa) residual checks.
# Engine code shared by the app and the R interface; changes that alter results must pass the golden tests.

# ---------------------------------------------------------------------------
# Data integrity diagnostics (new in 0.7.0)
# ---------------------------------------------------------------------------
# Starting suggestion only: a Poisson GLM on a quadratic in age gives a Pearson dispersion
# and the ratio of observed to expected zeros. Individual heterogeneity also inflates
# both, so confirm with the error-family check (GLMM AICs) on the Model comparison tab.
suggest_family <- function(trait, age = NULL) {
  ok <- is.finite(trait) & (if (is.null(age)) TRUE else is.finite(age))
  v <- trait[ok]
  if (length(v) < 10) return(list(family = "gaussian", kind = "Unknown", text = "Too few trait values to assess the distribution."))
  is_count <- all(v >= 0) && all(abs(v - round(v)) < 1e-8)
  # Proportions (values within 0-1 that are not all 0 or 1): binomial with a trials column.
  if (!is_count && all(v >= 0) && all(v <= 1)) {
    return(list(family = "binomial", kind = "Proportion (0-1)", zeros = mean(v == 0), dispersion = NA_real_,
                text = "Proportion trait (values between 0 and 1): binomial mixed models. Choose the number of trials (for example clutch size or the number of eggs laid) under 'Weights' below the error family on the Modelling tab, so that each proportion is weighted by its sample size; without it the proportions cannot be fitted as binomial and the Gaussian option is the fallback. If the proportions are more variable than binomial sampling allows, use the beta-binomial family."))
  }
  if (!is_count) return(list(family = "gaussian", kind = "Continuous",
                             text = paste0("Continuous trait: Gaussian mixed models (lme4).",
                                           if (all(v > 0)) " All values are positive: if right-skewed, compare Gamma and lognormal with the error-family check." else "")))
  # A binary (0/1) trait is not a count: the count families would be wrong, and there is no binomial
  # family here, so Gaussian is offered as a linear-probability approximation.
  if (all(v %in% c(0, 1))) {
    return(list(family = "binomial", kind = "Binary (0/1)", zeros = mean(v == 0), dispersion = NA_real_,
                text = sprintf("Binary trait (only 0 and 1, %.0f%% zeros): binomial mixed models with a logit link (glmmTMB). The count families do not apply. The Gaussian option fits a linear-probability model instead: it does not keep predictions within 0 and 1 and, because the variance of a binary trait changes with the predicted probability, it can favour the interaction models (4, 6) even when there is no selective disappearance at all, so prefer the binomial family here.",
                               100 * mean(v == 0))))
  }
  zeros <- mean(v == 0)
  disp <- NA_real_
  zero_ratio <- NA_real_
  if (!is.null(age) && length(unique(age[ok])) >= 3 && mean(v) > 0) {
    a <- age[ok]
    g <- tryCatch(suppressWarnings(stats::glm(v ~ a + I(a^2), family = stats::poisson())), error = function(e) NULL)
    if (!is.null(g)) {
      mu <- stats::fitted(g)
      disp <- sum((v - mu)^2 / mu) / max(1, length(v) - 3)
      expected_zeros <- mean(stats::dpois(0, mu))
      zero_ratio <- if (expected_zeros > 0) zeros / expected_zeros else Inf
    }
  }
  if (!is.finite(disp) && mean(v) > 0) disp <- stats::var(v) / mean(v)
  # Underdispersed counts (clutch or litter size, for example) are not Poisson: Poisson would
  # overstate the residual variance, so Gaussian is the better starting point.
  under <- isTRUE(disp < 0.7)
  fam <- if (under) "gaussian" else if (isTRUE(zero_ratio > 1.5) && zeros > 0.1) "zinb" else if (isTRUE(disp > 2)) "nbinom2" else "poisson"
  zr_txt <- if (is.finite(zero_ratio)) {
    if (zero_ratio > 999) " (more than 999\u00d7 the Poisson expectation)" else sprintf(" (%.1f\u00d7 the Poisson expectation)", zero_ratio)
  } else ""
  txt <- sprintf("Non-negative integer trait: %.0f%% zeros%s, Pearson dispersion %.1f.%s Starting suggestion: %s; the error-family check can test this.",
                 100 * zeros, zr_txt, disp,
                 if (under) " The counts are underdispersed (dispersion well below 1), so a Poisson model would overstate the error." else "",
                 family_label(fam))
  list(family = fam, kind = if (under) "Counts (underdispersed)" else "Counts", text = txt, zeros = zeros, dispersion = disp)
}

run_performance <- function(fit, family, time_limit = 60) {
  if (!requireNamespace("performance", quietly = TRUE)) {
    return(list(ok = FALSE, message = "Install the performance package (install.packages('performance')) for these checks."))
  }
  tryCatch(run_performance_checks(fit, family, time_limit),
           error = function(e) list(ok = FALSE, message = paste("The performance checks could not be run for this model:", conditionMessage(e))))
}

run_performance_checks <- function(fit, family, time_limit = 60) {
  num1 <- function(x) {
    x <- suppressWarnings(as.numeric(unlist(x)))
    x <- x[is.finite(x)]
    if (length(x)) x[[1]] else NA_real_
  }
  element <- function(x, name) if (is.list(x) && !is.null(x[[name]])) x[[name]] else NULL
  explain <- function(e) {
    msg <- conditionMessage(e)
    if (grepl("time limit", msg, ignore.case = TRUE)) paste0("stopped after ", time_limit, " s (too slow for this model)") else paste("not available for this model:", msg)
  }
  rows <- list()
  run_check <- function(label, note, code) {
    res <- tryCatch(with_time_limit(suppressWarnings(suppressMessages(code)), time_limit), error = explain)
    if (!is.character(res) || length(res) != 1 || is.na(res) || !nzchar(res)) res <- "not estimable"
    rows[[length(rows) + 1]] <<- data.frame(Check = label, Result = res, Note = note, stringsAsFactors = FALSE)
  }
  run_check("R\u00b2 (marginal / conditional)",
            "Nakagawa R\u00b2: variance explained by fixed effects (marginal) and by fixed plus random effects (conditional).", {
    r2 <- performance::r2(fit)
    vals <- suppressWarnings(as.numeric(unlist(r2)))
    nm <- names(unlist(r2))
    ok <- is.finite(vals)
    if (any(ok) && length(nm) == length(vals)) paste(paste0(sub("^R2_", "", nm[ok]), " = ", sprintf("%.3f", vals[ok])), collapse = "; ") else "not estimable"
  })
  run_check("ICC (adjusted)", "Share of variance due to the random effects.", {
    v <- num1(element(performance::icc(fit), "ICC_adjusted"))
    if (is.finite(v)) format_num(v) else "not estimable"
  })
  run_check("Singular fit", "Singular: a random-effect variance is estimated at (or near) zero; simplify the random effects.", {
    if (isTRUE(performance::check_singularity(fit))) "yes" else "no"
  })
  run_check("Convergence", "", {
    if (isTRUE(performance::check_convergence(fit))) "converged" else "possible convergence problem"
  })
  run_check("Collinearity (largest VIF)", "Polynomial and interaction terms are collinear by construction; high VIFs for them are expected and not a fault.", {
    coll <- performance::check_collinearity(fit)
    vif <- if (is.data.frame(coll) && "VIF" %in% names(coll)) suppressWarnings(as.numeric(coll$VIF)) else numeric(0)
    if (any(is.finite(vif))) {
      j <- which(vif == max(vif[is.finite(vif)]))[[1]]
      sprintf("%s (%s)", format_num(vif[[j]]), if ("Term" %in% names(coll)) as.character(coll$Term[[j]]) else "")
    } else "not estimable"
  })
  if (family %in% c(COUNT_FAMILIES, BINOMIAL_FAMILIES)) {
    run_check("Overdispersion", if (family %in% BINOMIAL_FAMILIES) "Ratios well above 1 suggest a beta-binomial family (proportions) or a missing random effect." else "Ratios well above 1 suggest a negative binomial family.", {
      od <- performance::check_overdispersion(fit)
      ratio <- num1(element(od, "dispersion_ratio"))
      if (is.finite(ratio)) sprintf("dispersion ratio %s, p = %s", format_num(ratio), format_p(num1(element(od, "p_value")))) else "not estimable"
    })
    if (family %in% COUNT_FAMILIES) run_check("Zero inflation", "Ratios below 1 mean the model predicts fewer zeros than observed.", {
      zi <- performance::check_zeroinflation(fit)
      obs <- num1(element(zi, "observed.zeros"))
      if (is.finite(obs)) sprintf("observed %s vs predicted %s zeros (ratio %s)", format_num(obs), format_num(num1(element(zi, "predicted.zeros"))),
                                  format_num(num1(element(zi, "ratio")))) else "not estimable"
    })
  }
  cm <- NULL
  plot_message <- ""
  if (requireNamespace("see", quietly = TRUE)) {
    # zero-inflated and other glmmTMB count models: skip the panels that need many simulations or refits
    checks <- if (inherits(fit, "glmmTMB")) c("qq", "reqq", "homogeneity", "linearity", "vif") else "all"
    cm <- tryCatch(with_time_limit(suppressWarnings(suppressMessages(performance::check_model(fit, check = checks, verbose = FALSE))), time_limit),
                   error = function(e) e)
    if (inherits(cm, "error")) {
      plot_message <- paste("performance::check_model() panels are not available for this model:", explain(cm))
      cm <- NULL
    }
  } else {
    plot_message <- "Install the see package (install.packages('see')) to draw performance::check_model() panels."
  }
  list(ok = TRUE, table = do.call(rbind, rows), check_model = cm, plot_message = plot_message)
}

run_dharma <- function(fit, family) {
  if (!HAS_DHARMA) return(list(ok = FALSE, message = "Install the DHARMa package for simulation-based residual checks."))
  sim <- tryCatch(DHARMa::simulateResiduals(fit, n = 250, plot = FALSE), error = function(e) e)
  if (inherits(sim, "error")) return(list(ok = FALSE, message = paste("DHARMa failed:", conditionMessage(sim))))
  p_of <- function(expr) tryCatch(expr$p.value, error = function(e) NA_real_)
  tab <- data.frame(
    Test = c("Uniformity (KS)", "Dispersion", "Zero inflation"),
    P_value = c(p_of(DHARMa::testUniformity(sim, plot = FALSE)),
                p_of(DHARMa::testDispersion(sim, plot = FALSE)),
                if (!family %in% c(COUNT_FAMILIES, BINOMIAL_FAMILIES)) NA_real_ else p_of(DHARMa::testZeroInflation(sim, plot = FALSE))),
    stringsAsFactors = FALSE)
  list(ok = TRUE, sim = sim, table = tab)
}

# ---- Moved unchanged from inst/app/server.R (0.9.11): pure helpers that use no reactive state ----

draw_check_model <- function(cm) {
  function() {
    tryCatch(with_time_limit(print(plot(cm)), 90), error = function(e) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, paste("check_model() panels could not be drawn:", conditionMessage(e)), cex = 0.9)
    })
    invisible(NULL)
  }
}

# ---- Random-effect checks before fitting (0.9.13) ----

# Can the data support the chosen random effects? Levels and records per level, repeated records per individual,
# individuals with enough distinct ages for slopes, and settings that often end in singular (boundary) fits.
random_effect_checks <- function(dd, random_structure = "none", random_terms = character(0), has_group = FALSE, has_group2 = FALSE) {
  rows <- list()
  add <- function(check, result, ok) rows[[length(rows) + 1]] <<- data.frame(Check = check, Result = result,
                                                                               Status = if (ok) "OK" else "Caution", stringsAsFactors = FALSE)
  sup <- individual_data_support(dd)
  if (!is.null(sup)) {
    add("Repeated records per individual",
        sprintf("%s individuals, median %s records; %.0f%% have records at 2 or more ages", sup$individuals, format_num(sup$median_obs), sup$pct_2),
        sup$pct_2 >= 50)
    if (random_structure %in% c("correlated", "uncorrelated", "correlated_all", "uncorrelated_all")) {
      add("Support for random slopes", sprintf("%d individuals have 3 or more distinct ages (%.0f%%)", as.integer(sup$n_3), sup$pct_3),
          sup$n_3 >= 10 && sup$pct_3 >= 20)
    }
  }
  level_check <- function(label, x) {
    x <- x[!is.na(x)]
    tab <- table(x)
    add(paste("Levels of", label), sprintf("%d levels; %s to %s records per level", length(tab), min(tab), max(tab)), length(tab) >= 5)
  }
  if (isTRUE(has_group2) && "group2" %in% names(dd)) level_check("the top grouping level", dd$group2)
  if (isTRUE(has_group) && "group" %in% names(dd)) {
    level_check("the grouping level", dd$group)
    per_group <- tapply(as.character(dd$id), as.character(dd$group), function(z) length(unique(z)))
    add("Individuals per group", sprintf("median %s; %.0f%% of groups have a single individual", format_num(stats::median(per_group)),
                                         100 * mean(per_group == 1)), mean(per_group == 1) < 0.5)
  }
  for (rt in random_terms) if (rt %in% names(dd)) level_check(rt, dd[[rt]])
  if (length(rows)) do.call(rbind, rows) else data.frame()
}



# Instant screen of the error family from a fitted model, without refitting (0.22.6). Returns short findings for the
# sensitivity banner: a count or binary trait fitted as Gaussian, right-skewed residuals of a positive trait fitted as
# Gaussian, overdispersion under a Poisson or binomial-with-trials fit, and more zeros than a count fit predicts.
family_screen <- function(res, m) {
  fit <- res$fits[[m]]
  if (is.null(fit)) return(character(0))
  fam <- res$family %||% "gaussian"
  y <- res$data$trait[is.finite(res$data$trait)]
  out <- character(0)
  if (identical(fam, "gaussian")) {
    sug <- tryCatch(suggest_family(res$data$trait, res$data$age)$family, error = function(e) "gaussian")
    if (!identical(sug, "gaussian")) {
      out <- c(out, sprintf("the trait looks %s but is fitted as Gaussian", if (sug %in% BINOMIAL_FAMILIES) "binary or binomial" else "like counts"))
    } else if (length(y) && all(y > 0)) {
      rr <- tryCatch(as.numeric(stats::residuals(fit)), error = function(e) NULL)
      rr <- rr[is.finite(rr)]
      if (length(rr) > 20) {
        sk <- mean((rr - mean(rr))^3) / stats::sd(rr)^3
        if (is.finite(sk) && sk > 1)
          out <- c(out, sprintf("residuals are right-skewed (skewness %s) for a positive trait: try Gamma or lognormal", format_num(sk)))
      }
    }
  }
  trials <- ".trials" %in% names(res$data) && any(res$data$.trials > 1, na.rm = TRUE)
  if (identical(fam, "poisson") || (identical(fam, "binomial") && trials)) {
    pr <- tryCatch(stats::residuals(fit, type = "pearson"), error = function(e) NULL)
    dfr <- tryCatch(stats::df.residual(fit), error = function(e) NA_real_)
    if (length(pr) && is.finite(dfr) && dfr > 10) {
      ratio <- sum(pr^2, na.rm = TRUE) / dfr
      if (is.finite(ratio) && ratio > 1.5)
        out <- c(out, sprintf("overdispersion (Pearson \u03c7\u00b2/df = %s): try %s", format_num(ratio),
                              if (identical(fam, "poisson")) "a negative binomial" else "the beta-binomial"))
    }
  }
  if (fam %in% c("poisson", "nbinom1", "nbinom2")) {
    obs <- tryCatch(as.numeric(stats::model.response(stats::model.frame(fit))), error = function(e) NULL)
    sims <- tryCatch(as.matrix(stats::simulate(fit, nsim = 50, seed = 1)), error = function(e) NULL)
    if (length(obs) && is.matrix(sims) && nrow(sims) == length(obs)) {
      z_obs <- mean(obs == 0)
      z_sim <- colMeans(sims == 0)
      if (z_obs > stats::quantile(z_sim, 0.975, names = FALSE) && z_obs - mean(z_sim) > 0.02)
        out <- c(out, sprintf("%.0f%% zeros against %.0f%% predicted: try a zero-inflated family", 100 * z_obs, 100 * mean(z_sim)))
    }
  }
  out
}
