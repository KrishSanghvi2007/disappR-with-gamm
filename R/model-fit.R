# disappR engine - Model fitting: families, data support for random effects, single-model fits, fit validity and the model suites.
# Engine code shared by the app and the R interface; changes that alter results must pass the golden tests.

family_label <- function(f) {
  switch(f, gaussian = "Gaussian (lme4)", gamma = "Gamma (log link)", lognormal = "lognormal (log link)",
         beta = "beta (logit link)", poisson = "Poisson", nbinom2 = "negative binomial (nbinom2)",
         nbinom1 = "negative binomial (nbinom1)", zip = "zero-inflated Poisson",
         zinb = "zero-inflated negative binomial (nbinom2)", zinb1 = "zero-inflated negative binomial (nbinom1)",
         binomial = "binomial (logit link)", betabinomial = "beta-binomial (logit link)", f)
}

# Data support for individual ageing slopes (A3 diagnostics and random-slope advice).
individual_data_support <- function(dat) {
  d <- dat[is.finite(dat$trait) & is.finite(dat$age), , drop = FALSE]
  if (!nrow(d)) return(NULL)
  sp <- split(d$age, d$id)
  n_obs <- vapply(sp, length, integer(1))
  n_ages <- vapply(sp, function(a) length(unique(a)), integer(1))
  span <- vapply(sp, function(a) diff(range(a)), numeric(1))
  q <- stats::quantile(n_obs, c(0.25, 0.75), names = FALSE)
  list(individuals = length(sp), observations = nrow(d), median_obs = stats::median(n_obs), iqr_low = q[[1]], iqr_high = q[[2]],
       pct_2 = 100 * mean(n_ages >= 2), pct_3 = 100 * mean(n_ages >= 3), pct_4 = 100 * mean(n_ages >= 4),
       n_3 = sum(n_ages >= 3), distinct_ages = length(unique(d$age)), median_span = stats::median(span))
}

# Heuristic advice: slope variance is separable from residual variance only through individuals with
# >= 3 distinct ages, and lme4 requires more observations than individual-level random effects.
# Warn when the data cannot support individual slopes at all.
data_support_warning <- function(sup) {
  if (is.null(sup)) return("")
  msgs <- character(0)
  # These comparisons are rules of thumb for how much information the data carry about among-individual
  # differences in ageing rate. They are not identifiability thresholds: nothing is blocked or changed by
  # them, models below them can still be fitted, and models above them are not thereby reliable.
  if (isTRUE(sup$individuals < 30)) msgs <- c(msgs, sprintf("%s individuals", format(sup$individuals, big.mark = ",")))
  if (isTRUE(sup$n_3 < 20)) msgs <- c(msgs, sprintf("%s individuals with 3 or more distinct ages", format(sup$n_3, big.mark = ",")))
  if (isTRUE(sup$median_obs < 3)) msgs <- c(msgs, sprintf("a median of %s records per individual", format_num(sup$median_obs)))
  if (isTRUE(sup$distinct_ages < 4)) msgs <- c(msgs, sprintf("%s distinct ages in the data", format(sup$distinct_ages, big.mark = ",")))
  if (!length(msgs)) return("")
  paste0("Caution: these data carry little information about individual-level slopes (", paste(msgs, collapse = "; "),
         "). Random slopes, if fitted, will be imprecise here.")
}

random_slope_advice <- function(sup) {
  if (is.null(sup)) return(list(level = "none", recommended = "none", text = "No trait data."))
  n_re <- 2 * sup$individuals
  facts <- sprintf("%d of %d individuals (%.0f%%) have \u2265 3 distinct ages; %s observations for %s individual-level random effects; %d distinct ages; median age span %s.",
                   sup$n_3, sup$individuals, sup$pct_3, format(sup$observations, big.mark = ","), format(n_re, big.mark = ","),
                   sup$distinct_ages, format_num(sup$median_span))
  if (sup$distinct_ages < 3 || sup$n_3 < 10 || sup$observations <= n_re) {
    list(level = "none", recommended = "none", facts = facts,
         text = paste("Random slopes are not supported by these data:", facts, "Use a random intercept."))
  } else if (sup$pct_3 >= 50 && sup$n_3 >= 30) {
    list(level = "good", recommended = "correlated", facts = facts,
         text = paste("Random slopes are supported:", facts, "A correlated intercept and slope can be estimated; if it has boundary or convergence notes, compare with the uncorrelated version."))
  } else {
    list(level = "limited", recommended = "uncorrelated", facts = facts,
         text = paste("Support for random slopes is limited:", facts, "The uncorrelated structure (no intercept\u2013slope correlation) is the more stable choice."))
  }
}

# The random structure the Modelling tab starts with for simulated and uploaded data (0.24.8): random slopes only when
# the data support them well (as judged by random_slope_advice), a slope for every age term only when most
# individuals are also recorded at four or more ages, and otherwise a random intercept. Bundled examples keep the
# structure of their published analysis.
default_random_structure <- function(dat) {
  sup <- tryCatch(individual_data_support(dat), error = function(e) NULL)
  if (is.null(sup)) return("none")
  adv <- random_slope_advice(sup)
  if (!identical(adv$level, "good")) return("none")
  n4 <- (sup$pct_4 %||% 0) / 100 * (sup$individuals %||% 0)
  if (isTRUE(sup$pct_4 >= 50) && isTRUE(n4 >= 30) && isTRUE(sup$median_obs >= 4)) "correlated_all" else "correlated"
}

MODEL_FAMILIES <- c(
  "Gaussian (lme4::lmer)" = "gaussian",
  "Gamma \u2014 positive continuous, log link (glmmTMB)" = "gamma",
  "Lognormal \u2014 positive continuous, log link (glmmTMB)" = "lognormal",
  "Beta \u2014 continuous proportions strictly between 0 and 1 (glmmTMB)" = "beta",
  "Poisson (glmmTMB)" = "poisson",
  "Negative binomial, quadratic variance (nbinom2)" = "nbinom2",
  "Negative binomial, linear variance (nbinom1)" = "nbinom1",
  "Zero-inflated Poisson" = "zip",
  "Zero-inflated negative binomial (nbinom2)" = "zinb",
  "Zero-inflated negative binomial (nbinom1)" = "zinb1",
  "Binomial \u2014 binary 0/1, or a proportion with a trials column" = "binomial",
  "Beta-binomial \u2014 overdispersed proportions (glmmTMB)" = "betabinomial"
)

COUNT_FAMILIES <- c("poisson", "nbinom2", "nbinom1", "zip", "zinb", "zinb1")

# Families with a zero-inflation component (glmmTMB ziformula).
ZI_FAMILIES <- c("zip", "zinb", "zinb1")

# Binomial families: the response is a binary outcome (0/1) or a proportion of successes, in which
# case the number of trials must be mapped on the Data tab and is passed as prior weights.
BINOMIAL_FAMILIES <- c("binomial", "betabinomial")

# Continuous families other than the Gaussian (0.22.0), fitted with glmmTMB: Gamma and lognormal for strictly positive
# traits (log link), beta for continuous proportions strictly between 0 and 1 (logit link).
CONTINUOUS_FAMILIES <- c("gamma", "lognormal", "beta")

# Families whose likelihood is a density for a continuous trait. AICs are comparable within this class (the same
# response, the same rows), and within the discrete class (counts, binomial), but never between the two.
DENSITY_FAMILIES <- c("gaussian", CONTINUOUS_FAMILIES)
family_density_class <- function(f) if (isTRUE(f %in% DENSITY_FAMILIES)) "continuous" else "discrete"

family_link <- function(f) {
  if (isTRUE(f %in% "gaussian")) "identity" else if (isTRUE(f %in% c(BINOMIAL_FAMILIES, "beta"))) "logit" else "log"
}

# Families with a dispersion (or residual variance) parameter that can be modelled with glmmTMB's dispformula.
DISPERSION_FAMILIES <- c("gaussian", "gamma", "lognormal", "beta", "nbinom2", "nbinom1", "zinb", "zinb1", "betabinomial")

# How the residual variance (Gaussian) or dispersion (other families) may change with age (0.22.0).
DISP_OPTIONS <- c("Constant (default)" = "constant",
                  "Changes with age (log-linear)" = "age",
                  "Changes with every ageing term" = "age_terms")
normalise_disp <- function(x) {
  x <- as.character(x %||% "constant")[1]
  if (is.na(x) || !x %in% DISP_OPTIONS) "constant" else x
}
disp_formula_string <- function(disp, b = "f1") {
  switch(normalise_disp(disp), age = "~ f1", age_terms = paste("~", paste(b, collapse = " + ")), "~1")
}
# The glmmTMB family call written into exported code and the equation panels.
family_r_call <- function(f, pkg = TRUE) {
  p <- if (isTRUE(pkg)) "glmmTMB::" else ""
  switch(f, gaussian = "gaussian()", gamma = "Gamma(link = \"log\")", lognormal = paste0(p, "lognormal(link = \"log\")"),
         beta = paste0(p, "beta_family(link = \"logit\")"), poisson = "poisson()", zip = "poisson()",
         nbinom1 = paste0(p, "nbinom1()"), zinb1 = paste0(p, "nbinom1()"), binomial = "binomial()",
         betabinomial = paste0(p, "betabinomial()"), paste0(p, "nbinom2()"))
}

disp_text <- function(disp) {
  switch(normalise_disp(disp), age = "variance changing log-linearly with age",
         age_terms = "variance changing with every ageing term",
         "constant variance")
}

# ok: the model can be fitted at all; default: selected by default (Model 6 is left
# unselected when LS is missing for some individuals, because the common-row AIC
# comparison would otherwise drop those individuals from every model).
model_availability <- function(dat, meta) {
  im <- individual_metrics(dat)
  im <- im[im$n_trait > 0, , drop = FALSE]
  ok <- stats::setNames(rep(TRUE, length(MODEL_IDS)), MODEL_IDS)
  why <- stats::setNames(rep("", length(MODEL_IDS)), MODEL_IDS)
  if (!isTRUE(stats::sd(im$alr, na.rm = TRUE) > 0)) {
    ok[c("M2", "M4", "M7", "M8")] <- FALSE
    why[c("M2", "M4", "M7", "M8")] <- "ALR does not vary among individuals."
  }
  if (!isTRUE(meta$has_life) || isTRUE(meta$life_auto)) {
    ok["M6"] <- FALSE
    why["M6"] <- "Needs a known lifespan column (automatic LS = last record is not a positive control)."
  } else if (!isTRUE(stats::sd(im$lifespan, na.rm = TRUE) > 0)) {
    ok["M6"] <- FALSE
    why["M6"] <- "LS does not vary among individuals."
  }
  afr <- ifelse(is.finite(im$entry), im$entry, im$first_recorded)
  if (!isTRUE(stats::sd(im$alr, na.rm = TRUE) > 0)) {
    ok[c("M9", "M10")] <- FALSE
    why[c("M9", "M10")] <- "ALR does not vary among individuals."
  }
  if (!isTRUE(stats::sd(afr, na.rm = TRUE) > 0)) {
    ok[c("M7", "M8", "M9", "M10")] <- FALSE
    why[c("M7", "M8", "M9", "M10")] <- "AFR does not vary among individuals."
  }
  default <- ok
  afr_f <- afr[is.finite(afr)]
  if (ok[["M7"]] && length(afr_f)) {
    mode_afr <- names_num(sort(table(afr_f), decreasing = TRUE))[[1]]
    n_diff <- sum(abs(afr_f - mode_afr) > 1e-8)
    if (n_diff < max(5, 0.05 * length(afr_f))) {
      default[c("M7", "M8", "M9", "M10")] <- FALSE
      why[c("M7", "M8", "M9", "M10")] <- sprintf("Only %d individuals have an AFR different from the most common value: Models 7-10 are left unselected.", n_diff)
    }
  }
  if (ok[["M6"]] && any(!is.finite(im$lifespan))) {
    default[["M6"]] <- FALSE
    why[["M6"]] <- sprintf("LS missing for %d individuals: selecting Model 6 drops them from every model.", sum(!is.finite(im$lifespan)))
  }
  list(ok = ok, default = default, why = why)
}

fit_one_model <- function(formula_str, random_str, data, family = "gaussian", zi_str = "~1", disp_str = "~1") {
  ff <- stats::as.formula(paste("trait ~", formula_str, "+", random_str))
  msgs <- character(0)
  fit <- tryCatch(
    withCallingHandlers({
      disp_on <- !identical(gsub(" ", "", disp_str %||% "~1"), "~1") && family %in% DISPERSION_FAMILIES
      disp_f <- if (disp_on) stats::as.formula(disp_str) else ~1
      if (identical(family, "gaussian") && !disp_on) {
        ctrl <- lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
        if (HAS_LMERTEST) lmerTest::lmer(ff, data = data, REML = FALSE, control = ctrl)
        else lme4::lmer(ff, data = data, REML = FALSE, control = ctrl)
      } else {
        fam <- switch(family, gaussian = stats::gaussian(), gamma = stats::Gamma(link = "log"), lognormal = glmmTMB::lognormal(link = "log"),
                      beta = glmmTMB::beta_family(link = "logit"),
                      poisson = stats::poisson(), zip = stats::poisson(), nbinom1 = glmmTMB::nbinom1(),
                      zinb1 = glmmTMB::nbinom1(), binomial = stats::binomial(), betabinomial = glmmTMB::betabinomial(),
                      glmmTMB::nbinom2())
        zi <- if (family %in% ZI_FAMILIES) stats::as.formula(zi_str) else ~0
        ctrl <- tryCatch(glmmTMB::glmmTMBControl(rank_check = "adjust"), error = function(e) glmmTMB::glmmTMBControl())
        # Binomial families: proportions are weighted by the number of trials (binary data have one trial).
        # The trials column exists (all NA) even when no trials column is mapped, so weights are used only when every
        # analysed row has a positive number of trials; otherwise binary 0/1 data would lose every row.
        trials <- if (family %in% BINOMIAL_FAMILIES && ".trials" %in% names(data)) as.numeric(data$.trials) else NULL
        wts <- if (length(trials) && all(is.finite(trials) & trials > 0)) trials else NULL
        if (is.null(wts)) {
          glmmTMB::glmmTMB(ff, data = data, family = fam, ziformula = zi, dispformula = disp_f, REML = FALSE, control = ctrl)
        } else {
          # a column, not a vector: with a vector, predict() on new data fails with "variable lengths differ
          # (found for '(weights)')", because the weights of the fitting data are re-used for the new rows
          data$.wts <- wts
          glmmTMB::glmmTMB(ff, data = data, family = fam, ziformula = zi, dispformula = disp_f, REML = FALSE, control = ctrl, weights = .wts)
        }
      }
    }, warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }, message = function(m) {
      txt <- trimws(conditionMessage(m))
      if (grepl("rank deficient|dropping", txt)) msgs <<- c(msgs, txt)
      invokeRestart("muffleMessage")
    }),
    error = function(e) {
      em <- conditionMessage(e)
      hint <- if (grepl("number of observations", em) && grepl("random effects", em)) {
        " (too few records per individual for this random-effect structure: choose a random intercept or use individuals with more records)"
      } else if (grepl("contrasts", em)) {
        " (a factor covariate has only one level in the analysed rows)"
      } else ""
      msgs <<- c(msgs, paste0("Error: ", em, hint))
      NULL
    })
  notes <- unique(msgs)
  if (!is.null(fit)) {
    if (inherits(fit, "merMod")) {
      if (isTRUE(tryCatch(lme4::isSingular(fit), error = function(e) FALSE))) notes <- c(notes, "singular fit (a random-effect variance is ~0)")
    } else if (inherits(fit, "glmmTMB")) {
      if (isFALSE(fit$sdr$pdHess)) notes <- c(notes, "non-positive-definite Hessian")
      small <- tryCatch({
        vc <- summary(fit)$varcor$cond
        sds <- unlist(lapply(vc, function(m) attr(m, "stddev")))
        any(!is.finite(sds) | sds < 1e-3)
      }, error = function(e) FALSE)
      if (isTRUE(small)) notes <- c(notes, "random-effect SD \u2248 0 (boundary)")
    }
    if (!is.finite(tryCatch(stats::AIC(fit), error = function(e) NA_real_))) {
      notes <- c(notes, "AIC not finite")
    }
  }
  notes <- unique(notes)
  list(fit = fit, notes = notes, validity = classify_fit(fit, notes),
       diagnostics = tryCatch(fit_diagnostics(fit, notes), error = function(e) NULL))
}

# Structured diagnostics read from the fitted object itself (not from warning text): optimiser convergence, singular
# (boundary) random effects, a positive-definite Hessian, dropped rank-deficient columns and a finite AIC. NA means the
# quantity is not available for that kind of fit. Reported alongside the Valid/Caution/Failed status, which is unchanged.
fit_diagnostics <- function(fit, notes = character(0)) {
  out <- list(fitted = !is.null(fit), converged = NA, singular = NA, hessian_ok = NA, rank_deficient = NA,
              dropped = character(0), aic_finite = NA, warnings = length(notes))
  if (is.null(fit)) return(out)
  if (inherits(fit, "merMod")) {
    conv <- tryCatch(fit@optinfo$conv, error = function(e) NULL)
    msgs <- tryCatch(as.character(conv$lme4$messages), error = function(e) character(0))
    out$converged <- isTRUE(identical(as.integer(conv$opt %||% 0L), 0L)) && !length(msgs)
    out$singular <- isTRUE(tryCatch(lme4::isSingular(fit), error = function(e) FALSE))
    out$hessian_ok <- !any(grepl("Hessian|unidentifiable|eigenvalue", msgs, ignore.case = TRUE))
    dropped <- tryCatch(attr(lme4::getME(fit, "X"), "col.dropped"), error = function(e) NULL)
    out$dropped <- if (length(dropped)) names(dropped) %||% as.character(dropped) else character(0)
    out$rank_deficient <- length(out$dropped) > 0
  } else if (inherits(fit, "glmmTMB")) {
    out$converged <- isTRUE(tryCatch(fit$fit$convergence == 0, error = function(e) FALSE))
    out$hessian_ok <- isTRUE(tryCatch(fit$sdr$pdHess, error = function(e) FALSE))
    out$singular <- isTRUE(tryCatch({
      vc <- summary(fit)$varcor$cond
      sds <- unlist(lapply(vc, function(v) attr(v, "stddev")))
      any(!is.finite(sds) | sds < 1e-3)
    }, error = function(e) FALSE))
    out$rank_deficient <- any(grepl("rank deficient|dropping", notes))
  }
  out$aic_finite <- is.finite(tryCatch(stats::AIC(fit), error = function(e) NA_real_))
  out
}

# One row per model: the structured diagnostics, the status they imply on their own, and the reported status.
diagnostics_table <- function(diags, validity) {
  if (!length(diags)) return(data.frame())
  yn <- function(x) if (is.na(x)) "\u2013" else if (isTRUE(x)) "yes" else "no"
  rows <- lapply(names(diags), function(m) {
    g <- diags[[m]]
    if (is.null(g)) return(NULL)
    implied <- if (!isTRUE(g$fitted) || isFALSE(g$hessian_ok) || isFALSE(g$aic_finite)) "Failed"
               else if (isTRUE(g$singular) || isFALSE(g$converged) || isTRUE(g$rank_deficient)) "Caution" else "Valid"
    data.frame(Model = model_label(m), Converged = yn(g$converged), Singular = yn(g$singular), `Hessian OK` = yn(g$hessian_ok),
               `Rank deficient` = yn(g$rank_deficient), `Warnings captured` = g$warnings, `Status from diagnostics` = implied,
               `Reported status` = validity[[m]] %||% "", check.names = FALSE, stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

# Valid: no warnings or notes. Caution: boundary (singular) fits, convergence-gradient warnings, rank
# deficiency or other notes. Failed: no fit, a non-positive-definite or singular Hessian, or a non-finite AIC;
# such fits are excluded from automated "lowest AIC" interpretation unless the user overrides it.
classify_fit <- function(fit, notes = character(0)) {
  if (is.null(fit)) return("Failed")
  invalid <- "Hessian is numerically singular|non-positive-definite Hessian|degenerate +Hessian|unable to evaluate scaled gradient|Hessian is not positive definite|invalid Hessian|AIC not finite"
  if (any(grepl(invalid, notes, ignore.case = TRUE))) return("Failed")
  if (length(notes)) "Caution" else "Valid"
}

fit_nonlinear_suite <- function(dat, meta, models = MODEL_IDS, random_slope = FALSE, include_invalid = FALSE,
                                extra = NULL, progress = NULL) {
  fail <- function(msg, status = list()) list(ok = FALSE, message = msg, fits = list(), aic = data.frame(),
                                              coefficients = data.frame(), status = status, lrt = data.frame())
  if (!requireNamespace("nlme", quietly = TRUE)) return(fail("The nlme package is needed for the non-linear exponential function."))
  covars <- meta$covars %||% character(0)
  cov_age <- intersect(meta$cov_age %||% character(0), covars)
  cov_pairs <- (meta$cov_pairs %||% character(0))[vapply(strsplit(meta$cov_pairs %||% character(0), ":", fixed = TRUE),
                                                          function(p) all(p %in% covars), logical(1))]
  extra <- clean_extra_terms(extra, covars)
  prep <- prepare_model_data(dat, "Linear", covars, TRUE, meta$has_group, character(0), has_group2 = isTRUE(meta$has_group2))
  if (is.null(prep)) return(fail("No usable trait data."))
  d <- prep$data
  status <- list()
  av <- model_availability(dat, meta)
  for (m in models) {
    if (m %in% c("M3", "M5")) {
      status[[m]] <- "Unavailable: the mean-age centring models are not defined for the non-linear exponential function."
    } else if (m %in% names(av$ok) && !av$ok[[m]]) {
      status[[m]] <- paste("Unavailable:", av$why[[m]])
    }
  }
  models <- setdiff(intersect(models, names(NONLINEAR_PARTS)), names(status))
  if (!length(models)) return(fail("None of the selected models can be fitted with the non-linear exponential function.", status = status))
  parts <- NONLINEAR_PARTS[models]
  if (length(extra)) {
    for (m in intersect(names(extra), models)) parts[[m]] <- nonlinear_extra_parts(parts[[m]], extra[[m]], covars)
  }
  for (m in models) {
    parts[[m]]$a <- union(c(covars, cov_pairs), parts[[m]]$a)
    parts[[m]]$b <- union(cov_age, parts[[m]]$b)
  }
  # interaction terms such as cv_a:cv_b are not columns: use their components
  vars <- unique(c(unlist(strsplit(unlist(lapply(parts, unlist)), ":", fixed = TRUE)), "f1", "id", if (isTRUE(meta$has_group)) "group", if (isTRUE(meta$has_group2)) "group2"))
  ok <- rep(TRUE, nrow(d))
  drop_by <- character(0)
  for (v in vars) {
    x <- d[[v]]
    bad <- if (is.null(x)) rep(TRUE, nrow(d)) else if (is.numeric(x)) !is.finite(x) else is.na(x)
    if (any(bad & ok)) drop_by <- c(drop_by, sprintf("%s: %d", display_term(v), sum(bad & ok)))
    ok <- ok & !bad
  }
  dd <- droplevels(d[ok, , drop = FALSE])
  if (nrow(dd) < 20 || length(unique(dd$id)) < 6) {
    return(fail("Too few complete observations shared by the selected models (need >= 20 rows from >= 6 individuals).", status = status))
  }
  if (length(unique(dd$age)) < 3) return(fail("The non-linear exponential function needs at least 3 distinct ages.", status = status))
  novar <- covars[!vapply(covars, function(cv) { x <- dd[[cv]]; if (is.numeric(x)) isTRUE(stats::sd(x) > 0) else length(unique(x)) >= 2 }, logical(1))]
  drop_term <- function(x) x[!vapply(strsplit(x, ":", fixed = TRUE), function(p) any(p %in% novar), logical(1))]
  for (m in models) {
    parts[[m]]$a <- drop_term(parts[[m]]$a)
    parts[[m]]$b <- drop_term(parts[[m]]$b)
  }
  if (length(novar)) drop_by <- c(drop_by, sprintf("covariate %s omitted (no variation in the analysed rows)", paste(unname(meta$cov_labels[novar]), collapse = ", ")))
  if (length(meta$random_terms)) drop_by <- c(drop_by, "additional crossed random intercepts are not used by the non-linear exponential models")
  rs_request <- normalise_slope(random_slope)
  advice <- random_slope_advice(individual_data_support(dd))
  rs <- if (identical(rs_request, "auto")) advice$recommended else rs_request
  random_note <- if (identical(rs_request, "auto")) paste0("Automatic random effects: ", slope_text(rs), ". ", advice$text) else ""
  # the non-linear model has two parameters, so a slope on "every age term" is the same as a slope on its rate
  id_pd <- switch(rs, correlated = , correlated_all = nlme::pdSymm(a + b ~ 1),
                  uncorrelated = , uncorrelated_all = nlme::pdDiag(a + b ~ 1), nlme::pdSymm(a ~ 1))
  rand <- if (isTRUE(meta$has_group2)) {
    list(group2 = nlme::pdSymm(a ~ 1), group = nlme::pdSymm(a ~ 1), id = id_pd)
  } else if (isTRUE(meta$has_group)) list(group = nlme::pdSymm(a ~ 1), id = id_pd) else id_pd
  grp <- if (isTRUE(meta$has_group2)) ~ group2 / group / id else if (isTRUE(meta$has_group)) ~ group / id else ~ id
  rstr <- paste0(if (isTRUE(meta$has_group2)) "random level a | group2 + " else "", if (isTRUE(meta$has_group)) "random level a | group + " else "",
                 switch(rs, correlated = , correlated_all = "correlated random level a and rate b | id",
                        uncorrelated = , uncorrelated_all = "uncorrelated random level a and rate b | id", "random level a | id"))
  pos <- dd$trait > 0
  st0 <- if (mean(pos) > 0.8 && sum(pos) >= 10) {
    c0 <- stats::coef(stats::lm(log(dd$trait[pos]) ~ dd$f1[pos]))
    c(exp(c0[[1]]), c0[[2]])
  } else c(mean(dd$trait), 0)
  rhs <- function(x) if (length(x)) paste(x, collapse = " + ") else "1"
  ncols <- function(x) ncol(stats::model.matrix(stats::as.formula(paste("~", rhs(x))), dd))
  fits <- list(); aic_rows <- list(); coef_rows <- list(); vc_rows <- list(); validity <- list()
  pseudo <- list(); shown <- list()
  for (i in seq_along(models)) {
    m <- models[[i]]
    if (is.function(progress)) progress(i, length(models), model_label(m))
    pa <- parts[[m]]$a
    pb <- parts[[m]]$b
    pseudo[[m]] <- paste(make.names(c(paste0("a_", c("int", pa)), paste0("b_", c("int", pb)))), collapse = " + ")
    shown[[m]] <- paste0("a\u00b7exp(b\u00b7z_age); a ~ ", rhs(pa), "; b ~ ", rhs(pb))
    msgs <- character(0)
    fx <- list(stats::as.formula(paste("a ~", rhs(pa))), stats::as.formula(paste("b ~", rhs(pb))))
    start <- c(st0[1], rep(0, ncols(pa) - 1), st0[2], rep(0, ncols(pb) - 1))
    fit <- tryCatch(withCallingHandlers(
      nlme::nlme(trait ~ a * exp(b * f1), data = dd, fixed = fx, random = rand, groups = grp, start = start, method = "ML",
                 control = nlme::nlmeControl(maxIter = 100, pnlsMaxIter = 20, msMaxIter = 200, returnObject = TRUE)),
      warning = function(w) {
        msgs <<- c(msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
      }), error = function(e) {
        msgs <<- c(msgs, paste("Error:", conditionMessage(e)))
        NULL
      })
    if (!is.null(fit)) {
      # predict.nlme() re-evaluates the call's fixed, random, groups and start arguments; as local names they are
      # "object 'fx' not found" outside this function, so the values themselves go into the call
      fit$call$fixed <- fx
      fit$call$random <- rand
      fit$call$groups <- grp
      fit$call$start <- start
      if (is.character(fit$apVar)) msgs <- c(msgs, "non-positive-definite approximate variance-covariance matrix (invalid Hessian)")
      if (!is.finite(tryCatch(stats::AIC(fit), error = function(e) NA_real_))) msgs <- c(msgs, "AIC not finite")
    }
    msgs <- unique(msgs)
    validity[[m]] <- classify_fit(fit, msgs)
    if (is.null(fit)) {
      status[[m]] <- paste("Failed:", paste(msgs, collapse = " | "))
      next
    }
    fits[[m]] <- fit
    status[[m]] <- if (length(msgs)) paste0(validity[[m]], ": ", paste(msgs, collapse = " | ")) else "Valid"
    ll <- stats::logLik(fit)
    r2m <- r2_pair(fit)
    aic_rows[[m]] <- data.frame(Model = model_label(m), AIC = tryCatch(stats::AIC(fit), error = function(e) NA_real_), df = attr(ll, "df"),
                                logLik = as.numeric(ll), N = nrow(dd), Fit = validity[[m]],
                                R2_marginal = r2m[["marginal"]], R2_conditional = r2m[["conditional"]],
                                stringsAsFactors = FALSE)
    ct <- coef_table(fit, model_label(m))
    if (!is.null(ct)) coef_rows[[m]] <- ct
    vc <- variance_components(fit, model_label(m))
    if (!is.null(vc)) vc_rows[[m]] <- vc
  }
  aic <- if (length(aic_rows)) do.call(rbind, aic_rows) else data.frame()
  n_excluded <- 0L
  if (nrow(aic)) {
    aic$Eligible <- is.finite(aic$AIC) & (aic$Fit != "Failed" | isTRUE(include_invalid))
    n_excluded <- sum(!aic$Eligible)
    aic$Delta_AIC <- NA_real_
    aic$Weight <- NA_real_
    if (any(aic$Eligible)) {
      best <- min(aic$AIC[aic$Eligible])
      aic$Delta_AIC[aic$Eligible] <- aic$AIC[aic$Eligible] - best
      w <- exp(-aic$Delta_AIC[aic$Eligible] / 2)
      aic$Weight[aic$Eligible] <- w / sum(w)
    }
    aic <- aic[order(!aic$Eligible, aic$AIC), , drop = FALSE]
    rownames(aic) <- NULL
  }
  n_eligible <- if (nrow(aic)) sum(aic$Eligible) else 0L
  res <- list(ok = n_eligible > 0,
       message = if (n_eligible > 0) "Non-linear exponential models fitted by maximum likelihood (nlme) on the same complete-case rows." else
         if (nrow(aic)) "Every non-linear fit has an invalid Hessian: simplify the random effects or tick 'Include fits with invalid Hessians' to inspect them." else
         "No non-linear model could be fitted (see the fitting status); non-linear fits need good starting values and positive trait values.",
       fits = fits, aic = aic, coefficients = if (length(coef_rows)) do.call(rbind, coef_rows) else data.frame(),
       status = status, data = dd, n_dropped = sum(!ok), n_ids_dropped = length(unique(d$id)) - length(unique(dd$id)), drop_by = drop_by, formulas = shown, random = rstr,
       basis = "f1", age_params = prep$age_params, proxy_params = prep$proxy_params, family = "gaussian", zi = "~1",
       covars = setdiff(covars, novar), random_terms = character(0),
       varcomp = if (length(vc_rows)) do.call(rbind, vc_rows) else data.frame(),
       age_function = A3_NONLINEAR, random_slope = rs, standardise = TRUE, nonlinear = TRUE, parts = parts,
       validity = validity, n_excluded = n_excluded, include_invalid = isTRUE(include_invalid),
       random_request = rs_request, random_structure = rs, random_note = random_note, random_advice = advice,
       among = "linear", cov_age = setdiff(cov_age, novar), extra = extra,
       cov_pairs = cov_pairs[!vapply(strsplit(cov_pairs, ":", fixed = TRUE), function(p) any(p %in% novar), logical(1))],
       lrt = lrt_table(fits, validity, include_invalid, pseudo))
  res$provenance <- tryCatch(analysis_provenance(res, meta), error = function(e) NULL)
  res
}

# Nakagawa R2: variance explained by the fixed effects alone (marginal) and by fixed plus random (conditional).
# Returned as a pair so that a failure in one model never stops the comparison table being built.
r2_pair <- function(fit) {
  out <- c(marginal = NA_real_, conditional = NA_real_)
  v <- tryCatch(suppressWarnings(performance::r2(fit)), error = function(e) NULL)
  if (is.null(v)) return(out)
  u <- suppressWarnings(as.numeric(unlist(v)))
  nm <- names(unlist(v))
  if (!length(u) || length(nm) != length(u)) return(out)
  m <- grep("marginal", nm, ignore.case = TRUE)
  cc <- grep("conditional", nm, ignore.case = TRUE)
  if (length(m)) out[["marginal"]] <- u[[m[[1]]]]
  if (length(cc)) out[["conditional"]] <- u[[cc[[1]]]]
  if (!length(m) && !length(cc) && length(u)) out[["marginal"]] <- u[[1]]
  out
}

fit_model_suite <- function(dat, meta, models = MODEL_IDS, age_function = "Quadratic", family = "gaussian",
                            random_slope = FALSE, standardise = TRUE, zi_str = "~1", progress = NULL,
                            among = "linear", include_invalid = FALSE, extra = NULL, dry_run = FALSE, disp = "constant") {
  disp <- normalise_disp(disp)
  fail <- function(msg, status = list()) list(ok = FALSE, message = msg, fits = list(), aic = data.frame(),
                                              coefficients = data.frame(), status = status, lrt = data.frame())
  nonlinear_note <- NULL
  disp_note <- NULL
  if (identical(age_function, A3_NONLINEAR)) {
    if (identical(family, "gaussian")) {
      if (!identical(disp, "constant")) return(fail("Age-dependent variance is not available with the non-linear exponential: set it to constant or choose another ageing function."))
      if (isTRUE(dry_run)) return(fail("The row check before fitting is not available for the a\u00b7exp(b\u00b7age) model."))
      return(fit_nonlinear_suite(dat, meta, models, random_slope, include_invalid, extra, progress))
    }
    age_function <- "Linear"
    nonlinear_note <- "the non-linear exponential fit is for Gaussian traits; with the other families the Linear function was used (on the log link of the count families a\u00b7exp(b\u00b7age) is a straight line)"
  }
  if (!identical(family, "gaussian") && !HAS_GLMMTMB) {
    return(fail("The selected family needs the glmmTMB package: install.packages('glmmTMB'), or choose Gaussian."))
  }
  if (!identical(disp, "constant") && !family %in% DISPERSION_FAMILIES) {
    disp_note <- sprintf("age-dependent variance not used: the %s family has no dispersion parameter", family_label(family))
    disp <- "constant"
  }
  if (!identical(disp, "constant") && !HAS_GLMMTMB) {
    return(fail("Age-dependent variance needs glmmTMB: install.packages('glmmTMB'), or set it to constant."))
  }
  if (family %in% c("gamma", "lognormal")) {
    v <- dat$trait[is.finite(dat$trait)]
    if (any(v <= 0)) {
      return(fail(sprintf("The %s family needs positive values; %d are zero or negative. Use Gaussian, or a count family for counts.",
                          family_label(family), sum(v <= 0))))
    }
  }
  if (identical(family, "beta")) {
    v <- dat$trait[is.finite(dat$trait)]
    if (any(v <= 0 | v >= 1)) {
      return(fail(sprintf("The beta family needs values strictly between 0 and 1; %d are not. For successes out of trials, use binomial with a trials column; proportions including 0 or 1 can be rescaled as (y(n - 1) + 0.5)/n (Smithson and Verkuilen 2006).",
                          sum(v <= 0 | v >= 1))))
    }
  }
  if (family %in% COUNT_FAMILIES) {
    v <- dat$trait[is.finite(dat$trait)]
    if (any(v < 0) || any(abs(v - round(v)) > 1e-8)) {
      return(fail(paste("Count families need non-negative integer trait values.",
                        "")))
    }
  }
  if (family %in% BINOMIAL_FAMILIES) {
    v <- dat$trait[is.finite(dat$trait)]
    if (any(v < 0) || any(v > 1)) {
      return(fail("Binomial families need the trait to be 0/1 (binary) or a proportion between 0 and 1. For counts of successes, divide by the number of trials and choose that number under 'Weights' below the error family."))
    }
    binary <- all(v %in% c(0, 1))
    if (!binary && !isTRUE(meta$has_trials)) {
      return(fail("These are proportions, so the binomial families need the number of trials: choose the column with the number of trials (for example clutch size) under 'Weights' below the error family, or choose Gaussian."))
    }
    if (identical(family, "betabinomial") && binary && !isTRUE(meta$has_trials)) {
      return(fail("The beta-binomial family needs proportions with a trials column: with binary 0/1 data there is no extra-binomial variation to estimate, so use the binomial family."))
    }
  }
  covars <- meta$covars %||% character(0)
  rterms <- meta$random_terms %||% character(0)
  # Fewer than two AFR values: the appearance models cannot be told apart from Models 1-4.
  afr_levels <- {
    im_afr <- individual_metrics(dat)
    av_afr <- ifelse(is.finite(im_afr$entry), im_afr$entry, im_afr$first_recorded)
    length(unique(av_afr[is.finite(av_afr)]))
  }
  prep <- prepare_model_data(dat, age_function, covars, standardise, meta$has_group, rterms, has_group2 = isTRUE(meta$has_group2))
  if (is.null(prep)) return(fail("No usable trait data."))
  d <- prep$data
  b <- prep$basis
  cov_age <- intersect(meta$cov_age %||% character(0), covars)
  cov_pairs <- meta$cov_pairs %||% character(0)
  extra <- clean_extra_terms(extra, covars)
  fs <- apply_extra_terms(model_formula_strings(b, covars, among, cov_age, cov_pairs), b, extra, among)
  models <- intersect(models, names(fs))
  status <- list()
  av <- model_availability(dat, meta)
  for (m in models) {
    if (!av$ok[[m]]) status[[m]] <- paste("Unavailable:", av$why[[m]])
  }
  models <- setdiff(models, names(status))
  if (!length(models)) return(fail("None of the selected models can be fitted with these data.", status = status))

  vars <- unique(unlist(lapply(fs[models], function(s) all.vars(stats::as.formula(paste("trait ~", s))))))
  vars <- c(vars, "id", if (isTRUE(meta$has_group)) "group", if (isTRUE(meta$has_group2)) "group2", rterms,
            if (family %in% BINOMIAL_FAMILIES && isTRUE(meta$has_trials)) ".trials")
  ok <- rep(TRUE, nrow(d))
  drop_by <- character(0)
  for (v in vars) {
    x <- d[[v]]
    bad <- if (is.null(x)) rep(TRUE, nrow(d)) else if (identical(v, ".trials")) (!is.finite(x) | x <= 0) else if (is.numeric(x)) !is.finite(x) else is.na(x)
    if (any(bad & ok)) drop_by <- c(drop_by, sprintf("%s: %d", if (identical(v, ".trials")) "rows without a positive number of trials" else display_term(sub("_raw$", "", v)), sum(bad & ok)))
    ok <- ok & !bad
  }
  dd <- droplevels(d[ok, , drop = FALSE])
  if (nrow(dd) < 20 || length(unique(dd$id)) < 6) {
    return(fail("Too few complete observations shared by the selected models (need >= 20 rows from >= 6 individuals).", status = status))
  }
  # Every model is fitted on the rows that all selected models can use, because AIC and the likelihood-ratio tests
  # only compare models fitted to the same data. What each model could have used on its own is recorded here, so the
  # app can say when the shared set costs a model rows (0.20.18).
  row_sets <- do.call(rbind, lapply(models, function(m) {
    vm <- c(unique(all.vars(stats::as.formula(paste("trait ~", fs[[m]])))), "id",
            if (isTRUE(meta$has_group)) "group", if (isTRUE(meta$has_group2)) "group2", rterms,
            if (family %in% BINOMIAL_FAMILIES && isTRUE(meta$has_trials)) ".trials")
    okm <- rep(TRUE, nrow(d))
    for (v in vm) {
      x <- d[[v]]
      bad <- if (is.null(x)) rep(TRUE, nrow(d)) else if (identical(v, ".trials")) (!is.finite(x) | x <= 0) else if (is.numeric(x)) !is.finite(x) else is.na(x)
      okm <- okm & !bad
    }
    data.frame(Model = model_label(m), Rows_usable = sum(okm), Individuals_usable = length(unique(d$id[okm])),
               stringsAsFactors = FALSE)
  }))
  row_sets$Rows_used <- sum(ok)
  row_sets$Individuals_used <- length(unique(d$id[ok]))
  n_age <- length(unique(dd$age))
  if (n_age < length(b) + 1) {
    return(fail(sprintf("The %s function needs at least %d distinct ages, but the analysed rows have %d. Choose a simpler ageing function.",
                        age_function, length(b) + 1, n_age), status = status))
  }
  # data that cannot support these models: say so before fitting
  if (length(unique(dd$trait)) < 2) {
    return(fail(sprintf("The trait takes a single value (%s) in all %d analysed rows, so there is no variation to model%s.",
                        format_num(dd$trait[[1]]), nrow(dd),
                        if (family %in% BINOMIAL_FAMILIES) " (a binary trait needs both outcomes)" else if (isTRUE(dd$trait[[1]] == 0)) " (all values are zero)" else ""),
                status = status))
  }
  if (!any(duplicated(dd$id))) {
    return(fail("Every individual has a single record in the analysed rows, so ageing within individuals and differences between individuals cannot be separated. These models need repeated records per individual.", status = status))
  }
  # covariates without variation in the analysed rows would stop every model (e.g. contrasts error)
  keep_cv <- covars[vapply(covars, function(cv) {
    x <- dd[[cv]]
    if (is.numeric(x)) isTRUE(stats::sd(x) > 0) else length(unique(x)) >= 2
  }, logical(1))]
  for (cv in setdiff(covars, keep_cv)) {
    drop_by <- c(drop_by, sprintf("covariate %s omitted (no variation in the analysed rows)", meta$cov_labels[[cv]]))
  }
  if (!identical(keep_cv, covars)) {
    covars <- keep_cv
    cov_age <- intersect(cov_age, covars)
    extra <- clean_extra_terms(extra, covars)
    fs <- apply_extra_terms(model_formula_strings(b, covars, among, cov_age, cov_pairs), b, extra, among)
  }
  if (afr_levels < 2) {
    drop_by <- c(drop_by, "AFR takes fewer than two values, so selective appearance cannot be estimated: Models 7-10 are unavailable (their AFR terms would be dropped as rank deficient, repeating Models 1-4).")
  }
  if (!is.null(nonlinear_note)) drop_by <- c(drop_by, nonlinear_note)
  if (!is.null(disp_note)) drop_by <- c(drop_by, disp_note)
  disp_str <- disp_formula_string(disp, b)
  # zero-inflation predictors must exist and vary in the analysed rows
  zi_vars <- if (family %in% ZI_FAMILIES) tryCatch(all.vars(stats::as.formula(zi_str)), error = function(e) character(0)) else character(0)
  zi_keep <- zi_vars[vapply(zi_vars, function(v) {
    x <- dd[[v]]
    !is.null(x) && (if (is.numeric(x)) isTRUE(stats::sd(x) > 0) else length(unique(x)) >= 2)
  }, logical(1))]
  if (!identical(zi_keep, zi_vars)) {
    drop_by <- c(drop_by, sprintf("zero-inflation term %s omitted (missing or without variation)", paste(display_term(setdiff(zi_vars, zi_keep)), collapse = ", ")))
  }
  if (family %in% ZI_FAMILIES) zi_str <- if (length(zi_keep)) paste("~", paste(zi_keep, collapse = " + ")) else "~1"
  # random-effect structure for individuals (automatic choice from the data support)
  rs_request <- normalise_slope(random_slope)
  advice <- random_slope_advice(individual_data_support(dd))
  rs <- if (identical(rs_request, "auto")) advice$recommended else rs_request
  random_note <- if (identical(rs_request, "auto")) {
    paste0("Automatic random effects: ", slope_text(rs), ". ", advice$text)
  } else if (!identical(rs, "none") && identical(advice$level, "none")) {
    paste("Random slopes were requested although the data barely support them:", advice$facts)
  } else ""
  # additional random intercepts need at least two levels in the analysed rows
  keep_re <- rterms[vapply(rterms, function(rt) length(unique(dd[[rt]])) >= 2, logical(1))]
  for (rt in setdiff(rterms, keep_re)) {
    drop_by <- c(drop_by, sprintf("random intercept for %s omitted (fewer than 2 levels)", meta$random_labels[[rt]]))
  }
  # one level per record: not separable from the residual in Gaussian models; an observation-level random effect
  # (extra dispersion) in count and binomial models
  per_record <- keep_re[vapply(keep_re, function(rt) length(unique(dd[[rt]])) >= nrow(dd), logical(1))]
  if (length(per_record)) {
    if (identical(family, "gaussian")) {
      keep_re <- setdiff(keep_re, per_record)
      for (rt in per_record) drop_by <- c(drop_by, sprintf("random intercept for %s omitted (one level per record cannot be separated from the residual)", meta$random_labels[[rt]]))
    } else {
      for (rt in per_record) drop_by <- c(drop_by, sprintf("random intercept for %s has one level per record and acts as an observation-level random effect (extra dispersion)", meta$random_labels[[rt]]))
    }
  }
  rstr <- random_term_string(b, rs, meta$has_group, keep_re, has_group2 = isTRUE(meta$has_group2))
  re_checks <- tryCatch(random_effect_checks(dd, rs, keep_re, isTRUE(meta$has_group), isTRUE(meta$has_group2)), error = function(e) NULL)
  # (4) terms that duplicate others; (7) raw-scale polynomials and interactions
  alias_notes <- tryCatch(duplicate_term_notes(dd, fs[models]), error = function(e) character(0))
  fit_notes <- character(0)
  high_order <- identical(age_function, "Cubic") || (among %in% c("same", "consistent") && length(b) > 1) ||
    (identical(age_function, "Quadratic") && any(grepl("[*:]", unlist(fs[models]))))
  if (!isTRUE(standardise) && high_order) {
    fit_notes <- c(fit_notes, "Standardisation is switched off: raw-scale polynomial and interaction terms can sometimes be numerically unstable when model predictions are calculated. Use 'Standardise age and proxies' in the model settings to switch it on or off.")
  }
  # dry run: stop before fitting and report the rows and individuals the selected models would share
  if (isTRUE(dry_run)) {
    return(list(ok = TRUE, dry_run = TRUE, models = models, status = status, drop_by = drop_by,
                n_rows = nrow(d), n_rows_used = nrow(dd), n_ids = length(unique(d$id)), n_ids_used = length(unique(dd$id)),
                formulas = fs[models], random = rstr, zi = if (family %in% ZI_FAMILIES) zi_str else "", family = family,
                disp = disp, disp_formula = disp_str,
                standardise = isTRUE(standardise), age_function = age_function, alias_notes = alias_notes, fit_notes = fit_notes,
                re_checks = re_checks))
  }
  fits <- list(); aic_rows <- list(); coef_rows <- list(); vc_rows <- list(); validity <- list(); diagnostics <- list()
  for (i in seq_along(models)) {
    m <- models[[i]]
    if (is.function(progress)) progress(i, length(models), model_label(m))
    res <- fit_one_model(fs[[m]], rstr, dd, family, zi_str, disp_str)
    validity[[m]] <- res$validity
    diagnostics[[m]] <- res$diagnostics
    if (is.null(res$fit)) {
      status[[m]] <- paste("Failed:", paste(res$notes, collapse = " | "))
      next
    }
    fits[[m]] <- res$fit
    status[[m]] <- if (length(res$notes)) paste0(res$validity, ": ", paste(res$notes, collapse = " | ")) else "Valid"
    ll <- stats::logLik(res$fit)
    aic_rows[[m]] <- data.frame(Model = model_label(m), AIC = tryCatch(stats::AIC(res$fit), error = function(e) NA_real_), df = attr(ll, "df"),
                                logLik = as.numeric(ll), N = stats::nobs(res$fit), Fit = res$validity, stringsAsFactors = FALSE)
    ct <- coef_table(res$fit, model_label(m))
    if (!is.null(ct)) coef_rows[[m]] <- ct
    vc <- variance_components(res$fit, model_label(m))
    if (!is.null(vc)) vc_rows[[m]] <- vc
  }
  aic <- if (length(aic_rows)) do.call(rbind, aic_rows) else data.frame()
  n_excluded <- 0L
  if (nrow(aic)) {
    # fits with an invalid Hessian (or non-finite AIC) are excluded from ranking unless the user overrides it
    aic$Eligible <- is.finite(aic$AIC) & (aic$Fit != "Failed" | isTRUE(include_invalid))
    n_excluded <- sum(!aic$Eligible)
    aic$Delta_AIC <- NA_real_
    aic$Weight <- NA_real_
    if (any(aic$Eligible)) {
      best <- min(aic$AIC[aic$Eligible])
      aic$Delta_AIC[aic$Eligible] <- aic$AIC[aic$Eligible] - best
      w <- exp(-aic$Delta_AIC[aic$Eligible] / 2)
      aic$Weight[aic$Eligible] <- w / sum(w)
    }
    aic <- aic[order(!aic$Eligible, aic$AIC), , drop = FALSE]
    rownames(aic) <- NULL
  }
  n_eligible <- if (nrow(aic)) sum(aic$Eligible) else 0L
  res <- list(ok = n_eligible > 0,
       message = if (n_eligible > 0) {
         paste0("Models fitted by maximum likelihood on the same complete-case rows.",
                if (n_excluded > 0) sprintf(" %d fit(s) with an invalid Hessian were excluded from the ranking.", n_excluded) else "")
       } else if (nrow(aic)) {
         "Every fitted model has an invalid (non-positive-definite or singular) Hessian: simplify the random effects, zero-inflation or fixed effects, or tick 'Include fits with invalid Hessians' to inspect them."
       } else "No model could be fitted (see the fitting status).",
       validity = validity, n_excluded = n_excluded, include_invalid = isTRUE(include_invalid),
       random_request = rs_request, random_structure = rs, random_note = random_note, random_advice = advice,
       among = if (among %in% c("same", "consistent") && length(b) > 1) among else "linear", cov_age = cov_age, extra = extra, nonlinear = FALSE,
       alias_notes = alias_notes, fit_notes = fit_notes, has_censor = isTRUE(meta$has_censor),
       schedule_irregular = isTRUE(tryCatch(schedule_irregular(dd, meta$age_step), error = function(e) FALSE)),
       cov_pairs = cov_pairs[vapply(strsplit(cov_pairs, ":", fixed = TRUE), function(p) all(p %in% covars), logical(1))],
       fits = fits, aic = aic, coefficients = if (length(coef_rows)) do.call(rbind, coef_rows) else data.frame(),
       status = status, data = dd, n_dropped = sum(!ok), n_ids_dropped = length(unique(d$id)) - length(unique(dd$id)), drop_by = drop_by, formulas = fs[models], random = rstr,
       row_sets = row_sets,
       basis = b, age_params = prep$age_params, proxy_params = prep$proxy_params, family = family, zi = zi_str,
       disp = disp, disp_formula = disp_str,
       covars = covars, random_terms = keep_re,
       varcomp = if (length(vc_rows)) do.call(rbind, vc_rows) else data.frame(),
       age_function = age_function, random_slope = rs, standardise = isTRUE(standardise),
       lrt = lrt_table(fits, validity, include_invalid, fs))
  res$diagnostics <- tryCatch(diagnostics_table(diagnostics, validity), error = function(e) data.frame())
  res$re_checks <- re_checks
  res$provenance <- tryCatch(analysis_provenance(res, meta), error = function(e) NULL)
  res
}
