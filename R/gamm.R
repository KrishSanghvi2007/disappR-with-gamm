# ---------------------------------------------------------------------------
# GAMM layer (step 7): generalized additive mixed models of ageing, fitted with mgcv. An independent layer: it reads
# the standardised data and meta of the app and changes nothing used by steps 1-6. Every name here starts gamm_/GAMM_.
#
# Approach (Wood 2017; Pedersen et al. 2019; Simpson 2018), as used in ageing studies (e.g. Hammers et al. 2016;
# Brown et al. 2021; Ravindran et al. 2025; Tsui et al. 2026):
#   - a penalised smooth of age, smoothness estimated by REML (ML when models with different fixed terms are compared);
#   - individual variation as random effects written as penalised smooths (intercept, intercept + slope, or a random
#     smooth per individual), plus any mapped random intercepts (year, cohort, observer...);
#   - selective disappearance through ALR or known lifespan, and selective appearance through AFR, each linear, smooth
#     or age-dependent through ti(age, proxy); a terminal-record effect is optional;
#   - groups (sex, treatment) as fixed-effect smooths: G, GS, GI, S and I of Pedersen et al. (2019), all with the group
#     main effect, so that "groups age differently" is never confused with "groups differ in level";
#   - ageing read from the first derivative of the population curve with pointwise and simultaneous intervals.
# Every model comparison fits all its models to the same records.
# ---------------------------------------------------------------------------

gamm_has_mgcv <- function() requireNamespace("mgcv", quietly = TRUE)
gamm_has_gamm4 <- function() requireNamespace("gamm4", quietly = TRUE)

GAMM_FAMILIES <- c("Gaussian" = "gaussian",
                   "Gaussian, variance changing with age (gaulss)" = "gaulss",
                   "Gamma, log link (positive values)" = "Gamma",
                   "Beta (proportions strictly between 0 and 1)" = "betar",
                   "Poisson (counts)" = "poisson",
                   "Negative binomial (overdispersed counts)" = "nb",
                   "Zero-inflated Poisson" = "ziP",
                   "Tweedie (non-negative, with zeros)" = "tw",
                   "Binomial (0/1, or proportions with trials)" = "binomial")
GAMM_BASES <- c("Thin plate (tp, default)" = "tp", "Cubic regression (cr, faster)" = "cr",
                "Thin plate with shrinkage (ts)" = "ts", "Cubic regression with shrinkage (cs)" = "cs",
                "Adaptive (ad; wiggliness varies with age)" = "ad")
GAMM_STRUCTURES <- c("G: one curve, groups differ only in level" = "G",
                     "GS: common curve + group deviations, shared wiggliness" = "GS",
                     "GI: common curve + group deviations, own wiggliness" = "GI",
                     "S: a curve per group, shared wiggliness" = "S",
                     "I: a separate curve per group" = "I")
GAMM_INDIVIDUAL <- c("Random intercept" = "ri",
                     "Random intercept and slope (uncorrelated)" = "rs",
                     "Random smooth per individual (fs)" = "fs",
                     "None (not recommended)" = "none")
GAMM_PROXY_FORMS <- c("None" = "none", "Linear" = "linear", "Smooth" = "smooth",
                      "Age-dependent, linear in the proxy: s(age, by = proxy)" = "vc",
                      "Age-dependent, smooth in both: smooth + ti(age, proxy)" = "ti")
GAMM_ENGINES <- c("Automatic (gam; bam or gamm4 for large data)" = "auto", "mgcv::gam" = "gam", "mgcv::bam (many records)" = "bam",
                  "gamm4 (lme4; thousands of individuals)" = "gamm4")
GAMM_BAM_ROWS <- 6000      # automatic engine: bam above this many records ...
GAMM_BAM_IND <- 400        # ... or this many individuals (gam slows sharply with many random-effect levels)
GAMM_FAST_LEVELS <- 300    # comparisons (ML) switch to gamm4 above this many random-effect levels, when the models allow it
GAMM_G4_IND <- 1500        # gamm4 (sparse lme4 random effects) above this many individuals, when the model allows it:
                           # mgcv stores each individual as a dense column, so time and memory grow with its square

gamm_defaults <- function() {
  list(engine = "auto", method = "REML", family = "gaussian", bs = "tp", k = 10, select = FALSE,
       individual = "ri", group = "", structure = "G", proxy = "ALR", proxy_form = "linear", afr_form = "none",
       terminal = FALSE, covars = TRUE, cov_smooth = FALSE, random = TRUE, ar1 = FALSE, olre = FALSE,
       cov_pairs = character(0), cov_age = character(0))
}

gamm_family_obj <- function(f) {
  fam <- switch(f, gaussian = stats::gaussian(), gaulss = mgcv::gaulss(), Gamma = stats::Gamma(link = "log"),
                betar = mgcv::betar(link = "logit"), poisson = stats::poisson(), nb = mgcv::nb(), ziP = mgcv::ziP(),
                tw = mgcv::tw(), binomial = stats::binomial(), stats::gaussian())
  # mgcv's tw() family looks up ldTweedie() on the search path, so it fails unless mgcv is attached with library().
  # The app never attaches mgcv, so the function is placed where the family's own functions look first.
  if (identical(f, "tw")) for (el in fam) if (is.function(el)) {
    e <- environment(el)
    if (!is.null(e) && !environmentIsLocked(e) && !exists("ldTweedie", envir = e, inherits = FALSE))
      try(assign("ldTweedie", mgcv::ldTweedie, envir = e), silent = TRUE)
  }
  fam
}
gamm_family_label <- function(f) { l <- names(GAMM_FAMILIES)[GAMM_FAMILIES == f]; if (length(l)) l[[1]] else f }
gamm_family_code <- function(f) switch(f, gaussian = "gaussian()", gaulss = "gaulss()", Gamma = "Gamma(link = \"log\")",
                                       betar = "betar(link = \"logit\")", poisson = "poisson()", nb = "nb()", ziP = "ziP()",
                                       tw = "tw()", binomial = "binomial()", "gaussian()")

# The family the app suggests for the trait (from the data-integrity check of step 1), mapped to a GAMM family.
gamm_suggest_family <- function(app_family) {
  switch(app_family %||% "gaussian", gaussian = "gaussian", gamma = "Gamma", lognormal = "Gamma", beta = "betar",
         poisson = "poisson", nbinom2 = "nb", nbinom1 = "nb", zip = "ziP", zinb = "nb", zinb1 = "nb",
         binomial = "binomial", betabinomial = "binomial", "gaussian")
}

# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------

# The analysis frame: one row per record with the trait, age, individual, the lifespan proxies (one value per
# individual), a terminal-record indicator, mapped covariates, grouping and random-effect columns, and binomial trials.
# ALR falls back to the last recorded age and AFR to the first; LS (known lifespan) stays missing where unknown.
gamm_data <- function(dat, meta) {
  d <- dat[is.finite(dat$trait) & is.finite(dat$age), , drop = FALSE]
  if (!nrow(d)) return(data.frame())
  d <- d[order(as.character(d$id), d$age), , drop = FALSE]
  out <- data.frame(id = factor(d$id), age = as.numeric(d$age), trait = as.numeric(d$trait), stringsAsFactors = FALSE)
  first_of <- function(v) {
    v <- as.numeric(v)
    stats::ave(v, d$id, FUN = function(x) { x <- x[is.finite(x)]; if (length(x)) x[[1]] else NA_real_ })
  }
  out$ALR <- if (!is.null(d$alr) && any(is.finite(d$alr))) first_of(d$alr) else stats::ave(out$age, d$id, FUN = max)
  out$ALR[!is.finite(out$ALR)] <- stats::ave(out$age, d$id, FUN = max)[!is.finite(out$ALR)]
  out$LS <- if (!is.null(d$life)) first_of(d$life) else NA_real_
  out$AFR <- if (!is.null(d$entry) && any(is.finite(d$entry))) first_of(d$entry) else stats::ave(out$age, d$id, FUN = min)
  out$AFR[!is.finite(out$AFR)] <- stats::ave(out$age, d$id, FUN = min)[!is.finite(out$AFR)]
  # Terminal record: the last record of an individual known to have died (a known lifespan, or not censored when a
  # censoring column is mapped). Unknown for the rest, which are coded "no" but counted, so the app can warn.
  last <- out$age == stats::ave(out$age, d$id, FUN = max)
  censored <- as.character(d$id) %in% as.character(meta$censored_ids %||% character(0))
  dead <- is.finite(out$LS) | (isTRUE(meta$has_censor) & !censored)
  out$.terminal <- factor(ifelse(last & dead, "yes", "no"), levels = c("no", "yes"))
  attr(out, "n_dead_known") <- length(unique(out$id[dead]))
  for (cv in meta$covars %||% character(0)) {
    if (!cv %in% names(d)) next
    out[[cv]] <- if (identical(meta$cov_types[[cv]], "categorical")) factor(d[[cv]]) else as.numeric(d[[cv]])
  }
  for (rt in meta$random_terms %||% character(0)) if (rt %in% names(d)) out[[rt]] <- factor(d[[rt]])
  if (isTRUE(meta$has_group) && !is.null(d$group)) out$group <- factor(d$group)
  if (isTRUE(meta$has_group2) && !is.null(d$group2)) out$group2 <- factor(d$group2)
  if (isTRUE(meta$has_trials) && !is.null(d$.trials)) out$.trials <- as.numeric(d$.trials)
  rownames(out) <- NULL
  out
}

# Categorical variables that can define group-specific smooths (sex, treatment...): categorical covariates and the
# grouping columns with 2 to 8 levels, each level with at least 5 individuals.
gamm_group_choices <- function(meta, gd) {
  if (!is.data.frame(gd) || !nrow(gd)) return(character(0))
  cv <- meta$covars %||% character(0)
  cand <- stats::setNames(cv, vapply(cv, function(v) as.character(meta$cov_labels[[v]] %||% sub("^cv_", "", v)), character(1)))
  cand <- cand[vapply(cand, function(v) identical(meta$cov_types[[v]], "categorical"), logical(1))]
  if (isTRUE(meta$has_group)) cand <- c(cand, stats::setNames("group", as.character(meta$map$group %||% "group")))
  ok <- vapply(cand, function(v) {
    if (!v %in% names(gd)) return(FALSE)
    x <- droplevels(factor(gd[[v]]))
    n_ind <- tapply(gd$id, x, function(z) length(unique(z)))
    nlevels(x) %in% 2:8 && all(n_ind >= 5)
  }, logical(1))
  cand[ok]
}

# Which covariate and random-effect columns can enter a model (vary in the data); the rest are left out with a note.
gamm_usable_terms <- function(gd, s) {
  cv <- if (isTRUE(s$covars)) grep("^cv_", names(gd), value = TRUE) else character(0)
  re <- if (isTRUE(s$random)) grep("^re_", names(gd), value = TRUE) else character(0)
  varies <- function(v) { x <- gd[[v]]; x <- x[!is.na(x)]; length(unique(x)) >= 2 }
  list(cv = cv[vapply(cv, varies, logical(1))], re = re[vapply(re, varies, logical(1))],
       dropped = c(cv[!vapply(cv, varies, logical(1))], re[!vapply(re, varies, logical(1))]))
}

# ---------------------------------------------------------------------------
# Formula
# ---------------------------------------------------------------------------

gamm_age_smooth <- function(s, by = NULL, m = NA, k = s$k, extra = "") {
  bs <- s$bs %||% "tp"
  use_m <- is.finite(m) && bs %in% c("tp", "ts")
  parts <- c(sprintf("bs = \"%s\"", bs), sprintf("k = %d", as.integer(k)), if (use_m) sprintf("m = %d", as.integer(m)),
             if (!is.null(by)) sprintf("by = %s", by), if (nzchar(extra)) extra)
  sprintf("s(age, %s)", paste(parts, collapse = ", "))
}

gamm_proxy_k <- function(gd, p) {
  nu <- length(unique(gd[[p]][is.finite(gd[[p]])]))
  max(3L, min(6L, nu - 1L))
}

# The model formula (a string) for the settings. structure, proxy_form and afr_form can be overridden for comparisons;
# age_poly > 0 replaces the smooth with a polynomial, age_poly < 0 leaves age out (the 'no age effect' model); ordered_by fits group difference smooths against a reference.
gamm_formula <- function(s, gd, structure = s$structure, proxy_form = s$proxy_form, afr_form = s$afr_form,
                         age_poly = 0L, ordered_by = FALSE) {
  grp <- if (nzchar(s$group %||% "") && s$group %in% names(gd)) s$group else ""
  if (!nzchar(grp)) structure <- "G"
  k <- as.integer(s$k)
  ut <- gamm_usable_terms(gd, s)
  terms <- character(0)
  if (nzchar(grp)) terms <- c(terms, if (isTRUE(ordered_by)) ".grp_o" else grp)       # the group main effect, always
  if (age_poly < 0) {
    # no age term: the null model of the shape comparison
  } else if (age_poly > 0) {
    terms <- c(terms, if (age_poly == 1) "age" else sprintf("poly(age, %d, raw = TRUE)", as.integer(age_poly)))
  } else if (isTRUE(ordered_by)) {
    terms <- c(terms, gamm_age_smooth(s), gamm_age_smooth(s, by = ".grp_o"))
  } else {
    xt <- sprintf("xt = list(bs = \"%s\")", if (s$bs %in% c("cr", "cs")) "cr" else "tp")
    terms <- c(terms, switch(structure,
      G = gamm_age_smooth(s),
      GS = c(gamm_age_smooth(s, m = 2), sprintf("s(age, %s, bs = \"sz\", k = %d, %s)", grp, k, xt)),
      GI = c(gamm_age_smooth(s, m = 2), gamm_age_smooth(s, by = grp, m = 1)),
      S = gamm_age_smooth(s, by = grp, extra = "id = 1"),
      I = gamm_age_smooth(s, by = grp)))
  }
  prox_terms <- function(p, form) {
    if (identical(form, "none") || !p %in% names(gd) || sum(is.finite(gd[[p]])) < 5) return(character(0))
    if (length(unique(gd[[p]][is.finite(gd[[p]])])) < 3 && !identical(form, "linear")) form <- "linear"
    kp <- gamm_proxy_k(gd, p)
    switch(form, linear = p, smooth = sprintf("s(%s, k = %d)", p, kp),
           vc = sprintf("s(age, by = .%s_c, k = %d)", p, min(5L, k)),      # f(age) x centred proxy; includes the linear proxy term
           ti = c(sprintf("s(%s, k = %d)", p, kp), sprintf("ti(age, %s, k = c(%d, %d))", p, min(5L, k), min(5L, kp))),
           character(0))
  }
  if (!identical(s$proxy %||% "none", "none")) terms <- c(terms, prox_terms(s$proxy, proxy_form))
  terms <- c(terms, prox_terms("AFR", afr_form))
  if (isTRUE(s$terminal) && ".terminal" %in% names(gd) && length(unique(gd$.terminal)) == 2) terms <- c(terms, ".terminal")
  cv <- setdiff(ut$cv, grp)
  for (v in cv) {
    nu <- length(unique(gd[[v]][!is.na(gd[[v]])]))
    terms <- c(terms, if (isTRUE(s$cov_smooth) && is.numeric(gd[[v]]) && nu >= 6) sprintf("s(%s, k = %d)", v, min(6L, nu - 1L)) else v)
  }
  # covariate interactions mapped on the Data tab: covariate x covariate as products; numeric covariate x age as ti()
  for (pp in s$cov_pairs %||% character(0)) { v <- strsplit(pp, ":", fixed = TRUE)[[1]]; if (length(v) == 2 && all(v %in% cv)) terms <- c(terms, pp) }
  for (v in intersect(s$cov_age %||% character(0), cv)) if (is.numeric(gd[[v]]) && length(unique(gd[[v]])) >= 5) terms <- c(terms, sprintf("ti(age, %s, k = c(5, 5))", v))
  terms <- c(terms, switch(s$individual %||% "ri",
                           ri = "s(id, bs = \"re\")",
                           rs = c("s(id, bs = \"re\")", "s(id, .age_c, bs = \"re\")"),
                           fs = sprintf("s(age, id, bs = \"fs\", k = %d, m = 1, xt = list(bs = \"%s\"))", min(5L, k),
                                        if (s$bs %in% c("cr", "cs")) "cr" else "tp"),
                           character(0)))
  for (rt in ut$re) terms <- c(terms, sprintf("s(%s, bs = \"re\")", rt))
  if (gamm_olre_on(s)) terms <- c(terms, "s(.obs, bs = \"re\")")       # observation-level random effect (overdispersion)
  paste("trait ~", paste(unique(terms), collapse = " + "))
}

# The variables a formula needs from the data (for complete-case rows).
gamm_formula_vars <- function(f_str, gd) {
  v <- intersect(all.vars(stats::as.formula(f_str)), names(gd))
  av <- all.vars(stats::as.formula(f_str))
  if (".age_c" %in% av) v <- c(v, "age")
  cp <- grep("^\\.(ALR|LS|AFR)_c$", av, value = TRUE)
  v <- c(v, intersect(sub("^\\.(.*)_c$", "\\1", cp), names(gd)))
  unique(v)
}

# Rows usable by every one of a set of formulas: comparisons fit all their models to the same records.
gamm_common_rows <- function(gd, formulas, family = "gaussian") {
  vars <- unique(unlist(lapply(formulas, gamm_formula_vars, gd = gd)))
  keep <- stats::complete.cases(gd[, vars, drop = FALSE])
  if (identical(family, "binomial") && ".trials" %in% names(gd) && any(is.finite(gd$.trials))) keep <- keep & is.finite(gd$.trials) & gd$.trials > 0
  keep
}

# ---------------------------------------------------------------------------
# Fitting
# ---------------------------------------------------------------------------

# Checks the trait against the family; returns a message, or NULL when the family suits the data.
gamm_family_problem <- function(y, fam, trials = NULL) {
  y <- y[is.finite(y)]
  if (fam %in% c("Gamma") && any(y <= 0)) return("The Gamma family needs strictly positive values: choose Gaussian or Tweedie.")
  if (identical(fam, "betar") && any(y <= 0 | y >= 1)) return("The beta family needs values strictly between 0 and 1.")
  if (fam %in% c("poisson", "nb", "ziP") && any(y < 0 | abs(y - round(y)) > 1e-8)) return("Count families need non-negative whole numbers.")
  if (identical(fam, "tw") && any(y < 0)) return("The Tweedie family needs non-negative values.")
  if (identical(fam, "binomial")) {
    if (any(y < 0 | y > 1)) return("The binomial family needs 0/1 values or proportions between 0 and 1 (with a trials column).")
    if (is.null(trials) && any(y > 0 & y < 1)) return("Proportions need a trials column (map it on the Data or Modelling tab); without one the binomial family takes only 0/1 values.")
  }
  if (length(unique(y)) < 2) return("The trait does not vary, so there is nothing to model.")
  if (max(abs(y)) > 1e12 || (max(abs(y)) > 0 && max(abs(y)) < 1e-12)) return("The trait's values are extremely large or small: rescale it (for example to other units, or z-scores) and fit again.")
  NULL
}

# Can gamm4 fit this model? It takes the standard families, individual random intercepts and slopes as lme4 terms, and
# smooths that lme4 can represent: not ti() interactions, factor-smooth (sz, fs) or linked (id = ) group smooths.
gamm_g4_compatible <- function(s, structure = s$structure, proxy_form = s$proxy_form, afr_form = s$afr_form, gd = NULL) {
  # covariate x age interactions become ti() only for numeric covariates (categorical ones are handled by group curves)
  ca <- s$cov_age %||% character(0)
  if (!is.null(gd)) ca <- ca[vapply(ca, function(v) v %in% names(gd) && is.numeric(gd[[v]]) && length(unique(gd[[v]])) >= 5, logical(1))]
  gamm_has_gamm4() && s$family %in% c("gaussian", "poisson", "binomial", "Gamma") && (s$individual %||% "ri") %in% c("ri", "rs", "none") &&
    !"ti" %in% c(proxy_form, afr_form) && structure %in% c("G", "GI", "I") && !isTRUE(s$ar1) && !isTRUE(s$select) && !length(ca)
}

gamm_engine_for <- function(s, n, n_ind, structure = s$structure, proxy_form = s$proxy_form, afr_form = s$afr_form, gd = NULL) {
  e <- s$engine %||% "auto"
  if (identical(s$family, "gaulss")) return("gam")                     # location-scale families: gam only
  if (isTRUE(s$ar1) && identical(s$family, "gaussian")) return("bam")  # AR1 errors: bam only
  g4 <- gamm_g4_compatible(s, structure, proxy_form, afr_form, gd = gd)
  if (identical(e, "gamm4") && !g4) e <- "bam"
  if (identical(e, "auto")) return(if ((n_ind > GAMM_G4_IND || gamm_olre_on(s)) && g4) "gamm4" else if (n > GAMM_BAM_ROWS || n_ind > GAMM_BAM_IND || gamm_olre_on(s)) "bam" else "gam")
  e
}

# Random-effect levels of a model on its complete records: individuals plus the levels of mapped random intercepts. With
# many levels, ML fits in mgcv are slow (no discretisation under ML, dense random-effect columns).
gamm_re_levels <- function(gd, s) {
  keep <- tryCatch(gamm_common_rows(gd, list(gamm_formula(s, gd)), s$family), error = function(e) rep(TRUE, nrow(gd)))
  ut <- gamm_usable_terms(gd[keep, , drop = FALSE], s)
  length(unique(gd$id[keep])) + sum(vapply(ut$re, function(v) length(unique(gd[[v]][keep])), numeric(1)))
}

# Random-effect levels above which comparisons use gamm4 (GAMM_FAST_LEVELS; the environment variable
# DISAPPR_GAMM_FAST_LEVELS overrides it for validation runs and tests).
gamm_fast_threshold <- function() {
  x <- suppressWarnings(as.numeric(Sys.getenv("DISAPPR_GAMM_FAST_LEVELS", GAMM_FAST_LEVELS)))
  if (length(x) == 1 && is.finite(x)) x else GAMM_FAST_LEVELS
}

# One engine for every model of a comparison, so that their AICs are comparable: gamm4 only if all the models allow it.
# fast_ok: comparisons (fitted by ML) use gamm4 from GAMM_FAST_LEVELS random-effect levels, not only from GAMM_G4_IND
# individuals.
gamm_set_engine <- function(gd, s, specs, fast_ok = FALSE) {
  # count records and individuals as a single fit would: those complete for the model's variables
  keep <- tryCatch(gamm_common_rows(gd, list(gamm_formula(s, gd)), s$family), error = function(e) rep(TRUE, nrow(gd)))
  n <- sum(keep); n_ind <- length(unique(gd$id[keep]))
  all_g4 <- all(vapply(specs, function(x) gamm_g4_compatible(s, x$structure %||% s$structure, x$proxy_form %||% s$proxy_form,
                                                            x$afr_form %||% s$afr_form, gd = gd), logical(1)))
  e <- s$engine %||% "auto"
  if (identical(e, "gamm4") && !all_g4) return("bam")
  if (identical(e, "auto")) {
    if (all_g4 && (n_ind > GAMM_G4_IND || gamm_olre_on(s) || (isTRUE(fast_ok) && gamm_re_levels(gd, s) > gamm_fast_threshold()))) return("gamm4")
    return(if (n > GAMM_BAM_ROWS || n_ind > GAMM_BAM_IND) "bam" else "gam")
  }
  e
}

# lme4 random-effect formula for gamm4: individual intercept (and uncorrelated slope of centred age, as in mgcv), and
# the mapped random intercepts.
# An observation-level random effect (one random intercept per record) absorbs overdispersion in Poisson counts and
# binomial proportions (Harrison 2014). It applies to those two families only: the others model dispersion themselves.
gamm_olre_on <- function(s) isTRUE(s$olre) && (s$family %||% "gaussian") %in% c("poisson", "binomial")

gamm_g4_random <- function(s, re_terms) {
  re <- switch(s$individual %||% "ri", rs = c("(1 | id)", "(0 + .age_c | id)"), none = character(0), "(1 | id)")
  re <- c(re, sprintf("(1 | %s)", re_terms), if (gamm_olre_on(s)) "(1 | .obs)")
  if (!length(re)) return(NULL)
  stats::as.formula(paste("~", paste(re, collapse = " + ")))
}

# Fitted values (response scale, random effects included) and residuals, whichever engine fitted the model.
gamm_fitted <- function(r) {
  f <- if (!is.null(r$mer)) stats::fitted(r$mer) else stats::fitted(r$fit)
  if (is.matrix(f)) f <- f[, 1]
  as.numeric(f)
}
gamm_resid <- function(r, type = "deviance") {
  x <- if (!is.null(r$mer)) tryCatch(stats::residuals(r$mer, type = type), error = function(e) stats::residuals(r$mer))
       else stats::residuals(r$fit, type = type)
  as.numeric(x)
}

# Lag-1 autocorrelation of residuals between consecutive records of the same individual.
gamm_lag1 <- function(res, id) {
  ok <- is.finite(res)
  res <- res[ok]; id <- id[ok]
  same <- c(FALSE, id[-1] == id[-length(id)])
  if (sum(same) < 10) return(NA_real_)
  suppressWarnings(stats::cor(res[which(same) - 1], res[same]))
}

# Fit one GAMM. Returns list(ok, fit, formula, data (the rows used), settings, method, engine, notes, n, n_ind, ...).
# rows: optional logical vector selecting the records to use (comparisons pass their common rows).
gamm_fit <- function(gd, s, method = s$method, structure = s$structure, proxy_form = s$proxy_form,
                     afr_form = s$afr_form, age_poly = 0L, ordered_by = FALSE, rows = NULL) {
  fail <- function(msg) list(ok = FALSE, message = msg)
  if (!gamm_has_mgcv()) return(fail("The GAMM layer needs the mgcv package: install.packages('mgcv')."))
  if (!is.data.frame(gd) || nrow(gd) < 20) return(fail("At least 20 records are needed to fit a GAMM."))
  s <- utils::modifyList(gamm_defaults(), s)
  notes <- character(0)
  fam <- s$family
  if (!identical(s$group %||% "", "") && !s$group %in% names(gd)) s$group <- ""
  if (!nzchar(s$group)) structure <- "G"
  if (identical(s$proxy, "LS") && !any(is.finite(gd$LS))) return(fail("Known lifespan (LS) is not mapped or is missing for everyone: choose ALR, or map a lifespan column on the Data tab."))
  if (isTRUE(ordered_by) && nzchar(s$group)) {
    gd$.grp_o <- factor(gd[[s$group]], ordered = TRUE)
    stats::contrasts(gd$.grp_o) <- "contr.treatment"
  }
  f_str <- gamm_formula(s, gd, structure, proxy_form, afr_form, age_poly, ordered_by)
  keep <- gamm_common_rows(gd, list(f_str), fam)
  if (!is.null(rows)) keep <- keep & rows
  dd <- droplevels(gd[keep, , drop = FALSE])
  if (sum(!keep)) notes <- c(notes, sprintf("%d record(s) with missing values in the model's variables were left out", sum(!keep)))
  if (nrow(dd) < 20) return(fail("Too few complete records (fewer than 20) for this model."))
  if (nlevels(droplevels(dd$id)) < 3) return(fail("At least 3 individuals are needed: individual differences cannot be separated from ageing with fewer."))
  if (nzchar(s$group) && nlevels(droplevels(factor(dd[[s$group]]))) < 2) {
    structure <- "G"; notes <- c(notes, "the grouping variable has one level in these records, so one curve was fitted")
    s$group <- ""
    f_str <- gamm_formula(s, dd, structure, proxy_form, afr_form, age_poly, FALSE)
  }
  prob <- gamm_family_problem(dd$trait, fam, if (".trials" %in% names(dd) && any(is.finite(dd$.trials))) dd$.trials else NULL)
  if (!is.null(prob)) return(fail(prob))
  nu <- length(unique(dd$age))
  if (age_poly == 0 && nu < 4) return(fail("A smooth of age needs at least 4 distinct ages."))
  k0 <- as.integer(s$k)
  s$k <- max(3L, min(k0, nu - 1L))
  if (s$k < k0) {
    notes <- c(notes, sprintf("k was lowered from %d to %d (the number of distinct ages minus one)", k0, s$k))
    f_str <- gamm_formula(s, dd, structure, proxy_form, afr_form, age_poly, ordered_by)
  }
  ut <- gamm_usable_terms(dd, s)
  if (length(ut$dropped)) notes <- c(notes, paste("left out because they do not vary in these records:", paste(ut$dropped, collapse = ", ")))
  dd$.age_c <- dd$age - mean(dd$age)
  if (gamm_olre_on(s)) dd$.obs <- factor(seq_len(nrow(dd)))
  if (isTRUE(s$olre) && !gamm_olre_on(s)) notes <- c(notes, "the observation-level random effect applies to Poisson and binomial families only, so it was left out")
  # proxies centred on their mean over individuals, for the varying-coefficient (vc) form
  centres <- list()
  for (pp in intersect(c("ALR", "LS", "AFR"), names(dd))) {
    v1 <- dd[[pp]][!duplicated(dd$id)]
    centres[[pp]] <- if (any(is.finite(v1))) mean(v1, na.rm = TRUE) else 0
    dd[[paste0(".", pp, "_c")]] <- dd[[pp]] - centres[[pp]]
  }
  wts <- if (identical(fam, "binomial") && ".trials" %in% names(dd) && all(is.finite(dd$.trials))) dd$.trials else NULL
  n_ind <- nlevels(dd$id)
  engine <- gamm_engine_for(s, nrow(dd), n_ind, structure, proxy_form, afr_form, gd = dd)
  if (identical(s$engine, "gamm4") && !identical(engine, "gamm4")) notes <- c(notes, "this model needs mgcv (gamm4 cannot fit it), so bam was used")
  if (identical(engine, "bam") && fam %in% c("gaulss")) engine <- "gam"
  f <- stats::as.formula(f_str)
  famobj <- gamm_family_obj(fam)
  if (identical(fam, "gaulss")) f <- list(f, stats::as.formula(paste("~", gamm_age_smooth(s, k = min(s$k, 6L)))))
  rho <- 0
  fit <- NULL; mer <- NULL; err <- NULL
  if (identical(engine, "gamm4")) {
    f_g4 <- gamm_formula(utils::modifyList(s, list(individual = "none", random = FALSE, olre = FALSE)), dd, structure, proxy_form, afr_form, age_poly, ordered_by)
    rf <- gamm_g4_random(s, ut$re)
    f_str <- paste0(f_g4, "; random = ", paste(deparse(rf), collapse = ""))
  }
  t0 <- proc.time()[["elapsed"]]
  # do.call passes values rather than names, so mgcv never has to find weights or AR.start in a calling frame
  do_fit <- function(rho) {
    if (identical(engine, "gamm4")) {
      args <- list(formula = stats::as.formula(f_g4), random = rf, family = famobj, data = dd, REML = !identical(method, "ML"))
      if (!is.null(wts)) args$weights <- wts
      g4 <- do.call(gamm4::gamm4, args)
      mer <<- g4$mer
      return(g4$gam)
    }
    args <- list(formula = f, family = famobj, data = dd, select = isTRUE(s$select))
    if (!is.null(wts)) args$weights <- wts
    if (identical(engine, "bam")) {
      args$method <- if (identical(method, "ML")) "ML" else "fREML"
      args$discrete <- !identical(method, "ML")
      if (rho != 0) { args$rho <- rho; args$AR.start <- !duplicated(dd$id) }
      do.call(mgcv::bam, args)
    } else {
      args$method <- method
      do.call(mgcv::gam, args)
    }
  }
  res <- tryCatch(withCallingHandlers({
    fit <- if (identical(engine, "bam")) tryCatch(do_fit(0), error = function(e) {
      # bam's discretised fit fails on some small data sets with random effects (an mgcv quirk): use gam instead
      if (nrow(dd) > GAMM_BAM_ROWS || n_ind > GAMM_BAM_IND || isTRUE(s$ar1)) stop(e)
      notes <<- c(notes, "bam could not fit these data, so gam was used"); engine <<- "gam"; do_fit(0)
    }) else do_fit(0)
    if (isTRUE(s$ar1) && identical(fam, "gaussian") && identical(engine, "bam") && age_poly == 0) {
      rho <- gamm_lag1(stats::residuals(fit, type = "response"), dd$id)
      if (is.finite(rho) && rho > 0.05) { rho <- min(rho, 0.95); fit <- do_fit(rho) } else rho <- 0
    }
    TRUE
  }, warning = function(w) { notes <<- c(notes, conditionMessage(w)); invokeRestart("muffleWarning") }),
  error = function(e) { err <<- conditionMessage(e); FALSE })
  if (!isTRUE(res) || is.null(fit)) return(fail(paste("The GAMM could not be fitted:", err %||% "unknown error")))
  # Population curves average categorical covariates over their observed mix of levels (link scale) rather than fixing
  # them at the commonest level, which can describe a minority of records (for example subordinate helpers).
  cov_colmeans <- tryCatch({
    pt <- intersect(c(ut$cv, Filter(function(pp) all(strsplit(pp, ":", fixed = TRUE)[[1]] %in% ut$cv), s$cov_pairs %||% character(0))),
                    attr(stats::terms(fit$pterms %||% stats::as.formula("~ 1")), "term.labels"))
    if (length(pt)) colMeans(stats::model.matrix(stats::as.formula(paste("~", paste(pt, collapse = " + "))), dd))[-1] else numeric(0)
  }, error = function(e) numeric(0))
  conv <- if (!is.null(mer)) tryCatch(!length(mer@optinfo$conv$lme4$messages), error = function(e) TRUE) else
    tryCatch(isTRUE(fit$converged) && (is.null(fit$outer.info$conv) || identical(fit$outer.info$conv, "full convergence")),
             error = function(e) TRUE)
  if (!conv) notes <- c(notes, "the smoothing-parameter optimisation did not report full convergence")
  list(ok = TRUE, fit = fit, mer = mer, formula = f_str, data = dd, settings = s, method = method, structure = structure,
       proxy_form = proxy_form, afr_form = afr_form, engine = engine, rho = rho, converged = conv,
       notes = unique(notes), n = nrow(dd), n_ind = n_ind, age_mean = mean(dd$age), centres = centres, cov_colmeans = cov_colmeans,
       n_dropped = sum(!keep), seconds = proc.time()[["elapsed"]] - t0)
}

# One number, or NA when a summary has none (gamm4 reports deviance explained as numeric(0)).
gamm_num1 <- function(x) if (length(x) && is.finite(x[[1]])) as.numeric(x[[1]]) else NA_real_

gamm_aic <- function(r) {
  if (!isTRUE(r$ok)) return(NA_real_)
  tryCatch(stats::AIC(if (!is.null(r$mer)) r$mer else r$fit), error = function(e) NA_real_)
}

# ---------------------------------------------------------------------------
# Summaries and checks
# ---------------------------------------------------------------------------

gamm_tables <- function(r) {
  sm <- summary(r$fit, re.test = FALSE)
  pt <- as.data.frame(sm$p.table)
  pt <- data.frame(Term = rownames(pt), Estimate = pt[[1]], SE = pt[[2]], Statistic = pt[[3]], P = pt[[4]], stringsAsFactors = FALSE)
  st <- if (!is.null(sm$s.table) && nrow(sm$s.table)) {
    x <- as.data.frame(sm$s.table)
    data.frame(Smooth = rownames(x), edf = x[[1]], Ref.df = x[[2]], Statistic = x[[3]], P = x[[4]], stringsAsFactors = FALSE)
  } else data.frame()
  list(parametric = pt, smooth = st, dev_expl = gamm_num1(sm$dev.expl), r_sq = gamm_num1(sm$r.sq), n = sm$n %||% nrow(r$data),
       aic = gamm_aic(r), stat_name = if (isTRUE(sm$residual.df > 0) && identical(r$fit$family$family, "gaussian")) "F" else "Chi.sq")
}

# ---------------------------------------------------------------------------
# Model output (the GAMM counterpart of step 5's Model output tab): model statistics, parametric terms, fixed smooth
# terms and random effects, each with a plain description of what the term is.
# ---------------------------------------------------------------------------

gamm_is_random_label <- function(x) grepl("^s\\((id|re_|\\.obs)|,id\\)$|\\.age_c", x)

# What a term of the fitted GAMM is, in words.
gamm_term_description <- function(x, covs = character(0)) {
  v <- function(z) sub("^cv_", "", sub("^re_", "", z))
  # a factor covariate and its level, from a coefficient name such as cv_sexM (the longest matching covariate wins)
  split_level <- function(z) {
    hit <- covs[startsWith(z, covs) & nchar(z) > nchar(covs)]
    if (!length(hit)) return(NULL)
    h <- hit[which.max(nchar(hit))]
    c(h, substring(z, nchar(h) + 1))
  }
  vapply(x, function(t) {
    if (identical(t, "(Intercept)")) return("Mean trait at the reference levels (link scale)")
    if (t %in% c("ALR", "LS", "AFR")) return(sprintf("Linear effect of %s, per unit (selective %s)", t, if (t == "AFR") "appearance" else "disappearance"))
    if (grepl("^\\.terminal", t)) return("Terminal-record effect: last record before death against earlier records")
    if (identical(t, "age")) return("Linear effect of age")
    if (grepl("^poly\\(age", t)) return("Polynomial term of age")
    if (grepl("^s\\(age\\)$", t)) return("Smooth of age (the population ageing curve)")
    if (startsWith(t, "s(age):cv_")) {
      sl <- split_level(sub("^s\\(age\\):", "", t))
      if (!is.null(sl)) return(sprintf("Smooth of age for %s = %s", v(sl[1]), sl[2]))
    }
    m <- regmatches(t, regexec("^s\\(age\\):\\.grp_o(.*)$", t))[[1]]
    if (length(m) == 2) return(sprintf("Difference of group %s's curve from the reference curve", m[2]))
    m <- regmatches(t, regexec("^s\\(age\\):\\.(ALR|LS|AFR)_c$", t))[[1]]
    if (length(m) == 2) return(sprintf("Age-dependent effect of %s: how its effect changes with age (varying coefficient)", m[2]))
    m <- regmatches(t, regexec("^s\\(age,(cv_[^)]+)\\)$", t))[[1]]
    if (length(m) == 2) return(sprintf("Deviations of each %s level's curve from the common curve", v(m[2])))
    m <- regmatches(t, regexec("^ti\\(age,(ALR|LS|AFR)\\)$", t))[[1]]
    if (length(m) == 2) return(sprintf("Age \u00d7 %s interaction: age-dependent selective %s", m[2], if (m[2] == "AFR") "appearance" else "disappearance"))
    m <- regmatches(t, regexec("^ti\\(age,(cv_[^)]+)\\)$", t))[[1]]
    if (length(m) == 2) return(sprintf("Age \u00d7 %s interaction", v(m[2])))
    m <- regmatches(t, regexec("^s\\((ALR|LS|AFR)\\)$", t))[[1]]
    if (length(m) == 2) return(sprintf("Smooth (non-linear) effect of %s", m[2]))
    m <- regmatches(t, regexec("^s\\((cv_[^)]+)\\)$", t))[[1]]
    if (length(m) == 2) return(sprintf("Smooth effect of %s", v(m[2])))
    if (grepl("^s\\(id\\)$|^id intercept$", t)) return("Individual random intercepts")
    if (identical(t, "id slope of age")) return("Individual random slopes of age")
    if (identical(t, ".obs intercept")) return("Observation-level random effect (overdispersion)")
    m <- regmatches(t, regexec("^(re_[^ ]+) intercept$", t))[[1]]
    if (length(m) == 2) return(sprintf("Random intercepts for %s", v(m[2])))
    if (grepl("^s\\(id,\\.age_c\\)$", t)) return("Individual random slopes of age")
    if (grepl("^s\\(age,id\\)$", t)) return("Individual random smooths of age")
    if (grepl("^s\\(\\.obs\\)$", t)) return("Observation-level random effect (overdispersion)")
    m <- regmatches(t, regexec("^s\\((re_[^)]+)\\)$", t))[[1]]
    if (length(m) == 2) return(sprintf("Random intercepts for %s", v(m[2])))
    if (grepl("^cv_", t) && !grepl(":", t)) {
      sl <- split_level(t)
      if (!is.null(sl)) return(sprintf("%s = %s, against the reference level", v(sl[1]), sl[2]))
    }
    if (grepl("^cv_", t) && !grepl(":", t)) return(sprintf("Linear effect of %s, per unit", v(t)))
    if (grepl(":", t)) return("Interaction between covariates")
    ""
  }, character(1), USE.NAMES = FALSE)
}

# Model statistics, parametric terms, fixed smooths and random effects of a fitted GAMM.
gamm_output <- function(r) {
  sm <- summary(r$fit, re.test = FALSE)
  covs <- grep("^cv_", names(r$data), value = TRUE)
  covs <- covs[!vapply(r$data[covs], is.numeric, logical(1))]
  fam <- r$fit$family
  pt <- as.data.frame(sm$p.table)
  stat_p <- sub(" value$", "", colnames(pt)[3])
  par <- data.frame(Term = rownames(pt), Description = gamm_term_description(rownames(pt), covs), Estimate = pt[[1]], SE = pt[[2]],
                    Statistic = pt[[3]], P = pt[[4]], stringsAsFactors = FALSE, row.names = NULL)
  attr(par, "stat") <- stat_p
  smt <- if (!is.null(sm$s.table) && nrow(sm$s.table)) {
    x <- as.data.frame(sm$s.table)
    data.frame(Smooth = rownames(x), Description = gamm_term_description(rownames(x), covs), edf = x[[1]], Ref.df = x[[2]],
               Statistic = x[[3]], P = x[[4]], stringsAsFactors = FALSE, row.names = NULL)
  } else data.frame()
  stat_s <- if (grepl("Chi", colnames(sm$s.table)[3] %||% "")) "Chi.sq" else "F"
  if (nrow(smt)) {
    smt <- smt[!gamm_is_random_label(smt$Smooth), , drop = FALSE]
    smt$Shape <- ifelse(smt$edf < 1.5, "close to a straight line", ifelse(smt$edf < 3, "gently curved", "clearly curved"))
    rownames(smt) <- NULL
  }
  attr(smt, "stat") <- stat_s
  vc <- tryCatch(gamm_vcomp(r), error = function(e) data.frame())
  if (nrow(vc) && !is.null(r$mer)) {
    # lme4 names, written as the gamm4 random formula has them
    g4 <- vc$Term != "scale"
    desc <- gamm_term_description(vc$Term)
    grp <- sub(" (intercept|slope of age)$", "", vc$Term)
    vc$Term[g4] <- ifelse(grepl("slope of age$", vc$Term[g4]), sprintf("(0 + age | %s)", grp[g4]), sprintf("(1 | %s)", grp[g4]))
    attr(vc, "desc") <- desc
  }
  if (nrow(vc)) {
    vc$Description <- ifelse(vc$Term == "scale", if (identical(fam$family, "gaussian")) "Residual (within-individual) variation" else "Dispersion (scale) parameter",
                             attr(vc, "desc") %||% gamm_term_description(vc$Term))
    vc$Description[!nzchar(vc$Description)] <- "Random effect"
    vc$Variance <- vc$SD^2
    vc <- vc[, c("Term", "Description", "SD", "Variance", "Lower", "Upper")]
    names(vc)[names(vc) %in% c("Lower", "Upper")] <- c("SD lower 95%", "SD upper 95%")
    ref <- if (any(vc$Term == "scale")) vc$SD[vc$Term == "scale"][1] else NA_real_
    vc$Flag <- ifelse(vc$Term != "scale" & is.finite(vc$SD) & (vc$SD < 1e-4 | (is.finite(ref) & vc$SD < 0.01 * ref)), "\u26a0 explains (next to) no variance", "")
  }
  ll <- tryCatch(stats::logLik(if (!is.null(r$mer)) r$mer else r$fit), error = function(e) NA)
  crit_name <- if (!is.null(r$mer)) "REML/ML criterion (lme4)" else paste0(r$method %||% "REML", " score")
  crit <- if (!is.null(r$mer)) tryCatch(as.numeric(lme4::REMLcrit(r$mer)), error = function(e) tryCatch(stats::deviance(r$mer), error = function(e) NA_real_)) else gamm_num1(r$fit$gcv.ubre)
  stats <- data.frame(Statistic = c("Family (link)", "Fitting engine", "Smoothing parameters estimated by", "Records", "Individuals",
                                    if (is.null(r$mer)) "Effective degrees of freedom (all terms, random effects included)" else "Effective degrees of freedom (fixed terms; random effects in lme4)", crit_name, "Log-likelihood", "AIC", "Deviance explained",
                                    "Adjusted R\u00b2", if (identical(fam$family, "gaussian")) "Residual variance" else "Scale (dispersion)"),
                      Value = c(sprintf("%s (%s)", fam$family, fam$link), r$engine %||% "", r$method %||% "", format(r$n, big.mark = ","),
                                format(r$n_ind, big.mark = ","), format_num(sum(r$fit$edf), 4), format_num(crit, 6),
                                if (inherits(ll, "logLik")) format_num(as.numeric(ll), 6) else "", format_num(gamm_aic(r), 6),
                                if (is.finite(gamm_num1(sm$dev.expl))) sprintf("%.1f%%", 100 * gamm_num1(sm$dev.expl)) else "not reported (gamm4)",
                                if (is.null(r$mer)) format_num(gamm_num1(sm$r.sq), 3) else "not reported (gamm4)", format_num(gamm_num1(sm$scale), 4)),
                      stringsAsFactors = FALSE)
  list(stats = stats, parametric = par, smooth = smt, random = vc, formula = r$formula, family = fam$family, link = fam$link)
}

# Tests of the random effects. mgcv tests each random-effect smooth against zero (Wood 2013; summary re.test = TRUE),
# which can be slow with many levels; gamm4 fits have no such test.
gamm_re_test <- function(r) {
  if (!is.null(r$mer)) return(list(ok = FALSE, message = "This model was fitted with gamm4 (lme4's random effects), for which mgcv's random-effect tests are not available. Read the variance components instead."))
  sm <- tryCatch(suppressWarnings(summary(r$fit, re.test = TRUE)), error = function(e) e)
  if (inherits(sm, "error")) return(list(ok = FALSE, message = paste("The random-effect tests failed:", conditionMessage(sm))))
  x <- as.data.frame(sm$s.table)
  keep <- gamm_is_random_label(rownames(x))
  if (!any(keep)) return(list(ok = FALSE, message = "The model has no random-effect smooths to test."))
  x <- x[keep, , drop = FALSE]
  list(ok = TRUE, table = data.frame(Term = rownames(x), Description = gamm_term_description(rownames(x)), edf = x[[1]],
                                     Statistic = x[[3]], P = x[[4]], stringsAsFactors = FALSE, row.names = NULL))
}

gamm_p_words <- function(p) { f <- format_p(p); if (startsWith(f, "<")) sub("^<", "< ", f) else paste("=", f) }

# A guided reading of the fitted GAMM, question by question, for readers who know parametric models. Each item has a
# question, the answer for this fit, and what to do next. comp: optional results of the shape (poly), group and proxy
# comparisons, used when they have been run.
gamm_guide <- function(r, o, poly = NULL, groups = NULL, proxy = NULL) {
  s <- r$settings
  sm <- o$smooth; pr <- o$parametric
  item <- function(q, a, nxt = "") list(question = q, answer = a, next_step = nxt)
  out <- list()
  # 1. is there an age effect at all?
  age <- sm[grepl("^s\\(age\\)", sm$Smooth) & !grepl("ALR|LS|AFR|grp_o", sm$Smooth), , drop = FALSE]
  if (nrow(age)) {
    sig <- is.finite(age$P) & age$P < 0.05
    a <- if (nrow(age) == 1) {
      if (sig) sprintf("Yes. The smooth of age differs from a flat line (%s = %s, p %s): the trait changes with age.", attr(sm, "stat"), format_num(age$Statistic, 3), gamm_p_words(age$P))
      else sprintf("Not clearly. The smooth of age does not differ significantly from a flat line (p %s). This is absence of evidence: with few old individuals a real decline can be missed.", gamm_p_words(age$P))
    } else sprintf("Group by group: %s. Each p-value tests whether that group's curve (or deviation) differs from a flat line.",
                   paste(sprintf("%s p %s", age$Description, vapply(age$P, gamm_p_words, "")), collapse = "; "))
    pc <- if (is.data.frame(poly) && nrow(poly)) paste(" Formal comparison:", gamm_poly_reading(poly)[1]) else ""
    out[[length(out) + 1]] <- item("1. Does the trait change with age at all?", paste0(a, pc),
      if (is.null(poly)) "Run 'Is a smooth needed?' below: it compares models with no age term, linear, quadratic and cubic age, and the smooth." else "")
  }
  # 2. is a curve needed, or would a straight line or polynomial do?
  if (nrow(age)) {
    e <- age$edf
    a <- paste0(sprintf("The effective degrees of freedom (edf) measure how wiggly the smooth is: about 1 is a straight line, 2\u20133 a gentle curve such as a single peak, above 3 a more complex shape. Here: %s.",
                        paste(sprintf("%s edf %s (%s)", if (nrow(age) > 1) age$Description else "age", format_num(e, 3), age$Shape), collapse = "; ")),
                if (any(e < 1.5)) " An edf near 1 means a linear age effect would do." else "")
    if (is.data.frame(poly) && nrow(poly)) a <- paste(a, paste(gamm_poly_reading(poly)[-1], collapse = " "))
    out[[length(out) + 1]] <- item("2. Is a curve needed, or would a straight line or polynomial do?", a,
      "A significant smooth tells you there is an age effect, not that it is curved: the edf and the comparison with polynomials answer that.")
  }
  # 3. groups
  if (nzchar(s$group %||% "")) {
    st <- r$structure
    desc <- switch(st, G = "one common curve, groups differing only in level", GS = "a common curve plus group deviations that share one wiggliness",
                   GI = "a common curve plus group deviations, each with its own wiggliness", S = "a separate curve per group, sharing one wiggliness",
                   I = "a separate curve per group, each with its own wiggliness")
    gm <- pr[startsWith(pr$Term, s$group), , drop = FALSE]
    lvl <- if (nrow(gm)) paste0(" Overall level: ", paste(sprintf("%s by %s (p %s)", sub(", against the reference level$", " differs from the reference level", gm$Description), format_num(gm$Estimate, 3), vapply(gm$P, gamm_p_words, "")), collapse = "; "), ".") else ""
    a <- paste0(sprintf("This fit uses structure %s: %s.", st, desc), lvl,
                if (st %in% c("GS", "GI")) " With GS and GI the group rows of the smooth table test whether each group's deviation from the common curve is flat: a significant deviation means that group's shape differs." else "",
                if (st %in% c("S", "I")) " With S and I each group has its own curve; its p-value tests whether that group's trait changes with age, not whether the groups differ. Whether the groups age differently is answered by comparing structures." else "")
    if (is.list(groups) && isTRUE(groups$ok)) a <- paste(a, "Comparison:", groups$verdict)
    best <- if (is.list(groups) && isTRUE(groups$ok) && is.data.frame(groups$table) && any(is.finite(groups$table$AIC))) groups$table$Model[which.min(groups$table$AIC)] else NA_character_
    out[[length(out) + 1]] <- item("3. Do the groups age differently, and which group structure fits?", a,
      if (is.null(groups)) "Open the Groups tab and press 'Compare group structures': if G (one shared curve) is within 2 AIC of the best, there is no clear evidence that the groups age differently."
      else if (!is.na(best) && !identical(best, st)) sprintf("The comparison supports structure %s, not the %s fitted here: choose %s under 'Group curves' on the Fit tab and refit, so that the curves and tables describe the supported model.", best, st, best)
      else "")
  }
  # 4. selective disappearance and appearance
  pf <- r$proxy_form %||% "none"; prox <- s$proxy %||% "none"
  if (!identical(prox, "none") || !identical(r$afr_form %||% "none", "none")) {
    lin <- pr[pr$Term %in% c("ALR", "LS", "AFR"), , drop = FALSE]
    sml <- sm[grepl("ALR|LS|AFR", sm$Smooth), , drop = FALSE]
    bits <- c(
      if (nrow(lin)) paste(vapply(seq_len(nrow(lin)), function(i) sprintf("%s: %s per year (p %s), %s", lin$Term[i], format_num(lin$Estimate[i], 3), gamm_p_words(lin$P[i]),
        if (is.finite(lin$P[i]) && lin$P[i] < 0.05) sprintf("so individuals with a %s differ in trait level by the same amount at every age (age-independent selective %s)",
                                                            switch(lin$Term[i], AFR = "later AFR", LS = "longer lifespan", "later last record (ALR)"), if (lin$Term[i] == "AFR") "appearance" else "disappearance")
        else "no clear age-independent effect"), ""), collapse = "; "),
      if (nrow(sml)) paste(vapply(seq_len(nrow(sml)), function(i) sprintf("%s: p %s, %s", sml$Description[i], gamm_p_words(sml$P[i]),
        if (grepl("^ti\\(|:\\.(ALR|LS|AFR)_c$", sml$Smooth[i])) { if (is.finite(sml$P[i]) && sml$P[i] < 0.05) "so the proxy's effect changes with age (age-dependent selection: the naive curve is distorted in shape)" else "no clear evidence that the proxy's effect changes with age" }
        else { if (is.finite(sml$P[i]) && sml$P[i] < 0.05) "a non-linear but age-independent effect" else "no clear effect" }), ""), collapse = "; "))
    out[[length(out) + 1]] <- item("4. Does selective disappearance or appearance shape the curve?",
      paste0(paste(bits[nzchar(bits)], collapse = ". "), "."),
      if (is.null(proxy)) "Open the Selective disappearance tab and press 'Compare proxy forms': it compares no proxy, linear, smooth and age-dependent forms on the same records." else paste("Comparison:", paste(unlist(proxy$texts), collapse = " ")))
  }
  # 5. the pattern
  out[[length(out) + 1]] <- item("5. What is the pattern of ageing?",
    "The curve on the Fit tab shows the shape. The Ageing rate tab says where the trait changes significantly (the first derivative with simultaneous intervals), the peak age and the onset of the final decline.",
    "Describe the pattern from where the derivative's interval excludes zero, not from wiggles in the curve, especially at old ages where few individuals remain.")
  out
}

# Model adequacy, as traffic lights with what each check means and what to do (the GAMM counterpart of step 5's
# fit diagnostics).
gamm_check_summary <- function(r, kc, cc, qr = NULL) {
  s <- r$settings; dd <- r$data
  row <- function(check, result, status, meaning, todo) data.frame(Check = check, Result = result, Status = status, `What it tells you` = meaning,
                                                                   `What to do` = todo, check.names = FALSE, stringsAsFactors = FALSE)
  rows <- list()
  bad_k <- if (nrow(kc)) kc$Smooth[kc$Flag != ""] else character(0)
  rows[[1]] <- row("Basis size (k)", if (!nrow(kc)) "not available" else if (length(bad_k)) paste("may be too small for", paste(bad_k, collapse = ", ")) else sprintf("adequate for %d smooth(s)", nrow(kc)),
    if (length(bad_k)) "caution" else "good",
    "k sets the maximum wiggliness a smooth may have; the penalty then chooses how much is used. The check asks whether the residuals still contain pattern along the term (k-index below 1 with a small p-value) while the edf is close to the maximum.",
    if (length(bad_k)) sprintf("Raise k (now %s) and refit. If the edf barely changes, the original k was fine.", s$k) else "Nothing: a larger k would not change the curve.")
  hi <- if (nrow(cc)) cc[cc$Flag != "", , drop = FALSE] else data.frame()
  rows[[2]] <- row("Concurvity", if (!nrow(cc)) "only one fixed smooth" else if (nrow(hi)) paste("high for", paste(sprintf("%s with %s (%s)", hi$Term, hi[["Most similar term"]], hi$Estimate), collapse = "; ")) else sprintf("largest %s", format_num(max(cc$Estimate), 2)),
    if (nrow(hi)) "caution" else "good",
    "The smooth version of collinearity: how far one term's curve can be reproduced by the other terms. Above 0.8, their separate effects are poorly identified (for example age and ALR when most individuals die young).",
    if (nrow(hi)) "Interpret the terms jointly, not separately; their combined prediction is still fine. Consider a simpler form for one of them (linear instead of smooth)." else "Nothing.")
  binary <- all(dd$trait %in% c(0, 1)) && !(".trials" %in% names(dd) && any(dd$.trials > 1, na.rm = TRUE))
  if (s$family %in% c("poisson", "binomial") && !binary) {
    disp <- if (is.list(qr)) qr$dispersion else NA_real_
    over <- is.list(qr) && is.finite(qr$dispersion_p) && qr$dispersion_p < 0.05 && disp > 1
    rows[[3]] <- row("Dispersion", if (is.finite(disp)) sprintf("ratio %s (p %s)", format_num(disp, 3), gamm_p_words(qr$dispersion_p)) else "not available",
      if (over) "problem" else "good",
      sprintf("Whether the data vary more than the %s family allows. Overdispersion makes intervals too narrow and p-values too small.", s$family),
      if (over) sprintf("Tick 'Observation-level random effect' on the Fit tab, or choose the %s family.", if (s$family == "poisson") "negative binomial" else "beta") else "Nothing.")
  }
  lag1 <- gamm_lag1(gamm_resid(r), dd$id)
  ac <- is.finite(lag1) && lag1 > 0.3 && !(isTRUE(s$ar1) && r$rho > 0)
  rows[[length(rows) + 1]] <- row("Correlation within individuals", if (is.finite(lag1)) sprintf("lag-1 r = %s", format_num(lag1, 2)) else "not available",
    if (ac) "caution" else "good",
    "Whether residuals of the same individual's consecutive records are still correlated after the random effects. If so, individuals age differently and the intervals are too narrow.",
    if (ac) "Use 'Random intercept and slope' or 'Random smooth per individual'; for a Gaussian trait, AR1 errors (Advanced)." else "Nothing.")
  old <- stats::quantile(dd$age, 0.9); n_old <- length(unique(dd$id[dd$age >= old]))
  rows[[length(rows) + 1]] <- row("Data at old ages", sprintf("%d individuals at or above age %s", n_old, format_num(old)), if (n_old < 20) "caution" else "good",
    "How many individuals inform the oldest tenth of the age range. The smooth is least certain where data are sparse.",
    if (n_old < 20) "Do not interpret wiggles or late declines at the oldest ages; the support bars under the curve show where data thin out." else "Nothing.")
  rows[[length(rows) + 1]] <- row("Convergence", if (isTRUE(r$converged)) "converged" else "not fully converged", if (isTRUE(r$converged)) "good" else "problem",
    "Whether the fitting algorithm found the best smoothing parameters.", if (isTRUE(r$converged)) "Nothing." else "Simplify the model (smaller k, fewer random terms, a linear instead of a smooth proxy) and refit.")
  out <- do.call(rbind, rows)
  attr(out, "overall") <- if (any(out$Status == "problem")) "problem" else if (any(out$Status == "caution")) "caution" else "good"
  out
}

# Plain reading of the model output: which terms matter, and how curved the age smooth is.
gamm_output_reading <- function(o) {
  out <- character(0)
  sm <- o$smooth
  if (nrow(sm)) {
    age <- sm[grepl("^s\\(age\\)", sm$Smooth) & !grepl("ALR|LS|AFR|grp_o", sm$Smooth), , drop = FALSE]
    for (i in seq_len(nrow(age))) {
      out <- c(out, sprintf("%s: edf %s (%s), %s = %s, p %s.", age$Description[i], format_num(age$edf[i], 3), age$Shape[i], attr(sm, "stat"),
                            format_num(age$Statistic[i], 3), gamm_p_words(age$P[i])))
    }
  }
  pr <- o$parametric
  prox <- pr[pr$Term %in% c("ALR", "LS", "AFR"), , drop = FALSE]
  for (i in seq_len(nrow(prox))) {
    out <- c(out, sprintf("%s: %s per unit (SE %s), p %s: %s.", prox$Term[i], format_num(prox$Estimate[i], 3), format_num(prox$SE[i], 3),
                          gamm_p_words(prox$P[i]),
                          if (is.finite(prox$P[i]) && prox$P[i] < 0.05) sprintf("individuals with a %s %s have %s trait values at a given age", if (prox$Estimate[i] > 0) "longer" else "shorter",
                                                                                if (prox$Term[i] == "AFR") "AFR" else "lifespan proxy", "higher")
                          else "no clear evidence of age-independent selective (dis)appearance"))
  }
  c(out, "The p-values of smooth terms are approximate (Wood 2013): read them together with the curves, and treat values near 0.05 with caution. Estimates are on the link scale for non-Gaussian families.")
}

# Basis-dimension check: k', edf, k-index and its p-value per smooth (mgcv::k.check). A small p-value with a k-index
# below 1 suggests k is too low, especially when edf is close to k'. Random-effect smooths are not checked.
gamm_kcheck <- function(r) {
  set.seed(1)
  kc <- tryCatch(mgcv::k.check(r$fit, subsample = 5000, n.rep = 200), error = function(e) NULL)
  if (is.null(kc) || !nrow(kc)) return(data.frame())
  out <- data.frame(Smooth = rownames(kc), k = kc[, 1], edf = kc[, 2], k_index = kc[, 3], P = kc[, 4], stringsAsFactors = FALSE)
  out <- out[!grepl("^s\\((id|re_)", out$Smooth) & !grepl(",id\\)$", out$Smooth), , drop = FALSE]
  out$Flag <- ifelse(is.finite(out$P) & out$P < 0.05 & out$k_index < 1 & out$edf > 0.8 * out$k, "k may be too low", "")
  rownames(out) <- NULL
  out
}

# Concurvity (the smooth analogue of collinearity) between the model's fixed terms: for each smooth, the largest
# pairwise concurvity with any other fixed term (parametric terms included) and which term that is. Random effects are
# left out: a proxy that is constant within individuals is always fully mimicked by the individual random intercept,
# which the penalty resolves, so it carries no information. Estimates above 0.8 flag terms that are hard to separate,
# for example age and ALR in short-lived populations.
gamm_concurvity <- function(r) {
  cc <- tryCatch(mgcv::concurvity(r$fit, full = FALSE), error = function(e) NULL)
  if (is.null(cc) || !is.list(cc) || is.null(cc$estimate)) return(data.frame())
  est <- cc$estimate; wst <- cc$worst
  fixed <- colnames(est)[!grepl("^s\\((id|re_)|,id\\)$|\\.age_c", colnames(est))]
  sm <- setdiff(fixed, "para")
  if (!length(sm) || length(fixed) < 2) return(data.frame())
  rows <- lapply(sm, function(t) {
    others <- setdiff(fixed, t)
    j <- others[[which.max(est[others, t])]]
    data.frame(Term = t, `Most similar term` = if (identical(j, "para")) "parametric terms" else j,
               Estimate = round(est[j, t], 3), Worst = round(wst[j, t], 3), check.names = FALSE, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out$Flag <- ifelse(out$Estimate > 0.8, "high", "")
  rownames(out) <- NULL
  out
}

# Smooths that belong to individuals and random effects: set to zero in population curves.
gamm_random_smooths <- function(fit) {
  sm <- fit$smooth
  if (!length(sm)) return(list(labels = character(0), cols = integer(0)))
  is_re <- vapply(sm, function(x) any(x$term %in% c("id", ".age_c", ".obs")) || any(grepl("^re_", x$term)), logical(1))
  labs <- vapply(sm[is_re], function(x) x$label, character(1))
  cols <- unlist(lapply(sm[is_re], function(x) seq(x$first.para, x$last.para)))
  list(labels = labs, cols = as.integer(cols))
}

# Covariance matrix of the coefficients, including smoothing-parameter uncertainty where mgcv provides it.
gamm_vcov <- function(fit) {
  V <- tryCatch(stats::vcov(fit, unconditional = TRUE), error = function(e) NULL, warning = function(w) NULL)
  if (is.null(V) || any(!is.finite(V))) V <- fit$Vp
  V
}

# Prediction grid: ages x a group level, other variables at reference values (numeric covariates at their mean,
# proxies at their mean over individuals unless set in `at`, factors at the commonest level, terminal = "no").
gamm_newdata <- function(r, ages, level = NULL, at = list()) {
  dd <- r$data
  ind <- dd[!duplicated(dd$id), , drop = FALSE]
  nd <- data.frame(age = ages)
  for (v in setdiff(names(dd), c("age", "trait", ".trials"))) {
    x <- dd[[v]]
    nd[[v]] <- if (identical(v, ".terminal")) factor("no", levels = levels(x))
               else if (is.ordered(x)) factor(levels(x)[[1]], levels = levels(x), ordered = TRUE)
               else if (is.factor(x)) factor(names(sort(table(x), decreasing = TRUE))[[1]], levels = levels(x))
               else if (v %in% c("ALR", "LS", "AFR")) mean(ind[[v]], na.rm = TRUE) else mean(x, na.rm = TRUE)
  }
  nd$.age_c <- nd$age - (r$age_mean %||% mean(dd$age))
  grp <- r$settings$group %||% ""
  if (nzchar(grp) && grp %in% names(dd) && !is.null(level)) nd[[grp]] <- factor(level, levels = levels(dd[[grp]]))
  if (".grp_o" %in% names(dd) && !is.null(level)) nd$.grp_o <- factor(level, levels = levels(dd$.grp_o), ordered = TRUE)
  for (v in names(at)) nd[[v]] <- at[[v]]
  for (pp in names(r$centres %||% list())) if (pp %in% names(nd)) nd[[paste0(".", pp, "_c")]] <- nd[[pp]] - r$centres[[pp]]
  nd
}

# Linear-predictor matrix of the population curve (random effects zeroed, first linear predictor only), with
# coefficients and covariance.
gamm_lp <- function(r, nd) {
  X <- stats::predict(r$fit, newdata = nd, type = "lpmatrix")
  lpi <- attr(X, "lpi")
  cols <- if (is.null(lpi)) seq_len(ncol(X)) else lpi[[1]]
  # Random-effect columns are zero on the population curve, so they are dropped rather than zeroed: the result is the
  # same, and draws from the covariance matrix no longer scale with the number of individuals (thousands of columns).
  cols <- setdiff(cols, gamm_random_smooths(r$fit)$cols)
  cm <- r$cov_colmeans %||% numeric(0)
  grp <- r$settings$group %||% ""
  if (length(cm)) cm <- cm[names(cm) %in% colnames(X) & !(nzchar(grp) & startsWith(names(cm), grp))]   # the group is set per curve
  for (nm in names(cm)) X[, nm] <- cm[[nm]]
  V <- gamm_vcov(r$fit)
  list(X = X[, cols, drop = FALSE], b = stats::coef(r$fit)[cols], V = V[cols, cols, drop = FALSE])
}

gamm_linkinv <- function(fit) fit$family$linfo[[1]]$linkinv %||% fit$family$linkinv

# Critical value for simultaneous 95% intervals of X %*% b (Ruppert et al. 2003; Simpson 2018).
gamm_crit <- function(X, V, se, n_sim = 2000, seed = 1L) {
  set.seed(seed)
  B <- mgcv::rmvn(n_sim, rep(0, ncol(X)), V)
  if (is.null(dim(B))) B <- matrix(B, nrow = 1)
  dev <- abs(X %*% t(B)) / pmax(se, 1e-12)
  stats::quantile(apply(dev, 2, max), 0.95, names = FALSE)
}

gamm_levels <- function(r) {
  dd <- r$data; grp <- r$settings$group %||% ""
  if (nzchar(grp) && !identical(r$structure, "G") && grp %in% names(dd)) levels(dd[[grp]]) else list(NULL)
}

gamm_age_grid <- function(r, n = 150) seq(min(r$data$age), max(r$data$age), length.out = n)

# Proxy values at which to draw curves: the mean (additive models) or the 10th, 50th and 90th percentiles over
# individuals (age-dependent models, where the curve's shape depends on the proxy).
gamm_proxy_values <- function(r, n = 3) {
  p <- r$settings$proxy %||% "none"
  if (identical(p, "none") || !r$proxy_form %in% c("ti", "vc") || !p %in% names(r$data)) return(NULL)
  ind <- r$data[!duplicated(r$data$id), , drop = FALSE]
  n <- max(2L, min(5L, as.integer(n %||% 3L)))
  pr <- seq(0.1, 0.9, length.out = n)
  q <- stats::quantile(ind[[p]], pr, na.rm = TRUE, names = FALSE)
  tag <- ifelse(pr == 0.1, " (short-lived)", ifelse(pr == 0.9, " (long-lived)", ifelse(abs(pr - 0.5) < 1e-9, " (median)", sprintf(" (%.0fth percentile)", 100 * pr))))
  stats::setNames(q, paste0(p, " = ", vapply(q, format_num, character(1)), tag))
}

# Population curve(s) with pointwise and simultaneous 95% intervals, on the response scale. One curve per group level;
# for age-dependent proxy models, also one per proxy value (low, median and high).
# Each record's random-effect contribution on the link scale (individual, mapped random terms and, if fitted, the
# observation-level effect): the full linear predictor minus its fixed part.
gamm_re_offsets <- function(r) {
  tryCatch({
    if (!is.null(r$mer)) {
      full <- stats::predict(r$mer, type = "link")
      fixed <- as.numeric(stats::predict(r$fit, newdata = r$data, type = "link"))
    } else {
      X <- stats::predict(r$fit, newdata = r$data, type = "lpmatrix")
      cols <- gamm_random_smooths(r$fit)$cols
      b <- stats::coef(r$fit)
      full <- as.numeric(X %*% b)
      fixed <- as.numeric(X[, -cols, drop = FALSE] %*% b[-cols])
    }
    u <- as.numeric(full) - fixed
    u[is.finite(u)]
  }, error = function(e) 0)
}

# Inverse link for the curve. scale = "typical": random effects at zero (a typical individual, the GAMM's own curve).
# scale = "average": at each age, the prediction averaged over every record's estimated random effects, so it is
# comparable with observed means; it uses the fitted random effects themselves rather than assuming they are normal.
# Identity links are unchanged.
gamm_curve_linkinv <- function(r, scale = "typical") {
  inv <- gamm_linkinv(r$fit)
  lk <- tryCatch(r$fit$family$link %||% r$fit$family$linfo[[1]]$link, error = function(e) "identity")
  if (!identical(scale, "average") || identical(lk, "identity")) return(inv)
  u <- gamm_re_offsets(r)
  if (!length(u) || all(u == 0)) return(inv)
  function(eta) vapply(eta, function(e) mean(inv(e + u)), numeric(1))
}

gamm_curve <- function(r, n = 150, by_proxy = TRUE, scale = "typical", n_proxy = 3) {
  ages <- gamm_age_grid(r, n)
  linkinv <- gamm_curve_linkinv(r, scale)
  pv <- if (by_proxy) gamm_proxy_values(r, n_proxy) else NULL
  p <- r$settings$proxy %||% "none"
  combos <- expand.grid(lv = seq_along(gamm_levels(r)), pv = if (is.null(pv)) NA else seq_along(pv))
  levs <- gamm_levels(r)
  out <- lapply(seq_len(nrow(combos)), function(i) {
    lv <- levs[[combos$lv[[i]]]]
    at <- if (is.na(combos$pv[[i]])) list() else stats::setNames(list(pv[[combos$pv[[i]]]]), p)
    L <- gamm_lp(r, gamm_newdata(r, ages, lv, at))
    eta <- as.numeric(L$X %*% L$b)
    se <- sqrt(pmax(rowSums((L$X %*% L$V) * L$X), 0))
    crit <- gamm_crit(L$X, L$V, se)
    data.frame(age = ages, level = if (is.null(lv)) "All" else as.character(lv),
               proxy = if (is.na(combos$pv[[i]])) "" else names(pv)[[combos$pv[[i]]]],
               fit = linkinv(eta), lower = linkinv(eta - 1.96 * se), upper = linkinv(eta + 1.96 * se),
               lower_sim = linkinv(eta - crit * se), upper_sim = linkinv(eta + crit * se), stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# Numbers of records and individuals at each age (rounded to the data's age step): where the curve is constrained.
gamm_support <- function(r) {
  dd <- r$data
  a <- dd$age
  step <- if (length(unique(a)) > 40) diff(range(a)) / 30 else NA
  ab <- if (is.finite(step)) round(a / step) * step else a
  data.frame(age = sort(unique(ab)), records = as.integer(table(ab)),
             individuals = as.integer(tapply(dd$id, ab, function(z) length(unique(z)))), stringsAsFactors = FALSE)
}

# First derivative of the population curve on the link scale, with pointwise and simultaneous 95% intervals.
# Ageing is significant where the interval excludes zero.
gamm_derivative <- function(r, n = 150) {
  ages <- gamm_age_grid(r, n)
  eps <- 1e-4 * diff(range(r$data$age))
  out <- lapply(gamm_levels(r), function(lv) {
    L0 <- gamm_lp(r, gamm_newdata(r, ages - eps, lv))
    L1 <- gamm_lp(r, gamm_newdata(r, ages + eps, lv))
    D <- (L1$X - L0$X) / (2 * eps)
    d <- as.numeric(D %*% L1$b)
    se <- sqrt(pmax(rowSums((D %*% L1$V) * D), 0))
    crit <- gamm_crit(D, L1$V, se)
    data.frame(age = ages, level = if (is.null(lv)) "All" else as.character(lv), derivative = d,
               lower = d - 1.96 * se, upper = d + 1.96 * se, lower_sim = d - crit * se, upper_sim = d + crit * se,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# Periods of significant change: runs of ages where the derivative's interval lies wholly below zero (decline) or
# above zero (increase).
gamm_periods <- function(dv, simultaneous = TRUE) {
  lo <- if (simultaneous) dv$lower_sim else dv$lower
  hi <- if (simultaneous) dv$upper_sim else dv$upper
  rows <- list()
  for (lv in unique(dv$level)) {
    k <- dv$level == lv
    a <- dv$age[k]; state <- ifelse(hi[k] < 0, "decline", ifelse(lo[k] > 0, "increase", "no clear change"))
    rl <- rle(state); ends <- cumsum(rl$lengths); starts <- c(1, utils::head(ends, -1) + 1)
    for (j in seq_along(rl$values)) if (!identical(rl$values[[j]], "no clear change"))
      rows[[length(rows) + 1]] <- data.frame(Group = lv, Change = rl$values[[j]], From = a[starts[[j]]], To = a[ends[[j]]],
                                             stringsAsFactors = FALSE)
  }
  if (length(rows)) do.call(rbind, rows) else data.frame(Group = character(0), Change = character(0), From = numeric(0), To = numeric(0))
}

# Peak and onset of the final decline per group, with 95% limits from 1,000 draws of the coefficients (the turning-
# point rules of the parametric layer: curve_turning_points()). Turning points are the same on the link and response
# scales, as the links are monotonic.
gamm_peak_onset <- function(r, n_draws = 1000, n = 150, seed = 1L) {
  ages <- gamm_age_grid(r, n)
  set.seed(seed)
  rows <- lapply(gamm_levels(r), function(lv) {
    L <- gamm_lp(r, gamm_newdata(r, ages, lv))
    tp <- curve_turning_points(ages, as.numeric(L$X %*% L$b))
    B <- mgcv::rmvn(n_draws, L$b, L$V)
    sims <- L$X %*% t(B)
    tps <- vapply(seq_len(ncol(sims)), function(j) {
      t <- curve_turning_points(ages, sims[, j])
      c(t$peak, as.numeric(t$peak_interior), t$onset, as.numeric(identical(t$onset_type, "interior")))
    }, numeric(4))
    q <- function(v) if (length(v) >= 20) paste0(format_num(stats::quantile(v, 0.025)), "\u2013", format_num(stats::quantile(v, 0.975))) else ""
    data.frame(Group = if (is.null(lv)) "All" else as.character(lv),
               `Peak age` = if (isTRUE(tp$peak_interior)) format_num(tp$peak) else "none within the sampled ages",
               `Peak 95% CI` = if (isTRUE(tp$peak_interior)) q(tps[1, tps[2, ] == 1]) else "",
               `Draws with an interior peak` = sprintf("%.0f%%", 100 * mean(tps[2, ] == 1)),
               `Onset of final decline` = switch(tp$onset_type, interior = format_num(tp$onset),
                                                 from_start = paste("at or before", format_num(min(ages))),
                                                 no_final_decline = "no final decline", ""),
               `Onset 95% CI` = if (identical(tp$onset_type, "interior")) q(tps[3, tps[4, ] == 1]) else "",
               check.names = FALSE, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# Difference between two groups' curves (link scale) with pointwise and simultaneous 95% intervals. The difference
# includes the groups' difference in level (the group main effect).
gamm_difference <- function(r, n = 150) {
  dd <- r$data
  grp <- r$settings$group %||% ""
  if (!nzchar(grp) || identical(r$structure, "G") || !grp %in% names(dd)) return(data.frame())
  levs <- levels(dd[[grp]])
  if (length(levs) < 2) return(data.frame())
  pairs <- utils::combn(levs, 2, simplify = FALSE)
  if (length(pairs) > 6) pairs <- pairs[1:6]
  ages <- gamm_age_grid(r, n)
  do.call(rbind, lapply(pairs, function(pr) {
    La <- gamm_lp(r, gamm_newdata(r, ages, pr[[1]]))
    Lb <- gamm_lp(r, gamm_newdata(r, ages, pr[[2]]))
    D <- La$X - Lb$X
    d <- as.numeric(D %*% La$b)
    se <- sqrt(pmax(rowSums((D %*% La$V) * D), 0))
    crit <- gamm_crit(D, La$V, se)
    data.frame(age = ages, pair = paste(pr[[1]], "\u2212", pr[[2]]), diff = d, lower = d - 1.96 * se, upper = d + 1.96 * se,
               lower_sim = d - crit * se, upper_sim = d + crit * se, stringsAsFactors = FALSE)
  }))
}

# ---------------------------------------------------------------------------
# Comparisons (each fits all its models by ML to the same records)
# ---------------------------------------------------------------------------

gamm_compare_table <- function(fits, labels) {
  tab <- data.frame(Model = labels, AIC = vapply(fits, gamm_aic, numeric(1)),
                    edf = vapply(fits, function(r) if (isTRUE(r$ok)) sum(r$fit$edf) else NA_real_, numeric(1)),
                    `Deviance explained (%)` = vapply(fits, function(r) if (isTRUE(r$ok)) tryCatch(100 * gamm_num1(summary(r$fit, re.test = FALSE)$dev.expl), error = function(e) NA_real_) else NA_real_, numeric(1)),
                    Status = vapply(fits, function(r) if (isTRUE(r$ok)) "fitted" else r$message %||% "failed", character(1)),
                    check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL)
  tab$dAIC <- tab$AIC - suppressWarnings(min(tab$AIC, na.rm = TRUE))
  tab$Weight <- if (any(is.finite(tab$dAIC))) { w <- exp(-0.5 * tab$dAIC); w / sum(w, na.rm = TRUE) } else NA_real_
  tab[, c("Model", "AIC", "dAIC", "Weight", "edf", "Deviance explained (%)", "Status")]
}

gamm_lrt <- function(a, b, label) {
  if (!isTRUE(a$ok) || !isTRUE(b$ok)) return(NULL)
  if (!is.null(a$mer) && !is.null(b$mer)) return(tryCatch({          # gamm4: likelihood-ratio test of the ML lme4 fits
    x <- suppressMessages(stats::anova(a$mer, b$mer, refit = FALSE))
    data.frame(Comparison = label, `Deviance difference` = abs(diff(x$deviance)), df = abs(diff(x$npar)), P = x[[ncol(x)]][[2]], check.names = FALSE)
  }, error = function(e) NULL))
  tryCatch({
    x <- stats::anova(a$fit, b$fit, test = "Chisq")
    data.frame(Comparison = label, `Deviance difference` = x$Deviance[[2]], df = x$Df[[2]], P = x[[ncol(x)]][[2]], check.names = FALSE)
  }, error = function(e) NULL)
}

# Do groups age differently? G, GS, GI, S and I by ML on the same records, compared by AIC; ordered-factor difference
# smooths (does each group's trajectory differ in shape from the reference group's?); and a likelihood-ratio test of
# G (one shape, groups differ in level) against I (a separate curve per group).
gamm_group_comparison <- function(gd, s, progress = NULL) {
  s <- utils::modifyList(gamm_defaults(), s)
  if (!nzchar(s$group %||% "") || !s$group %in% names(gd)) return(list(ok = FALSE, message = "Choose a grouping variable first."))
  structs <- GAMM_STRUCTURES
  # with many random-effect levels, G, GI and I are fitted with gamm4 (fast); GS and S need mgcv (sz and linked smooths)
  fast <- identical(gamm_set_engine(gd, s, lapply(c("G", "GI", "I"), function(st) list(structure = st)), fast_ok = TRUE), "gamm4")
  s$engine <- if (fast) "gamm4" else gamm_set_engine(gd, s, c(lapply(structs, function(st) list(structure = st)), list(list(structure = "I"))))
  rows <- gamm_common_rows(gd, lapply(structs, function(st) gamm_formula(s, gd, structure = st)), s$family)
  fits <- list()
  for (i in seq_along(structs)) {
    if (is.function(progress)) progress(i, length(structs) + 1, names(structs)[[i]])
    fits[[structs[[i]]]] <- if (fast && structs[[i]] %in% c("GS", "S"))
      list(ok = FALSE, message = "not fitted (gamm4 cannot fit it; choose the bam engine to include it)")
    else gamm_fit(gd, s, method = "ML", structure = structs[[i]], rows = rows)
  }
  tab <- gamm_compare_table(fits, names(structs))
  tab$Model <- unname(structs)
  tab$Description <- sub("^[A-Z]+: ", "", names(structs))
  tab <- tab[order(tab$AIC), c("Model", "Description", "AIC", "dAIC", "Weight", "edf", "Deviance explained (%)", "Status")]
  if (is.function(progress)) progress(length(structs) + 1, length(structs) + 1, "difference smooths")
  od <- gamm_fit(gd, s, method = s$method, structure = "I", ordered_by = TRUE, rows = rows)
  diff_tab <- if (isTRUE(od$ok)) tryCatch({
    st <- gamm_tables(od)$smooth
    st[grepl(".grp_o", st$Smooth, fixed = TRUE), , drop = FALSE]
  }, error = function(e) data.frame()) else data.frame()
  if (nrow(diff_tab)) diff_tab$Smooth <- sub("s(age):.grp_o", "shape difference: ", diff_tab$Smooth, fixed = TRUE)
  best <- tab$Model[[1]]
  close <- tab$Model[is.finite(tab$dAIC) & tab$dAIC < 2]
  verdict <- if (identical(close, "G") || (best == "G")) "One curve (G) is best supported: no clear evidence that the groups age differently."
             else if ("G" %in% close) sprintf("%s is best supported, but G (one curve) is within 2 AIC: weak evidence that the groups age differently.", best)
             else sprintf("%s is best supported and G is more than 2 AIC worse: the groups appear to age differently.", best)
  if (fast) verdict <- paste(verdict, "(Fitted with gamm4 because of the many random-effect levels; structures GS and S need mgcv and were not fitted.)")
  list(ok = TRUE, table = tab, differences = diff_tab, reference = if (isTRUE(od$ok)) levels(od$data$.grp_o)[[1]] else "",
       lrt = gamm_lrt(fits$G, fits$I, "G (one shape) vs I (a curve per group)"), verdict = verdict, n = sum(rows), engine = s$engine, fast = fast)
}

# Selective disappearance and appearance: ML comparison, on the same records, of no proxy, a linear, a smooth and an
# age-dependent (ti) proxy; then the same for AFR. Returns a verdict for the summary page.
gamm_proxy_comparison <- function(gd, s, progress = NULL) {
  s <- utils::modifyList(gamm_defaults(), s)
  out <- list(ok = TRUE, tables = list(), verdicts = character(0), texts = character(0))
  sets <- list()
  if (!identical(s$proxy, "none")) sets[[s$proxy]] <- "proxy"
  if (any(is.finite(gd$AFR)) && length(unique(gd$AFR[is.finite(gd$AFR)])) >= 3) sets[["AFR"]] <- "afr"
  if (!length(sets)) return(list(ok = FALSE, message = "Choose ALR or lifespan as the proxy first (or map AFR on the Data tab)."))
  # Comparisons are fitted by ML, where mgcv cannot discretise and stores random effects as dense columns, so with many
  # random-effect levels they are fitted with gamm4 (lme4's sparse random effects). gamm4 cannot fit ti(), so the
  # age-dependent form is then the varying-coefficient smooth s(age, by = proxy) (vc), which is linear in the proxy.
  vc_specs <- lapply(c("none", "linear", "smooth", "vc"), function(fm) list(proxy_form = fm, afr_form = fm))
  eng_fast <- gamm_set_engine(gd, s, vc_specs, fast_ok = TRUE)
  fast <- identical(eng_fast, "gamm4")
  forms <- c("none", "linear", "smooth", if (fast) "vc" else "ti")
  s$engine <- if (fast) "gamm4" else gamm_set_engine(gd, s, c(lapply(forms, function(fm) list(proxy_form = fm)), lapply(forms, function(fm) list(afr_form = fm))))
  out$engine <- s$engine
  out$fast <- fast
  step <- 0; total <- length(sets) * length(forms)
  for (p in names(sets)) {
    args_for <- function(form) if (identical(sets[[p]], "proxy")) list(proxy_form = form, afr_form = s$afr_form) else list(proxy_form = s$proxy_form, afr_form = form)
    fs_ <- lapply(forms, function(fm) do.call(gamm_formula, c(list(s = s, gd = gd), args_for(fm))))
    rows <- gamm_common_rows(gd, fs_, s$family)
    fits <- lapply(forms, function(fm) {
      step <<- step + 1
      if (is.function(progress)) progress(step, total, paste(p, fm))
      do.call(gamm_fit, c(list(gd = gd, s = s, method = "ML", rows = rows), args_for(fm)))
    })
    names(fits) <- forms
    labs <- c(paste("No", p, "term"), paste("Linear", p), paste("Smooth", p),
              if (fast) paste0("Age-dependent ", p, ": s(age, by = ", p, ")") else paste0("Age-dependent ", p, ": ti(age, ", p, ")"))
    tab <- gamm_compare_table(fits, labs)
    tab$Records <- sum(rows)
    out$tables[[p]] <- tab
    out$fits[[p]] <- lapply(fits, function(r) if (isTRUE(r$ok)) tryCatch(gamm_tables(r)$smooth, error = function(e) NULL) else NULL)
    d <- stats::setNames(tab$dAIC, c("none", "linear", "smooth", "ad"))
    v <- if (!is.finite(d[["ad"]]) || !is.finite(d[["none"]])) "caution"
         else if (d[["ad"]] == 0 && min(d[c("none", "linear", "smooth")], na.rm = TRUE) >= 2) "age_dependent"
         else if (d[["none"]] >= 2) "age_independent" else "none"
    out$verdicts[[p]] <- v
    out$texts[[p]] <- switch(v,
      age_dependent = sprintf("Age-dependent %s term best supported (other forms at least 2 AIC worse): the association between %s and the trait changes with age.", p, p),
      age_independent = sprintf("A %s term is supported (no-%s model at least 2 AIC worse), but not an age-dependent one: consistent with age-independent selective %s.", p, p, if (p == "AFR") "appearance" else "disappearance"),
      none = sprintf("The model without %s is within 2 AIC of the best: no clear evidence of selective %s through %s.", p, if (p == "AFR") "appearance" else "disappearance", p),
      sprintf("The %s comparison could not be completed.", p))
    if (fast) out$texts[[p]] <- paste(out$texts[[p]], sprintf("(Fitted with gamm4 because of the many random-effect levels; the age-dependent form is s(age, by = %s), linear in %s.)", p, p))
  }
  out
}

# Naive and corrected population curves on the same records: the curve without any proxy term (what a naive analysis
# shows) against the fitted model's curve (proxy at its mean), to show how much selective disappearance moves it.
gamm_naive_vs_corrected <- function(gd, r) {
  s <- r$settings
  if (identical(s$proxy, "none") && identical(r$afr_form, "none")) return(NULL)
  rows <- rep(FALSE, nrow(gd))
  key <- paste(gd$id, gd$age, gd$trait)
  rows[key %in% paste(r$data$id, r$data$age, r$data$trait)] <- TRUE
  s$engine <- r$engine
  naive <- gamm_fit(gd, s, method = r$method, structure = r$structure, proxy_form = "none", afr_form = "none", rows = rows)
  if (!isTRUE(naive$ok)) return(NULL)
  a <- gamm_curve(naive, by_proxy = FALSE); a$model <- "Without proxy terms (population curve)"
  b <- gamm_curve(r, by_proxy = FALSE); b$model <- "With proxy terms (within-individual curve)"
  rbind(a, b)
}

# Is a smooth needed? The same model with linear, quadratic and cubic age against the smooth (ML, same records).
gamm_parametric_reference <- function(gd, s) {
  s <- utils::modifyList(gamm_defaults(), s)
  # one common age effect for every model (structure G: the group main effect stays), so the smooth is compared with the
  # polynomials like for like; whether groups need their own curves is the Groups comparison
  s$structure <- "G"
  polys <- list(`No age effect` = -1L, `Linear age` = 1L, `Quadratic age` = 2L, `Cubic age` = 3L, `Smooth of age` = 0L)
  s$engine <- gamm_set_engine(gd, s, list(list()), fast_ok = TRUE)
  rows <- gamm_common_rows(gd, lapply(polys, function(p) gamm_formula(s, gd, age_poly = p)), s$family)
  fits <- lapply(polys, function(p) gamm_fit(gd, s, method = "ML", age_poly = p, rows = rows))
  tab <- gamm_compare_table(fits, names(polys))
  out <- tab[order(tab$AIC), c("Model", "AIC", "dAIC", "Weight", "edf"), drop = FALSE]
  attr(out, "engine") <- s$engine
  out
}

# What the shape comparison says, in the order a reader needs it: is there an age effect at all, is it curved, and
# does the smooth describe it better than a polynomial (the functions of step 5).
gamm_poly_reading <- function(tab) {
  if (!is.data.frame(tab) || !nrow(tab) || !all(c("Model", "AIC") %in% names(tab))) return("The comparison could not be fitted.")
  a <- stats::setNames(tab$AIC, tab$Model)
  best <- tab$Model[which.min(tab$AIC)]
  d <- function(m) if (m %in% names(a)) a[[m]] - min(a, na.rm = TRUE) else NA_real_
  out <- character(0)
  if (is.finite(d("No age effect"))) {
    out <- c(out, if (d("No age effect") > 2) sprintf("Age matters: the model without any age term is %s AIC worse than the best.", format_num(d("No age effect"), 3))
                  else "No clear age effect: the model without an age term is within 2 AIC of the best, so these data give little evidence that the trait changes with age.")
  }
  polys <- intersect(c("Linear age", "Quadratic age", "Cubic age"), names(a))
  bp <- polys[which.min(a[polys])]
  if (length(bp) && is.finite(d("Smooth of age"))) {
    gap <- a[[bp]] - a[["Smooth of age"]]
    out <- c(out, if (gap > 2) sprintf("The smooth fits better than the best polynomial (%s) by %s AIC: the ageing shape has features a simple function misses, so describe it with the GAMM (and read the step-5 models' shape with caution).", tolower(bp), format_num(gap, 3))
                  else if (gap < -2) sprintf("%s fits better than the smooth by %s AIC: a simple function describes the shape, so the parametric models of step 5 are adequate for it.", bp, format_num(-gap, 3))
                  else sprintf("The smooth and %s fit about equally well (within 2 AIC): the simpler function describes the shape adequately, and the two approaches should agree.", tolower(bp)))
  }
  c(out, sprintf("Best supported: %s. All models were fitted by maximum likelihood to the same records, with one age effect shared by all groups; a difference of less than 2 AIC is not a meaningful difference.", best))
}

# Random-effect standard deviations (mgcv::gam.vcomp).
gamm_vcomp <- function(r) {
  if (!is.null(r$mer)) {
    vc <- as.data.frame(lme4::VarCorr(r$mer))
    vc <- vc[!grepl("^Xr", vc$grp), , drop = FALSE]                        # gamm4's smooth penalties are not random effects of the data
    grp <- sub("\\.[0-9]+$", "", vc$grp)
    return(data.frame(Term = ifelse(vc$grp == "Residual", "scale", paste(grp, ifelse(vc$var1 == ".age_c", "slope of age", "intercept"))), SD = vc$sdcor, Lower = NA_real_, Upper = NA_real_,
                      stringsAsFactors = FALSE))
  }
  vc <- NULL
  utils::capture.output(vc <- tryCatch(suppressWarnings(mgcv::gam.vcomp(r$fit, rescale = FALSE)), error = function(e) NULL))
  if (is.null(vc)) return(data.frame())
  m <- if (is.list(vc)) vc$all else vc
  if (is.null(dim(m))) return(data.frame(Term = names(m), SD = as.numeric(m), Lower = NA_real_, Upper = NA_real_))
  out <- data.frame(Term = rownames(m), SD = m[, 1], Lower = if (ncol(m) >= 3) m[, 2] else NA_real_,
                    Upper = if (ncol(m) >= 3) m[, 3] else NA_real_, stringsAsFactors = FALSE, row.names = NULL)
  out[grepl("id|re_|scale", out$Term), , drop = FALSE]
}

# Individual trajectories predicted with every term (random effects included) for a sample of individuals.
gamm_individual_curves <- function(r, n_ind = 12, seed = 1L) {
  dd <- r$data
  set.seed(seed)
  per <- table(dd$id)
  ids <- names(per)[per >= 3]
  if (!length(ids)) return(data.frame())
  pick <- sample(ids, min(n_ind, length(ids)))
  do.call(rbind, lapply(pick, function(i) {
    di <- dd[dd$id == i, , drop = FALSE]
    ages <- seq(min(di$age), max(di$age), length.out = 30)
    nd <- di[rep(1, length(ages)), , drop = FALSE]
    nd$age <- ages; nd$.age_c <- ages - r$age_mean
    if (".terminal" %in% names(nd)) nd$.terminal <- factor("no", levels = levels(dd$.terminal))
    p <- if (!is.null(r$mer)) tryCatch({
      eta <- as.numeric(stats::predict(r$fit, newdata = nd, type = "link"))
      re <- lme4::ranef(r$mer)
      for (nm in grep("^id(\\.|$)", names(re), value = TRUE)) {
        b <- re[[nm]][i, , drop = FALSE]
        if ("(Intercept)" %in% names(b)) eta <- eta + b[["(Intercept)"]]
        if (".age_c" %in% names(b)) eta <- eta + b[[".age_c"]] * nd$.age_c
      }
      gamm_linkinv(r$fit)(eta)
    }, error = function(e) rep(NA_real_, length(ages))) else
      tryCatch(stats::predict(r$fit, newdata = nd, type = "response", exclude = if (".obs" %in% names(nd)) "s(.obs)" else NULL),
               error = function(e) rep(NA_real_, length(ages)))
    if (is.matrix(p)) p <- p[, 1]
    data.frame(id = i, age = ages, fit = as.numeric(p), stringsAsFactors = FALSE)
  }))
}

# Simulated responses from the fitted model (conditional on the random effects), for quantile residuals.
gamm_simulate <- function(r, nsim = 250, seed = 1L) {
  fit <- r$fit; fam <- r$settings$family
  set.seed(seed)
  mu <- gamm_fitted(r)
  n <- length(mu)
  wt <- if (!is.null(r$mer)) { w <- r$data$.trials; if (is.null(w) || !all(is.finite(w))) rep(1, n) else w } else fit$prior.weights
  if (!is.null(r$mer)) fit$sig2 <- stats::sigma(r$mer)^2
  sim1 <- switch(fam,
    gaussian = function() stats::rnorm(n, mu, sqrt(fit$sig2 / wt)),
    Gamma = function() { shp <- 1 / fit$sig2; stats::rgamma(n, shape = shp, rate = shp / mu) },
    poisson = function() stats::rpois(n, mu),
    binomial = function() { w <- pmax(round(wt), 1); stats::rbinom(n, w, mu) / w },
    NULL)
  if (is.null(sim1) && is.function(fit$family$rd)) {
    sim1 <- function() fit$family$rd(mu = stats::fitted(fit), wt = wt, scale = fit$sig2)
  }
  if (is.null(sim1)) return(NULL)
  out <- tryCatch(replicate(nsim, as.numeric(sim1())), error = function(e) NULL)
  if (is.null(out) || !is.matrix(out) || nrow(out) != n) return(NULL)
  out
}

# Randomised quantile residuals from simulation (as in DHARMa; Dunn and Smyth 1996): uniform on 0-1 when the model fits.
# Also returns tests of uniformity (Kolmogorov-Smirnov) and dispersion (simulated vs observed variance of residuals).
gamm_quantile_residuals <- function(r, nsim = 250, seed = 1L) {
  sims <- gamm_simulate(r, nsim, seed)
  if (is.null(sims)) return(NULL)
  y <- r$data$trait
  set.seed(seed + 1)
  lo <- rowMeans(sims < y); hi <- rowMeans(sims <= y)
  u <- lo + stats::runif(length(y)) * (hi - lo)
  u <- pmin(pmax(u, 1 / (2 * nsim)), 1 - 1 / (2 * nsim))
  mu <- gamm_fitted(r)
  obs_disp <- sum((y - mu)^2); sim_disp <- colSums((sims - mu)^2)
  disp_ratio <- obs_disp / mean(sim_disp)
  disp_p <- 2 * min(mean(sim_disp >= obs_disp), mean(sim_disp <= obs_disp))
  ks <- suppressWarnings(stats::ks.test(u, "punif"))
  list(u = u, fitted = mu, age = r$data$age, ks_p = ks$p.value, dispersion = disp_ratio, dispersion_p = min(1, disp_p))
}

# Residual diagnostics: deviance residuals against fitted values and age, and lag-1 autocorrelation within individuals.
gamm_residuals <- function(r) {
  data.frame(fitted = gamm_fitted(r), residual = gamm_resid(r), age = r$data$age, id = r$data$id)
}

# Automatic cautions for the fitted model (short, one line each).
gamm_cautions <- function(r, kc = gamm_kcheck(r), cc = gamm_concurvity(r), qr = NULL, gd = NULL) {
  out <- character(0)
  s <- r$settings
  dd <- r$data
  if (nrow(kc) && any(kc$Flag != "")) out <- c(out, sprintf("Basis may be too small for %s: raise k and refit.", paste(kc$Smooth[kc$Flag != ""], collapse = ", ")))
  if (nrow(cc) && any(cc$Flag != "")) out <- c(out, sprintf("High concurvity (> 0.8) of %s: these terms can mimic each other, so their separate effects are poorly identified.", paste(sprintf("%s with %s", cc$Term, cc[["Most similar term"]])[cc$Flag != ""], collapse = "; ")))
  old <- stats::quantile(dd$age, 0.9)
  n_old <- length(unique(dd$id[dd$age >= old]))
  if (n_old < 20) out <- c(out, sprintf("Only %d individuals have records at or above age %s: the curve is weakly constrained at the oldest ages.", n_old, format_num(old)))
  if (identical(s$individual, "none")) out <- c(out, "No individual random effect: repeated records are treated as independent, so the intervals are too narrow.")
  if (identical(s$individual, "fs") && stats::median(table(dd$id)) < 4) out <- c(out, "A random smooth per individual needs several records per individual; most individuals have fewer than 4, so consider a random intercept and slope.")
  if (identical(s$proxy %||% "none", "none") && identical(r$afr_form %||% "none", "none"))
    out <- c(out, "No lifespan or AFR term: the curve is the population trajectory, which selective disappearance and appearance can distort.")
  if (identical(s$proxy, "LS") && !is.null(gd)) {
    n_unk <- length(unique(gd$id[!is.finite(gd$LS)]))
    if (n_unk > 0) out <- c(out, sprintf("%d individual(s) without a known lifespan (for example still alive) were left out. If they differ from the rest, this biases the curve; ALR keeps everyone.", n_unk))
  }
  if (isTRUE(s$terminal) && !is.null(gd) && (attr(gd, "n_dead_known") %||% 0) < length(unique(gd$id)))
    out <- c(out, "The terminal-record term treats the last record of individuals not known to be dead as non-terminal: map lifespan or censoring for a clean terminal effect.")
  if (nzchar(s$group %||% "")) {
    nind <- tapply(dd$id, dd[[s$group]], function(z) length(unique(z)))
    if (any(nind < 15)) out <- c(out, sprintf("Some groups have few individuals (%s): their curves are uncertain.", paste(names(nind), nind, sep = " ", collapse = ", ")))
  }
  lag1 <- gamm_lag1(gamm_resid(r), dd$id)
  if (is.finite(lag1) && lag1 > 0.3 && !(isTRUE(s$ar1) && r$rho > 0))
    out <- c(out, sprintf("Residuals of consecutive records of the same individual are correlated (lag-1 r = %s): consider a random smooth per individual or, for a Gaussian trait, AR1 errors.", format_num(lag1, 2)))
  # Overdispersion only, and only for count and proportion families: simulation conditional on the fitted random effects
  # makes even a correct model look slightly underdispersed, and dispersion is undefined for 0/1 data.
  binary <- all(dd$trait %in% c(0, 1)) && !(".trials" %in% names(dd) && any(dd$.trials > 1, na.rm = TRUE))
  if (!is.null(qr) && s$family %in% c("poisson", "binomial") && !binary && is.finite(qr$dispersion_p) && qr$dispersion_p < 0.05 && qr$dispersion > 1)
    out <- c(out, sprintf("Simulated residuals suggest overdispersion (ratio %s): add an observation-level random effect, or try the %s family.", format_num(qr$dispersion, 2), if (s$family == "poisson") "negative binomial or Tweedie" else "beta"))
  if (!isTRUE(r$converged)) out <- c(out, "The fit did not report full convergence: simplify the model (smaller k, fewer random terms) and refit.")
  other <- setdiff(r$notes, grep("left out|lowered|do not vary|one level", r$notes, value = TRUE))
  if (length(other)) out <- c(out, paste("Fitting notes:", paste(utils::head(other, 3), collapse = "; ")))
  out
}

# Plain-language reading of the fitted model (one sentence per point), for the top of the results.
gamm_reading <- function(r, dv, po) {
  tb <- gamm_tables(r)
  st <- tb$smooth
  age_rows <- st[grepl("^s\\(age\\)|^s\\(age\\):", st$Smooth), , drop = FALSE]
  out <- character(0)
  for (i in seq_len(nrow(age_rows))) {
    e <- age_rows$edf[[i]]
    lab <- if (nrow(age_rows) > 1) sub("^s\\(age\\):", "", age_rows$Smooth[[i]]) else ""
    out <- c(out, sprintf("The age smooth%s has %s effective degrees of freedom, so it %s, and it %s.", if (nzchar(lab)) paste0(" for ", lab) else "",
                          format_num(e, 3),
                          if (e < 1.5) "is close to a straight line" else if (e < 3) "is gently curved" else "is clearly non-linear",
                          if (is.finite(age_rows$P[[i]]) && age_rows$P[[i]] < 0.05) "differs significantly from flat" else "does not differ significantly from flat"))
  }
  pr <- gamm_periods(dv, simultaneous = TRUE)
  if (nrow(pr)) {
    out <- c(out, paste0("Significant change (simultaneous 95% intervals): ",
                         paste(sprintf("%s%s from %s to %s", ifelse(pr$Group == "All", "", paste0(pr$Group, ": ")), pr$Change,
                                       vapply(pr$From, format_num, character(1)), vapply(pr$To, format_num, character(1))), collapse = "; "), "."))
  } else out <- c(out, "No period of significant change with simultaneous 95% intervals.")
  if (is.finite(tb$dev_expl)) out <- c(out, sprintf("Deviance explained: %.1f%%.", 100 * tb$dev_expl))
  out
}

# ---------------------------------------------------------------------------
# R code
# ---------------------------------------------------------------------------

# R code that refits the model with mgcv on the downloaded GAMM data, with checks and derivatives (gratia).
gamm_code <- function(r, file = "disappR_gamm_data.csv") {
  s <- r$settings
  f <- if (identical(s$family, "gaulss")) sprintf("list(%s, ~ %s)", r$formula, gamm_age_smooth(s, k = min(s$k, 6L))) else r$formula
  wt <- if (identical(s$family, "binomial") && ".trials" %in% names(r$data) && all(is.finite(r$data$.trials))) ", weights = .trials" else ""
  fam <- gamm_family_code(s$family)
  facs <- names(r$data)[vapply(r$data, is.factor, logical(1))]
  fitline <- if (identical(r$engine, "gamm4")) {
    parts <- strsplit(r$formula, "; random = ", fixed = TRUE)[[1]]
    sprintf("g4 <- gamm4::gamm4(%s,\n                    random = %s, family = %s, data = gd, REML = %s%s)\nfit <- g4$gam   # the smooth part; g4$mer holds the random effects",
            parts[[1]], parts[[2]], fam, if (identical(r$method, "ML")) "FALSE" else "TRUE", wt)
  } else if (identical(r$engine, "bam")) {
    sprintf("fit <- bam(%s,\n           family = %s, data = gd, method = \"%s\", discrete = %s%s%s)", f, fam,
            if (identical(r$method, "ML")) "ML" else "fREML", if (identical(r$method, "ML")) "FALSE" else "TRUE", wt,
            if (isTRUE(r$rho > 0)) sprintf(", rho = %s, AR.start = !duplicated(gd$id)", format_num(r$rho, 4)) else "")
  } else {
    sprintf("fit <- gam(%s,\n           family = %s, data = gd, method = \"%s\", select = %s%s)", f, fam, r$method,
            if (isTRUE(s$select)) "TRUE" else "FALSE", wt)
  }
  c("# GAMM of ageing, as fitted in disappR (step 7). Packages: mgcv; gratia for derivatives and plots (optional).",
    "library(mgcv)",
    sprintf("gd <- read.csv(\"%s\")   # downloaded from the GAMM tab: the records this model used", file),
    "gd <- gd[order(gd$id, gd$age), ]",
    if (length(facs)) sprintf("for (v in c(%s)) gd[[v]] <- factor(gd[[v]])", paste0("\"", facs, "\"", collapse = ", ")),
    if (".terminal" %in% facs) "gd$.terminal <- factor(gd$.terminal, levels = c(\"no\", \"yes\"))",
    if (grepl(".age_c", r$formula, fixed = TRUE)) "gd$.age_c <- gd$age - mean(gd$age)   # centred age for the random slope",
    if (grepl(".obs", r$formula, fixed = TRUE)) "gd$.obs <- factor(seq_len(nrow(gd)))   # one level per record: observation-level random effect",
    fitline,
    "summary(fit)",
    "k.check(fit)                       # basis dimension: k-index well below 1 with a small p-value means k is too low",
    "concurvity(fit, full = TRUE)",
    "plot(fit, pages = 1, shade = TRUE, seWithMean = TRUE)",
    "",
    "# Optional: derivatives of the age smooth with simultaneous intervals, and model checks (gratia)",
    "if (requireNamespace(\"gratia\", quietly = TRUE)) {",
    "  d1 <- gratia::derivatives(fit, select = \"s(age)\", interval = \"simultaneous\", unconditional = TRUE)",
    "  print(gratia::draw(d1))",
    "  print(gratia::appraise(fit))",
    "}",
    "# Note: disappR's population curve sets random effects to zero and other terms to reference values",
    "# (numeric covariates and proxies at their mean, factors at their commonest level); gratia's partial",
    "# effect of s(age) differs from it only by a constant for additive models.")
}
