# ---------------------------------------------------------------------------
# Effect sizes and a permutation test for selective disappearance.
#
# AIC says which model the data prefer. It does not say how much the correction changed the answer, and
# it does not say whether a preference of that size could have arisen with no selection at all. These two
# functions answer those questions: selection_effect_sizes() reports the size of the correction on the raw
# trait scale, and permutation_test() builds the null distribution by breaking the link between each
# individual's proxy value and its own records.
# ---------------------------------------------------------------------------

# Coefficient of one term of one model, on the original (unstandardised) scale, with its SE.
raw_coef <- function(res, model, term) {
  cf <- res$coefficients
  if (is.null(cf) || !nrow(cf)) return(NULL)
  x <- cf[cf$Model == model_label(model) & cf$Raw_term == term, , drop = FALSE]
  if (!nrow(x)) return(NULL)
  sc <- tryCatch(term_scaling(term, res), error = function(e) NULL)
  f <- 1
  if (is.data.frame(sc) && "Scale_factor" %in% names(sc) && nrow(sc)) {
    v <- suppressWarnings(as.numeric(sc$Scale_factor[[1]]))
    if (is.finite(v) && v != 0) f <- v
  }
  list(est = x$Estimate[[1]] / f, se = x$SE[[1]] / f, p = x$P_value[[1]])
}

# The age term of an ageing function, on the raw scale: the slope at the mean age.
age_slope <- function(res, model) {
  ap <- res$age_params
  b <- raw_coef(res, model, "f1")
  if (is.null(b)) return(NULL)
  b
}

#' Effect sizes for selective disappearance
#'
#' Three quantities on the trait's own scale: how far the estimated ageing slope moves when the proxy is
#' added, how much of the trait an extra unit of lifespan buys, and how different the ageing rates of
#' short- and long-lived individuals are.
#'
#' @param res A fitted model suite from \code{fit_model_suite()}.
#' @param dat The standardised data, used for the lifespan spread the age-dependent effect is expressed over.
#' @return A data frame of effect sizes with 95% intervals, or an empty data frame.
selection_effect_sizes <- function(res, dat = NULL) {
  empty <- data.frame()
  if (is.null(res) || !isTRUE(res$ok) || is.null(res$coefficients) || !nrow(res$coefficients)) return(empty)
  fitted_models <- unique(res$aic$Model)
  has <- function(m) model_label(m) %in% fitted_models
  rows <- list()
  add <- function(effect, est, se, unit, note) {
    if (is.null(est) || !is.finite(est)) return(invisible(NULL))
    rows[[length(rows) + 1]] <<- data.frame(
      Effect = effect, Estimate = est,
      Lower = est - 1.96 * se, Upper = est + 1.96 * se,
      Unit = unit, Note = note, stringsAsFactors = FALSE)
  }
  b1 <- if (has("M1")) age_slope(res, "M1") else NULL
  # the coefficients are on the link scale: trait units only for Gaussian models
  scale_word <- if (identical(res$family, "gaussian")) "trait" else if (isTRUE(res$family %in% c(BINOMIAL_FAMILIES, "beta"))) "log-odds of the trait" else "log expected trait"

  # 1. How much the ageing slope moves once selective disappearance is corrected for.
  for (m in c("M2", "M4", "M6")) {
    if (!has(m) || is.null(b1)) next
    bm <- age_slope(res, m)
    if (is.null(bm)) next
    d <- bm$est - b1$est
    se <- sqrt(bm$se^2 + b1$se^2)   # conservative: the two fits share data, so this interval is wide
    add(paste0("Change in ageing slope, Model 1 to ", model_label(m)), d, se,
        paste(scale_word, "per unit age"),
        sprintf("uncorrected %.4g, corrected %.4g%s", b1$est, bm$est,
                if (abs(b1$est) > 1e-12) sprintf(" (%+.0f%%)", 100 * d / abs(b1$est)) else ""))
  }

  # 2. The level effect of the proxy: trait difference per unit of lifespan.
  for (m in c("M2", "M4")) {
    if (!has(m)) next
    b <- raw_coef(res, m, "ALR")
    if (is.null(b)) next
    add(paste0("Trait per unit ALR (", model_label(m), ")"), b$est, b$se, paste(scale_word, "per unit ALR"),
        "how much higher the trait is in individuals observed one unit longer")
  }

  # 3. The age-dependent effect, expressed over the lifespan spread actually present in the data:
  #    the difference in ageing rate between a short-lived and a long-lived individual.
  if (has("M4")) {
    b <- raw_coef(res, "M4", "f1:ALR")
    if (is.null(b)) b <- raw_coef(res, "M4", "ALR:f1")
    span <- NA_real_
    if (!is.null(dat) && "id" %in% names(dat)) {
      im <- tryCatch(individual_metrics(dat), error = function(e) NULL)
      if (!is.null(im) && sum(is.finite(im$alr)) > 4) {
        q <- stats::quantile(im$alr[is.finite(im$alr)], c(0.1, 0.9), names = FALSE)
        span <- q[[2]] - q[[1]]
      }
    }
    if (!is.null(b) && is.finite(span) && span > 0) {
      add("Difference in ageing rate, long- vs short-lived", b$est * span, b$se * span,
          paste(scale_word, "per unit age"),
          sprintf("between the 10th and 90th percentile of ALR (a spread of %.3g)", span))
    }
  }
  if (!length(rows)) return(empty)
  out <- do.call(rbind, rows)
  if (!isTRUE(res$standardise))
    out$Note <- paste0(out$Note, "; standardisation is off, so slopes refer to age 0 (and Model 4's to an individual with ALR = 0)")
  out
}

# Models whose among-individual term is age at last record, known lifespan or age at first record, all of which
# the permutation shuffles. Models 3 and 5 use each individual's mean age, computed from its own records, which a
# shuffle of lifespan does not touch - testing them would reproduce the observed result every time.
PERMUTABLE_MODELS <- c("M2", "M4", "M6", "M7", "M8", "M9", "M10")
INTERACTION_MODELS <- c("M4", "M5", "M6", "M8", "M9", "M10")

# Models the null-model bootstrap can test: every model with a lifespan or mean-age term. Unlike a permutation
# test it keeps each individual's own ages, so the mean-age models (3 and 5) can be tested too.
BOOTSTRAP_MODELS <- c("M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9", "M10")

# With fewer refitted datasets p < 0.05 is impossible: the smallest p is 1 / (n_ok + 1).
BOOTSTRAP_MIN_OK <- 19L

#' Null-model parametric bootstrap for selective disappearance
#'
#' Fits a null model with no lifespan-proxy term - a flexible (cubic) ageing function, individual differences in level
#' and in rate of ageing, and the analysis's covariates and family - then simulates new trait values from it while
#' keeping every individual's real ages, ALR, AFR and lifespan, refits the chosen model and Model 1, and records
#' the AIC advantage. Shuffling lifespan between individuals, as a permutation test does, would create records
#' after an individual's last record or death, so anything tied to the observation window would beat every
#' shuffle; simulating the trait keeps the real observation windows, at the same cost.
#'
#' @param dat Standardised data.
#' @param meta Mapping metadata.
#' @param model The model to test (for example "M4").
#' @param against The reference model, usually "M1".
#' @param n_boot Number of simulated datasets.
#' @param settings A list of fitting settings (family, age_function, random_slope, standardise, zi, among, extra).
#' @param progress Optional function(fraction, message) for a progress bar.
#' @param seed Seed for the simulations; the session's random-number stream is restored afterwards.
#' @return A list with the observed statistics, the null draws, one-sided p-values and the null model used.
bootstrap_test <- function(dat, meta, model = "M4", against = "M1", n_boot = 39,
                           settings = list(), progress = NULL, seed = 1L) {
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL
  on.exit({
    if (had_seed) assign(".Random.seed", old_seed, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  }, add = TRUE)
  set.seed(seed)
  stop_if <- function(cond, msg) if (cond) stop(msg, call. = FALSE)
  stop_if(is.null(dat) || !nrow(dat), "No data to simulate from.")
  stop_if(!model %in% BOOTSTRAP_MODELS, paste0(model_label(model), " has no lifespan or mean-age term to test."))
  s <- utils::modifyList(list(family = "gaussian", age_function = "Quadratic", random_slope = "none",
                              standardise = TRUE, zi = "~1", among = "linear", extra = NULL, disp = "constant"), settings)
  # the residual variance (or dispersion) model of the analysis is carried by the null model and by every refit
  # (0.22.2), so a variance that changes with age is simulated under the null as it was fitted to the data
  s$disp <- normalise_disp(s$disp)
  stop_if(isTRUE(s$family %in% BINOMIAL_FAMILIES) && isTRUE(meta$has_trials),
          "The bootstrap is not yet available for binomial traits with a number of trials.")
  stop_if(identical(s$age_function, A3_NONLINEAR), "The bootstrap is not available for the non-linear exponential function.")
  fit_pair <- function(d) {
    r <- tryCatch(fit_model_suite(d, meta, models = unique(c(against, model)), age_function = s$age_function,
                                  family = s$family, random_slope = s$random_slope, standardise = s$standardise,
                                  zi_str = s$zi, among = s$among, extra = s$extra, disp = s$disp),
                  error = function(e) NULL)
    if (is.null(r) || !isTRUE(r$ok) || is.null(r$aic) || !nrow(r$aic)) return(NULL)
    a <- r$aic
    ref <- a$AIC[a$Model == model_label(against)]
    tst <- a$AIC[a$Model == model_label(model)]
    if (!length(ref) || !length(tst) || !is.finite(ref[[1]]) || !is.finite(tst[[1]])) return(NULL)
    int <- NULL
    for (tm in c("f1:ALR", "ALR:f1", "ALR")) {
      int <- raw_coef(r, model, tm)
      if (!is.null(int)) break
    }
    list(gain = ref[[1]] - tst[[1]], coef = if (is.null(int)) NA_real_ else abs(int$est), key = evidence_settings_key(r))
  }
  obs <- fit_pair(dat)
  stop_if(is.null(obs), "The observed models could not be fitted, so there is nothing to compare against.")

  # the null model: no lifespan-proxy term, a flexible ageing function, individual differences in level and in rate of
  # ageing (correlated if the analysis uses that, otherwise uncorrelated, simplified only if it cannot be fitted)
  n_ages <- length(unique(dat$age[is.finite(dat$trait) & is.finite(dat$age)]))
  null_fun <- if (n_ages >= 6) "Cubic" else s$age_function
  null_fit <- NULL
  null_rs <- NA_character_
  for (rs in unique(c(if (identical(s$random_slope, "correlated")) "correlated", "uncorrelated", "none"))) {
    r0 <- tryCatch(fit_model_suite(dat, meta, models = against, age_function = null_fun, family = s$family,
                                   random_slope = rs, standardise = s$standardise, zi_str = s$zi, among = "linear", disp = s$disp),
                   error = function(e) NULL)
    f0 <- if (!is.null(r0) && isTRUE(r0$ok)) r0$fits[[against]] else NULL
    if (!is.null(f0)) { null_fit <- f0; null_rs <- rs; break }
  }
  stop_if(is.null(null_fit), "The null model (no lifespan-proxy term) could not be fitted.")
  simulate_null <- function() {
    sim <- if (inherits(null_fit, "merMod")) stats::simulate(null_fit, nsim = 1, re.form = NA) else stats::simulate(null_fit, nsim = 1)
    v <- sim[[1]]
    if (is.matrix(v)) v <- v[, 1]
    as.numeric(v)
  }
  # the rows the models use: a trait, an age, and no missing covariate or grouping value
  keep_row <- is.finite(dat$trait) & is.finite(dat$age)
  for (cv in intersect(c(meta$covars %||% character(0), meta$random_terms %||% character(0),
                         if (isTRUE(meta$has_group)) "group", if (isTRUE(meta$has_group2)) "group2"), names(dat)))
    keep_row <- keep_row & !is.na(dat[[cv]])
  rows <- which(keep_row)
  first_sim <- simulate_null()
  stop_if(length(first_sim) != length(rows),
          "The simulated values could not be matched to the data rows. Remove rows with missing covariate values and try again.")

  gains <- rep(NA_real_, n_boot); coefs <- rep(NA_real_, n_boot)
  for (k in seq_len(n_boot)) {
    if (is.function(progress)) progress(k / n_boot, sprintf("Simulation %d of %d", k, n_boot))
    d <- dat
    d$trait[rows] <- if (k == 1) first_sim else simulate_null()
    z <- fit_pair(d)
    if (!is.null(z)) { gains[[k]] <- z$gain; coefs[[k]] <- z$coef }
  }
  ok <- is.finite(gains)
  p_gain <- if (!any(ok)) NA_real_ else (1 + sum(gains[ok] >= obs$gain)) / (1 + sum(ok))
  p_coef <- if (!any(is.finite(coefs)) || !is.finite(obs$coef)) NA_real_ else
    (1 + sum(coefs[is.finite(coefs)] >= obs$coef)) / (1 + sum(is.finite(coefs)))
  list(method = "bootstrap", model = model, against = against, n_perm = n_boot, n_ok = sum(ok), seed = seed,
       observed_gain = obs$gain, observed_coef = obs$coef,
       null_gain = gains[ok], null_coef = coefs[is.finite(coefs)],
       p_gain = p_gain, p_coef = p_coef,
       null_gain_95 = if (any(ok)) stats::quantile(gains[ok], 0.95, names = FALSE) else NA_real_,
       null_model = list(age_function = null_fun, random_slope = null_rs, family = s$family,
                         disp = if (isTRUE(s$family %in% DISPERSION_FAMILIES)) s$disp else "constant"),
       settings_key = obs$key)
}

#' Plain-language reading of the null-model bootstrap
#'
#' Uses the same pattern keys as the permutation reading ("drops" meaning the advantage exceeds the null), which
#' the evidence grader reads.
#'
#' @param z The result of bootstrap_test().
#' @return A list with pattern, headline, meaning, trust and alternatives, or NULL.
bootstrap_reading <- function(z) {
  if (is.null(z) || !is.null(z$error) || !length(z$null_gain) || !is.finite(z$observed_gain)) return(NULL)
  obs <- z$observed_gain
  med <- stats::median(z$null_gain)
  q95 <- stats::quantile(z$null_gain, 0.95, names = FALSE)
  p <- z$p_gain
  m <- model_label(z$model)
  inter <- z$model %in% INTERACTION_MODELS
  if (obs <= 2) {
    return(list(pattern = "no_advantage",
      headline = sprintf("%s does not clearly beat Model 1 in your data (AIC advantage %.1f), so there is no advantage to test.", m, obs),
      meaning = "The data give no support for adding this lifespan-proxy term (ALR, lifespan or mean age, whichever this model uses).",
      trust = "The simpler model, Model 1, is the more parsimonious biological interpretation of the data.",
      alternatives = character(0)))
  }
  n_ok <- as.integer(z$n_ok %||% length(z$null_gain))
  if (n_ok < BOOTSTRAP_MIN_OK) {
    return(list(pattern = "too_few",
      headline = sprintf("Only %d simulated dataset%s could be refitted, so this run cannot reach p < 0.05 (the smallest possible p is %.3f).",
                         n_ok, if (n_ok == 1) "" else "s", 1 / (n_ok + 1)),
      meaning = "Not enough simulated datasets to judge the advantage.",
      trust = "Not on this run. Rerun with more simulations (39 or more).",
      alternatives = character(0)))
  }
  if (is.finite(p) && p < 0.05) {
    return(list(pattern = "drops",
      headline = sprintf("%s's advantage over Model 1 (%.1f AIC) is larger than data simulated without any lifespan effect give: at most %.1f in 95%% of %d simulations.",
                         m, obs, q95, length(z$null_gain)),
      meaning = paste0("The simulated data keep your ageing curve, the differences between individuals in level and in rate of ageing, ",
                       "and every individual's real ages, ALR and AFR, but have no link between lifespan and the trait. They rarely give ",
                       "an advantage this large, so the lifespan proxy captures a real association: ",
                       if (inter) "how an individual's trait changes with age differs with how long it lives." else "an individual's trait level differs with how long it lives."),
      trust = "Yes, as evidence of a link between lifespan and the trait, which is what selective disappearance produces.",
      alternatives = c("A change in the trait just before death (terminal decline or investment) is also a link to lifespan, but not selective disappearance. Check 'Is there terminal investment?' in step 2.",
                       "Missing records that depend on the trait itself make individuals look shorter-lived. Check the missingness drivers (step 3).")))
  }
  if (obs > med) {
    return(list(pattern = "partial",
      headline = sprintf("%s's advantage (%.1f AIC) is above most simulations without a lifespan-proxy effect (typical %.1f), but not clearly: 95%% reach up to %.1f.",
                         m, obs, med, q95),
      meaning = "Part of the advantage may come from a real link between lifespan and the trait, but data without one reach similar advantages too often to be sure.",
      trust = "Not on this evidence alone. Treat the lifespan effect as uncertain.",
      alternatives = "Too few simulations or too little data to tell the two apart. Rerun with more simulations."))
  }
  list(pattern = "no_drop",
    headline = sprintf("%s's advantage over Model 1 (%.1f AIC) is no larger than data simulated without any lifespan effect give (typical %.1f).", m, obs, med),
    meaning = "Individual differences in ageing, the shape of the ageing curve and the observation windows alone produce advantages this large, so the lifespan term is not evidence of selective disappearance.",
    trust = "No - not as evidence of selective disappearance.",
    alternatives = "The advantage comes from structure the null model already contains.")
}

#' Plain-language reading of a permutation test
#'
#' Compares the model's advantage over Model 1 in the real data with its advantage when lifespan is shuffled
#' between individuals, and says what that pattern means, whether to trust the model, and what else could
#' explain it.
#'
#' @param z The result of permutation_test().
#' @return A list with pattern, headline, meaning, trust and alternatives, or NULL.
permutation_reading <- function(z) {
  if (is.null(z) || !is.null(z$error) || !length(z$null_gain) || !is.finite(z$observed_gain)) return(NULL)
  obs <- z$observed_gain
  med <- stats::median(z$null_gain)
  q95 <- stats::quantile(z$null_gain, 0.95, names = FALSE)
  p <- z$p_gain
  m <- model_label(z$model)
  inter <- z$model %in% INTERACTION_MODELS
  if (obs <= 2) {
    return(list(pattern = "no_advantage",
      headline = sprintf("%s does not clearly beat Model 1 in your data (AIC advantage %.1f), so there is no advantage to explain.", m, obs),
      meaning = "The data give no support for adding this lifespan-proxy term (ALR, lifespan or mean age, whichever this model uses).",
      trust = "The simpler model, Model 1, is the more parsimonious biological interpretation of the data.",
      alternatives = character(0)))
  }
  if (is.finite(p) && p < 0.05) {
    return(list(pattern = "drops",
      headline = sprintf("%s's advantage over Model 1 drops when lifespan is shuffled between individuals: %.1f AIC with the real lifespans, no more than %.1f in 95%% of shuffles.", m, obs, q95),
      meaning = paste0("The advantage depends on which individual had which lifespan. That is what selective disappearance predicts: ",
                       if (inter) "how an individual's trait changes with age differs with how long it lives." else "an individual's trait level differs with how long it lives."),
      trust = "Yes, provided two other explanations are ruled out.",
      alternatives = c("A misspecified ageing function. The real lifespan term can absorb curvature left by the wrong function, because each individual is only observed up to its own last age; a shuffled lifespan cannot. Check that the ageing function has the most support before relying on this result.",
                       "A change in the trait just before death (terminal decline or investment). That is a genuine link to lifespan but not selective disappearance. Check 'Is there terminal investment?'.")))
  }
  if (obs > med) {
    return(list(pattern = "partial",
      headline = sprintf("%s's advantage shrinks when lifespan is shuffled, but not clearly: %.1f AIC with the real lifespans, %.1f in a typical shuffle, and up to %.1f in 95%% of shuffles (p = %s).", m, obs, med, q95, format_p(p)),
      meaning = "Part of the advantage may come from a real link between lifespan and the trait, but shuffled lifespans reach similar advantages too often to be sure.",
      trust = "Not on this evidence alone. Treat the lifespan effect as uncertain.",
      alternatives = c(if (inter) "Differences between individuals in how fast they age, which the interaction term can absorb. Fit random slopes of age and repeat the test." else NULL,
                       "Too few permutations or too little data to tell the two apart. Rerun with more permutations.")))
  }
  list(pattern = "no_drop",
    headline = sprintf("%s's advantage over Model 1 does not drop when lifespan is shuffled: %.1f AIC with the real lifespans, %.1f in a typical shuffle.", m, obs, med),
    meaning = "The advantage does not depend on individuals' real lifespans, so it is not evidence of selective disappearance. The model fits better for another reason.",
    trust = "No - not as evidence of selective disappearance.",
    alternatives = if (inter) c("The most likely explanation: individuals differ in how fast they age, and the interaction term is absorbing those differences because each individual's own rate of ageing is not in the model. Fit random slopes of age and see whether the model still wins.")
                   else c("The extra lifespan term is fitting chance structure; with shuffled lifespans it does as well. Model 1 is the more parsimonious biological interpretation of the data."))
}

#' Permutation test for selective disappearance
#'
#' Breaks the link between each individual's lifespan proxy and its own trait records by permuting the
#' proxy across individuals, then refits. Everything else - the ageing pattern, the repeated-measures
#' structure, the missingness, the number of records per individual - is left exactly as it is. Under this
#' null there is no selective disappearance, so the distribution of the AIC advantage across permutations
#' is what the chosen model can win by through chance and model flexibility alone.
#'
#' @param dat Standardised data.
#' @param meta Mapping metadata.
#' @param model The model to test (for example "M4").
#' @param against The reference model, usually "M1".
#' @param n_perm Number of permutations.
#' @param settings A list of fitting settings (family, age_function, random_slope, standardise, zi, among, extra).
#' @param progress Optional function(fraction, message) for a progress bar.
#' @param seed Seed for the permutations; the session's random-number stream is restored afterwards.
#' @return A list with the observed statistics, the null draws and one-sided p-values.
permutation_test <- function(dat, meta, model = "M4", against = "M1", n_perm = 100,
                             settings = list(), progress = NULL, seed = 1L) {
  # The permutations draw their own random numbers from a fixed seed, so a rerun gives the same p-value, and the
  # session's random-number stream is restored afterwards, so nothing random elsewhere in the app is shifted.
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL
  on.exit({
    if (had_seed) assign(".Random.seed", old_seed, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  }, add = TRUE)
  set.seed(seed)
  stop_if <- function(cond, msg) if (cond) stop(msg, call. = FALSE)
  stop_if(is.null(dat) || !nrow(dat), "No data to permute.")
  stop_if(!model %in% PERMUTABLE_MODELS,
          paste0(model_label(model), " cannot be tested this way. Its among-individual term is each individual's mean age, ",
                 "which is computed from its own records and is unchanged when lifespan is shuffled, so every shuffle would ",
                 "reproduce the observed result and the test would always report no effect."))
  s <- utils::modifyList(list(family = "gaussian", age_function = "Quadratic", random_slope = "none",
                              standardise = TRUE, zi = "~1", among = "linear", extra = NULL), settings)
  fit_pair <- function(d) {
    r <- tryCatch(fit_model_suite(d, meta, models = unique(c(against, model)), age_function = s$age_function,
                                  family = s$family, random_slope = s$random_slope, standardise = s$standardise,
                                  zi_str = s$zi, among = s$among, extra = s$extra),
                  error = function(e) NULL)
    if (is.null(r) || !isTRUE(r$ok) || is.null(r$aic) || !nrow(r$aic)) return(NULL)
    a <- r$aic
    ref <- a$AIC[a$Model == model_label(against)]
    tst <- a$AIC[a$Model == model_label(model)]
    if (!length(ref) || !length(tst) || !is.finite(ref[[1]]) || !is.finite(tst[[1]])) return(NULL)
    int <- NULL
    for (tm in c("f1:ALR", "ALR:f1", "ALR")) {
      int <- raw_coef(r, model, tm)
      if (!is.null(int)) break
    }
    list(gain = ref[[1]] - tst[[1]], coef = if (is.null(int)) NA_real_ else abs(int$est))
  }
  obs <- fit_pair(dat)
  stop_if(is.null(obs), "The observed models could not be fitted, so there is nothing to compare against.")

  # permute the proxy columns across individuals, keeping each individual's own records intact
  ids <- unique(dat$id)
  # every individual-level proxy moves together, so each individual's (ALR, LS, AFR) stays a coherent set
  prox <- intersect(c("alr", "life", "entry"), names(dat))
  per_id <- dat[!duplicated(dat$id), c("id", prox), drop = FALSE]
  gains <- rep(NA_real_, n_perm); coefs <- rep(NA_real_, n_perm)
  for (k in seq_len(n_perm)) {
    if (is.function(progress)) progress(k / n_perm, sprintf("Permutation %d of %d", k, n_perm))
    map <- per_id
    ord <- sample.int(nrow(map))
    for (p in prox) map[[p]] <- map[[p]][ord]
    d <- dat
    idx <- match(d$id, map$id)
    for (p in prox) d[[p]] <- map[[p]][idx]
    z <- fit_pair(d)
    if (!is.null(z)) { gains[[k]] <- z$gain; coefs[[k]] <- z$coef }
  }
  ok <- is.finite(gains)
  p_gain <- if (!any(ok)) NA_real_ else (1 + sum(gains[ok] >= obs$gain)) / (1 + sum(ok))
  p_coef <- if (!any(is.finite(coefs)) || !is.finite(obs$coef)) NA_real_ else
    (1 + sum(coefs[is.finite(coefs)] >= obs$coef)) / (1 + sum(is.finite(coefs)))
  list(model = model, against = against, n_perm = n_perm, n_ok = sum(ok), seed = seed,
       observed_gain = obs$gain, observed_coef = obs$coef,
       null_gain = gains[ok], null_coef = coefs[is.finite(coefs)],
       p_gain = p_gain, p_coef = p_coef,
       null_gain_95 = if (any(ok)) stats::quantile(gains[ok], 0.95, names = FALSE) else NA_real_)
}
