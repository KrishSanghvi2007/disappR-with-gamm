# disappR engine - Trajectories: individual fits (A3), observed means and population predictions from fitted models.
# Engine code shared by the app and the R interface; changes that alter results must pass the golden tests.

# ---------------------------------------------------------------------------
# A3: individual parametric fits
# ---------------------------------------------------------------------------
# Minimum records (at distinct ages) to fit a function to one individual: Linear 2, Quadratic 3, Cubic 4 (fits
# through every point are allowed), Logarithmic and both exponential functions 3.
A3_MIN_RECORDS <- c(Linear = 2L, Quadratic = 3L, Cubic = 4L, Logarithmic = 3L, "Asymptotic exponential" = 3L)

min_records_for <- function(fun) {
  if (identical(fun, A3_NONLINEAR)) return(3L)
  v <- unname(A3_MIN_RECORDS[fun])
  if (length(v) != 1 || is.na(v)) 3L else v
}

# min_resid_df is kept for compatibility; the minimum number of records depends on the function, or on
# min_records when that is larger. link = "log" fits the function on the log scale by Poisson likelihood - the
# scale on which count traits are modelled and simulated; "identity" fits it to the raw trait by least squares.
poisson_individual_stats <- function(trait, mu, k, fit) {
  n <- length(trait)
  ll <- sum(trait * log(pmax(mu, 1e-300)) - mu - lgamma(trait + 1))   # valid for non-integer means too
  r2 <- if (isTRUE(fit$null.deviance > 0)) 1 - fit$deviance / fit$null.deviance else NA_real_
  df_res <- n - k
  adj <- if (df_res > 0 && is.finite(r2)) 1 - (1 - r2) * (n - 1) / df_res else NA_real_
  aic <- -2 * ll + 2 * k
  aicc <- if ((n - k - 1) > 0) aic + 2 * k * (k + 1) / (n - k - 1) else NA_real_
  # the log-likelihood, the number of coefficients and the Pearson chi-square allow a quasi-AICc (QAICc) when the
  # counts are overdispersed or zero-inflated
  data.frame(n = n, R2 = r2, Adjusted_R2 = adj, AICc = aicc, logLik = ll, k = k,
             pearson = sum((trait - mu)^2 / pmax(mu, 1e-12)), df_res = df_res)
}
poisson_individual_fit <- function(X, trait) {
  if (any(trait < 0)) return(NULL)
  fit <- tryCatch(suppressWarnings(stats::glm.fit(X, trait, family = stats::poisson())), error = function(e) NULL)
  if (is.null(fit) || !isTRUE(fit$converged) || fit$rank < ncol(X)) return(NULL)
  cf <- fit$coefficients
  if (any(!is.finite(cf)) || max(abs(X %*% cf)) > 50) return(NULL)
  fit
}
individual_fit_one <- function(age, trait, p, min_resid_df = 1, link = "identity", min_records = NULL) {
  b <- age_basis(age, p)
  k <- ncol(b) + 1
  n <- length(trait)
  need <- max(k, min_records_for(p$fun), if (is.numeric(min_records) && length(min_records) == 1 && is.finite(min_records)) min_records else 0)
  if (n < need || length(unique(age)) < k) return(NULL)
  X <- cbind(`(Intercept)` = 1, as.matrix(b))
  if (identical(link, "log")) {
    fit <- poisson_individual_fit(X, trait)
    if (is.null(fit)) return(NULL)
    return(list(fit = fit, coefficients = fit$coefficients,
                stat = poisson_individual_stats(trait, fit$fitted.values, k, fit)))
  }
  fit <- tryCatch(stats::lm.fit(X, trait), error = function(e) NULL)
  if (is.null(fit) || fit$rank < k) return(NULL)
  cf <- fit$coefficients
  if (any(!is.finite(cf))) return(NULL)
  rss <- sum(fit$residuals^2)
  tss <- sum((trait - mean(trait))^2)
  r2 <- if (tss > 0) 1 - rss / tss else NA_real_
  df_res <- n - k
  adj <- if (df_res > 0 && is.finite(r2)) 1 - (1 - r2) * (n - 1) / df_res else NA_real_
  kk <- k + 1
  aic <- if (rss > 1e-12) n * log(rss / n) + 2 * kk else NA_real_
  aicc <- if (is.finite(aic) && (n - kk - 1) > 0) aic + 2 * kk * (kk + 1) / (n - kk - 1) else NA_real_
  list(fit = fit, coefficients = cf, stat = data.frame(n = n, R2 = r2, Adjusted_R2 = adj, AICc = aicc))
}

# Non-linear individual function a * exp(b * age), fitted by nls with log-linear starting values.
individual_fit_nonlinear <- function(age, trait, min_resid_df = 1, link = "identity", min_records = NULL) {
  n <- length(trait)
  k <- 2
  need <- max(min_records_for(A3_NONLINEAR), if (is.numeric(min_records) && length(min_records) == 1 && is.finite(min_records)) min_records else 0)
  if (n < need || length(unique(age)) < k) return(NULL)
  if (identical(link, "log")) {
    X <- cbind(1, age)
    fit <- poisson_individual_fit(X, trait)
    if (is.null(fit)) return(NULL)
    cf <- fit$coefficients
    return(list(fit = fit, coefficients = c(a = exp(cf[[1]]), b = cf[[2]]),
                stat = poisson_individual_stats(trait, fit$fitted.values, k, fit)))
  }
  pos <- trait > 0
  st <- if (sum(pos) >= 2 && length(unique(age[pos])) >= 2) {
    c0 <- stats::coef(stats::lm(log(trait[pos]) ~ age[pos]))
    list(a = exp(c0[[1]]), b = c0[[2]])
  } else {
    list(a = max(mean(trait), 1e-6), b = 0)
  }
  dd <- data.frame(age = age, trait = trait)
  fit <- tryCatch(suppressWarnings(stats::nls(trait ~ a * exp(b * age), data = dd, start = st,
                                              control = stats::nls.control(maxiter = 200, warnOnly = TRUE))),
                  error = function(e) NULL)
  if (is.null(fit) || !isTRUE(fit$convInfo$isConv)) return(NULL)
  cf <- stats::coef(fit)
  if (any(!is.finite(cf))) return(NULL)
  rss <- sum(stats::residuals(fit)^2)
  tss <- sum((trait - mean(trait))^2)
  r2 <- if (tss > 0) 1 - rss / tss else NA_real_
  df_res <- n - k
  adj <- if (df_res > 0 && is.finite(r2)) 1 - (1 - r2) * (n - 1) / df_res else NA_real_
  kk <- k + 1
  aic <- if (rss > 1e-12) n * log(rss / n) + 2 * kk else NA_real_
  aicc <- if (is.finite(aic) && (n - kk - 1) > 0) aic + 2 * kk * (kk + 1) / (n - kk - 1) else NA_real_
  list(fit = fit, coefficients = c(a = cf[["a"]], b = cf[["b"]]), stat = data.frame(n = n, R2 = r2, Adjusted_R2 = adj, AICc = aicc))
}

# Two population-level reconstructions from individual fits f(age; theta_i), i = 1..N:
#   mean of coefficients:  f(age; theta_bar), theta_bar = (1/N) sum_i theta_i
#   mean of functions:     f_bar(age) = (1/N) sum_i f(age; theta_i)
# They coincide when f is linear in its parameters (all AGE_FUNCTIONS) and differ otherwise.
# 95% bands: delta method with S/N for the coefficient mean; SD_i[f(age; theta_i)] / sqrt(N) for the function mean.
# exp() capped below the largest finite double, so an extreme fitted curve cannot become Inf or NaN (0.20.5)
exp_capped <- function(x) exp(pmin(x, 700))

# progress: optional function(fraction, detail), called as individuals are fitted (at most about 100 times).
# Ages held by fewer individuals than this are left off the average trajectory (its drawn range only).
A3_MIN_IND_PER_AGE <- 5L

fit_individual_function <- function(dat, fun, min_resid_df = 1, draw_ids = character(0), link = "identity",
                                    min_records = NULL, progress = NULL) {
  d <- dat[is.finite(dat$trait) & is.finite(dat$age), , drop = FALSE]
  if (identical(link, "log") && any(d$trait < 0)) link <- "identity"   # a log scale needs non-negative values
  log_scale <- identical(link, "log")
  empty <- list(curves = data.frame(), coefs = data.frame(), fit_stats = data.frame(), mean_curve = data.frame(),
                n_total = length(unique(d$id)), fitted_ids = character(0), nonlinear = identical(fun, A3_NONLINEAR))
  if (!nrow(d)) return(empty)
  nonlin <- identical(fun, A3_NONLINEAR)
  p <- if (nonlin) NULL else make_age_params(d$age, fun, standardise = FALSE)
  design <- function(ages) cbind(`(Intercept)` = 1, as.matrix(age_basis(ages, p)))
  curve_of <- function(cf, ages) {
    if (nonlin) return(cf[["a"]] * exp_capped(cf[["b"]] * ages))
    eta <- as.numeric(design(ages)[, names(cf), drop = FALSE] %*% cf)
    if (log_scale) exp_capped(eta) else eta
  }
  sp <- split(seq_len(nrow(d)), d$id)
  coef_rows <- list(); stat_rows <- list(); curve_rows <- list()
  n_id <- length(sp)
  every <- max(1L, n_id %/% 100L)
  k_id <- 0L
  for (z in names(sp)) {
    k_id <- k_id + 1L
    if (is.function(progress) && (k_id %% every == 0L || k_id == n_id)) progress(k_id / n_id, sprintf("individual %d of %d", k_id, n_id))
    ii <- sp[[z]]
    one <- if (nonlin) individual_fit_nonlinear(d$age[ii], d$trait[ii], min_resid_df, link, min_records)
           else individual_fit_one(d$age[ii], d$trait[ii], p, min_resid_df, link, min_records)
    if (is.null(one)) next
    cr <- as.data.frame(as.list(one$coefficients), check.names = FALSE)
    cr$id <- z
    coef_rows[[z]] <- cr
    st <- one$stat
    st$id <- z
    stat_rows[[z]] <- st
    if (z %in% draw_ids) {
      ag <- seq(min(d$age[ii]), max(d$age[ii]), length.out = 40)
      curve_rows[[z]] <- data.frame(id = z, age = ag, fitted = curve_of(one$coefficients, ag))
    }
  }
  if (!length(coef_rows)) return(empty)
  coefs <- do.call(rbind, coef_rows)
  fit_stats <- do.call(rbind, stat_rows)
  curves <- if (length(curve_rows)) do.call(rbind, curve_rows) else data.frame()
  cn <- setdiff(names(coefs), "id")
  cm <- as.matrix(coefs[cn])
  N <- nrow(cm)
  mcf <- stats::setNames(colMeans(cm), cn)
  # The average curves span the ages held by at least A3_MIN_IND_PER_AGE individuals, because beyond that they
  # extrapolate few individuals' fits. The threshold sets the drawn range only: the curve is the fitted function at
  # the mean coefficients, so the values at ages already covered do not change when it moves (0.20.22: 10 -> 5).
  n_at_age <- table(id_age_means(d)$age)
  ok_age <- names_num(n_at_age)[as.numeric(n_at_age) >= A3_MIN_IND_PER_AGE]
  rng <- if (length(ok_age)) range(ok_age) else range(d$age)
  ag <- seq(rng[1], rng[2], length.out = 120)
  pred <- curve_of(mcf, ag)
  if (nonlin) {
    Fm <- outer(cm[, "a"], rep(1, length(ag))) * exp_capped(outer(cm[, "b"], ag))
  } else {
    Xg <- design(ag)[, cn, drop = FALSE]
    Fm <- cm %*% t(Xg)
    if (log_scale) Fm <- exp_capped(Fm)
  }
  # an individual whose curve is not finite anywhere on the grid is left out of the mean of functions
  ok_f <- apply(Fm, 1, function(r) all(is.finite(r)))
  Fm <- Fm[ok_f, , drop = FALSE]
  fun_mean <- if (nrow(Fm)) colMeans(Fm) else rep(NA_real_, length(ag))
  se <- se_fun <- rep(NA_real_, length(ag))
  if (N >= 3) {
    S <- stats::cov(cm)
    G <- if (nonlin) cbind(exp_capped(mcf[["b"]] * ag), mcf[["a"]] * ag * exp_capped(mcf[["b"]] * ag)) else Xg
    se <- sqrt(pmax(0, rowSums((G %*% (S / N)) * G)))            # on the log scale when log_scale
    if (nrow(Fm) >= 3) se_fun <- apply(Fm, 2, stats::sd) / sqrt(nrow(Fm))
  }
  # the coefficient mean's band is built on the log scale and transformed, so it stays positive
  lo <- if (log_scale && !nonlin) exp(log(pred) - 1.96 * se) else pred - 1.96 * se
  hi <- if (log_scale && !nonlin) exp(log(pred) + 1.96 * se) else pred + 1.96 * se
  list(curves = curves, coefs = coefs, fit_stats = fit_stats,
       mean_curve = data.frame(age = ag, fitted = pred, lo = lo, hi = hi,
                               fitted_fun = fun_mean, lo_fun = fun_mean - 1.96 * se_fun, hi_fun = fun_mean + 1.96 * se_fun),
       n_total = length(sp), fitted_ids = fit_stats$id, nonlinear = nonlin, log_scale = log_scale, link = link,
       age_params = p, n_nonfinite = sum(!ok_f))
}

#' Curves for chosen individuals from their stored coefficients
#'
#' Redrawing a different set of individuals needs no refitting: each curve is rebuilt from the coefficients that
#' fit_individual_function() stored, over that individual's own age range.
#'
#' @param z The result of fit_individual_function().
#' @param dat The data the fits came from.
#' @param ids The individuals to draw.
#' @return A data frame with id, age and fitted.
individual_curves <- function(z, dat, ids) {
  if (!length(ids) || !is.data.frame(z$coefs) || !nrow(z$coefs)) return(data.frame())
  d <- dat[is.finite(dat$trait) & is.finite(dat$age), , drop = FALSE]
  cn <- setdiff(names(z$coefs), "id")
  out <- lapply(intersect(as.character(ids), as.character(z$coefs$id)), function(i) {
    a <- d$age[as.character(d$id) == i]
    if (!length(a)) return(NULL)
    ag <- seq(min(a), max(a), length.out = 40)
    cf <- unlist(z$coefs[as.character(z$coefs$id) == i, cn, drop = FALSE][1, , drop = TRUE])
    fitted <- if (isTRUE(z$nonlinear)) {
      cf[["a"]] * exp_capped(cf[["b"]] * ag)
    } else {
      X <- cbind(`(Intercept)` = 1, as.matrix(age_basis(ag, z$age_params)))
      eta <- as.numeric(X[, cn, drop = FALSE] %*% cf[cn])
      if (isTRUE(z$log_scale)) exp_capped(eta) else eta
    }
    data.frame(id = i, age = ag, fitted = fitted, stringsAsFactors = FALSE)
  })
  out <- Filter(Negate(is.null), out)
  if (length(out)) do.call(rbind, out) else data.frame()
}

# Several functions' individual fits in one call: step 4's comparison runs this in a separate R process (0.20.12).
# A failure for one function is returned as list(error = ...) and does not stop the others.
fit_individual_functions <- function(dat, funs, link = "identity", min_records = NULL, progress = NULL) {
  out <- list()
  for (i in seq_along(funs)) {
    pr_i <- if (is.function(progress)) function(f, detail) progress((i - 1 + f) / length(funs), paste0(funs[[i]], ": ", detail)) else NULL
    out[[funs[[i]]]] <- tryCatch(fit_individual_function(dat, funs[[i]], 1, character(0), link = link, min_records = min_records,
                                                         progress = pr_i),
                                 error = function(e) list(error = conditionMessage(e)))
  }
  out
}

# fits: optional named list of fit_individual_function() results, by function, reused instead of refitting (0.20.5)
compare_individual_functions <- function(dat, min_resid_df = 1, max_individuals = 2000, link = "identity",
                                         min_records = NULL, quasi = FALSE, fits = NULL) {
  ids <- unique(dat$id)
  if (length(ids) > max_individuals) {
    keep <- ids[unique(round(seq(1, length(ids), length.out = max_individuals)))]
    dat <- dat[dat$id %in% keep, , drop = FALSE]
  }
  n_considered <- length(unique(dat$id))
  kept <- unique(dat$id)
  per <- lapply(A3_FUNCTIONS, function(fn) {
    z <- if (!is.null(fits[[fn]])) fits[[fn]] else fit_individual_function(dat, fn, min_resid_df, link = link, min_records = min_records)
    if (!is.null(z$error) || is.null(z$fit_stats) || !nrow(z$fit_stats)) return(NULL)
    s <- z$fit_stats[z$fit_stats$id %in% kept, , drop = FALSE]
    if (!nrow(s)) return(NULL)
    s$Function <- fn
    s
  })
  per <- Filter(Negate(is.null), per)
  if (!length(per)) return(data.frame())
  per <- do.call(rbind, per)
  # For overdispersed or zero-inflated counts the Poisson likelihood exaggerates differences between functions and
  # favours complex shapes. Functions are then compared by QAICc, with the dispersion estimated once, from the most
  # flexible function: the pooled Pearson chi-square over the pooled residual degrees of freedom (Burnham and
  # Anderson 2002). The coefficients are unaffected: Poisson estimates of the mean are consistent under overdispersion.
  chat <- NA_real_
  if (isTRUE(quasi) && identical(link, "log") && all(c("pearson", "df_res", "logLik", "k") %in% names(per))) {
    glob <- per[per$Function == "Cubic" & is.finite(per$pearson) & per$df_res > 0, , drop = FALSE]
    if (!nrow(glob)) glob <- per[is.finite(per$pearson) & per$df_res > 0, , drop = FALSE]
    if (nrow(glob) && sum(glob$df_res) > 0) chat <- max(1, sum(glob$pearson) / sum(glob$df_res))
    if (is.finite(chat) && chat > 1) {
      kq <- per$k + 1
      per$AICc <- ifelse(per$n - kq - 1 > 0, -2 * per$logLik / chat + 2 * kq + 2 * kq * (kq + 1) / (per$n - kq - 1), NA_real_)
    }
  }
  funs <- unique(per$Function)
  # AICc needs n > k + 1 records per individual. A function whose AICc cannot be computed for most individuals (a
  # cubic with six records per individual) used to empty the common set, so the whole ranking fell back to adjusted
  # R-squared without saying so. Such a function is now left out of the AICc comparison and reported (0.21.15);
  # with fewer than two comparable functions the previous behaviour is kept.
  share_fin <- vapply(funs, function(fn) mean(is.finite(per$AICc[per$Function == fn])), numeric(1))
  comparable <- funs[share_fin >= 0.5]
  if (length(comparable) < 2) comparable <- funs
  excluded <- setdiff(funs, comparable)
  fin <- per[is.finite(per$AICc) & per$Function %in% comparable, , drop = FALSE]
  common <- character(0)
  if (nrow(fin)) {
    nf <- tapply(fin$Function, fin$id, function(v) length(unique(v)))
    common <- names(nf)[nf == length(comparable)]
  }
  per$Delta_AICc <- NA_real_
  cc <- per$id %in% common & per$Function %in% comparable
  if (any(cc)) per$Delta_AICc[cc] <- stats::ave(per$AICc[cc], per$id[cc], FUN = function(v) v - min(v))
  # share of the common individuals for which each function has the lowest AICc; an individual whose lowest AICc is
  # shared by k functions (identical fits, for example two two-parameter functions on two distinct ages) gives each
  # of them 1/k, so the shares sum to 100%
  n_common <- length(common)
  wins <- numeric(0)
  if (any(cc)) {
    best <- per$Delta_AICc[cc] <= 1e-9
    n_tied <- stats::ave(as.numeric(best), per$id[cc], FUN = sum)
    wins <- tapply(ifelse(best, 1 / n_tied, 0), per$Function[cc], sum)
  }
  out <- do.call(rbind, lapply(funs, function(fn) {
    z <- per[per$Function == fn, , drop = FALSE]
    zc <- z[z$id %in% common, , drop = FALSE]
    data.frame(Function = fn, N_considered = n_considered, N_fitted = nrow(z),
               Mean_R2 = mean(z$R2, na.rm = TRUE), Mean_adj_R2 = mean(z$Adjusted_R2, na.rm = TRUE),
               Median_adj_R2 = stats::median(z$Adjusted_R2, na.rm = TRUE),
               N_common = length(common),
               Mean_dAICc = if (nrow(zc)) mean(zc$Delta_AICc) else NA_real_,
               Share_best = if (n_common > 0 && fn %in% names(wins)) round(100 * as.numeric(wins[[fn]]) / n_common, 1) else if (n_common > 0 && fn %in% comparable) 0 else NA_real_,
               stringsAsFactors = FALSE)
  }))
  out <- out[order(out$Mean_dAICc, -out$Mean_adj_R2, na.last = TRUE), , drop = FALSE]
  attr(out, "chat") <- chat
  # functions left out of the AICc ranking, with the share of individuals for which their AICc was incomputable
  attr(out, "excluded") <- stats::setNames(round(100 * (1 - share_fin[excluded])), excluded)
  out
}

observed_trajectory <- function(dat) {
  ia <- id_age_means(dat)
  if (!nrow(ia)) return(data.frame())
  z <- stats::aggregate(trait ~ age, data = ia, FUN = mean)
  z$n <- as.numeric(table(ia$age)[as.character(z$age)])
  names(z)[names(z) == "trait"] <- "fitted"
  z
}

# Population prediction from the fixed effects of a glmmTMB fit (random effects at zero), on the response scale.
# Used only when predict() refuses: with rank_check = "adjust", glmmTMB drops collinear columns and then rejects new
# data whose design contains them ("unknown fixed effects"). A dropped column has no coefficient, which is the same as
# a coefficient of zero, so the retained coefficients give the model's own prediction (0.21.15).
glmmtmb_fixed_predict <- function(fit, nd) {
  fe <- glmmTMB::fixef(fit)
  lin <- function(b, form) {
    if (!length(b) || is.null(form)) return(NULL)
    b[!is.finite(b)] <- 0
    X <- stats::model.matrix(stats::delete.response(stats::terms(form)), nd)
    keep <- intersect(colnames(X), names(b))
    as.numeric(X[, keep, drop = FALSE] %*% b[keep])
  }
  eta <- lin(fe$cond, stats::formula(fit, fixed.only = TRUE))
  if (is.null(eta)) stop("no conditional fixed effects")
  mu <- stats::family(fit)$linkinv(eta)
  zf <- fit$modelInfo$allForm$ziformula
  if (length(fe$zi) && !is.null(zf)) {
    ez <- lin(fe$zi, zf)
    if (!is.null(ez)) mu <- mu * (1 - stats::plogis(ez))
  }
  mu
}


# Model prediction trajectory from a fitted model (see the "i" help for the Models tab):
#   * random effects excluded (re.form = NA);
#   * numeric covariates held at their mean across individuals;
#   * factor covariates marginalised over the observed combinations of levels, weighted by the number of
#     individuals with each combination (unless `by` names a factor, which is then held at each level);
#   * ALR, LS, AFR and individual mean-age terms held at their individual-level means (or at `hold` values,
#     given on the model's standardised scale), with polynomial proxy terms computed from the held value;
#   * response scale (count models include the zero-inflation probability).
# The prediction grid of predict_population_curve() (0.22.0: separated so that the same rows serve the confidence
# draws behind the peak and onset ages): one row per age x combination of factor-covariate levels, with weights.
population_newdata <- function(res, ages, hold = NULL, by = NULL) {
  d <- res$data
  ind <- d[!duplicated(d$id), , drop = FALSE]
  covs <- intersect(res$covars %||% character(0), names(d))
  fac <- covs[!vapply(d[covs], is.numeric, logical(1))]
  num <- setdiff(covs, fac)
  by <- if (length(by) == 1 && isTRUE(by %in% fac)) by else NULL
  other <- setdiff(fac, by)
  if (length(other)) {
    # weights from all records, each individual counting once in total, so row order cannot change them
    key <- do.call(paste, c(lapply(d[other], as.character), sep = "\r"))
    w_rec <- 1 / as.numeric(table(d$id)[as.character(d$id)])
    w_key <- tapply(w_rec, key, sum)
    first_key <- !duplicated(key)
    wt <- as.numeric(w_key[key[first_key]])
    base <- d[first_key, other, drop = FALSE]
  } else {
    base <- data.frame(.one = 1)
    wt <- 1
  }
  if (!is.null(by)) {
    lv <- levels(d[[by]])
    nb0 <- nrow(base)
    base <- base[rep(seq_len(nb0), times = length(lv)), , drop = FALSE]
    wt <- rep(wt, times = length(lv))
    base[[by]] <- factor(rep(lv, each = nb0), levels = lv)
    base$.level <- rep(lv, each = nb0)
  } else {
    base$.level <- "All"
  }
  nb <- nrow(base)
  nd <- base[rep(seq_len(nb), times = length(ages)), , drop = FALSE]
  nd$.w <- rep(wt, times = length(ages))
  nd$age <- rep(ages, each = nb)
  # numeric covariates at the mean of the individuals' own means (independent of row order)
  for (v in num) nd[[v]] <- mean(tapply(d[[v]], d$id, mean, na.rm = TRUE), na.rm = TRUE)
  bb <- age_basis(nd$age, res$age_params)
  for (nm in names(bb)) nd[[nm]] <- bb[[nm]]
  for (v in c("ALR", "LS", "AFR")) {
    val <- if (!is.null(hold[[v]])) hold[[v]] else if (v %in% names(ind)) mean(ind[[v]], na.rm = TRUE) else 0
    if (!is.finite(val)) val <- 0
    nd[[v]] <- val
    nd[[paste0(v, "2")]] <- val^2
    nd[[paste0(v, "3")]] <- val^3
  }
  for (nm in res$basis) {
    hk <- paste0("mean_", nm)
    mv <- if (!is.null(hold[[hk]])) hold[[hk]] else mean(ind[[hk]], na.rm = TRUE)
    nd[[hk]] <- mv
    nd[[paste0("delta_", nm)]] <- nd[[nm]] - mv
  }
  # 'same polynomial order' uses powers of mean age in Models 3 and 5; without them those models cannot be predicted
  if (!is.null(nd[["mean_f1"]])) {
    nd[["mean_f1_2"]] <- nd[["mean_f1"]]^2
    nd[["mean_f1_3"]] <- nd[["mean_f1"]]^3
  }
  nd$id <- d$id[[1]]
  nd$.wts <- 1          # binomial fits weight by trials; the mean on the response scale does not depend on it
  if ("group" %in% names(d)) nd$group <- d$group[[1]]
  if ("group2" %in% names(d)) nd$group2 <- d$group2[[1]]
  for (rt in intersect(res$random_terms %||% character(0), names(d))) nd[[rt]] <- d[[rt]][[1]]
  list(nd = nd, by = by)
}

predict_population_curve <- function(fit, res, ages, hold = NULL, by = NULL) {
  pn <- population_newdata(res, ages, hold, by)
  nd <- pn$nd
  by <- pn$by
  err <- NULL
  pr <- tryCatch({
    if (inherits(fit, "nlme")) as.numeric(stats::predict(fit, newdata = nd, level = 0))
    else as.numeric(stats::predict(fit, newdata = nd, re.form = NA, type = "response", allow.new.levels = TRUE))
  }, error = function(e) { err <<- conditionMessage(e); rep(NA_real_, nrow(nd)) })
  if (!any(is.finite(pr)) && inherits(fit, "glmmTMB")) {
    pr2 <- tryCatch(glmmtmb_fixed_predict(fit, nd), error = function(e) {
      err <<- paste0(err, " | fixed-effect fallback: ", conditionMessage(e)); NULL })
    if (length(pr2) == nrow(nd)) pr <- pr2
  }
  ok <- is.finite(pr)
  if (!any(ok)) {
    # an empty curve is how the app signals "cannot predict here"; the reason is attached for the tests and the logs
    empty <- data.frame()
    attr(empty, "predict_error") <- err %||% "prediction returned no finite value"
    attr(empty, "predict_newdata_names") <- names(nd)
    return(empty)
  }
  grp <- paste(nd$.level[ok], format(nd$age[ok], digits = 15), sep = "\r")
  first <- !duplicated(grp)
  num_w <- tapply(pr[ok] * nd$.w[ok], grp, sum)
  den_w <- tapply(nd$.w[ok], grp, sum)
  out <- data.frame(level = nd$.level[ok][first], age = nd$age[ok][first],
                    fitted = as.numeric(num_w[grp[first]] / den_w[grp[first]]), stringsAsFactors = FALSE)
  out <- out[order(out$level, out$age), , drop = FALSE]
  rownames(out) <- NULL
  if (is.null(by)) out$level <- NULL
  out
}

# Ages for smooth prediction curves in the figure: a fine grid across the observed age range.
smooth_prediction_ages <- function(age, n = 100) {
  a <- age[is.finite(age)]
  if (!length(a)) return(numeric(0))
  if (diff(range(a)) <= 0) return(unique(a))
  seq(min(a), max(a), length.out = n)
}

# Ages at which to draw predictions as lines: the distinct observed ages; with more than 60 distinct ages (continuous or
# irregular ages), the sampling occasions (ages rounded to the sampling step from the youngest age); otherwise the
# standard grid.
observed_prediction_ages <- function(age, id = NULL, step = NULL) {
  a <- sort(unique(age[is.finite(age)]))
  if (length(a) <= 60) return(a)
  step <- if (valid_age_step(step)) step else if (is.null(id)) NA_real_ else tryCatch(infer_age_step(age[is.finite(age)], id[is.finite(age)]), error = function(e) NA_real_)
  if (is.finite(step) && step > 0) {
    occ <- sort(unique(min(a) + round((a - min(a)) / step) * step))
    occ <- occ[occ <= max(a) + 1e-9]
    if (length(occ) >= 2 && length(occ) <= 200) return(occ)
  }
  prediction_ages(age)
}

prediction_ages <- function(age) {
  u <- sort(unique(age[is.finite(age)]))
  if (length(u) <= 30) u else seq(min(u), max(u), length.out = 80)
}

# ---- Moved unchanged from inst/app/server.R (0.9.11): pure helpers that use no reactive state ----

# ======================================================================
# 4. A3 individual parametric fits
# ======================================================================
# Minimum records per function: see min_records_for() (Linear 2; Quadratic, logarithmic, exponential 3; Cubic 4).
a3_min_df <- function() 1

pred_curves_for <- function(r, models, by = NULL, ages = prediction_ages(r$data$age)) {
  lst <- lapply(models, function(m) {
    cv <- predict_population_curve(r$fits[[m]], r, ages, by = by)
    if (!nrow(cv)) return(NULL)
    cv$Method <- model_label(m)
    cv$Fit <- r$validity[[m]] %||% "Valid"
    cv
  })
  lst <- Filter(Negate(is.null), lst)
  if (length(lst)) do.call(rbind, lst) else data.frame(age = numeric(0), fitted = numeric(0), Method = character(0), Fit = character(0))
}

# ---- Predictions before coefficients (0.9.13) ----

# The trait each eligible model predicts at representative observed ages (the youngest, quartiles and oldest), on the
# trait scale: easier to read than raw coefficients of polynomial, standardised or interaction terms.
prediction_summary <- function(r, n_ages = 5) {
  if (is.null(r) || !isTRUE(r$ok) || !length(r$fits)) return(data.frame())
  a <- r$data$age[is.finite(r$data$age)]
  if (!length(a)) return(data.frame())
  ages <- sort(unique(stats::quantile(a, seq(0, 1, length.out = n_ages), names = FALSE, type = 1)))
  elig <- intersect(names(r$fits), sub("^Model ", "M", r$aic$Model[r$aic$Eligible %in% TRUE]))
  if (!length(elig)) elig <- names(r$fits)
  rows <- lapply(elig, function(m) {
    pc <- tryCatch(predict_population_curve(r$fits[[m]], r, ages), error = function(e) NULL)
    if (!is.data.frame(pc) || !nrow(pc)) return(NULL)
    vals <- rep(NA_real_, length(ages))
    vals[match(signif(pc$age, 10), signif(ages, 10))] <- signif(pc$fitted, 4)
    out <- as.data.frame(as.list(stats::setNames(vals, paste("Age", vapply(ages, format_num, character(1))))), check.names = FALSE)
    cbind(Model = model_label(m), out, stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows)) do.call(rbind, rows) else data.frame()
}


# ---------------------------------------------------------------------------
# Peak age and onset of senescence (0.22.0), read from a model's predicted population trajectory: the curve of a
# typical individual (random effects at zero, lifespan proxies at their individual-level means, covariates as in the
# prediction figure) on a fine grid over the sampled ages. Confidence limits come from draws of the fixed effects
# from their estimated sampling distribution, so they reflect uncertainty in the population curve only.
# ---------------------------------------------------------------------------
fixed_design <- function(fit, nd) {
  # fixed-effect part of the formula; formula(fixed.only = TRUE) works for lme4 and glmmTMB fits alike, and avoids
  # lme4::nobars(), which lme4 has moved to the reformulas package (0.24.1)
  form <- stats::formula(fit, fixed.only = TRUE)
  stats::model.matrix(stats::delete.response(stats::terms(form)), nd)
}

# Population curves for n_draws draws of the fixed effects: a matrix with one row per age and one column per draw.
# NULL when the model type or its zero-inflation structure is not supported (non-linear fits; zero inflation that
# varies with covariates).
population_curve_draws <- function(fit, res, ages, n_draws = 1000, seed = 1L) {
  if (!(inherits(fit, "merMod") || inherits(fit, "glmmTMB"))) return(NULL)
  if (inherits(fit, "glmmTMB")) {
    zf <- tryCatch(paste(deparse(fit$modelInfo$allForm$ziformula), collapse = ""), error = function(e) "~0")
    if (!gsub(" ", "", zf) %in% c("~0", "~1")) return(NULL)   # a constant zero inflation leaves the curve's shape unchanged
    beta <- glmmTMB::fixef(fit)$cond
    V <- tryCatch(as.matrix(stats::vcov(fit)$cond), error = function(e) NULL)
    linkinv <- stats::family(fit)$linkinv
  } else {
    beta <- lme4::fixef(fit)
    V <- tryCatch(as.matrix(stats::vcov(fit)), error = function(e) NULL)
    linkinv <- function(x) x
  }
  if (is.null(V) || !length(beta)) return(NULL)
  beta <- beta[is.finite(beta)]
  keep <- intersect(names(beta), intersect(rownames(V), colnames(V)))
  if (!length(keep)) return(NULL)
  beta <- beta[keep]
  V <- V[keep, keep, drop = FALSE]
  if (any(!is.finite(V))) return(NULL)
  nd <- population_newdata(res, ages)$nd
  X <- tryCatch(fixed_design(fit, nd), error = function(e) NULL)
  if (is.null(X) || !all(keep %in% colnames(X))) return(NULL)
  X <- X[, keep, drop = FALSE]
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL
  on.exit({
    if (had_seed) assign(".Random.seed", old_seed, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  }, add = TRUE)
  set.seed(seed)
  ev <- eigen((V + t(V)) / 2, symmetric = TRUE)
  L <- ev$vectors %*% diag(sqrt(pmax(ev$values, 0)), nrow = length(ev$values))
  B <- as.numeric(beta) + L %*% matrix(stats::rnorm(length(keep) * n_draws), length(keep), n_draws)
  eta <- X %*% B
  mu <- matrix(linkinv(as.vector(eta)), nrow = nrow(eta))
  g <- match(nd$age, ages)
  num <- rowsum(mu * nd$.w, g)
  den <- rowsum(matrix(nd$.w, ncol = 1), g)
  out <- num / as.vector(den)
  out[order(as.integer(rownames(out))), , drop = FALSE]
}

# Turning points of one curve: the global maximum (peak), the start of the final uninterrupted decline to the oldest
# age (onset of senescence), and the global minimum. "Interior" means within the sampled ages, not at their edge.
curve_turning_points <- function(age, y) {
  empty <- list(peak = NA_real_, peak_interior = FALSE, onset = NA_real_, onset_type = "none",
                trough = NA_real_, trough_interior = FALSE)
  ok <- is.finite(age) & is.finite(y)
  age <- age[ok]; y <- y[ok]
  o <- order(age); age <- age[o]; y <- y[o]
  n <- length(y)
  if (n < 3) return(empty)
  tol <- 1e-10 * max(1, diff(range(y)))
  i_max <- which.max(y)
  i_min <- which.min(y)
  dy <- diff(y)
  if (dy[[n - 1]] < -tol) {
    k <- n - 1
    while (k > 1 && dy[[k - 1]] < -tol) k <- k - 1
    onset <- age[[k]]
    onset_type <- if (k == 1) "from_start" else "interior"
  } else {
    onset <- NA_real_
    onset_type <- "no_final_decline"
  }
  list(peak = age[[i_max]], peak_interior = i_max > 1 && i_max < n, onset = onset, onset_type = onset_type,
       trough = age[[i_min]], trough_interior = i_min > 1 && i_min < n)
}

peak_onset_summary <- function(res, m, n_draws = 1000, n_grid = 200) {
  fit <- res$fits[[m]]
  if (is.null(fit)) return(list(ok = FALSE, message = "This model was not fitted."))
  ages <- smooth_prediction_ages(res$data$age, n_grid)
  if (length(ages) < 3) return(list(ok = FALSE, message = "Too few distinct ages to locate a peak."))
  pc <- predict_population_curve(fit, res, ages)
  if (!is.data.frame(pc) || nrow(pc) < 3) return(list(ok = FALSE, message = "The model's population curve could not be computed."))
  pc <- pc[order(pc$age), , drop = FALSE]
  tp <- curve_turning_points(pc$age, pc$fitted)
  ci <- list(peak = c(NA_real_, NA_real_), onset = c(NA_real_, NA_real_), share_peak = NA_real_, share_onset = NA_real_)
  dr <- if (n_draws > 0) tryCatch(population_curve_draws(fit, res, pc$age, n_draws), error = function(e) NULL) else NULL
  if (!is.null(dr) && ncol(dr) > 0) {
    tps <- vapply(seq_len(ncol(dr)), function(j) {
      t <- curve_turning_points(pc$age, dr[, j])
      c(t$peak, as.numeric(t$peak_interior), t$onset, as.numeric(identical(t$onset_type, "interior")))
    }, numeric(4))
    pk <- tps[1, tps[2, ] == 1]
    on <- tps[3, tps[4, ] == 1]
    ci$share_peak <- mean(tps[2, ] == 1)
    ci$share_onset <- mean(tps[4, ] == 1)
    if (length(pk) >= 20) ci$peak <- stats::quantile(pk, c(0.025, 0.975), names = FALSE)
    if (length(on) >= 20) ci$onset <- stats::quantile(on, c(0.025, 0.975), names = FALSE)
  }
  list(ok = TRUE, model = m, turning = tp, ci = ci, draws = !is.null(dr), age_range = range(pc$age))
}

# One sentence per model for the peak-and-onset box (features of a fitted curve, so worded as such). The CI source is
# stated once, in the box.
peak_onset_sentence <- function(z) {
  if (is.null(z) || !isTRUE(z$ok)) return(if (is.list(z) && length(z$message)) z$message else "Not available for this model.")
  tp <- z$turning
  ci <- function(lim, share) {
    if (!all(is.finite(lim))) return("")
    paste0(" (95% CI ", format_num(lim[[1]]), "\u2013", format_num(lim[[2]]),
           if (is.finite(share) && share < 0.95) sprintf("; within the sampled ages in %.0f%% of draws", 100 * share) else "", ")")
  }
  same <- isTRUE(tp$peak_interior) && identical(tp$onset_type, "interior") && abs(tp$onset - tp$peak) <= 1e-6 * max(1, abs(tp$peak))
  peak <- if (isTRUE(tp$peak_interior)) paste0("peak at age ", format_num(tp$peak), ci(z$ci$peak, z$ci$share_peak))
          else sprintf("no peak within ages %s\u2013%s (highest at %s)", format_num(z$age_range[[1]]), format_num(z$age_range[[2]]), format_num(tp$peak))
  onset <- if (same) ", then decline to the oldest sampled age" else switch(tp$onset_type,
    interior = paste0("; final decline from age ", format_num(tp$onset), ci(z$ci$onset, z$ci$share_onset)),
    from_start = "; declines throughout",
    no_final_decline = "; no final decline", "")
  trough <- if (isTRUE(tp$trough_interior)) paste0(" Lowest at age ", format_num(tp$trough), ".") else ""
  paste0(model_label(z$model), ": ", peak, onset, ".", trough, if (isTRUE(z$draws)) "" else " No CI for this model type.")
}
