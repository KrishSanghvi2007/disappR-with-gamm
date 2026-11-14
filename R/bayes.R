# disappR engine - Step 8 (optional): Bayesian refits of the parametric models of step 5 (Models 1-10) with brms and
# Stan. Step 8 refits one model that step 5 has already fitted, on the same records and with the same formula, so the
# Bayesian and frequentist answers can be read side by side. It never changes a step 1-7 result.
#
# Workflow taught by the tab (Gelman et al. 2020; Gabry et al. 2019): choose priors -> look at the ageing curves the
# priors allow (prior predictive check) -> fit -> check convergence (Vehtari et al. 2021) -> read the posterior ->
# check the model against the data (posterior predictive checks).
#
# Priors are passed to Stan as data (brms stanvars), so changing a prior width, the chains or the data re-uses the
# compiled model; only a different model structure is compiled again. Compiled models are cached on disk by the hash
# of their Stan code.

BAYES_FAMILIES <- c("gaussian", "poisson", "binomial")

# Width multipliers of the default priors on the fixed effects (autoscaled as in rstanarm: the prior SD of a
# coefficient is w * SD(trait) / SD(predictor) for Gaussian traits and w / SD(predictor) on the log or logit scale).
BAYES_WIDTHS <- c("Narrow, sceptical (1)" = "narrow", "Weakly informative (2.5, recommended)" = "default",
                  "Wide (10)" = "wide", "Very wide, nearly flat (100)" = "flat")
BAYES_WIDTH_VALUES <- c(narrow = 1, default = 2.5, wide = 10, flat = 100)

BAYES_MODES <- list(
  draft = list(chains = 2L, iter = 1000L, warmup = 500L),
  full  = list(chains = 4L, iter = 2000L, warmup = 1000L))

bayes_has_brms <- function() requireNamespace("brms", quietly = TRUE) && requireNamespace("rstan", quietly = TRUE)

bayes_width_value <- function(width) {
  w <- BAYES_WIDTH_VALUES[as.character(width %||% "default")[1]]
  if (is.na(w)) BAYES_WIDTH_VALUES[["default"]] else unname(w)
}

# ---- environment -------------------------------------------------------------------------------------------------
# Packages are checked always; the C++ compiler only when asked (check_compiler = TRUE), because pkgbuild's check can
# open RStudio's "install build tools" dialog.
bayes_environment <- function(check_compiler = FALSE) {
  pk <- c("brms", "rstan", "StanHeaders", "posterior", "bayesplot")
  have <- vapply(pk, function(p) requireNamespace(p, quietly = TRUE), logical(1))
  ver <- vapply(pk, function(p) if (have[[p]]) as.character(utils::packageVersion(p)) else "", character(1))
  os <- tryCatch(Sys.info()[["sysname"]], error = function(e) "")
  fix <- switch(os,
    Windows = "Install Rtools for your version of R (https://cran.r-project.org/bin/windows/Rtools/), restart R, then run pkgbuild::has_build_tools(debug = TRUE) to confirm.",
    Darwin = "Install Apple's command-line tools: in the Terminal run xcode-select --install, restart R, then run pkgbuild::has_build_tools(debug = TRUE) to confirm.",
    "Install a C++ compiler and make (for example: sudo apt install build-essential), restart R, then run pkgbuild::has_build_tools(debug = TRUE) to confirm.")
  compiler <- NA
  if (isTRUE(check_compiler)) {
    compiler <- if (requireNamespace("pkgbuild", quietly = TRUE)) {
      isTRUE(tryCatch(pkgbuild::has_build_tools(debug = FALSE), error = function(e) FALSE))
    } else nzchar(Sys.which("g++")) || nzchar(Sys.which("clang++"))
  }
  missing <- pk[!have]
  msg <- if (length(missing)) {
    sprintf("Step 8 needs the %s package%s: install.packages(c(%s)). Installing brms installs the others.",
            paste(missing, collapse = ", "), if (length(missing) > 1) "s" else "", paste0("\"", missing, "\"", collapse = ", "))
  } else if (isFALSE(compiler)) {
    paste("brms is installed, but no C++ compiler was found. Stan translates each model into C++ and compiles it, so a compiler is needed.", fix)
  } else if (isTRUE(compiler)) {
    "Ready: brms, Stan and a C++ compiler are installed."
  } else "brms and Stan are installed. Press 'Check my set-up' to confirm that a C++ compiler is available."
  list(ok = !length(missing) && !isFALSE(compiler), packages_ok = !length(missing), compiler = compiler, os = os, fix = fix,
       message = msg, packages = data.frame(Package = pk, Installed = ifelse(have, "yes", "no"), Version = ver, stringsAsFactors = FALSE))
}

# ---- what is refitted ----------------------------------------------------------------------------------------------
# The model to refit by default: the best-ranked eligible model of the step-5 fit.
bayes_default_model <- function(r) {
  if (!is.list(r) || !length(r$fits)) return(NULL)
  a <- r$aic
  if (is.data.frame(a) && nrow(a) && "Eligible" %in% names(a)) {
    a <- a[a$Eligible, , drop = FALSE]
    if (nrow(a)) return(sub("^Model ", "M", a$Model[[1]]))
  }
  names(r$fits)[[1]]
}

# The specification of the Bayesian refit of model m of a step-5 result r: formula, family, data and everything needed
# for curves and comparisons. Everything here is computed in the main R process (it needs the step-5 fit).
bayes_spec <- function(r, m) {
  fail <- function(msg) list(ok = FALSE, message = msg)
  if (!is.list(r) || !isTRUE(r$ok) || !length(r$fits)) return(fail("Fit the models on the Modelling tab (step 5) first: step 8 refits one of them."))
  if (isTRUE(r$nonlinear)) return(fail("The non-linear a\u00b7exp(b\u00b7age) model cannot be refitted in step 8 yet: choose another ageing function in step 5."))
  if (is.null(m) || !m %in% names(r$fits)) return(fail("Choose a model that was fitted in step 5."))
  fam <- r$family %||% "gaussian"
  if (!fam %in% BAYES_FAMILIES) {
    return(fail(sprintf("Step 8 covers Gaussian, Poisson and binomial traits in this version; step 5 used the %s family. Other families will follow.",
                        family_label(fam))))
  }
  if (!identical(normalise_disp(r$disp), "constant")) {
    return(fail("Step 8 does not yet model a variance that changes with age: set 'variance across age' to constant in step 5 and refit."))
  }
  d <- r$data
  fixed_rhs <- r$formulas[[m]]
  rhs <- paste(fixed_rhs, "+", r$random)
  resp <- "trait"
  brms_family <- fam
  if (identical(fam, "binomial")) {
    tr <- if (".trials" %in% names(d)) as.numeric(d$.trials) else numeric(0)
    if (length(tr) && all(is.finite(tr) & tr > 0) && !all(d$trait %in% c(0, 1) & tr == 1)) {
      d$.succ <- round(d$trait * tr)
      d$.trials <- round(tr)
      resp <- ".succ | trials(.trials)"
    } else {
      brms_family <- "bernoulli"
    }
  }
  vars <- unique(c(all.vars(stats::as.formula(paste("trait ~", rhs))), "age", if (grepl("trials", resp)) c(".succ", ".trials")))
  dd <- d[, intersect(vars, names(d)), drop = FALSE]
  res <- list(data = d, covars = r$covars %||% character(0), age_params = r$age_params, basis = r$basis,
              random_terms = r$random_terms %||% character(0), proxy_params = r$proxy_params)
  ages <- smooth_prediction_ages(d$age, 100)
  freq_curve <- tryCatch(predict_population_curve(r$fits[[m]], r, ages), error = function(e) data.frame())
  coefs <- r$coefficients
  coefs <- if (is.data.frame(coefs) && nrow(coefs)) coefs[coefs$Model == model_label(m), , drop = FALSE] else data.frame()
  vc <- r$varcomp
  vc <- if (is.data.frame(vc) && nrow(vc)) vc[vc$Model == model_label(m), , drop = FALSE] else data.frame()
  list(ok = TRUE, model = m, label = model_label(m), formula = paste(resp, "~", rhs), fixed_rhs = fixed_rhs, random = r$random,
       family = fam, brms_family = brms_family, data = dd, res = res, ages = ages, freq_curve = freq_curve, freq_coef = coefs,
       freq_varcomp = vc, age_function = r$age_function, standardise = isTRUE(r$standardise), random_slope = r$random_slope,
       n = nrow(dd), n_ind = length(unique(dd$id)), freq_ci_note = if (identical(fam, "gaussian")) "" else "Wald")
}

bayes_family_obj <- function(f) {
  switch(f, gaussian = stats::gaussian(), poisson = stats::poisson(), bernoulli = brms::bernoulli(), binomial = stats::binomial())
}
bayes_linkinv <- function(fam) if (identical(fam, "gaussian")) identity else if (identical(fam, "poisson")) exp else stats::plogis
bayes_link <- function(fam) if (identical(fam, "gaussian")) identity else if (identical(fam, "poisson")) log else stats::qlogis

# Fixed-effect design matrix of the data (columns as brms names them, intercept removed).
bayes_design <- function(spec, nd = spec$data) {
  X <- stats::model.matrix(stats::as.formula(paste("~", spec$fixed_rhs)), nd)
  X
}

# ---- priors ----------------------------------------------------------------------------------------------------------
# Default priors (weakly informative, autoscaled). Returns the prior table shown to the user, the values passed to Stan
# as data, and the brms objects.
bayes_priors <- function(spec, width = "default") {
  w <- bayes_width_value(width)
  fam <- spec$family
  y <- spec$data$trait
  if (identical(fam, "binomial") && ".succ" %in% names(spec$data)) y <- spec$data$.succ / spec$data$.trials
  X <- bayes_design(spec)
  X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  sx <- apply(X, 2, stats::sd)
  sx[!is.finite(sx) | sx <= 0] <- 1
  gauss <- identical(fam, "gaussian")
  sy <- if (gauss) stats::sd(y) else 1
  if (!is.finite(sy) || sy <= 0) sy <- 1
  my <- mean(y)
  int_loc <- if (gauss) my else if (identical(fam, "poisson")) log(max(my, 0.01)) else stats::qlogis(min(max(my, 0.01), 0.99))
  int_scale <- 2.5 * sy
  b_sd <- w * sy / sx
  sd_scale <- sy
  vals <- c(int_loc = int_loc, int_scale = int_scale, sd_scale = sd_scale, sigma_scale = if (gauss) sy else NA_real_)
  scale_txt <- if (gauss) "trait units" else if (identical(fam, "poisson")) "log scale" else "logit scale"
  coef_tab <- data.frame(Class = "Fixed effect", Term = display_term(colnames(X)), Raw = colnames(X),
                         Prior = sprintf("Normal(0, %s)", format_num(b_sd, 3)), SD = unname(b_sd), stringsAsFactors = FALSE)
  other <- data.frame(Class = c("Intercept", "Random-effect SDs", if (gauss) "Residual SD", if (grepl("correlated", spec$random_slope %||% "") && !grepl("uncorrelated", spec$random_slope %||% "")) "Correlations"),
                      Term = c("at the mean of the predictors", "every SD", if (gauss) "sigma", if (grepl("^correlated", spec$random_slope %||% "")) "every correlation"),
                      Raw = "", Prior = c(sprintf("Student-t(3, %s, %s)", format_num(int_loc, 3), format_num(int_scale, 3)),
                                          sprintf("half Student-t(3, 0, %s)", format_num(sd_scale, 3)),
                                          if (gauss) sprintf("half Student-t(3, 0, %s)", format_num(sy, 3)),
                                          if (grepl("^correlated", spec$random_slope %||% "")) "LKJ(2)"),
                      SD = NA_real_, stringsAsFactors = FALSE)
  tab <- rbind(other[1, , drop = FALSE], coef_tab, other[-1, , drop = FALSE])
  meaning <- c(
    Intercept = sprintf("The mean trait (%s) is expected near %s, give or take about %s.", scale_txt, format_num(int_loc, 3), format_num(2 * int_scale, 3)),
    b = sprintf("Each coefficient is centred on no effect. With width %s, a change of one standard deviation in a predictor is expected to shift the trait by less than about %s %s.",
                format_num(w), format_num(2 * w * sy, 3), if (gauss) "trait units (95% of the prior)" else paste("units on the", scale_txt, "(95% of the prior)")),
    sd = sprintf("Individuals (and other random-effect groups) are expected to differ with an SD of up to a few times %s (%s); an SD near zero is allowed.", format_num(sd_scale, 3), scale_txt))
  list(width = width, w = w, values = vals, b_sd = stats::setNames(unname(b_sd), colnames(X)), colmeans = colMeans(X),
       table = tab, meaning = meaning, scale = scale_txt)
}

# brms prior and stanvars objects for bayes_priors(); one stanvar per coefficient, so widths are data, not code.
bayes_brms_priors <- function(spec, pv) {
  nm <- names(pv$b_sd)
  svn <- paste0("psd_", seq_along(nm))
  sv <- brms::stanvar(pv$values[["int_loc"]], "p_int_loc") + brms::stanvar(pv$values[["int_scale"]], "p_int_scale") +
        brms::stanvar(pv$values[["sd_scale"]], "p_sd_scale")
  pr <- brms::set_prior("student_t(3, p_int_loc, p_int_scale)", class = "Intercept") +
        brms::set_prior("student_t(3, 0, p_sd_scale)", class = "sd")
  gp <- brms::get_prior(stats::as.formula(spec$formula), data = spec$data, family = bayes_family_obj(spec$brms_family))
  bcoef <- gp$coef[gp$class == "b" & nzchar(gp$coef)]
  for (i in seq_along(nm)) {
    if (!nm[[i]] %in% bcoef) next
    sv <- sv + brms::stanvar(pv$b_sd[[i]], svn[[i]])
    pr <- pr + brms::set_prior(sprintf("normal(0, %s)", svn[[i]]), class = "b", coef = nm[[i]])
  }
  if (identical(spec$family, "gaussian")) {
    sv <- sv + brms::stanvar(pv$values[["sigma_scale"]], "p_sigma_scale")
    pr <- pr + brms::set_prior("student_t(3, 0, p_sigma_scale)", class = "sigma")
  }
  if (any(gp$class == "cor")) pr <- pr + brms::set_prior("lkj(2)", class = "cor")
  list(prior = pr, stanvars = sv)
}

# The same priors written with numbers, for the exported R code.
bayes_prior_code <- function(spec, pv) {
  nm <- names(pv$b_sd)
  lines <- c(sprintf("  prior(student_t(3, %s, %s), class = Intercept)", signif(pv$values[["int_loc"]], 4), signif(pv$values[["int_scale"]], 4)),
             sprintf("  prior(student_t(3, 0, %s), class = sd)", signif(pv$values[["sd_scale"]], 4)),
             sprintf("  set_prior(\"normal(0, %s)\", class = \"b\", coef = \"%s\")", signif(pv$b_sd, 4), nm),
             if (identical(spec$family, "gaussian")) sprintf("  prior(student_t(3, 0, %s), class = sigma)", signif(pv$values[["sigma_scale"]], 4)),
             if (grepl("^correlated", spec$random_slope %||% "")) "  prior(lkj(2), class = cor)")
  paste0("priors <- c(\n", paste(lines, collapse = ",\n"), "\n)")
}

# Population curves (random effects at zero, as in step 5) for a matrix of fixed-effect draws B (rows named as the
# design columns, "(Intercept)" included); one column per draw, one row per age.
bayes_curve_matrix <- function(spec, B, ages = spec$ages) {
  nd <- population_newdata(spec$res, ages)$nd
  X <- stats::model.matrix(stats::as.formula(paste("~", spec$fixed_rhs)), nd)
  keep <- intersect(colnames(X), rownames(B))
  eta <- X[, keep, drop = FALSE] %*% B[keep, , drop = FALSE]
  mu <- matrix(bayes_linkinv(spec$family)(as.vector(eta)), nrow = nrow(eta))
  g <- match(nd$age, ages)
  num <- rowsum(mu * nd$.w, g)
  den <- rowsum(matrix(nd$.w, ncol = 1), g)
  out <- num / as.vector(den)
  out[order(as.integer(rownames(out))), , drop = FALSE]
}

bayes_curve_summary <- function(M, ages, probs = c(0.025, 0.975)) {
  data.frame(age = ages, median = apply(M, 1, stats::median), lo = apply(M, 1, stats::quantile, probs[1], na.rm = TRUE),
             hi = apply(M, 1, stats::quantile, probs[2], na.rm = TRUE))
}

# Prior predictive check of the ageing curve, simulated directly from the priors (no Stan, so it is instant): n_draws
# population curves with the intercept and every fixed effect drawn from its prior. brms puts the intercept prior on
# the trait at the mean of the predictors, so the intercept of each draw is recovered from it.
bayes_prior_curves <- function(spec, pv, n_draws = 200, seed = 1L) {
  set.seed(seed)
  nb <- length(pv$b_sd)
  B <- matrix(stats::rnorm(nb * n_draws, 0, rep(pv$b_sd, n_draws)), nb, n_draws, dimnames = list(names(pv$b_sd), NULL))
  ic <- pv$values[["int_loc"]] + pv$values[["int_scale"]] * stats::rt(n_draws, 3)
  b0 <- ic - colSums(B * pv$colmeans[rownames(B)])
  B <- rbind(`(Intercept)` = b0, B)
  M <- bayes_curve_matrix(spec, B)
  y <- spec$data$trait
  rng <- range(y, na.rm = TRUE)
  span <- diff(rng); if (!is.finite(span) || span <= 0) span <- 1
  lim <- if (identical(spec$family, "gaussian")) c(rng[1] - 2 * span, rng[2] + 2 * span) else if (identical(spec$family, "poisson")) c(0, rng[2] + 3 * span) else c(0, 1)
  outside <- mean(apply(M, 2, function(v) any(v < lim[1] | v > lim[2])))
  extreme <- if (identical(spec$family, "binomial")) mean(apply(M, 2, function(v) any(v < 0.01 | v > 0.99))) else NA_real_
  long <- data.frame(draw = rep(seq_len(n_draws), each = length(spec$ages)), age = rep(spec$ages, n_draws), value = as.vector(M))
  list(curves = long, summary = bayes_curve_summary(M, spec$ages, c(0.05, 0.95)), observed = range(y, na.rm = TRUE), limits = lim,
       share_outside = outside, share_extreme = extreme, n_draws = n_draws)
}

bayes_prior_reading <- function(pc, spec, pv) {
  pct <- function(x) sprintf("%.0f%%", 100 * x)
  base <- sprintf("Each grey line is one ageing curve the priors allow before seeing the data (%d draws, random effects at zero).", pc$n_draws)
  judge <- if (identical(spec$family, "binomial")) {
    sprintf("%s of the curves reach probabilities below 1%% or above 99%% somewhere: %s", pct(pc$share_extreme),
            if (pc$share_extreme > 0.5) "the priors pile probability at 0 and 1 (common with very wide priors on the logit scale), which is not 'uninformative'. A narrower width is more neutral here." else "the priors are reasonably spread across plausible probabilities.")
  } else {
    sprintf("%s of the curves leave a generous band around the observed values (%s to %s): %s", pct(pc$share_outside),
            format_num(pc$limits[1], 3), format_num(pc$limits[2], 3),
            if (pc$share_outside > 0.5) {
              paste0("the priors allow many implausible trajectories, mostly through the curvature terms. That is typical of weakly informative priors, which rule out only absurd values",
                     if (isTRUE(spec$n_ind >= 100)) sprintf("; with %s individuals the data will dominate, which you can confirm by refitting with the narrow width and seeing whether the results change.", format(spec$n_ind, big.mark = ","))
                     else sprintf(". With only %d individuals the priors can matter: compare a fit with the narrow width.", spec$n_ind))
            } else if (pc$share_outside < 0.02 && pv$w <= 1) "the priors are fairly tight. Check that they do not rule out trajectories you consider plausible; if they do, the posterior will be pulled towards no effect."
            else "the priors cover the plausible range without forcing a shape, which is what weakly informative priors are for.")
  }
  paste(base, judge)
}

# ---- fitting -------------------------------------------------------------------------------------------------------
bayes_settings <- function(mode = "draft", chains = NULL, iter = NULL, warmup = NULL, adapt_delta = 0.95, max_treedepth = 10,
                           seed = 2024L, width = "default") {
  m <- BAYES_MODES[[mode %||% "draft"]] %||% BAYES_MODES$draft
  ch <- as.integer(chains %||% m$chains); it <- as.integer(iter %||% m$iter); wu <- as.integer(warmup %||% m$warmup)
  if (!is.finite(ch) || ch < 1) ch <- m$chains
  if (!is.finite(it) || it < 200) it <- m$iter
  if (!is.finite(wu) || wu < 100 || wu >= it) wu <- floor(it / 2)
  ad <- as.numeric(adapt_delta); if (!is.finite(ad) || ad <= 0.5 || ad >= 1) ad <- 0.95
  td <- as.integer(max_treedepth); if (!is.finite(td) || td < 5) td <- 10L
  list(mode = mode %||% "draft", chains = ch, iter = it, warmup = wu, adapt_delta = ad, max_treedepth = td,
       seed = as.integer(seed %||% 2024L), width = width %||% "default")
}

# Fits the model (re-using a cached compiled model when the Stan code is unchanged) and summarises it. Returns no
# brmsfit (it would be large); the fit is saved in cache_dir for later steps.
bayes_fit <- function(spec, settings = bayes_settings(), cache_dir = file.path(tempdir(), "disappR_bayes"), cores = NULL) {
  fail <- function(msg) list(ok = FALSE, message = msg)
  if (!isTRUE(spec$ok)) return(fail(spec$message %||% "Nothing to fit."))
  if (!bayes_has_brms()) return(fail(bayes_environment()$message))
  t0 <- Sys.time()
  pv <- bayes_priors(spec, settings$width)
  bp <- tryCatch(bayes_brms_priors(spec, pv), error = function(e) e)
  if (inherits(bp, "error")) return(fail(paste("The priors could not be set up:", conditionMessage(bp))))
  form <- brms::bf(stats::as.formula(spec$formula))
  fam <- bayes_family_obj(spec$brms_family)
  code <- tryCatch(brms::make_stancode(form, data = spec$data, family = fam, prior = bp$prior, stanvars = bp$stanvars),
                   error = function(e) e)
  if (inherits(code, "error")) return(fail(paste("brms could not build the model:", conditionMessage(code))))
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  key <- bayes_hash(paste(code, collapse = "\n"))
  cache_file <- file.path(cache_dir, paste0("compiled_", key, ".rds"))
  cores <- cores %||% min(settings$chains, max(1L, parallel::detectCores(logical = FALSE) %||% 1L))
  msgs <- character(0)
  compiled <- !file.exists(cache_file)
  args <- list(chains = settings$chains, iter = settings$iter, warmup = settings$warmup, cores = cores, seed = settings$seed,
               control = list(adapt_delta = settings$adapt_delta, max_treedepth = settings$max_treedepth), refresh = 0, silent = 2)
  fit <- tryCatch(withCallingHandlers({
    if (!compiled) {
      old <- readRDS(cache_file)
      do.call(stats::update, c(list(old, newdata = spec$data, stanvars = bp$stanvars, recompile = FALSE), args))
    } else {
      f <- do.call(brms::brm, c(list(formula = form, data = spec$data, family = fam, prior = bp$prior, stanvars = bp$stanvars), args))
      saveRDS(f, cache_file)
      f
    }
  }, warning = function(w) { msgs <<- c(msgs, conditionMessage(w)); invokeRestart("muffleWarning") },
     message = function(m) invokeRestart("muffleMessage")),
  error = function(e) e)
  if (inherits(fit, "error")) {
    em <- conditionMessage(fit)
    hint <- if (grepl("compil|g\\+\\+|make|Rtools|toolchain|clang", em, ignore.case = TRUE)) paste0(" ", bayes_environment()$fix) else ""
    return(fail(paste0("Stan could not fit the model: ", em, hint)))
  }
  last <- file.path(cache_dir, "last_fit.rds")
  try(saveRDS(fit, last), silent = TRUE)
  out <- bayes_summarise(fit, spec, pv, settings)
  out$compiled <- compiled
  out$seconds <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  out$warnings <- unique(msgs)
  out$fit_file <- last
  out$stancode <- paste(code, collapse = "\n")
  out
}

bayes_hash <- function(x) {
  f <- tempfile(); on.exit(unlink(f))
  writeLines(x, f)
  unname(tools::md5sum(f))
}

# ---- summaries -----------------------------------------------------------------------------------------------------
bayes_param_label <- function(p) {
  p <- sub("^b_Intercept$", "Intercept", p)
  p <- sub("^b_", "", p)
  p <- sub("^sd_(.+?)__(.*)$", "SD \\1: \\2", p)
  p <- sub("^cor_(.+?)__(.*)__(.*)$", "Correlation \\1: \\2, \\3", p)
  p <- sub("^sigma$", "Residual SD", p)
  display_term(p)
}

bayes_summarise <- function(fit, spec, pv, settings) {
  vars <- brms::variables(fit)
  keep <- grep("^(b_|sd_|cor_|sigma$)", vars, value = TRUE)
  dr <- posterior::as_draws_array(fit, variable = keep)
  sm <- suppressWarnings(posterior::summarise_draws(dr, "median", "sd", ~stats::quantile(.x, c(0.025, 0.975)), "rhat", "ess_bulk", "ess_tail"))
  sm <- as.data.frame(sm)
  names(sm)[names(sm) %in% c("2.5%", "97.5%")] <- c("lo", "hi")
  dmat <- posterior::as_draws_matrix(dr)
  pd <- vapply(sm$variable, function(v) { x <- as.numeric(dmat[, v]); max(mean(x > 0), mean(x < 0)) }, numeric(1))
  sm$pd <- ifelse(grepl("^b_", sm$variable), pd, NA_real_)
  # convergence over every parameter, the individual random effects included
  all_dr <- posterior::as_draws_array(fit)
  allsum <- suppressWarnings(posterior::summarise_draws(all_dr, "rhat", "ess_bulk", "ess_tail"))
  np <- brms::nuts_params(fit)
  div <- sum(np$Value[np$Parameter == "divergent__"])
  tdh <- sum(np$Value[np$Parameter == "treedepth__"] >= settings$max_treedepth)
  bfmi <- tryCatch(as.numeric(rstan::get_bfmi(fit$fit)), error = function(e) NA_real_)
  diag <- bayes_diagnostics(allsum, div, tdh, bfmi, settings)
  # population curve and its turning points from the fixed-effect draws
  bnames <- grep("^b_", colnames(dmat), value = TRUE)
  B <- t(as.matrix(dmat[, bnames, drop = FALSE]))
  rownames(B) <- sub("^b_", "", rownames(B))
  rownames(B)[rownames(B) == "Intercept"] <- "(Intercept)"
  n_curve <- min(1000, ncol(B))
  idx <- unique(round(seq(1, ncol(B), length.out = n_curve)))
  M <- bayes_curve_matrix(spec, B[, idx, drop = FALSE])
  curve <- bayes_curve_summary(M, spec$ages)
  tps <- vapply(seq_len(ncol(M)), function(j) {
    t <- curve_turning_points(spec$ages, M[, j])
    c(t$peak, as.numeric(t$peak_interior), t$onset, as.numeric(identical(t$onset_type, "interior")))
  }, numeric(4))
  turning <- list(peak = tps[1, tps[2, ] == 1], onset = tps[3, tps[4, ] == 1], share_peak = mean(tps[2, ] == 1), share_onset = mean(tps[4, ] == 1),
                  median_curve = curve_turning_points(spec$ages, curve$median))
  # posterior predictive draws for the checks (50 replicated data sets, random effects of the observed individuals)
  yrep <- tryCatch(brms::posterior_predict(fit, ndraws = 50), error = function(e) NULL)
  y <- if (".succ" %in% names(spec$data)) spec$data$.succ else spec$data$trait
  ppc <- if (!is.null(yrep)) bayes_ppc_data(y, yrep, spec) else NULL
  # trace draws of the key parameters (up to 6), by chain
  key <- head(c(intersect(c("b_Intercept"), keep), setdiff(grep("^b_", keep, value = TRUE), "b_Intercept"), grep("^(sd_|sigma$)", keep, value = TRUE)), 6)
  arr <- posterior::as_draws_array(fit, variable = key)
  trace <- do.call(rbind, lapply(key, function(k) {
    a <- arr[, , k, drop = TRUE]
    if (is.null(dim(a))) a <- matrix(a, ncol = 1)
    data.frame(parameter = bayes_param_label(k), iteration = rep(seq_len(nrow(a)), ncol(a)), chain = factor(rep(seq_len(ncol(a)), each = nrow(a))),
               value = as.vector(a), stringsAsFactors = FALSE)
  }))
  est <- data.frame(Parameter = bayes_param_label(sm$variable), Raw = sm$variable, Median = sm$median, SD = sm$sd, Lower = sm$lo, Upper = sm$hi,
                    PD = sm$pd, Rhat = sm$rhat, ESS_bulk = sm$ess_bulk, ESS_tail = sm$ess_tail, stringsAsFactors = FALSE)
  list(ok = TRUE, model = spec$model, label = spec$label, formula = spec$formula, family = spec$family, brms_family = spec$brms_family,
       settings = settings, priors = pv$table, prior_meaning = pv$meaning, prior_values = pv$values, prior_b_sd = pv$b_sd,
       estimates = est, diagnostics = diag, curve = curve, turning = turning, ppc = ppc, trace = trace,
       n = nrow(spec$data), n_ind = length(unique(spec$data$id)), draws = ncol(B),
       versions = c(brms = as.character(utils::packageVersion("brms")), rstan = as.character(utils::packageVersion("rstan")),
                    Stan = tryCatch(rstan::stan_version(), error = function(e) "")))
}

# Plain traffic lights for every convergence check, each with what it means and what to do.
bayes_diagnostics <- function(allsum, div, tdh, bfmi, settings) {
  rh <- suppressWarnings(max(allsum$rhat, na.rm = TRUE))
  eb <- suppressWarnings(min(allsum$ess_bulk, na.rm = TRUE))
  et <- suppressWarnings(min(allsum$ess_tail, na.rm = TRUE))
  ess_ok <- 100 * settings$chains
  n_post <- settings$chains * (settings$iter - settings$warmup)
  lb <- suppressWarnings(min(bfmi, na.rm = TRUE))
  row <- function(check, value, status, meaning, todo) data.frame(Check = check, Value = value, Status = status, Meaning = meaning, `What to do` = todo,
                                                                  check.names = FALSE, stringsAsFactors = FALSE)
  rows <- rbind(
    row("R-hat (largest)", format_num(rh, 4), if (!is.finite(rh)) "caution" else if (rh < 1.01) "good" else if (rh < 1.05) "caution" else "problem",
        "Do the chains agree? Each chain started from a different place; R-hat compares them. Below 1.01 they agree (Vehtari et al. 2021).",
        if (is.finite(rh) && rh >= 1.01) "Run longer (the 'Full' run, or more iterations). Look at the trace plots: chains that wander apart point to a poorly identified term; simplify the random effects or narrow the priors." else "Nothing."),
    row("Bulk ESS (smallest)", format(round(eb), big.mark = ","), if (!is.finite(eb)) "caution" else if (eb >= ess_ok) "good" else if (eb >= 100) "caution" else "problem",
        sprintf("How many independent draws the %s correlated draws are worth, for medians and means. At least 100 per chain (%d here).", format(n_post, big.mark = ","), ess_ok),
        if (is.finite(eb) && eb < ess_ok) "Run more iterations. A low ESS for a random-effect SD is common and usually needs a longer run." else "Nothing."),
    row("Tail ESS (smallest)", format(round(et), big.mark = ","), if (!is.finite(et)) "caution" else if (et >= ess_ok) "good" else if (et >= 100) "caution" else "problem",
        "The same for the 2.5% and 97.5% limits of the credible intervals, which need more draws than the median.",
        if (is.finite(et) && et < ess_ok) "Run more iterations before reporting intervals." else "Nothing."),
    row("Divergent transitions", format(div), if (div == 0) "good" else "problem",
        "The sampler hit regions of the posterior it could not explore accurately. Even a few mean the results can be biased.",
        if (div > 0) "Raise adapt_delta (Advanced, e.g. 0.99). If they persist, the model is hard to sample: simplify the random effects (e.g. no correlated slope), or use narrower priors on the SDs." else "Nothing."),
    row("Maximum tree depth reached", format(tdh), if (tdh == 0) "good" else "caution",
        "Efficiency only: the sampler took the longest steps allowed. The results are valid but sampling was slow.",
        if (tdh > 0) "Raise the maximum tree depth (Advanced), or simplify the model." else "Nothing."),
    row("E-BFMI (smallest chain)", if (is.finite(lb)) format_num(lb, 3) else "n/a", if (!is.finite(lb)) "caution" else if (lb >= 0.3) "good" else "caution",
        "Whether the sampler moved freely between high and low regions of the posterior. Below 0.3 is a warning.",
        if (is.finite(lb) && lb < 0.3) "Usually a sign of a hard-to-sample variance: try narrower priors on the SDs or a simpler random-effect structure." else "Nothing."))
  worst <- if (any(rows$Status == "problem")) "problem" else if (any(rows$Status == "caution")) "caution" else "good"
  attr(rows, "overall") <- worst
  rows
}

bayes_overall <- function(z) {
  ov <- attr(z$diagnostics, "overall") %||% "caution"
  draft <- identical(z$settings$mode, "draft")
  txt <- switch(ov,
    good = "Every convergence check passed: the draws describe the posterior well.",
    caution = "Some convergence checks need attention (see the table): read the results as provisional.",
    "At least one convergence check failed: do not interpret the results until it is fixed (see 'What to do').")
  if (draft) txt <- paste(txt, "This was a draft run (2 chains, 500 warm-up and 500 sampling iterations each): good for exploring, not for reporting. Use the full run before saving results for a paper.")
  list(status = if (draft && identical(ov, "good")) "caution" else ov, text = txt)
}

# Data for the posterior predictive checks: densities (continuous) or frequencies (discrete) of the data and the
# replicated data, and the mean at each age.
bayes_ppc_data <- function(y, yrep, spec) {
  age <- spec$data$age
  ua <- sort(unique(age))
  grp <- if (length(ua) > 25) cut(age, unique(stats::quantile(age, seq(0, 1, length.out = 21))), include.lowest = TRUE) else factor(age)
  gmid <- tapply(age, grp, mean)
  obs_mean <- tapply(y, grp, mean)
  rep_mean <- apply(yrep, 1, function(r) tapply(r, grp, mean))
  if (is.null(dim(rep_mean))) rep_mean <- matrix(rep_mean, nrow = 1)
  by_age <- data.frame(age = as.numeric(gmid), observed = as.numeric(obs_mean), n = as.numeric(table(grp)),
                       median = apply(rep_mean, 1, stats::median), lo = apply(rep_mean, 1, stats::quantile, 0.05),
                       hi = apply(rep_mean, 1, stats::quantile, 0.95))
  by_age$inside <- by_age$observed >= by_age$lo & by_age$observed <= by_age$hi
  discrete <- !identical(spec$family, "gaussian")
  dist <- if (discrete) {
    vmax <- min(max(c(y, yrep), na.rm = TRUE), 30)
    vals <- 0:vmax
    obs <- vapply(vals, function(v) mean(y == v), numeric(1))
    reps <- vapply(vals, function(v) rowMeans(yrep == v), numeric(nrow(yrep)))
    if (is.null(dim(reps))) reps <- matrix(reps, nrow = 1)
    data.frame(value = vals, observed = obs, median = apply(reps, 2, stats::median), lo = apply(reps, 2, stats::quantile, 0.05),
               hi = apply(reps, 2, stats::quantile, 0.95))
  } else NULL
  stat <- function(f) { o <- f(y); r <- apply(yrep, 1, f); c(observed = o, p = mean(r >= o)) }
  stats_tab <- rbind(
    c(Statistic = "Mean", round(stat(mean), 4)),
    c(Statistic = "SD", round(stat(stats::sd), 4)),
    if (discrete && identical(spec$family, "poisson")) c(Statistic = "Proportion of zeros", round(stat(function(v) mean(v == 0)), 4)),
    c(Statistic = "Maximum", round(stat(max), 4)))
  stats_tab <- as.data.frame(stats_tab, stringsAsFactors = FALSE)
  names(stats_tab) <- c("Statistic", "Observed", "Share of replicates at or above the observed value")
  list(y = y, yrep = yrep[seq_len(min(30, nrow(yrep))), , drop = FALSE], by_age = by_age, dist = dist, stats = stats_tab, discrete = discrete)
}

bayes_ppc_reading <- function(ppc) {
  if (is.null(ppc)) return("The posterior predictive checks could not be computed.")
  out <- sum(!ppc$by_age$inside)
  st <- suppressWarnings(as.numeric(ppc$stats[[3]]))
  extreme <- ppc$stats$Statistic[is.finite(st) & (st < 0.05 | st > 0.95)]
  paste0(
    sprintf("The observed mean falls inside the 90%% range of the replicated data at %d of %d ages", sum(ppc$by_age$inside), nrow(ppc$by_age)),
    if (out > max(1, 0.1 * nrow(ppc$by_age))) " - more misses than the roughly 10% expected by chance: the ageing function may be too simple for these data (compare a more flexible function in step 5, or the GAMM of step 7)." else ", about what is expected by chance.",
    if (length(extreme)) paste0(" The model reproduces the data poorly for: ", paste(tolower(extreme), collapse = ", "),
                                " (the observed value is in the outer 5% of replicates).",
                                if ("Proportion of zeros" %in% extreme) " Too many or too few zeros suggest a zero-inflated or negative binomial family." else
                                if ("SD" %in% extreme) " A spread the model cannot reproduce suggests overdispersion or a variance that changes with age." else "")
    else " Mean, spread and maximum of the data are all within the range the model reproduces.")
}

# Numbers for display: three significant digits each, without the common-width padding of format() on a vector.
bayes_fmt <- function(x, digits = 3) vapply(x, function(v) if (is.finite(v)) format_num(v, digits) else "", character(1), USE.NAMES = FALSE)
bayes_ci <- function(lo, hi) ifelse(is.finite(lo) & is.finite(hi), paste(bayes_fmt(lo), "to", bayes_fmt(hi)), "")

# Fixed effects of the Bayesian fit side by side with the step-5 estimates.
bayes_compare_table <- function(z, spec) {
  e <- z$estimates[grepl("^b_", z$estimates$Raw), , drop = FALSE]
  raw <- sub("^b_", "", e$Raw); raw[raw == "Intercept"] <- "(Intercept)"
  fc <- spec$freq_coef
  mi <- if (is.data.frame(fc) && nrow(fc)) match(raw, fc$Raw_term) else rep(NA_integer_, length(raw))
  fe <- if (length(mi)) fc$Estimate[mi] else NA_real_
  fse <- if (length(mi)) fc$SE[mi] else NA_real_
  dirn <- ifelse(e$Median >= 0, "> 0", "< 0")
  data.frame(Term = e$Parameter,
             `Posterior median` = bayes_fmt(e$Median), `95% credible interval` = bayes_ci(e$Lower, e$Upper),
             `P(direction)` = sprintf("%.3f (%s)", e$PD, dirn),
             `Step 5 estimate` = bayes_fmt(fe), `Step 5 95% CI` = bayes_ci(fe - 1.96 * fse, fe + 1.96 * fse),
             `Shift (step-5 SEs)` = ifelse(is.finite(fse) & fse > 0, sprintf("%+.2f", (e$Median - fe) / fse), ""),
             check.names = FALSE, stringsAsFactors = FALSE)
}

bayes_variance_table <- function(z, spec) {
  e <- z$estimates[!grepl("^b_", z$estimates$Raw), , drop = FALSE]
  if (!nrow(e)) return(data.frame())
  data.frame(Parameter = e$Parameter, `Posterior median` = bayes_fmt(e$Median), `95% credible interval` = bayes_ci(e$Lower, e$Upper),
             check.names = FALSE, stringsAsFactors = FALSE)
}

bayes_peak_table <- function(z) {
  t <- z$turning
  ci <- function(x) if (length(x) >= 20) paste(bayes_fmt(stats::quantile(x, c(0.025, 0.975))), collapse = " to ") else "not estimable"
  data.frame(Feature = c("Peak age", "Onset of the final decline"),
             `Median curve` = c(format_num(t$median_curve$peak, 3), if (is.finite(t$median_curve$onset)) format_num(t$median_curve$onset, 3) else "none"),
             `95% credible interval` = c(ci(t$peak), ci(t$onset)),
             `Share of draws with this feature inside the age range` = sprintf("%.0f%%", 100 * c(t$share_peak, t$share_onset)),
             check.names = FALSE, stringsAsFactors = FALSE)
}

# Plain-language reading of the fit: convergence, the ageing and selective-disappearance terms, and agreement with
# step 5.
bayes_reading <- function(z, spec) {
  ov <- bayes_overall(z)
  e <- z$estimates[grepl("^b_", z$estimates$Raw) & z$estimates$Raw != "b_Intercept", , drop = FALSE]
  pd_line <- function(i) {
    sprintf("%s: %s (95%% credible interval %s to %s); the probability that it is %s is %.2f.", e$Parameter[i], format_num(e$Median[i], 3),
            format_num(e$Lower[i], 3), format_num(e$Upper[i], 3), if (e$Median[i] >= 0) "positive" else "negative", e$PD[i])
  }
  sel <- grep("ALR|LS|AFR|mean_", e$Raw)
  age <- setdiff(grep("f[1-3]|delta_", e$Raw), sel)
  ct <- bayes_compare_table(z, spec)
  shift <- suppressWarnings(abs(as.numeric(ct$`Shift (step-5 SEs)`)))
  agree <- if (any(is.finite(shift))) {
    if (max(shift, na.rm = TRUE) < 0.5) "Every coefficient is within half a standard error of its step-5 estimate: with these data the priors matter little and both approaches tell the same story."
    else sprintf("The largest shift from step 5 is %.1f standard errors (%s). Shifts come from the priors (stronger with few data or wide priors on the logit scale) or from poor convergence; check the convergence table and try another prior width.",
                 max(shift, na.rm = TRUE), ct$Term[which.max(shift)])
  } else ""
  c(ov$text,
    if (length(age)) paste("Ageing terms.", paste(vapply(age, pd_line, ""), collapse = " ")),
    if (length(sel)) paste("Selective disappearance and appearance terms.", paste(vapply(sel, pd_line, ""), collapse = " ")),
    agree,
    "A credible interval is the range that contains the coefficient with 95% probability, given the data, the model and the priors. The probability of direction is not a p-value: 0.975 corresponds roughly to a two-sided p of 0.05, but read it as a degree of certainty, not a pass/fail test.")
}

# ---- exported R code and methods paragraph ----------------------------------------------------------------------------
bayes_code <- function(z, spec) {
  pv <- list(values = z$prior_values, b_sd = z$prior_b_sd)
  s <- z$settings
  fam <- switch(spec$brms_family, gaussian = "gaussian()", poisson = "poisson()", bernoulli = "bernoulli()", binomial = "binomial()")
  paste(c(
    "# Bayesian refit of a disappR step-5 model with brms (step 8).",
    sprintf("# %s, %s family; %d records from %d individuals. Variables as prepared by disappR (age and proxies standardised).", spec$label,
            spec$family, spec$n, spec$n_ind),
    "library(brms)",
    "dat <- read.csv(\"disappR_bayes_data.csv\")   # 'Download the model's data' on this tab",
    "dat$id <- factor(dat$id)",
    bayes_prior_code(spec, pv),
    sprintf("fit <- brm(%s,\n           data = dat, family = %s, prior = priors,\n           chains = %d, iter = %d, warmup = %d, cores = %d, seed = %d,\n           control = list(adapt_delta = %s, max_treedepth = %d))",
            spec$formula, fam, s$chains, s$iter, s$warmup, s$chains, s$seed, s$adapt_delta, s$max_treedepth),
    "summary(fit)                  # estimates, R-hat, bulk and tail ESS",
    "plot(fit)                     # trace plots",
    "pp_check(fit, ndraws = 50)    # posterior predictive check",
    "prior_summary(fit)            # the priors used"), collapse = "\n")
}

bayes_methods <- function(z, spec) {
  s <- z$settings
  ov <- attr(z$diagnostics, "overall")
  rh <- z$diagnostics$Value[z$diagnostics$Check == "R-hat (largest)"]
  eb <- z$diagnostics$Value[z$diagnostics$Check == "Bulk ESS (smallest)"]
  et <- z$diagnostics$Value[z$diagnostics$Check == "Tail ESS (smallest)"]
  dv <- z$diagnostics$Value[z$diagnostics$Check == "Divergent transitions"]
  paste0("We refitted ", spec$label, " in a Bayesian framework with brms ", z$versions[["brms"]], " (B\u00fcrkner 2017) and Stan ",
         z$versions[["Stan"]], " (via rstan ", z$versions[["rstan"]], "), with the same formula, ", spec$family, " error distribution and records (",
         format(spec$n, big.mark = ","), " records from ", format(spec$n_ind, big.mark = ","), " individuals). ",
         "Fixed effects had weakly informative normal priors centred on zero with standard deviation ", format_num(bayes_width_value(s$width)), if (identical(spec$family, "gaussian")) " \u00d7 SD(trait)" else "", " / SD(predictor)",
         " (autoscaled as in rstanarm); the intercept a Student-t(3) prior centred on the mean trait, and random-effect standard deviations",
         if (identical(spec$family, "gaussian")) " and the residual standard deviation" else "", " half Student-t(3) priors",
         if (grepl("^correlated", spec$random_slope %||% "")) "; correlations an LKJ(2) prior" else "", ". ",
         "We ran ", s$chains, " chains of ", format(s$iter, big.mark = ","), " iterations (", format(s$warmup, big.mark = ","), " warm-up; adapt_delta = ", s$adapt_delta, "). ",
         "Convergence was assessed with rank-normalised R-hat and bulk and tail effective sample sizes (Vehtari et al. 2021) and by checking for divergent transitions: ",
         "the largest R-hat was ", rh, ", the smallest bulk and tail ESS ", eb, " and ", et, ", with ", dv, " divergent transitions",
         if (!identical(ov, "good")) " (some checks did not pass; see the diagnostics)" else "", ". ",
         "Prior predictive curves were inspected before fitting and posterior predictive checks after (Gabry et al. 2019). ",
         "We report posterior medians with 95% equal-tailed credible intervals and the probability of direction.")
}
