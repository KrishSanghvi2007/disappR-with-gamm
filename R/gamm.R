# ---------------------------------------------------------------------------
# GAMM layer (0.23.0): generalized additive mixed models of ageing, fitted with mgcv (or gamm4). An independent layer:
# it reads the standardised data and meta of the app and changes nothing else. Every function here is prefixed gamm_.
# Approach (Wood 2017; Pedersen et al. 2019, PeerJ; as applied to ageing by, e.g., Tsui et al. 2026, Evol. Lett.):
# a penalised smooth of age fitted by REML, individual random effects as penalised smooths, selective disappearance
# and appearance through ALR, lifespan or AFR terms (additive, smooth, or age-dependent through ti()), group-specific
# smooths compared as models G, GS, GI, S and I, and ageing read from the first derivative of the age smooth.
# ---------------------------------------------------------------------------

gamm_has_mgcv <- function() requireNamespace("mgcv", quietly = TRUE)
gamm_has_gamm4 <- function() requireNamespace("gamm4", quietly = TRUE)

GAMM_BASES <- c("Thin plate (tp)" = "tp", "Thin plate, shrinkage (ts)" = "ts", "Cubic regression (cr)" = "cr",
                "Cubic regression, shrinkage (cs)" = "cs", "P-spline (ps)" = "ps", "B-spline (bs)" = "bs", "Adaptive (ad)" = "ad")
GAMM_FAMILIES <- c("Gaussian" = "gaussian", "Gaussian, variance smooth in age (gaulss)" = "gaulss",
                   "Gamma (log link)" = "Gamma", "Beta (betar)" = "betar", "Poisson" = "poisson",
                   "Negative binomial (nb)" = "nb", "Zero-inflated Poisson (ziP)" = "ziP", "Tweedie (tw)" = "tw",
                   "Binomial" = "binomial")
GAMM_STRUCTURES <- c("G: one smooth for everyone" = "G",
                     "GS: global smooth + group deviations, shared wiggliness" = "GS",
                     "GI: global smooth + group deviations, own wiggliness" = "GI",
                     "S: a smooth per group, shared wiggliness" = "S",
                     "I: a separate smooth per group" = "I")
GAMM_INDIVIDUAL <- c("Random intercept" = "ri", "Random intercept and slope" = "rs",
                     "Random smooth per individual (fs)" = "fs", "None" = "none")
GAMM_PROXY_FORMS <- c("None" = "none", "Additive, linear" = "linear", "Additive, smooth" = "smooth",
                      "Age-dependent: smooth + ti(age, proxy)" = "ti")
GAMM_ENGINES <- c("mgcv::gam" = "gam", "mgcv::bam (large data)" = "bam", "gamm4 (lme4)" = "gamm4")

gamm_defaults <- function() {
  list(engine = "gam", method = "REML", family = "gaussian", bs = "tp", k = 10, m = NA_real_, knots = "",
       select = FALSE, individual = "ri", group = "", structure = "G", proxy = "ALR", proxy_form = "linear",
       afr_form = "none", covars = TRUE, random = TRUE)
}

gamm_family_obj <- function(f) {
  switch(f, gaussian = stats::gaussian(), gaulss = mgcv::gaulss(), Gamma = stats::Gamma(link = "log"),
         betar = mgcv::betar(link = "logit"), poisson = stats::poisson(), nb = mgcv::nb(), ziP = mgcv::ziP(),
         tw = mgcv::tw(), binomial = stats::binomial(), stats::gaussian())
}
gamm_family_label <- function(f) { l <- names(GAMM_FAMILIES)[GAMM_FAMILIES == f]; if (length(l)) l[[1]] else f }

# The analysis data: one row per record with the trait, age, individual, the lifespan proxies (one value per
# individual), mapped covariates, grouping and random-effect columns, and binomial trials.
gamm_data <- function(dat, meta) {
  d <- dat[is.finite(dat$trait) & is.finite(dat$age), , drop = FALSE]
  if (!nrow(d)) return(d)
  out <- data.frame(id = factor(d$id), age = as.numeric(d$age), trait = as.numeric(d$trait), stringsAsFactors = FALSE)
  first_of <- function(v) { v <- as.numeric(v); stats::ave(v, d$id, FUN = function(x) { x <- x[is.finite(x)]; if (length(x)) x[[1]] else NA_real_ }) }
  out$ALR <- if (!is.null(d$alr)) first_of(d$alr) else stats::ave(out$age, d$id, FUN = max)
  out$LS <- if (!is.null(d$life)) first_of(d$life) else NA_real_
  out$AFR <- if (!is.null(d$entry)) first_of(d$entry) else stats::ave(out$age, d$id, FUN = min)
  for (cv in meta$covars %||% character(0)) {
    if (!cv %in% names(d)) next
    out[[cv]] <- if (identical(meta$cov_types[[cv]], "categorical")) factor(d[[cv]]) else as.numeric(d[[cv]])
  }
  for (rt in meta$random_terms %||% character(0)) if (rt %in% names(d)) out[[rt]] <- factor(d[[rt]])
  if (isTRUE(meta$has_group) && !is.null(d$group)) out$group <- factor(d$group)
  if (isTRUE(meta$has_group2) && !is.null(d$group2)) out$group2 <- factor(d$group2)
  if (isTRUE(meta$has_trials) && !is.null(d$.trials)) out$.trials <- as.numeric(d$.trials)
  out
}

# Categorical variables that can define group-specific smooths (sex, treatment...): categorical covariates and the
# grouping columns with 2 to 8 levels.
gamm_group_choices <- function(meta, gd) {
  cand <- c(stats::setNames(meta$covars %||% character(0), unname(as.character(meta$cov_labels[meta$covars %||% character(0)]))))
  cand <- cand[vapply(cand, function(v) identical(meta$cov_types[[v]], "categorical"), logical(1))]
  if (isTRUE(meta$has_group)) cand <- c(cand, stats::setNames("group", meta$map$group %||% "group"))
  if (!nrow(gd)) return(character(0))
  ok <- vapply(cand, function(v) v %in% names(gd) && nlevels(droplevels(factor(gd[[v]]))) %in% 2:8, logical(1))
  cand[ok]
}

gamm_knots <- function(s) {
  if (!nzchar(s$knots %||% "") || !s$bs %in% c("cr", "cs")) return(NULL)
  kn <- suppressWarnings(as.numeric(strsplit(gsub(" ", "", s$knots), ",")[[1]]))
  kn <- sort(unique(kn[is.finite(kn)]))
  if (length(kn) < 3) NULL else list(age = kn)
}

gamm_age_smooth <- function(s, by = NULL, m = s$m, k = s$k, extra = "") {
  kn <- gamm_knots(s)
  k_use <- if (!is.null(kn)) length(kn$age) else k
  parts <- c(sprintf("bs = \"%s\"", s$bs), sprintf("k = %d", as.integer(k_use)),
             if (is.finite(m %||% NA_real_)) sprintf("m = %d", as.integer(m)), if (!is.null(by)) sprintf("by = %s", by), if (nzchar(extra)) extra)
  sprintf("s(age, %s)", paste(parts, collapse = ", "))
}

# The model formula (a string) for the settings; for gamm4 the individual random effects go in `random` instead.
gamm_formula <- function(s, gd, structure = s$structure, proxy_form = s$proxy_form, afr_form = s$afr_form,
                         age_poly = 0L, ordered_by = FALSE) {
  terms <- character(0)
  grp <- if (nzchar(s$group %||% "") && s$group %in% names(gd)) s$group else ""
  if (!nzchar(grp)) structure <- "G"
  k <- as.integer(s$k)
  if (age_poly > 0) {
    terms <- c(terms, if (age_poly == 1) "age" else sprintf("poly(age, %d, raw = TRUE)", as.integer(age_poly)))
    if (!identical(structure, "G") && nzchar(grp)) terms <- c(terms, grp)
  } else if (isTRUE(ordered_by)) {
    terms <- c(terms, ".grp_o", gamm_age_smooth(s), gamm_age_smooth(s, by = ".grp_o"))
  } else {
    fs_xt <- sprintf("xt = list(bs = \"%s\")", if (s$bs %in% c("ad")) "tp" else s$bs)
    terms <- c(terms, switch(structure,
      G = gamm_age_smooth(s),
      GS = c(gamm_age_smooth(s, m = 2), sprintf("s(age, %s, bs = \"fs\", k = %d, m = 2, %s)", grp, k, fs_xt)),
      GI = c(gamm_age_smooth(s, m = 2), gamm_age_smooth(s, by = grp, m = 1), grp),
      S = sprintf("s(age, %s, bs = \"fs\", k = %d, m = 2, %s)", grp, k, fs_xt),
      I = c(gamm_age_smooth(s, by = grp), grp)))
  }
  prox_terms <- function(p, form) {
    if (identical(form, "none") || !p %in% names(gd) || sum(is.finite(gd[[p]])) < 5) return(character(0))
    nu <- length(unique(gd[[p]][is.finite(gd[[p]])]))
    kp <- max(3L, min(5L, nu - 1L))
    switch(form, linear = p, smooth = sprintf("s(%s, k = %d)", p, kp),
           ti = c(sprintf("s(%s, k = %d)", p, kp), sprintf("ti(age, %s, k = c(%d, %d))", p, min(5L, k), kp)), character(0))
  }
  if (!identical(s$proxy %||% "none", "none")) terms <- c(terms, prox_terms(s$proxy, proxy_form))
  terms <- c(terms, prox_terms("AFR", afr_form))
  if (isTRUE(s$covars)) {
    cv <- grep("^cv_", names(gd), value = TRUE)
    # a factor-smooth ("fs") by group already carries each group's level, so the group is not added again
    if (structure %in% c("GS", "S")) cv <- setdiff(cv, grp)
    terms <- c(terms, cv)
  }
  if (!identical(s$engine, "gamm4")) {
    terms <- c(terms, switch(s$individual, ri = "s(id, bs = \"re\")", rs = c("s(id, bs = \"re\")", "s(id, age, bs = \"re\")"),
                             fs = sprintf("s(age, id, bs = \"fs\", k = %d, m = 1)", min(5L, k)), character(0)))
    if (isTRUE(s$random)) for (rt in grep("^re_", names(gd), value = TRUE)) terms <- c(terms, sprintf("s(%s, bs = \"re\")", rt))
  }
  paste("trait ~", paste(unique(terms), collapse = " + "))
}

gamm_random_formula <- function(s, gd) {
  re <- switch(s$individual, rs = "(1 + age | id)", none = character(0), "(1 | id)")
  if (isTRUE(s$random)) re <- c(re, sprintf("(1 | %s)", grep("^re_", names(gd), value = TRUE)))
  if (!length(re)) return(NULL)
  stats::as.formula(paste("~", paste(re, collapse = " + ")))
}

# Fit one GAMM. Returns list(ok, fit (the mgcv gam object), mer (gamm4 only), formula, data, settings, message, notes).
gamm_fit <- function(gd, s, method = s$method, structure = s$structure, proxy_form = s$proxy_form,
                     afr_form = s$afr_form, age_poly = 0L, ordered_by = FALSE) {
  fail <- function(msg) list(ok = FALSE, message = msg)
  if (!gamm_has_mgcv()) return(fail("The GAMM layer needs the mgcv package: install.packages('mgcv')."))
  if (!is.data.frame(gd) || nrow(gd) < 20) return(fail("At least 20 records are needed to fit a GAMM."))
  notes <- character(0)
  fam <- s$family
  y <- gd$trait
  if (fam %in% c("Gamma") && any(y <= 0)) return(fail("The Gamma family needs positive values: choose Gaussian or tw."))
  if (identical(fam, "betar") && any(y <= 0 | y >= 1)) return(fail("The beta family needs values strictly between 0 and 1."))
  if (fam %in% c("poisson", "nb", "ziP") && any(y < 0 | abs(y - round(y)) > 1e-8)) return(fail("Count families need non-negative whole numbers."))
  if (identical(fam, "tw") && any(y < 0)) return(fail("The Tweedie family needs non-negative values."))
  nu <- length(unique(gd$age))
  if (nu < 4) return(fail("A smooth of age needs at least 4 distinct ages."))
  k0 <- as.integer(s$k)
  s$k <- max(3L, min(k0, nu - 1L))
  if (s$k < k0) notes <- c(notes, "k was lowered to the number of distinct ages minus one")
  if (isTRUE(ordered_by)) {
    gd$.grp_o <- factor(gd[[s$group]], ordered = TRUE)
    contrasts(gd$.grp_o) <- "contr.treatment"
  }
  f_str <- gamm_formula(s, gd, structure, proxy_form, afr_form, age_poly, ordered_by)
  f <- stats::as.formula(f_str)
  needed <- intersect(all.vars(f), names(gd))
  keep <- stats::complete.cases(gd[, needed, drop = FALSE])
  if (fam == "binomial" && ".trials" %in% names(gd)) keep <- keep & is.finite(gd$.trials) & gd$.trials > 0
  dd <- droplevels(gd[keep, , drop = FALSE])
  if (sum(!keep)) notes <- c(notes, sprintf("%d record(s) with missing values in model variables were left out", sum(!keep)))
  if (nrow(dd) < 20) return(fail("Too few complete records for this model."))
  wts <- if (fam == "binomial" && ".trials" %in% names(dd)) dd$.trials else NULL
  famobj <- gamm_family_obj(fam)
  if (identical(fam, "gaulss")) f <- list(f, stats::as.formula(paste("~", gamm_age_smooth(s))))
  kn <- gamm_knots(s)
  fit <- NULL; mer <- NULL; err <- NULL
  t0 <- proc.time()[["elapsed"]]
  res <- tryCatch(withCallingHandlers({
    if (identical(s$engine, "gamm4")) {
      if (!gamm_has_gamm4()) stop("gamm4 is not installed: install.packages('gamm4'), or choose mgcv.")
      if (!fam %in% c("gaussian", "poisson", "binomial", "Gamma")) stop("gamm4 fits only Gaussian, Poisson, binomial and Gamma families.")
      g4 <- gamm4::gamm4(f, random = gamm_random_formula(s, dd), family = famobj, data = dd, weights = wts,
                         REML = identical(method, "REML"), knots = kn)
      fit <- g4$gam; mer <- g4$mer
    } else if (identical(s$engine, "bam")) {
      fit <- mgcv::bam(f, family = famobj, data = dd, weights = wts, method = if (identical(method, "ML")) "ML" else "fREML",
                       select = isTRUE(s$select), knots = kn, discrete = !identical(method, "ML"))
    } else {
      fit <- mgcv::gam(f, family = famobj, data = dd, weights = wts, method = method, select = isTRUE(s$select), knots = kn)
    }
    TRUE
  }, warning = function(w) { notes <<- c(notes, conditionMessage(w)); invokeRestart("muffleWarning") }),
  error = function(e) { err <<- conditionMessage(e); FALSE })
  if (!isTRUE(res) || is.null(fit)) return(fail(paste("The GAMM could not be fitted:", err %||% "unknown error")))
  list(ok = TRUE, fit = fit, mer = mer, formula = f_str, data = dd, settings = s, method = method, structure = structure,
       proxy_form = proxy_form, afr_form = afr_form, notes = unique(notes), n = nrow(dd), n_ind = nlevels(dd$id),
       seconds = proc.time()[["elapsed"]] - t0)
}

gamm_aic <- function(r) {
  if (!isTRUE(r$ok)) return(NA_real_)
  if (!is.null(r$mer)) return(stats::AIC(r$mer))
  tryCatch(stats::AIC(r$fit), error = function(e) NA_real_)
}

# Summary tables: parametric terms and smooth terms (edf, reference df, statistic, approximate p-value).
gamm_tables <- function(r) {
  sm <- summary(r$fit)
  pt <- as.data.frame(sm$p.table)
  pt <- data.frame(Term = rownames(pt), Estimate = pt[[1]], SE = pt[[2]], Statistic = pt[[3]], P = pt[[4]], stringsAsFactors = FALSE)
  st <- if (!is.null(sm$s.table) && nrow(sm$s.table)) {
    x <- as.data.frame(sm$s.table)
    data.frame(Smooth = rownames(x), edf = x[[1]], Ref.df = x[[2]], Statistic = x[[3]], P = x[[4]], stringsAsFactors = FALSE)
  } else data.frame()
  list(parametric = pt, smooth = st, dev_expl = sm$dev.expl, r_sq = sm$r.sq, n = sm$n,
       aic = gamm_aic(r), score = tryCatch(as.numeric(r$fit$gcv.ubre), error = function(e) NA_real_))
}

# Basis-dimension check: k', edf, k-index and its p-value per smooth (mgcv::k.check). A small p-value with k-index
# below 1 suggests k is too low, especially when edf is close to k'.
gamm_kcheck <- function(r) {
  kc <- tryCatch(mgcv::k.check(r$fit, subsample = 5000, n.rep = 200), error = function(e) NULL)
  if (is.null(kc) || !nrow(kc)) return(data.frame())
  data.frame(Smooth = rownames(kc), k = kc[, 1], edf = kc[, 2], k_index = kc[, 3], P = kc[, 4],
             Flag = ifelse(is.finite(kc[, 4]) & kc[, 4] < 0.05 & kc[, 3] < 1, "k may be too low", ""), stringsAsFactors = FALSE)
}

# Concurvity (the smooth analogue of collinearity): worst-case values above 0.8 flag terms that the others can mimic,
# for example age and ALR in short-lived populations.
gamm_concurvity <- function(r) {
  cc <- tryCatch(mgcv::concurvity(r$fit, full = TRUE), error = function(e) NULL)
  if (is.null(cc)) return(data.frame())
  if (is.list(cc) && !is.matrix(cc)) cc <- cc[[1]]
  data.frame(Term = colnames(cc), Worst = cc["worst", ], Observed = cc["observed", ], Estimate = cc["estimate", ],
             Flag = ifelse(cc["worst", ] > 0.8, "high", ""), stringsAsFactors = FALSE, row.names = NULL)
}

# Which smooths belong to individuals and random effects (left out of population curves).
gamm_random_smooths <- function(fit) {
  sm <- fit$smooth
  if (!length(sm)) return(list(labels = character(0), cols = integer(0)))
  is_re <- vapply(sm, function(x) any(x$term %in% "id") || any(grepl("^re_", x$term)) || inherits(x, "random.effect"), logical(1))
  labs <- vapply(sm[is_re], function(x) x$label, character(1))
  cols <- unlist(lapply(sm[is_re], function(x) seq(x$first.para, x$last.para)))
  list(labels = labs, cols = as.integer(cols))
}

# Prediction grid for population curves: ages x group levels, other variables at reference values (numeric at the
# mean over individuals, factors at the most common level; excluded random effects at their first level).
gamm_newdata <- function(r, ages, level = NULL) {
  dd <- r$data
  ind <- dd[!duplicated(dd$id), , drop = FALSE]
  nd <- data.frame(age = ages)
  for (v in setdiff(names(dd), c("age", "trait", ".trials"))) {
    x <- dd[[v]]
    nd[[v]] <- if (is.factor(x)) factor(names(sort(table(x), decreasing = TRUE))[[1]], levels = levels(x))
               else if (v %in% c("ALR", "LS", "AFR")) mean(ind[[v]], na.rm = TRUE) else mean(x, na.rm = TRUE)
  }
  grp <- r$settings$group %||% ""
  if (nzchar(grp) && grp %in% names(dd) && !is.null(level)) nd[[grp]] <- factor(level, levels = levels(dd[[grp]]))
  if (".grp_o" %in% names(dd) && !is.null(level)) nd$.grp_o <- factor(level, levels = levels(dd$.grp_o), ordered = TRUE)
  nd
}

gamm_mean_cols <- function(X) {
  lpi <- attr(X, "lpi")
  if (is.null(lpi)) seq_len(ncol(X)) else lpi[[1]]
}

# Population curve(s) with 95% pointwise intervals, on the response scale.
gamm_curve <- function(r, n = 150) {
  dd <- r$data
  ages <- seq(min(dd$age), max(dd$age), length.out = n)
  grp <- r$settings$group %||% ""
  levs <- if (nzchar(grp) && !identical(r$structure, "G") && grp %in% names(dd)) levels(dd[[grp]]) else list(NULL)
  rs <- gamm_random_smooths(r$fit)
  linkinv <- r$fit$family$linfo[[1]]$linkinv %||% r$fit$family$linkinv
  out <- lapply(levs, function(lv) {
    nd <- gamm_newdata(r, ages, lv)
    X <- stats::predict(r$fit, newdata = nd, type = "lpmatrix")
    cols <- gamm_mean_cols(X)
    X[, rs$cols] <- 0
    X <- X[, cols, drop = FALSE]
    b <- stats::coef(r$fit)[cols]
    V <- r$fit$Vp[cols, cols, drop = FALSE]
    eta <- as.numeric(X %*% b)
    se <- sqrt(pmax(rowSums((X %*% V) * X), 0))
    data.frame(age = ages, level = if (is.null(lv)) "All" else lv, fit = linkinv(eta),
               lower = linkinv(eta - 1.96 * se), upper = linkinv(eta + 1.96 * se), stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# First derivative of the population curve on the link scale, with pointwise and simultaneous 95% intervals
# (simultaneous from 2,000 draws of the coefficients). Ageing is significant where the interval excludes zero.
gamm_derivative <- function(r, n = 150, n_sim = 2000, seed = 1L) {
  dd <- r$data
  ages <- seq(min(dd$age), max(dd$age), length.out = n)
  eps <- 1e-4 * diff(range(dd$age))
  grp <- r$settings$group %||% ""
  levs <- if (nzchar(grp) && !identical(r$structure, "G") && grp %in% names(dd)) levels(dd[[grp]]) else list(NULL)
  rs <- gamm_random_smooths(r$fit)
  set.seed(seed)
  out <- lapply(levs, function(lv) {
    X0 <- stats::predict(r$fit, newdata = gamm_newdata(r, ages - eps, lv), type = "lpmatrix")
    X1 <- stats::predict(r$fit, newdata = gamm_newdata(r, ages + eps, lv), type = "lpmatrix")
    cols <- gamm_mean_cols(X0)
    D <- (X1 - X0) / (2 * eps)
    D[, rs$cols] <- 0
    D <- D[, cols, drop = FALSE]
    b <- stats::coef(r$fit)[cols]
    V <- r$fit$Vp[cols, cols, drop = FALSE]
    d <- as.numeric(D %*% b)
    se <- sqrt(pmax(rowSums((D %*% V) * D), 0))
    B <- mgcv::rmvn(n_sim, rep(0, length(b)), V)
    dev <- abs(D %*% t(B)) / pmax(se, 1e-12)
    crit <- stats::quantile(apply(dev, 2, max), 0.95, names = FALSE)
    data.frame(age = ages, level = if (is.null(lv)) "All" else lv, derivative = d, lower = d - 1.96 * se, upper = d + 1.96 * se,
               lower_sim = d - crit * se, upper_sim = d + crit * se, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# Periods of significant change from the derivative: runs of ages where the interval lies wholly below (decline)
# or above (increase) zero. Onset of decline: the start of the last run of significant decline.
gamm_periods <- function(dv, simultaneous = FALSE) {
  lo <- if (simultaneous) dv$lower_sim else dv$lower
  hi <- if (simultaneous) dv$upper_sim else dv$upper
  rows <- list()
  for (lv in unique(dv$level)) {
    k <- dv$level == lv
    a <- dv$age[k]; state <- ifelse(hi[k] < 0, "decline", ifelse(lo[k] > 0, "increase", "no clear change"))
    rl <- rle(state); ends <- cumsum(rl$lengths); starts <- c(1, head(ends, -1) + 1)
    for (j in seq_along(rl$values)) if (!identical(rl$values[[j]], "no clear change"))
      rows[[length(rows) + 1]] <- data.frame(Group = lv, Change = rl$values[[j]], From = a[starts[[j]]], To = a[ends[[j]]], stringsAsFactors = FALSE)
  }
  if (length(rows)) do.call(rbind, rows) else data.frame(Group = character(0), Change = character(0), From = numeric(0), To = numeric(0))
}

# Peak and onset per group from the curve, with 95% limits from 1,000 draws of the coefficients (the turning-point
# rules of the parametric layer: curve_turning_points()).
gamm_peak_onset <- function(r, n_draws = 1000, n = 150, seed = 1L) {
  dd <- r$data
  ages <- seq(min(dd$age), max(dd$age), length.out = n)
  grp <- r$settings$group %||% ""
  levs <- if (nzchar(grp) && !identical(r$structure, "G") && grp %in% names(dd)) levels(dd[[grp]]) else list(NULL)
  rs <- gamm_random_smooths(r$fit)
  set.seed(seed)
  rows <- lapply(levs, function(lv) {
    X <- stats::predict(r$fit, newdata = gamm_newdata(r, ages, lv), type = "lpmatrix")
    cols <- gamm_mean_cols(X)
    X[, rs$cols] <- 0
    X <- X[, cols, drop = FALSE]
    b <- stats::coef(r$fit)[cols]
    V <- r$fit$Vp[cols, cols, drop = FALSE]
    tp <- curve_turning_points(ages, as.numeric(X %*% b))
    B <- mgcv::rmvn(n_draws, b, V)
    sims <- X %*% t(B)
    tps <- vapply(seq_len(ncol(sims)), function(j) {
      t <- curve_turning_points(ages, sims[, j])
      c(t$peak, as.numeric(t$peak_interior), t$onset, as.numeric(identical(t$onset_type, "interior")))
    }, numeric(4))
    q <- function(v) if (length(v) >= 20) paste0(format_num(stats::quantile(v, 0.025)), "\u2013", format_num(stats::quantile(v, 0.975))) else ""
    data.frame(Group = if (is.null(lv)) "All" else lv,
               `Peak age` = if (isTRUE(tp$peak_interior)) format_num(tp$peak) else "none within the sampled ages",
               `Peak 95% CI` = if (isTRUE(tp$peak_interior)) q(tps[1, tps[2, ] == 1]) else "",
               `Onset of final decline` = switch(tp$onset_type, interior = format_num(tp$onset), from_start = paste("at or before", format_num(min(ages))),
                                                 no_final_decline = "no final decline", ""),
               `Onset 95% CI` = if (identical(tp$onset_type, "interior")) q(tps[3, tps[4, ] == 1]) else "",
               check.names = FALSE, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# Difference between two groups' curves (link scale) with pointwise and simultaneous 95% intervals.
gamm_difference <- function(r, n = 150, n_sim = 2000, seed = 1L) {
  dd <- r$data
  grp <- r$settings$group %||% ""
  if (!nzchar(grp) || identical(r$structure, "G") || !grp %in% names(dd)) return(data.frame())
  levs <- levels(dd[[grp]])
  if (length(levs) < 2) return(data.frame())
  pairs <- utils::combn(levs, 2, simplify = FALSE)
  if (length(pairs) > 6) pairs <- pairs[1:6]
  ages <- seq(min(dd$age), max(dd$age), length.out = n)
  rs <- gamm_random_smooths(r$fit)
  set.seed(seed)
  do.call(rbind, lapply(pairs, function(pr) {
    Xa <- stats::predict(r$fit, newdata = gamm_newdata(r, ages, pr[[1]]), type = "lpmatrix")
    Xb <- stats::predict(r$fit, newdata = gamm_newdata(r, ages, pr[[2]]), type = "lpmatrix")
    cols <- gamm_mean_cols(Xa)
    D <- Xa - Xb
    D[, rs$cols] <- 0
    D <- D[, cols, drop = FALSE]
    b <- stats::coef(r$fit)[cols]
    V <- r$fit$Vp[cols, cols, drop = FALSE]
    d <- as.numeric(D %*% b)
    se <- sqrt(pmax(rowSums((D %*% V) * D), 0))
    B <- mgcv::rmvn(n_sim, rep(0, length(b)), V)
    crit <- stats::quantile(apply(abs(D %*% t(B)) / pmax(se, 1e-12), 2, max), 0.95, names = FALSE)
    data.frame(age = ages, pair = paste(pr[[1]], "\u2212", pr[[2]]), diff = d, lower = d - 1.96 * se, upper = d + 1.96 * se,
               lower_sim = d - crit * se, upper_sim = d + crit * se, stringsAsFactors = FALSE)
  }))
}

# Do groups age differently? Fits models G, GS, GI, S and I by ML on the same records and compares AIC; tests the
# ordered-factor difference smooths (the p-value of each difference smooth tests whether that group's trajectory
# differs from the reference group's); and a likelihood-ratio test of G against I.
gamm_group_comparison <- function(gd, s, progress = NULL) {
  if (!nzchar(s$group %||% "")) return(list(ok = FALSE, message = "Choose a grouping variable first."))
  structs <- GAMM_STRUCTURES
  fits <- list()
  for (i in seq_along(structs)) {
    if (is.function(progress)) progress(i, length(structs) + 1, names(structs)[[i]])
    fits[[structs[[i]]]] <- gamm_fit(gd, s, method = "ML", structure = structs[[i]])
  }
  tab <- do.call(rbind, lapply(names(fits), function(nm) {
    r <- fits[[nm]]
    data.frame(Model = nm, Description = sub("^[A-Z]+: ", "", names(structs)[structs == nm]),
               AIC = if (isTRUE(r$ok)) gamm_aic(r) else NA_real_,
               edf = if (isTRUE(r$ok)) sum(r$fit$edf) else NA_real_,
               `Deviance explained (%)` = if (isTRUE(r$ok)) 100 * summary(r$fit)$dev.expl else NA_real_,
               Status = if (isTRUE(r$ok)) "fitted" else r$message, check.names = FALSE, stringsAsFactors = FALSE)
  }))
  tab$dAIC <- tab$AIC - min(tab$AIC, na.rm = TRUE)
  tab <- tab[order(tab$AIC), c("Model", "Description", "AIC", "dAIC", "edf", "Deviance explained (%)", "Status")]
  if (is.function(progress)) progress(length(structs) + 1, length(structs) + 1, "difference smooths")
  od <- gamm_fit(gd, s, method = s$method, structure = "I", ordered_by = TRUE)
  diff_tab <- if (isTRUE(od$ok)) {
    st <- gamm_tables(od)$smooth
    st[grepl(".grp_o", st$Smooth, fixed = TRUE), , drop = FALSE]
  } else data.frame()
  if (nrow(diff_tab)) diff_tab$Smooth <- sub(":.grp_o", " difference: ", diff_tab$Smooth, fixed = TRUE)
  lrt <- NULL
  if (isTRUE(fits$G$ok) && isTRUE(fits$I$ok) && is.null(fits$G$mer)) {
    lrt <- tryCatch({
      a <- stats::anova(fits$G$fit, fits$I$fit, test = "Chisq")
      data.frame(Comparison = "G (one smooth) vs I (a smooth per group)", `Deviance difference` = a$Deviance[[2]],
                 df = a$Df[[2]], P = a[[ncol(a)]][[2]], check.names = FALSE)
    }, error = function(e) NULL)
  }
  list(ok = TRUE, table = tab, differences = diff_tab, reference = if (isTRUE(od$ok)) levels(od$data$.grp_o)[[1]] else "", lrt = lrt)
}

# Selective disappearance and appearance: the proxy terms of the fitted model, and ML comparisons of no proxy,
# an additive proxy and an age-dependent proxy (ti).
gamm_proxy_comparison <- function(gd, s, progress = NULL) {
  forms <- c(none = "none", additive = "linear", `age-dependent` = "ti")
  fits <- list()
  for (i in seq_along(forms)) {
    if (is.function(progress)) progress(i, length(forms), names(forms)[[i]])
    fits[[names(forms)[[i]]]] <- gamm_fit(gd, s, method = "ML", proxy_form = forms[[i]])
  }
  tab <- data.frame(Model = c("No lifespan proxy", paste("Additive", s$proxy), paste("Age-dependent", s$proxy, "(ti)")),
                    AIC = vapply(fits, gamm_aic, numeric(1)), stringsAsFactors = FALSE)
  tab$dAIC <- tab$AIC - min(tab$AIC, na.rm = TRUE)
  lrt <- tryCatch({
    if (!is.null(fits$additive$mer) || !isTRUE(fits$additive$ok) || !isTRUE(fits[["age-dependent"]]$ok)) NULL else {
      a <- stats::anova(fits$none$fit, fits$additive$fit, fits[["age-dependent"]]$fit, test = "Chisq")
      data.frame(Comparison = c("none vs additive", "additive vs age-dependent"), `Deviance difference` = a$Deviance[2:3],
                 df = a$Df[2:3], P = a[[ncol(a)]][2:3], check.names = FALSE)
    }
  }, error = function(e) NULL)
  list(ok = TRUE, table = tab, lrt = lrt)
}

# The proxy terms of one fitted model (parametric rows or smooth rows naming ALR, LS or AFR).
gamm_proxy_terms <- function(r) {
  tb <- gamm_tables(r)
  p <- tb$parametric[tb$parametric$Term %in% c("ALR", "LS", "AFR"), , drop = FALSE]
  sm <- tb$smooth[grepl("ALR|LS|AFR", tb$smooth$Smooth), , drop = FALSE]
  list(parametric = p, smooth = sm)
}

# Is a smooth needed? The same model with linear, quadratic and cubic age (fitted by ML with the same engine)
# against the smooth. AIC of the smooth uses its effective degrees of freedom, so the comparison is approximate.
gamm_parametric_reference <- function(gd, s) {
  rows <- lapply(list(`Linear age` = 1L, `Quadratic age` = 2L, `Cubic age` = 3L, `Smooth of age` = 0L), function(p) {
    r <- gamm_fit(gd, s, method = "ML", age_poly = p)
    c(AIC = gamm_aic(r), edf = if (isTRUE(r$ok)) sum(r$fit$edf) else NA_real_)
  })
  tab <- data.frame(Model = names(rows), AIC = vapply(rows, `[[`, numeric(1), "AIC"), edf = vapply(rows, `[[`, numeric(1), "edf"),
                    stringsAsFactors = FALSE, row.names = NULL)
  tab$dAIC <- tab$AIC - min(tab$AIC, na.rm = TRUE)
  tab[order(tab$AIC), c("Model", "AIC", "dAIC", "edf")]
}

# Random-effect standard deviations (mgcv::gam.vcomp).
gamm_vcomp <- function(r) {
  if (!is.null(r$mer)) {
    vc <- as.data.frame(lme4::VarCorr(r$mer))
    return(data.frame(Term = paste(vc$grp, vc$var1), SD = vc$sdcor, Lower = NA_real_, Upper = NA_real_, stringsAsFactors = FALSE))
  }
  vc <- tryCatch(mgcv::gam.vcomp(r$fit, rescale = FALSE), error = function(e) NULL)
  if (is.null(vc)) return(data.frame())
  m <- if (is.list(vc)) vc$all else vc
  if (is.null(dim(m))) return(data.frame(Term = names(m), SD = as.numeric(m), Lower = NA_real_, Upper = NA_real_))
  data.frame(Term = rownames(m), SD = m[, 1], Lower = if (ncol(m) >= 3) m[, 2] else NA_real_, Upper = if (ncol(m) >= 3) m[, 3] else NA_real_,
             stringsAsFactors = FALSE, row.names = NULL)
}

# Individual trajectories predicted with every term (random effects included) for a sample of individuals.
gamm_individual_curves <- function(r, n_ind = 12, seed = 1L) {
  dd <- r$data
  set.seed(seed)
  ids <- levels(dd$id)
  ids <- ids[vapply(ids, function(i) sum(dd$id == i) >= 3, logical(1))]
  if (!length(ids)) return(data.frame())
  pick <- sample(ids, min(n_ind, length(ids)))
  do.call(rbind, lapply(pick, function(i) {
    di <- dd[dd$id == i, , drop = FALSE]
    ages <- seq(min(di$age), max(di$age), length.out = 30)
    nd <- di[rep(1, length(ages)), , drop = FALSE]
    nd$age <- ages
    p <- tryCatch(stats::predict(r$fit, newdata = nd, type = "response"), error = function(e) rep(NA_real_, length(ages)))
    if (is.matrix(p)) p <- p[, 1]
    data.frame(id = i, age = ages, fit = as.numeric(p), stringsAsFactors = FALSE)
  }))
}

# Residuals for the diagnostic figures (deviance residuals against fitted values, and their quantiles).
gamm_residuals <- function(r) {
  f <- stats::fitted(r$fit)
  if (is.matrix(f)) f <- f[, 1]
  data.frame(fitted = as.numeric(f), residual = as.numeric(stats::residuals(r$fit, type = "deviance")))
}

# Automatic cautions for the fitted model (short, one line each).
gamm_cautions <- function(r, kc = gamm_kcheck(r), cc = gamm_concurvity(r)) {
  out <- character(0)
  if (nrow(kc) && any(kc$Flag != "")) out <- c(out, sprintf("Basis too small for %s: raise k and refit.", paste(kc$Smooth[kc$Flag != ""], collapse = ", ")))
  if (nrow(cc) && any(cc$Flag != "")) out <- c(out, sprintf("High concurvity (> 0.8) for %s: these terms can mimic each other, so their separate effects are poorly identified.", paste(cc$Term[cc$Flag != ""], collapse = ", ")))
  dd <- r$data
  old <- stats::quantile(dd$age, 0.9)
  n_old <- length(unique(dd$id[dd$age >= old]))
  if (n_old < 20) out <- c(out, sprintf("Only %d individuals above age %s: the smooth is weakly constrained at old ages.", n_old, format_num(old)))
  if (identical(r$settings$individual, "none")) out <- c(out, "No individual random effect: repeated records are treated as independent, so intervals are too narrow.")
  if (identical(r$settings$proxy_form %||% "none", "none") && identical(r$settings$afr_form %||% "none", "none"))
    out <- c(out, "No lifespan or AFR term: the smooth is the population trajectory, which selective disappearance can distort.")
  if (length(r$notes)) out <- c(out, paste("Fitting notes:", paste(utils::head(r$notes, 3), collapse = "; ")))
  out
}

# R code that refits the model with mgcv (or gamm4).
gamm_code <- function(r) {
  s <- r$settings
  fam <- switch(s$family, gaussian = "gaussian()", gaulss = "gaulss()", Gamma = "Gamma(link = \"log\")", betar = "betar(link = \"logit\")",
                poisson = "poisson()", nb = "nb()", ziP = "ziP()", tw = "tw()", binomial = "binomial()", "gaussian()")
  f <- if (identical(s$family, "gaulss")) sprintf("list(%s, ~ %s)", r$formula, gamm_age_smooth(s)) else r$formula
  wt <- if (identical(s$family, "binomial") && ".trials" %in% names(r$data)) ", weights = .trials" else ""
  kn <- gamm_knots(s)
  knt <- if (!is.null(kn)) sprintf(", knots = list(age = c(%s))", paste(kn$age, collapse = ", ")) else ""
  fitline <- switch(s$engine,
    bam = sprintf("fit <- bam(%s, family = %s, data = gd, method = \"%s\", discrete = %s%s%s)", f, fam, if (identical(r$method, "ML")) "ML" else "fREML",
                  if (identical(r$method, "ML")) "FALSE" else "TRUE", wt, knt),
    gamm4 = sprintf("g4 <- gamm4::gamm4(%s, random = %s, family = %s, data = gd%s%s); fit <- g4$gam", f,
                    paste(deparse(gamm_random_formula(s, r$data)), collapse = ""), fam, wt, knt),
    sprintf("fit <- gam(%s, family = %s, data = gd, method = \"%s\", select = %s%s%s)", f, fam, r$method, if (isTRUE(s$select)) "TRUE" else "FALSE", wt, knt))
  c("library(mgcv)",
    "# gd: one row per record with columns id (factor), age, trait, ALR, LS and AFR (one value per individual),",
    "# covariates (cv_*) and random-effect columns (re_*), as built by disappR's gamm_data(dat, meta).",
    fitline, "summary(fit)", "k.check(fit)                     # basis dimension check", "concurvity(fit, full = TRUE)",
    "plot(fit, pages = 1, shade = TRUE, seWithMean = TRUE)")
}
