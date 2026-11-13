# ---------------------------------------------------------------------------
# Evidence summary: from saved results to a graded finding.
#
# Each saved result that bears on selective disappearance carries a small structured `evidence` record,
# written when it is saved. grade_evidence() reads those records - never the display text - and returns:
#   * a finding, worded neutrally ("higher body mass", never "outperform") and cautiously ("suggests");
#   * one line per kind of evidence, marked as supporting, a caveat, contradicting, or not yet saved;
#   * an overall level: strong, moderate, weak, mixed, none, or insufficient;
#   * the checks still to run, in the order that matters most for the finding.
#
# The level is deliberately conservative. It cannot be "strong" unless every relevant check has been run and
# passed, and a failed check does not merely lower it: it changes the explanation offered, because the checks
# that fail (random slopes, the null-model bootstrap, the ageing function) each point to a specific alternative.
# ---------------------------------------------------------------------------

EVIDENCE_ALPHA <- 0.05

# Coefficient of the trait on the lifespan proxy at each age, among the individuals present at that age.
# Exact ages avoid the two artefacts found in calibration: lifespan bins converge under purely age-independent
# selection (each bin loses its own shortest-lived members), and wide age bins confound age with lifespan
# (longer-lived individuals contribute later records). Selection acts on lifespan, the predictor, which leaves a
# regression slope unbiased. Frequency weights let the bootstrap resample individuals without copying data.
age_coefficients <- function(by_age, x, y, id, w, min_ind = 8) {
  out <- NULL
  for (a in names(by_age)) {
    k <- by_age[[a]]
    k <- k[w[id[k]] > 0]
    if (length(k) < 3) next
    wk <- w[id[k]]
    if (sum(wk) < min_ind || length(unique(x[k])) < 3) next
    xm <- sum(wk * x[k]) / sum(wk); ym <- sum(wk * y[k]) / sum(wk)
    sxx <- sum(wk * (x[k] - xm)^2)
    if (!is.finite(sxx) || sxx <= 0) next
    b <- sum(wk * (x[k] - xm) * (y[k] - ym)) / sxx
    r <- y[k] - ym - b * (x[k] - xm)
    s2 <- sum(wk * r^2) / max(sum(wk) - 2, 1)
    se <- sqrt(s2 / sxx)
    if (is.finite(se) && se > 0) out <- rbind(out, c(age = as.numeric(a), b = b, se = se))
  }
  out
}
summarise_coefficients <- function(co) {
  w <- 1 / co[, "se"]^2
  X <- cbind(1, co[, "age"])
  beta <- solve(crossprod(X, w * X), crossprod(X, w * co[, "b"]))
  c(trend = beta[[2]], level = sum(w * co[, "b"]) / sum(w))
}

#' Classify the visual pattern from the trait-lifespan coefficient across age
#'
#' @param dat Standardised data with id, age, trait and alr.
#' @param B Number of bootstrap draws.
#' @param alpha Significance threshold for the bootstrap p-values.
#' @param min_ind Minimum individuals sharing an age for that age to contribute a coefficient.
#' @param seed Seed for the bootstrap; the session's random-number stream is restored afterwards.
#' @param proxy The column the figure groups by: "alr", "life" (known lifespan) or "entry" (AFR, for selective appearance).
#' @param process Which process this classifies ("disappearance" or "appearance"), carried through to the result.
#' @return A visual evidence record; kind "unavailable" when too few individuals share ages.
classify_visual_coef <- function(dat, B = 200L, alpha = EVIDENCE_ALPHA, min_ind = 8L, seed = 11L, proxy = "alr",
                                 process = "disappearance") {
  if (is.null(dat) || !nrow(dat) || !all(c("id", "age", "trait", proxy) %in% names(dat))) return(NULL)
  d <- dat[is.finite(dat$age) & is.finite(dat$trait) & is.finite(dat[[proxy]]), c("id", "age", "trait", proxy)]
  names(d)[4] <- "alr"   # the proxy, whichever it is
  if (!nrow(d)) return(NULL)
  if (length(unique(d$age)) > 60) {           # continuous ages: place them on a grid of 30 points
    grid <- seq(min(d$age), max(d$age), length.out = 30)
    d$age <- grid[pmax(1L, findInterval(d$age, grid))]
  }
  d <- stats::aggregate(cbind(trait, alr) ~ id + age, data = d, FUN = mean)
  ids <- unique(d$id); idx <- match(d$id, ids)
  by_age <- split(seq_len(nrow(d)), d$age)
  co <- age_coefficients(by_age, d$alr, d$trait, idx, rep(1, length(ids)), min_ind)
  if (is.null(co) || nrow(co) < 3) return(list(type = "visual", process = process, proxy = proxy, kind = "unavailable"))
  est <- summarise_coefficients(co)
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL
  on.exit({
    if (had) assign(".Random.seed", old, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  }, add = TRUE)
  set.seed(seed)
  boot <- NULL
  for (b in seq_len(B)) {
    w <- tabulate(sample.int(length(ids), length(ids), replace = TRUE), nbins = length(ids))
    cb <- age_coefficients(by_age, d$alr, d$trait, idx, w, min_ind)
    if (!is.null(cb) && nrow(cb) >= 3) boot <- rbind(boot, summarise_coefficients(cb))
  }
  if (is.null(boot) || nrow(boot) < 20) return(list(type = "visual", process = process, proxy = proxy, kind = "unavailable"))
  p_trend <- 2 * stats::pnorm(-abs(est[["trend"]]) / stats::sd(boot[, "trend"]))
  p_level <- 2 * stats::pnorm(-abs(est[["level"]]) / stats::sd(boot[, "level"]))
  kind <- if (is.finite(p_trend) && p_trend < alpha) "age_dependent"
          else if (is.finite(p_level) && p_level < alpha) "age_independent" else "none"
  list(type = "visual", process = process, proxy = proxy, kind = kind, slope = est[["trend"]], mean_gap = est[["level"]],
       p_trend = p_trend, p_level = p_level)
}

# What a model says about each process, read from its own structure (MODEL_MEANING): "none" when the process has no
# term, "age_independent" for a main effect, "age_dependent" for a term interacting with age. field is "dis" or "app".
model_process_kind <- function(label, field = "dis") {
  id <- names(MODEL_MEANING)[vapply(names(MODEL_MEANING), function(k) identical(model_label(k), label), logical(1))]
  if (!length(id)) return(NA_character_)
  v <- as.character(MODEL_MEANING[[id[[1]]]][[field]] %||% "")
  if (grepl("^age-dependent", v)) "age_dependent" else if (grepl("^age-independent", v)) "age_independent" else "none"
}

#' Evidence from a fitted model comparison
#'
#' @param r A fitted suite from fit_model_suite().
#' @param verdict The kind of process the supported set points to ("age_dependent", "age_independent", "none").
#' @param shape_ok Whether the ageing function in use is within 2 AIC of the best at the model level.
#' @param shape_supported Ageing functions within 2 AIC of the best at the model level.
#' @return A list summarising the evidence for selective disappearance and appearance.
models_evidence <- function(r, verdict, shape_ok = NA, shape_supported = character(0)) {
  if (is.null(r) || !isTRUE(r$ok) || is.null(r$aic) || !nrow(r$aic)) return(NULL)
  a <- r$aic[is.finite(r$aic$Delta_AIC), , drop = FALSE]
  if (!nrow(a)) return(NULL)
  sup <- a$Model[a$Delta_AIC < 2]
  lrt_p <- function(cmp) {
    l <- r$lrt
    if (is.null(l) || !nrow(l) || !cmp %in% l$Comparison) return(NA_real_)
    as.numeric(l$P_value[l$Comparison == cmp][[1]])
  }
  est <- function(m, term) {
    b <- tryCatch(raw_coef(r, m, term), error = function(e) NULL)
    if (is.null(b) || !is.finite(b$est)) NA_real_ else b$est
  }
  inter <- est("M4", "f1:ALR")
  if (!is.finite(inter)) inter <- est("M4", "ALR:f1")
  p24 <- lrt_p("Model 2 vs Model 4")
  # The kind is the winning model's own structure (any of Models 1-10). The one exception is the naive model being
  # within 2 AIC of it: the data then support no proxy term at all, which is the more cautious reading.
  win_kind <- model_process_kind(a$Model[[1]], "dis")
  kind_from_winner <- if (model_label("M1") %in% sup) "none" else win_kind
  kind <- if (!is.na(kind_from_winner)) kind_from_winner else if ("Model 1" %in% sup) "none"
          else if ("Model 4" %in% sup && (!"Model 2" %in% sup || (is.finite(p24) && p24 < EVIDENCE_ALPHA))) "age_dependent"
          else if ("Model 2" %in% sup) "age_independent"
          else if (verdict %in% c("age_dependent", "age_independent", "none")) verdict else "none"
  # The direction comes from the finding's own model, read from its predictions: the slopes of a long- and a short-
  # lived individual (90th and 10th percentiles of the proxy) between the 25th and 75th age percentiles, on the link
  # scale. Not from Model 1, which selective disappearance biases (possibly to the opposite sign), and not from a
  # coefficient's sign: the asymptotic-exponential basis exp(-z_age) falls with age, so its sign is reversed.
  sm <- evidence_model_for(kind, a, r)
  dir <- evidence_direction(r, sm)
  basis_sign <- if (identical(r$age_function, "Asymptotic exponential")) -1 else 1
  age_slope <- dir$slope_mid
  if (!is.finite(age_slope)) {
    age_slope <- basis_sign * switch(kind, age_dependent = est("M4", "f1"), age_independent = est("M2", "f1"), est("M1", "f1"))
    for (alt in c("M4", "M2")) if (!is.finite(age_slope)) age_slope <- basis_sign * est(alt, "f1")
  }
  pred_inter <- dir$slope_long - dir$slope_short
  if (is.finite(pred_inter)) inter <- pred_inter else inter <- basis_sign * inter
  level <- if (is.finite(dir$level_diff)) dir$level_diff else est("M2", "ALR")
  # selective appearance, from Models 7-10 (they need individual AFR): its kind is that of the best model overall
  # when that is one of them; otherwise the data give no support for it
  app_fitted <- a$Model[a$Model %in% model_label(c("M7", "M8", "M9", "M10"))]
  app_kind <- if (!length(app_fitted)) NA_character_ else {
    k <- model_process_kind(a$Model[[1]], "app")
    if (is.na(k)) "none" else k
  }
  list(type = "models", kind = kind, supported = sup, best = a$Model[[1]],
       appearance_kind = app_kind, appearance_model = if (length(app_fitted)) app_fitted[[1]] else NA_character_,
       appearance_gap = if (length(app_fitted)) a$Delta_AIC[a$Model == app_fitted[[1]]][[1]] else NA_real_,
       gap_m1 = if ("Model 1" %in% a$Model) a$Delta_AIC[a$Model == "Model 1"][[1]] else NA_real_,
       next_model = if (nrow(a) > 1) a$Model[[2]] else NA_character_, gap_next = if (nrow(a) > 1) a$Delta_AIC[[2]] else NA_real_,
       supported_df = if ("df" %in% names(a)) stats::setNames(as.numeric(a$df[a$Delta_AIC < 2]), sup) else NULL,
       p_level = lrt_p("Model 1 vs Model 2"), p_interaction = lrt_p("Model 2 vs Model 4"),
       age_slope = age_slope, level_effect = level, interaction = inter,
       slope_model = sm %||% "", slope_long = dir$slope_long, slope_short = dir$slope_short,
       level_difference = dir$level_diff, direction_ages = dir$ages,
       random_slope = if (is.null(r$random_slope)) "none" else as.character(r$random_slope)[[1]],
       age_function = if (is.null(r$age_function)) "" else r$age_function,
       settings_key = evidence_settings_key(r),
       shape_ok = shape_ok, shape_supported = shape_supported,
       appearance = any(sup %in% model_label(c("M7", "M8", "M9", "M10"))))
}

# The model whose predictions describe the finding: Model 4 (or another lifespan x age model) for an age-dependent
# finding, Model 2 (or another additive lifespan model) for an age-independent one. NULL when none was fitted.
evidence_model_for <- function(kind, a, r) {
  ranked <- intersect(sub("^Model ", "M", a$Model), names(r$fits))
  pref <- switch(kind,
    age_dependent = c("M4", intersect(ranked, c("M6", "M8", "M9", "M5", "M10"))),
    age_independent = c("M2", intersect(ranked, c("M7", "M3"))),
    character(0))
  hit <- intersect(pref, ranked)
  if (length(hit)) hit[[1]] else NULL
}

# Proxy values that stand for a short- and a long-lived individual (10th and 90th percentiles), on the model's own
# scale, for predict_population_curve(hold = ...). ALR, known lifespan, or the individual mean age (Models 3 and 5).
evidence_proxy_holds <- function(r, m) {
  d <- r$data
  ind <- d[!duplicated(d$id), , drop = FALSE]
  v <- if (m %in% c("M3", "M5")) "mean" else if (identical(m, "M6")) "LS" else "ALR"
  if (identical(v, "mean")) {
    mf <- ind[["mean_f1"]]
    if (is.null(mf) || sum(is.finite(mf)) < 3) return(NULL)
    q <- stats::quantile(mf[is.finite(mf)], c(0.1, 0.9), names = FALSE, type = 7)
    pick <- function(target) {
      j <- which.min(abs(mf - target))
      stats::setNames(lapply(r$basis, function(nm) ind[[paste0("mean_", nm)]][[j]]), paste0("mean_", r$basis))
    }
    return(list(lo = pick(q[[1]]), hi = pick(q[[2]])))
  }
  raw <- ind[[paste0(v, "_raw")]]
  cs <- r$proxy_params[[v]]
  if (is.null(raw) || is.null(cs) || sum(is.finite(raw)) < 3) return(NULL)
  q <- stats::quantile(raw[is.finite(raw)], c(0.1, 0.9), names = FALSE, type = 7)
  z <- (q - cs[["centre"]]) / cs[["scale"]]
  list(lo = stats::setNames(list(z[[1]]), v), hi = stats::setNames(list(z[[2]]), v))
}

# Slopes and levels read from a model's predictions between the 25th and 75th percentiles of the analysed ages, on the
# link scale (identity, log or logit). Reading predictions rather than a coefficient is correct for every ageing
# function: the asymptotic-exponential basis decreases with age, so the sign of its coefficient is not the direction.
evidence_direction <- function(r, m) {
  out <- list(model = m %||% "", slope_mid = NA_real_, slope_long = NA_real_, slope_short = NA_real_,
              level_diff = NA_real_, ages = c(NA_real_, NA_real_))
  if (is.null(m) || is.null(r$fits[[m]]) || is.null(r$data) || !nrow(r$data)) return(out)
  fit <- r$fits[[m]]
  a <- r$data$age[is.finite(r$data$age)]
  ages <- unique(as.numeric(stats::quantile(a, c(0.25, 0.75), names = FALSE, type = 7)))
  if (length(ages) < 2) ages <- unique(range(a))
  if (length(ages) < 2) return(out)
  fam <- r$family %||% "gaussian"
  to_link <- function(x) {
    if (identical(fam, "gaussian")) x
    else if (fam %in% c(BINOMIAL_FAMILIES, "beta")) stats::qlogis(pmin(pmax(x, 1e-9), 1 - 1e-9))
    else log(pmax(x, 1e-300))
  }
  summ <- function(hold) {
    pc <- tryCatch(predict_population_curve(fit, r, ages, hold = hold), error = function(e) data.frame())
    if (!is.data.frame(pc) || nrow(pc) < 2 || !all(is.finite(pc$fitted))) return(c(slope = NA_real_, level = NA_real_))
    pc <- pc[order(pc$age), , drop = FALSE]
    y <- to_link(pc$fitted)
    n <- nrow(pc)
    c(slope = (y[[n]] - y[[1]]) / (pc$age[[n]] - pc$age[[1]]), level = mean(y))
  }
  out$ages <- ages
  out$slope_mid <- summ(NULL)[["slope"]]
  hl <- tryCatch(evidence_proxy_holds(r, m), error = function(e) NULL)
  if (!is.null(hl)) {
    lo <- summ(hl$lo)
    hi <- summ(hl$hi)
    out$slope_short <- lo[["slope"]]
    out$slope_long <- hi[["slope"]]
    out$level_diff <- hi[["level"]] - lo[["level"]]
  }
  out
}

# The settings that define an analysis apart from its random structure, read from a fitted suite. The null-model
# bootstrap records the same key from its own observed fit, so the two always agree when they describe one analysis.
# The model set is part of the key (0.20.18): saving Models 1-4 and later Models 1-10 on the same data compares
# different sets, which is not a disagreement between findings.
evidence_settings_key <- function(r) paste(r$age_function %||% "", r$family %||% "", r$among %||% "linear",
                                           isTRUE(r$standardise), paste(sort(as.character(r$aic$Model %||% character(0))), collapse = ","), sep = "|")

# ---- the grader --------------------------------------------------------------------------------------------

latest_evidence <- function(entries, type) {
  hits <- Filter(function(e) is.list(e$evidence) && identical(e$evidence$type, type), entries)
  if (!length(hits)) return(NULL)
  hits[[length(hits)]]$evidence
}
all_evidence <- function(entries, type) {
  lapply(Filter(function(e) is.list(e$evidence) && identical(e$evidence$type, type), entries), function(e) e$evidence)
}

KIND_TEXT <- c(age_dependent = "age-dependent selective disappearance",
               age_independent = "age-independent selective disappearance",
               none = "no selective disappearance")

# The finding, worded neutrally (never "better" or "worse") from the corrected model's own predictions: the ageing
# slopes of a long- and a short-lived individual (age-dependent) or their difference in level (age-independent).
finding_sentence <- function(kind, mv, vis, trait) {
  tr <- if (nzchar(trait %||% "")) trait else "the trait"
  span <- ""
  ag <- if (is.null(mv)) NULL else mv$direction_ages
  if (length(ag) == 2 && all(is.finite(ag)) && !identical(mv$age_function %||% "", "Linear"))
    span <- sprintf(" between ages %s and %s (the middle half of the records)", format_num(ag[[1]]), format_num(ag[[2]]))
  if (identical(kind, "age_dependent")) {
    sl <- if (is.null(mv)) NA_real_ else mv$slope_long %||% NA_real_
    ss <- if (is.null(mv)) NA_real_ else mv$slope_short %||% NA_real_
    if (length(sl) == 1L && length(ss) == 1L && isTRUE(is.finite(sl) && is.finite(ss)) && sl != ss) {
      word <- function(x) if (x < 0) "decline" else if (x > 0) "increase" else "change little"
      txt <- if (sign(sl) == sign(ss) && sl != 0) {
        sprintf("longer-lived individuals %s %s in %s with age%s than shorter-lived individuals", word(sl),
                if (abs(sl) < abs(ss)) "more slowly" else "faster", tr, span)
      } else {
        sprintf("longer-lived individuals %s in %s with age%s, while shorter-lived individuals %s", word(sl), tr, span, word(ss))
      }
      return(paste0(txt, ", so the difference between them changes with age"))
    }
    # records without the corrected model's predictions: the average slope and the interaction, if present
    sa <- if (!is.null(mv) && length(mv$age_slope) == 1L) sign(mv$age_slope) else NA
    si <- if (!is.null(mv) && length(mv$interaction) == 1L) sign(mv$interaction) else NA
    if (isTRUE(is.finite(sa) && is.finite(si) && sa != 0 && si != 0)) {
      verb <- if (sa < 0) "decline" else "increase"
      pace <- if ((sa < 0 && si > 0) || (sa > 0 && si < 0)) "more slowly" else "faster"
      return(sprintf("longer-lived individuals %s %s in %s with age%s than shorter-lived individuals, so the difference between them changes with age", verb, pace, tr, span))
    }
    if (!is.null(vis) && isTRUE(is.finite(vis$slope)))
      return(sprintf("the difference in %s between longer- and shorter-lived individuals %s with age", tr,
                     if (vis$slope > 0) "grows in favour of longer-lived individuals" else "shrinks or reverses"))
    return(sprintf("the difference in %s between longer- and shorter-lived individuals changes with age", tr))
  }
  if (identical(kind, "age_independent")) {
    s <- if (!is.null(mv) && isTRUE(is.finite(mv$level_difference))) sign(mv$level_difference)
         else if (!is.null(mv) && isTRUE(is.finite(mv$level_effect))) sign(mv$level_effect)
         else if (!is.null(vis) && isTRUE(is.finite(vis$mean_gap))) sign(vis$mean_gap) else NA
    if (is.finite(s) && s != 0)
      return(sprintf("longer-lived individuals have consistently %s %s than shorter-lived individuals, by a similar amount at all ages",
                     if (s > 0) "higher" else "lower", tr))
    return(sprintf("%s differs between longer- and shorter-lived individuals by a similar amount at all ages", tr))
  }
  sprintf("there is no clear difference in %s between longer- and shorter-lived individuals", tr)
}

#' Grade the saved evidence for selective disappearance
#'
#' @param entries Saved results, in any order; each may carry an `evidence` record.
#' @param trait A label for the trait, used in the finding.
#' @return A list: level, headline, finding, lines (data frame), explanation, pending, cautions.
grade_evidence <- function(entries, trait = "the trait") {
  # figures saved with a lifespan proxy; figures grouped by AFR are evidence for selective appearance instead
  vis_d <- Filter(function(x) !identical(x$process %||% "disappearance", "appearance"), all_evidence(entries, "visual"))
  vis <- if (length(vis_d)) vis_d[[length(vis_d)]] else NULL
  all_mv <- all_evidence(entries, "models")
  # The finding comes from the latest comparison fitted with a random intercept only for the latest analysis (same
  # data and settings), when one was saved: comparisons with random slopes are the random-slope sensitivity check, so
  # one that disagrees weakens the finding instead of silently replacing it.
  latest <- latest_evidence(entries, "models")
  same_as_latest <- function(x) (is.null(latest$data_sig) || is.null(x$data_sig) || identical(x$data_sig, latest$data_sig)) &&
    (is.null(latest$settings_key) || is.null(x$settings_key) || identical(x$settings_key, latest$settings_key))
  ri_mv <- Filter(function(x) same_as_latest(x) && identical(x$random_slope %||% "none", "none"), all_mv)
  mv <- if (length(ri_mv)) ri_mv[[length(ri_mv)]] else latest
  # Records are combined only when they come from the same analysis as the latest model comparison: the same data
  # and the same settings apart from the random structure (varying that is the sensitivity check). An earlier
  # comparison with another ageing function, family or decomposition is superseded, not contradicted.
  same_data <- function(x) is.null(mv) || is.null(mv$data_sig) || is.null(x$data_sig) || identical(x$data_sig, mv$data_sig)
  same_settings <- function(x) is.null(mv) || is.null(mv$settings_key) || is.null(x$settings_key) ||
    identical(x$settings_key, mv$settings_key)
  n_superseded <- 0L
  if (length(all_mv)) {
    keep <- vapply(all_mv, function(x) same_data(x) && same_settings(x), logical(1))
    n_superseded <- sum(!keep)
    all_mv <- all_mv[keep]
  }
  perm <- latest_evidence(entries, "permutation")
  shp <- latest_evidence(entries, "shape")
  miss_all <- all_evidence(entries, "missingness")
  miss <- NULL
  if (length(miss_all)) {
    pick <- function(f) { v <- vapply(miss_all, function(x) as.numeric(x[[f]] %||% NA_real_)[1], numeric(1)); v <- v[is.finite(v)]
                          if (length(v)) v[[length(v)]] else NA_real_ }
    miss <- list(trait_p = pick("trait_p"), percent = pick("percent"))
  }
  line <- function(name, status, note) data.frame(Evidence = name, Status = status, Note = note, Footnote = "", stringsAsFactors = FALSE)
  pending <- character(0)
  todo <- function(key) pending <<- c(pending, key)

  if (is.null(mv) && is.null(vis)) {
    return(list(level = "insufficient", headline = "Not enough saved evidence to summarise yet.",
                finding = NULL, lines = data.frame(), explanation = NULL,
                pending = c("visual", "models", "missingness"), cautions = character(0)))
  }
  # which finding the evidence points to: models first, the figures if no model was saved
  kind <- if (!is.null(mv) && mv$kind %in% names(KIND_TEXT)) mv$kind
          else if (!is.null(vis) && vis$kind %in% names(KIND_TEXT)) vis$kind else "none"
  lines <- list()

  ## visual pattern
  if (!is.null(vis) && identical(vis$kind, "unavailable")) {
    lines[[length(lines) + 1]] <- line("Visual pattern", "caveat", "too few individuals share each age to read the pattern")
    vis <- NULL
  } else if (is.null(vis)) { lines[[length(lines) + 1]] <- line("Visual pattern", "not saved", "Save the trajectory figure from step 2."); todo("visual")
  } else {
    agree <- identical(vis$kind, kind)
    note <- switch(vis$kind,
      age_dependent = "the gap between lifespan groups changes with age",
      age_independent = "lifespan groups differ by a similar amount at all ages",
      "lifespan groups do not clearly differ")
    lines[[length(lines) + 1]] <- line("Visual pattern", if (agree) "supports" else "contradicts", paste0(note, visual_stats_note(vis)))
    gl <- gap_evidence_line(vis, kind, "lifespan groups")
    if (!is.null(gl)) lines[[length(lines) + 1]] <- gl
  }
  ## model comparison
  models_ambiguous <- FALSE
  if (is.null(mv)) { lines[[length(lines) + 1]] <- line("Model comparison", "not saved", "Fit and save the model comparison."); todo("models")
  } else {
    rd <- model_set_reading(mv)
    st <- if (!(mv$kind %in% c(kind))) "contradicts" else if (rd$clear) "supports" else "caveat"
    models_ambiguous <- identical(st, "caveat")
    lines[[length(lines) + 1]] <- line("Model comparison", st, rd$note)
  }
  ## (0.20.6: no separate line for the lifespan term: the model comparison, all models together, is the evidence)
  ## robustness: only the checks that bear on this kind of finding
  sel <- kind %in% c("age_dependent", "age_independent")
  fails <- character(0); caveats <- character(0)
  shp_use <- !is.null(shp) && same_data(shp) &&
    (is.null(mv) || is.null(shp$current) || identical(shp$current, mv$age_function)) &&
    !(sel && identical(shp$model %||% "", "M1"))
  shape_ok <- if (shp_use) shp$ok else if (!is.null(mv)) mv$shape_ok else NA
  if (sel) {
    if (is.na(shape_ok)) { lines[[length(lines) + 1]] <- line("Ageing shape", "not saved", "Run the Ageing-function check."); todo("shape")
    } else if (isTRUE(shape_ok)) { lines[[length(lines) + 1]] <- line("Ageing shape", "supports", "the ageing function in use is among the best supported")
    } else { lines[[length(lines) + 1]] <- line("Ageing shape", "caveat", "another ageing function fits better; a misspecified shape can produce this result"); caveats <- c(caveats, "shape") }
  }
  if (identical(kind, "age_dependent")) {
    slope_fits <- Filter(function(x) !identical(x$random_slope, "none"), all_mv)
    if (!length(slope_fits)) { lines[[length(lines) + 1]] <- line("Random-slope sensitivity", "not saved", "Refit with random slopes and save the model comparison."); todo("slopes")
    } else {
      s <- slope_fits[[length(slope_fits)]]
      holds <- identical(s$kind, "age_dependent") && (!is.finite(s$p_interaction) || s$p_interaction < EVIDENCE_ALPHA)
      lines[[length(lines) + 1]] <- line("Random-slope sensitivity", if (holds) "supports" else "contradicts",
        if (holds) "the age-dependent model is still preferred with random slopes, suggesting that the interaction term's influence is not via explaining heterogeneity in ageing, but via accounting for age-dependent selective disappearance" else "the age-dependent model is no longer preferred once random slopes are fitted")
      if (!holds) fails <- c(fails, "slopes")
    }
  }
  if (sel) {
    perm_fits <- !is.null(perm) && !is.null(perm$model) && same_data(perm) && same_settings(perm) &&
      (if (identical(kind, "age_dependent")) isTRUE(perm$model %in% INTERACTION_MODELS) else !isTRUE(perm$model %in% INTERACTION_MODELS))
    if (is.null(perm)) { lines[[length(lines) + 1]] <- line("Null-model bootstrap", "not saved", "Run the bootstrap test."); todo("permutation")
    } else if (!perm_fits) {
      lines[[length(lines) + 1]] <- line("Null-model bootstrap", "not saved",
        sprintf("the saved test was for %s%s; run it on the %s model", model_label(perm$model %||% "M1"),
                if (!same_data(perm) || !same_settings(perm)) " with other data or settings" else "",
                if (identical(kind, "age_dependent")) "age-dependent (for example Model 4)" else "age-independent (for example Model 2)"))
      todo("permutation")
    } else {
      st <- switch(perm$pattern, drops = "supports", partial = "caveat", no_drop = "contradicts", "caveat")
      note <- switch(perm$pattern, drops = "the advantage exceeds what data simulated without a lifespan effect give",
                     partial = "the advantage is above most datasets simulated without a lifespan effect, but not clearly",
                     no_drop = "data simulated without a lifespan effect give advantages as large",
                     too_few = "too few simulated datasets were refitted to reach p < 0.05; rerun with more",
                     "the model had no advantage to test")
      lines[[length(lines) + 1]] <- line("Null-model bootstrap", st, note)
      if (identical(perm$pattern, "no_drop")) fails <- c(fails, "permutation")
      if (isTRUE(perm$pattern %in% c("partial", "too_few"))) caveats <- c(caveats, "permutation")
    }
  }
  ## observation bias
  bias <- NA_character_
  if (is.null(miss)) { todo("missingness")
  } else {
    bias <- if (is.finite(miss$trait_p %||% NA) && miss$trait_p < EVIDENCE_ALPHA) "High"
            else if (is.finite(miss$percent %||% NA) && miss$percent >= 25) "Moderate" else "Low"
    lines[[length(lines) + 1]] <- line("Observation bias", switch(bias, High = "contradicts", Moderate = "caveat", "supports"),
      switch(bias, High = "missing records are associated with the trait itself, which can imitate selective disappearance",
             Moderate = sprintf("%.0f%% of expected occasions are missing", miss$percent), "no sign that missingness depends on the trait"))
  }

  ## the level
  statuses <- vapply(lines, function(x) x$Status, character(1))
  vis_contra <- !is.null(vis) && !identical(vis$kind, kind)
  # saved model comparisons disagree only if they differ with the SAME random structure: a difference between a
  # fit without random slopes and one with them is the random-slope sensitivity check, not a contradiction
  by_structure <- split(vapply(all_mv, function(x) x$kind %||% "", character(1)),
                        paste(vapply(all_mv, function(x) x$random_slope %||% "none", character(1)),
                              vapply(all_mv, function(x) x$settings_key %||% "", character(1))))
  models_disagree <- any(vapply(by_structure, function(g) length(unique(g)) > 1, logical(1)))
  if (!sel) {
    level <- if (vis_contra) "mixed" else "none"
  } else {
    level <- 4L
    if (any(statuses == "not saved")) level <- min(level, 3L)
    if (length(caveats) || identical(bias, "Moderate")) level <- min(level, 3L)
    if (models_ambiguous) level <- min(level, 3L)          # no clear winner among the models: at most moderate
    if (vis_contra) level <- min(level, 2L)
    if (length(fails)) level <- min(level, 2L)
    if (identical(bias, "High")) level <- min(level, 2L)
    level <- c("insufficient", "weak", "moderate", "strong")[[max(1L, level)]]
    if (level == "insufficient") level <- "weak"
    if (vis_contra && !length(fails) && !is.null(vis) && vis$kind %in% c("age_dependent", "age_independent")) level <- "mixed"
  }
  if (models_disagree && sel) level <- "mixed"

  ## what explains a weak result
  explanation <- NULL
  if ("slopes" %in% fails || "permutation" %in% fails)
    explanation <- "The model's support is better explained by individuals ageing at different rates than by selective disappearance."
  else if ("shape" %in% caveats && level != "strong")
    explanation <- "Resolve the ageing function first: a misspecified shape can produce this result."
  else if (identical(bias, "High"))
    explanation <- "Missing records depend on the trait, which can imitate selective disappearance."

  finding <- finding_sentence(kind, mv, vis, trait)
  headline <- switch(level,
    strong = sprintf("STRONG evidence: the saved results consistently suggest %s.", toupper(KIND_TEXT[[kind]])),
    moderate = sprintf("MODERATE evidence: the saved results suggest %s, but some checks are incomplete or carry caveats.", toupper(KIND_TEXT[[kind]])),
    weak = sprintf("WEAK evidence for %s.", toupper(KIND_TEXT[[kind]])),
    mixed = "MIXED evidence: the saved results point in different directions.",
    none = "The saved results are consistent with NO SELECTIVE DISAPPEARANCE.",
    "Not enough saved evidence to summarise yet.")
  order_keys <- if (identical(kind, "age_dependent")) c("visual", "models", "shape", "slopes", "permutation", "missingness")
                else c("visual", "models", "shape", "permutation", "missingness")
  pending <- unique(pending[order(match(pending, order_keys))])
  cautions <- character(0)
  if (n_superseded > 0)
    cautions <- c(cautions, sprintf("%d earlier model comparison%s with other data or settings %s set aside; the latest settings are used.",
                                    n_superseded, if (n_superseded == 1) "" else "s", if (n_superseded == 1) "was" else "were"))
  list(level = level, headline = headline, finding = finding, kind = kind,
       lines = do.call(rbind, lines), explanation = explanation, pending = pending, cautions = cautions)
}

# The checks still to run, as instructions a user can follow.
# The gap figure's reading, secondary to the coefficient analysis: a weighted least-squares line of the difference
# between neighbouring groups against age (inverse-variance weights when available), its level at the mean age and its
# slope. Its p-values are optimistic (points share individuals), and group gaps drift with age as each group loses
# its shortest-lived members first, so this reading can flag a caveat but never sets the evidence level.
visual_gap_stats <- function(dz, alpha = EVIDENCE_ALPHA) {
  if (is.null(dz) || !is.data.frame(dz) || nrow(dz) < 3 || length(unique(dz$age)) < 3) return(NULL)
  w <- if ("w" %in% names(dz) && all(is.finite(dz$w) & dz$w > 0)) dz$w else rep(1, nrow(dz))
  dd <- data.frame(difference = dz$difference, a = dz$age - sum(w * dz$age) / sum(w), wt = w)
  cf <- tryCatch(summary(stats::lm(difference ~ a, data = dd, weights = wt))$coefficients, error = function(e) NULL)
  if (is.null(cf) || nrow(cf) < 2 || ncol(cf) < 4) return(NULL)
  p_level <- cf[1, 4]
  p_trend <- cf[2, 4]
  kind <- if (is.finite(p_trend) && p_trend < alpha) "age_dependent" else if (is.finite(p_level) && p_level < alpha) "age_independent" else "none"
  list(kind = kind, level = cf[1, 1], p_level = p_level, trend = cf[2, 1], p_trend = p_trend, n = nrow(dd))
}

# The gap line of an evidence summary: "supports" when its reading agrees with the finding, otherwise "caveat".
gap_evidence_line <- function(vis, kind, groups) {
  gp <- vis$gap
  if (is.null(gp) || is.null(gp$kind)) return(NULL)
  data.frame(Evidence = "Gap between groups (secondary)", Status = if (identical(gp$kind, kind)) "supports" else "caveat",
             Note = paste0(switch(gp$kind, age_dependent = sprintf("the gap between %s changes with age", groups),
                                  age_independent = sprintf("%s differ by a constant gap", groups), sprintf("no clear gap between %s", groups)),
                           sprintf(" (average %s, p = %s; change per unit age %s, p = %s).",
                                   format_num(gp$level), format_p(gp$p_level), format_num(gp$trend), format_p(gp$p_trend))),
             Footnote = "Secondary evidence: these p-values are optimistic because points share individuals, and group gaps drift with age as each group loses its shortest-lived members first.",
             stringsAsFactors = FALSE)
}

# The model comparison's reading: one clear winner (no other model within 2 AIC of the best), or a supported set of
# several models, of which parsimony suggests interpreting the simplest (fewest parameters).
model_set_reading <- function(mv) {
  sup <- mv$supported %||% mv$best
  if (!length(sup)) sup <- mv$best
  if (length(sup) <= 1L) {
    nxt <- mv$next_model %||% NA_character_
    gap <- mv$gap_next %||% NA_real_
    m1 <- mv$gap_m1 %||% NA_real_
    return(list(clear = TRUE, note = paste0(mv$best, " best; ",
      if (!is.na(nxt) && is.finite(gap)) sprintf("the next model, %s, is %.1f AIC behind", nxt, gap) else "no other model is within 2 AIC",
      if (is.finite(m1) && !identical(nxt, "Model 1") && !identical(mv$best, "Model 1")) sprintf("; Model 1 is %.1f AIC behind", m1) else "")))
  }
  d <- mv$supported_df
  simplest <- if (length(d) && all(sup %in% names(d))) { d <- d[sup]; paste(names(d)[d == min(d)], collapse = " or ") } else NA_character_
  others <- setdiff(sup, mv$best)
  list(clear = FALSE,
       note = sprintf("%s best, but %s %s within 2 AIC of it, so the supported set is %s. Parsimony suggests interpreting the simplest%s.",
                      mv$best, paste(others, collapse = ", "), if (length(others) > 1) "are" else "is", paste(sup, collapse = ", "),
                      if (!is.na(simplest)) paste0(", ", simplest) else " of them"))
}

# The two statistics behind a visual reading, in words: the average coefficient of the trait on the proxy across ages
# (overall difference from zero) and its change with age (the slope across age), each with its bootstrap p-value.
visual_stats_note <- function(vis) {
  if (is.null(vis) || !isTRUE(is.finite(vis$p_level))) return("")
  lab <- switch(vis$proxy %||% "alr", life = "lifespan", entry = "AFR", "ALR")
  sprintf(" (coefficient of the trait on %s: average across ages %s, p = %s; change per unit age %s, p = %s)",
          lab, format_num(vis$mean_gap), format_p(vis$p_level), format_num(vis$slope), format_p(vis$p_trend))
}

KIND_TEXT_APP <- c(age_dependent = "age-dependent selective appearance",
                   age_independent = "age-independent selective appearance",
                   none = "no selective appearance")

#' Grade the saved evidence for selective appearance (individuals that enter the study later differ)
#'
#' Reads the figures saved with AFR as the grouping variable and the Models 7-10 part of the latest model comparison.
#' NULL when nothing about selective appearance was saved. The random-slope and null-model checks are run for
#' selective disappearance only, so this summary is at most 'moderate'.
#'
#' @param entries Saved evidence entries (as recorded by the app's "Save to summary" buttons).
#' @param trait Name of the trait, used in the returned notes.
#' @return A grading table (Evidence, Status, Note, Footnote), or NULL when nothing was saved.
grade_appearance <- function(entries, trait = "the trait") {
  vis_all <- Filter(function(x) identical(x$process %||% "", "appearance"), all_evidence(entries, "visual"))
  vis <- if (length(vis_all)) vis_all[[length(vis_all)]] else NULL
  mv <- latest_evidence(entries, "models")
  has_models <- !is.null(mv) && length(mv$appearance_kind) == 1L && !is.na(mv$appearance_kind)
  if (is.null(vis) && !has_models) return(NULL)
  line <- function(name, status, note) data.frame(Evidence = name, Status = status, Note = note, Footnote = "", stringsAsFactors = FALSE)
  pending <- character(0)
  vis_ok <- !is.null(vis) && isTRUE(vis$kind %in% names(KIND_TEXT_APP))
  kind <- if (has_models) mv$appearance_kind else if (vis_ok) vis$kind else "none"
  lines <- list()
  if (is.null(vis)) {
    lines[[1]] <- line("Visual pattern (AFR groups)", "not saved", "Save a step-2 figure grouped by AFR.")
    pending <- c(pending, "visual_afr")
  } else if (!vis_ok) {
    lines[[1]] <- line("Visual pattern (AFR groups)", "caveat", "too few individuals share each age to read the pattern")
  } else {
    note <- switch(vis$kind, age_dependent = "the gap between early- and late-entering individuals changes with age",
                   age_independent = "early- and late-entering individuals differ by a similar amount at all ages",
                   "early- and late-entering individuals do not clearly differ")
    lines[[1]] <- line("Visual pattern (AFR groups)", if (identical(vis$kind, kind)) "supports" else "contradicts",
                       paste0(note, visual_stats_note(vis)))
    gl <- gap_evidence_line(vis, kind, "early- and late-entering individuals")
    if (!is.null(gl)) lines[[length(lines) + 1]] <- gl
  }
  if (!has_models) {
    lines[[length(lines) + 1]] <- line("Model comparison (Models 7\u201310)", "not saved", "Fit Models 7\u201310 and save the model comparison.")
    pending <- c(pending, "models_afr")
  } else {
    rd <- model_set_reading(mv)
    if (kind %in% c("age_dependent", "age_independent")) {
      lines[[length(lines) + 1]] <- line("Model comparison (Models 7\u201310)", if (rd$clear) "supports" else "caveat", rd$note)
    } else {
      close <- is.finite(mv$appearance_gap %||% NA_real_) && mv$appearance_gap < 2
      lines[[length(lines) + 1]] <- line("Model comparison (Models 7\u201310)", if (close) "caveat" else "supports",
        if (close) paste0(sprintf("The best model with AFR, %s, is within 2 AIC of the best model. ", mv$appearance_model), rd$note)
        else sprintf("the best model with AFR, %s, is %.1f AIC behind the best model (%s)", mv$appearance_model, mv$appearance_gap, mv$best))
    }
  }
  miss_all <- all_evidence(entries, "missingness")
  bias <- NA_character_
  if (length(miss_all)) {
    pick <- function(f) { v <- vapply(miss_all, function(x) as.numeric(x[[f]] %||% NA_real_)[1], numeric(1)); v <- v[is.finite(v)]
                          if (length(v)) v[[length(v)]] else NA_real_ }
    tp <- pick("trait_p"); pc <- pick("percent")
    bias <- if (is.finite(tp) && tp < EVIDENCE_ALPHA) "High" else if (is.finite(pc) && pc >= 25) "Moderate" else "Low"
    lines[[length(lines) + 1]] <- line("Observation bias", switch(bias, High = "contradicts", Moderate = "caveat", "supports"),
      switch(bias, High = "missing records are associated with the trait itself", Moderate = sprintf("%.0f%% of expected occasions are missing", pc),
             "no sign that missingness depends on the trait"))
  }
  statuses <- vapply(lines, function(x) x$Status, character(1))
  vis_contra <- vis_ok && !identical(vis$kind, kind)
  sel <- kind %in% c("age_dependent", "age_independent")
  level <- if (!sel) {
    if (vis_contra) "mixed" else "none"
  } else {
    lv <- 3L                                               # at most moderate (see above)
    if (vis_contra || identical(bias, "High")) lv <- 2L
    c("insufficient", "weak", "moderate")[[lv]]
  }
  tr <- if (nzchar(trait %||% "")) trait else "the trait"
  s <- if (vis_ok && isTRUE(is.finite(vis$mean_gap))) sign(vis$mean_gap) else NA
  finding <- if (identical(kind, "age_dependent")) {
    sprintf("the difference in %s between early- and late-entering individuals changes with age", tr)
  } else if (identical(kind, "age_independent")) {
    if (is.finite(s) && s != 0) sprintf("individuals that enter the study later have consistently %s %s than those that enter earlier, by a similar amount at all ages",
                                        if (s > 0) "higher" else "lower", tr)
    else sprintf("%s differs between early- and late-entering individuals by a similar amount at all ages", tr)
  } else sprintf("there is no clear difference in %s between early- and late-entering individuals", tr)
  headline <- switch(level,
    moderate = sprintf("MODERATE evidence: the saved results suggest %s.", toupper(KIND_TEXT_APP[[kind]])),
    weak = sprintf("WEAK evidence for %s.", toupper(KIND_TEXT_APP[[kind]])),
    mixed = "MIXED evidence: the saved results point in different directions.",
    none = "The saved results are consistent with NO SELECTIVE APPEARANCE.",
    "Not enough saved evidence to summarise yet.")
  list(level = level, headline = headline, finding = finding, kind = kind, lines = do.call(rbind, lines),
       explanation = NULL, pending = pending,
       cautions = if (sel) "The random-slope and null-model checks are run for selective disappearance only, so this summary is at most 'moderate'." else character(0))
}

PENDING_TEXT <- c(
  visual_afr = "Save a step-2 figure with AFR as the grouping variable (step 2).",
  models_afr = "Fit Models 7\u201310 (they need individual AFR) and save the model comparison (step 5).",
  visual = "Save the trajectory figure 'Do long- and short-lived, or early- and late-entering, individuals differ in their phenotype and how it ages?' (step 2).",
  models = "Fit models and save the model comparison 'Which selective processes best explain the data?' (step 5, Fit tab).",
  shape = "Run and save the Ageing-function check (step 5, Checks tab), to confirm the ageing function is not misspecified.",
  slopes = "Refit with random slopes (step 5, model settings, random effects), then save the model comparison again.",
  permutation = "Run and save the null-model bootstrap on the finding's model (step 5, Advanced tab). The default 39 simulated datasets leave room for a few failed refits; at least 19 must be refitted to reach p < 0.05.",
  missingness = "Save 'Is what is missing related to the trait or the individual?' (step 3).")
