# disappR engine - Missingness: the expected sampling grid, coverage by age and missingness drivers.
# Engine code shared by the app and the R interface; changes that alter results must pass the golden tests.

# ---------------------------------------------------------------------------
# Expected sampling grid and missingness (B3 inputs)
# ---------------------------------------------------------------------------
# Each individual's expected window runs from its AFR to its ALR, both included (0.24.2): the AFR and ALR the models
# use, i.e. the mapped columns, or else the first and last ages with a trait value (trait-specific, the default) or
# of any record (dataset-wide). An occasion in the window without a trait value counts as missed, including an AFR
# or ALR record without one. Nothing after the ALR is counted, with or without a known lifespan (the gap between
# ALR and death is summarised separately, by r(ALR, LS)).
# start_mode = "afr": the window opens at the individual's AFR (never after its first trait record).
# start_mode = "same": trait expression starts at start_age for everyone, so unrecorded occasions between
# start_age and an individual's first record count as missing.
build_missing_grid <- function(dat, margin = 0, start_mode = "afr", start_age = NA_real_, max_cells = 300000, step = NULL) {
  d <- dat[is.finite(dat$age), , drop = FALSE]
  if (!nrow(d)) return(data.frame())
  # Individuals never measured for the trait enter no model, so they are left out of the grid (and counted).
  has_trait <- tapply(is.finite(d$trait), d$id, any)
  n_no_trait <- sum(!has_trait)
  d <- d[d$id %in% names(has_trait)[has_trait], , drop = FALSE]
  if (!nrow(d)) return(data.frame())
  step0 <- resolve_age_step(d$age, d$id, step)   # a sampling interval set in the app, else the inferred one
  if (!is.finite(step0) || step0 <= 0) step0 <- 1
  ids <- unique(d$id)
  f <- factor(d$id, levels = ids)
  fi <- as.integer(f)
  first_age <- as.numeric(tapply(d$age, f, min))
  col_or_na <- function(nm) if (nm %in% names(d)) d[[nm]] else rep(NA_real_, nrow(d))
  entry <- as.numeric(tapply(col_or_na("entry"), f, function(v) if (any(is.finite(v))) min(v[is.finite(v)]) else NA_real_))
  life <- as.numeric(tapply(col_or_na("life"), f, finite_mean))
  alr_i <- as.numeric(tapply(if ("alr" %in% names(d)) d$alr else rep(NA_real_, nrow(d)), f, finite_mean))
  amax <- max(c(d$age, alr_i[is.finite(alr_i)]))
  common_start <- identical(start_mode, "same") && isTRUE(is.finite(start_age))
  # An age at first trait expression (AFE), where supplied, opens the window earlier than entry into the
  # dataset: occasions between expression and entry then count as missed rather than as "not yet expressing".
  first_trait_age <- as.numeric(tapply(ifelse(is.finite(d$trait), d$age, NA_real_), f, function(v) min(v, na.rm = TRUE)))
  # the AFR the models use; a mapped AFR later than the first trait record (an inconsistency) cannot push observed
  # records out of the window
  afr_ref <- ifelse(is.finite(entry), pmin(entry, first_trait_age), first_trait_age)
  # AFE mode opens the window at the earlier of the AFE and the AFR (0.24.4; before, at the earlier of the AFE and the
  # first record of any kind, which ignored the trait-specific option and a mapped AFR)
  start_ref <- if (common_start) pmin(start_age, afr_ref) else afr_ref
  build <- function(step) {
    # Each individual's schedule is anchored on its first record, so individuals sampled on schedules
    # offset from each other (staggered cohorts) are not given spurious missed occasions.
    n_before <- pmax(0, floor((first_age - start_ref) / step + 1e-9))
    anchor <- first_age - n_before * step
    k_rec <- round((d$age - anchor[fi]) / step)
    last_k <- as.numeric(tapply(k_rec, f, max))
    # The window closes at the ALR the models use (0.24.2; before, at the last record of any kind when that was
    # later): a mapped ALR later than the last record keeps those occasions expected, and a trait record after a
    # mapped ALR (an inconsistency) stays inside the window
    last_tk <- as.numeric(tapply(ifelse(is.finite(d$trait), k_rec, NA_real_), f,
                                 function(v) if (any(is.finite(v))) max(v, na.rm = TRUE) else NA_real_))
    alr_k <- ifelse(is.finite(alr_i), round((alr_i - anchor) / step), NA_real_)
    end_k <- pmax(alr_k, last_tk, na.rm = TRUE)
    end_k <- pmin(end_k, floor((amax - anchor) / step + 1e-9))
    end_k <- pmax(end_k, last_tk, na.rm = TRUE)
    end_k[!is.finite(end_k)] <- last_k[!is.finite(end_k)]
    # The window opens at start_ref: the AFR (AFR mode), or the earlier of the AFE and the AFR (AFE mode); a record
    # without a trait value from there on counts as missed
    first_k <- round((start_ref - anchor) / step)
    start_k <- pmax(0, first_k, end_k - 4999)
    list(anchor = anchor, k_rec = k_rec, start_k = start_k, end_k = end_k,
         n_cells = as.integer(pmax(1, end_k - start_k + 1)))
  }
  step <- step0
  b <- build(step)
  coarsen <- 1
  while (sum(as.numeric(b$n_cells)) > max_cells && coarsen < 1e6) {
    coarsen <- coarsen * max(2, ceiling(sum(as.numeric(b$n_cells)) / max_cells))
    step <- step0 * coarsen
    b <- build(step)
  }

  gi <- rep(seq_along(ids), b$n_cells)
  grid <- data.frame(id = ids[gi], stringsAsFactors = FALSE)
  grid$k <- rep(b$start_k, b$n_cells) + sequence(b$n_cells) - 1
  grid$age <- b$anchor[gi] + grid$k * step

  okt <- is.finite(d$trait)
  dkey <- paste(d$id, b$k_rec, sep = "\r")
  gkey <- paste(grid$id, grid$k, sep = "\r")
  grid$trait <- NA_real_
  if (any(okt)) {
    cell_mean <- tapply(d$trait[okt], dkey[okt], mean)
    grid$trait <- as.numeric(cell_mean[gkey])
  }
  grid$trait_observed <- is.finite(grid$trait)
  grid$missing <- !grid$trait_observed

  fo <- rep(NA_real_, length(ids))
  lo <- rep(NA_real_, length(ids))
  if (any(okt)) {
    fok <- factor(d$id[okt], levels = ids)
    fo <- as.numeric(tapply(b$k_rec[okt], fok, min))
    lo <- as.numeric(tapply(b$k_rec[okt], fok, max))
  }
  grid$pattern <- ifelse(!grid$missing, "Observed",
                  ifelse(!is.finite(fo[gi]), "Missed: no trait records",
                  ifelse(grid$k < fo[gi], "Missed: before first record",
                  ifelse(grid$k > lo[gi], "Missed: after last record", "Missed: between records"))))

  im <- individual_metrics(d)
  mm <- match(grid$id, im$id)
  grid$life <- im$lifespan[mm]
  grid$entry <- im$entry[mm]
  grid$alr <- im$alr[mm]
  grid$condition <- im$condition[mm]
  grid$condition_label <- im$condition_label[mm]
  grid$afr <- afr_ref[gi]   # the AFR the window opens at (shown as the first-record tile)

  n <- nrow(grid)
  tr <- grid$trait
  loi <- cummax(ifelse(is.finite(tr), seq_len(n), 0))
  prev <- c(0, loi[-n])
  okp <- prev > 0
  if (any(okp)) okp[okp] <- grid$id[prev[okp]] == grid$id[okp]
  grid$prior_trait <- NA_real_
  grid$prior_trait[okp] <- tr[prev[okp]]
  grid$anchor <- b$anchor[gi]
  grid$step <- step
  attr(grid, "n_no_trait") <- n_no_trait
  attr(grid, "step_note") <- if (coarsen > 1) {
    sprintf("The sampling step was coarsened from %s to %s to keep the grid below %s cells; consider rounding ages on the Data tab.",
            format_num(step0), format_num(step), format(max_cells, big.mark = ","))
  } else ""
  grid
}

missingness_drivers <- function(grid, alpha = 0.05) {
  empty <- data.frame(Predictor = character(0), Effect = character(0), P_value = numeric(0), Signal = character(0))
  if (!nrow(grid) || length(unique(grid$missing)) < 2 || sum(grid$missing) < 10) return(empty)
  specs <- c(Age = "age", LS = "life", AFR = "entry", `Prior observed trait` = "prior_trait", Condition = "condition")
  ans <- lapply(names(specs), function(lbl) {
    x <- grid[[specs[[lbl]]]]
    ok <- is.finite(x)
    if (sum(ok) < 20 || length(unique(x[ok])) < 2) return(NULL)
    z <- data.frame(missing = as.integer(grid$missing[ok]), x = as.numeric(scale(x[ok])))
    if (length(unique(z$missing)) < 2) return(NULL)
    fit <- tryCatch(suppressWarnings(stats::glm(missing ~ x, data = z, family = stats::binomial())), error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    sm <- summary(fit)$coefficients
    if (!"x" %in% rownames(sm)) return(NULL)
    p <- sm["x", 4]
    data.frame(Predictor = lbl, Effect = sprintf("OR %.2f per SD", exp(sm["x", 1])), P_value = p,
               Signal = if (is.finite(p) && p < alpha) "Associated" else "Not detected", stringsAsFactors = FALSE)
  })
  ans <- Filter(Negate(is.null), ans)
  if (!length(ans)) return(empty)
  do.call(rbind, ans)
}

missingness_summary <- function(dat, grid, alpha = 0.05, life_known = FALSE) {
  im <- individual_metrics(dat)
  out <- list(percent = NA_real_, p = NA_real_, step = NA_real_,
              r_mean_alr = cor_safe(im$mean_age, im$alr), r_alr_ls = NA_real_, r_mean_ls = NA_real_,
              gap_steps = numeric(0), pattern = "Unknown", drivers = missingness_drivers(grid, alpha),
              guidance = "Insufficient information.", type = "Could not construct the sampling grid.")
  if (!nrow(grid)) return(out)
  out$step <- grid$step[[1]]
  out$step_note <- attr(grid, "step_note") %||% ""
  out$percent <- 100 * mean(grid$missing)
  out$p <- 1 - mean(grid$missing)
  if (isTRUE(life_known)) {
    out$r_alr_ls <- cor_safe(im$alr, im$lifespan)
    out$r_mean_ls <- cor_safe(im$mean_age, im$lifespan)
    g <- (im$lifespan - im$alr) / out$step
    out$gap_steps <- g[is.finite(g)]
  }
  tab <- table(grid$pattern[grid$missing])
  out$pattern <- if (!length(tab)) "No missing cells" else
    paste(paste0(names(tab), ": ", as.integer(tab), " (", round(100 * as.numeric(tab) / sum(tab)), "%)"), collapse = "; ")
  drv <- out$drivers
  sig <- if (nrow(drv)) drv$Predictor[is.finite(drv$P_value) & drv$P_value < alpha] else character(0)
  out$type <- if (out$percent < 1) {
    "Sampling is effectively complete within the expected windows."
  } else if (!length(sig)) {
    "No association between missingness and the tested observed variables (compatible with, but not proof of, MCAR)."
  } else {
    paste0("Missingness is associated with: ", paste(sig, collapse = ", "), ".")
  }
  rtxt <- if (is.finite(out$r_mean_alr)) sprintf(" (r mean age\u2013ALR = %.3f)", out$r_mean_alr) else ""
  ls_txt <- if (is.finite(out$r_alr_ls) && is.finite(out$r_mean_ls)) {
    sprintf(" With known lifespan: r(ALR, LS) = %.2f, r(mean age, LS) = %.2f.", out$r_alr_ls, out$r_mean_ls)
  } else ""
  out$guidance <- if (out$p >= 0.95) {
    paste0("Detection is near complete (p = ", sprintf("%.2f", out$p), "): ALR and mean age are near-equivalent proxies", rtxt,
           ". Expect Models 4 and 5 to receive similar support; differences then mainly reflect their different design spaces, not proxy quality.", ls_txt)
  } else {
    paste0("Detection is incomplete (p = ", sprintf("%.2f", out$p), "): intermittent gaps shift mean age but not ALR, so ALR is usually the less error-prone lifespan proxy", rtxt,
           ". In the manuscript's MCAR, MWO and MWY simulations Model 4 outperformed Model 5; compare both rather than defaulting to mean-age centring.", ls_txt)
  }
  out$guidance <- paste(out$guidance, if (isTRUE(life_known))
    "Missed occasions are counted only between each individual's first observation and its ALR, so occasions between the ALR and death are not counted; the agreement between ALR and lifespan shows how far ALR falls short."
  else "Missed occasions are counted only between each individual's first observation and its ALR, so any missed occasions near death are invisible and p is an upper bound.")
  if (nzchar(out$step_note %||% "")) out$type <- paste(out$type, out$step_note)
  out
}

missingness_by_variable <- function(dat, grid, variable = "Age") {
  if (!nrow(grid)) return(data.frame())
  if (identical(variable, "Age")) {
    z <- stats::aggregate(as.integer(grid$missing), list(value = grid$age), mean)
    names(z)[2] <- "missing_rate"
    z$n <- as.numeric(table(grid$age)[as.character(z$value)])
    z$type <- "continuous"
    return(z)
  }
  im <- individual_metrics(dat)
  val <- switch(variable, LS = im$lifespan, ALR = im$alr, Trait = im$trait_mean,
                Condition = if (any(is.finite(im$condition))) im$condition else im$condition_label, im$mean_age)
  if (is.null(val) || length(val) != nrow(im)) val <- rep(NA_real_, nrow(im))
  map <- stats::setNames(val, im$id)
  x <- grid
  x$x <- unname(map[x$id])
  if (is.numeric(x$x)) {
    ok <- is.finite(x$x)
    if (!any(ok)) return(data.frame())
    if (length(unique(x$x[ok])) > 8) {
      q <- unique(stats::quantile(x$x[ok], probs = seq(0, 1, length.out = 7), type = 7))
      if (length(q) >= 3) {
        x <- x[ok, , drop = FALSE]
        x$group <- cut(x$x, breaks = q, include.lowest = TRUE)
        z <- stats::aggregate(as.integer(x$missing), list(group = x$group), mean)
        names(z)[2] <- "missing_rate"
        mids <- tapply(x$x, x$group, mean)
        z$value <- as.numeric(mids[as.character(z$group)])
        z$type <- "binned"
        return(z[order(z$value), , drop = FALSE])
      }
    }
    x <- x[ok, , drop = FALSE]
    z <- stats::aggregate(as.integer(x$missing), list(value = x$x), mean)
    names(z)[2] <- "missing_rate"
    z$type <- "continuous"
    return(z)
  }
  x$group <- as.character(x$x)
  x$group[is.na(x$group) | !nzchar(x$group)] <- "Unknown"
  z <- stats::aggregate(as.integer(x$missing), list(group = x$group), mean)
  names(z)[2] <- "missing_rate"
  z$type <- "categorical"
  z
}

# Sampling grid for display: every shown individual x expected occasion across the displayed age range
# (on that individual's own schedule), classified as observed, missed (expected but unrecorded), final
# expected occasion (death if LS is known, else ALR), or not expected (before AFR or after death / ALR).
# The number of tiles is capped so that the plot stays drawable; attr "n_shown" gives the individuals kept.
grid_display <- function(grid, ids, max_tiles = 60000, life_known = FALSE) {
  g <- grid[grid$id %in% ids, , drop = FALSE]
  if (!nrow(g)) return(data.frame())
  step <- g$step[[1]]
  ids <- ids[ids %in% g$id]
  anc <- tapply(g$anchor, g$id, function(v) v[[1]])
  a_i <- as.numeric(anc[ids])
  amin <- min(g$age)
  amax <- max(g$age)
  kmin <- ceiling((amin - a_i) / step - 1e-9)
  kmax <- floor((amax - a_i) / step + 1e-9)
  n <- as.integer(pmax(1, kmax - kmin + 1))
  if (sum(as.numeric(n)) > max_tiles) {
    keep <- unique(round(seq(1, length(ids), length.out = max(2, floor(max_tiles / mean(n))))))
    ids <- ids[keep]; a_i <- a_i[keep]; kmin <- kmin[keep]; n <- n[keep]
  }
  full <- data.frame(id = rep(ids, n), stringsAsFactors = FALSE)
  full$k <- rep(kmin, n) + sequence(n) - 1
  full$age <- rep(a_i, n) + full$k * step
  m <- match(paste(full$id, full$k, sep = "\r"), paste(g$id, g$k, sep = "\r"))
  status <- ifelse(is.na(m), "Not expected", ifelse(g$missing[m], "Missed", "Observed"))
  # The last OBSERVED occasion is the ALR the rows can be sorted by, so it is marked on its own. When lifespan is
  # known, expected occasions continue to death, and the occasions in between show as missed.
  obs <- !g$missing
  last_obs_k <- if (any(obs)) tapply(g$k[obs], g$id[obs], max) else numeric(0)
  lo <- as.numeric(last_obs_k[full$id])
  # the marker is the ALR the models use (last recorded age, with or without a trait value, or the mapped
  # column); grids built without it fall back to the last observed occasion
  if ("alr" %in% names(g)) {
    alr_by <- tapply(g$alr, g$id, function(v) v[[1]])
    anc_by <- tapply(g$anchor, g$id, function(v) v[[1]])
    ak <- round((as.numeric(alr_by[full$id]) - as.numeric(anc_by[full$id])) / step)
    lo <- ifelse(is.finite(ak), ak, lo)
  }
  end_k <- as.numeric(tapply(g$k, g$id, max)[full$id])
  # the AFR the window opens at is marked in blue (0.24.2); where it is also the ALR (a single occasion), the tile has
  # its own colour and legend entry (0.24.7)
  afr_outline <- rep(FALSE, nrow(full))
  if ("afr" %in% names(g)) {
    afr_by <- tapply(g$afr, g$id, function(v) v[[1]])
    anc_by2 <- tapply(g$anchor, g$id, function(v) v[[1]])
    fk <- round((as.numeric(afr_by[full$id]) - as.numeric(anc_by2[full$id])) / step)
    at_afr <- !is.na(m) & is.finite(fk) & full$k == fk
    status[at_afr] <- "First record (AFR)"
    afr_outline <- at_afr & is.finite(lo) & full$k == lo
  }
  status[!is.na(m) & is.finite(lo) & full$k == lo] <- "Last record (ALR)"
  status[afr_outline] <- "AFR = ALR (one occasion)"
  full$afr_outline <- afr_outline
  # the window ends at the ALR, so nothing after it is drawn as expected, with or without a known lifespan
  full$status <- status
  attr(full, "n_shown") <- length(ids)
  full
}

coverage_by_age <- function(grid) {
  if (!nrow(grid)) return(data.frame())
  ages <- sort(unique(grid$age))
  n_obs <- as.numeric(tapply(grid$trait_observed, grid$age, sum)[as.character(ages)])
  n_exp <- as.numeric(table(grid$age)[as.character(ages)])
  data.frame(Age = ages, Expected = n_exp, Observed = n_obs, Percent_observed = round(100 * n_obs / n_exp, 1))
}
