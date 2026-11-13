# disappR engine - Selective disappearance and appearance: proxy bins and slopes (A1-A2), disappearance models (A4-A7), the
# decomposition and AFR-ALR agreement.
# Engine code shared by the app and the R interface; changes that alter results must pass the golden tests.

# ---------------------------------------------------------------------------
# Proxies: individual-level values used in A1/A2
# ---------------------------------------------------------------------------
proxy_choices <- function(im, meta, target = "disappearance") {
  if (identical(target, "all")) {
    ch <- c("ALR", "Mean age")
    if (isTRUE(meta$has_life) && !isTRUE(meta$life_auto) && sum(is.finite(im$lifespan)) >= 4) ch <- c(ch, "LS")
    if (length(unique(im$entry[is.finite(im$entry)])) >= 2) ch <- c(ch, "AFR")
    return(ch)
  }
  if (identical(target, "appearance")) {
    if (length(unique(im$entry[is.finite(im$entry)])) < 2) return(character(0))
    return(c("AFR"))
  }
  ch <- c("ALR", "Mean age")
  if (isTRUE(meta$has_life) && !isTRUE(meta$life_auto) && sum(is.finite(im$lifespan)) >= 4) ch <- c(ch, "LS")
  ch
}

proxy_values <- function(im, proxy) {
  v <- switch(proxy, ALR = im$alr, `Mean age` = im$mean_age, LS = im$lifespan, AFR = im$entry, im$alr)
  if (is.null(v) || length(v) != nrow(im)) v <- rep(NA_real_, nrow(im))
  stats::setNames(v, im$id)
}

# ---------------------------------------------------------------------------
# A1: trait trajectories within proxy bins (individual-level binning)
# ---------------------------------------------------------------------------
# With no more distinct proxy values than requested bins, every distinct value is its own bin.
make_proxy_bins <- function(values, n_bins = 4, method = "equal") {
  x <- values[is.finite(values)]
  n_bins <- max(2L, as.integer(n_bins))
  ux <- sort(unique(x))
  if (length(ux) < 2) return(NULL)
  if (length(ux) <= n_bins) {
    labs <- make.unique(paste0(seq_along(ux), ": ", format_num(ux)))
    return(factor(labs[match(values, ux)], levels = labs))
  }
  br <- if (identical(method, "quantile")) {
    unique(as.numeric(stats::quantile(x, probs = seq(0, 1, length.out = n_bins + 1), type = 7)))
  } else {
    seq(min(x), max(x), length.out = n_bins + 1)
  }
  if (length(br) < 3) br <- seq(min(x), max(x), length.out = n_bins + 1)
  if (length(unique(br)) < 3) return(NULL)
  labs <- make.unique(paste0(seq_len(length(br) - 1), ": ", format_num(br[-length(br)]), "\u2013", format_num(br[-1])))
  cut(values, breaks = br, include.lowest = TRUE, labels = labs)
}

# Upper limit for the number of bins: distinct values (snapped to the sampling step) shared by at
# least n_min individuals. Individuals with a single record count at their only age.
max_bins_for <- function(values, n_min = 3, step = NA_real_, floor_bins = 3L) {
  v <- values[is.finite(values)]
  if (!length(v)) return(as.integer(floor_bins))
  if (is.finite(step) && step > 0) v <- round(v / step) * step
  tab <- table(v)
  as.integer(max(floor_bins, sum(tab >= max(1, n_min))))
}

# facet: optional named vector (individual id -> panel label); NULL puts everyone in one panel.
binned_trajectory <- function(dat, pvals, n_bins = 4, method = "equal", n_min = 3, facet = NULL) {
  b <- make_proxy_bins(pvals, n_bins, method)
  if (is.null(b)) return(data.frame())
  bmap <- stats::setNames(as.character(b), names(pvals))
  ia <- id_age_means(dat)
  if (!nrow(ia)) return(data.frame())
  ia$bin <- unname(bmap[ia$id])
  ia$facet <- if (is.null(facet)) "All" else unname(facet[ia$id])
  ia <- ia[!is.na(ia$bin) & !is.na(ia$facet), , drop = FALSE]
  if (!nrow(ia)) return(data.frame())
  key <- paste(ia$facet, ia$bin, ia$age, sep = "\r")
  first <- !duplicated(key)
  s <- ia[first, c("facet", "bin", "age"), drop = FALSE]
  kk <- key[first]
  s$mean <- as.numeric(tapply(ia$trait, key, mean)[kk])
  s$n <- as.numeric(tapply(ia$trait, key, length)[kk])
  s$se <- as.numeric(tapply(ia$trait, key, function(v) if (length(v) > 1) stats::sd(v) / sqrt(length(v)) else NA_real_)[kk])
  s$bin <- factor(s$bin, levels = levels(b))
  s <- s[s$n >= n_min, , drop = FALSE]
  s[order(s$facet, s$bin, s$age), , drop = FALSE]
}

# Differences in mean trait between proxy bins at each age (input: binned_trajectory output), per facet.
# mode = "successive": each bin minus the bin below it; mode = "all": every pair of bins.
# Differences are signed (higher bin minus lower bin) so that crossing trajectories stay visible.
bin_differences <- function(s, mode = "successive") {
  empty <- data.frame(age = numeric(0), difference = numeric(0), pair = character(0), facet = character(0), n = numeric(0),
                      var = numeric(0), w = numeric(0))
  if (!nrow(s)) return(empty)
  if (!"facet" %in% names(s)) s$facet <- "All"
  lev <- levels(s$bin)
  if (length(lev) < 2) return(empty)
  prs <- if (identical(mode, "all")) {
    utils::combn(seq_along(lev), 2, simplify = FALSE)
  } else {
    lapply(seq_len(length(lev) - 1), function(j) c(j, j + 1))
  }
  num <- sub(":.*$", "", lev)
  pair_levels <- vapply(prs, function(pr) paste0("Bin ", num[[pr[2]]], " \u2212 bin ", num[[pr[1]]]), character(1))
  rows <- list()
  for (fv in unique(as.character(s$facet))) {
    sf <- s[as.character(s$facet) == fv, , drop = FALSE]
    mean_of <- stats::setNames(sf$mean, paste(as.character(sf$bin), sf$age, sep = "\r"))
    n_of <- stats::setNames(if ("n" %in% names(sf)) sf$n else rep(NA_real_, nrow(sf)), names(mean_of))
    # Variance of each bin mean: its squared standard error. A bin whose individuals all share one value (or with
    # a single individual) has no usable standard error, so it takes the panel's pooled within-bin variance / n.
    sd2 <- if (all(c("se", "n") %in% names(sf))) sf$se^2 * sf$n else rep(NA_real_, nrow(sf))
    okv <- is.finite(sd2) & sd2 > 0 & is.finite(sf$n) & sf$n > 1
    pooled <- if (any(okv)) sum(sd2[okv] * (sf$n[okv] - 1)) / sum(sf$n[okv] - 1) else NA_real_
    var_of <- stats::setNames(ifelse(okv, sd2 / sf$n, pooled / sf$n), names(mean_of))
    ages <- sort(unique(sf$age))
    for (j in seq_along(prs)) {
      pr <- prs[[j]]
      a_lo <- mean_of[paste(lev[[pr[1]]], ages, sep = "\r")]
      a_hi <- mean_of[paste(lev[[pr[2]]], ages, sep = "\r")]
      ok <- !is.na(a_lo) & !is.na(a_hi)
      if (!any(ok)) next
      n_lo <- n_of[paste(lev[[pr[1]]], ages, sep = "\r")]
      n_hi <- n_of[paste(lev[[pr[2]]], ages, sep = "\r")]
      # the variance of a difference of two independent means is the sum of their variances; its inverse weights it
      vd <- as.numeric(var_of[paste(lev[[pr[1]]], ages, sep = "\r")][ok]) + as.numeric(var_of[paste(lev[[pr[2]]], ages, sep = "\r")][ok])
      rows[[length(rows) + 1]] <- data.frame(age = ages[ok], difference = as.numeric(a_hi[ok] - a_lo[ok]),
                                             pair = pair_levels[[j]], facet = fv,
                                             n = as.numeric(n_lo[ok]) + as.numeric(n_hi[ok]), var = vd,
                                             w = ifelse(is.finite(vd) & vd > 0, 1 / vd, NA_real_), stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) return(empty)
  out <- do.call(rbind, rows)
  out$pair <- factor(out$pair, levels = intersect(pair_levels, unique(out$pair)))
  out
}

# Least-squares trend of each bin difference against age (lines in the A1 difference plot); with weighted = TRUE
# each difference is weighted by the inverse of its variance (column w of bin_differences()).
bin_difference_trends <- function(dz, weighted = FALSE) {
  if (!nrow(dz)) return(data.frame())
  if (!"facet" %in% names(dz)) dz$facet <- "All"
  grp <- paste(dz$facet, as.character(dz$pair), sep = "\r")
  rows <- lapply(split(dz, grp), function(x) {
    if (weighted && "w" %in% names(x)) x <- x[is.finite(x$w) & x$w > 0, , drop = FALSE]
    if (length(unique(x$age)) < 2) return(NULL)
    fit <- if (weighted && "w" %in% names(x)) stats::lm(difference ~ age, data = x, weights = w)
           else stats::lm(difference ~ age, data = x)
    cf <- suppressWarnings(summary(fit)$coefficients)
    if (!"age" %in% rownames(cf)) return(NULL)
    p <- if (nrow(x) > 2 && ncol(cf) >= 4) cf["age", 4] else NA_real_
    data.frame(Facet = x$facet[[1]], Pair = as.character(x$pair[[1]]), N_ages = nrow(x),
               Age_from = min(x$age), Age_to = max(x$age), Intercept = cf[1, 1], Slope_per_age = cf["age", 1],
               P = p, stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  out <- do.call(rbind, rows)
  out$Pair <- factor(out$Pair, levels = levels(dz$pair))
  out <- out[order(out$Facet, out$Pair), , drop = FALSE]
  out$Pair <- as.character(out$Pair)
  rownames(out) <- NULL
  out
}

# A2: each individual's mean trait within age bins, against an individual-level proxy such as LS.
# With no more distinct ages than bins, every age is its own bin. age_mid = mean sampled age in the bin.
trait_by_age_bins <- function(dat, pvals, n_age_bins = 4, facet = NULL) {
  ia <- id_age_means(dat)
  if (!nrow(ia)) return(data.frame())
  ia$proxy <- unname(pvals[ia$id])
  ia$facet <- if (is.null(facet)) "All" else unname(facet[ia$id])
  ia <- ia[is.finite(ia$trait) & is.finite(ia$proxy) & !is.na(ia$facet), , drop = FALSE]
  if (!nrow(ia)) return(data.frame())
  ua <- sort(unique(ia$age))
  n_age_bins <- max(2L, as.integer(n_age_bins))
  if (length(ua) <= n_age_bins) {
    ia$age_bin <- factor(as.character(ia$age), levels = as.character(ua))
  } else {
    br <- seq(min(ua), max(ua), length.out = n_age_bins + 1)
    labs <- make.unique(paste0(format_num(br[-length(br)]), "\u2013", format_num(br[-1])))
    ia$age_bin <- cut(ia$age, breaks = br, include.lowest = TRUE, labels = labs)
  }
  z <- stats::aggregate(trait ~ id + facet + age_bin, data = ia, FUN = mean)
  z$age_bin <- factor(as.character(z$age_bin), levels = levels(ia$age_bin))
  z$proxy <- unname(pvals[as.character(z$id)])
  mids <- tapply(ia$age, ia$age_bin, mean)
  z$age_mid <- as.numeric(mids[as.character(z$age_bin)])
  z
}

# A2 coefficient table: regression coefficient of the trait on the proxy within each age bin (per
# facet), its 95% CI, and the change from the previous age bin.
a2_bin_slopes <- function(z) {
  if (!nrow(z)) return(data.frame())
  if (!"facet" %in% names(z)) z$facet <- "All"
  lev <- levels(z$age_bin)
  rows <- list()
  for (fv in unique(as.character(z$facet))) {
    for (ab in lev) {
      x <- z[as.character(z$facet) == fv & as.character(z$age_bin) == ab, , drop = FALSE]
      if (!nrow(x)) next
      est <- se <- lo <- hi <- r <- p <- NA_real_
      if (nrow(x) >= 3 && length(unique(x$proxy)) >= 2) {
        fit <- stats::lm(trait ~ proxy, data = x)
        cf <- suppressWarnings(summary(fit)$coefficients)
        if ("proxy" %in% rownames(cf)) {
          est <- cf["proxy", 1]
          se <- cf["proxy", 2]
          p <- if (ncol(cf) >= 4) cf["proxy", 4] else NA_real_
          tq <- stats::qt(0.975, df = nrow(x) - 2)
          lo <- est - tq * se
          hi <- est + tq * se
        }
        r <- suppressWarnings(stats::cor(x$proxy, x$trait))
      }
      rows[[length(rows) + 1]] <- data.frame(Facet = fv, Age_bin = ab, Mean_age = x$age_mid[[1]], N = nrow(x),
                                             Coefficient = est, SE = se, Lower_95 = lo, Upper_95 = hi, r = r, P = p,
                                             stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) return(data.frame())
  out <- do.call(rbind, rows)
  out$Change_from_previous <- stats::ave(out$Coefficient, out$Facet, FUN = function(v) c(NA_real_, diff(v)))
  out
}

# Weighted trend of the A2 coefficients across age bins (per facet): change per unit age.
a2_slope_trend <- function(tab) {
  if (!nrow(tab)) return(data.frame())
  rows <- lapply(split(tab, tab$Facet), function(x) {
    x <- x[is.finite(x$Coefficient) & is.finite(x$SE) & x$SE > 0, , drop = FALSE]
    if (nrow(x) < 3) return(NULL)
    fit <- stats::lm(Coefficient ~ Mean_age, data = x, weights = 1 / x$SE^2)
    cf <- suppressWarnings(summary(fit)$coefficients)
    if (!"Mean_age" %in% rownames(cf)) return(NULL)
    data.frame(Facet = x$Facet[[1]], Change_per_age = cf["Mean_age", 1], P = cf["Mean_age", 4], N_bins = nrow(x),
               stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# A2 (summary statistic): association between the proxy and the trait at each age
# ---------------------------------------------------------------------------
proxy_slopes_by_age <- function(dat, pvals, metric = "slope", max_ages = 30, min_n = 5) {
  empty <- list(table = data.frame(), trend_est = NA_real_, trend_p = NA_real_, mean_est = NA_real_,
                mean_p = NA_real_, metric = metric)
  ia <- id_age_means(dat)
  if (!nrow(ia)) return(empty)
  ia$proxy <- unname(pvals[ia$id])
  ia <- ia[is.finite(ia$trait) & is.finite(ia$proxy), , drop = FALSE]
  if (!nrow(ia)) return(empty)
  if (length(unique(ia$age)) > max_ages) {
    br <- unique(as.numeric(stats::quantile(ia$age, probs = seq(0, 1, length.out = 9))))
    grp <- cut(ia$age, breaks = br, include.lowest = TRUE)
    ia$age_grp <- as.numeric(stats::ave(ia$age, grp, FUN = mean))
  } else {
    ia$age_grp <- ia$age
  }
  res <- lapply(split(ia, ia$age_grp), function(x) {
    n <- nrow(x)
    if (n < min_n || length(unique(x$proxy)) < 2 || length(unique(x$trait)) < 2) return(NULL)
    if (identical(metric, "r")) {
      r <- stats::cor(x$proxy, x$trait)
      if (!is.finite(r) || n < 4) return(NULL)
      zr <- atanh(max(min(r, 0.999999), -0.999999))
      se <- 1 / sqrt(n - 3)
      data.frame(age = x$age_grp[[1]], estimate = r, lo = tanh(zr - 1.96 * se), hi = tanh(zr + 1.96 * se),
                 tz = zr, tse = se, n = n)
    } else {
      fit <- stats::lm(trait ~ proxy, data = x)
      cf <- summary(fit)$coefficients
      if (!"proxy" %in% rownames(cf)) return(NULL)
      est <- cf["proxy", 1]
      se <- cf["proxy", 2]
      if (!is.finite(se) || se <= 0) return(NULL)
      data.frame(age = x$age_grp[[1]], estimate = est, lo = est - 1.96 * se, hi = est + 1.96 * se,
                 tz = est, tse = se, n = n)
    }
  })
  res <- Filter(Negate(is.null), res)
  if (!length(res)) return(empty)
  tab <- do.call(rbind, res)
  tab <- tab[order(tab$age), , drop = FALSE]
  rownames(tab) <- NULL
  out <- empty
  out$table <- tab
  w <- 1 / tab$tse^2
  if (nrow(tab) >= 2) {
    m0 <- stats::lm(tz ~ 1, data = tab, weights = w)
    s0 <- summary(m0)$coefficients
    out$mean_est <- s0[1, 1]
    out$mean_p <- s0[1, 4]
  }
  if (nrow(tab) >= 3) {
    m1 <- stats::lm(tz ~ age, data = tab, weights = w)
    s1 <- summary(m1)$coefficients
    if ("age" %in% rownames(s1)) {
      out$trend_est <- s1["age", 1]
      out$trend_p <- s1["age", 4]
    }
  }
  out
}

# ---------------------------------------------------------------------------
# Decomposition (Rebke et al. 2010)
# ---------------------------------------------------------------------------
# Rebke-style decomposition on a POPULATION sampling grid.
#
# Occasions are the distinct ages at which the population was sampled (ages within a quarter of a step of each other
# are one occasion; repeated records of an individual within an occasion are averaged). Two occasions are LINKED when
# they are adjacent on that grid and about one sampling step apart (0.5 to 1.5 steps). An individual contributes to a
# link only when it was recorded at BOTH of its occasions, so a gap in an individual's records is a gap: an animal seen
# at ages 1, 2, 5 and 6 on a population grid of 1..6 contributes to 1->2 and to 5->6 and to nothing else, rather than
# having 2->5 treated as a transition because those are its own consecutive records.
#
# The curve is the chain of mean within-individual changes across linked occasions, anchored at the mean trait of the
# individuals contributing to the first link of the chain. Where the grid breaks (occasions more than about one step
# apart, or a link with no individual recorded at both ends) the chain cannot continue, so the curve is returned in
# segments; each segment is anchored separately and the "segment" column keeps them from being joined when drawn.
decomposition_trajectory <- function(dat, step = NULL) {
  ia <- id_age_means(dat)
  if (nrow(ia) < 2) return(data.frame())
  inferred <- infer_age_step(ia$age, ia$id)
  # an interval set to the inferred value changes nothing: the decomposition is then exactly the automatic one
  manual <- valid_age_step(step) && !isTRUE(abs(step - inferred) <= 1e-8 * max(1, step))
  step <- resolve_age_step(ia$age, ia$id, step)
  if (!is.finite(step) || step <= 0) return(data.frame())
  n_off <- 0L
  if (manual) {
    # An interval set by the user (0.22.0) defines the occasions. The grid is anchored on the most common phase of the
    # ages within the interval (so a design whose first occasion is offset, such as day 1 then weekly from day 7, keeps
    # its occasions), records more than a quarter of an interval off that grid are left out (for example an extra
    # mid-year record in annual sampling, which would otherwise break the chain of annual links), and each occasion is
    # drawn at the mean of its records' real ages, so no age is moved.
    ph <- (round(((ia$age / step) %% 1) * 20) / 20) %% 1
    a0 <- as.numeric(names(which.max(table(ph)))) * step
    kk <- (ia$age - a0) / step
    on_grid <- abs(kk - round(kk)) <= 0.25
    n_off <- sum(!on_grid)
    ia <- ia[on_grid, , drop = FALSE]
    ia$age_real <- ia$age
    ia$age <- a0 + round(kk[on_grid]) * step
    if (nrow(ia) < 2) return(data.frame())
  }
  ia <- ia[order(ia$age), , drop = FALSE]
  # population occasions: distinct ages, merging ages closer together than a quarter of a step
  ages <- sort(unique(ia$age))
  occ_id <- cumsum(c(TRUE, diff(ages) > 0.25 * step))
  occ_age <- as.numeric(tapply(ages, occ_id, mean))
  merged <- any(as.numeric(table(occ_id)) > 1)
  ia$occ <- occ_id[match(ia$age, ages)]
  if (manual) occ_age <- as.numeric(tapply(ia$age_real, factor(ia$occ, levels = seq_along(occ_age)), mean))
  # one value per individual per occasion
  key <- paste(ia$id, ia$occ, sep = "\r")
  if (anyDuplicated(key)) {
    agg <- stats::aggregate(list(trait = ia$trait), by = list(id = ia$id, occ = ia$occ), FUN = mean, na.rm = TRUE)
    ia <- agg[order(agg$occ), , drop = FALSE]
  }
  k <- length(occ_age)
  if (k < 2) return(data.frame())
  by_occ <- split(ia, ia$occ)
  inc <- rep(NA_real_, k - 1); npair <- integer(k - 1); anchor <- rep(NA_real_, k - 1)
  for (j in seq_len(k - 1)) {
    gap <- occ_age[j + 1] - occ_age[j]
    if (!(gap >= 0.5 * step && gap < 1.5 * step)) next          # occasions must be one sampling step apart
    a <- by_occ[[as.character(j)]]; b <- by_occ[[as.character(j + 1)]]
    if (is.null(a) || is.null(b)) next
    both <- intersect(a$id, b$id)                                # recorded at BOTH occasions
    if (!length(both)) next
    ta <- a$trait[match(both, a$id)]; tb <- b$trait[match(both, b$id)]
    ok <- is.finite(ta) & is.finite(tb)
    if (!any(ok)) next
    inc[j] <- mean(tb[ok] - ta[ok]); npair[j] <- sum(ok); anchor[j] <- mean(ta[ok])
  }
  if (!any(is.finite(inc))) return(data.frame())
  out <- NULL; seg <- 0L; j <- 1L
  while (j <= k - 1) {
    if (!is.finite(inc[j])) { j <- j + 1L; next }
    seg <- seg + 1L
    current <- anchor[j]
    out <- rbind(out, data.frame(age = occ_age[j], fitted = current, n_pairs = npair[j], segment = seg))
    while (j <= k - 1 && is.finite(inc[j])) {
      current <- current + inc[j]
      out <- rbind(out, data.frame(age = occ_age[j + 1], fitted = current, n_pairs = npair[j], segment = seg))
      j <- j + 1L
    }
  }
  if (is.null(out)) return(data.frame())
  rownames(out) <- NULL
  cau <- character(0)
  if (merged) cau <- c(cau, "records of different ages were merged into shared occasions, so the decomposition is approximate")
  if (seg > 1L) cau <- c(cau, sprintf("the sampling grid breaks, so the decomposition is drawn in %d separate segments that cannot be joined (a chain of within-individual changes cannot cross an occasion where no individual was recorded at both ends)", seg))
  if (n_off > 0) cau <- c(cau, sprintf("%d record%s off the set interval's grid left out", n_off, if (n_off == 1) "" else "s"))
  if (length(cau)) attr(out, "caution") <- paste0(paste(cau, collapse = "; "), ".")
  out
}

# ---------------------------------------------------------------------------
# A4-A7: disappearance diagnostics on person-occasion data
# ---------------------------------------------------------------------------
# One row per individual x sampled age (with a trait value). "Disappears" = dies before the next sampling
# occasion when lifespan is known; otherwise the record is the individual's last one. Records of censored
# individuals at their last record, and last records at the oldest sampled age of the study (their fate is
# unknown), are excluded. Trait is standardised within each age (z); change = trait minus the individual's
# record one sampling step earlier.
disappearance_data <- function(dat, meta, log_scale = FALSE, step = meta$age_step) {
  ia <- id_age_means(dat)
  if (nrow(ia) < 10) return(NULL)
  if (isTRUE(log_scale)) ia$trait <- log1p(pmax(ia$trait, 0))
  step <- resolve_age_step(ia$age, ia$id, step)
  if (!is.finite(step) || step <= 0) step <- 1
  im <- individual_metrics(dat)
  mi <- match(ia$id, im$id)
  life <- im$lifespan[mi]
  last <- stats::ave(ia$age, ia$id, FUN = max)
  cens <- ia$id %in% (meta$censored_ids %||% character(0))
  known <- isTRUE(meta$has_life) && !isTRUE(meta$life_auto) && sum(is.finite(im$lifespan)) >= 5
  tol <- 1e-8
  if (known) {
    event <- ifelse(is.finite(life), as.numeric(life < ia$age + step - tol), NA_real_)
    event[!is.finite(life) & ia$age < last] <- 0
  } else {
    event <- as.numeric(abs(ia$age - last) < tol)
    event[event == 1 & abs(last - max(ia$age)) < tol] <- NA_real_
  }
  event[cens & abs(ia$age - last) < tol] <- NA_real_
  ia$event <- event
  ia <- ia[order(ia$id, ia$age), , drop = FALSE]
  n <- nrow(ia)
  prev_same <- c(FALSE, ia$id[-1] == ia$id[-n] & abs(diff(ia$age) - step) <= 0.01 * step)
  ia$change <- NA_real_
  ia$change[prev_same] <- ia$trait[prev_same] - ia$trait[which(prev_same) - 1]
  zs <- stats::ave(ia$trait, ia$age, FUN = function(v) {
    s <- stats::sd(v)
    if (length(v) >= 3 && is.finite(s) && s > 0) (v - mean(v)) / s else rep(NA_real_, length(v))
  })
  ia$trait_z <- zs
  ia$step <- step
  attr(ia, "lifespan_known") <- known
  ia
}

# Discrete-time hazard: logit P(disappear before next occasion) = age-specific baseline + beta * trait_z
# (+ trait_z x age, + change). Age classes are the sampled ages (or 10 quantile classes if there are more
# than 30); classes with no events or only events carry no information and are removed.
# Discrete-time hazard models with an individual random intercept (frailty) when it can be estimated (0.23.2; code
# review): person-occasions of one individual are not independent, so a plain binomial GLM gives anticonservative
# p-values. Falls back to the GLM when lme4 is missing, the data are very large, too few individuals, the fit fails,
# or the frailty variance is estimated at zero. Returns list(fit, type).
hazard_glmm <- function(f, data, max_rows = 50000) {
  glm_fit <- function() tryCatch(suppressWarnings(stats::glm(stats::as.formula(f), data = data, family = stats::binomial())), error = function(e) NULL)
  if (!requireNamespace("lme4", quietly = TRUE) || nrow(data) > max_rows || !"id" %in% names(data) || length(unique(data$id)) < 10)
    return(list(fit = glm_fit(), type = "GLM"))
  g <- tryCatch(suppressWarnings(suppressMessages(
         lme4::glmer(stats::as.formula(paste(f, "+ (1 | id)")), data = data, family = stats::binomial(),
                     control = lme4::glmerControl(optimizer = "bobyqa")))), error = function(e) NULL)
  if (is.null(g)) return(list(fit = glm_fit(), type = "GLM (frailty model failed)"))
  if (isTRUE(lme4::isSingular(g))) return(list(fit = glm_fit(), type = "GLM (frailty variance at zero)"))
  list(fit = g, type = "GLMM with individual frailty")
}
hazard_fixef <- function(fit) if (inherits(fit, "merMod")) lme4::fixef(fit) else stats::coef(fit)
# Likelihood-ratio test of two nested hazard fits; both are refitted as GLMs when only one could take the frailty
hazard_lrt <- function(f0, f1, data, df) {
  a <- hazard_glmm(f0, data); b <- hazard_glmm(f1, data)
  if (!identical(inherits(a$fit, "merMod"), inherits(b$fit, "merMod"))) {
    a <- list(fit = tryCatch(suppressWarnings(stats::glm(stats::as.formula(f0), data = data, family = stats::binomial())), error = function(e) NULL), type = "GLM")
    b <- list(fit = tryCatch(suppressWarnings(stats::glm(stats::as.formula(f1), data = data, family = stats::binomial())), error = function(e) NULL), type = "GLM")
  }
  if (is.null(a$fit) || is.null(b$fit)) return(list(p = NA_real_, type = a$type))
  stat <- max(0, 2 * (as.numeric(stats::logLik(b$fit)) - as.numeric(stats::logLik(a$fit))))
  list(p = stats::pchisq(stat, df = max(1, df), lower.tail = FALSE), type = b$type)
}

disappearance_models <- function(ia) {
  if (is.null(ia)) return(list(ok = FALSE, message = "Too few records."))
  x <- ia[is.finite(ia$event) & is.finite(ia$trait_z), , drop = FALSE]
  if (nrow(x) < 30 || sum(x$event) < 5 || sum(1 - x$event) < 5) {
    return(list(ok = FALSE, message = sprintf("Too few records or disappearances (%d records, %d disappearances; need 30 and 5).", nrow(x), sum(x$event))))
  }
  ua <- sort(unique(x$age))
  x$age_class <- if (length(ua) <= 30) factor(x$age) else cut(x$age, unique(stats::quantile(x$age, seq(0, 1, 0.1))), include.lowest = TRUE)
  inf <- tapply(x$event, x$age_class, function(e) any(e == 1) && any(e == 0))
  x <- droplevels(x[x$age_class %in% names(inf)[inf %in% TRUE], , drop = FALSE])
  if (nrow(x) < 30 || nlevels(x$age_class) < 1) return(list(ok = FALSE, message = "No age class has both survivors and disappearances."))
  x$age_c <- (x$age - mean(x$age)) / (if (stats::sd(x$age) > 0) stats::sd(x$age) else 1)
  base_rhs <- if (nlevels(x$age_class) > 1) "age_class" else "1"
  types <- character(0)
  glm_safe <- function(f, data) { h <- hazard_glmm(f, data); types <<- c(types, h$type); h$fit }
  row_for <- function(fit, term, label, n, ev) {
    if (is.null(fit)) return(NULL)
    cf <- summary(fit)$coefficients
    if (!term %in% rownames(cf)) return(NULL)
    est <- cf[term, 1]; se <- cf[term, 2]
    data.frame(Model = label, Term = term, Odds_ratio = exp(est), Lower_95 = exp(est - 1.96 * se), Upper_95 = exp(est + 1.96 * se),
               P = cf[term, 4], Records = n, Disappearances = ev,
               Fit = if (inherits(fit, "merMod")) "GLMM, individual frailty" else "GLM", stringsAsFactors = FALSE)
  }
  m1 <- glm_safe(paste("event ~", base_rhs, "+ trait_z"), x)
  m2 <- glm_safe(paste("event ~", base_rhs, "+ trait_z + trait_z:age_c"), x)
  xc <- x[is.finite(x$change), , drop = FALSE]
  m3 <- NULL
  if (nrow(xc) >= 30 && sum(xc$event) >= 5 && stats::sd(xc$change) > 0) {
    xc$change_z <- (xc$change - mean(xc$change)) / stats::sd(xc$change)
    xc <- droplevels(xc)
    m3 <- glm_safe(paste("event ~", if (nlevels(xc$age_class) > 1) "age_class" else "1", "+ trait_z + change_z"), xc)
  }
  tab <- rbind(row_for(m1, "trait_z", "Trait now", nrow(x), sum(x$event)),
               row_for(m2, "trait_z:age_c", "Trait effect changing with age (per SD of age)", nrow(x), sum(x$event)),
               row_for(m3, "trait_z", "Trait now, adjusting for change", nrow(xc), sum(xc$event)),
               row_for(m3, "change_z", "Change since the previous occasion", nrow(xc), sum(xc$event)))
  p_age <- if (!is.null(m1) && !is.null(m2)) hazard_lrt(paste("event ~", base_rhs, "+ trait_z"), paste("event ~", base_rhs, "+ trait_z + trait_z:age_c"), x, 1)$p else NA_real_
  ages <- as.numeric(stats::quantile(x$age, c(0.1, 0.5, 0.9), names = FALSE))
  or_at <- if (!is.null(m2)) {
    cf <- hazard_fixef(m2)
    vc <- as.matrix(stats::vcov(m2))
    ac <- (ages - mean(x$age)) / (if (stats::sd(x$age) > 0) stats::sd(x$age) else 1)
    data.frame(Age = ages, Odds_ratio = exp(cf[["trait_z"]] + cf[["trait_z:age_c"]] * ac),
               Lower_95 = exp(cf[["trait_z"]] + cf[["trait_z:age_c"]] * ac - 1.96 * sqrt(vc["trait_z", "trait_z"] + ac^2 * vc["trait_z:age_c", "trait_z:age_c"] + 2 * ac * vc["trait_z", "trait_z:age_c"])),
               Upper_95 = exp(cf[["trait_z"]] + cf[["trait_z:age_c"]] * ac + 1.96 * sqrt(vc["trait_z", "trait_z"] + ac^2 * vc["trait_z:age_c", "trait_z:age_c"] + 2 * ac * vc["trait_z", "trait_z:age_c"])))
  } else data.frame()
  # empirical disappearance rates by within-age trait quintile, in three age bands
  x$band <- cut(x$age, unique(stats::quantile(x$age, c(0, 1/3, 2/3, 1))), include.lowest = TRUE)
  x$tq <- cut(x$trait_z, unique(stats::quantile(x$trait_z, seq(0, 1, 0.2))), include.lowest = TRUE)
  emp <- stats::aggregate(cbind(event, trait_z) ~ band + tq, data = x, FUN = mean)
  emp$n <- stats::aggregate(event ~ band + tq, data = x, FUN = length)$event
  list(ok = !is.null(tab) && nrow(tab) > 0, table = tab, p_age_dependence = p_age, or_at_ages = or_at, empirical = emp,
       lifespan_known = isTRUE(attr(ia, "lifespan_known")), hazard_model = unique(types), message = "")
}

# A6: selection differentials at each age, in within-age SD units. S = mean(survivors) - mean(all at that age);
# contrast = mean(survivors) - mean(disappearing). Ages need >= 3 survivors and >= 3 disappearances.
selection_differentials <- function(ia) {
  if (is.null(ia)) return(data.frame())
  x <- ia[is.finite(ia$event) & is.finite(ia$trait), , drop = FALSE]
  rows <- lapply(split(x, x$age), function(g) {
    s <- g$trait[g$event == 0]; d <- g$trait[g$event == 1]
    if (length(s) < 3 || length(d) < 3) return(NULL)
    sdv <- stats::sd(g$trait)
    if (!is.finite(sdv) || sdv <= 0) return(NULL)
    p <- length(d) / nrow(g)
    diff_sd <- (mean(s) - mean(d)) / sdv
    se_diff <- sqrt(stats::var(s) / length(s) + stats::var(d) / length(d)) / sdv
    data.frame(Age = g$age[[1]], N = nrow(g), Disappearing = length(d), Proportion_disappearing = p,
               Differential = p * diff_sd, Differential_SE = p * se_diff, Contrast = diff_sd, Contrast_SE = se_diff,
               stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  out <- do.call(rbind, rows)
  out[order(out$Age), , drop = FALSE]
}

# A7: life table of disappearance. Known lifespan: at risk at age a = individuals first observed at or before a
# with LS >= a (censored individuals stop contributing after their last record); events = LS in [a, a + step).
# Unknown lifespan: ALR replaces LS and events at the oldest sampled age are not counted.
life_table <- function(dat, meta, step = meta$age_step) {
  im <- individual_metrics(dat)
  im <- im[is.finite(im$first_recorded), , drop = FALSE]
  if (nrow(im) < 5) return(list(ok = FALSE, message = "Too few individuals."))
  step <- resolve_age_step(dat$age, dat$id, step)
  if (!is.finite(step) || step <= 0) step <- 1
  known <- isTRUE(meta$has_life) && !isTRUE(meta$life_auto) && sum(is.finite(im$lifespan)) >= 5
  cens <- im$id %in% (meta$censored_ids %||% character(0))
  start <- ifelse(is.finite(im$entry), pmin(im$entry, im$first_recorded), im$first_recorded)
  end <- if (known) ifelse(is.finite(im$lifespan), im$lifespan, im$last_recorded) else im$last_recorded
  dead <- if (known) is.finite(im$lifespan) & !cens else !cens & im$last_recorded < max(dat$age) - 1e-8
  ages <- seq(min(start), max(end), by = step)
  rows <- lapply(ages, function(a) {
    at_risk <- sum(start <= a + 1e-8 & end >= a - 1e-8)
    events <- sum(dead & end >= a - 1e-8 & end < a + step - 1e-8)
    if (at_risk == 0) return(NULL)
    q <- events / at_risk
    z <- 1.96
    centre <- (q + z^2 / (2 * at_risk)) / (1 + z^2 / at_risk)
    half <- z * sqrt(q * (1 - q) / at_risk + z^2 / (4 * at_risk^2)) / (1 + z^2 / at_risk)
    data.frame(Age = a, At_risk = at_risk, Disappearances = events, Hazard = q, Lower_95 = max(0, centre - half), Upper_95 = min(1, centre + half))
  })
  lt <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(lt) || !nrow(lt) || sum(lt$Disappearances) == 0) return(list(ok = FALSE, message = "No disappearances to estimate a hazard."))
  overall <- sum(lt$Disappearances) / sum(lt$At_risk)
  lt_fit <- lt[lt$At_risk > 0, , drop = FALSE]
  p_const <- tryCatch({
    m0 <- suppressWarnings(stats::glm(cbind(Disappearances, At_risk - Disappearances) ~ 1, data = lt_fit, family = stats::binomial()))
    m1 <- suppressWarnings(stats::glm(cbind(Disappearances, At_risk - Disappearances) ~ factor(Age), data = lt_fit, family = stats::binomial()))
    stats::pchisq(max(0, m0$deviance - m1$deviance), df = max(1, m0$df.residual - m1$df.residual), lower.tail = FALSE)
  }, error = function(e) NA_real_)
  hazard_model <- "GLM"
  # the same test on individual person-occasions with an individual frailty (0.23.2); kept when the frailty can be
  # estimated, otherwise the test above stands
  po <- tryCatch({
    rows_i <- lapply(seq_along(start), function(i) {
      a <- ages[ages >= start[[i]] - 1e-8 & ages <= end[[i]] + 1e-8]
      if (!length(a)) return(NULL)
      data.frame(id = im$id[[i]], Age = a, event = as.integer(dead[[i]] & end[[i]] >= a - 1e-8 & end[[i]] < a + step - 1e-8))
    })
    do.call(rbind, rows_i)
  }, error = function(e) NULL)
  if (is.data.frame(po) && nrow(po) && length(unique(po$Age)) > 1) {
    po$Age <- factor(po$Age)
    lr <- tryCatch(hazard_lrt("event ~ 1", "event ~ Age", po, nlevels(po$Age) - 1), error = function(e) NULL)
    if (!is.null(lr) && is.finite(lr$p) && identical(lr$type, "GLMM with individual frailty")) {
      p_const <- lr$p
      hazard_model <- lr$type
    } else if (!is.null(lr)) hazard_model <- lr$type
  }
  list(ok = TRUE, table = lt, overall = overall, p_constant = p_const, lifespan_known = known, step = step, hazard_model = hazard_model)
}

# A5: trait against occasions before death (known LS) or before the last record, by lifespan tercile.
terminal_data <- function(dat, meta, n_min = 3, step = meta$age_step) {
  ia <- id_age_means(dat)
  if (nrow(ia) < 10) return(data.frame())
  # deviation from the mean of all individuals at the same age, so that the population age trend does not
  # masquerade as a terminal change (occasions further before death are also younger ages)
  ia$resid <- ia$trait - stats::ave(ia$trait, ia$age, FUN = mean)
  step <- resolve_age_step(ia$age, ia$id, step)
  if (!is.finite(step) || step <= 0) step <- 1
  im <- individual_metrics(dat)
  known <- isTRUE(meta$has_life) && !isTRUE(meta$life_auto) && sum(is.finite(im$lifespan)) >= 5
  cens <- im$id %in% (meta$censored_ids %||% character(0))
  endv <- if (known) im$lifespan else im$last_recorded
  endv[cens] <- NA_real_
  mi <- match(ia$id, im$id)
  ia$end <- endv[mi]
  ia <- ia[is.finite(ia$end), , drop = FALSE]
  if (nrow(ia) < 10) return(data.frame())
  ia$before <- floor((ia$end - ia$age) / step + 1e-8)
  ia <- ia[ia$before >= 0, , drop = FALSE]
  if (nrow(ia) < 3) return(data.frame())
  ends <- endv[is.finite(endv)]
  br <- unique(stats::quantile(ends, c(0, 1/3, 2/3, 1), names = FALSE))
  if (length(br) < 2) return(data.frame())
  ia$end_bin <- cut(ia$end, br, include.lowest = TRUE, labels = make.unique(paste0(format_num(br[-length(br)]), "\u2013", format_num(br[-1]))))
  z <- stats::aggregate(cbind(trait, resid) ~ end_bin + before, data = ia, FUN = mean)
  z$n <- stats::aggregate(trait ~ end_bin + before, data = ia, FUN = length)$trait
  z <- z[z$n >= n_min, , drop = FALSE]
  attr(z, "lifespan_known") <- known
  z
}

# Agreement between age at first record and age at last record. A strong positive association means
# individuals recorded late are also recorded late in life, so entry and exit are not independent and the
# observation window, rather than lifespan alone, drives who is seen at which age.
afr_alr_agreement <- function(dat, meta) {
  im <- individual_metrics(dat)
  im <- im[im$n_trait > 0, , drop = FALSE]
  afr <- ifelse(is.finite(im$entry), im$entry, im$first_recorded)
  alr <- im$alr
  ok <- is.finite(afr) & is.finite(alr)
  if (sum(ok) < 5 || stats::sd(afr[ok]) == 0 || stats::sd(alr[ok]) == 0) {
    return(list(ok = FALSE, message = if (sum(ok) < 5) "Fewer than 5 individuals with both AFR and ALR." else
      "AFR or ALR does not vary among individuals, so the two cannot be correlated."))
  }
  r <- stats::cor(afr[ok], alr[ok])
  rho <- suppressWarnings(stats::cor(afr[ok], alr[ok], method = "spearman"))
  span <- alr[ok] - afr[ok]
  list(ok = TRUE, n = sum(ok), r = r, rho = rho, mean_span = mean(span), sd_span = stats::sd(span),
       table = data.frame(
         Measure = c("Individuals", "Correlation AFR vs ALR (Pearson)", "Correlation AFR vs ALR (Spearman)",
                     "Observation window (ALR \u2212 AFR)"),
         Value = c(format(sum(ok), big.mark = ","), format_num(r), format_num(rho),
                   sprintf("mean %s, SD %s", format_num(mean(span)), format_num(stats::sd(span)))),
         stringsAsFactors = FALSE))
}

# ---- Moved unchanged from inst/app/server.R (0.9.11): pure helpers that use no reactive state ----

proxy_pair_plot <- function(x, y, xlab, ylab) {
  ok <- is.finite(x) & is.finite(y)
  shiny::validate(shiny::need(sum(ok) >= 4, "Not enough individuals with both values."))
  df <- data.frame(x = x[ok], y = y[ok])
  r <- cor_safe(df$x, df$y)
  ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_count(colour = warm_palette[[3]], alpha = 0.7) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 3, colour = "grey50") +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "black", linewidth = 0.9, na.rm = TRUE) +
    ggplot2::annotate("text", x = -Inf, y = Inf, label = if (is.finite(r)) sprintf("r = %.3f", r) else "r = NA",
                      hjust = -0.2, vjust = 1.5, colour = "#453A32") +
    ggplot2::scale_size_area(max_size = 6, guide = "none") +
    ggplot2::labs(x = xlab, y = ylab, subtitle = paste(ylab, "vs", xlab, "(one point per individual; dotted line: 1:1; solid black line: linear regression)")) +
    theme_disappR(12)
}
