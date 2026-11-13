# disappR engine - Data preparation: standardised analysis data, individual-level metrics, age bases and model data.
# Engine code shared by the app and the R interface; changes that alter results must pass the golden tests.

cov_name <- function(nm) paste0("cv_", make.names(nm))

re_name <- function(nm) paste0("re_", make.names(nm))

# ---------------------------------------------------------------------------
# Standardise the user's data into the internal format
# ---------------------------------------------------------------------------
# The column of the second grouping level (the group containing the group, e.g. family above father), or "" when it
# is not mapped, missing from the data, or duplicates the group, ID, age or trait column.
group2_column <- function(map, cols) {
  g2 <- map$group2 %||% ""
  g1 <- map$group %||% ""
  ok <- length(g2) == 1 && !is.na(g2) && nzchar(g2) && g2 %in% cols &&
    length(g1) == 1 && !is.na(g1) && nzchar(g1) && g1 %in% cols && !g2 %in% c(g1, map$id, map$age, map$trait)
  if (isTRUE(ok)) g2 else ""
}

# Individual identifiers as the models see them, built from the raw columns exactly as in standardise_data():
# with nesting, group + ID, and top-level group + group + ID when a second grouping level is mapped.
individual_ids <- function(df, map) {
  ids <- trimws(as.character(df[[map$id]]))
  clean <- function(nm) {
    v <- trimws(as.character(df[[nm]]))
    v[!is.na(v) & !nzchar(v)] <- NA_character_
    v
  }
  g1 <- map$group %||% ""
  if (!isTRUE(map$nested) || !(length(g1) == 1 && !is.na(g1) && nzchar(g1) && g1 %in% names(df))) return(ids)
  g <- clean(g1)
  g2 <- group2_column(map, names(df))
  if (nzchar(g2)) {
    h <- clean(g2)
    g <- ifelse(is.na(g) | is.na(h), g, paste(h, g, sep = "/"))
  }
  ifelse(is.na(g), ids, paste(g, ids, sep = "/"))
}

#' Individuals whose single-valued measures disagree between their records
#'
#' An individual has one ALR, one lifespan and one AFR. When a mapped column gives an individual different values in
#' different records - a column that repeats each record's age, or two individuals sharing an ID - there is no single
#' correct value (their mean can even fall before the individual's own last record), so standardise_data() stops with
#' a data-integrity error naming them, or excludes them when the mapping asks it to (inconsistent = "exclude").
#' Columns the app computes itself are constant by construction.
#'
#' @param dat Standardised data with alr, life and entry columns.
#' @param tol Relative tolerance for treating two values as equal.
#' @return A named list of IDs by column (alr, life, entry); empty when there are no conflicts.
individual_value_conflicts <- function(dat, tol = 1e-8) {
  out <- list()
  for (v in intersect(c("alr", "life", "entry"), names(dat))) {
    x <- suppressWarnings(as.numeric(dat[[v]]))
    ok <- is.finite(x)
    if (!any(ok)) next
    lim <- tol * max(1, max(abs(x[ok])))
    rng <- tapply(x[ok], as.character(dat$id[ok]), function(z) max(z) - min(z))
    bad <- names(rng)[is.finite(rng) & rng > lim]
    if (length(bad)) out[[v]] <- bad
  }
  out
}

# The data-integrity error: which column differs within which individuals, and what to do about it.
# IDs that appear under more than one group, and IDs missing a group on some rows. With nesting the individual is
# "group/ID", so each of these becomes several individuals - right when IDs are reused between groups, wrong when the
# same animal moved between them. The default stands; the data checks name them so the user can decide (0.20.19).
multi_group_report <- function(id, group) {
  out <- list(ids = character(0), examples = character(0), n_gap = 0L)
  if (!length(id)) return(out)
  idc <- as.character(id)
  g <- ifelse(is.na(group) | !nzchar(as.character(group)), "(no group)", as.character(group))
  tab <- tapply(g, idc, function(v) length(unique(v)))
  ids <- names(tab)[is.finite(tab) & tab > 1]
  if (!length(ids)) return(out)
  groups_of <- function(i) sort(unique(g[idc == i]))
  out$ids <- ids
  out$n_gap <- sum(vapply(ids, function(i) "(no group)" %in% groups_of(i), logical(1)))
  out$examples <- unname(vapply(utils::head(ids, 6L), function(i) sprintf("%s (%s)", i, paste(groups_of(i), collapse = ", ")), character(1)))
  out
}

# Individual x age rows that appear more than once, split into exact duplicates (every mapped column agrees) and rows
# that differ, with the differing columns and a few examples named for the data checks.
duplicate_report <- function(out) {
  empty <- list(identical = character(0), differing = character(0), columns = character(0), examples = character(0))
  if (!nrow(out)) return(empty)
  key <- paste(out$id, out$age, sep = "\r")
  dup_keys <- unique(key[duplicated(key)])
  if (!length(dup_keys)) return(empty)
  cols <- setdiff(names(out), ".row")
  same_cols <- lapply(dup_keys, function(k) {
    rows <- out[key == k, cols, drop = FALSE]
    vapply(cols, function(cc) { v <- rows[[cc]]; all(is.na(v)) || length(unique(v[!is.na(v)])) == 1L && !any(is.na(v)) }, logical(1))
  })
  ident <- vapply(same_cols, all, logical(1))
  label <- function(k) { p <- strsplit(k, "\r", fixed = TRUE)[[1]]; sprintf("%s at age %s", p[[1]], p[[2]]) }
  differing <- dup_keys[!ident]
  list(identical = dup_keys[ident], differing = differing,
       columns = unique(unlist(lapply(same_cols[!ident], function(s) names(s)[!s]))),
       examples = vapply(utils::head(differing, 6L), label, character(1)))
}

integrity_condition <- function(conflicts, map) {
  role_lab <- c(alr = "ALR", life = "lifespan", entry = "AFR")
  role_col <- c(alr = map$alr %||% "", life = map$life %||% "", entry = map$entry %||% "")
  parts <- vapply(names(conflicts), function(v) {
    ids <- conflicts[[v]]
    sprintf("%s (column '%s') differs between the records of %d individual%s: %s%s", role_lab[[v]], role_col[[v]],
            length(ids), if (length(ids) == 1) "" else "s", paste(utils::head(ids, 5), collapse = ", "),
            if (length(ids) > 5) ", ..." else "")
  }, character(1))
  msg <- paste0("Data integrity: each individual must have one ALR, one lifespan and one AFR. ", paste(parts, collapse = "; "),
                ". Check whether that column repeats the age of each record, or whether two individuals share an ID (then map",
                " a group column so IDs are nested within it). To analyse the other individuals, exclude these: in the app,",
                " tick the exclusion box under the lifespan column; in R, set mapping$inconsistent = \"exclude\".")
  structure(class = c("disappr_integrity_error", "error", "condition"),
            list(message = msg, call = NULL, conflicts = conflicts))
}

standardise_data <- function(df, map, dup_action = "keep") {
  if (!is.data.frame(df)) stop("The data must be a data frame.", call. = FALSE)
  n_raw <- nrow(df)
  has_col <- function(nm) length(nm) == 1 && !is.na(nm) && nzchar(nm) && nm %in% names(df)
  # a mapped column that is not in the data gives a clear message, not "arguments imply differing number of rows"
  need <- c(id = map$id, age = map$age, trait = map$trait)
  gone <- need[!vapply(need, has_col, logical(1))]
  if (length(gone)) stop(structure(class = c("disappr_input_error", "error", "condition"), list(
    message = sprintf("Mapped column%s not found in the data: %s. Available: %s.",
                      if (length(gone) == 1) "" else "s",
                      paste(sprintf("%s (%s)", gone, names(gone)), collapse = ", "),
                      paste(utils::head(names(df), 12), collapse = ", ")),
    call = NULL)))
  out <- data.frame(
    id = trimws(as.character(df[[map$id]])),
    age = safe_numeric(df[[map$age]]),
    trait = safe_numeric(df[[map$trait]]),
    .row = seq_len(n_raw),
    stringsAsFactors = FALSE
  )
  # optional rounding of ages to a sampling resolution (e.g. irregular field ages)
  age_res <- suppressWarnings(as.numeric(map$age_round %||% NA_real_))
  rounding <- length(age_res) == 1 && is.finite(age_res) && age_res > 0
  round_age <- function(a) if (rounding) round(a / age_res) * age_res else a
  out$age <- round_age(out$age)
  keep <- !is.na(out$id) & nzchar(out$id) & is.finite(out$age)
  out <- out[keep, , drop = FALSE]
  rows <- out$.row
  n_dropped <- n_raw - nrow(out)

  has_group <- has_col(map$group)
  # Optional second grouping level above the group (e.g. individuals within fathers within families).
  group2_col <- group2_column(map, names(df))
  has_group2 <- has_group && nzchar(group2_col)
  out$group <- if (has_group) trimws(as.character(df[[map$group]][rows])) else rep(NA_character_, nrow(out))
  out$group[!is.na(out$group) & !nzchar(out$group)] <- NA_character_
  out$group2 <- if (has_group2) trimws(as.character(df[[group2_col]][rows])) else rep(NA_character_, nrow(out))
  out$group2[!is.na(out$group2) & !nzchar(out$group2)] <- NA_character_
  n_multi_group <- 0L
  n_multi_group2 <- 0L
  # set before the branch: without a mapped group there is nothing to report, and the meta list reads it either way
  multi_group <- list(ids = character(0), examples = character(0), n_gap = 0L)
  if (has_group && nrow(out)) {
    if (has_group2) {
      ng2 <- tapply(out$group2, out$group, function(g) length(unique(g[!is.na(g)])))
      n_multi_group2 <- sum(ng2 > 1)
      # Three levels: a group is identified by top-level group + group (equivalent to (1 | group2/group)).
      if (isTRUE(map$nested)) out$group <- ifelse(is.na(out$group) | is.na(out$group2), out$group, paste(out$group2, out$group, sep = "/"))
    }
    ng <- tapply(out$group, out$id, function(g) length(unique(g[!is.na(g)])))
    n_multi_group <- sum(ng > 1)
    multi_group <- multi_group_report(out$id, out$group)
    # Nested design: an individual is identified by group + ID (equivalent to (1 | group/ID)).
    if (isTRUE(map$nested)) out$id <- ifelse(is.na(out$group), out$id, paste(out$group, out$id, sep = "/"))
  }

  # Automatic ALR and AFR (0.24.2). Trait-specific (the default, map$trait_specific_ages = TRUE): the last and first
  # ages at which the trait has a value (zero is a value, NA is not), the same records mean age uses. Dataset-wide
  # (FALSE, used by the bundled examples): the last and first ages of any record, with or without a trait value.
  # Individuals without a single trait value get NA under the trait-specific rule; they enter no model.
  trait_specific <- !isFALSE(map$trait_specific_ages)
  if (nrow(out)) {
    a_use <- if (trait_specific) ifelse(is.finite(out$trait), out$age, NA_real_) else out$age
    last_rec <- stats::ave(a_use, out$id, FUN = function(v) if (any(is.finite(v))) max(v, na.rm = TRUE) else NA_real_)
    first_rec <- stats::ave(a_use, out$id, FUN = function(v) if (any(is.finite(v))) min(v, na.rm = TRUE) else NA_real_)
  } else {
    last_rec <- numeric(0)
    first_rec <- numeric(0)
  }

  alr_mapped <- has_col(map$alr) && !identical(map$alr, "__AUTO_LAST__")
  out$alr <- if (alr_mapped) round_age(safe_numeric(df[[map$alr]])[rows]) else last_rec

  life_auto <- identical(map$life, "__AUTO_LAST__")
  out$life <- if (life_auto) last_rec else if (has_col(map$life)) safe_numeric(df[[map$life]])[rows] else rep(NA_real_, nrow(out))
  # Censored individuals (e.g. escaped or still alive at the end of the study): their recorded
  # lifespan is not a death age, so LS is set to missing for every row of those individuals.
  n_censored <- 0L
  censored_ids <- character(0)
  has_censor <- has_col(map$censor) && !life_auto && nrow(out) > 0
  if (has_censor) {
    cens_row <- as.character(df[[map$censor]][rows]) == as.character(map$censor_value %||% "")
    cens_row[is.na(cens_row)] <- FALSE
    cens_ids <- unique(out$id[cens_row])
    n_censored <- length(cens_ids)
    censored_ids <- cens_ids
    out$life[out$id %in% cens_ids] <- NA_real_
  }

  # AFR only. The age at first trait expression is never used here: it affects the missingness window alone.
  entry_mapped <- has_col(map$entry) && !identical(map$entry, "__AUTO_FIRST__")
  out$entry <- if (entry_mapped) round_age(safe_numeric(df[[map$entry]])[rows]) else first_rec

  # Number of binomial trials behind each proportion (optional; used as prior weights).
  has_trials <- has_col(map$trials)
  out$.trials <- if (has_trials) safe_numeric(df[[map$trials]])[rows] else rep(NA_real_, nrow(out))
  n_bad_trials <- if (has_trials) sum(!is.finite(out$.trials) | out$.trials <= 0) else 0L
  out$condition <- rep(NA_real_, nrow(out))
  out$condition_label <- rep(NA_character_, nrow(out))
  if (has_col(map$condition)) {
    c0 <- df[[map$condition]][rows]
    cn <- safe_numeric(c0)
    if (sum(is.finite(cn)) >= 0.8 * sum(!is.na(c0) & nzchar(as.character(c0)))) {
      out$condition <- cn
    } else {
      out$condition_label <- as.character(c0)
    }
  }

  covars <- character(0)
  cov_labels <- character(0)
  cov_types <- character(0)
  cov_age <- character(0)
  cov_pairs <- character(0)
  skipped <- character(0)
  # map$cov_factor lists categorical covariates; the others are continuous. Without it (older presets),
  # a covariate is continuous when at least 95% of its values are numbers.
  declared <- !is.null(map$cov_factor)
  for (nm in unique(map$covars %||% character(0))) {
    if (!has_col(nm) || nm %in% c(map$id, map$age, map$trait, map$group, if (has_group2) group2_col)) next
    raw_vals <- df[[nm]][rows]
    vals <- if (!declared) {
      maybe_numeric(raw_vals)
    } else if (nm %in% map$cov_factor) {
      v <- trimws(as.character(raw_vals))
      v[!is.na(v) & !nzchar(v)] <- NA_character_
      v
    } else {
      safe_numeric(raw_vals)
    }
    if (!any(!is.na(vals) & nzchar(as.character(vals)))) {
      skipped <- c(skipped, nm)
      next
    }
    cn <- cov_name(nm)
    while (cn %in% names(out)) cn <- paste0(cn, "_")
    out[[cn]] <- vals
    if (nm %in% (map$cov_age %||% character(0))) cov_age <- c(cov_age, cn)
    covars <- c(covars, cn)
    cov_labels[cn] <- nm
    cov_types[cn] <- if (is.numeric(vals)) "continuous" else "categorical"
  }
  # interactions chosen as "a|||b" (two covariates) or "a|||age" (covariate x ageing terms)
  # (with no covariates, names(cov_labels) is NULL and setNames() would fail on it)
  internal_of <- if (length(cov_labels)) stats::setNames(names(cov_labels), unname(cov_labels)) else character(0)
  for (pr in unique(map$cov_int %||% character(0))) {
    parts <- strsplit(pr, "|||", fixed = TRUE)[[1]]
    if (length(parts) != 2 || !parts[[1]] %in% names(internal_of)) next
    if (identical(parts[[2]], "age")) {
      cov_age <- union(cov_age, internal_of[[parts[[1]]]])
    } else if (parts[[2]] %in% names(internal_of) && !identical(parts[[1]], parts[[2]])) {
      cov_pairs <- union(cov_pairs, paste(internal_of[[parts[[1]]]], internal_of[[parts[[2]]]], sep = ":"))
    }
  }

  random_terms <- character(0)
  random_labels <- character(0)
  for (nm in map$random %||% character(0)) {
    if (!has_col(nm) || nm %in% c(map$id, map$age, map$trait, map$group, if (has_group2) group2_col)) next
    vals <- trimws(as.character(df[[nm]][rows]))
    vals[!is.na(vals) & !nzchar(vals)] <- NA_character_
    if (all(is.na(vals))) {
      skipped <- c(skipped, nm)
      next
    }
    rn <- re_name(nm)
    while (rn %in% names(out)) rn <- paste0(rn, "_")
    out[[rn]] <- vals
    random_terms <- c(random_terms, rn)
    random_labels[rn] <- nm
  }

  n_dup_raw <- if (nrow(out)) sum(duplicated(paste(out$id, out$age, sep = "\r"))) else 0L
  # Two rows for the same individual and age are only ever collapsed when they agree in every mapped column - a row
  # entered twice (0.20.18). Rows that differ in anything, including the trait itself, are two measurements and are
  # kept; the data checks name them. Averaging them would invent values: a mean proportion beside a trials count it
  # never came from, or one trait's value merged with another's.
  dup <- duplicate_report(out)
  if (identical(dup_action, "mean") && length(dup$identical)) {
    key <- paste(out$id, out$age, sep = "\r")
    out <- out[!(duplicated(key) & key %in% dup$identical), , drop = FALSE]
  }

  out$.row <- NULL
  out <- out[order(out$id, out$age), , drop = FALSE]
  # One ALR, lifespan and AFR per individual. Averaging conflicting values (0.20.1) produced impossible ones - a
  # lifespan before the individual's own last breeding record, an AFR after its first - so loading stops here and
  # names the individuals, unless the mapping asks to exclude them.
  conflicts <- individual_value_conflicts(out)
  conflict_ids <- unique(unlist(conflicts, use.names = FALSE))
  n_conflict_rows <- 0L
  if (length(conflict_ids)) {
    if (!identical(map$inconsistent %||% "error", "exclude")) stop(integrity_condition(conflicts, map))
    n_conflict_rows <- sum(out$id %in% conflict_ids)
    out <- out[!out$id %in% conflict_ids, , drop = FALSE]
  }
  rownames(out) <- NULL
  meta <- list(
    n_raw = n_raw, n_dropped = n_dropped, n_dup = n_dup_raw, dup_action = dup_action,
    n_dup_identical = length(dup$identical), n_dup_differing = length(dup$differing), dup_examples = dup$examples,
    dup_columns = dup$columns,
    alr_mapped = alr_mapped, life_auto = life_auto, has_life = life_auto || has_col(map$life),
    trait_specific_ages = trait_specific,
    entry_mapped = entry_mapped, has_group = has_group, nested = isTRUE(map$nested),
    n_multi_group = n_multi_group, multi_group_examples = multi_group$examples, n_multi_group_gap = multi_group$n_gap,
    has_group2 = has_group2, n_multi_group2 = n_multi_group2, covars = covars, cov_labels = cov_labels,
    random_terms = random_terms, random_labels = random_labels,
    has_trials = has_trials, n_bad_trials = n_bad_trials, trials_col = if (has_trials) map$trials else "",
    has_censor = has_censor, n_censored = n_censored, age_round = if (rounding) age_res else NA_real_,
    skipped_columns = unique(skipped), cov_age = cov_age, cov_types = cov_types, cov_pairs = cov_pairs,
    censored_ids = censored_ids, map = map, conflicts = conflicts, inconsistent_ids = conflict_ids,
    n_inconsistent_rows = n_conflict_rows, inconsistent = map$inconsistent %||% "error"
  )
  list(data = out, meta = meta)
}

# ---------------------------------------------------------------------------
# Individual-level summaries (vectorised)
# ---------------------------------------------------------------------------
individual_metrics <- function(dat) {
  if (!nrow(dat)) return(data.frame())
  ids <- unique(dat$id)
  f <- factor(dat$id, levels = ids)
  okt <- is.finite(dat$trait)
  fo <- factor(dat$id[okt], levels = ids)
  # a column the data frame does not carry (condition, in hand-built frames) gives NA, not an error
  num_mean <- function(v) if (is.null(v) || length(v) != length(f)) rep(NA_real_, length(ids)) else as.numeric(tapply(v, f, finite_mean))
  out <- data.frame(
    id = ids,
    n_rows = tabulate(as.integer(f), nbins = length(ids)),
    n_trait = tabulate(as.integer(fo), nbins = length(ids)),
    first_recorded = as.numeric(tapply(dat$age, f, min)),
    last_recorded = as.numeric(tapply(dat$age, f, max)),
    stringsAsFactors = FALSE
  )
  if (any(okt)) {
    out$first_observed <- as.numeric(tapply(dat$age[okt], fo, min))
    out$last_observed <- as.numeric(tapply(dat$age[okt], fo, max))
    out$mean_age <- as.numeric(tapply(dat$age[okt], fo, mean))
    out$trait_mean <- as.numeric(tapply(dat$trait[okt], fo, mean))
  } else {
    out$first_observed <- NA_real_
    out$last_observed <- NA_real_
    out$mean_age <- NA_real_
    out$trait_mean <- NA_real_
  }
  out$alr <- num_mean(dat$alr)
  out$lifespan <- num_mean(dat$life)
  out$entry <- num_mean(dat$entry)
  out$condition <- num_mean(dat$condition)
  # the same guard for the character summaries: a column the frame does not carry gives NA per individual
  chr_by <- function(v, fun) if (is.null(v) || length(v) != length(f)) rep(NA_character_, length(ids)) else
    as.character(tapply(v, f, fun))
  out$condition_label <- chr_by(dat$condition_label, mode_or_na)
  out$group <- chr_by(dat$group, function(g) {
    g <- g[!is.na(g)]
    if (length(g)) as.character(g[[1]]) else NA_character_
  })
  out
}

id_age_means <- function(dat) {
  d <- dat[is.finite(dat$trait) & is.finite(dat$age), c("id", "age", "trait"), drop = FALSE]
  if (!nrow(d)) return(data.frame(id = character(0), age = numeric(0), trait = numeric(0)))
  z <- stats::aggregate(trait ~ id + age, data = d, FUN = mean)
  z[order(z$id, z$age), , drop = FALSE]
}

# ---------------------------------------------------------------------------
# Ageing-function bases
# ---------------------------------------------------------------------------
# With standardise = TRUE, polynomial bases use z = (age - mean)/sd and powers of z;
# log and exponential bases are centred and scaled. These are linear
# reparameterisations of the raw bases (given an intercept), so likelihoods and AICs
# are unchanged while numerical conditioning improves (cf. the NaN/Hessian problems
# of raw age x ALR interactions in glmmTMB).
make_age_params <- function(age, fun, standardise = TRUE) {
  age <- age[is.finite(age)]
  p <- list(fun = fun, standardise = isTRUE(standardise), mean_age = mean(age), sd_age = stats::sd(age),
            min_age = min(age), all_positive = min(age) > 0)
  if (!is.finite(p$sd_age) || p$sd_age <= 0) p$sd_age <- 1
  lx <- if (p$all_positive) log(age) else log(age - p$min_age + 1)
  p$mean_log <- mean(lx)
  p$sd_log <- stats::sd(lx)
  if (!is.finite(p$sd_log) || p$sd_log <= 0) p$sd_log <- 1
  ex <- exp(-(age - p$mean_age) / p$sd_age)
  p$mean_exp <- mean(ex)
  p$sd_exp <- stats::sd(ex)
  if (!is.finite(p$sd_exp) || p$sd_exp <= 0) p$sd_exp <- 1
  p
}

age_basis <- function(age, p) {
  age <- as.numeric(age)
  z <- (age - p$mean_age) / p$sd_age
  if (p$fun %in% c("Linear", "Quadratic", "Cubic")) {
    x <- if (isTRUE(p$standardise)) z else age
    out <- data.frame(f1 = x)
    if (p$fun %in% c("Quadratic", "Cubic")) out$f2 <- x^2
    if (identical(p$fun, "Cubic")) out$f3 <- x^3
  } else if (identical(p$fun, "Logarithmic")) {
    lx <- if (isTRUE(p$all_positive)) log(pmax(age, 1e-12)) else log(pmax(age - p$min_age + 1, 1e-12))
    out <- data.frame(f1 = if (isTRUE(p$standardise)) (lx - p$mean_log) / p$sd_log else lx)
  } else {
    ex <- exp(-z)
    out <- data.frame(f1 = if (isTRUE(p$standardise)) (ex - p$mean_exp) / p$sd_exp else ex)
  }
  out
}

MODEL_FUNCTIONS <- c(AGE_FUNCTIONS, A3_NONLINEAR)

basis_label <- function(fun) {
  if (identical(fun, A3_NONLINEAR)) return("a\u00b7exp(b\u00b7z_age)")
  switch(fun, Linear = "age", Quadratic = "age + age\u00b2", Cubic = "age + age\u00b2 + age\u00b3",
         Logarithmic = "log(age)", "Asymptotic exponential" = "exp(\u2212z_age)", "age")
}

prepare_model_data <- function(dat, age_function = "Quadratic", covars = character(0), standardise = TRUE,
                               has_group = FALSE, random_terms = character(0), has_group2 = FALSE) {
  d <- dat[is.finite(dat$trait) & is.finite(dat$age), , drop = FALSE]
  if (nrow(d) < 2) return(NULL)
  im <- individual_metrics(dat)
  mp <- match(d$id, im$id)
  afr_i <- ifelse(is.finite(im$entry), im$entry, im$first_recorded)
  raw <- list(ALR = im$alr[mp], LS = im$lifespan[mp], AFR = afr_i[mp])
  first <- !duplicated(d$id)
  pp <- list()
  for (v in names(raw)) {
    x <- raw[[v]][first]
    x <- x[is.finite(x)]
    ctr <- if (isTRUE(standardise) && length(x)) mean(x) else 0
    s <- if (length(x) > 1) stats::sd(x) else NA_real_
    scl <- if (isTRUE(standardise) && is.finite(s) && s > 0) s else 1
    pp[[v]] <- c(centre = ctr, scale = scl)
    d[[paste0(v, "_raw")]] <- raw[[v]]
    d[[v]] <- (raw[[v]] - ctr) / scl
    d[[paste0(v, "2")]] <- d[[v]]^2
    d[[paste0(v, "3")]] <- d[[v]]^3
  }
  ap <- make_age_params(d$age, age_function, standardise)
  b <- age_basis(d$age, ap)
  d <- cbind(d, b)
  for (nm in names(b)) {
    mm <- stats::ave(d[[nm]], d$id, FUN = mean)
    d[[paste0("mean_", nm)]] <- mm
    d[[paste0("delta_", nm)]] <- d[[nm]] - mm
  }
  # Among-individual polynomial of MEAN AGE. mean_f2 is the individual mean of age^2, which is not the square of
  # the individual mean age: mean(age^2) = mean(age)^2 + Var_i(age). The among-individual terms used when
  # "same polynomial order" is chosen are therefore powers of mean_f1, mirroring ALR + ALR^2 for the other proxies.
  if (!is.null(d[["mean_f1"]])) {
    d[["mean_f1_2"]] <- d[["mean_f1"]]^2
    d[["mean_f1_3"]] <- d[["mean_f1"]]^3
  }
  for (cv in covars) {
    if (cv %in% names(d) && !is.numeric(d[[cv]])) d[[cv]] <- factor(d[[cv]])
  }
  for (rt in random_terms) {
    if (rt %in% names(d)) d[[rt]] <- factor(d[[rt]])
  }
  d$id <- factor(d$id)
  if (isTRUE(has_group)) d$group <- factor(d$group)
  if (isTRUE(has_group2)) d$group2 <- factor(d$group2)
  list(data = d, age_params = ap, proxy_params = pp, basis = names(b))
}
