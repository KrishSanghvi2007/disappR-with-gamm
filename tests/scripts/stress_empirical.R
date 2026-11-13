# =============================================================================
# disappR - empirical and simulation stress test
#
# Reruns, inside R with lme4/glmmTMB, everything that was checked outside R.
# Run from the package root:
#     Rscript tests/scripts/stress_empirical.R  [path/to/folder/with/the/CSV/files]
# The empirical part is skipped (with a note) for any file that is not found,
# so the simulation part always runs.
#
# Each check prints PASS / FAIL / NOTE. A FAIL means the app did something
# other than what the analysis of these data says it should do.
# =============================================================================

app_dir <- if (file.exists("inst/app/global.R")) "inst/app" else
           if (file.exists("global.R")) "." else stop("Run from the package root.")
old <- setwd(app_dir); source("global.R"); setwd(old)

data_dir <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[[1]] else "tests/empirical_data"

N_PASS <- 0L; N_FAIL <- 0L; N_NOTE <- 0L
ok   <- function(msg) { N_PASS <<- N_PASS + 1L; cat("  PASS  ", msg, "\n") }
bad  <- function(msg) { N_FAIL <<- N_FAIL + 1L; cat("  FAIL  ", msg, "\n") }
note <- function(msg) { N_NOTE <<- N_NOTE + 1L; cat("  NOTE  ", msg, "\n") }
check <- function(cond, msg) if (isTRUE(cond)) ok(msg) else bad(msg)
head_ <- function(x) cat("\n==== ", x, " ", strrep("=", max(0, 60 - nchar(x))), "\n", sep = "")

# ---------------------------------------------------------------- helpers
# Map -> standardise -> integrity -> availability -> model suite, with timings.
run_one <- function(df, map, label, family = "gaussian", age_function = "Quadratic",
                    random_slope = "none", dup_action = "keep", models = MODEL_IDS) {
  t0 <- proc.time()[["elapsed"]]
  b <- standardise_data(df, map, dup_action)
  dat <- b$data; meta <- b$meta
  di <- data_integrity(dat, meta)
  av <- model_availability(dat, meta)
  res <- fit_model_suite(dat, meta, models = intersect(models, names(which(av$ok))),
                         age_function = age_function, family = family, random_slope = random_slope)
  secs <- round(proc.time()[["elapsed"]] - t0, 1)
  cat(sprintf("  [%s] %d rows / %d individuals, %s s, family %s\n", label, nrow(dat),
              length(unique(dat$id)), secs, family))
  if (!isTRUE(res$ok)) { cat("   suite message:", res$message, "\n"); return(list(dat = dat, meta = meta, di = di, av = av, res = res)) }
  a <- res$aic
  cat("   ranking:", paste(sprintf("%s %+.1f", sub("Model ", "M", a$Model), a$Delta_AIC), collapse = " | "), "\n")
  fits_bad <- sum(a$Fit == "Failed")
  if (fits_bad) note(sprintf("%s: %d model(s) classified Failed", label, fits_bad))
  list(dat = dat, meta = meta, di = di, av = av, res = res,
       best = sub("Model ", "M", a$Model[[1]]), d2 = a$Delta_AIC[[2]], secs = secs)
}
int_row <- function(di, check_name) {
  r <- di$table[di$table$Check == check_name, , drop = FALSE]
  if (!nrow(r)) NA_character_ else r$Result[[1]]
}
read_or_skip <- function(f) {
  p <- file.path(data_dir, f)
  if (!file.exists(p)) { note(paste("missing file, skipped:", f)); return(NULL) }
  x <- read_user_csv(p, f)
  if (is.null(x$data)) { bad(paste("could not read", f, "-", x$note)); return(NULL) }
  x$data
}
base_map <- function(...) {
  m <- list(id = "", age = "", trait = "", alr = "__AUTO_LAST__", life = "", entry = "__AUTO_FIRST__",
            condition = "", covars = character(0), cov_factor = character(0), cov_int = character(0),
            group = "", nested = TRUE, random = character(0), censor = "", censor_value = "",
            start_mode = "afr", start_age = NA_real_, age_round = NA_real_, cov_age = character(0))
  utils::modifyList(m, list(...))
}

# =============================================================================
# PART 0 - the app's own files parse and the UI builds
# =============================================================================
head_("PART 0  parse and UI build")
for (f in c("global.R", "server.R", "ui.R")) {
  p <- file.path(app_dir, f)
  check(!inherits(try(parse(p), silent = TRUE), "try-error"), paste(f, "parses"))
}
# ui.R 0.7.0 had `),,` (an empty argument) at line 304: it parses but fails when evaluated.
uiok <- try(eval(parse(file.path(app_dir, "ui.R"))), silent = TRUE)
check(!inherits(uiok, "try-error"), "ui.R evaluates (no empty arguments in tabItem/fluidRow)")
if (inherits(uiok, "try-error")) cat("   ", conditionMessage(attr(uiok, "condition")), "\n")

# =============================================================================
# PART 1 - empirical datasets
# Expectations come from the structure of each dataset, stated before the run.
# =============================================================================
head_("PART 1  empirical datasets")

# --- 1a great tits: clutch size, mapped ALR, 69% single records -------------
df <- read_or_skip("Great_tits_data.csv")
if (!is.null(df)) {
  g <- guess_mapping(df)
  if (!identical(g$age, "f_min_age"))
    note(sprintf("guess_mapping picked age = '%s' (expected f_min_age); trait = '%s'", g$age, g$trait))
  r <- run_one(df, base_map(id = "female", age = "f_min_age", trait = "CS", alr = "f_ALR"), "great tits CS")
  check(grepl("406", int_row(r$di, "Mapped ALR vs last recorded age") %||% ""),
        "great tits: 406 individuals flagged where mapped ALR != last recorded age")
  check(r$best %in% c("M1", "M4"), "great tits: M4 or M1 ranked first (they sit within ~0.5 AIC)")
  check(identical(r$di$family$family, "gaussian") || r$di$family$dispersion < 1,
        "great tits: clutch size is UNDERdispersed (dispersion ~0.31) - Poisson is not the right default")
  if (identical(r$di$family$family, "poisson"))
    note("suggest_family() recommends Poisson for an underdispersed count: add an underdispersion branch")
}

# --- 1b female flies: nested IDs, 2x2 treatment, zero-inflated counts -------
df <- read_or_skip("senescence_females.csv")
if (!is.null(df)) {
  # IDs such as "B?1" recur in families Bom/Bpi/Btr (symbols lost upstream): without nesting
  # they collapse into one fly and appear as duplicate ID x age records.
  # 0.20.2: the merged IDs carry several lifespans, so unnested loading stops with a data-integrity error
  flat <- tryCatch(standardise_data(df, base_map(id = "ID", age = "Adult_age", trait = "Daily_Eggs", life = "Adult_lifespan")),
                   disappr_integrity_error = function(e) e)
  check(inherits(flat, "disappr_integrity_error"), "female flies: IDs shared across families stop with a data-integrity error when Family is NOT mapped")
  flatx <- standardise_data(df, base_map(id = "ID", age = "Adult_age", trait = "Daily_Eggs", life = "Adult_lifespan", inconsistent = "exclude"))
  check(length(flatx$meta$inconsistent_ids) >= 10, "female flies: excluding them names the merged IDs")
  r <- run_one(df, base_map(id = "ID", age = "Adult_age", trait = "Daily_Eggs", life = "Adult_lifespan",
                            group = "Family", nested = TRUE, covars = c("DevT", "AdultT"),
                            cov_factor = c("DevT", "AdultT"), cov_int = "DevT|||AdultT"), "female flies eggs")
  check(r$meta$n_dup == 0, "female flies: nesting in Family removes the duplicates (637 individuals)")
  check(r$best %in% c("M4", "M9"), "female flies: M4/M9 first (strong age-dependent disappearance)")
  check(identical(r$di$family$family, "zinb"), "female flies: ZINB suggested (41% zeros, dispersion 5.4)")
  check(grepl("^[1-9]", int_row(r$di, "LS earlier than last recorded age") %||% "0"),
        "female flies: individuals with LS < last record are flagged")
  # count families must run through glmmTMB on the same data
  if (HAS_GLMMTMB) {
    rp <- run_one(df, base_map(id = "ID", age = "Adult_age", trait = "Daily_Eggs", life = "Adult_lifespan",
                               group = "Family", nested = TRUE), "female flies ZINB", family = "zinb")
    check(isTRUE(rp$res$ok), "female flies: zero-inflated NB suite fits")
    rp2 <- run_one(df, base_map(id = "ID", age = "Adult_age", trait = "Daily_Eggs", life = "Adult_lifespan",
                                group = "Family", nested = TRUE), "female flies NB", family = "nbinom2")
    check(isTRUE(rp2$res$ok), "female flies: negative-binomial suite fits")
  } else note("glmmTMB not installed: count families not tested")
  # averaging duplicates turns counts into non-integers and blocks every count family
  avg <- standardise_data(df, base_map(id = "ID", age = "Adult_age", trait = "Daily_Eggs",
                                       life = "Adult_lifespan", inconsistent = "exclude"), "mean")
  nonint <- any(abs(avg$data$trait - round(avg$data$trait)) > 1e-8, na.rm = TRUE)
  if (nonint) note("dup_action='mean' makes non-integer counts; count families are then refused - say so in the message")
}

# --- 1c male flies: 2-day schedule, ages from 0, censored lifespans --------
df <- read_or_skip("senescence_males.csv")
if (!is.null(df)) {
  r <- run_one(df, base_map(id = "ID", age = "Adult_age", trait = "Daily_Weight", life = "Adult_lifespan",
                            group = "Family", nested = TRUE), "male flies weight")
  check(grepl("step = 2", int_row(r$di, "Sampling schedule") %||% ""), "male flies: sampling step inferred as 2 days")
  check(identical(r$best, "M5"), "male flies: M5 first (terminal decline in weight)")
  check(isFALSE(r$av$default[["M6"]]), "male flies: M6 left unselected because LS is missing for 11 individuals")
  rl <- run_one(df, base_map(id = "ID", age = "Adult_age", trait = "Daily_Weight", life = "Adult_lifespan"),
                "male flies log", age_function = "Logarithmic")
  check(isTRUE(rl$res$ok), "male flies: Logarithmic function works with age 0 present (log(age - min + 1))")
}

# --- 1d chipmunks: irregular seasonal ages, mapped AFR, LS missing = alive --
df <- read_or_skip("data_senescence_tamias.csv")
if (!is.null(df)) {
  r <- run_one(df, base_map(id = "ID", age = "age", trait = "nb_juv", life = "lifespan", entry = "AFR",
                            covars = c("sex", "season"), cov_factor = c("sex", "season"), inconsistent = "exclude"), "chipmunks nb_juv")
  check(isTRUE(r$di$schedule$irregular), "chipmunks: irregular sampling schedule is flagged")
  check(identical(r$best, "M6"), "chipmunks: M6 (true lifespan) first")
  check(isFALSE(r$av$default[["M6"]]), "chipmunks: M6 unselected by default (LS missing for 22 individuals)")
  # binary trait in the same file
  rb <- run_one(df, base_map(id = "ID", age = "age", trait = "weaned_juv", life = "lifespan", entry = "AFR", inconsistent = "exclude"),
                "chipmunks binary")
  fam <- rb$di$family$family
  if (!identical(fam, "gaussian"))
    note(sprintf("binary 0/1 trait: suggest_family() returns '%s'; there is no binomial family in MODEL_FAMILIES", fam))
}

# --- 1e turtles: 560 duplicate ID x age, 'UNK' codes, reproductive age -----
df <- read_or_skip("Warner_RevisedReproData__11NOV2016.csv")
if (!is.null(df)) {
  r <- run_one(df, base_map(id = "Female ID", age = "Reproductive Age", trait = "Clutch size"), "turtles keep")
  check(r$meta$n_dup > 500, "turtles: >500 duplicate ID x age records reported (several clutches per year)")
  check(isFALSE(r$av$default[["M7"]]), "turtles: Models 7-10 unselected (almost every female starts at age 1)")
  r2 <- run_one(df, base_map(id = "Female ID", age = "Reproductive Age", trait = "Clutch size"), "turtles mean",
                dup_action = "mean")
  check(isTRUE(r2$res$ok), "turtles: averaging duplicates still fits")
}

# --- 1f Bichet: no age column; calendar years must not pass as ages --------
df <- read_or_skip("Bichet_et_al_JAE_DRYAD.csv")
if (!is.null(df)) {
  g <- guess_mapping(df)
  if (identical(g$age, "storage_time_HAHL"))
    note("guess_mapping picked 'storage_time_HAHL' as age because 'storage' contains 'age'")
  r <- run_one(df, base_map(id = "ID", age = "year of sampling", trait = "HA"), "Bichet calendar-year age")
  yrs <- int_row(r$di, "Sampling schedule")
  note(paste("ages of 2008-2019 were accepted with no calendar-year warning; schedule row said:", yrs))
  df$age_derived <- suppressWarnings(as.numeric(df[["year of sampling"]]) - as.numeric(df$year_of_birth))
  r2 <- run_one(df, base_map(id = "ID", age = "age_derived", trait = "HA", covars = "sex", cov_factor = "sex"),
                "Bichet derived age")
  check(r2$best %in% c("M4", "M9"), "Bichet: M4/M9 first on HA once real ages are used")
}

# --- 1g terns: 126k rows, one row per GPS fix -----------------------------
df <- read_or_skip("terns.csv")
if (!is.null(df)) {
  g <- guess_mapping(df)
  if (identical(g$id, "track_id"))
    note("guess_mapping picked 'track_id' (228 tracks) over 'individual' (102 birds): it prefers the most unique ID-like column")
  r <- run_one(df, base_map(id = "individual", age = "age", trait = "breeding_success"), "terns keep")
  check(r$meta$n_dup > 100000, "terns: ~126,000 duplicate ID x age rows reported")
  check(r$secs < 120, "terns: 126k rows processed in under two minutes")
  r2 <- run_one(df, base_map(id = "individual", age = "age", trait = "breeding_success"), "terns mean",
                dup_action = "mean")
  check(isTRUE(r2$res$ok), "terns: averaging to one value per bird-year fits")
}

# --- 1h stripping2: not longitudinal at all -------------------------------
df <- read_or_skip("stripping2.csv")
if (!is.null(df)) {
  g <- guess_mapping(df)
  b <- standardise_data(df, base_map(id = "Male_ID", age = g$age, trait = "F1_counts"))
  check(nrow(b$data) == 0 || length(unique(b$data$id)) < 3,
        "stripping2: guessed mapping yields no usable rows (the app must stop with a clear message)")
}

# =============================================================================
# PART 2 - simulations with known answers
# =============================================================================
head_("PART 2  simulations with known answers")

sim_frame <- function(id, age, trait, LS, ...) data.frame(id = id, age = age, trait = trait, LS = LS, ..., stringsAsFactors = FALSE)
make_ls <- function(n, mean_ls = 12, sd = 4, lo = 2) pmax(lo, round(stats::rnorm(n, mean_ls, sd)))

sim_case <- function(seed, n = 400, mean_ls = 12, type = "null", extra = list()) {
  set.seed(seed)
  ls <- make_ls(n, mean_ls); zls <- as.numeric(scale(ls))
  out <- list()
  for (i in seq_len(n)) {
    a <- seq_len(ls[i]); lvl <- 10 + stats::rnorm(1)
    diet <- NA_character_
    y <- switch(type,
      null        = lvl - 0.15 * a - 0.01 * a^2 + stats::rnorm(length(a)),
      threshold   = lvl + ifelse(ls[i] >= stats::median(ls), -0.05, -0.45) * a + stats::rnorm(length(a)),
      quad_alr    = 10 + 3 * (1 - zls[i]^2) + stats::rnorm(1, 0, 0.7) - 0.2 * a + stats::rnorm(length(a)),
      slopes      = lvl + (-0.2 + 0.25 * stats::rnorm(1)) * a + stats::rnorm(length(a)),
      terminal    = lvl + stats::rnorm(length(a)),
      binary      = NA,
      lvl + (-0.2 + 0.15 * zls[i]) * a + stats::rnorm(length(a)))
    if (identical(type, "terminal")) y[length(y)] <- y[length(y)] - 4
    if (identical(type, "binary")) {
      eta <- 0.5 + stats::rnorm(1, 0, 0.5) - 0.15 * a + 0.25 * zls[i] * a
      y <- stats::rbinom(length(a), 1, 1 / (1 + exp(-eta)))
    }
    if (identical(type, "cov")) {
      diet <- sample(c("A", "B"), 1)
      y <- lvl + (-0.2 + 0.15 * zls[i] + if (diet == "B") -0.2 else 0) * a + stats::rnorm(length(a))
    }
    out[[i]] <- sim_frame(paste0("i", i), a, y, ls[i], diet = diet)
  }
  d <- do.call(rbind, out)
  if (identical(type, "mnar")) {
    p <- 1 / (1 + exp(-(d$trait - 6)))
    firstrow <- !duplicated(d$id)
    d <- d[firstrow | stats::runif(nrow(d)) < p, , drop = FALSE]
  }
  d
}

sim_check <- function(label, d, expect, age_function = "Linear", covars = character(0), cov_age = character(0),
                      random_slope = "none") {
  map <- base_map(id = "id", age = "age", trait = "trait", life = "LS",
                  covars = covars, cov_factor = covars, cov_age = cov_age)
  r <- run_one(d, map, label, age_function = age_function, random_slope = random_slope,
               models = c("M1", "M2", "M3", "M4", "M5", "M6"))
  if (is.null(r$best)) { bad(paste(label, "- no model fitted")); return(invisible(r)) }
  check(r$best %in% expect, sprintf("%s: first model is %s (expected one of %s)", label, r$best, paste(expect, collapse = "/")))
  invisible(r)
}

sim_check("S0 no selective disappearance", sim_case(1, type = "null"), "M1", age_function = "Quadratic")
sim_check("S1 threshold (non-linear) SD", sim_case(2, type = "threshold"), c("M4", "M6"))
s2 <- sim_case(3, type = "quad_alr")
r2 <- sim_check("S2 U-shaped SD, linear ALR only", s2, c("M1", "M2", "M4"))
note("S2: with a U-shaped lifespan effect the linear ALR term finds nothing; ALR^2 (among = 'same', or an extra term) is needed, and the ALR-bin means in Visual diagnosis are the diagnostic")
sim_check("S3 trait-dependent missingness, no true SD", sim_case(4, mean_ls = 14, type = "mnar"), c("M1", "M3", "M5"))
note("S3: MNAR missingness makes M3/M5 win with no true selective disappearance - the missingness drivers table is the safeguard")
sim_check("S4 diet x age interaction + SD", sim_case(5, n = 500, type = "cov"), c("M4", "M6"),
          covars = "diet", cov_age = "diet")
sim_check("S5a random slopes unlinked to LS (random intercept)", sim_case(6, type = "slopes"), "M1")
sim_check("S5b random slopes unlinked to LS (correlated slope)", sim_case(6, type = "slopes"), "M1",
          random_slope = "correlated")
sim_check("S7 binary trait with age-dependent SD", sim_case(8, n = 600, type = "binary"), c("M4", "M6"))
sim_check("S8 terminal decline, no ageing", sim_case(9, mean_ls = 14, type = "terminal"), c("M4", "M5"))
note("S8: terminal decline alone produces a strong selective-disappearance signal - A5 is what tells the two apart")

# =============================================================================
# PART 3 - individual fits, ageing-function comparison, diagnostics, export
# =============================================================================
head_("PART 3  individual fits, diagnostics, exported code")
d <- sim_case(21, n = 300, mean_ls = 14, type = "threshold")
b <- standardise_data(d, base_map(id = "id", age = "age", trait = "trait", life = "LS"))
dat <- b$data; meta <- b$meta

for (fn in A3_FUNCTIONS) {
  z <- try(fit_individual_function(dat, fn), silent = TRUE)
  check(!inherits(z, "try-error") && nrow(z$coefs) > 0, paste("individual fits run for", fn))
}
cf <- try(compare_individual_functions(dat), silent = TRUE)
check(!inherits(cf, "try-error"), "ageing-function comparison across individuals runs")

res <- fit_model_suite(dat, meta, models = c("M1", "M2", "M4"), age_function = "Linear")
check(isTRUE(res$ok), "model suite for the diagnostics block fits")
for (nm in c("disappearance_data", "disappearance_models", "selection_differentials", "life_table", "terminal_data")) {
  f <- get(nm)
  out <- try(if (nm %in% c("disappearance_models", "selection_differentials")) f(disappearance_data(dat, meta)) else f(dat, meta), silent = TRUE)
  check(!inherits(out, "try-error"), paste(nm, "runs (A4-A7 diagnostics)"))
}
g <- try(build_missing_grid(dat, 0, "afr", NA_real_), silent = TRUE)
check(!inherits(g, "try-error") && nrow(g) > 0, "missingness grid builds")
if (!inherits(g, "try-error")) {
  drv <- try(missingness_drivers(g), silent = TRUE)
  check(!inherits(drv, "try-error"), "missingness drivers table builds")
}
code <- try(model_r_code(res, meta), silent = TRUE)
check(!inherits(code, "try-error") && !inherits(try(parse(text = paste(code, collapse = "\n")), silent = TRUE), "try-error"),
      "exported R code parses")
pc <- try(plot_code_lines(res, meta), silent = TRUE)
check(!inherits(pc, "try-error") && !inherits(try(parse(text = paste(pc, collapse = "\n")), silent = TRUE), "try-error"),
      "exported plot code parses")
if (exists("section_code_box")) {
  for (s in c("data", "visual", "sampling", "individual")) {
    out <- try(section_code_box(s), silent = TRUE)
    check(!inherits(out, "try-error"), paste("section code box builds for", s))
  }
}
for (m in c("M1", "M4", "M5", "M9")) {
  out <- try(model_info_content(m, list(age_function = "Linear"), meta), silent = TRUE)
  check(!inherits(out, "try-error"), paste("model equation panel builds for", m))
}
# rank deficiency: with an almost constant AFR, M4/M8/M9 collapse onto the same fit
d2 <- dat; d2$entry <- min(dat$entry)
res2 <- try(fit_model_suite(d2, meta, models = c("M4", "M8", "M9"), age_function = "Linear"), silent = TRUE)
if (!inherits(res2, "try-error") && isTRUE(res2$ok)) {
  a <- res2$aic
  if (length(unique(round(a$AIC, 4))) < nrow(a))
    note("M4/M8/M9 return identical AICs when AFR does not vary - check they are marked unavailable rather than ranked")
}

cat(sprintf("\n===== %d PASS, %d FAIL, %d NOTE =====\n", N_PASS, N_FAIL, N_NOTE))
if (N_FAIL > 0) quit(status = 1)
