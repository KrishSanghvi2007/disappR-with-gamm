# Stress and break tests of the GAMM engine (step 7).
# Run from the package root:   Rscript tests/scripts/gamm_stress.R
#   1. every bundled example (15) with the suggested family and, where given, the published GAMM settings;
#   2. every out-of-sample dataset in tests/stress_data (as mapped by the stress tier), under several settings;
#   3. deliberately awkward data (constant traits, two ages, one individual, wrong families, huge values, ...).
# Each fit must either succeed or return a plain message: an R error escaping the engine is a failure. Every result
# that fits is pushed through every post-fit function (curve, derivative, peak, checks, residuals, cautions, code).
root <- getwd()
app_dir <- file.path(root, "inst", "app")
if (!dir.exists(app_dir)) stop("Run from the package root.")
setwd(app_dir); on.exit(setwd(root), add = TRUE)
suppressMessages(source("global.R"))
if (!requireNamespace("mgcv", quietly = TRUE)) { cat("mgcv not installed: nothing to test.\n"); quit(status = 0) }
errors <- character(0); checks <- character(0); n_ok <- 0L; n_msg <- 0L
log_row <- function(...) cat(sprintf(...), "\n")

post_fit <- function(label, r, gd) {
  for (fn in c("gamm_tables", "gamm_kcheck", "gamm_concurvity", "gamm_curve", "gamm_derivative", "gamm_peak_onset", "gamm_difference",
               "gamm_vcomp", "gamm_individual_curves", "gamm_quantile_residuals", "gamm_residuals", "gamm_support")) {
    x <- tryCatch(if (fn == "gamm_peak_onset") get(fn)(r, n_draws = 200) else get(fn)(r), error = function(e) e)
    if (inherits(x, "error")) errors <<- c(errors, sprintf("%s | %s: %s", label, fn, conditionMessage(x)))
  }
  cau <- tryCatch(gamm_cautions(r, gd = gd), error = function(e) e)
  if (inherits(cau, "error")) errors <<- c(errors, sprintf("%s | gamm_cautions: %s", label, conditionMessage(cau)))
  dv <- tryCatch(gamm_derivative(r), error = function(e) NULL)
  if (!is.null(dv)) { rd <- tryCatch(gamm_reading(r, dv), error = function(e) e); if (inherits(rd, "error")) errors <<- c(errors, paste(label, "| gamm_reading:", conditionMessage(rd))) }
  code <- tryCatch(paste(gamm_code(r), collapse = "\n"), error = function(e) "")
  if (inherits(tryCatch(parse(text = code), error = function(e) e), "error") || !nzchar(code)) errors <<- c(errors, paste(label, "| R code does not parse"))
  cv <- tryCatch(gamm_curve(r), error = function(e) NULL)
  if (is.data.frame(cv) && any(!is.finite(cv$fit))) checks <<- c(checks, paste(label, "| non-finite values on the population curve"))
}
try_fit <- function(label, gd, s) {
  t0 <- Sys.time()
  r <- tryCatch(gamm_fit(gd, s), error = function(e) e)
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (inherits(r, "error")) { errors <<- c(errors, sprintf("%s | gamm_fit error: %s", label, conditionMessage(r))); log_row("  %-58s ERROR", label); return(NULL) }
  if (!isTRUE(r$ok)) {
    n_msg <<- n_msg + 1L
    if (!is.character(r$message) || !nzchar(r$message)) errors <<- c(errors, paste(label, "| failed without a message"))
    log_row("  %-58s message: %s", label, substr(r$message, 1, 90)); return(NULL)
  }
  n_ok <<- n_ok + 1L
  log_row("  %-58s ok  %5.1fs  %s, %d rec, %d ind, s(age) edf %.2f", label, secs, r$engine, r$n, r$n_ind,
          tryCatch(sum(r$fit$edf[grepl("^s\\(age", names(r$fit$edf)) & !grepl("id", names(r$fit$edf))]), error = function(e) NA_real_))
  post_fit(label, r, gd)
  r
}
comparisons <- function(label, gd, s) {
  for (fn in c("gamm_proxy_comparison", "gamm_parametric_reference", if (nzchar(s$group)) "gamm_group_comparison")) {
    x <- tryCatch(get(fn)(gd, s), error = function(e) e)
    if (inherits(x, "error")) errors <<- c(errors, sprintf("%s | %s: %s", label, fn, conditionMessage(x)))
    else if (is.list(x) && !is.data.frame(x) && isFALSE(x$ok) && !nzchar(x$message %||% "")) errors <<- c(errors, paste(label, "|", fn, "failed without a message"))
  }
}

# ---- 1. bundled examples ------------------------------------------------------------------------------------------
cat("== 1. Bundled examples (published family; published GAMM settings where the example gives them)\n")
for (key in names(EXAMPLES)) {
  ex <- EXAMPLES[[key]]
  d <- load_example_file(ex$file)
  b <- tryCatch(standardise_data(d, ex$mapping), error = function(e) e)
  if (inherits(b, "error")) { errors <- c(errors, paste(key, "| prepare:", conditionMessage(b))); next }
  if (!is.null(ex$subset)) { keep <- as.character(d[[ex$subset$var]]) %in% ex$subset$levels; b <- standardise_data(d[keep, , drop = FALSE], ex$mapping) }
  gd <- gamm_data(b$data, b$meta)
  fam <- gamm_suggest_family(ex$family %||% tryCatch(data_integrity(b$data, b$meta)$family$family, error = function(e) "gaussian"))   # as the app does
  s <- utils::modifyList(gamm_defaults(), c(list(family = fam, cov_pairs = b$meta$cov_pairs, cov_age = b$meta$cov_age), ex$gamm %||% list()))
  if (isTRUE(b$meta$has_trials) && fam == "binomial") s$family <- "binomial"
  r <- try_fit(sprintf("%s [%s]", key, fam), gd, s)
  if (!is.null(r) && key %in% c("brown_haematocrit", "szejnersigal_activity")) comparisons(key, gd, s)
}

# ---- 2. out-of-sample stress datasets -----------------------------------------------------------------------------
cat("\n== 2. Out-of-sample datasets (tests/stress_data)\n")
ex <- parse(file.path(root, "tests", "scripts", "stress_test.R"), keep.source = FALSE)
for (e in ex) if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% c("mk_map", "deg", "specs")) eval(e)
fam_map <- c(gaussian = "gaussian", poisson = "poisson", nbinom2 = "nb", zinb = "nb")
for (ds in names(specs)) {
  sp <- specs[[ds]]
  if (isTRUE(sp$expect_unreadable) || isTRUE(sp$expect_stop)) next
  rd <- tryCatch(read_user_csv(file.path(root, "tests", "stress_data", sp$file), sp$file), error = function(e) NULL)
  d <- if (is.data.frame(rd)) rd else rd$data
  if (!is.data.frame(d)) { log_row("  %-58s not readable (as expected by the stress tier?)", ds); next }
  b <- tryCatch(standardise_data(d, sp$map), error = function(e) e)
  if (inherits(b, "error")) { log_row("  %-58s data refused at step 1: %s", ds, substr(conditionMessage(b), 1, 70)); next }
  gd <- gamm_data(b$data, b$meta)
  fam <- fam_map[[sp$family %||% "gaussian"]] %||% "gaussian"
  base <- utils::modifyList(gamm_defaults(), list(family = fam))
  grp <- if (!is.null(sp$facet)) { g <- gamm_group_choices(b$meta, gd); if (length(g)) g[[1]] else "" } else ""
  variants <- list(default = list(), ti = list(proxy_form = "ti"), fs = list(individual = "fs"),
                   LS = list(proxy = "LS"), terminal = list(terminal = TRUE), bam = list(engine = "bam"))
  if (nzchar(grp)) variants$groups <- list(group = grp, structure = "GS")
  if (nlevels(gd$id) > 300) variants$fs <- NULL          # a random smooth per individual takes minutes with many individuals
  for (v in names(variants)) try_fit(sprintf("%s / %s [%s]", ds, v, fam), gd, utils::modifyList(base, variants[[v]]))
  if (ds %in% c("few_individuals", "covariate_edges", "grouping_edges", "count_edges")) comparisons(ds, gd, utils::modifyList(base, list(group = grp)))
}

# ---- 3. awkward data ----------------------------------------------------------------------------------------------
cat("\n== 3. Awkward data (each must fit or return a message)\n")
set.seed(11)
mk <- function(n_id = 40, ages = 1:8, f = function(a) sin(a / 2), sd = 0.3, ...) {
  d <- do.call(rbind, lapply(seq_len(n_id), function(i) { a <- ages[seq_len(sample(2:length(ages), 1))]; data.frame(id = i, age = a, trait = f(a) + rnorm(1, 0, .3) + rnorm(length(a), 0, sd)) }))
  extra <- list(...); for (nm in names(extra)) d[[nm]] <- extra[[nm]](d)
  d
}
prep <- function(d, ...) { b <- standardise_data(d, mk_map(id = "id", age = "age", trait = "trait", ...)); gamm_data(b$data, b$meta) }
awkward <- list(
  constant_trait = list(d = transform(mk(), trait = 5)),
  two_ages = list(d = mk(ages = 1:2)),
  three_ages = list(d = mk(ages = 1:3)),
  one_individual = list(d = data.frame(id = 1, age = 1:30, trait = rnorm(30))),
  huge_values = list(d = transform(mk(), trait = trait * 1e9)),
  tiny_values = list(d = transform(mk(), trait = trait * 1e-9)),
  negative_for_gamma = list(d = mk(), s = list(family = "Gamma")),
  non_integer_for_poisson = list(d = transform(mk(), trait = abs(trait)), s = list(family = "poisson")),
  all_zero_counts = list(d = transform(mk(), trait = 0), s = list(family = "poisson")),
  binary_all_ones = list(d = transform(mk(), trait = 1), s = list(family = "binomial")),
  binary = list(d = transform(mk(), trait = as.numeric(trait > 0)), s = list(family = "binomial")),
  proportion_without_trials = list(d = transform(mk(), trait = plogis(trait)), s = list(family = "binomial")),
  beta_with_bounds = list(d = transform(mk(), trait = pmin(1, pmax(0, plogis(trait * 3)))), s = list(family = "betar")),
  beta_inside = list(d = transform(mk(), trait = plogis(trait)), s = list(family = "betar")),
  k_too_large = list(d = mk(), s = list(k = 40)),
  k_minimum = list(d = mk(), s = list(k = 3)),
  continuous_ages = list(d = { x <- mk(); x$age <- x$age + runif(nrow(x)); x }),
  gaulss = list(d = mk(sd = 0.5), s = list(family = "gaulss")),
  shrinkage_select = list(d = mk(), s = list(select = TRUE, bs = "ts")),
  adaptive_basis = list(d = mk(n_id = 80, ages = 1:15), s = list(bs = "ad")),
  cr_basis_fs = list(d = mk(), s = list(bs = "cr", individual = "fs")),
  ar1_non_gaussian = list(d = transform(mk(), trait = rpois(length(trait), exp(trait))), s = list(family = "poisson", ar1 = TRUE)),
  no_individual_effect = list(d = mk(), s = list(individual = "none")),
  afr_all_same = list(d = mk(), s = list(afr_form = "ti")),
  ti_constant_alr = list(d = { x <- mk(); x$alr <- 8; x }, map = list(alr = "alr"), s = list(proxy_form = "ti")),
  group_one_level = list(d = { x <- mk(); x$sex <- "F"; x }, map = list(covars = "sex", cov_factor = "sex"), s = list(group = "cv_sex", structure = "GS")),
  group_tiny_level = list(d = { x <- mk(n_id = 60); x$sex <- ifelse(x$id <= 2, "M", "F"); x }, map = list(covars = "sex", cov_factor = "sex"), s = list(group = "cv_sex", structure = "I")),
  group_missing_values = list(d = { x <- mk(n_id = 60); x$sex <- ifelse(x$id %% 2 == 0, "F", "M"); x$sex[x$id %% 7 == 0] <- NA; x }, map = list(covars = "sex", cov_factor = "sex"), s = list(group = "cv_sex", structure = "S")),
  all_na_lifespan = list(d = { x <- mk(); x$ls <- NA; x }, map = list(life = "ls"), s = list(proxy = "LS")),
  many_individuals = list(d = mk(n_id = 1500, ages = 1:6)),
  nonexistent_group = list(d = mk(), s = list(group = "cv_nothing", structure = "GS"))
)
for (nm in names(awkward)) {
  a <- awkward[[nm]]
  gd <- tryCatch(do.call(prep, c(list(a$d), a$map %||% list())), error = function(e) e)
  if (inherits(gd, "error")) { log_row("  %-58s refused at step 1: %s", nm, substr(conditionMessage(gd), 1, 70)); next }
  r <- try_fit(nm, gd, utils::modifyList(gamm_defaults(), a$s %||% list()))
}
expect_msg <- c("constant_trait", "two_ages", "three_ages", "negative_for_gamma", "non_integer_for_poisson", "all_zero_counts", "binary_all_ones",
                "proportion_without_trials", "beta_with_bounds", "all_na_lifespan")
cat(sprintf("\n%d fits succeeded, %d returned a message.\n", n_ok, n_msg))
if (length(checks)) cat("\nCHECK:\n", paste("-", checks, collapse = "\n"), "\n")
if (length(errors)) { cat("\nERRORS:\n", paste("-", errors, collapse = "\n"), "\n"); quit(status = 1) }
cat("No errors escaped the GAMM engine.\n")
