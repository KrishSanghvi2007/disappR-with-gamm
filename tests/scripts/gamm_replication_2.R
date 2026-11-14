# Replication of two more published GAMM analyses with the app's GAMM engine (step 7): superb fairy-wren reproduction
# and survival (Cooper et al. 2021) and Asian elephant body mass by sex (Lalande et al. 2022).
# Run from the package root:   Rscript tests/scripts/gamm_replication_2.R  [> inst/validation/gamm/replication_results_2.txt]
# (a) the authors' specification fitted directly (gamm4 / mgcv) on the bundled data; (b) the same analysis through
# disappR's engine, as a user would run it with the example's settings. Fails (status 1) only if a fit errors.
suppressPackageStartupMessages({ library(mgcv) })
if (!file.exists("DESCRIPTION")) stop("Run from the package root.")
for (f in c("utils.R", "provenance.R", "simulation.R", "import.R", "validation.R", "data-preparation.R", "formulas.R", "model-fit.R",
            "model-comparison.R", "diagnostics.R", "trajectories.R", "disappearance.R", "missingness.R", "interpretation.R",
            "effects.R", "evidence.R", "ui-helpers.R", "export.R", "gamm.R")) suppressMessages(source(file.path("R", f), local = TRUE))
errors <- character(0)
safe <- function(label, expr) tryCatch(expr, error = function(e) { errors <<- c(errors, paste(label, conditionMessage(e))); NULL })
line <- function(...) cat(..., "\n", sep = "")
sm_row <- function(fit, lab) { st <- summary(fit)$s.table; i <- grep(lab, rownames(st), fixed = TRUE)[1]; st[i, , drop = FALSE] }
show <- function(label, fit, lab) { x <- sm_row(fit, lab); line(sprintf("  %-58s edf %6.3f   stat %8.2f   p %s", label, x[1, 1], x[1, 3], format_p(x[1, 4]))) }
example_data <- function(id) {
  ex <- EXAMPLES[[id]]
  d <- utils::read.csv(file.path("inst", "app", "data", ex$file), stringsAsFactors = FALSE)
  list(ex = ex, d = d, b = standardise_data(d, ex$mapping))
}
app_fit <- function(label, gd, s) {
  r <- safe(label, gamm_fit(gd, utils::modifyList(gamm_defaults(), s)))
  if (!isTRUE(r$ok)) { if (!is.null(r)) errors <<- c(errors, paste(label, r$message)); return(NULL) }
  line(sprintf("(b) disappR engine, %s: %d records, %d individuals, %s", label, r$n, r$n_ind, r$engine))
  line("  formula: ", r$formula)
  show("s(age)", r$fit, "s(age)")
  pr <- gamm_periods(gamm_derivative(r), simultaneous = TRUE)
  line("  significant change (simultaneous 95%): ", if (nrow(pr)) paste(sprintf("%s%s %s-%s", ifelse(pr$Group == "All", "", paste0(pr$Group, ": ")), pr$Change,
                                                                                 format_num(pr$From, 3), format_num(pr$To, 3)), collapse = "; ") else "none")
  po <- gamm_peak_onset(r, n_draws = 500)
  line("  peak: ", paste(sprintf("%s %s [%s]", po$Group, po[["Peak age"]], po[["Peak 95% CI"]]), collapse = "; "))
  invisible(r)
}

# =====================================================================================================================
line("==== 3. Cooper et al. 2021, American Naturalist: superb fairy-wren ====")
line("Published (Table 1, age smooth edf; ** P < .01): female survival 1.000**; female independent offspring 4.540**;")
line("male total offspring 3.567**; male extra-group 3.731**; male within-group 2.019 (n.s.). Poisson GAMMs (gamm4) with")
line("individual, year and observation-level random effects; longevity linear; final breeding season; male dominance.")
fw <- example_data("cooper_wren_females"); fm <- example_data("cooper_wren_males")
f <- fw$d; f$IDf <- factor(f$ID); f$yr <- factor(f$year); f$fb <- factor(f$final_breed)
fr <- f[!is.na(f$offspring_inde), ]; fr$obs <- factor(seq_len(nrow(fr)))
g <- safe("wren female offspring (authors)", gamm4::gamm4(offspring_inde ~ s(age, k = 8) + long_years + fb, random = ~ (1 | IDf) + (1 | yr) + (1 | obs),
                                                          family = poisson, data = fr))
if (!is.null(g)) { line(sprintf("(a) authors' specification, female independent offspring (gamm4): %d records", nrow(fr))); show("s(age)", g$gam, "s(age)") }
g <- safe("wren female survival (authors)", gamm4::gamm4(survived ~ s(age, k = 8), random = ~ (1 | IDf) + (1 | yr), family = binomial, data = f))
if (!is.null(g)) { line(sprintf("(a) authors' specification, female survival (gamm4): %d records", nrow(f))); show("s(age)", g$gam, "s(age)") }
m <- fm$d; m$IDf <- factor(m$ID); m$yr <- factor(m$year); m$fb <- factor(m$final_breed); m$domf <- factor(m$dom); m$obs <- factor(seq_len(nrow(m)))
for (tr in c("offspring", "offspring_EG", "offspring_WG")) {
  g <- safe(paste("wren male", tr), gamm4::gamm4(stats::as.formula(paste(tr, "~ s(age, k = 10) + long_years + fb + domf")),
                                                 random = ~ (1 | IDf) + (1 | yr) + (1 | obs), family = poisson, data = m))
  if (!is.null(g)) { line(sprintf("(a) authors' specification, male %s (gamm4): %d records", tr, nrow(m))); show("s(age)", g$gam, "s(age)") }
}
gd <- gamm_data(fw$b$data, fw$b$meta)
app_fit("female independent offspring (Poisson, longevity linear)", gd,
        list(family = "poisson", k = 8, proxy = "LS", proxy_form = "linear", cov_pairs = fw$b$meta$cov_pairs))
bs <- standardise_data(fw$d, utils::modifyList(fw$ex$mapping, list(trait = "survived", life = "", covars = character(0), cov_factor = character(0))))
app_fit("female survival (binomial, no proxy)", gamm_data(bs$data, bs$meta),
        list(family = "binomial", k = 8, proxy = "none", proxy_form = "none"))
gdm <- gamm_data(fm$b$data, fm$b$meta)
for (tr in c("offspring", "offspring_EG", "offspring_WG")) {
  bm <- standardise_data(fm$d, utils::modifyList(fm$ex$mapping, list(trait = tr)))
  app_fit(paste("male", tr, "(Poisson, longevity linear)"), gamm_data(bm$data, bm$meta), list(family = "poisson", k = 10, proxy = "LS", proxy_form = "linear"))
}
line("  note: the app has no observation-level random effect; for overdispersed counts choose the negative binomial family.")
pc <- safe("wren female proxy comparison", gamm_proxy_comparison(gd, list(family = "poisson", k = 8, proxy = "LS", proxy_form = "linear")))
if (isTRUE(pc$ok)) { print(pc$tables$LS[, c("Model", "AIC", "dAIC", "Weight", "Records")], row.names = FALSE); line("  ", pc$texts[["LS"]]) }

# =====================================================================================================================
line("")
line("==== 4. Lalande et al. 2022, Journal of Evolutionary Biology: Asian elephant body mass ====")
line("Published: by-sex smooth of within-individual age beats a common smooth by 65.7 AICc (Table S2); delta-age smooth")
line("edf 6.096 (males) and 6.899 (females) (Table S5). mgcv::gamm, log body mass, cr basis k = 15, age at last measurement")
line("(linear + quadratic), terminal year, random intercepts for elephant and township.")
el <- example_data("lalande_elephants"); e <- el$d
e$idf <- factor(e$id); e$tw <- factor(e$Township); e$sexf <- factor(e$sex); e$term <- factor(e$terminal)
e$age_last <- stats::ave(e$age, e$id, FUN = max); e$delta_age <- e$age - stats::ave(e$age, e$id, FUN = mean)
e$age_last_s <- as.numeric(scale(e$age_last)); e$delta_age_s <- as.numeric(scale(e$delta_age))
by_sex <- safe("elephant by sex (authors)", gamm4::gamm4(log_bm ~ term + age_last_s + I(age_last_s^2) + s(delta_age_s, by = sexf, bs = "cr", k = 15) + sexf,
                                                         random = ~ (1 | idf) + (1 | tw), data = e, REML = FALSE))
common <- safe("elephant common (authors)", gamm4::gamm4(log_bm ~ term + age_last_s + I(age_last_s^2) + s(delta_age_s, bs = "cr", k = 15) + sexf,
                                                         random = ~ (1 | idf) + (1 | tw), data = e, REML = FALSE))
if (!is.null(by_sex) && !is.null(common)) {
  line(sprintf("(a) authors' specification (gamm4, ML): AIC by-sex %.1f, common %.1f, difference %.1f", AIC(by_sex$mer), AIC(common$mer), AIC(common$mer) - AIC(by_sex$mer)))
  show("s(delta_age):sexF", by_sex$gam, "sexfF"); show("s(delta_age):sexM", by_sex$gam, "sexfM")
}
gde <- gamm_data(el$b$data, el$b$meta)
se <- list(family = "gaussian", k = 15, bs = "cr", proxy = "ALR", proxy_form = "linear", group = "cv_sex", structure = "I")
app_fit("elephant log body mass by sex (structure I, ALR linear)", gde, se)
gc <- safe("elephant group comparison", gamm_group_comparison(gde, utils::modifyList(gamm_defaults(), se)))
if (isTRUE(gc$ok)) { line(sprintf("(b) disappR group comparison (%s): ", gc$engine), paste(sprintf("%s %.1f", gc$table$Model, gc$table$dAIC), collapse = ", ")); line("  ", gc$verdict) }

if (length(errors)) { cat("\nErrors:\n", paste("-", errors, collapse = "\n"), "\n"); quit(status = 1) }
cat("\nReplication script finished without errors.\n")
