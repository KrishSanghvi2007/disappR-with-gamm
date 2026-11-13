# Replication of published GAMM analyses with the app's GAMM engine (step 7).
# Run from the package root:   Rscript tests/scripts/gamm_replication.R  [> inst/validation/gamm/replication_results.txt]
# For each study: (a) the authors' own model specification fitted directly with mgcv on the bundled data, and (b) the
# same analysis through disappR's engine (standardise_data -> gamm_data -> gamm_fit), as a user would run it in the app.
# The published results are quoted for comparison. Fails (status 1) only if a fit errors.
suppressPackageStartupMessages({ library(mgcv) })
if (!file.exists("DESCRIPTION")) stop("Run from the package root.")
for (f in c("utils.R", "provenance.R", "simulation.R", "import.R", "validation.R", "data-preparation.R", "formulas.R", "model-fit.R",
            "model-comparison.R", "diagnostics.R", "trajectories.R", "disappearance.R", "missingness.R", "interpretation.R",
            "effects.R", "evidence.R", "ui-helpers.R", "export.R", "gamm.R")) suppressMessages(source(file.path("R", f), local = TRUE))
errors <- character(0)
dat_dir <- file.path("inst", "app", "data")
line <- function(...) cat(..., "\n", sep = "")
row_of <- function(fit, lab) { st <- summary(fit)$s.table; st[lab, , drop = FALSE] }
show_smooth <- function(label, fit, lab) { x <- row_of(fit, lab); line(sprintf("  %-46s edf %6.2f   stat %7.2f   p %s", label, x[1, 1], x[1, 3], format_p(x[1, 4]))) }
app_prep <- function(d, map) standardise_data(d, utils::modifyList(example_map(), map))
onset_of <- function(r) { po <- gamm_peak_onset(r, n_draws = 1000); po }
periods_txt <- function(r) {
  dv <- gamm_derivative(r)
  one <- function(sim) {
    pr <- gamm_periods(dv, simultaneous = sim)
    if (!nrow(pr)) return("none")
    paste(sprintf("%s%s %s-%s", ifelse(pr$Group == "All", "", paste0(pr$Group, ": ")), pr$Change, format_num(pr$From, 3), format_num(pr$To, 3)), collapse = "; ")
  }
  sprintf("simultaneous: %s | pointwise: %s", one(TRUE), one(FALSE))
}
safe <- function(label, expr) tryCatch(expr, error = function(e) { errors <<- c(errors, paste(label, conditionMessage(e))); NULL })

# =====================================================================================================================
line("==== 1. Tsui et al. 2026, Evolution Letters: Seychelles warbler body mass (z-scored) ====")
line("Published (paper, Results and Table 2): body mass declines with age; onset of decline 4.7 years (limits 1-8.5) from the")
line("derivative of the age smooth; lifespan fitted as a linear term; age x lifespan interaction significant for body mass;")
line("model choice among G, GS and S by AIC (delta < 4). Authors' code: model1.R (Zenodo 10.5281/zenodo.21101620).")
tw <- utils::read.csv(file.path(dat_dir, "tsui_2026_warbler_body_mass.csv"))
a <- tw
for (v in c("BirdID", "Observer", "SexEstimate", "newstat")) a[[v]] <- factor(a[[v]])
t0 <- Sys.time()
bm_g <- safe("Tsui authors' G", gam(BodyMass_z ~ s(age_year, k = 15) + newlifespan + RightTarsus_z + newstat + SexEstimate + CatchTime_mins + avg_invert +
                                      s(birthyear, bs = "re") + s(Observer, bs = "re") + s(BirdID, bs = "re"), data = a, method = "REML"))
if (!is.null(bm_g)) {
  line(sprintf("(a) Authors' specification, mgcv::gam REML: %d records, %d birds (%.0f s)", nrow(bm_g$model), length(unique(bm_g$model$BirdID)), as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  show_smooth("s(age_year)", bm_g, "s(age_year)")
  pt <- summary(bm_g)$p.table
  line(sprintf("  lifespan (per year): %.4f (SE %.4f), p %s", 365.25 * pt["newlifespan", 1], 365.25 * pt["newlifespan", 2], format_p(pt["newlifespan", 4])))
}
map_t <- list(id = "BirdID", age = "age_year", trait = "BodyMass_z", life = "lifespan_years",
              covars = c("RightTarsus_z", "newstat", "SexEstimate", "CatchTime_mins", "avg_invert"), cov_factor = c("newstat", "SexEstimate"),
              random = c("birthyear", "Observer"))
bt <- safe("Tsui prep", app_prep(tw, map_t))
if (!is.null(bt)) {
  gd <- gamm_data(bt$data, bt$meta)
  s <- utils::modifyList(gamm_defaults(), list(k = 15, proxy = "LS", proxy_form = "linear"))
  t0 <- Sys.time()
  r <- safe("Tsui app fit", gamm_fit(gd, s))
  if (isTRUE(r$ok)) {
    line(sprintf("(b) disappR engine (proxy = known lifespan, linear; same covariates and random terms): %d records, %d birds (%.0f s)", r$n, r$n_ind, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    line("  formula: ", r$formula)
    show_smooth("s(age)", r$fit, "s(age)")
    pt <- summary(r$fit)$p.table
    line(sprintf("  lifespan (per year): %.4f (SE %.4f), p %s", pt["LS", 1], pt["LS", 2], format_p(pt["LS", 4])))
    line("  periods of significant change (95% intervals) ", periods_txt(r))
    po <- onset_of(r); line(sprintf("  peak %s [%s]; onset of final decline %s [%s]", po[["Peak age"]], po[["Peak 95% CI"]], po[["Onset of final decline"]], po[["Onset 95% CI"]]))
    if (!is.null(bm_g)) line(sprintf("  difference in s(age) edf between (a) and (b): %.3f", summary(r$fit)$s.table["s(age)", 1] - summary(bm_g)$s.table["s(age_year)", 1]))
    line("  (one record differs: its Observer is blank, which disappR treats as missing and the authors' factor() kept as a level;")
    line("   the engine chose mgcv::bam because there are more than 400 birds)")
    line("  cautions: ", paste(gamm_cautions(r, gd = gd), collapse = " | "))
  } else if (!is.null(r)) errors <- c(errors, paste("Tsui app fit:", r$message))
  # sex: the authors' G, GS and S (factor-smooth 'fs' for sex) compared by REML AIC, against the app's five structures by ML
  if (!is.null(bm_g)) {
    bm_gs <- safe("Tsui GS", gam(BodyMass_z ~ s(age_year, k = 15) + newlifespan + RightTarsus_z + newstat + s(age_year, SexEstimate, k = 15, bs = "fs") + CatchTime_mins + avg_invert +
                                   s(birthyear, bs = "re") + s(Observer, bs = "re") + s(BirdID, bs = "re"), data = a, method = "REML"))
    bm_s <- safe("Tsui S", gam(BodyMass_z ~ s(age_year, SexEstimate, k = 15, bs = "fs") + newlifespan + RightTarsus_z + newstat + CatchTime_mins + avg_invert +
                                 s(birthyear, bs = "re") + s(Observer, bs = "re") + s(BirdID, bs = "re"), data = a, method = "REML"))
    if (!is.null(bm_gs) && !is.null(bm_s)) { ai <- AIC(bm_g, bm_gs, bm_s); line("(a) Authors' G / GS / S by REML AIC: ", paste(sprintf("%s %.1f", c("G", "GS", "S"), ai$AIC - min(ai$AIC)), collapse = ", ")) }
  }
  sg <- utils::modifyList(s, list(group = "cv_SexEstimate", structure = "I"))
  gc <- safe("Tsui app groups", gamm_group_comparison(gd, sg))
  if (isTRUE(gc$ok)) { line("(b) disappR group structures by ML AIC (dAIC): ", paste(sprintf("%s %.1f", gc$table$Model, gc$table$dAIC), collapse = ", ")); line("  ", gc$verdict) }
  # age x lifespan
  if (!is.null(bm_g)) {
    bm_ti <- safe("Tsui ti", gam(BodyMass_z ~ s(age_year, k = 15) + newlifespan + ti(age_year, newlifespan) + RightTarsus_z + newstat + SexEstimate + CatchTime_mins + avg_invert +
                                   s(birthyear, bs = "re") + s(Observer, bs = "re") + s(BirdID, bs = "re"), data = a, method = "REML"))
    if (!is.null(bm_ti)) show_smooth("(a) authors' style: ti(age_year, newlifespan)", bm_ti, "ti(age_year,newlifespan)")
  }
  pc <- safe("Tsui app proxy", gamm_proxy_comparison(gd, s))
  if (isTRUE(pc$ok)) { print(pc$tables$LS[, c("Model", "AIC", "dAIC", "Weight", "Records")], row.names = FALSE); line("  ", pc$texts[["LS"]]) }
}

# =====================================================================================================================
line("")
line("==== 2. Brown et al. 2021, Ecology and Evolution: Seychelles warbler haematocrit ====")
line("Published (Table 1, gamm4): age smooth edf = 7.805, F = 16.73, p < .001; N = 1379 records, 730 birds, 747 breed groups,")
line("14 catch years; fixed effects sex, status, catch time and sex x status; peak at about 1.5 years (Section 3.1).")
bw <- utils::read.csv(file.path(dat_dir, "brown_2021_warbler_haematocrit.csv"))
b2 <- bw[stats::complete.cases(bw[, c("Haematocrit", "AgeYears", "Sex", "Status", "DecimalCatchTime")]), ]
for (v in c("BirdID", "BreedGroupID", "CatchYear", "Sex", "Status")) b2[[v]] <- factor(b2[[v]])
line(sprintf("  bundled data: %d records, %d birds, %d breed groups, %d catch years", nrow(b2), nlevels(b2$BirdID), nlevels(b2$BreedGroupID), nlevels(b2$CatchYear)))
if (requireNamespace("gamm4", quietly = TRUE)) {
  g4 <- safe("Brown gamm4", gamm4::gamm4(Haematocrit ~ s(AgeYears) + Sex * Status + DecimalCatchTime, random = ~ (1 | BirdID) + (1 | BreedGroupID) + (1 | CatchYear), data = b2))
  if (!is.null(g4)) show_smooth("(a) authors' specification with gamm4: s(AgeYears)", g4$gam, "s(AgeYears)")
}
bb <- safe("Brown mgcv", gam(Haematocrit ~ s(AgeYears) + Sex * Status + DecimalCatchTime + s(BirdID, bs = "re") + s(BreedGroupID, bs = "re") + s(CatchYear, bs = "re"), data = b2, method = "REML"))
if (!is.null(bb)) show_smooth("(a') same model in mgcv::gam REML: s(AgeYears)", bb, "s(AgeYears)")
map_b <- list(id = "BirdID", age = "AgeYears", trait = "Haematocrit", alr = "AgeLastSeen",
              covars = c("Sex", "Status", "DecimalCatchTime"), cov_factor = c("Sex", "Status"), cov_int = "Sex|||Status",
              random = c("BreedGroupID", "CatchYear"))
bp <- safe("Brown prep", app_prep(bw, map_b))
if (!is.null(bp)) {
  gd <- gamm_data(bp$data, bp$meta)
  s0 <- utils::modifyList(gamm_defaults(), list(proxy = "none", proxy_form = "none", cov_pairs = bp$meta$cov_pairs, cov_age = bp$meta$cov_age))
  r <- safe("Brown app", gamm_fit(gd, s0))
  if (isTRUE(r$ok)) {
    line(sprintf("(b) disappR engine, as published (no lifespan term): %d records, %d birds", r$n, r$n_ind))
    line("  formula: ", r$formula)
    show_smooth("s(age)", r$fit, "s(age)")
    line("  periods of significant change (95% intervals) ", periods_txt(r))
    po <- onset_of(r); line(sprintf("  peak %s [%s]", po[["Peak age"]], po[["Peak 95% CI"]]))
  } else if (!is.null(r)) errors <- c(errors, paste("Brown app:", r$message))
  r2 <- safe("Brown app ALR", gamm_fit(gd, utils::modifyList(s0, list(proxy = "ALR", proxy_form = "linear"))))
  if (isTRUE(r2$ok)) {
    pt <- summary(r2$fit)$p.table
    line(sprintf("(b') adding ALR (age last seen) as a linear term: ALR %.4f (SE %.4f), p %s; s(age) edf %.2f",
                 pt["ALR", 1], pt["ALR", 2], format_p(pt["ALR", 4]), summary(r2$fit)$s.table["s(age)", 1]))
    pc <- safe("Brown proxy", gamm_proxy_comparison(gd, utils::modifyList(s0, list(proxy = "ALR", proxy_form = "linear"))))
    if (isTRUE(pc$ok)) { print(pc$tables$ALR[, c("Model", "AIC", "dAIC", "Weight", "Records")], row.names = FALSE); line("  ", pc$texts[["ALR"]]) }
  }
}

if (length(errors)) { cat("\nErrors:\n", paste("-", errors, collapse = "\n"), "\n"); quit(status = 1) }
cat("\nReplication script finished without errors.\n")
