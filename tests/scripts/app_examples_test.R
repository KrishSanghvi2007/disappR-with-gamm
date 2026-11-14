# disappR server test of every bundled example through steps 1-7 (no browser needed)
# Run from the package root:   Rscript tests/scripts/app_examples_test.R  [example ids ...]
# For each published example, as a user would: choose it on the Data tab (with its column mapping), render the outputs
# of steps 1-4, fit its step-5 models with its published settings, render step 5, fit the GAMM of step 7 with the
# example's settings, render every GAMM output, run the shape comparison, save results and draft the methods (step 6).
# Prints one line per example; exits with status 1 if any output errors or a fit that should work fails.

library(shiny)
app <- shiny::shinyAppDir(normalizePath(file.path("inst", "app")))
eng <- new.env()
for (f in c("utils.R", "provenance.R", "simulation.R", "import.R")) sys.source(file.path("R", f), envir = eng)
EX <- get("EXAMPLES", eng)
ids <- commandArgs(trailingOnly = TRUE)
if (!length(ids)) ids <- names(EX)
problems <- character(0)
touch <- function(output_env, names, ex) {
  for (nm in names) {
    res <- tryCatch({ output_env[[nm]]; "ok" }, shiny.silent.error = function(e) "ok", error = function(e) conditionMessage(e))
    if (!identical(res, "ok")) problems <<- c(problems, sprintf("[%s] %s: %s", ex, nm, res))
  }
}
step1_4 <- c("mapping_ui", "data_metrics", "integrity_table", "data_preview", "dist_plot", "visual_proxy_ui", "a1_plot", "bin_diff_plot", "a2_plot",
             "sampling_metrics", "heatmap", "missing_by_age", "proxy_plots_ui", "a3_metrics", "a3_plot", "a3_mean_plot", "b2_table", "b2_plot")
step5 <- c("model_fit_note", "aic_plot", "aic_table", "lrt_table", "status_table", "drop_note", "pred_plot", "coef_model_ui", "coef_table",
           "coef_interpretation", "coef_re_table", "scaling_table", "peak_table", "structure_note", "random_support_note")
step7 <- c("gamm_data_note", "gamm_family_hint", "gamm_individual_note", "gamm_group_ui", "gamm_proxy_hint", "gamm_formula_ui", "gamm_reading_ui",
           "gamm_cautions_ui", "gamm_next_ui", "gamm_scale_ui", "gamm_n_proxy_ui", "gamm_curve_plot", "gamm_curve_note", "gamm_support_plot",
           "gamm_guide_ui", "gamm_output_reading", "gamm_out_stats", "gamm_out_smooth", "gamm_out_param", "gamm_out_random", "gamm_out_formula",
           "gamm_deriv_plot", "gamm_periods_table", "gamm_peak_table", "gamm_proxy_terms_table", "gamm_naive_plot", "gamm_groups_note",
           "gamm_check_overall", "gamm_check_summary", "gamm_kcheck_table", "gamm_concurvity_table", "gamm_resid_note", "gamm_resid_plot",
           "gamm_poly_reading", "gamm_poly_table", "gamm_engine_note_poly", "gamm_code")
summary_rows <- list()

shiny::testServer(app, {
  for (ex_id in ids) {
    ex <- EX[[ex_id]]; m <- ex$mapping
    t0 <- Sys.time()
    session$setInputs(data_source = "example", example_id = ex_id)
    cov <- m$covars %||% character(0); fac <- m$cov_factor %||% character(0)
    session$setInputs(col_id = m$id, col_age = m$age, col_trait = m$trait, col_alr = m$alr %||% "__AUTO_LAST__", col_entry = m$entry %||% "__AUTO_FIRST__",
                      col_life = m$life %||% "", col_condition = m$condition %||% "", col_cov_num = setdiff(cov, fac), col_cov_fac = intersect(fac, cov),
                      col_cov_int = c(if (length(m$cov_age)) paste(m$cov_age, "age", sep = "|||"), m$cov_int %||% character(0)),
                      col_group = m$group %||% "", col_group2 = m$group2 %||% "", group_nested = isTRUE(m$nested), col_random = m$random %||% character(0),
                      col_censor = m$censor %||% "", censor_value = m$censor_value %||% "", col_trials = m$trials %||% "",
                      exclude_inconsistent = identical(m$inconsistent, "exclude"), trait_ages = !isFALSE(m$trait_specific_ages),
                      start_mode = m$start_mode %||% "afr", start_age = m$start_age %||% NA, age_round = m$age_round %||% NA,
                      visual_proxy = "ALR", dist_var = m$trait, n_bins = 4, bin_method = "equal", a3_function = ex$age_function %||% "Quadratic",
                      b2_functions = c("Linear", "Quadratic"), heat_order = "alr", heat_n = 500)
    d <- tryCatch(dat(), error = function(e) NULL)
    if (is.null(d) || !nrow(d)) { problems <<- c(problems, sprintf("[%s] the data did not load", ex_id)); next }
    touch(output, step1_4, ex_id)
    # step 5 with the example's published settings
    mods <- ex$models %||% c("M1", "M2", "M3", "M4")
    use <- stats::setNames(as.list(paste0("M", 1:10) %in% mods), paste0("use_M", 1:10))
    do.call(session$setInputs, c(use, list(model_family = ex$family %||% "gaussian", model_age_function = ex$age_function %||% "Quadratic",
                                           random_structure = ex$random_structure %||% "none", among_order = "linear", standardise = TRUE,
                                           include_invalid = FALSE)))
    session$setInputs(fit_models = sample.int(1e6, 1))
    r5 <- tryCatch(model_res(), error = function(e) NULL)
    ok5 <- isTRUE(r5$ok)
    if (!ok5) problems <<- c(problems, sprintf("[%s] step 5 fit failed: %s", ex_id, r5$message %||% "no result"))
    else session$setInputs(coef_model = names(r5$fits)[[1]])
    touch(output, step5, ex_id)
    # step 7 with the example's GAMM settings (or the defaults), the family suggested for the trait
    g <- ex$gamm %||% list()
    session$setInputs(gamm_family = gamm_suggest_family(ex$family %||% "gaussian"), gamm_k = g$k %||% 10, gamm_individual = g$individual %||% "ri",
                      gamm_proxy = g$proxy %||% "ALR", gamm_proxy_form = g$proxy_form %||% "linear", gamm_afr_form = g$afr_form %||% "none",
                      gamm_olre = isTRUE(g$olre), gamm_terminal = FALSE, gamm_group = "", gamm_engine = "auto", gamm_covars = TRUE,
                      gamm_random = TRUE, gamm_cov_smooth = FALSE, gamm_ar1 = FALSE, gamm_show_data = "means", gamm_age_bins = 8)
    session$setInputs(gamm_fit = sample.int(1e6, 1))
    r7 <- gamm_store()
    ok7 <- isTRUE(r7$ok)
    if (!ok7) problems <<- c(problems, sprintf("[%s] GAMM fit failed: %s", ex_id, r7$message %||% "no result"))
    touch(output, step7, ex_id)
    for (sd in c("bins", "points", "none")) { session$setInputs(gamm_show_data = sd); touch(output, c("gamm_curve_plot", "gamm_curve_note"), ex_id) }
    session$setInputs(gamm_run_poly = sample.int(1e6, 1))
    touch(output, c("gamm_poly_reading", "gamm_poly_table", "gamm_guide_ui"), ex_id)
    poly_ok <- is.data.frame(gamm_poly_store())
    if (ok7 && !poly_ok) problems <<- c(problems, sprintf("[%s] shape comparison failed: %s", ex_id, gamm_poly_store()$message %||% ""))
    # step 6: save one result from each step and draft the methods
    n0 <- length(saved_results())
    if (ok5) session$setInputs(save_models = sample.int(1e6, 1))
    if (ok7) { session$setInputs(save_gamm_curve = sample.int(1e6, 1)); session$setInputs(save_gamm_output = sample.int(1e6, 1)) }
    n_saved <- length(saved_results()) - n0
    mt <- tryCatch(paste(methods_text(utils::tail(saved_results(), n_saved))$paragraphs, collapse = " "), error = function(e) paste("ERROR", conditionMessage(e)))
    if (grepl("^ERROR", mt)) problems <<- c(problems, sprintf("[%s] methods draft: %s", ex_id, mt))
    touch(output, c("saved_ui"), ex_id)
    summary_rows[[ex_id]] <<- data.frame(Example = ex_id, Records = nrow(d), Individuals = length(unique(d$id)), Step5 = if (ok5) r5$aic$Model[[1]] else "FAILED",
                                         GAMM = if (ok7) sprintf("%s, %d rows", r7$engine, r7$n) else "FAILED", Shape = if (poly_ok) gamm_poly_store()$Model[[1]] else "-",
                                         Saved = n_saved, Seconds = round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), stringsAsFactors = FALSE)
    cat(sprintf("%-24s records %5d  step 5 %-9s GAMM %-18s shape %-14s saved %d  %4.0f s\n", ex_id, nrow(d), summary_rows[[ex_id]]$Step5,
                summary_rows[[ex_id]]$GAMM, summary_rows[[ex_id]]$Shape, n_saved, summary_rows[[ex_id]]$Seconds))
  }
})

if (length(problems)) { cat("\nProblems:\n", paste("-", problems, collapse = "\n"), "\n"); quit(status = 1) }
cat("\nEvery example ran through steps 1-7 without errors.\n")
