# disappR server test (no browser needed)
# Run from the package root:   Rscript tests/scripts/app_test.R
# Drives the real server function with shiny::testServer(): sets inputs, fits models and
# renders every output, so runtime errors in server code surface here.

library(shiny)
app_dir <- normalizePath(file.path("inst", "app"))
app <- shiny::shinyAppDir(app_dir)
problems <- character(0)

# validate()/req() messages are expected states, not errors
touch <- function(output_env, names) {
  for (nm in names) {
    res <- tryCatch({
      output_env[[nm]]
      "ok"
    }, shiny.silent.error = function(e) "ok", error = function(e) conditionMessage(e))
    if (!identical(res, "ok")) problems <<- c(problems, paste0(nm, ": ", res))
  }
}

# outputs the server defines (0.24.6: five names of removed outputs dropped; audit Part H checks this list)
all_outputs <- c("store_plot_pick_ui", "store_pred_plot", "trait_ages_note", "toy_status", "mapping_ui", "censor_value_ui", "data_metrics", "integrity_table",
                 "visual_scale_advice", "a3_cap_warning", "model_rowset_warning", "coef_re_flags",
                 "integrity_extra", "dist_var_ui", "dist_plot", "data_preview",
                 "visual_proxy_ui", "a1_plot", "bin_diff_plot", "a2_plot", "toy_card_visual",
                 "sampling_metrics", "heatmap", "missing_by_age", "missing_vs_var",
                 "drivers_table", "proxy_plots_ui", "proxy_alr_mean", "proxy_alr_ls", "proxy_mean_ls",
                 "a3_metrics", "a3_plot", "a3_mean_plot", "a3_compare_table", "a3_coef_table",
                 "family_hint", "structure_note", "b2_table", "b2_plot",
                 "model_fit_note", "aic_plot", "aic_table", "lrt_table", "status_table", "drop_note", "pred_plot",
                 "deviation_note", "deviation_table", "coef_model_ui", "coef_table", "family_table", "dharma_model_ui", "dharma_table", "code_ui",
                 "facet_ui", "coef_re_table", "saved_controls", "saved_ui", "function_default_note", "use_a3_function_ui", "code_data_text", "code_visual_text", "code_sampling_text", "code_individual_text",
                 "subset_ui", "subset_levels_ui", "subset_status", "cov_int_ui", "sampling_caveat",
                 "a5_plot", "a5_note", "a6_plot", "a6_note",
                 "a7_plot", "a7_note", "zi_ui", "random_support_note", "consistency_ui",
                 "pred_models_ui", "pred_by_ui", "scaling_table", "coef_interpretation", "performance_model_ui",
                 "performance_table", "performance_note")

base_inputs <- list(n_bins = 4, bin_method = "equal", show_se = TRUE,
                    diff_lines = "pairs", a2_points = TRUE, trait_scale = "raw", heat_order = "alr", heat_n = 500,
                    a6_compare = "population", subset_var = "",
                    miss_var = "ALR", dist_unit = "row",
                    a3_function = "Quadratic", a3_recon = "both", a3_n = 10, a3_order = "random", facet_var = "",
                    b2_functions = c("Linear", "Quadratic"),
                    zi_vars = NULL, model_age_function = "Quadratic", random_structure = "none", among_order = "linear",
                    include_invalid = FALSE, standardise = TRUE,
                    show_observed = TRUE, show_decomp = TRUE, show_a3 = FALSE, pred_by = "", family_check_model = "M4")

shiny::testServer(app, {
  do.call(session$setInputs, base_inputs)

  # --- simulated: dramatic body mass, MCAR, selective appearance, diet and families ---
  session$setInputs(data_source = "toy", toy_trait = "mass", toy_form = "Quadratic", toy_strength = "dramatic",
                    toy_sd_type = "both", toy_sd_dir = "1", toy_rate_var = "low", toy_mean_ls = 12,
                    toy_missingness = "mcar", toy_afr_mode = "individual", toy_sa_type = "both", toy_sa_dir = "-1",
                    toy_diet = TRUE, toy_groups = TRUE, toy_n = 150, toy_seed = 3, dup_action = "keep")
  session$setInputs(toy_commit = 1)
  session$setInputs(col_id = "ID", col_age = "age", col_trait = "body_mass", col_alr = "__AUTO_LAST__",
                    col_entry = "AFR", col_life = "lifespan", col_cov_num = NULL, col_cov_fac = "diet", col_cov_int = "diet|||age",
                    col_group = "family", group_nested = TRUE, col_condition = "condition",
                    col_random = "diet", col_censor = "", start_mode = "afr", dist_var = "body_mass")
  cat("Simulated rows:", nrow(dat()), " individuals:", length(unique(dat()$id)), "\n")
  session$setInputs(visual_proxy = "ALR", model_family = "gaussian", use_M1 = TRUE, use_M2 = TRUE, use_M3 = TRUE, use_M4 = TRUE, use_M5 = TRUE, use_M6 = TRUE, use_M7 = TRUE, use_M8 = TRUE, use_M9 = FALSE, use_M10 = FALSE)
  session$setInputs(fit_models = 1)
  touch(output, all_outputs)
  # 0.24.2: dataset-wide and trait-specific ALR/AFR, with the grid and the proxy panels redrawn under each
  session$setInputs(trait_ages = FALSE)
  touch(output, c("mapping_ui", "trait_ages_note", "heatmap", "missing_by_age", "sampling_metrics", "proxy_plots_ui", "integrity_table"))
  session$setInputs(heat_order = "afr")
  touch(output, "heatmap")
  session$setInputs(trait_ages = TRUE, heat_order = "alr")
  touch(output, c("trait_ages_note", "heatmap", "missing_by_age", "sampling_metrics", "integrity_table"))
  session$setInputs(pred_as_lines = TRUE)
  touch(output, "pred_plot")
  session$setInputs(pred_as_lines = FALSE)
  r <- current_models()
  cat("Toy (Gaussian) best model:", r$aic$Model[[1]], "\n")
  session$setInputs(facet_var = "diet", n_bins = 12)
  touch(output, c("a1_plot", "bin_diff_plot", "a2_plot"))
  session$setInputs(visual_proxy = "AFR",
                    trait_scale = "log1p", dist_unit = "individual", dist_var = "diet", facet_var = "")
  touch(output, c("a1_plot", "bin_diff_plot", "a2_plot", "dist_plot"))
  for (ho in c("afr", "trait", "random")) {
    session$setInputs(heat_order = ho)
    touch(output, "heatmap")
  }
  session$setInputs(a3_function = A3_NONLINEAR, a3_recon = "both")
  touch(output, c("a3_metrics", "a3_mean_plot", "a3_compare_table"))
  session$setInputs(a3_function = "Quadratic", run_functions = 1, use_a3_function = 1)
  touch(output, c("b2_table", "b2_plot", "use_a3_function_ui", "function_default_note"))
  session$setInputs(coef_model = "M4")
  touch(output, c("coef_table", "coef_re_table", "scaling_table", "coef_interpretation", "consistency_ui"))
  # new model options: random structures, polynomial among terms, extra terms, Models 9-10, predictions by level with A3 overlay
  for (rs in c("uncorrelated", "correlated", "auto")) {
    session$setInputs(random_structure = rs, use_M1 = TRUE, use_M2 = TRUE, use_M3 = FALSE, use_M4 = TRUE, use_M5 = FALSE, use_M6 = FALSE, use_M7 = FALSE, use_M8 = FALSE, use_M9 = FALSE, use_M10 = FALSE, fit_models = 10 + match(rs, c("uncorrelated", "correlated", "auto")))
    touch(output, c("model_fit_note", "aic_table", "random_support_note", "status_table"))
  }
  session$setInputs(random_structure = "none", among_order = "same", extra_M5 = "ALR", extra_M6 = c("ALR", "AFR_x_age"),
                    use_M1 = TRUE, use_M2 = TRUE, use_M3 = TRUE, use_M4 = TRUE, use_M5 = TRUE, use_M6 = TRUE, use_M7 = TRUE, use_M8 = TRUE, use_M9 = TRUE, use_M10 = TRUE, fit_models = 20)
  touch(output, c("model_fit_note", "aic_plot", "aic_table", "lrt_table", "code_ui", "pred_models_ui"))
  session$setInputs(pred_models = c("M1", "M4"), show_a3 = TRUE, pred_by = "cv_diet")
  session$setInputs(pred_models = character(0))
  touch(output, "pred_plot")
  session$setInputs(pred_models = c("M1", "M4"))
  for (sec in c("data", "visual", "sampling", "individual")) {
    txt <- tryCatch(output[[paste0("code_", sec, "_text")]], error = function(e) "")
    if (!nzchar(txt)) problems <- c(problems, paste("no R code for the", sec, "section"))
    else if (inherits(tryCatch(parse(text = txt), error = function(e) e), "error")) problems <- c(problems, paste("R code for the", sec, "section does not parse"))
  }
  touch(output, c("pred_plot", "pred_by_ui", "deviation_table"))
  session$setInputs(coef_model = "M9")
  touch(output, c("coef_table", "coef_interpretation", "scaling_table"))
  for (mid in MODEL_IDS) do.call(session$setInputs, stats::setNames(list(1), paste0("info_model_", mid)))
  session$setInputs(performance_model = "M4", run_performance = 1)
  touch(output, c("performance_table", "performance_note", "performance_plot"))
  if (requireNamespace("nlme", quietly = TRUE)) {
    session$setInputs(model_age_function = A3_NONLINEAR, among_order = "linear", extra_M5 = NULL, extra_M6 = NULL,
                      use_M1 = TRUE, use_M2 = TRUE, use_M3 = FALSE, use_M4 = TRUE, use_M5 = FALSE, use_M6 = FALSE, use_M7 = FALSE, use_M8 = FALSE, use_M9 = FALSE, use_M10 = FALSE, fit_models = 21, pred_by = "")
    touch(output, c("model_fit_note", "aic_table", "coef_table", "coef_interpretation", "pred_plot", "code_ui"))
    session$setInputs(model_age_function = "Quadratic", use_M1 = TRUE, use_M2 = TRUE, use_M3 = TRUE, use_M4 = TRUE, use_M5 = TRUE, use_M6 = FALSE, use_M7 = FALSE, use_M8 = FALSE, use_M9 = FALSE, use_M10 = FALSE, fit_models = 22)
  }
  # A4-A7 diagnostics, subsetting, trend-line and point options
  session$setInputs(a6_compare = "contrast", diff_lines = "pooled", a2_points = FALSE)
  touch(output, c("a5_plot", "a5_note", "a6_plot", "a6_note",
                  "a7_plot", "a7_note", "bin_diff_plot", "a2_plot", "sampling_caveat"))
  session$setInputs(subset_var = "diet")
  session$setInputs(subset_levels = "Standard")
  touch(output, c("subset_status", "data_metrics", "integrity_table", "a1_plot"))
  session$setInputs(subset_var = "")
  # the default ageing function follows the individual fits when the Modelling tab is opened
  session$setInputs(tabs = "individual")
  touch(output, c("a3_compare_table", "b2_table"))
  session$setInputs(tabs = "models")
  touch(output, "function_default_note")
  code_txt <- tryCatch(code_text(), error = function(e) "")
  if (!grepl("ggplot(", code_txt, fixed = TRUE)) problems <- c(problems, "exported R code does not include the figure code")
  if (!inherits(tryCatch(parse(text = code_txt), error = function(e) e), "error")) cat("Exported R code (models + figure) parses.\n") else
    problems <- c(problems, "exported R code does not parse")
  save_ids <- c("save_a5", "save_a6", "save_a7", "save_performance",
                "save_integrity", "save_distribution", "save_a1", "save_a1_diff", "save_a2", "save_heatmap",
                "save_sampling", "save_missing_age", "save_missing_var", "save_proxies", "save_a3", "save_a3_compare",
                "save_b2", "save_models", "save_predictions", "save_coefs", "save_code", "save_overview")
  for (sid in save_ids) do.call(session$setInputs, stats::setNames(list(1), sid))
  cat("Saved results:", length(saved_results()), "of", length(save_ids), "save buttons\n")
  touch(output, c("saved_controls", "saved_ui"))
  rep_html <- tryCatch(html_report(saved_results(), "test"), error = function(e) paste("ERROR", conditionMessage(e)))
  rep_txt <- tryCatch(text_report(saved_results(), "test"), error = function(e) paste("ERROR", conditionMessage(e)))
  if (any(grepl("^ERROR", c(rep_html[1], rep_txt[1])))) problems <<- c(problems, paste("report:", rep_html[1], rep_txt[1]))
  cat("HTML report lines:", length(rep_html), " embedded plots:", sum(grepl("data:image/png;base64", rep_html)), "\n")
  session$setInputs(saved_remove_id = names(saved_results())[[1]], saved_remove = 1)
  cat("After removing one saved result:", length(saved_results()), "\n")
  if (HAS_DHARMA) {
    session$setInputs(dharma_model = sub("Model ", "M", r$aic$Model[[1]]), run_dharma = 1)
    touch(output, c("dharma_table", "dharma_plot"))
  }

  # --- simulated counts (if glmmTMB) ---
  if (HAS_GLMMTMB) {
    session$setInputs(toy_trait = "count_zinb", toy_form = "Asymptotic exponential", toy_missingness = "complete", toy_afr_mode = "same",
                      toy_diet = FALSE, toy_groups = FALSE, toy_n = 120, toy_commit = 2)
    session$setInputs(col_trait = "fecundity", col_cov_num = NULL, col_cov_fac = NULL, col_cov_int = NULL, col_group = "", col_random = NULL, col_entry = "__AUTO_FIRST__",
                      start_mode = "same", start_age = 1,
                      visual_proxy = "ALR", trait_scale = "raw", dist_var = "fecundity", dist_unit = "row",
                      model_family = "zinb", zi_vars = c("f1", "ALR"), use_M1 = TRUE, use_M2 = TRUE, use_M3 = TRUE, use_M4 = TRUE, use_M5 = TRUE, use_M6 = TRUE, use_M7 = FALSE, use_M8 = FALSE, use_M9 = FALSE, use_M10 = FALSE)
    session$setInputs(fit_models = 2, run_family_check = 1)
    touch(output, all_outputs)
    cat("Toy (ZINB) best model:", current_models()$aic$Model[[1]], "\n")
  }

  # --- fruit-fly data ---
  session$setInputs(data_source = "example", example_id = "fly")
  session$setInputs(col_id = "F1_ID", col_age = "F1_Age", col_trait = "F2_Count", col_alr = "ALR",
                    col_entry = "__AUTO_FIRST__", col_life = "LS", col_cov_num = NULL, col_cov_fac = c("Rep", "Paternal_age", "Paternal_sperm_age"),
                    col_group = "F0_ID", group_nested = TRUE, col_condition = "", col_random = NULL,
                    col_censor = "Censored", censor_value = "0", start_mode = "same", start_age = 4,
                    dist_var = "F2_Count", visual_proxy = "ALR",
                    zi_vars = NULL, use_M1 = TRUE, use_M2 = TRUE, use_M3 = TRUE, use_M4 = TRUE, use_M5 = TRUE, use_M6 = FALSE, use_M7 = FALSE, use_M8 = FALSE, use_M9 = FALSE, use_M10 = FALSE)
  cat("Fly rows:", nrow(dat()), " individuals:", length(unique(dat()$id)), " censored:", meta()$n_censored, "\n")
  cat(sprintf("Fly missing expected occasions: %.1f%%\n", msum()$percent))
  # a bundled example ticked shows the benchmark note; unticked again for the rest of the test (0.24.2)
  session$setInputs(trait_ages = TRUE)
  touch(output, c("trait_ages_note", "heatmap", "integrity_table"))
  if (!grepl("documented", paste(capture.output(print(output$trait_ages_note)), collapse = " ")))
    problems <<- c(problems, "trait_ages_note: no benchmark note for a ticked bundled example")
  session$setInputs(trait_ages = FALSE)
  # the same data without LS: only the ALR vs mean age panel is drawn
  session$setInputs(col_life = "")
  touch(output, c("proxy_plots_ui", "proxy_alr_mean", "a2_plot"))
  session$setInputs(col_life = "LS")
  session$setInputs(model_family = if (HAS_GLMMTMB) "zinb" else "gaussian", fit_models = 3)
  touch(output, all_outputs)
  r <- current_models()
  print(r$aic[, c("Model", "AIC", "Delta_AIC")], row.names = FALSE)
  if (is.data.frame(r$row_sets)) {
    cat("Row sets (usable vs used):\n")
    print(r$row_sets, row.names = FALSE)
    if (any(r$row_sets$Rows_usable < r$row_sets$Rows_used)) problems <<- c(problems, "row_sets: a model is recorded as using more rows than it can")
  }

  # --- controls added in 0.20.9-0.20.19 ---
  session$setInputs(diff_points = FALSE, a3_show_fun = FALSE, trait_scale = "log1p", a3_scale = "log")
  touch(output, c("bin_diff_plot", "a3_plot", "a3_mean_plot", "visual_scale_advice"))
  session$setInputs(diff_points = TRUE, a3_show_fun = TRUE, trait_scale = "raw", a3_scale = "auto")
  session$setInputs(run_a3_compare = 1)
  touch(output, c("a3_compare_table", "a3_metrics", "a3_coef_table"))
  session$setInputs(dup_action = "mean")
  touch(output, c("integrity_table", "data_metrics"))
  session$setInputs(dup_action = "keep")
  session$setInputs(use_M7 = TRUE, use_M8 = TRUE, use_M9 = TRUE, use_M10 = TRUE, fit_models = 4)
  touch(output, all_outputs)
  ev <- evidence_grade()
  if (!is.null(ev) && is.data.frame(ev$lines) && !all(ev$lines$Status %in% c("supports", "caveat", "contradicts", "not saved"))) {
    problems <<- c(problems, paste("evidence line status:", paste(setdiff(ev$lines$Status, c("supports", "caveat", "contradicts", "not saved")), collapse = ", ")))
  }
})

if (length(problems)) {
  cat("\nOutput problems:\n", paste("-", problems, collapse = "\n"), "\n")
  # a non-zero exit status, so that run_all.R and the unit test report the failure (0.23.2)
  if (!interactive()) quit(status = 1)
} else {
  cat("\nAll outputs rendered without errors.\n")
}
