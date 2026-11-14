# disappR server test for step 8 (Bayesian refits with brms; no browser needed)
# Run from the package root:   Rscript tests/scripts/bayes_app_test.R
# Drives the real server with shiny::testServer(): fits step 5 on simulated data, refits a model with brms (short
# runs, in this process), renders every step-8 output, saves every step-8 result and checks the methods draft.
# Needs brms, rstan and a C++ compiler; exits with status 1 on any problem.

library(shiny)
if (!requireNamespace("brms", quietly = TRUE)) { cat("brms not installed: skipped.\n"); quit(status = 0) }
app <- shiny::shinyAppDir(normalizePath(file.path("inst", "app")))
problems <- character(0)
touch <- function(output_env, names) {
  for (nm in names) {
    res <- tryCatch({ output_env[[nm]]; "ok" }, shiny.silent.error = function(e) "ok", error = function(e) conditionMessage(e))
    if (!identical(res, "ok")) problems <<- c(problems, paste0(nm, ": ", res))
  }
}
bayes_outputs <- c("bayes_status_note", "bayes_env_ui", "bayes_env_table", "bayes_model_ui", "bayes_spec_ui", "bayes_prior_meaning",
                   "bayes_prior_table", "bayes_prior_reading", "bayes_prior_plot", "bayes_fit_note", "bayes_overall_ui", "bayes_diag_table",
                   "bayes_trace_plot", "bayes_reading_ui", "bayes_compare_table", "bayes_var_table", "bayes_peak_table", "bayes_curve_plot",
                   "bayes_ppc_reading", "bayes_ppc_dist_plot", "bayes_ppc_age_plot", "bayes_ppc_stats", "bayes_code", "bayes_methods_ui")
base_inputs <- list(model_age_function = "Quadratic", random_structure = "none", among_order = "linear", include_invalid = FALSE,
                    standardise = TRUE, bayes_width = "default", bayes_mode = "draft", bayes_iter = 400, bayes_adapt_delta = 0.95,
                    bayes_treedepth = 10, bayes_seed = 7, bayes_show_freq = TRUE)

shiny::testServer(app, {
  do.call(session$setInputs, base_inputs)
  # before any step-5 fit: every output renders a message, nothing errors
  touch(output, bayes_outputs)
  session$setInputs(bayes_check_env = 1)
  if (!isTRUE(bayes_env()$ok)) problems <<- c(problems, paste("environment:", bayes_env()$message))

  # --- Gaussian: simulated body mass with selective disappearance ---
  session$setInputs(data_source = "toy", toy_trait = "mass", toy_form = "Quadratic", toy_strength = "dramatic",
                    toy_sd_type = "both", toy_sd_dir = "1", toy_rate_var = "low", toy_mean_ls = 8,
                    toy_missingness = "mcar", toy_afr_mode = "fixed", toy_diet = TRUE, toy_groups = FALSE, toy_n = 80, toy_seed = 3, dup_action = "keep")
  session$setInputs(toy_commit = 1)
  session$setInputs(col_id = "ID", col_age = "age", col_trait = "body_mass", col_alr = "__AUTO_LAST__", col_life = "lifespan",
                    col_cov_num = NULL, col_cov_fac = "diet", col_group = "", col_random = NULL, col_censor = "")
  session$setInputs(model_family = "gaussian", use_M1 = TRUE, use_M2 = TRUE, use_M3 = TRUE, use_M4 = TRUE, use_M5 = FALSE, use_M6 = FALSE,
                    use_M7 = FALSE, use_M8 = FALSE, use_M9 = FALSE, use_M10 = FALSE)
  session$setInputs(fit_models = 1)
  r <- current_models()
  cat("Step 5 best model:", r$aic$Model[[1]], "\n")
  touch(output, c("bayes_status_note", "bayes_model_ui", "bayes_spec_ui", "bayes_prior_plot", "bayes_prior_table", "bayes_prior_reading"))
  for (w in c("narrow", "wide", "flat", "default")) { session$setInputs(bayes_width = w); touch(output, c("bayes_prior_plot", "bayes_prior_reading", "bayes_prior_table")) }
  session$setInputs(bayes_model = "M4")
  t0 <- Sys.time()
  session$setInputs(bayes_fit = 1)
  z <- bayes_store()
  cat(sprintf("Gaussian M4 refit: ok %s, %.0f s (compiled %s)\n", isTRUE(z$ok), as.numeric(difftime(Sys.time(), t0, units = "secs")), isTRUE(z$compiled)))
  if (!isTRUE(z$ok)) problems <<- c(problems, paste("Gaussian fit:", z$message))
  touch(output, bayes_outputs)
  ct <- bayes_compare_df()
  sh <- suppressWarnings(abs(as.numeric(ct$`Shift (step-5 SEs)`)))
  cat("Largest shift from step 5 (SEs):", format(max(sh, na.rm = TRUE), digits = 3), "\n")
  if (!any(is.finite(sh))) problems <<- c(problems, "no step-5 estimates matched the Bayesian ones")
  if (inherits(tryCatch(parse(text = bayes_code(z, z$spec)), error = function(e) e), "error")) problems <<- c(problems, "Bayesian R code does not parse")
  # saving: four results, and a step-8 setting must not alter the fingerprint of a step-5 result
  n0 <- length(saved_results())
  session$setInputs(save_models = 1)
  n1 <- length(saved_results())
  for (sid in c("save_bayes_diag", "save_bayes_results", "save_bayes_curve", "save_bayes_ppc")) do.call(session$setInputs, stats::setNames(list(1), sid))
  n2 <- length(saved_results())
  if (n2 - n1 != 4) problems <<- c(problems, sprintf("Bayesian saves: %d of 4 saved", n2 - n1))
  session$setInputs(bayes_seed = 8)
  session$setInputs(save_models = 2)
  if (length(saved_results()) != n2) problems <<- c(problems, "a step-8 setting changed the fingerprint of a step-5 result")
  # a changed setting makes the fit stale: saving is refused until refitted
  session$setInputs(save_bayes_curve = 2)
  if (length(saved_results()) != n2) problems <<- c(problems, "a stale Bayesian fit was saved")
  mt <- tryCatch(paste(methods_text(Filter(function(e) startsWith(e$section, "8"), saved_results()))$paragraphs, collapse = " "), error = function(e) paste("ERROR", conditionMessage(e)))
  if (!grepl("brms", mt)) problems <<- c(problems, paste("Bayesian methods paragraph missing:", substr(mt, 1, 200)))

  # --- the same model again with another prior width: the compiled model is re-used ---
  session$setInputs(bayes_seed = 7, bayes_width = "narrow")
  t0 <- Sys.time(); session$setInputs(bayes_fit = 2)
  z2 <- bayes_store()
  cat(sprintf("Refit with the narrow prior: ok %s, %.0f s (compiled %s)\n", isTRUE(z2$ok), as.numeric(difftime(Sys.time(), t0, units = "secs")), isTRUE(z2$compiled)))
  if (isTRUE(z2$compiled)) problems <<- c(problems, "changing the prior width recompiled the model")

  # --- Poisson: simulated counts ---
  session$setInputs(data_source = "toy", toy_trait = "count_pois", toy_n = 80, toy_seed = 5, toy_diet = FALSE)
  session$setInputs(toy_commit = 2)
  session$setInputs(col_id = "ID", col_age = "age", col_trait = "fecundity",
                    col_cov_fac = NULL)
  session$setInputs(model_family = "poisson", fit_models = 2)
  r <- model_res()
  if (isTRUE(r$ok)) {
    session$setInputs(bayes_model = "M2", bayes_width = "default", bayes_fit = 3)
    z3 <- bayes_store()
    cat(sprintf("Poisson M2 refit: ok %s\n", isTRUE(z3$ok)))
    if (!isTRUE(z3$ok)) problems <<- c(problems, paste("Poisson fit:", z3$message))
    touch(output, bayes_outputs)
  } else problems <<- c(problems, paste("Poisson step-5 fit failed:", r$message))
})

if (length(problems)) { cat("\nProblems:\n", paste("-", problems, collapse = "\n"), "\n"); quit(status = 1) }
cat("\nStep 8 server test passed.\n")
