# Step 8 - Bayesian models (optional). Server code, evaluated inside server() (see the end of server.R), so it can read
# the step-5 fit (model_res(), model_sig()), the shared data reactives and the server's helpers (notify, add_saved,
# nothing_to_save, step4_child). It writes only its own outputs (bayes_*), downloads (download_bayes_*) and saved
# results (section "8 Bayesian").

BAYES_TIMEOUT <- 3600   # seconds before a Bayesian fit running in a separate R process is stopped
bayes_cache_dir <- file.path(tempdir(), "disappR_bayes")

# Runs the fit in a separate R process (as steps 4 and 7 do), with the elapsed time in the progress bar; in the same
# process when callr is not installed or under testServer().
bayes_run <- function(args) {
  if (!isTRUE(step4_isolated) || !is.function(session$sendProgress)) return(do.call(bayes_fit, args))
  p <- callr::r_bg(step4_child, args = list(engine_dir = step4_engine_dir, files = DISAPPR_ENGINE_FILES, what = "bayes_fit",
                                           args = args, progress_file = ""), supervise = TRUE)
  t0 <- Sys.time()
  withProgress(message = "Fitting with brms and Stan", value = 0.1, {
    while (p$is_alive()) {
      p$wait(1000)
      el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      setProgress(value = min(0.95, 0.1 + el / 600),
                  detail = sprintf("%d s. A new model is first compiled to C++ (about 1–2 minutes); later fits of it start at once.", round(el)))
      if (el > BAYES_TIMEOUT) {
        p$kill()
        return(list(ok = FALSE, message = sprintf("Stopped after %d minutes. Use the draft run, fewer iterations or a simpler random-effect structure.", BAYES_TIMEOUT / 60)))
      }
    }
  })
  tryCatch(p$get_result(), error = function(e) list(ok = FALSE, message = paste("The separate R process stopped:", conditionMessage(e))))
}

# ---- environment and the model to refit --------------------------------------------------------------------------
bayes_env <- reactiveVal(bayes_environment(FALSE))
observeEvent(input$bayes_check_env, disappr_guard("input$bayes_check_env", {
  bayes_env(withProgress(message = "Checking brms, Stan and the C++ compiler", value = 0.5, bayes_environment(TRUE)))
}))
output$bayes_env_ui <- renderUI({
  e <- bayes_env()
  div(class = if (isTRUE(e$ok) && isTRUE(e$compiler)) "prefit-box" else "caution-box", e$message)
})
output$bayes_env_table <- renderTable(bayes_env()$packages, striped = TRUE, spacing = "xs")

bayes_r5 <- reactive({
  r <- model_res()
  shiny::validate(shiny::need(!is.null(r), "Fit the models on the Modelling tab (step 5) first: step 8 refits one of them."))
  shiny::validate(shiny::need(isTRUE(r$ok), "The step-5 fit has no usable model: fix it on the Modelling tab first."))
  r
})
bayes_r5_stale <- reactive({ r <- model_res(); !is.null(r) && !identical(r$signature, safe_get(model_sig())) })

output$bayes_status_note <- renderUI({
  r <- model_res()
  e <- bayes_env()
  if (!isTRUE(e$packages_ok)) return(div(class = "caution-box", e$message))
  if (is.null(r)) return(p(class = "small-note", "Step 8 refits a model of step 5. Fit the models on the Modelling tab first; then come back here."))
  if (!isTRUE(r$ok)) return(div(class = "caution-box", "The step-5 fit has no usable model: fix it on the Modelling tab first."))
  tagList(
    fluidRow(
      metric_card("Step-5 models fitted", length(r$fits)),
      metric_card("Records", format(nrow(r$data), big.mark = ",")),
      metric_card("Individuals", format(length(unique(r$data$id)), big.mark = ",")),
      metric_card("Family", sub(" \\(lme4\\)$", "", family_label(r$family)))),
    if (isTRUE(bayes_r5_stale())) div(class = "caution-box", "The data or step-5 settings changed since the step-5 fit: refit on the Modelling tab so that both steps use the same model."))
})

output$bayes_model_ui <- renderUI({
  r <- safe_get(bayes_r5())
  if (is.null(r)) return(p(class = "small-note", "No step-5 fit yet."))
  ms <- names(r$fits)
  a <- r$aic
  lab <- vapply(ms, function(m) {
    i <- if (is.data.frame(a) && nrow(a)) match(model_label(m), a$Model) else NA
    if (is.finite(i) && is.finite(a$Delta_AIC[i])) sprintf("%s (ΔAIC %s)", model_label(m), format_num(a$Delta_AIC[i], 3)) else model_label(m)
  }, character(1))
  best <- bayes_default_model(r)
  tagList(
    selectInput("bayes_model", "Model to refit", stats::setNames(ms, lab), best),
    p(class = "small-note", sprintf("%s is preselected because it ranks best by AIC in step 5. Refitting the models you are choosing between is a later option (comparison by LOO).", model_label(best))))
})
bayes_spec_now <- reactive({
  r <- bayes_r5()
  m <- input$bayes_model %||% bayes_default_model(r)
  sp <- bayes_spec(r, m)
  shiny::validate(shiny::need(isTRUE(sp$ok), sp$message %||% "This model cannot be refitted."))
  sp
})
output$bayes_spec_ui <- renderUI({
  r <- safe_get(bayes_r5())
  if (is.null(r)) return(NULL)
  sp <- bayes_spec(r, input$bayes_model %||% bayes_default_model(r))
  if (!isTRUE(sp$ok)) return(div(class = "caution-box", sp$message))
  tagList(
    p(strong("What brms will fit: "), code(display_term(sp$formula))),
    p(sprintf("%s family%s; %s records from %s individuals, the same records as step 5. Age and the lifespan proxies are on the scale step 5 used (%s).",
              family_label(sp$family), if (identical(sp$brms_family, "bernoulli")) " (Bernoulli: binary 0/1 records)" else if (identical(sp$family, "binomial")) " (successes out of trials)" else "",
              format(sp$n, big.mark = ","), format(sp$n_ind, big.mark = ","), if (isTRUE(sp$standardise)) "standardised" else "raw")),
    p(class = "small-note", "The formula is step 5's, written in lme4 syntax, which brms shares: the fixed effects are estimated with the same meaning, so the two fits can be compared term by term."))
})

# ---- priors and the prior predictive check -----------------------------------------------------------------------
bayes_pv <- reactive(bayes_priors(bayes_spec_now(), input$bayes_width %||% "default"))
bayes_prior_pc <- reactive(bayes_prior_curves(bayes_spec_now(), bayes_pv()))
output$bayes_prior_meaning <- renderUI({
  pv <- bayes_pv()
  tags$ul(class = "small-note", lapply(unname(pv$meaning), tags$li))
})
output$bayes_prior_table <- renderTable({
  t <- bayes_pv()$table
  t[, c("Class", "Term", "Prior")]
}, striped = TRUE, spacing = "xs")
output$bayes_prior_reading <- renderUI(p(bayes_prior_reading(bayes_prior_pc(), bayes_spec_now(), bayes_pv())))
bayes_obs_means <- function(sp) {
  y <- if (identical(sp$family, "binomial") && ".succ" %in% names(sp$data)) sp$data$.succ / sp$data$.trials else sp$data$trait
  o <- stats::aggregate(list(mean = y), list(age = sp$data$age), mean)
  o$n <- as.numeric(table(sp$data$age)[as.character(o$age)])
  o
}
bayes_trait_label <- function(sp) {
  lab <- safe_get(current_map()$trait) %||% "Trait"
  if (identical(sp$family, "binomial")) paste(lab, "(probability)") else lab
}
bayes_prior_obj <- reactive({
  pc <- bayes_prior_pc(); sp <- bayes_spec_now()
  ob <- bayes_obs_means(sp)
  ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = pc$observed[1], ymax = pc$observed[2], fill = "#E69F00", alpha = 0.15) +
    geom_line(data = pc$curves, aes(age, value, group = draw), colour = "grey40", alpha = 0.18) +
    geom_point(data = ob, aes(age, mean), colour = "#35637A", size = 2) +
    coord_cartesian(ylim = pc$limits) +
    labs(x = "Age", y = bayes_trait_label(sp), subtitle = "Grey: curves allowed by the priors. Orange band: range of the observed values. Points: observed means.") +
    theme_disappR(13)
})
output$bayes_prior_plot <- renderPlot(bayes_prior_obj())

# ---- fitting -----------------------------------------------------------------------------------------------------
bayes_settings_now <- reactive({
  it <- suppressWarnings(as.integer(input$bayes_iter))
  bayes_settings(input$bayes_mode %||% "draft", iter = if (is.finite(it)) it else NULL, warmup = if (is.finite(it)) floor(it / 2) else NULL,
                 adapt_delta = input$bayes_adapt_delta %||% 0.95, max_treedepth = input$bayes_treedepth %||% 10,
                 seed = input$bayes_seed %||% 2024L, width = input$bayes_width %||% "default")
})
bayes_sig <- reactive(paste(safe_get(model_res()$signature) %||% "", input$bayes_model %||% "", paste(unlist(bayes_settings_now()), collapse = "|")))
bayes_store <- reactiveVal(NULL)
observeEvent(input$bayes_fit, disappr_guard("input$bayes_fit", {
  e <- bayes_env()
  if (!isTRUE(e$packages_ok)) { notify(e$message, type = "error", duration = 10); return(invisible(NULL)) }
  sp <- safe_get(bayes_spec_now())
  if (is.null(sp)) { notify("Fit the models in step 5 first, and choose a model on the Set up tab.", type = "error"); return(invisible(NULL)) }
  if (isFALSE(e$compiler)) notify("No C++ compiler was found when you checked: the fit will probably fail. See the Set up tab.", type = "warning", duration = 8)
  z <- bayes_run(list(spec = sp, settings = bayes_settings_now(), cache_dir = bayes_cache_dir))
  z$signature <- bayes_sig()
  z$spec <- sp
  bayes_store(z)
  if (!isTRUE(z$ok)) notify(z$message, type = "error", duration = 12)
}))
bayes_res <- reactive({
  z <- bayes_store()
  shiny::validate(shiny::need(!is.null(z), "Press 'Fit with brms' on the 'Fit and convergence' tab."))
  shiny::validate(shiny::need(isTRUE(z$ok), z$message %||% "The Bayesian fit failed."))
  z
})
bayes_stale <- reactive({ z <- bayes_store(); !is.null(z) && !identical(z$signature, bayes_sig()) })
output$bayes_fit_note <- renderUI({
  z <- bayes_store()
  if (is.null(z)) return(p(class = "small-note", style = "margin-top:8px;", "The first fit of a model compiles it to C++ (about 1–2 minutes); refits with other priors, runs or data start sampling at once."))
  tagList(
    if (isTRUE(bayes_stale())) div(class = "caution-box", style = "margin-top:8px;", "The model, priors or run settings changed since this fit: press 'Fit with brms' again."),
    if (isTRUE(z$ok)) div(class = "small-note", style = "margin-top:8px;",
      p(sprintf("%s: %s chains × %s iterations (%s warm-up), %s posterior draws; %.0f s%s.", z$label, z$settings$chains, format(z$settings$iter, big.mark = ","),
                format(z$settings$warmup, big.mark = ","), format(z$draws, big.mark = ","), z$seconds, if (isTRUE(z$compiled)) ", including compilation" else ", compiled model re-used")),
      if (length(z$warnings)) tags$details(tags$summary(sprintf("%d message(s) from Stan", length(z$warnings))), tags$ul(lapply(z$warnings, tags$li)))))
})

# ---- convergence -----------------------------------------------------------------------------------------------------
bayes_status_class <- function(s) switch(s, good = "prefit-box", caution = "caution-box", "caution-box")
output$bayes_overall_ui <- renderUI({
  z <- bayes_res(); ov <- bayes_overall(z)
  div(class = bayes_status_class(ov$status), strong(switch(ov$status, good = "Converged. ", caution = "Check. ", "Problem. ")), ov$text)
})
bayes_diag_df <- reactive({
  d <- bayes_res()$diagnostics
  d$Status <- c(good = "✔ good", caution = "⚠ check", problem = "✖ problem")[d$Status]
  as.data.frame(d)
})
output$bayes_diag_table <- renderTable(bayes_diag_df(), striped = TRUE, spacing = "xs")
bayes_trace_obj <- reactive({
  z <- bayes_res()
  ggplot(z$trace, aes(iteration, value, colour = chain)) +
    geom_line(alpha = 0.7, linewidth = 0.3) +
    facet_wrap(~parameter, scales = "free_y", ncol = 3) +
    scale_colour_manual(values = distinct_colours(max(2, nlevels(z$trace$chain)))) +
    labs(x = "Iteration after warm-up", y = NULL, colour = "Chain") +
    theme_disappR(12)
})
output$bayes_trace_plot <- renderPlot(bayes_trace_obj())

# ---- results -----------------------------------------------------------------------------------------------------
bayes_compare_df <- reactive({ z <- bayes_res(); bayes_compare_table(z, z$spec) })
output$bayes_reading_ui <- renderUI({
  z <- bayes_res()
  tagList(
    if (isTRUE(bayes_stale())) div(class = "caution-box", "Settings changed since this fit: these results are for the previous settings."),
    tags$ul(lapply(bayes_reading(z, z$spec), tags$li)))
})
output$bayes_compare_table <- renderTable(bayes_compare_df(), striped = TRUE, spacing = "xs")
output$bayes_var_table <- renderTable({ z <- bayes_res(); bayes_variance_table(z, z$spec) }, striped = TRUE, spacing = "xs")
output$bayes_peak_table <- renderTable(bayes_peak_table(bayes_res()), striped = TRUE, spacing = "xs")
bayes_curve_obj <- reactive({
  z <- bayes_res(); sp <- z$spec
  ob <- bayes_obs_means(sp)
  fc <- sp$freq_curve
  g <- ggplot() +
    geom_point(data = ob, aes(age, mean, size = n), shape = 21, fill = "grey60", colour = "black", alpha = 0.8) +
    geom_ribbon(data = z$curve, aes(age, ymin = lo, ymax = hi), fill = "#35637A", alpha = 0.25) +
    geom_line(data = z$curve, aes(age, median), colour = "#35637A", linewidth = 1.2)
  if (isTRUE(input$bayes_show_freq) && is.data.frame(fc) && nrow(fc)) g <- g + geom_line(data = fc, aes(age, fitted), colour = "#C0392B", linewidth = 1, linetype = "dashed")
  g + scale_size_area(max_size = 4, guide = "none") +
    labs(x = "Age", y = bayes_trait_label(sp),
         subtitle = sprintf("Blue: posterior median curve of %s with its 95%% credible band%s. Points: observed means.", sp$label,
                            if (isTRUE(input$bayes_show_freq)) "; red dashed: the step-5 fit" else "")) +
    theme_disappR(13)
})
output$bayes_curve_plot <- renderPlot(bayes_curve_obj())

# ---- posterior predictive checks -------------------------------------------------------------------------------------
output$bayes_ppc_reading <- renderUI(p(bayes_ppc_reading(bayes_res()$ppc)))
bayes_ppc_dist_obj <- reactive({
  z <- bayes_res(); pp <- z$ppc
  shiny::validate(shiny::need(!is.null(pp), "The posterior predictive checks could not be computed."))
  if (isTRUE(pp$discrete)) {
    ggplot(pp$dist, aes(value)) +
      geom_col(aes(y = observed), fill = "grey70", colour = "grey40") +
      geom_pointrange(aes(y = median, ymin = lo, ymax = hi), colour = "#35637A") +
      labs(x = "Value", y = "Proportion of records", subtitle = "Bars: data. Points and lines: replicated data (median, 90% range).") +
      theme_disappR(13)
  } else {
    reps <- data.frame(rep = rep(seq_len(nrow(pp$yrep)), each = ncol(pp$yrep)), y = as.vector(t(pp$yrep)))
    ggplot() +
      geom_density(data = reps, aes(y, group = rep), colour = "#9FC2D6", linewidth = 0.3) +
      geom_density(data = data.frame(y = pp$y), aes(y), colour = "#1B3A4B", linewidth = 1.2) +
      labs(x = bayes_trait_label(z$spec), y = "Density", subtitle = "Dark: data. Light: data sets replicated from the posterior.") +
      theme_disappR(13)
  }
})
output$bayes_ppc_dist_plot <- renderPlot(bayes_ppc_dist_obj())
bayes_ppc_age_obj <- reactive({
  z <- bayes_res(); pp <- z$ppc
  shiny::validate(shiny::need(!is.null(pp), "The posterior predictive checks could not be computed."))
  ggplot(pp$by_age, aes(age)) +
    geom_pointrange(aes(y = median, ymin = lo, ymax = hi), colour = "#9FC2D6", linewidth = 1) +
    geom_point(aes(y = observed, fill = inside), shape = 21, size = 2.6, colour = "black") +
    scale_fill_manual(values = c(`TRUE` = "#1B3A4B", `FALSE` = "#C0392B"), labels = c(`TRUE` = "inside", `FALSE` = "outside"), name = "Observed mean") +
    labs(x = "Age", y = paste("Mean", tolower(bayes_trait_label(z$spec))), subtitle = "Light: mean of the replicated data at each age (median, 90% range).") +
    theme_disappR(13)
})
output$bayes_ppc_age_plot <- renderPlot(bayes_ppc_age_obj())
output$bayes_ppc_stats <- renderTable(bayes_res()$ppc$stats, striped = TRUE, spacing = "xs")

# ---- code, data and methods --------------------------------------------------------------------------------------------
output$bayes_code <- renderText({ z <- bayes_res(); bayes_code(z, z$spec) })
output$bayes_methods_ui <- renderUI({ z <- bayes_res(); p(bayes_methods(z, z$spec)) })
output$download_bayes_code <- downloadHandler(filename = function() "disappR_bayes.R",
  content = function(file) { z <- bayes_res(); writeLines(bayes_code(z, z$spec), file) })
output$download_bayes_data <- downloadHandler(filename = function() "disappR_bayes_data.csv",
  content = function(file) { sp <- bayes_spec_now(); utils::write.csv(sp$data, file, row.names = FALSE) })

# ---- saving to the summary -----------------------------------------------------------------------------------------
bayes_desc <- function(z) {
  s <- z$settings
  sprintf("Bayesian refit of %s with brms (%s family): %s run, %d chains x %s iterations (%s warm-up), prior width %s; %s records, %s individuals.",
          z$label, z$family, s$mode, s$chains, format(s$iter, big.mark = ","), format(s$warmup, big.mark = ","), format_num(bayes_width_value(s$width)),
          format(z$n, big.mark = ","), format(z$n_ind, big.mark = ","))
}
bayes_check_fresh <- function() {
  if (isTRUE(bayes_stale())) { notify("Settings changed since the fit: press 'Fit with brms' again before saving.", type = "warning"); return(FALSE) }
  TRUE
}
bayes_saved_fit <- function() {
  z <- safe_get(bayes_res())
  if (is.null(z)) { nothing_to_save("Fit the model with brms first."); return(NULL) }
  if (!bayes_check_fresh()) return(NULL)
  z
}
observeEvent(input$save_bayes_diag, disappr_guard("input$save_bayes_diag", {
  z <- bayes_saved_fit(); if (is.null(z)) return(invisible(NULL))
  add_saved("8 Bayesian", "Bayesian convergence checks", text = c(bayes_desc(z), bayes_overall(z)$text),
            tables = list(`Convergence checks` = safe_get(bayes_diag_df())[, 1:3]), plots = list(safe_get(bayes_trace_obj())),
            verdict = if (identical(bayes_overall(z)$status, "good")) "context" else "caution")
}))
observeEvent(input$save_bayes_results, disappr_guard("input$save_bayes_results", {
  z <- bayes_saved_fit(); if (is.null(z)) return(invisible(NULL))
  add_saved("8 Bayesian", "Bayesian estimates", text = c(bayes_desc(z), bayes_reading(z, z$spec)),
            tables = list(`Fixed effects: Bayesian and step 5` = safe_get(bayes_compare_df()), `Random-effect and residual SDs` = bayes_variance_table(z, z$spec),
                          `Peak and onset` = bayes_peak_table(z)),
            code = bayes_code(z, z$spec), verdict = "context")
}))
observeEvent(input$save_bayes_curve, disappr_guard("input$save_bayes_curve", {
  z <- bayes_saved_fit(); if (is.null(z)) return(invisible(NULL))
  add_saved("8 Bayesian", "Bayesian trajectory", text = bayes_desc(z), plots = list(safe_get(bayes_curve_obj())), verdict = "context")
}))
observeEvent(input$save_bayes_ppc, disappr_guard("input$save_bayes_ppc", {
  z <- bayes_saved_fit(); if (is.null(z)) return(invisible(NULL))
  add_saved("8 Bayesian", "Bayesian posterior predictive checks", text = c(bayes_desc(z), bayes_ppc_reading(z$ppc)),
            tables = list(`Summary statistics` = z$ppc$stats), plots = list(safe_get(bayes_ppc_dist_obj()), safe_get(bayes_ppc_age_obj())), verdict = "context")
}))
