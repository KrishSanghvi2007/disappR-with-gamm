# Step 7 - GAMMs (optional). Server code, evaluated inside server() (see the end of server.R), so it can read the
# shared data reactives dat(), meta(), data_sig(), truth() and is_toy(), and use the server's helpers (notify,
# run_with_progress, add_saved, nothing_to_save, step4_child). It reads nothing else from steps 1-6 and writes only
# its own outputs (gamm_*), downloads (download_gamm_*) and saved results (section "7 GAMM").

GAMM_TIMEOUT <- 300   # seconds before a GAMM fit or comparison running in a separate R process is stopped

gamm_gd <- reactive(gamm_data(dat(), meta()))
gamm_pretty <- function(x) {
  x <- sub("^s\\(id\\)$", "s(id): individual random intercept", x); x <- sub("^s\\(id,\\.age_c\\)$", "s(id, age): individual random slope", x)
  x <- sub("^s\\(age,id\\)$", "s(age, id): individual random smooths", x)
  x <- gsub("\\bcv_", "", x); x <- gsub("\\bre_", "", x)
  x <- gsub(".terminal", "terminal record ", x, fixed = TRUE); x <- gsub(".age_c", "age (centred)", x, fixed = TRUE)
  x
}
gamm_fmt <- function(x) { if (is.data.frame(x) && "P" %in% names(x)) x$P <- vapply(x$P, format_p, character(1)); x }
gamm_col <- function(n) if (n <= 1) "#35637A" else distinct_colours(n)

# Runs an engine function in a separate R process (as step 4 does), so a slow or crashing fit cannot stop the app;
# in the same process when callr is not installed or under testServer().
gamm_run <- function(what, args) {
  if (!isTRUE(step4_isolated) || !is.function(session$sendProgress)) return(do.call(get(what), args))
  p <- callr::r_bg(step4_child, args = list(engine_dir = step4_engine_dir, files = DISAPPR_ENGINE_FILES, what = what,
                                           args = args, progress_file = ""), supervise = TRUE)
  t0 <- Sys.time()
  while (p$is_alive()) {
    p$wait(250)
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > GAMM_TIMEOUT) {
      p$kill()
      return(list(ok = FALSE, message = sprintf("Stopped after %d seconds. Simplify the model (smaller k, a random intercept instead of individual smooths, no ti term) or choose the bam engine.", GAMM_TIMEOUT)))
    }
  }
  tryCatch(p$get_result(), error = function(e) list(ok = FALSE, message = paste("The separate R process stopped:", conditionMessage(e))))
}

# ---- data note and suggestions ------------------------------------------------------------------------------------
output$gamm_data_note <- renderUI({
  gd <- safe_get(gamm_gd()); m <- safe_get(meta())
  if (is.null(gd) || !nrow(gd)) return(p(class = "small-note", "No data yet: choose or upload data on the Data tab (step 1)."))
  if (!gamm_has_mgcv()) return(div(class = "caution-box", "The GAMM step needs the mgcv package: install.packages(\"mgcv\")."))
  n_unk <- length(unique(gd$id[!is.finite(gd$LS)]))
  tagList(
    fluidRow(
      metric_card("Records", format(nrow(gd), big.mark = ",")),
      metric_card("Individuals", format(nlevels(gd$id), big.mark = ",")),
      metric_card("Ages", sprintf("%s–%s", format_num(min(gd$age)), format_num(max(gd$age))), sprintf("%d distinct", length(unique(gd$age)))),
      metric_card("Records per individual", format_num(stats::median(table(gd$id)), 2), "median")),
    p(class = "small-note", sprintf("The GAMM uses the data mapped on the Data tab. ALR %s; known lifespan %s; AFR %s.%s",
      if (isTRUE(m$alr_mapped)) "from its column" else "is each individual's last recorded age",
      if (all(!is.finite(gd$LS))) "not mapped" else if (n_unk) sprintf("mapped, unknown for %d individual(s)", n_unk) else "mapped for everyone",
      if (isTRUE(m$entry_mapped)) "from its column" else "is each individual's first recorded age",
      if (isTRUE(m$has_trials)) sprintf(" Binomial trials: column %s.", m$trials_col) else "")),
    if (nlevels(gd$id) > GAMM_G4_IND) div(class = "caution-box", sprintf(paste(
      "%s individuals: GAMMs are fitted with gamm4 when the model allows it (Gaussian, Poisson, binomial or Gamma family; random",
      "intercept or slope; one curve or a separate curve per group; no age-dependent proxy). Other models need mgcv::bam, which",
      "with this many individuals can take many minutes and several GB of memory.%s"), format(nlevels(gd$id), big.mark = ","),
      if (gamm_has_gamm4()) "" else " The gamm4 package is not installed: install.packages(\"gamm4\").")))
})
# Suggest the error family (from the data-integrity check of step 1) and the individual random effects when the data
# change; the user can override both. As on the Modelling tab (0.24.8), random slopes are the default only when the data
# support them well; bundled examples keep their published settings (see the end of this file).
observeEvent(data_sig(), disappr_guard("gamm family suggestion", {
  # bundled examples use the family of their published analysis; other data the family suggested for the trait
  ex_fam <- if (identical(input$data_source, "example")) safe_get(current_example()$family) else NULL
  fam <- ex_fam %||% safe_get(integrity()$family$family)
  if (!is.null(fam)) updateSelectInput(session, "gamm_family", selected = gamm_suggest_family(fam))
  if (!identical(input$data_source, "example")) {
    rs <- safe_get(default_random_structure(dat()))
    if (!is.null(rs)) updateSelectInput(session, "gamm_individual", selected = if (identical(rs, "none")) "ri" else "rs")
  }
}), ignoreInit = FALSE)
output$gamm_family_hint <- renderUI({
  fam <- safe_get(integrity()$family$family); m <- safe_get(meta())
  hint <- if (!is.null(fam)) sprintf("Suggested for this trait: %s.", gamm_family_label(gamm_suggest_family(fam))) else NULL
  tri <- if (identical(input$gamm_family, "binomial") && !isTRUE(m$has_trials))
    "Binomial without a trials column: the trait must be 0/1. For proportions, choose the trials column on the Modelling tab (step 5)." else NULL
  if (is.null(hint) && is.null(tri)) return(NULL)
  p(class = "small-note", paste(c(hint, tri), collapse = " "))
})
output$gamm_individual_note <- renderUI({
  gd <- safe_get(gamm_gd()); if (is.null(gd) || !nrow(gd)) return(NULL)
  per <- table(gd$id)
  msg <- if (identical(input$gamm_individual, "fs") && stats::median(per) < 4) "Most individuals have fewer than 4 records: a random smooth per individual is poorly supported; prefer a random intercept and slope."
         else if (identical(input$gamm_individual, "fs") && length(per) > 300) sprintf("A random smooth for each of %s individuals can take several minutes to fit (and longer in the comparisons).", format(length(per), big.mark = ","))
         else if (identical(input$gamm_individual, "none")) "Without an individual random effect, repeated records are treated as independent and intervals are too narrow."
         else if (mean(per >= 3) >= 0.5 && length(per) >= 30) "Many individuals have 3 or more records: consider also a random slope or smooth, and compare the curves."
         else NULL
  if (is.null(msg)) NULL else p(class = "small-note", msg)
})
output$gamm_group_ui <- renderUI({
  gd <- safe_get(gamm_gd()); m <- safe_get(meta())
  ch <- if (is.null(gd) || is.null(m)) character(0) else gamm_group_choices(m, gd)
  if (!length(ch)) return(p(class = "small-note", "Curves per group: map a categorical covariate (such as sex) with 2–8 levels on the Data tab."))
  tagList(
    selectInput("gamm_group", "Separate curves for a group (optional)", c("(none)" = "", ch), isolate(input$gamm_group) %||% ""),
    conditionalPanel("input.gamm_group != ''",
      selectInput("gamm_structure", "Group curves", GAMM_STRUCTURES, isolate(input$gamm_structure) %||% "I")))
})
outputOptions(output, "gamm_group_ui", suspendWhenHidden = FALSE)
output$gamm_truth_ui <- renderUI(if (isTRUE(is_toy())) checkboxInput("gamm_show_truth", "True trajectory (simulation)", TRUE) else NULL)

# ---- settings and fit ---------------------------------------------------------------------------------------------
gamm_settings <- reactive({
  s <- gamm_defaults()
  s$engine <- input$gamm_engine %||% s$engine
  s$method <- input$gamm_method %||% "REML"
  s$family <- input$gamm_family %||% "gaussian"
  s$bs <- input$gamm_bs %||% "tp"
  s$k <- max(3, min(40, round(num_input(input$gamm_k, 10))))
  s$select <- isTRUE(input$gamm_select)
  s$individual <- input$gamm_individual %||% "ri"
  s$group <- input$gamm_group %||% ""
  s$structure <- if (nzchar(s$group)) input$gamm_structure %||% "I" else "G"
  s$proxy <- input$gamm_proxy %||% "ALR"
  s$proxy_form <- if (identical(s$proxy, "none")) "none" else input$gamm_proxy_form %||% "linear"
  s$afr_form <- input$gamm_afr_form %||% "none"
  s$terminal <- isTRUE(input$gamm_terminal)
  s$covars <- !identical(input$gamm_covars, FALSE)
  s$cov_smooth <- isTRUE(input$gamm_cov_smooth)
  s$random <- !identical(input$gamm_random, FALSE)
  s$ar1 <- isTRUE(input$gamm_ar1)
  m <- safe_get(meta())
  s$cov_pairs <- m$cov_pairs %||% character(0)        # interactions mapped on the Data tab
  s$cov_age <- m$cov_age %||% character(0)
  s
})
# The data fingerprint leaves out binomial trials, so the trials column is added here.
gamm_sig <- reactive({
  m <- safe_get(meta())
  paste(safe_get(data_sig()) %||% "", m$trials_col %||% "", paste(unlist(gamm_settings()), collapse = "|"))
})
gamm_store <- reactiveVal(NULL)
gamm_groups_store <- reactiveVal(NULL)
gamm_proxy_store <- reactiveVal(NULL)
gamm_poly_store <- reactiveVal(NULL)
observeEvent(input$gamm_fit, disappr_guard("input$gamm_fit", {
  gd <- safe_get(gamm_gd())
  if (is.null(gd) || !nrow(gd)) { notify("The data are not ready: check the Data tab.", type = "error"); return(invisible(NULL)) }
  s <- gamm_settings()
  r <- withProgress(message = "Fitting the GAMM", value = 0.3, gamm_run("gamm_fit", list(gd = gd, s = s)))
  r$signature <- gamm_sig()
  gamm_store(r)
  gamm_groups_store(NULL); gamm_proxy_store(NULL); gamm_poly_store(NULL)
  if (!isTRUE(r$ok)) notify(r$message, type = "error", duration = 10)
}))
gamm_res <- reactive({
  r <- gamm_store()
  shiny::validate(shiny::need(!is.null(r), "Choose the settings and press 'Fit GAMM' on the Fit tab."))
  shiny::validate(shiny::need(isTRUE(r$ok), r$message %||% "The GAMM could not be fitted."))
  r
})
gamm_stale <- reactive({ r <- gamm_store(); !is.null(r) && !identical(r$signature, gamm_sig()) })
output$gamm_formula_ui <- renderUI({
  r <- gamm_store()
  if (is.null(r)) return(NULL)
  tagList(
    if (isTRUE(gamm_stale())) div(class = "caution-box", style = "margin-top:8px;", "Data or settings changed since this fit: press 'Fit GAMM' again."),
    if (isTRUE(r$ok)) div(class = "small-note", style = "margin-top:8px;",
      p(sprintf("%s records, %s individuals; %s, %s; %.1f s%s.", format(r$n, big.mark = ","), format(r$n_ind, big.mark = ","),
                switch(r$engine, bam = "mgcv::bam", gamm4 = "gamm4", "mgcv::gam"), r$method, r$seconds,
                if (isTRUE(r$rho > 0)) sprintf("; AR1 rho = %s", format_num(r$rho, 2)) else "")),
      tags$code(gamm_pretty(r$formula)),
      if (length(grep("left out|lowered|do not vary|one level", r$notes, value = TRUE)))
        p(paste0("Note: ", paste(grep("left out|lowered|do not vary|one level", r$notes, value = TRUE), collapse = "; "), "."))))
})

# ---- trajectory ---------------------------------------------------------------------------------------------------
gamm_sim <- reactive(!identical(input$gamm_interval, "pointwise"))
gamm_kc <- reactive(gamm_kcheck(gamm_res()))
gamm_cc <- reactive(gamm_concurvity(gamm_res()))
gamm_qr <- reactive(gamm_quantile_residuals(gamm_res()))
gamm_curve_df <- reactive(gamm_curve(gamm_res()))
gamm_deriv_df <- reactive(gamm_derivative(gamm_res()))
gamm_cautions_now <- reactive(gamm_cautions(gamm_res(), safe_get(gamm_kc()) %||% data.frame(), safe_get(gamm_cc()) %||% data.frame(),
                                            safe_get(gamm_qr()), safe_get(gamm_gd())))
output$gamm_reading_ui <- renderUI({
  r <- safe_get(gamm_res()); dv <- safe_get(gamm_deriv_df())
  if (is.null(r) || is.null(dv)) return(NULL)
  div(class = "diagnosis-card", style = "border-left: 5px solid #35637A;",
      div(class = "diagnosis-title", "What the GAMM shows"),
      lapply(gamm_pretty(gamm_reading(r, dv)), function(x) div(class = "diagnosis-detail", x)))
})
output$gamm_cautions_ui <- renderUI({
  cau <- safe_get(gamm_cautions_now())
  if (!length(cau)) return(NULL)
  div(class = "diagnosis-card", style = "border-left: 5px solid #B35806;",
      div(class = "diagnosis-title", badge("caution"), " Check before interpreting"),
      lapply(gamm_pretty(cau), function(x) div(class = "diagnosis-detail", x)))
})
gamm_level_of <- function(r) {
  grp <- r$settings$group %||% ""
  if (nzchar(grp) && !identical(r$structure, "G") && grp %in% names(r$data)) as.character(r$data[[grp]]) else rep("All", nrow(r$data))
}
gamm_curve_obj <- reactive({
  r <- gamm_res(); cv <- gamm_curve_df(); sim <- gamm_sim()
  cv$lo <- if (sim) cv$lower_sim else cv$lower
  cv$hi <- if (sim) cv$upper_sim else cv$upper
  dd <- r$data; dd$level <- gamm_level_of(r)
  by_proxy <- any(nzchar(cv$proxy))
  cv$series <- if (by_proxy) ifelse(cv$level == "All", cv$proxy, paste(cv$level, cv$proxy, sep = ", ")) else cv$level
  n_series <- length(unique(cv$series))
  cols <- stats::setNames(gamm_col(n_series), unique(cv$series))
  p <- ggplot(cv, aes(age, fit))
  if (isTRUE(input$gamm_show_points)) p <- p + geom_point(data = dd, aes(age, trait), inherit.aes = FALSE, alpha = 0.12, size = 0.8, colour = "grey40")
  if (isTRUE(input$gamm_show_means)) {
    om <- stats::aggregate(trait ~ age + level, data = dd, FUN = mean)
    om$n <- stats::aggregate(trait ~ age + level, data = dd, FUN = length)$trait
    p <- p + geom_point(data = om[om$n >= 3, , drop = FALSE], aes(age, trait, shape = level), inherit.aes = FALSE, colour = "grey30", size = 1.8) +
      scale_shape_manual(values = rep(c(1, 2, 0, 5, 6, 3, 4, 8), length.out = length(unique(om$level))), name = NULL, guide = if (length(unique(om$level)) > 1) "legend" else "none")
  }
  p <- p + geom_ribbon(aes(ymin = lo, ymax = hi, fill = series), alpha = 0.18) + geom_line(aes(colour = series), linewidth = 1.1) +
    scale_colour_manual(values = cols, name = NULL) + scale_fill_manual(values = cols, name = NULL)
  tr <- if (isTRUE(is_toy()) && isTRUE(input$gamm_show_truth)) safe_get(truth()) else NULL
  if (!is.null(tr)) {
    ages <- sort(unique(cv$age))
    tc <- safe_get(toy_true_curve(tr, ages))
    if (length(tc) == length(ages)) p <- p + geom_line(data = data.frame(age = ages, fit = tc), aes(age, fit), inherit.aes = FALSE, linetype = 2, colour = "black", linewidth = 0.9)
  }
  p <- p + labs(x = "Age", y = "Trait (population curve)") + theme_disappR(13)
  if (n_series == 1) p <- p + guides(colour = "none", fill = "none")
  p
})
output$gamm_curve_plot <- renderPlot(gamm_curve_obj())
output$gamm_support_plot <- renderPlot({
  r <- gamm_res(); sp <- gamm_support(r)
  ggplot(sp, aes(age, individuals)) + geom_col(fill = "#9DB4C0", width = if (nrow(sp) > 1) 0.8 * min(diff(sp$age)) else 0.8) +
    geom_hline(yintercept = 20, linetype = 3, colour = "#B35806") +
    labs(x = NULL, y = "Individuals") + theme_disappR(10)
})

# ---- ageing rate --------------------------------------------------------------------------------------------------
gamm_deriv_obj <- reactive({
  dv <- gamm_deriv_df(); sim <- gamm_sim()
  dv$lo <- if (sim) dv$lower_sim else dv$lower
  dv$hi <- if (sim) dv$upper_sim else dv$upper
  dv$sig <- ifelse(dv$hi < 0, "Significant decline", ifelse(dv$lo > 0, "Significant increase", "No clear change"))
  p <- ggplot(dv, aes(age, derivative)) + geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
    geom_ribbon(aes(ymin = lo, ymax = hi, group = level), fill = "grey70", alpha = 0.45) +
    geom_line(aes(group = level), colour = "grey25") +
    geom_point(data = dv[dv$sig != "No clear change", , drop = FALSE], aes(colour = sig), size = 1.1) +
    scale_colour_manual(values = c("Significant decline" = "#A50026", "Significant increase" = "#313695"), name = NULL) +
    labs(x = "Age", y = "Rate of change of the trait (slope, link scale)") + theme_disappR(13)
  if (length(unique(dv$level)) > 1) p <- p + facet_wrap(~level)
  p
})
output$gamm_deriv_plot <- renderPlot(gamm_deriv_obj())
gamm_periods_df <- reactive({
  pr <- gamm_periods(gamm_deriv_df(), simultaneous = gamm_sim())
  if (!nrow(pr)) return(data.frame(Note = sprintf("No period of significant change (%s intervals).", if (gamm_sim()) "simultaneous" else "pointwise")))
  pr
})
output$gamm_periods_table <- renderTable(gamm_periods_df(), striped = TRUE, spacing = "xs")
gamm_peak_df <- reactive(gamm_peak_onset(gamm_res()))
output$gamm_peak_table <- renderTable(gamm_peak_df(), striped = TRUE, spacing = "xs")

# ---- checks -------------------------------------------------------------------------------------------------------
gamm_tabs <- reactive(gamm_tables(gamm_res()))
output$gamm_fit_stats <- renderUI({
  tb <- gamm_tabs()
  p(class = "small-note", sprintf("Deviance explained %s; adjusted R² %s; AIC %s; %s records.", if (is.finite(tb$dev_expl)) sprintf("%.1f%%", 100 * tb$dev_expl) else "not available (gamm4)",
                                  format_num(tb$r_sq %||% NA_real_), format_num(tb$aic), format(tb$n, big.mark = ",")))
})
output$gamm_smooth_table <- renderTable({
  st <- gamm_tabs()$smooth
  if (!nrow(st)) return(data.frame(Note = "No smooth terms."))
  st$Smooth <- gamm_pretty(st$Smooth); names(st)[names(st) == "Statistic"] <- gamm_tabs()$stat_name
  gamm_fmt(st)
}, striped = TRUE, spacing = "xs")
output$gamm_param_table <- renderTable({ x <- gamm_tabs()$parametric; x$Term <- gamm_pretty(x$Term); gamm_fmt(x) }, striped = TRUE, spacing = "xs")
output$gamm_kcheck_table <- renderTable({
  kc <- gamm_kc()
  if (!nrow(kc)) return(data.frame(Note = "No basis check available for this model."))
  kc$Smooth <- gamm_pretty(kc$Smooth); gamm_fmt(kc)
}, striped = TRUE, spacing = "xs")
output$gamm_concurvity_table <- renderTable({
  cc <- gamm_cc()
  if (!nrow(cc)) return(data.frame(Note = "Only one fixed smooth term: nothing to compare."))
  cc$Term <- gamm_pretty(cc$Term); cc[["Most similar term"]] <- gamm_pretty(cc[["Most similar term"]]); cc
}, striped = TRUE, spacing = "xs")
output$gamm_resid_note <- renderUI({
  r <- gamm_res(); qr <- safe_get(gamm_qr())
  lag1 <- gamm_lag1(gamm_resid(r), r$data$id)
  p(class = "small-note", paste0(
    sprintf("Lag-1 correlation of residuals within individuals: %s. ", if (is.finite(lag1)) format_num(lag1, 2) else "not available"),
    if (!is.null(qr)) sprintf("Simulated residuals: uniformity test p = %s; dispersion ratio %s (p = %s).", format_p(qr$ks_p), format_num(qr$dispersion, 2), format_p(qr$dispersion_p))
    else "Simulated residuals are not available for this family."))
})
output$gamm_resid_plot <- renderPlot({
  r <- gamm_res(); rr <- gamm_residuals(r); qr <- safe_get(gamm_qr())
  qq <- stats::qqnorm(rr$residual, plot.it = FALSE)
  long <- rbind(data.frame(panel = "1. Residuals against fitted values", x = rr$fitted, y = rr$residual),
                data.frame(panel = "2. Residuals against age", x = rr$age, y = rr$residual),
                data.frame(panel = "3. Normal quantile plot of residuals", x = qq$x, y = qq$y))
  if (!is.null(qr)) {
    u <- sort(qr$u)
    long <- rbind(long, data.frame(panel = "4. Simulated (quantile) residuals: expected vs observed", x = stats::ppoints(length(u)), y = u))
  }
  ref <- rbind(data.frame(panel = c("1. Residuals against fitted values", "2. Residuals against age"), a = 0, b = 0),
               data.frame(panel = "3. Normal quantile plot of residuals", a = 0, b = stats::sd(rr$residual)),
               if (!is.null(qr)) data.frame(panel = "4. Simulated (quantile) residuals: expected vs observed", a = 0, b = 1))
  ggplot(long, aes(x, y)) + geom_point(alpha = 0.3, size = 0.8, colour = "#35637A") +
    geom_abline(data = ref, aes(intercept = a, slope = b), linetype = 2, colour = "grey30") +
    geom_smooth(data = long[long$panel %in% c("1. Residuals against fitted values", "2. Residuals against age"), , drop = FALSE],
                method = "loess", formula = y ~ x, se = FALSE, colour = "#B35806", linewidth = 0.8, span = 0.75) +
    facet_wrap(~panel, scales = "free", ncol = 2) + labs(x = NULL, y = NULL) + theme_disappR(11)
})
observeEvent(input$gamm_run_poly, disappr_guard("input$gamm_run_poly", {
  gd <- safe_get(gamm_gd())
  if (is.null(gd)) return(invisible(NULL))
  res <- withProgress(message = "Fitting linear, quadratic and cubic age", value = 0.3, gamm_run("gamm_parametric_reference", list(gd = gd, s = gamm_settings())))
  if (is.data.frame(res)) attr(res, "signature") <- gamm_sig()
  gamm_poly_store(res)
}))
output$gamm_poly_table <- renderTable({
  z <- gamm_poly_store()
  shiny::validate(shiny::need(!is.null(z), "Press the button to compare the smooth with polynomial age (ML, same records)."))
  shiny::validate(shiny::need(is.data.frame(z), z$message %||% "The comparison could not be run."))
  z
}, striped = TRUE, spacing = "xs")

# ---- selective disappearance --------------------------------------------------------------------------------------
output$gamm_proxy_terms_table <- renderTable({
  r <- gamm_res(); tb <- gamm_tables(r)
  p <- tb$parametric[tb$parametric$Term %in% c("ALR", "LS", "AFR") | grepl("terminal", tb$parametric$Term), , drop = FALSE]
  sm <- tb$smooth[grepl("ALR|LS|AFR", tb$smooth$Smooth), , drop = FALSE]
  rows <- rbind(if (nrow(p)) data.frame(Term = gamm_pretty(p$Term), Type = "linear", `Estimate or edf` = p$Estimate, SE = p$SE, P = p$P, check.names = FALSE),
                if (nrow(sm)) data.frame(Term = sm$Smooth, Type = "smooth (edf)", `Estimate or edf` = sm$edf, SE = NA_real_, P = sm$P, check.names = FALSE))
  if (is.null(rows) || !nrow(rows)) return(data.frame(Note = "The fitted model has no ALR, lifespan, AFR or terminal term."))
  gamm_fmt(rows)
}, striped = TRUE, spacing = "xs")
observeEvent(input$gamm_run_proxy, disappr_guard("input$gamm_run_proxy", {
  gd <- safe_get(gamm_gd()); s <- gamm_settings()
  if (is.null(gd)) return(invisible(NULL))
  res <- withProgress(message = "Comparing proxy forms (up to 8 models)", value = 0.3, gamm_run("gamm_proxy_comparison", list(gd = gd, s = s)))
  res$signature <- gamm_sig()
  gamm_proxy_store(res)
  if (!isTRUE(res$ok)) notify(res$message, type = "warning", duration = 8)
}))
output$gamm_proxy_verdict <- renderUI({
  z <- gamm_proxy_store()
  if (is.null(z) || !isTRUE(z$ok)) return(NULL)
  tagList(
    if (!identical(z$signature, gamm_sig())) div(class = "caution-box", "Settings changed since this comparison: run it again."),
    lapply(names(z$texts), function(p) div(class = "diagnosis-card", style = "border-left: 5px solid #35637A; margin-top:8px;",
                                           div(class = "diagnosis-detail", z$texts[[p]]))))
})
output$gamm_proxy_tables <- renderUI({
  z <- gamm_proxy_store()
  if (is.null(z) || !isTRUE(z$ok)) return(p(class = "small-note", "Fits no, linear, smooth and age-dependent forms of each proxy by ML on the same records, and compares them by AIC."))
  lapply(names(z$tables), function(p) {
    id <- paste0("gamm_proxy_tab_", p)
    output[[id]] <- renderTable(z$tables[[p]][, c("Model", "AIC", "dAIC", "Weight", "edf", "Records", "Status")], striped = TRUE, spacing = "xs")
    tagList(h5(strong(p)), tableOutput(id))
  })
})
gamm_naive_df <- reactive({
  r <- gamm_res()
  shiny::validate(shiny::need(!(identical(r$settings$proxy, "none") && identical(r$afr_form, "none")),
                              "The fitted model has no ALR, lifespan or AFR term: choose one on the Fit tab and refit."))
  nc <- gamm_naive_vs_corrected(gamm_gd(), r)
  shiny::validate(shiny::need(is.data.frame(nc), "The model without proxy terms could not be fitted."))
  nc
})
gamm_naive_obj <- reactive({
  nc <- gamm_naive_df(); sim <- gamm_sim()
  nc$lo <- if (sim) nc$lower_sim else nc$lower
  nc$hi <- if (sim) nc$upper_sim else nc$upper
  cols <- c("Without proxy terms (population curve)" = "#B35806", "With proxy terms (within-individual curve)" = "#35637A")
  nc$model <- factor(nc$model, levels = names(cols))
  p <- ggplot(nc, aes(age, fit, colour = model, fill = model, linetype = model)) + geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, colour = NA) +
    geom_line(linewidth = 1.1) + scale_colour_manual(values = cols, name = NULL) + scale_fill_manual(values = cols, name = NULL) +
    scale_linetype_manual(values = c(1, 2), name = NULL) +
    labs(x = "Age", y = "Trait (population curve)") + theme_disappR(13)
  if (length(unique(nc$level)) > 1) p <- p + facet_wrap(~level)
  p
})
output$gamm_naive_plot <- renderPlot(gamm_naive_obj())

# ---- groups -------------------------------------------------------------------------------------------------------
output$gamm_groups_note <- renderUI({
  if (!nzchar(gamm_settings()$group)) return(p(class = "small-note", "Choose 'Separate curves for a group' on the Fit tab (a categorical covariate such as sex, mapped on the Data tab) to compare trajectories between groups."))
  p(class = "small-note", "Fits structures G, GS, GI, S and I by ML on the same records and compares them by AIC. Can take a minute.")
})
observeEvent(input$gamm_run_groups, disappr_guard("input$gamm_run_groups", {
  gd <- safe_get(gamm_gd()); s <- gamm_settings()
  if (is.null(gd) || !nzchar(s$group)) { notify("Choose a grouping variable on the Fit tab first.", type = "warning"); return(invisible(NULL)) }
  res <- withProgress(message = "Comparing group structures (6 models)", value = 0.3, gamm_run("gamm_group_comparison", list(gd = gd, s = s)))
  res$signature <- gamm_sig()
  gamm_groups_store(res)
  if (!isTRUE(res$ok)) notify(res$message, type = "warning", duration = 8)
}))
gamm_groups_res <- reactive({
  z <- gamm_groups_store()
  shiny::validate(shiny::need(!is.null(z), "Press 'Compare group structures'."))
  shiny::validate(shiny::need(isTRUE(z$ok), z$message %||% "Not available."))
  z
})
output$gamm_groups_verdict <- renderUI({
  z <- safe_get(gamm_groups_res()); if (is.null(z)) return(NULL)
  tagList(if (!identical(z$signature, gamm_sig())) div(class = "caution-box", "Settings changed since this comparison: run it again."),
          div(class = "diagnosis-card", style = "border-left: 5px solid #35637A; margin-top:8px;", div(class = "diagnosis-detail", z$verdict)))
})
output$gamm_groups_table <- renderTable(gamm_groups_res()$table, striped = TRUE, spacing = "xs")
output$gamm_diff_test_table <- renderTable({
  z <- gamm_groups_res()
  if (!nrow(z$differences)) return(data.frame(Note = "Difference smooths could not be fitted."))
  x <- gamm_fmt(z$differences); x$Smooth <- paste0(x$Smooth, " (reference: ", z$reference, ")")
  x
}, striped = TRUE, spacing = "xs")
output$gamm_lrt_table <- renderTable({ z <- gamm_groups_res(); if (is.null(z$lrt)) return(NULL); gamm_fmt(z$lrt) }, striped = TRUE, spacing = "xs")
gamm_diff_df <- reactive(gamm_difference(gamm_res()))
gamm_diff_obj <- reactive({
  dfm <- gamm_diff_df()
  shiny::validate(shiny::need(nrow(dfm) > 0, "Fit the GAMM with separate group curves (structure GS, GI, S or I) on the Fit tab to draw where groups differ."))
  sim <- gamm_sim()
  dfm$lo <- if (sim) dfm$lower_sim else dfm$lower
  dfm$hi <- if (sim) dfm$upper_sim else dfm$upper
  ggplot(dfm, aes(age, diff)) + geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey70", alpha = 0.45) + geom_line(colour = "grey20") +
    facet_wrap(~pair) + labs(x = "Age", y = "Difference between group curves (link scale)") + theme_disappR(12)
})
output$gamm_diff_plot <- renderPlot(gamm_diff_obj())

# ---- individuals --------------------------------------------------------------------------------------------------
output$gamm_vcomp_table <- renderTable({
  vc <- gamm_vcomp(gamm_res())
  if (!nrow(vc)) return(data.frame(Note = "No random-effect variances for this model."))
  vc$Term <- gamm_pretty(sub("^scale$", "residual", vc$Term)); vc
}, striped = TRUE, spacing = "xs")
output$gamm_indiv_plot <- renderPlot({
  r <- gamm_res(); ic <- gamm_individual_curves(r)
  shiny::validate(shiny::need(nrow(ic) > 0, "No individual has three or more records: individual curves need more records per individual."))
  pts <- r$data[as.character(r$data$id) %in% unique(ic$id), , drop = FALSE]
  ggplot(ic, aes(age, fit)) + geom_point(data = pts, aes(age, trait), inherit.aes = FALSE, alpha = 0.6, size = 1, colour = "#35637A") +
    geom_line(colour = "black") + facet_wrap(~id) + labs(x = "Age", y = "Trait (fitted for the individual)") + theme_disappR(11)
})

# ---- code and downloads -------------------------------------------------------------------------------------------
output$gamm_code <- renderText(paste(gamm_code(gamm_res()), collapse = "\n"))
output$download_gamm_code <- downloadHandler(
  filename = function() "disappR_gamm.R",
  content = function(con) writeLines(enc2utf8(gamm_code(gamm_res())), con, useBytes = TRUE))
output$download_gamm_data <- downloadHandler(
  filename = function() "disappR_gamm_data.csv",
  content = function(con) {
    d <- gamm_res()$data
    utils::write.csv(d[, setdiff(names(d), c(".age_c", ".grp_o")), drop = FALSE], con, row.names = FALSE)
  })

# ---- saving to the summary ----------------------------------------------------------------------------------------
gamm_desc <- function(r) {
  s <- r$settings
  sprintf("%s GAMM (%s, %s), k = %s, %s basis; individuals: %s; proxy: %s (%s); AFR: %s; terminal effect: %s; groups: %s; %s records, %s individuals.",
          gamm_family_label(s$family), r$engine, r$method, s$k, s$bs, names(GAMM_INDIVIDUAL)[GAMM_INDIVIDUAL == s$individual],
          s$proxy, r$proxy_form, r$afr_form, if (isTRUE(s$terminal)) "yes" else "no",
          if (nzchar(s$group)) paste(gamm_pretty(s$group), r$structure) else "none", format(r$n, big.mark = ","), format(r$n_ind, big.mark = ","))
}
gamm_check_fresh <- function() {
  if (isTRUE(gamm_stale())) { notify("Data or settings changed since the fit: press 'Fit GAMM' again before saving.", type = "warning"); return(FALSE) }
  TRUE
}
observeEvent(input$save_gamm_curve, disappr_guard("input$save_gamm_curve", {
  r <- safe_get(gamm_res()); pl <- safe_get(gamm_curve_obj())
  if (is.null(r) || is.null(pl)) return(nothing_to_save("Fit a GAMM first."))
  if (!gamm_check_fresh()) return(invisible(NULL))
  add_saved("7 GAMM", "GAMM trajectory", text = c(gamm_desc(r), gamm_pretty(r$formula), gamm_pretty(gamm_reading(r, gamm_deriv_df())),
                                                   gamm_pretty(safe_get(gamm_cautions_now()) %||% character(0))),
            plots = list(pl), code = paste(gamm_code(r), collapse = "\n"), verdict = "context")
}))
observeEvent(input$save_gamm_deriv, disappr_guard("input$save_gamm_deriv", {
  r <- safe_get(gamm_res()); pl <- safe_get(gamm_deriv_obj())
  if (is.null(r) || is.null(pl)) return(nothing_to_save("Fit a GAMM first."))
  if (!gamm_check_fresh()) return(invisible(NULL))
  add_saved("7 GAMM", "GAMM derivatives, peak and onset", text = c(gamm_desc(r), paste("Intervals:", if (gamm_sim()) "simultaneous 95%" else "pointwise 95%")),
            tables = list(`Periods of significant change` = safe_get(gamm_periods_df()), `Peak and onset` = safe_get(gamm_peak_df())),
            plots = list(pl), verdict = "context")
}))
observeEvent(input$save_gamm_check, disappr_guard("input$save_gamm_check", {
  r <- safe_get(gamm_res())
  if (is.null(r)) return(nothing_to_save("Fit a GAMM first."))
  if (!gamm_check_fresh()) return(invisible(NULL))
  tb <- gamm_tabs(); cau <- safe_get(gamm_cautions_now()) %||% character(0)
  st <- tb$smooth; st$Smooth <- gamm_pretty(st$Smooth)
  add_saved("7 GAMM", "GAMM checks", text = c(gamm_desc(r), gamm_pretty(cau)),
            tables = list(`Smooth terms` = gamm_fmt(st), `Parametric terms` = gamm_fmt(tb$parametric),
                          `Basis dimension check` = gamm_fmt(gamm_kc()), Concurvity = gamm_cc()),
            verdict = if (length(cau)) "caution" else "context")
}))
observeEvent(input$save_gamm_groups, disappr_guard("input$save_gamm_groups", {
  z <- safe_get(gamm_groups_res())
  if (is.null(z)) return(nothing_to_save("Compare group structures first."))
  add_saved("7 GAMM", "GAMM group comparison", text = z$verdict,
            tables = list(`Group structures (ML)` = z$table, `Shape differences from the reference group` = gamm_fmt(z$differences)),
            plots = Filter(Negate(is.null), list(safe_get(gamm_diff_obj()))), verdict = "context")
}))
observeEvent(input$save_gamm_proxy, disappr_guard("input$save_gamm_proxy", {
  z <- gamm_proxy_store()
  if (is.null(z) || !isTRUE(z$ok)) return(nothing_to_save("Compare the proxy forms first."))
  v <- if (!identical(gamm_settings()$proxy, "none") && gamm_settings()$proxy %in% names(z$verdicts)) z$verdicts[[gamm_settings()$proxy]] else "context"
  add_saved("7 GAMM", "GAMM selective disappearance", text = unname(z$texts),
            tables = lapply(z$tables, function(t) t[, c("Model", "AIC", "dAIC", "Weight", "edf", "Records")]),
            plots = Filter(Negate(is.null), list(safe_get(gamm_naive_obj()))), verdict = v)
}))
observeEvent(input$save_gamm_poly, disappr_guard("input$save_gamm_poly", {
  z <- gamm_poly_store()
  if (!is.data.frame(z)) return(nothing_to_save("Compare with polynomial age first."))
  add_saved("7 GAMM", "GAMM against polynomials", tables = list(`Smooth against polynomial age (ML)` = z), verdict = "context")
}))

# ---- bundled examples with a published GAMM: set its settings when the example is chosen --------------------------
observeEvent(list(input$data_source, input$example_id), disappr_guard("gamm example settings", {
  if (!identical(input$data_source, "example")) return(invisible(NULL))
  g <- safe_get(current_example()$gamm)
  if (!is.list(g)) return(invisible(NULL))
  if (!is.null(g$k)) updateNumericInput(session, "gamm_k", value = g$k)
  if (!is.null(g$proxy)) updateSelectInput(session, "gamm_proxy", selected = g$proxy)
  if (!is.null(g$proxy_form)) updateSelectInput(session, "gamm_proxy_form", selected = g$proxy_form)
  if (!is.null(g$afr_form)) updateSelectInput(session, "gamm_afr_form", selected = g$afr_form)
  if (!is.null(g$individual)) updateSelectInput(session, "gamm_individual", selected = g$individual)
}), ignoreInit = TRUE)

# ---- guidance: a hint for the proxy settings, and suggested next steps after a fit ---------------------------------
output$gamm_proxy_hint <- renderUI({
  pr <- input$gamm_proxy %||% "ALR"; fm <- input$gamm_proxy_form %||% "linear"
  txt <- c(switch(pr, ALR = "ALR: each individual's age at its last record (or the mapped ALR); keeps every individual.",
                  LS = "Known lifespan: leaves out individuals whose lifespan is unknown (for example still alive).",
                  none = "No proxy: the curve is the population trajectory, which selective disappearance can distort."),
           if (!identical(pr, "none")) switch(fm,
             linear = "Linear: longer- and shorter-lived individuals differ by the same amount at every age (age-independent selective disappearance).",
             smooth = "Smooth: as linear, but the difference can change non-linearly with the proxy.",
             vc = "Age-dependent, linear in the proxy: the age curve tilts with the proxy (the smooth analogue of Model 4).",
             ti = "Age-dependent, smooth in both: the age curve can change shape with the proxy; needs plenty of data and mgcv."))
  p(class = "small-note", paste(txt, collapse = " "))
})
output$gamm_next_ui <- renderUI({
  r <- safe_get(gamm_res()); if (is.null(r)) return(NULL)
  s <- r$settings; dd <- r$data; nxt <- character(0)
  kc <- safe_get(gamm_kc()) %||% data.frame()
  if (nrow(kc) && any(kc$Flag != "")) nxt <- c(nxt, sprintf("Raise k (now %s) and refit: the basis check says the age smooth may be too stiff.", s$k))
  lag1 <- gamm_lag1(gamm_resid(r), dd$id)
  per <- table(dd$id)
  if (is.finite(lag1) && lag1 > 0.3 && identical(s$individual, "ri"))
    nxt <- c(nxt, if (stats::median(per) >= 5) "Try 'Random smooth per individual' or 'Random intercept and slope': residuals of the same individual are correlated, so individuals seem to age differently."
                  else "Try 'Random intercept and slope': residuals of the same individual are correlated, so individuals seem to age at different rates.")
  if (!identical(s$proxy, "none") && is.null(gamm_proxy_store()))
    nxt <- c(nxt, "Open the Selective disappearance tab and press 'Compare proxy forms': it tests whether the proxy matters, and whether its effect changes with age.")
  if (identical(s$proxy, "none")) nxt <- c(nxt, "Choose ALR (or known lifespan) as a proxy: without one the curve can be distorted by selective disappearance.")
  ch <- safe_get(gamm_group_choices(meta(), gamm_gd())) %||% character(0)
  if (length(ch) && !nzchar(s$group %||% "")) nxt <- c(nxt, sprintf("Your data have a grouping variable (%s): choose it under 'Separate curves for a group' to ask whether groups age differently.", paste(names(ch), collapse = ", ")))
  if (nzchar(s$group %||% "") && is.null(gamm_groups_store())) nxt <- c(nxt, "Open the Groups tab and press 'Compare group structures'.")
  if (is.null(gamm_poly_store())) nxt <- c(nxt, "On the Checks tab, compare the smooth with linear, quadratic and cubic age: if a polynomial fits as well, the models of step 5 describe the shape.")
  nxt <- c(nxt, "Read the Ageing rate tab for where the trait changes significantly with age, and save the results you will report.")
  div(class = "diagnosis-card", style = "border-left: 5px solid #7C8060;",
      div(class = "diagnosis-title", "Suggested next steps"),
      lapply(utils::head(nxt, 4), function(x) div(class = "diagnosis-detail", x)))
})
