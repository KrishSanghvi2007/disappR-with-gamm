# Step 7 - GAMMs (optional). Server code, evaluated inside server() (see the end of server.R), so it can read the
# shared data reactives dat(), meta(), data_sig(), truth() and is_toy(), and use the server's helpers (notify,
# run_with_progress, add_saved, nothing_to_save, step4_child). It reads nothing else from steps 1-6 and writes only
# its own outputs (gamm_*), downloads (download_gamm_*) and saved results (section "7 GAMM").

GAMM_TIMEOUT <- 300           # seconds before a single GAMM fit running in a separate R process is stopped
GAMM_COMPARE_TIMEOUT <- 3600  # comparisons fit several models by ML: they get an hour, with the elapsed time shown

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
gamm_run <- function(what, args, timeout = GAMM_TIMEOUT, message = NULL) {
  if (!isTRUE(step4_isolated) || !is.function(session$sendProgress)) return(do.call(get(what), args))
  p <- callr::r_bg(step4_child, args = list(engine_dir = step4_engine_dir, files = DISAPPR_ENGINE_FILES, what = what,
                                           args = args, progress_file = ""), supervise = TRUE)
  t0 <- Sys.time()
  wait <- function() {
    while (p$is_alive()) {
      p$wait(500)
      el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      if (!is.null(message)) setProgress(value = min(0.95, 0.1 + el / 300), detail = sprintf("%d s elapsed; large data can take several minutes.", round(el)))
      if (el > timeout) {
        p$kill()
        return(list(ok = FALSE, message = sprintf("Stopped after %d minutes. Simplify the model (smaller k, a random intercept instead of individual smooths, a linear instead of a ti proxy term) or choose the bam engine.", round(timeout / 60))))
      }
    }
    tryCatch(p$get_result(), error = function(e) list(ok = FALSE, message = paste("The separate R process stopped:", conditionMessage(e))))
  }
  if (is.null(message)) wait() else withProgress(message = message, value = 0.1, wait())
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
      metric_card("Ages", sprintf("%s\u2013%s", format_num(min(gd$age)), format_num(max(gd$age))), sprintf("%d distinct", length(unique(gd$age)))),
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
  if (!length(ch)) return(p(class = "small-note", "Curves per group: map a categorical covariate (such as sex) with 2\u20138 levels on the Data tab."))
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
  s$method <- "REML"
  s$family <- input$gamm_family %||% "gaussian"
  s$bs <- "tp"
  s$k <- max(3, min(40, round(num_input(input$gamm_k, 10))))
  s$select <- FALSE
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
  s$olre <- isTRUE(input$gamm_olre)
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
gamm_sim <- reactive(TRUE)   # simultaneous intervals throughout (Simpson 2018): they support statements about the whole curve
gamm_kc <- reactive(gamm_kcheck(gamm_res()))
gamm_cc <- reactive(gamm_concurvity(gamm_res()))
gamm_qr <- reactive(gamm_quantile_residuals(gamm_res()))
gamm_curve_df <- reactive(gamm_curve(gamm_res(), scale = input$gamm_curve_scale %||% "typical", n_proxy = as.integer(input$gamm_n_proxy %||% 3)))
output$gamm_n_proxy_ui <- renderUI({
  r <- safe_get(gamm_res())
  if (is.null(r) || is.null(gamm_proxy_values(r))) return(NULL)
  selectInput("gamm_n_proxy", sprintf("Curves at how many %s values", r$settings$proxy), c("2" = 2, "3" = 3, "5" = 5), isolate(input$gamm_n_proxy) %||% 3)
})
output$gamm_scale_ui <- renderUI({
  r <- safe_get(gamm_res()); if (is.null(r)) return(NULL)
  lk <- tryCatch(r$fit$family$link %||% r$fit$family$linfo[[1]]$link, error = function(e) "identity")
  if (identical(lk, "identity")) return(NULL)
  tagList(radioButtons("gamm_curve_scale", NULL, c("Typical individual (random effects at zero)" = "typical",
                                                   "Population average (comparable with the observed means)" = "average"),
                       isolate(input$gamm_curve_scale) %||% "typical", inline = TRUE),
          p(class = "small-note", "For counts and proportions the GAMM's curve is for a typical individual (and typical year). When individuals or years differ a lot it lies below the observed means; the population average averages the prediction over the estimated random effects of every record."))
})
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
  r <- gamm_res(); cv <- gamm_curve_df()
  cv$lo <- cv$lower_sim; cv$hi <- cv$upper_sim
  dd <- r$data; dd$level <- gamm_level_of(r)
  grp_lab <- if (nzchar(r$settings$group %||% "")) gamm_pretty(r$settings$group) else NULL
  by_group <- length(unique(cv$level)) > 1
  by_proxy <- any(nzchar(cv$proxy))
  levs <- unique(cv$level)
  cols <- stats::setNames(if (by_group) gamm_col(length(levs)) else "#35637A", levs)
  if (by_proxy) cv$proxy <- factor(cv$proxy, levels = unique(cv$proxy))
  cv$series <- interaction(cv$level, if (by_proxy) cv$proxy else "", drop = TRUE)
  p <- ggplot(cv, aes(age, fit))
  show <- input$gamm_show_data %||% "means"
  if (identical(show, "points")) p <- p + geom_point(data = dd, aes(age, trait, colour = level), inherit.aes = FALSE, alpha = 0.15, size = 0.8)
  if (show %in% c("means", "bins")) {
    if (identical(show, "bins")) {
      nb <- max(3L, min(25L, as.integer(num_input(input$gamm_age_bins, 8))))
      br <- unique(stats::quantile(dd$age, seq(0, 1, length.out = nb + 1), names = FALSE))
      dd$abin <- cut(dd$age, br, include.lowest = TRUE)
      om <- stats::aggregate(cbind(trait, age) ~ abin + level, data = dd, FUN = mean)
      om$n <- stats::aggregate(trait ~ abin + level, data = dd, FUN = length)$trait
      om$se <- stats::aggregate(trait ~ abin + level, data = dd, FUN = function(v) stats::sd(v) / sqrt(length(v)))$trait
      p <- p + geom_errorbar(data = om, aes(x = age, ymin = trait - 1.96 * se, ymax = trait + 1.96 * se, colour = level), inherit.aes = FALSE, width = 0, alpha = 0.6)
    } else {
      om <- stats::aggregate(trait ~ age + level, data = dd, FUN = mean)
      om$n <- stats::aggregate(trait ~ age + level, data = dd, FUN = length)$trait
      om <- om[om$n >= 3, , drop = FALSE]
    }
    p <- p + geom_point(data = om, aes(age, trait, colour = level, shape = level, size = n), inherit.aes = FALSE, alpha = 0.75) +
      scale_size_area(max_size = 3.5, guide = "none") +
      scale_shape_manual(values = rep(c(16, 17, 15, 18, 1, 2, 0, 5), length.out = length(levs)), name = grp_lab, guide = if (by_group) "legend" else "none")
  }
  p <- p + geom_ribbon(aes(ymin = lo, ymax = hi, fill = level, group = series), alpha = 0.10, colour = NA)
  p <- if (by_proxy) p + geom_line(aes(colour = level, linetype = proxy, group = series), linewidth = 1.1) +
                       scale_linetype_manual(values = c("dotted", "solid", "dashed", "dotdash", "longdash")[seq_len(nlevels(cv$proxy))], name = r$settings$proxy)
       else p + geom_line(aes(colour = level, group = series), linewidth = 1.2)
  p <- p + scale_colour_manual(values = cols, name = grp_lab, guide = if (by_group) "legend" else "none") +
    scale_fill_manual(values = cols, guide = "none")
  tr <- if (isTRUE(is_toy()) && isTRUE(input$gamm_show_truth)) safe_get(truth()) else NULL
  if (!is.null(tr)) {
    ages <- sort(unique(cv$age))
    tc <- safe_get(toy_true_curve(tr, ages))
    if (length(tc) == length(ages)) p <- p + geom_line(data = data.frame(age = ages, fit = tc), aes(age, fit), inherit.aes = FALSE, linetype = "longdash", colour = "black", linewidth = 0.8)
  }
  p + labs(x = "Age", y = "Trait (population curve)",
           subtitle = paste0("Lines: fitted curve", if (by_group) " per group (colour)" else "", if (by_proxy) sprintf(" at %d %s values (line type)", nlevels(cv$proxy), r$settings$proxy) else "",
                             "; shaded: simultaneous 95% interval", switch(show, means = "; points: observed means (size = records)", bins = "; points: observed means in age bins with 95% CI", points = "; points: records", ""),
                             if (!is.null(tr)) "; black long-dash: true curve" else "")) +
    theme_disappR(13) + theme(legend.position = "bottom", legend.box = "vertical")
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
  if (!nrow(pr)) return(data.frame(Note = "No period of significant change (simultaneous 95% intervals)."))
  pr
})
output$gamm_periods_table <- renderTable(gamm_periods_df(), striped = TRUE, spacing = "xs")
gamm_peak_df <- reactive(gamm_peak_onset(gamm_res()))
output$gamm_peak_table <- renderTable(gamm_peak_df(), striped = TRUE, spacing = "xs")

# ---- model output -------------------------------------------------------------------------------------------------
gamm_out <- reactive(gamm_output(gamm_res()))
gamm_num_cols <- function(x, digits = 4) {
  for (cl in intersect(c("Estimate", "SE", "Statistic", "edf", "Ref.df", "SD", "Variance", "SD lower 95%", "SD upper 95%"), names(x)))
    x[[cl]] <- vapply(x[[cl]], function(v) if (is.finite(v)) format_num(v, digits) else "", character(1))
  if ("P" %in% names(x)) x$P <- vapply(x$P, format_p, character(1))
  x
}
gamm_out_param_df <- reactive({
  o <- gamm_out(); x <- o$parametric
  x$Term <- gamm_pretty(x$Term); x <- gamm_num_cols(x)
  names(x)[names(x) == "Statistic"] <- paste0(attr(o$parametric, "stat") %||% "t", " value")
  x
})
gamm_out_smooth_df <- reactive({
  o <- gamm_out(); x <- o$smooth
  if (!nrow(x)) return(data.frame(Note = "No fixed smooth terms."))
  x$Smooth <- gamm_pretty(x$Smooth); x <- gamm_num_cols(x, 3)
  names(x)[names(x) == "Statistic"] <- attr(o$smooth, "stat") %||% "F"
  x
})
gamm_out_random_df <- reactive({
  x <- gamm_out()$random
  if (!nrow(x)) return(data.frame(Note = "No random effects in this model."))
  x$Term <- gamm_pretty(sub("^scale$", "residual", x$Term)); x <- gamm_num_cols(x)
  if (all(!nzchar(x[["SD lower 95%"]]))) x[["SD lower 95%"]] <- x[["SD upper 95%"]] <- NULL
  if (!any(nzchar(x$Flag))) x$Flag <- NULL
  x
})
output$gamm_out_stats <- renderTable(gamm_out()$stats, striped = TRUE, spacing = "xs")
output$gamm_out_param <- renderTable(gamm_out_param_df(), striped = TRUE, spacing = "xs")
output$gamm_out_smooth <- renderTable(gamm_out_smooth_df(), striped = TRUE, spacing = "xs")
output$gamm_out_random <- renderTable(gamm_out_random_df(), striped = TRUE, spacing = "xs")
output$gamm_out_formula <- renderUI(tagList(strong("Formula: "), code(gamm_pretty(gamm_res()$formula))))
output$gamm_output_reading <- renderUI(tags$ul(class = "small-note", lapply(gamm_pretty(gamm_output_reading(gamm_out())), tags$li)))
output$gamm_guide_ui <- renderUI({
  r <- gamm_res(); o <- gamm_out()
  fresh <- function(z, sig = if (is.data.frame(z)) attr(z, "signature") else z$signature) if (!is.null(z) && identical(sig, gamm_sig())) z else NULL
  g <- gamm_guide(r, o, poly = fresh(gamm_poly_store()), groups = fresh(gamm_groups_store()), proxy = fresh(gamm_proxy_store()))
  tagList(lapply(g, function(x) div(class = "diagnosis-card", style = "border-left: 5px solid #35637A; margin-bottom: 8px;",
    div(class = "diagnosis-title", x$question),
    div(class = "diagnosis-detail", gamm_pretty(x$answer)),
    if (nzchar(x$next_step)) div(class = "diagnosis-detail small-note", em(x$next_step)))))
})
gamm_re_test_store <- reactiveVal(NULL)
observeEvent(input$gamm_run_re_test, disappr_guard("input$gamm_run_re_test", {
  r <- safe_get(gamm_res())
  if (is.null(r)) { notify("Fit a GAMM first.", type = "warning"); return(invisible(NULL)) }
  z <- gamm_run("gamm_re_test", list(r = r), GAMM_COMPARE_TIMEOUT, "Testing the random effects")
  z$signature <- gamm_sig()
  gamm_re_test_store(z)
}))
output$gamm_re_test_note <- renderUI({
  z <- gamm_re_test_store()
  if (is.null(z)) return(p(class = "small-note", style = "margin-top:8px;", "Optional: tests whether each random effect differs from zero (Wood 2013). The variance components above usually tell you enough."))
  if (!identical(z$signature, gamm_sig())) return(div(class = "caution-box", "Settings changed since the test: run it again."))
  if (!isTRUE(z$ok)) return(div(class = "caution-box", z$message))
  p(class = "small-note", style = "margin-top:8px;", "A small p-value means the random effect explains variation in the trait. Tests of variances against zero are on the boundary of the parameter space, so the p-values are approximate (conservative).")
})
output$gamm_re_test_table <- renderTable({
  z <- gamm_re_test_store()
  if (is.null(z) || !isTRUE(z$ok) || !identical(z$signature, gamm_sig())) return(NULL)
  x <- z$table; x$Term <- gamm_pretty(x$Term); x <- gamm_num_cols(x, 3); names(x)[names(x) == "Statistic"] <- "Chi.sq"; x
}, striped = TRUE, spacing = "xs")

# ---- checks -------------------------------------------------------------------------------------------------------
gamm_check_df <- reactive(gamm_check_summary(gamm_res(), safe_get(gamm_kc()) %||% data.frame(), safe_get(gamm_cc()) %||% data.frame(), safe_get(gamm_qr())))
output$gamm_check_overall <- renderUI({
  ov <- attr(gamm_check_df(), "overall")
  div(class = if (identical(ov, "good")) "prefit-box" else "caution-box",
      strong(switch(ov, good = "No problems found. ", caution = "Check. ", "Problem. ")),
      switch(ov, good = "The basis is large enough, terms are separable, and the residuals show no structure the model misses.",
             caution = "Some checks need attention: see 'What to do'. The results can still be read, with the stated care.",
             "At least one check failed: fix it (see 'What to do') before interpreting the results."))
})
output$gamm_check_summary <- renderTable({
  d <- gamm_check_df()
  d$Status <- unname(c(good = "\u2714 good", caution = "\u26a0 check", problem = "\u2716 problem")[d$Status])
  d$Result <- gamm_pretty(d$Result)
  d
}, striped = TRUE, spacing = "xs")
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
  res <- gamm_run("gamm_parametric_reference", list(gd = gd, s = gamm_settings()), GAMM_COMPARE_TIMEOUT, "Fitting no age, linear, quadratic, cubic and smooth age (ML)")
  if (is.data.frame(res)) attr(res, "signature") <- gamm_sig()
  gamm_poly_store(res)
  if (is.data.frame(res) && identical(attr(res, "engine"), "gamm4")) gamm_fast_disclaimer()
}))
output$gamm_poly_table <- renderTable({
  z <- gamm_poly_store()
  shiny::validate(shiny::need(!is.null(z), "Press the button to compare models with no age term, linear, quadratic and cubic age and the smooth (ML, same records)."))
  shiny::validate(shiny::need(is.data.frame(z), z$message %||% "The comparison could not be run."))
  z
}, striped = TRUE, spacing = "xs")
output$gamm_poly_reading <- renderUI({
  z <- gamm_poly_store()
  if (!is.data.frame(z)) return(NULL)
  if (!identical(attr(z, "signature"), gamm_sig())) return(div(class = "caution-box", "Settings changed since this comparison: run it again."))
  tags$ul(lapply(gamm_poly_reading(z), tags$li))
})
output$gamm_engine_note_poly <- renderUI({
  z <- gamm_poly_store()
  if (is.data.frame(z) && identical(attr(z, "engine"), "gamm4") && identical(attr(z, "signature"), gamm_sig())) return(gamm_fast_box())
  gamm_engine_note("poly")
})

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
  res <- gamm_run("gamm_proxy_comparison", list(gd = gd, s = s), GAMM_COMPARE_TIMEOUT, "Comparing proxy forms (up to 8 models, ML)")
  res$signature <- gamm_sig()
  gamm_proxy_store(res)
  if (!isTRUE(res$ok)) notify(res$message, type = "warning", duration = 8)
  if (isTRUE(res$fast)) gamm_fast_disclaimer()
}))
output$gamm_proxy_verdict <- renderUI({
  z <- gamm_proxy_store()
  if (is.null(z) || !isTRUE(z$ok)) return(NULL)
  tagList(
    if (!identical(z$signature, gamm_sig())) div(class = "caution-box", "Settings changed since this comparison: run it again."),
    if (isTRUE(z$fast)) gamm_fast_box(),
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
  res <- gamm_run("gamm_group_comparison", list(gd = gd, s = s), GAMM_COMPARE_TIMEOUT, "Comparing group structures (6 models, ML)")
  res$signature <- gamm_sig()
  gamm_groups_store(res)
  if (!isTRUE(res$ok)) notify(res$message, type = "warning", duration = 8)
  if (isTRUE(res$fast)) gamm_fast_disclaimer()
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
          if (isTRUE(z$fast)) gamm_fast_box(),
          div(class = "diagnosis-card", style = "border-left: 5px solid #35637A; margin-top:8px;", div(class = "diagnosis-detail", z$verdict)))
})
output$gamm_groups_table <- renderTable({
  x <- gamm_groups_res()$table
  x <- x[, vapply(x, function(v) !all(is.na(v)), logical(1)), drop = FALSE]     # e.g. deviance explained, which gamm4 does not report
  gamm_num_cols(x, 4)
}, striped = TRUE, spacing = "xs")
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
  add_saved("7 GAMM", "GAMM derivatives, peak and onset", text = c(gamm_desc(r), "Intervals: simultaneous 95%."),
            tables = list(`Periods of significant change` = safe_get(gamm_periods_df()), `Peak and onset` = safe_get(gamm_peak_df())),
            plots = list(pl), verdict = "context")
}))
observeEvent(input$save_gamm_output, disappr_guard("input$save_gamm_output", {
  r <- safe_get(gamm_res())
  if (is.null(r)) return(nothing_to_save("Fit a GAMM first."))
  if (!gamm_check_fresh()) return(invisible(NULL))
  o <- gamm_out()
  z <- gamm_re_test_store()
  re_tab <- if (is.list(z) && isTRUE(z$ok) && identical(z$signature, gamm_sig())) { x <- z$table; x$Term <- gamm_pretty(x$Term); gamm_num_cols(x, 3) } else NULL
  add_saved("7 GAMM", "GAMM model output", text = c(gamm_desc(r), gamm_pretty(r$formula), gamm_pretty(gamm_output_reading(o))),
            tables = Filter(Negate(is.null), list(`Model statistics` = o$stats, `Smooth terms` = safe_get(gamm_out_smooth_df()),
                          `Parametric terms` = safe_get(gamm_out_param_df()), `Random effects` = safe_get(gamm_out_random_df()),
                          `Random-effect tests` = re_tab)),
            verdict = "context")
}))
observeEvent(input$save_gamm_check, disappr_guard("input$save_gamm_check", {
  r <- safe_get(gamm_res())
  if (is.null(r)) return(nothing_to_save("Fit a GAMM first."))
  if (!gamm_check_fresh()) return(invisible(NULL))
  cau <- safe_get(gamm_cautions_now()) %||% character(0)
  cs <- safe_get(gamm_check_df())
  add_saved("7 GAMM", "GAMM checks", text = c(gamm_desc(r), gamm_pretty(cau)),
            tables = Filter(Negate(is.null), list(`Model checks` = if (is.data.frame(cs)) cs[, c("Check", "Result", "Status")] else NULL,
                          `Basis dimension check` = gamm_fmt(gamm_kc()), Concurvity = gamm_cc())),
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
  add_saved("7 GAMM", "GAMM against polynomials", text = gamm_poly_reading(z), tables = list(`No age, polynomial and smooth age (ML)` = z), verdict = "context")
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
  updateCheckboxInput(session, "gamm_olre", value = isTRUE(g$olre))
}), ignoreInit = TRUE)

# ---- guidance: a hint for the proxy settings, and suggested next steps after a fit ---------------------------------
output$gamm_proxy_hint <- renderUI({
  pr <- input$gamm_proxy %||% "ALR"; fm <- input$gamm_proxy_form %||% "linear"
  txt <- c(switch(pr, ALR = "ALR: each individual's age at its last record (or the mapped ALR); keeps every individual.",
                  LS = "Known lifespan: leaves out individuals whose lifespan is unknown (for example still alive).",
                  none = "No proxy: the curve is the population trajectory, which selective disappearance can distort."),
           if (!identical(pr, "none")) switch(fm,
             linear = "Linear (like Model 2 of step 5): one number, the difference in the trait per year of the proxy, the same at every age. Age-independent selective disappearance: it shifts the curve's level, not its shape.",
             smooth = "Smooth s(proxy): the trait may depend on the proxy non-linearly (for example only the shortest-lived differ), but still by the same amount at every age. Age-independent, like linear, only more flexible.",
             vc = "Age-dependent, linear in the proxy, s(age, by = proxy): at any given age the trait changes linearly with the proxy, but the size of that change is a smooth function of age. The smooth analogue of Model 4 (age \u00d7 ALR): selection that grows or shrinks with age.",
             ti = "Age-dependent, smooth in both, s(proxy) + ti(age, proxy): the proxy's effect may be non-linear and may change with age in any smooth way, a fully flexible age \u00d7 proxy surface. The most general form: needs plenty of data, is fitted with mgcv only, and can overfit."))
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
  if (is.null(gamm_poly_store())) nxt <- c(nxt, "On the Model output tab, read the step-by-step guide and press 'Is a smooth needed?': it compares no age, linear, quadratic and cubic age with the smooth.")
  nxt <- c(nxt, "Read the Ageing rate tab for where the trait changes significantly with age, and save the results you will report.")
  div(class = "diagnosis-card", style = "border-left: 5px solid #7C8060;",
      div(class = "diagnosis-title", "Suggested next steps"),
      lapply(utils::head(nxt, 4), function(x) div(class = "diagnosis-detail", x)))
})

# ---- disclaimer when comparisons switch to gamm4 (more than GAMM_FAST_LEVELS random-effect levels) -----------------
# Before a comparison: a note under its button says which method it will use. After one fitted with gamm4: a pop-up
# the first time in a session (a short notification afterwards), and a note kept above the results.
gamm_fast_plan <- reactive({
  gd <- safe_get(gamm_gd()); s <- gamm_settings()
  if (is.null(gd) || !nrow(gd)) return(NULL)
  lv <- safe_get(gamm_re_levels(gd, s)) %||% NA_real_
  list(levels = lv,
       proxy = identical(safe_get(gamm_set_engine(gd, s, lapply(c("none", "linear", "smooth", "vc"), function(fm) list(proxy_form = fm, afr_form = fm)), fast_ok = TRUE)), "gamm4"),
       groups = nzchar(s$group) && identical(safe_get(gamm_set_engine(gd, s, lapply(c("G", "GI", "I"), function(st) list(structure = st)), fast_ok = TRUE)), "gamm4"),
       poly = identical(safe_get(gamm_set_engine(gd, s, list(list()), fast_ok = TRUE)), "gamm4"))
})
gamm_engine_note <- function(which) {
  pl <- safe_get(gamm_fast_plan()); if (is.null(pl)) return(NULL)
  if (!isTRUE(pl[[which]]) && is.finite(pl$levels) && pl$levels > gamm_fast_threshold() && identical(gamm_settings()$engine, "auto")) {
    s <- gamm_settings()
    why <- c(if (identical(s$proxy_form, "ti") || identical(s$afr_form, "ti")) "an age-dependent proxy smooth in both (ti)",
             if (s$individual %in% c("fs")) "a random smooth per individual",
             if (!s$family %in% c("gaussian", "poisson", "binomial", "Gamma")) "this error family",
             if (which == "groups" && nzchar(s$group) && !s$structure %in% c("G", "GI", "I")) NULL,
             if (isTRUE(s$ar1)) "AR1 errors", if (isTRUE(s$select)) "shrinkage selection")
    if (which == "groups" && !nzchar(s$group)) return(NULL)
    return(p(class = "small-note", style = "color:#B35806;", icon("hourglass-half"),
      sprintf(" %s random-effect levels: this comparison needs mgcv because the model has %s, and may take a long time. For a fast comparison (gamm4), use 'Age-dependent, linear in the proxy' and a random intercept or slope.",
              format(pl$levels, big.mark = ","), if (length(why)) paste(why, collapse = " and ") else "terms gamm4 cannot fit")))
  }
  if (!isTRUE(pl[[which]])) return(NULL)
  p(class = "small-note", style = "color:#B35806;", icon("bolt"),
    sprintf(" %s random-effect levels: this comparison will be fitted with gamm4, a faster method for many random-effect levels. Results can differ from mgcv; see the note that appears with the results.",
            format(pl$levels, big.mark = ",")))
}
# shown until a comparison is run, and again once its results are out of date (data or settings changed)
gamm_note_due <- function(z) is.null(z) || !identical(z$signature, gamm_sig())
output$gamm_engine_note_proxy <- renderUI(if (gamm_note_due(gamm_proxy_store())) gamm_engine_note("proxy") else NULL)
output$gamm_engine_note_groups <- renderUI(if (gamm_note_due(gamm_groups_store())) gamm_engine_note("groups") else NULL)
gamm_fast_text <- function() {
  lv <- safe_get(gamm_fast_plan()$levels) %||% NA_real_
  tagList(
    p(sprintf("Your data have %s random-effect levels (individuals plus mapped random terms such as year, cohort or observer). With more than %d, comparing models with mgcv can take tens of minutes, so this comparison was fitted with gamm4, which handles random effects much faster.",
              if (is.finite(lv)) format(lv, big.mark = ",") else "many", as.integer(GAMM_FAST_LEVELS))),
    p(strong("What this changes")),
    tags$ul(
      tags$li("Models are compared by gamm4's likelihood, which is more cautious about extra smooth terms. In our simulations it rarely flagged age-dependent selective disappearance that was not there (about 2% of datasets, against 11% with mgcv) and found real effects as often."),
      tags$li("The age-dependent proxy form is s(age, by = proxy): the age curve tilts linearly with the proxy, rather than changing shape freely as with ti(age, proxy)."),
      tags$li("Group structures GS and S cannot be fitted with gamm4 and are left out; G, GI and I are compared.")),
    p("Results can therefore differ from an mgcv comparison of the same data. To use mgcv anyway, choose the bam engine under Advanced options on the Fit tab (this can be slow). The methods draft records which method was used."))
}
gamm_fast_box <- function() div(class = "caution-box", style = "margin-top:8px;", icon("bolt"),
  " Fitted with gamm4 (fast method for many random-effect levels): models compared by gamm4's likelihood; the age-dependent form is s(age, by = proxy); group structures GS and S not fitted. Results can differ from mgcv. ",
  actionLink("gamm_fast_more", "Why?"))
gamm_fast_seen <- reactiveVal(FALSE)
gamm_fast_disclaimer <- function() {
  if (!is.function(session$sendNotification)) return(invisible(NULL))            # not under testServer()
  if (!isTRUE(gamm_fast_seen())) {
    gamm_fast_seen(TRUE)
    showModal(modalDialog(title = "This comparison used a faster method (gamm4)", gamm_fast_text(), easyClose = TRUE,
                          footer = modalButton("OK, understood")))
  } else notify("Fitted with gamm4 (many random-effect levels): see the note above the results.", duration = 6)
}
observeEvent(input$gamm_fast_more, showModal(modalDialog(title = "Why gamm4 was used", gamm_fast_text(), easyClose = TRUE, footer = modalButton("Close"))))
