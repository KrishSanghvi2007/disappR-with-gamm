server <- function(input, output, session) {

  # Every table passes through display_numbers() (0.22.4), so no value smaller than 0.005 is shown as 0.00.
  renderTable <- function(expr, ..., env = parent.frame(), quoted = FALSE) {
    if (!quoted) expr <- substitute(expr)
    shiny::renderTable(bquote(display_numbers(.(expr))), ..., env = env, quoted = TRUE)
  }

  HEAT_COLOURS <- c("Observed" = "#7C8060", "Missed" = "#C0392B", "First record (AFR)" = "#2C6FAC", "Last record (ALR)" = "#E67E22", "AFR = ALR (one occasion)" = "#7B4F9E", "Not expected" = "#FFFFFF")

  # "i" help modals for every section
  lapply(names(INFO), function(key) {
    observeEvent(input[[paste0("info_", key)]], disappr_guard("input[[paste0('info_', key)]]", {
      if (is.function(session$sendModal)) {
        showModal(modalDialog(title = INFO[[key]]$title, info_body(INFO[[key]]), easyClose = TRUE, footer = modalButton("Close")))
      }
    }), ignoreInit = TRUE)
  })

  # Progress bars and notifications only when the session supports them (so the server
  # can also be exercised with shiny::testServer()).
  notify <- function(msg, type = "message", duration = 5) {
    if (is.function(session$sendNotification)) showNotification(msg, type = type, duration = duration)
    invisible(NULL)
  }
  run_with_progress <- function(msg, f) {
    if (is.function(session$sendProgress)) {
      withProgress(message = msg, value = 0, f(function(i, n, lab) incProgress(1 / n, detail = lab)))
    } else {
      f(NULL)
    }
  }


  # ======================================================================
  # 1. Data
  # ======================================================================
  read_toy_inputs <- function() {
    individual <- identical(input$toy_afr_mode, "individual")
    sd_type <- input$toy_sd_type %||% "both"
    sa_type <- if (individual) (input$toy_sa_type %||% "none") else "none"
    list(
      trait = input$toy_trait %||% "mass",
      form = input$toy_form %||% "Quadratic",
      shape = input$toy_shape %||% "default",
      strength = input$toy_strength %||% "dramatic",
      sd_type = sd_type,
      sd_dir = if (identical(sd_type, "none")) 1 else num_input(input$toy_sd_dir, 1),
      rate_var = input$toy_rate_var %||% "low",
      mean_ls = min(30, max(3, round(num_input(input$toy_mean_ls, 20)))),
      missingness = input$toy_missingness %||% "complete",
      afr_mode = input$toy_afr_mode %||% "same",
      sa_type = sa_type,
      sa_dir = if (identical(sa_type, "none")) 1 else num_input(input$toy_sa_dir, 1),
      diet = isTRUE(input$toy_diet), groups = isTRUE(input$toy_groups),
      n_id = min(2000, max(30, round(num_input(input$toy_n, 300)))), seed = round(num_input(input$toy_seed, 42))
    )
  }
  toy_cfg <- reactiveVal(TOY_DEFAULTS)
  observeEvent(input$toy_commit, disappr_guard("input$toy_commit", {
    toy_cfg(read_toy_inputs())
    notify("Simulated dataset ready and used by every tab; refit the models (Modelling tab).", duration = 4)
  }))
  # Shapes offered within each simulated ageing form (biological scenarios); "default" is the form's original shape.
  observeEvent(input$toy_form, disappr_guard("input$toy_form", {
    ch <- toy_shape_choices(input$toy_form)
    cur <- isolate(input$toy_shape)
    updateSelectInput(session, "toy_shape", choices = ch, selected = if (isTRUE(cur %in% ch)) cur else "default")
  }), ignoreInit = TRUE)

  output$toy_status <- renderUI({
    active <- toy_cfg()
    pending <- read_toy_inputs()
    txt <- toy_truth_text(active)
    changed <- !identical(lapply(pending, as.character), lapply(utils::modifyList(TOY_DEFAULTS, active), as.character))
    tagList(
      div(class = "truth-card", strong("Active simulation. "), txt$sim),
      if (changed) div(class = "diagnosis-card", div(class = "diagnosis-detail", strong("Settings changed: "), "press 'Simulate & use this dataset' to apply them.")) else NULL
    )
  })

  raw_data <- reactive({
    src <- input$data_source %||% "toy"
    if (identical(src, "upload")) {
      shiny::validate(shiny::need(input$data_file, "Upload a CSV file on the Data tab to begin."))
      rd <- tryCatch(read_user_csv(input$data_file$datapath, input$data_file$name %||% ""),
                     error = function(e) list(data = NULL, note = paste("The file could not be read:", conditionMessage(e))))
      if (!is.list(rd)) rd <- list(data = NULL, note = "The file could not be read.")
      note <- if (is.character(rd$note) && length(rd$note) == 1 && !is.na(rd$note)) rd$note else "The file could not be read."
      x <- rd$data
      shiny::validate(shiny::need(is.data.frame(x), note))
      shiny::validate(shiny::need(ncol(x) >= 3 && nrow(x) >= 10, paste(note, "The app needs at least 3 columns and 10 rows: check the separator and header.")))
      attr(x, "read_note") <- note
      x
    } else if (identical(src, "example")) {
      ex <- current_example()
      x <- load_example_file(ex$file)
      shiny::validate(shiny::need(!is.null(x), paste0("The bundled dataset (data/", ex$file, ") was not found.")))
      x
    } else {
      simulate_toy_data(toy_cfg())
    }
  })

  current_example <- reactive({
    id <- input$example_id %||% names(EXAMPLES)[[1]]
    if (!id %in% names(EXAMPLES)) id <- names(EXAMPLES)[[1]]
    EXAMPLES[[id]]
  })
  # 0.24.7: a short summary of each study, with the full note folded under 'More details'
  output$example_note <- renderUI({
    ex <- current_example()
    a <- ex$about
    if (is.null(a)) return(div(class = "truth-card", strong("Empirical example. "), ex$note))
    item <- function(k, v) if (!is.null(v) && nzchar(v)) tags$li(strong(paste0(k, ": ")), v) else NULL
    div(class = "truth-card",
        tags$ul(style = "margin: 0; padding-left: 18px;",
                item("Species", a$species), item("Trait", a$trait), item("Default model", a$model),
                item("Covariates", a$covariates), item("Main result", a$finding), item("Other traits", a$other)),
        tags$details(style = "margin-top: 6px;", tags$summary(style = "cursor: pointer;", "More details"),
                     p(class = "small-note", style = "margin-top: 4px;", ex$note)))
  })
  is_toy <- reactive(identical(input$data_source %||% "toy", "toy"))
  truth <- reactive(if (is_toy()) attr(raw_data(), "truth") else NULL)
  # The truth line names the simulation it belongs to, so it is always clear which one is drawn. For counts the
  # simulated form acts on the log scale, which is why a "linear" count trajectory is curved.
  truth_label <- function(tc) {
    if (is.null(tc)) return("True (simulated)")
    if (isTRUE(tc$paper)) return("True (simulated, manuscript scenario)")
    paste0("True (simulated ", tc$form %||% "", if (grepl("^count", tc$type %||% "")) " on the log scale" else "", ")")
  }
  # TRUE when the simulation settings on the Data tab differ from the simulation in use
  toy_settings_pending <- reactive({
    if (!isTRUE(is_toy())) return(FALSE)
    pending <- tryCatch(read_toy_inputs(), error = function(e) NULL)
    if (is.null(pending)) return(FALSE)
    !identical(lapply(pending, as.character), lapply(utils::modifyList(TOY_DEFAULTS, toy_cfg()), as.character))
  })
  truth_pending_note <- function() {
    if (!isTRUE(toy_settings_pending())) return(NULL)
    tc <- truth()
    div(class = "diagnosis-card", style = "border-left: 5px solid #9a6b1e;",
        div(class = "diagnosis-detail",
            strong("Simulation settings not yet applied. "),
            sprintf("The settings on the Data tab have changed, but this plot still shows the simulation in use%s. Press 'Simulate & use this dataset' on the Data tab to apply them.",
                    if (!is.null(tc) && nzchar(tc$form %||% "")) paste0(" (", tc$form, ")") else "")))
  }
  output$truth_pending_a3 <- renderUI(truth_pending_note())
  output$truth_pending_pred <- renderUI(truth_pending_note())

  preset_map <- reactive({
    df <- raw_data()
    switch(input$data_source %||% "toy", toy = toy_mapping(df), example = current_example()$mapping, guess_mapping(df))
  })

  output$mapping_ui <- renderUI({
    df <- raw_data()
    pm <- preset_map()
    cols <- names(df)
    pick <- function(x, fallback = "") if (length(x) == 1 && !is.na(x) && x %in% cols) x else fallback
    pm_cov <- intersect(pm$covars %||% character(0), cols)
    pm_fac <- if (!is.null(pm$cov_factor)) intersect(pm$cov_factor, pm_cov) else pm_cov[!vapply(pm_cov, function(nm) is.numeric(maybe_numeric(df[[nm]])), logical(1))]
    pm_num <- setdiff(pm_cov, pm_fac)
    map_row <- fluidRow(
      column(4,
        selectInput("col_id", "Individual ID *", choices = cols, selected = pick(pm$id, cols[[1]])),
        selectInput("col_age", "Age / time *", choices = cols, selected = pick(pm$age, cols[[min(2, length(cols))]])),
        box_note("Age must be numeric (whole numbers or decimals, e.g. years, days or sampling occasions); it cannot be categorical. Rows without a numeric age are dropped."),
        uiOutput("age_type_note"),
        selectInput("col_trait", "Trait *", choices = cols, selected = pick(pm$trait, cols[[min(3, length(cols))]]))
      ),
      column(4,
        selectInput("col_alr", "ALR (age at last record)", choices = c("Automatic: last age (see 'Trait-specific ALR/AFR')" = "__AUTO_LAST__", cols),
                    selected = pick(pm$alr, "__AUTO_LAST__")),
        selectInput("col_life", "Known lifespan (LS)", choices = c("(not available)" = "", "Automatic: the automatic ALR (proxy only)" = "__AUTO_LAST__", cols),
                    selected = if (identical(pm$life, "__AUTO_LAST__")) "__AUTO_LAST__" else pick(pm$life, "")),
        checkboxInput("exclude_inconsistent", "Exclude individuals who have more than one ALR, lifespan or AFR value. If not excluded, loading stops (see the data integrity check below)",
                      value = identical(pm$inconsistent, "exclude")),
        # 0.24.2: automatic ALR and AFR from the ages with a trait value (ticked) or from every record (unticked);
        # bundled examples open unticked, uploads and simulations ticked
        div(style = "display:flex; align-items:flex-start; gap:6px;",
            checkboxInput("trait_ages", "Trait-specific ALR/AFR (default)", value = !isFALSE(pm$trait_specific_ages)),
            actionLink("info_trait_ages", icon("circle-info"), class = "info-link", style = "margin-top:10px;")),
        uiOutput("trait_ages_note")
      ),
      column(4,
        selectizeInput("col_cov_num", tags$span(tags$b("CONTINUOUS"), " fixed-effect covariates (linear effects)"), choices = cols, selected = pm_num,
                       multiple = TRUE, options = list(placeholder = "e.g. temperature, body size")),
        selectizeInput("col_cov_fac", tags$span(tags$b("CATEGORICAL"), " fixed-effect covariates (factors)"), choices = cols, selected = pm_fac,
                       multiple = TRUE, options = list(placeholder = "e.g. treatment, sex, diet")),
        uiOutput("cov_type_warning"),
        uiOutput("cov_int_ui"),
        box_note("Fixed-effect covariates and their interactions are used in the mixed models on tab 5 (Modelling). Values of a continuous covariate that are not numbers are treated as missing."),
        selectInput("col_group", "Higher-level random effect (group containing individuals, e.g. father)", choices = c("(none)" = "", cols), selected = pick(pm$group, "")),
        selectInput("col_group2", "Next level up (optional: group containing that group, e.g. family)", choices = c("(none)" = "", cols), selected = pick(pm$group2, "")),
        checkboxInput("group_nested", "Nested: individuals within the group (and the group within the next level up)", value = isTRUE(pm$nested)),
        uiOutput("nesting_multi_group_note"),
        selectizeInput("col_random", "Additional random intercepts: + (1 | X)", choices = cols,
                       selected = intersect(pm$random %||% character(0), cols), multiple = TRUE,
                       options = list(placeholder = "e.g. year, observer, replicate")),
        selectInput("col_condition", "Condition / state (optional)", choices = c("(none)" = "", cols), selected = pick(pm$condition, ""))
      )
    )
    read_note <- attr(df, "read_note")
    age_vals <- safe_numeric(df[[pick(pm$age, cols[[min(2, length(cols))]])]])
    min_age <- if (any(is.finite(age_vals))) min(age_vals[is.finite(age_vals)]) else 1
    tagList(
      if (!is.null(read_note)) p(class = "small-note", icon("file-csv"), " ", read_note) else NULL,
      map_row,
      conditionalPanel("input.data_source != 'toy'",
        fluidRow(column(5, numericInput("age_round", info_title("Round ages to multiples of (optional)", "age_round"),
                                        value = if (isTRUE(is.finite(pm$age_round))) pm$age_round else NA, min = 0)))),
      tags$hr(),
      conditionalPanel("input.data_source != 'toy'",
        fluidRow(
          column(7,
            radioButtons("afr_mode", "Age at first record (AFR): when the individual enters the data",
                         choices = c("Automatic: first age (default; see 'Trait-specific ALR/AFR')" = "auto",
                                     "Choose a column" = "column"),
                         selected = if (isTRUE(nzchar(pick(pm$entry, ""))) && !identical(pm$entry, "__AUTO_FIRST__")) "column" else "auto"),
            conditionalPanel("input.afr_mode == 'column'",
              selectInput("col_entry", "Column holding each individual's AFR", choices = cols,
                          selected = pick(pm$entry, cols[[1]]))),
            box_note("If AFR is not the age when the trait is first expressed (AFE), this can be set on the '3 \u00b7 Missingness and proxies' tab, where it only changes the missingness calculation.")
          ),
          column(5,
            selectInput("col_censor", "Censoring indicator (optional)", choices = c("(none)" = "", cols), selected = pick(pm$censor, "")),
            uiOutput("censor_value_ui")
          )
        )
      ),
      conditionalPanel("input.data_source == 'toy'",
        p(class = "small-note", "Simulated data: AFR (age at first observation), censoring and the start of sampling follow the simulation settings on the left."))
    )
  })
  try(outputOptions(output, "mapping_ui", suspendWhenHidden = FALSE), silent = TRUE)
  # a bundled example analysed with trait-specific ALR/AFR no longer matches its documented results (0.24.2)
  output$trait_ages_note <- renderUI({
    alr_col <- !identical(input$col_alr %||% "__AUTO_LAST__", "__AUTO_LAST__")
    afr_col <- identical(input$afr_mode %||% "auto", "column") && !identical(input$data_source, "toy")
    msgs <- c(
      if (alr_col && afr_col) "ALR and AFR both come from columns, so this box changes neither (only an automatic lifespan)."
      else if (alr_col) "ALR comes from a column, so this box changes only AFR."
      else if (afr_col) "AFR comes from a column, so this box changes only ALR.",
      if (identical(input$data_source, "example") && isTRUE(input$trait_ages))
        "Ticked for a bundled example: automatic ALR and AFR now use only ages with a trait value, so results can differ from the documented ones, which use every record.")
    if (!length(msgs)) return(NULL)
    div(class = "small-note", style = "border-left: 3px solid #9a6b1e; padding-left: 7px; margin: 0 0 8px;", lapply(msgs, function(m) div(m)))
  })
  # Immediate checks under the column menus: age must be numeric, and covariates should sit in the right box.
  output$age_type_note <- renderUI({
    df <- safe_get(raw_data())
    col <- input$col_age %||% ""
    if (is.null(df) || !nzchar(col) || !col %in% names(df)) return(NULL)
    msg <- age_type_message(df[[col]], col)
    if (!nzchar(msg)) return(NULL)
    div(class = "small-note", style = "border-left: 3px solid #A50026; padding-left: 7px; margin: 4px 0 8px;", msg)
  })
  # Beside the nesting toggle: how many IDs belong to more than one group, before anything is fitted (0.20.21)
  output$nesting_multi_group_note <- renderUI({
    m <- safe_get(meta())
    n <- as.integer(m$n_multi_group %||% 0L)
    if (!isTRUE(n > 0)) return(NULL)
    ex <- if (length(m$multi_group_examples)) paste0(" (", paste(utils::head(m$multi_group_examples, 3), collapse = "; "),
                                                     if (length(m$multi_group_examples) > 3) "; ..." else "", ")") else ""
    div(style = "margin: -6px 0 10px; padding: 8px 12px; border-radius: 4px; border-left: 6px solid #A50026; background: #fbeaea;",
        strong(sprintf("%d individual ID%s mapped to more than one group%s. ", n, if (n == 1) " is" else "s are", ex)),
        if (isTRUE(input$group_nested))
          "With nesting each of these becomes a separate individual (group/ID), with its own random intercept, ALR, AFR and mean age. That is right when IDs are only unique within a group. If the same animal moved between groups, untick nesting and add the grouping column under 'Additional random intercepts' instead."
        else
          "Without nesting they are treated as one individual across groups. Tick 'Nested' if the same ID means different animals in different groups.",
        " See 'IDs linked to >1 higher-level group' in the data integrity checks below.")
  })

  output$cov_type_warning <- renderUI({
    df <- safe_get(raw_data())
    if (is.null(df)) return(NULL)
    msgs <- covariate_type_warnings(df, input$col_cov_num, input$col_cov_fac)
    if (!length(msgs)) return(NULL)
    div(class = "small-note", style = "border-left: 3px solid #A50026; padding-left: 7px; margin: 4px 0 8px;", lapply(msgs, div))
  })

  output$censor_value_ui <- renderUI({
    df <- raw_data()
    cc <- input$col_censor %||% ""
    if (!nzchar(cc) || !cc %in% names(df)) return(NULL)
    vals <- sort(unique(as.character(df[[cc]][!is.na(df[[cc]])])))
    if (!length(vals)) return(p(class = "small-note", "This column has no values."))
    if (length(vals) > 50) return(p(class = "small-note", "More than 50 distinct values: choose an indicator column (e.g. 0/1)."))
    pm <- isolate(preset_map())
    cur <- isolate(input$censor_value)
    sel <- if (isTRUE(cur %in% vals)) cur else if (isTRUE(pm$censor_value %in% vals)) pm$censor_value else vals[[1]]
    tagList(
      selectInput("censor_value", "Value marking a censored individual", choices = vals, selected = sel),
      box_note("LS is set to missing for censored individuals (e.g. escaped, or alive when the study ended).")
    )
  })
  try(outputOptions(output, "censor_value_ui", suspendWhenHidden = FALSE), silent = TRUE)

  # interactions: any covariate x age, or any pair of covariates ("a|||b"; "a|||age")
  output$cov_int_ui <- renderUI({
    cv <- unique(c(input$col_cov_num, input$col_cov_fac))
    if (!length(cv)) return(NULL)
    ch <- stats::setNames(paste(cv, "age", sep = "|||"), paste(cv, "\u00d7 age"))
    if (length(cv) > 1) {
      pr <- utils::combn(cv, 2)
      ch <- c(ch, stats::setNames(paste(pr[1, ], pr[2, ], sep = "|||"), paste(pr[1, ], "\u00d7", pr[2, ])))
    }
    pm <- isolate(preset_map())
    cur <- isolate(input$col_cov_int)
    pre <- c(if (length(pm$cov_age)) paste(pm$cov_age, "age", sep = "|||") else character(0), pm$cov_int %||% character(0))
    selectizeInput("col_cov_int", "Interactions (optional)", choices = ch, selected = intersect(if (!is.null(cur)) cur else pre, ch),
                   multiple = TRUE, options = list(placeholder = "e.g. diet \u00d7 age, diet \u00d7 sex"))
  })
  try(outputOptions(output, "cov_int_ui", suspendWhenHidden = FALSE), silent = TRUE)

  # ---- optional subset of the rows
  output$subset_ui <- renderUI({
    df <- safe_get(raw_data())
    if (is.null(df)) return(NULL)
    cand <- subset_candidates(df)
    cur <- isolate(input$subset_var)
    # an example opens with its own subset (e.g. one sex, as analysed in the paper) or with none
    if (identical(isolate(input$data_source), "example")) cur <- isolate(current_example())$subset$var %||% ""
    tagList(
      fluidRow(
        column(4, selectInput("subset_var", "Keep only rows where", choices = c("(no subsetting)" = "", cand),
                              selected = if (isTRUE(cur %in% cand)) cur else "")),
        column(8, uiOutput("subset_levels_ui"))
      ),
      uiOutput("subset_status")
    )
  })
  output$subset_levels_ui <- renderUI({
    df <- safe_get(raw_data())
    v <- input$subset_var %||% ""
    if (is.null(df) || !nzchar(v) || !v %in% names(df)) return(p(class = "small-note", style = "margin-top:26px;", "All rows are used."))
    lv <- sort(unique(as.character(df[[v]][!is.na(df[[v]])])))
    cur <- isolate(input$subset_levels)
    ex_sub <- if (identical(isolate(input$data_source), "example")) isolate(current_example())$subset else NULL
    if (!is.null(ex_sub) && identical(ex_sub$var, v) && length(intersect(ex_sub$levels, lv))) cur <- intersect(ex_sub$levels, lv)
    selectizeInput("subset_levels", paste(v, "is one of"), choices = lv, multiple = TRUE,
                   selected = if (length(intersect(cur, lv))) intersect(cur, lv) else lv[[1]])
  })
  output$subset_status <- renderUI({
    df <- safe_get(raw_data())
    du <- safe_get(raw_used())
    if (is.null(df) || is.null(du) || nrow(du) == nrow(df)) return(NULL)
    p(class = "small-note", sprintf("Subset: %s of %s rows are analysed (%s = %s).", format(nrow(du), big.mark = ","), format(nrow(df), big.mark = ","),
                                    input$subset_var, paste(input$subset_levels, collapse = ", ")))
  })
  try(outputOptions(output, "subset_ui", suspendWhenHidden = FALSE), silent = TRUE)
  try(outputOptions(output, "subset_levels_ui", suspendWhenHidden = FALSE), silent = TRUE)
  raw_used <- reactive({
    df <- raw_data()
    v <- input$subset_var %||% ""
    lv <- input$subset_levels %||% character(0)
    if (!nzchar(v) || !v %in% names(df) || !length(lv)) return(df)
    out <- df[!is.na(df[[v]]) & as.character(df[[v]]) %in% lv, , drop = FALSE]
    shiny::validate(shiny::need(nrow(out) >= 10, "The subset keeps fewer than 10 rows: choose more levels."))
    out
  })

  # ---- distribution of any variable
  output$dist_var_ui <- renderUI({
    cols <- names(raw_data())
    cur <- isolate(input$dist_var)
    tr <- preset_map()$trait
    sel <- if (isTRUE(cur %in% cols)) cur else if (isTRUE(tr %in% cols)) tr else cols[[1]]
    selectInput("dist_var", "Variable", choices = cols, selected = sel)
  })
  dist_values <- reactive({
    df <- raw_used()
    v <- input$dist_var
    shiny::validate(shiny::need(isTRUE(v %in% names(df)), "Choose a variable."))
    x <- df[[v]]
    if (identical(input$dist_unit, "individual")) {
      idc <- input$col_id
      shiny::validate(shiny::need(isTRUE(idc %in% names(df)), "Map the individual ID column first."))
      ids <- as.character(df[[idc]])
      ok <- !is.na(ids)
      xn <- safe_numeric(x)
      if (sum(is.finite(xn)) >= 0.8 * sum(!is.na(x) & nzchar(as.character(x)))) {
        x <- as.numeric(tapply(xn[ok], ids[ok], finite_mean))
      } else {
        x <- as.character(tapply(as.character(x)[ok], ids[ok], mode_or_na))
      }
    }
    x
  })
  output$dist_plot <- renderPlot(dist_plot_obj())
  dist_plot_obj <- reactive({
    x <- dist_values()
    v <- input$dist_var
    present <- !is.na(x) & nzchar(as.character(x))
    shiny::validate(shiny::need(any(present), "No non-missing values: check the column mapping on the Data tab."))
    xn <- safe_numeric(x)
    unit <- if (identical(input$dist_unit, "individual")) "individuals" else "rows"
    if (sum(is.finite(xn)) >= 0.8 * sum(present)) {
      xn <- xn[is.finite(xn)]
      ux <- sort(unique(xn))
      sub <- sprintf("n = %d %s; mean %s, median %s, SD %s; %.0f%% zeros; %d missing",
                     length(xn), unit, format_num(mean(xn)), format_num(stats::median(xn)),
                     if (length(xn) > 1) format_num(stats::sd(xn)) else "NA", 100 * mean(xn == 0), sum(!present))
      p <- if (length(ux) <= 30 && all(abs(ux - round(ux)) < 1e-8)) {
        ggplot(data.frame(value = xn), aes(value)) + geom_bar(fill = warm_palette[[2]])
      } else {
        ggplot(data.frame(value = xn), aes(value)) + geom_histogram(bins = 30, fill = warm_palette[[2]], colour = "#FFFDF9")
      }
      p + labs(x = v, y = paste("Number of", unit), subtitle = sub) + theme_disappR(12)
    } else {
      tab <- sort(table(as.character(x[present])), decreasing = TRUE)
      n_lev <- length(tab)
      if (n_lev > 30) tab <- tab[1:30]
      df <- data.frame(level = factor(names(tab), levels = rev(names(tab))), n = as.numeric(tab))
      ggplot(df, aes(level, n)) + geom_col(fill = warm_palette[[2]], width = 0.7) + coord_flip() +
        labs(x = v, y = paste("Number of", unit),
             subtitle = paste0(n_lev, " distinct values", if (n_lev > 30) " (30 most common shown)" else "")) +
        theme_disappR(12)
    }
  })

  current_map <- reactive({
    req(input$col_id, input$col_age, input$col_trait)
    mp <- list(id = input$col_id, age = input$col_age, trait = input$col_trait,
         alr = input$col_alr %||% "__AUTO_LAST__", life = input$col_life %||% "",
         entry = if (identical(input$afr_mode %||% "auto", "column")) (input$col_entry %||% "__AUTO_FIRST__") else "__AUTO_FIRST__",
         condition = input$col_condition %||% "", trials = input$col_trials %||% "",
         covars = unique(c(input$col_cov_num, input$col_cov_fac)), cov_factor = input$col_cov_fac %||% character(0),
         cov_int = input$col_cov_int %||% character(0), group = input$col_group %||% "", group2 = input$col_group2 %||% "",
         nested = isTRUE(input$group_nested), random = input$col_random %||% character(0),
         censor = input$col_censor %||% "", censor_value = input$censor_value %||% "",
         cov_age = character(0),
         inconsistent = if (isTRUE(input$exclude_inconsistent)) "exclude" else "error",
         trait_specific_ages = if (is.null(input$trait_ages)) !isFALSE(safe_get(preset_map())$trait_specific_ages) else isTRUE(input$trait_ages),
         age_round = if (identical(input$data_source, "toy")) NA_real_ else num_input(input$age_round, NA_real_))
    # a second grouping level chosen without the first is used as the first level
    if (!nzchar(mp$group) && nzchar(mp$group2)) {
      mp$group <- mp$group2
      mp$group2 <- ""
    }
    mp
  })

  bundle <- reactive({
    df <- raw_used()
    mp <- current_map()
    shiny::validate(
      shiny::need(all(c(mp$id, mp$age, mp$trait) %in% names(df)), "Map the ID, age and trait columns."),
      shiny::need(length(unique(c(mp$id, mp$age, mp$trait))) == 3, "ID, age and trait must be three different columns.")
    )
    b <- tryCatch(standardise_data(df, mp, input$dup_action %||% "keep"), disappr_integrity_error = function(e) e)
    shiny::validate(shiny::need(!inherits(b, "disappr_integrity_error"),
                                if (inherits(b, "disappr_integrity_error")) conditionMessage(b) else ""))
    # reproducibility bundle: where the data came from, its checksum, and the full mapping
    src <- input$data_source %||% "toy"
    b$meta$source_file <- if (identical(src, "upload")) (input$data_file$name %||% NA_character_)
      else if (identical(src, "example")) paste0("data/", (current_example()$file %||% NA_character_))
      else "simulated in the app"
    b$meta$source_md5 <- if (identical(src, "upload") && !is.null(input$data_file$datapath))
      tryCatch(unname(tools::md5sum(input$data_file$datapath)), error = function(e) NA_character_)
      else if (identical(src, "example"))
        tryCatch({
          f_loaded <- file.path("data", current_example()$file)   # the copy this app read (review finding 4)
          if (!file.exists(f_loaded)) f_loaded <- system.file("app", "data", current_example()$file, package = "disappR")
          unname(tools::md5sum(f_loaded))
        }, error = function(e) NA_character_)
      else NA_character_
    b$meta$n_rows_raw <- nrow(df)
    b$meta$map <- mp
    sv <- input$subset_var %||% ""
    b$meta$subset <- if (nzchar(sv) && length(input$subset_levels)) list(var = sv, levels = input$subset_levels) else NULL
    d <- b$data
    shiny::validate(
      shiny::need(nrow(d) >= 10, paste0("Need at least 10 rows with an ID and a numeric age (found ", nrow(d),
                                 "). Check the age column: dates must be converted to ages, and decimal commas or text codes are not numbers.")),
      shiny::need(length(unique(d$id)) >= 3, "Need at least three individuals: check the ID column."),
      shiny::need(sum(is.finite(d$trait)) >= 10, paste0("Need at least 10 numeric trait values (found ", sum(is.finite(d$trait)),
                                                "): check the trait column for text, units or decimal commas."))
    )
    b
  })
  dat <- reactive(bundle()$data)
  # The sampling interval (0.22.0): inferred from the ages unless the user sets it on the Missingness tab. It travels
  # in meta, so every engine function that takes meta uses it; the others receive it as their 'step' argument.
  age_step_manual <- reactive({
    if (!identical(input$age_step_auto, FALSE)) return(NULL)
    v <- num_input(input$age_step_value, NA_real_)
    if (valid_age_step(v)) v else NULL
  })
  meta <- reactive({
    m <- bundle()$meta
    m$age_step <- age_step_manual()
    m
  })
  imet <- reactive(individual_metrics(dat()))
  integrity <- reactive(data_integrity(dat(), meta()))
  life_known <- reactive(isTRUE(meta()$has_life) && !isTRUE(meta()$life_auto))

  data_sig <- reactive({
    d <- dat()
    m <- meta()
    paste(input$data_source, input$example_id %||% "", nrow(d), length(unique(d$id)), signif(sum(d$age), 12), signif(sum(d$trait, na.rm = TRUE), 12),
          signif(sum(d$alr, na.rm = TRUE), 12), signif(sum(d$life, na.rm = TRUE), 12), signif(sum(d$entry, na.rm = TRUE), 12),
          paste(m$covars, collapse = ","), paste(m$random_terms, collapse = ","), m$has_group, m$nested, m$life_auto,
          m$dup_action, m$n_censored, isTRUE(m$has_group2), !isFALSE(m$trait_specific_ages), if (isTRUE(m$has_group2)) m$map$group2 else "", input$afr_mode %||% "auto", paste(m$cov_age, collapse = ","), paste(m$cov_types, collapse = ","),
          paste(m$cov_pairs, collapse = ","), input$subset_var %||% "",
          paste(input$subset_levels %||% character(0), collapse = ","), sep = "|")
  })

  # Sensible defaults whenever the data change: family and model selection. The data signature is compared with the
  # previous one, so settings that leave the analysed data unchanged (e.g. the binomial weights column chosen on the
  # Modelling tab) keep the user's family and model choices.
  defaults_sig <- reactiveVal(NULL)
  observeEvent(safe_get(data_sig()), disappr_guard("data_sig()", {
    sig <- safe_get(data_sig())
    if (identical(sig, isolate(defaults_sig()))) return(invisible(NULL))
    defaults_sig(sig)
    ex <- if (identical(input$data_source %||% "toy", "example")) current_example() else NULL
    fam <- if (is_toy()) {
      toy_family(toy_cfg()$trait)
    } else if (!is.null(ex$family)) {
      ex$family
    } else {
      integrity()$family$family
    }
    if (!identical(fam, "gaussian") && !HAS_GLMMTMB) fam <- "gaussian"
    updateSelectInput(session, "model_family", selected = fam)
    if (!is.null(ex$age_function)) updateSelectInput(session, "model_age_function", selected = ex$age_function)
    if (!is.null(ex$among)) updateRadioButtons(session, "among_order", selected = ex$among)
    # the random structure the source paper used (0.20.25); examples without one go back to a random intercept. Other
    # data start with random slopes when the data support them well, otherwise a random intercept (0.24.8)
    updateSelectInput(session, "random_structure",
                      selected = if (!is.null(ex)) normalise_slope(ex$random_structure %||% "none") else default_random_structure(safe_get(dat())))
    av <- model_availability(dat(), meta())
    for (mid in MODEL_IDS) {
      on <- if (!is.null(ex$models)) (mid %in% ex$models && isTRUE(av$ok[[mid]])) else isTRUE(av$default[[mid]])
      updateCheckboxInput(session, paste0("use_", mid), value = on)
    }
  }))

  output$download_toy <- downloadHandler(
    filename = function() paste0("disappR_simulated_", toy_cfg()$trait, "_", toy_cfg()$missingness, ".csv"),
    content = function(file) utils::write.csv(simulate_toy_data(toy_cfg()), file, row.names = FALSE)
  )

  output$data_metrics <- renderUI({
    d <- dat()
    im <- imet()
    st <- resolve_age_step(d$age, d$id, age_step_manual())
    fam <- integrity()$family
    tagList(
      metric_card("Rows / individuals", paste0(format(nrow(d), big.mark = ","), " / ", format(nrow(im), big.mark = ",")),
                  paste0(sum(is.finite(d$trait)), " trait values")),
      metric_card("Ages", paste0(format_num(min(d$age)), "\u2013", format_num(max(d$age))),
                  paste0(length(unique(d$age)), " distinct; step ", if (is.finite(st)) format_num(st) else "NA",
                         if (!is.null(age_step_manual())) " (set)" else "")),
      metric_card("Records per individual", format_num(stats::median(im$n_trait)), "median"),
      metric_card("Trait type", fam$kind %||% (if (identical(fam$family, "gaussian")) "Continuous" else "Count"),
                  paste("Suggested:", family_label(fam$family)))
    )
  })

  output$integrity_table <- renderTable({
    integrity()$table
  }, striped = TRUE, spacing = "s")

  output$integrity_extra <- renderUI({
    bad <- integrity()$bad_ls_ids
    if (!length(bad)) return(NULL)
    p(class = "small-note", strong("IDs with LS < last recorded age: "), paste(utils::head(bad, 30), collapse = ", "),
      if (length(bad) > 30) " \u2026" else "")
  })

  # Bundled examples whose data needed a correction say so above the integrity checks.
  output$integrity_example_note <- renderUI({
    if (!identical(input$data_source %||% "toy", "example")) return(NULL)
    note <- current_example()$integrity_note
    if (is.null(note) || !nzchar(note)) return(NULL)
    div(class = "truth-card", style = "margin-bottom:8px;", strong("About these data: "), note)
  })

  output$data_preview <- renderTable(utils::head(raw_data(), 10), striped = TRUE, spacing = "xs")

  # ======================================================================
  # 2. A1-A2 visual diagnosis
  # ======================================================================
  output$visual_proxy_ui <- renderUI({
    ch <- proxy_choices(imet(), meta(), "all")
    labs <- c(ALR = "ALR (age at last record)", `Mean age` = "Mean age of the individual's records", LS = "LS (known lifespan)",
              AFR = "AFR (age at first observation): selective appearance")
    cur <- isolate(input$visual_proxy)
    tagList(
      selectInput("visual_proxy", "Group individuals by (both figures)", choices = stats::setNames(ch, unname(labs[ch])),
                  selected = if (isTRUE(cur %in% ch)) cur else "ALR"),
      if (!"AFR" %in% ch) p(class = "small-note", "AFR does not vary among individuals, so selective appearance cannot be diagnosed with these data.") else NULL
    )
  })
  visual_target <- reactive(if (identical(input$visual_proxy, "AFR")) "appearance" else "disappearance")
  try(outputOptions(output, "visual_proxy_ui", suspendWhenHidden = FALSE), silent = TRUE)

  # Facets: any column with 2-8 distinct values (mapped covariates first); one level per individual
  output$facet_ui <- renderUI({
    df <- safe_get(raw_data())
    m <- safe_get(meta())
    if (is.null(df) || is.null(m)) return(NULL)
    map <- m$map
    excl <- c(map$id, map$age, map$trait, map$alr, map$life, map$entry, map$censor)
    cand <- setdiff(names(df), excl)
    few <- cand[vapply(cand, function(nm) {
      v <- as.character(df[[nm]])
      u <- unique(v[!is.na(v) & nzchar(v)])
      length(u) >= 2 && length(u) <= 8
    }, logical(1))]
    ch <- unique(c(intersect(unname(m$cov_labels), few), few))
    cur <- isolate(input$facet_var)
    selectInput("facet_var", "Facet (separate panels) by", choices = c("(none)" = "", ch),
                selected = if (isTRUE(cur %in% ch)) cur else "")
  })
  try(outputOptions(output, "facet_ui", suspendWhenHidden = FALSE), silent = TRUE)
  # One panel label per individual (the individual's most common level of the chosen column), keyed by the
  # individual IDs the app uses (group + ID when nested).
  facet_map_for <- function(v) {
    if (!nzchar(v %||% "")) return(NULL)
    df <- raw_data()
    m <- meta()
    if (!v %in% names(df)) return(NULL)
    ids <- individual_ids(df, m$map)
    lv <- as.character(df[[v]])
    ok <- !is.na(ids) & nzchar(ids) & !is.na(lv) & nzchar(lv)
    if (!any(ok)) return(NULL)
    per <- tapply(lv[ok], ids[ok], mode_or_na)
    stats::setNames(paste0(v, ": ", as.character(per)), names(per))
  }
  facet_map <- reactive(facet_map_for(input$facet_var %||% ""))
  facet_layer <- function() if (!is.null(facet_map())) facet_wrap(~ facet) else NULL
  facet_text <- function() if (nzchar(input$facet_var %||% "")) paste0("; panels by ", input$facet_var) else ""
  # The trajectory figure (and the bin differences built from it) can have its own panels.
  a1_facet_var <- reactive({
    v <- input$a1_facet %||% "__same__"
    if (identical(v, "__same__")) input$facet_var %||% "" else v
  })
  a1_facet_map <- reactive(facet_map_for(a1_facet_var()))
  a1_facet_layer <- function() if (!is.null(a1_facet_map())) facet_wrap(~ facet) else NULL
  a1_facet_text <- function() if (nzchar(a1_facet_var())) paste0("; panels by ", a1_facet_var()) else ""
  output$a1_facet_ui <- renderUI({
    df <- safe_get(raw_data())
    m <- safe_get(meta())
    if (is.null(df) || is.null(m)) return(NULL)
    map <- m$map
    cand <- setdiff(names(df), c(map$id, map$age, map$trait, map$alr, map$life, map$entry, map$censor))
    n_lev <- vapply(cand, function(nm) {
      v <- as.character(df[[nm]])
      length(unique(v[!is.na(v) & nzchar(v)]))
    }, integer(1))
    cat_cov <- unname(m$cov_labels[names(m$cov_types)[m$cov_types == "categorical"]])
    ch <- unique(c(intersect(cat_cov, cand[n_lev >= 2 & n_lev <= 12]), cand[n_lev >= 2 & n_lev <= 8]))
    cur <- isolate(input$a1_facet) %||% "__same__"
    selectInput("a1_facet", "Panels by (categorical variable)", choices = c("As in the settings above" = "__same__", "(none)" = "", ch),
                selected = if (cur %in% c("__same__", "", ch)) cur else "__same__")
  })
  try(outputOptions(output, "a1_facet_ui", suspendWhenHidden = FALSE), silent = TRUE)

  bins_data_sig <- reactiveVal("")
  # Bins range from 3 to the number of distinct values observed in >= the minimum number of individuals
  observe(disappr_guard("observer", {
    d <- safe_get(dat())
    im <- safe_get(imet())
    px <- safe_get(a1_px())
    if (is.null(d) || is.null(im) || is.null(px)) return(invisible(NULL))
    nmin <- 3
    step <- resolve_age_step(d$age, d$id, age_step_manual())
    mx1 <- max_bins_for(proxy_values(im, px), nmin, step)
    mx2 <- max_bins_for(id_age_means(d)$age, nmin, step)
    mx <- max(mx1, mx2)
    cur <- isolate(input$n_bins) %||% 4
    # A new dataset gets the default: half the average number of time steps per individual, within the range the
    # data allow. Changing the proxy or other settings keeps the user's own choice.
    sig <- paste(nrow(d), length(unique(d$id)), signif(sum(d$age, na.rm = TRUE), 8))
    if (!identical(sig, isolate(bins_data_sig()))) {
      bins_data_sig(sig)
      steps <- mean(tapply(d$age, d$id, function(a) length(unique(a[is.finite(a)]))), na.rm = TRUE)
      if (is.finite(steps)) cur <- round(steps / 2)
    }
    try(updateSliderInput(session, "n_bins", min = 3, max = mx, value = min(max(3, cur), mx)), silent = TRUE)
  }))

  # Which trait scale the visual diagnostics need: raw for continuous traits, log(trait + 1) for counts. On the wrong
  # scale the differences between groups can shrink, vanish or reverse, so the advice is shown prominently.
  output$visual_scale_advice <- renderUI({
    d <- safe_get(dat())
    if (is.null(d) || !nrow(d)) return(NULL)
    fam <- tryCatch(suggest_family(d$trait)$family, error = function(e) NA_character_)
    if (length(fam) != 1 || is.na(fam)) return(NULL)
    count <- fam %in% COUNT_FAMILIES
    ok <- identical(input$trait_scale %||% "raw", if (count) "log1p" else "raw")
    what <- if (count) "counts (for example fecundity)" else if (fam %in% BINOMIAL_FAMILIES) "proportions or 0/1 values" else "continuous (Gaussian-like)"
    advice <- if (ok) {
      if (count) "log(trait + 1) is the right scale for these figures." else "The raw scale is the right scale for these figures."
    } else if (count) {
      "Choose log(trait + 1): on the raw scale, individuals with high counts dominate, and the differences between groups can shrink, vanish or even reverse."
    } else {
      "Choose the raw scale: a log scale distorts the differences between groups of a continuous trait."
    }
    div(style = paste0("margin: 4px 0 10px; padding: 8px 12px; border-radius: 4px; border-left: 6px solid ",
                       if (ok) "#2f6b34" else "#A50026", "; background: ", if (ok) "#eef5ee" else "#fbeaea", ";"),
        sprintf("Trait looks like %s data. Suggested visualisation is on the %s scale, which can be changed in the 'Trait scale' option.",
                if (count) "count" else if (fam %in% BINOMIAL_FAMILIES) "proportion or 0/1" else "continuous",
                if (count) "log(trait + 1)" else "raw"))
  })

  visual_dat <- reactive({
    d <- dat()
    if (identical(input$trait_scale, "log1p")) {
      shiny::validate(shiny::need(all(d$trait[is.finite(d$trait)] > -1), "log(trait + 1) needs trait values above -1: choose the raw scale."))
      d$trait <- log1p(d$trait)
    }
    d
  })
  pick_proxy <- function(value, fallback_order) {
    ch <- proxy_choices(imet(), meta(), "all")
    shiny::validate(shiny::need(length(ch) > 0, "No usable grouping variable: map a categorical covariate on the Data tab."))
    if (isTRUE(value %in% ch)) return(value)
    hit <- intersect(fallback_order, ch)
    if (length(hit)) hit[[1]] else ch[[1]]
  }
  vis_px <- reactive(pick_proxy(input$visual_proxy, c("ALR", "LS", "Mean age", "AFR")))
  a1_px <- reactive(vis_px())
  a2_px <- reactive(vis_px())
  trait_label <- reactive({
    lab <- current_map()$trait
    if (identical(input$trait_scale, "log1p")) paste0("log(", lab, " + 1)") else lab
  })
  a1_bins <- reactive({
    binned_trajectory(visual_dat(), proxy_values(imet(), a1_px()), input$n_bins %||% 4, input$bin_method %||% "quantile",
                      3, facet = a1_facet_map())
  })

  # The observed mean of the trait at each age, across every individual, on the scale the figures use (raw or
  # log(trait + 1)): the data the bins are drawn from, for comparison with the bin means (0.20.23).
  a1_observed <- reactive({
    ia <- id_age_means(visual_dat())
    if (!nrow(ia)) return(NULL)
    fm <- a1_facet_map()
    ia$facet <- if (is.null(fm)) "All" else unname(fm[ia$id])
    ia <- ia[!is.na(ia$facet) & is.finite(ia$age) & is.finite(ia$trait), , drop = FALSE]
    if (!nrow(ia)) return(NULL)
    key <- paste(ia$facet, ia$age, sep = "\r")
    first <- !duplicated(key)
    o <- ia[first, c("facet", "age"), drop = FALSE]
    kk <- key[first]
    o$mean <- as.numeric(tapply(ia$trait, key, mean)[kk])
    o$n <- as.numeric(tapply(ia$trait, key, length)[kk])
    o[order(o$facet, o$age), , drop = FALSE]
  })

  output$a1_plot <- renderPlot(a1_plot_obj())
  a1_plot_obj <- reactive({
    px <- a1_px()
    s <- a1_bins()
    shiny::validate(shiny::need(nrow(s) > 0, "Not enough variation in the proxy, or too few individuals per bin \u00d7 age point: use fewer bins."))
    cols <- ordered_colours(length(levels(s$bin)))
    p <- ggplot(s, aes(age, mean, colour = bin, group = bin))
    obs <- if (isTRUE(input$show_a1_obs %||% TRUE)) safe_get(a1_observed()) else NULL
    if (!is.null(obs) && nrow(obs))                       # drawn first, so the bin lines sit on top
      p <- p + geom_point(data = obs, aes(age, mean, size = n), inherit.aes = FALSE, shape = 21,
                          colour = "grey30", fill = NA, stroke = 0.9, alpha = 0.9)
    if (isTRUE(input$show_se)) p <- p + geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0, alpha = 0.6, na.rm = TRUE)
    p + geom_line(linewidth = 1.1) + geom_point(aes(size = n), alpha = 0.95) +
      scale_colour_manual(values = cols, drop = FALSE,
                          guide = if (isTRUE(input$show_bin_legend %||% TRUE)) "legend" else "none") +
      scale_size_area(max_size = 4.5, guide = "none") +
      labs(x = "Age", y = paste("Mean", trait_label()), colour = paste(px, "bin"),
           title = paste(trait_label(), "across age by", px, "bin"),
           subtitle = paste0("Each line joins bin means of individual \u00d7 age means; point size = number of individuals",
                             if (isTRUE(input$show_a1_obs %||% TRUE)) "; grey open circles: observed means across all individuals" else "")) +
      a1_facet_layer() + theme_disappR(13)
  })

  a2_data <- reactive({
    z <- trait_by_age_bins(visual_dat(), proxy_values(imet(), a2_px()), input$n_bins %||% 4, facet = facet_map())
    shiny::validate(shiny::need(nrow(z) > 0, "No individuals with both trait records and this variable: choose another variable."))
    n_ind <- stats::ave(rep(1, nrow(z)), z$facet, z$age_bin, FUN = length)
    z <- z[n_ind >= 3, , drop = FALSE]
    shiny::validate(shiny::need(nrow(z) > 0, "No age bin has at least 3 individuals: use fewer bins."))
    z
  })
  a2_slopes <- reactive(a2_bin_slopes(a2_data()))
  a2_slope_display <- function(tab) {
    fmt_ci <- function(lo, hi) ifelse(is.finite(lo) & is.finite(hi),
                                      paste0(vapply(lo, format_num, character(1)), " to ", vapply(hi, format_num, character(1))), "NA")
    out <- data.frame(`Age bin` = tab$Age_bin, `Mean age` = signif(tab$Mean_age, 3), N = tab$N,
                      Coefficient = signif(tab$Coefficient, 3), SE = signif(tab$SE, 3), `95% CI` = fmt_ci(tab$Lower_95, tab$Upper_95),
                      r = round(tab$r, 2), P = vapply(tab$P, format_p, character(1)),
                      `Change from previous bin` = signif(tab$Change_from_previous, 3), check.names = FALSE, stringsAsFactors = FALSE)
    if (!is.null(facet_map())) out <- cbind(Panel = tab$Facet, out, stringsAsFactors = FALSE)
    out
  }
  a2_trend_lines <- reactive({
    tab <- a2_slopes()
    tr <- a2_slope_trend(tab)
    px <- a2_px()
    if (!nrow(tr)) return("Too few age bins with estimable coefficients (at least 3 needed) to test for a trend across age.")
    vapply(seq_len(nrow(tr)), function(i) {
      sprintf("%sThe coefficient of %s changes by %s per unit age across %d age bins (inverse-variance weighted trend, p = %s).",
              if (!is.null(facet_map())) paste0(tr$Facet[i], ": ") else "", px, format_num(tr$Change_per_age[i]),
              tr$N_bins[i], format_p(tr$P[i]))
    }, character(1))
  })
  output$a2_plot <- renderPlot(a2_plot_obj())
  a2_plot_obj <- reactive({
    px <- a2_px()
    z <- a2_data()
    cols <- ordered_colours(length(levels(z$age_bin)))
    p <- ggplot(z, aes(proxy, trait, colour = age_bin))
    if (isTRUE(input$a2_points %||% TRUE)) {
      jw <- diff(range(z$proxy, na.rm = TRUE))
      jw <- if (is.finite(jw) && jw > 0) 0.01 * jw else 0
      p <- p + geom_point(alpha = 0.35, size = 1.4, position = position_jitter(width = jw, height = 0, seed = 1))
    }
    p +
      geom_smooth(aes(group = age_bin, fill = age_bin), method = "lm", formula = y ~ x, se = TRUE, level = 0.95, alpha = 0.15,
                  linewidth = 1.2, na.rm = TRUE) +
      scale_colour_manual(values = cols, drop = FALSE,
                          guide = if (isTRUE(input$show_legend_a2 %||% TRUE)) "legend" else "none") +
      scale_fill_manual(values = cols, drop = FALSE, guide = "none") +
      labs(x = px, y = paste("Mean", trait_label(), "within age bin"), colour = "Age",
           title = paste(trait_label(), "against", px, "within age bins"),
           subtitle = if (isTRUE(input$a2_points %||% TRUE)) "Points (jittered): each individual's mean within an age bin; lines: linear fit per age bin with its 95% confidence band" else "Lines: linear fit of the trait on the proxy within each age bin, with 95% confidence bands (points hidden)") +
      labs(caption = safe_get(a2_stats_caption())) +
      facet_layer() + theme_disappR(13) +
      theme(plot.caption = element_text(hjust = 0, size = 11, colour = "#3b2f27", lineheight = 1.15))
  })
  # The slope in each age bin, its change between consecutive bins and the inverse-variance weighted trend of the
  # slopes across age, printed on the figure. (0.20.4 removed the separate change-in-slope figure: a line through a
  # handful of changes, with a very wide band, contradicted what the slopes themselves show.)
  a2_stats_caption <- reactive({
    tab <- safe_get(a2_slopes())
    trend <- safe_get(a2_trend_lines())
    if (is.null(tab) || !nrow(tab)) return(NULL)
    wrap <- function(x) paste(strwrap(paste(x, collapse = " "), width = 115), collapse = "\n")
    if (!is.null(facet_map())) return(if (length(trend)) wrap(trend) else NULL)
    ok <- is.finite(tab$Coefficient)
    slopes <- if (any(ok)) paste0("Slope in each age bin: ", paste(paste0(tab$Age_bin[ok], ": ", vapply(tab$Coefficient[ok], format_num, character(1))), collapse = "; "), ".") else NULL
    ch <- tab$Change_from_previous[is.finite(tab$Change_from_previous)]
    changes <- if (length(ch)) paste0("Change between consecutive bins: ", paste(sprintf("%+.3g", ch), collapse = ", "), ".") else NULL
    wrap(c(slopes, changes, trend))
  })

  bin_diff_data <- reactive({
    s <- a1_bins()
    shiny::validate(shiny::need(nrow(s) > 0, "No bins to compare: use fewer bins (see the trajectory figure)."))
    dz <- bin_differences(s, "successive")
    shiny::validate(shiny::need(nrow(dz) > 0, "No age at which two bins both have 3 individuals: use fewer bins."))
    dz
  })
  bin_diff_trends <- reactive(bin_difference_trends(bin_diff_data(), weighted = isTRUE(input$weight_diff)))
  # one least-squares line of difference against age across all bin pairs (per panel)
  bin_diff_pooled <- reactive({
    dz <- bin_diff_data()
    rows <- lapply(split(dz, dz$facet), function(x) {
      wtd <- isTRUE(input$weight_diff) && "w" %in% names(x)
      if (wtd) x <- x[is.finite(x$w) & x$w > 0, , drop = FALSE]
      if (length(unique(x$age)) < 2) return(NULL)
      fit <- if (wtd) stats::lm(difference ~ age, data = x, weights = w) else stats::lm(difference ~ age, data = x)
      cf <- suppressWarnings(summary(fit)$coefficients)
      if (!"age" %in% rownames(cf)) return(NULL)
      data.frame(facet = x$facet[[1]], x = min(x$age), xend = max(x$age),
                 y = cf[1, 1] + cf["age", 1] * min(x$age), yend = cf[1, 1] + cf["age", 1] * max(x$age),
                 Slope_per_age = cf["age", 1], P = if (nrow(x) > 2 && ncol(cf) >= 4) cf["age", 4] else NA_real_,
                 N_points = nrow(x), stringsAsFactors = FALSE)
    })
    rows <- Filter(Negate(is.null), rows)
    if (length(rows)) do.call(rbind, rows) else data.frame()
  })
  output$bin_diff_plot <- renderPlot(bin_diff_obj())
  # Least-squares lines of the differences against age (one per bin pair, or pooled) with 95% confidence bands.
  bin_diff_bands <- reactive({
    dz <- bin_diff_data()
    pooled <- identical(input$diff_lines, "pooled")
    grp <- if (pooled) dz$facet else paste(dz$facet, as.character(dz$pair), sep = "\r")
    rows <- lapply(split(dz, grp), function(x) {
      bd <- lm_band(x$age, x$difference, w = if (isTRUE(input$weight_diff) && "w" %in% names(x)) x$w else NULL)
      if (!nrow(bd)) return(NULL)
      bd$facet <- x$facet[[1]]
      if (!pooled) bd$pair <- factor(as.character(x$pair[[1]]), levels = levels(dz$pair))
      bd
    })
    rows <- Filter(Negate(is.null), rows)
    if (length(rows)) do.call(rbind, rows) else data.frame()
  })
  bin_diff_obj <- reactive({
    dz <- bin_diff_data()
    ages <- sort(unique(dz$age))
    st <- if (length(ages) > 1) min(diff(ages)) else 1
    np <- length(levels(dz$pair))
    dz$age_plot <- dz$age + (as.integer(dz$pair) - (np + 1) / 2) * st * min(0.6 / np, 0.15)
    bands <- bin_diff_bands()
    pooled <- identical(input$diff_lines, "pooled")
    p <- ggplot(dz, aes(age_plot, difference, colour = pair)) +
      geom_hline(yintercept = 0, linetype = 2, colour = "grey55")
    if (nrow(bands) && pooled) {
      p <- p + geom_ribbon(data = bands, aes(x = x, ymin = lo, ymax = hi), inherit.aes = FALSE, fill = "grey45", alpha = 0.2, na.rm = TRUE) +
        geom_line(data = bands, aes(x = x, y = fit), inherit.aes = FALSE, colour = "black", linewidth = 1.2)
    } else if (nrow(bands)) {
      p <- p + geom_ribbon(data = bands, aes(x = x, ymin = lo, ymax = hi, fill = pair, group = pair), inherit.aes = FALSE, alpha = 0.13, na.rm = TRUE) +
        geom_line(data = bands, aes(x = x, y = fit, colour = pair, group = pair), inherit.aes = FALSE, linewidth = 1)
    }
    weighted <- isTRUE(input$weight_diff) && "w" %in% names(dz) && any(is.finite(dz$w))
    # the points (one difference per age and bin pair) can be hidden to see only the trend lines; the weighting still
    # applies to the lines
    if (isTRUE(input$diff_points %||% TRUE)) {
      p <- if (weighted) p + geom_point(data = dz[is.finite(dz$w), , drop = FALSE], aes(size = w), alpha = 0.9) + scale_size(range = c(1.3, 5.5), guide = "none")
           else p + geom_point(size = 3, alpha = 0.9)
    }
    p +
      scale_colour_manual(values = distinct_colours(np), drop = FALSE,
                          guide = if (isTRUE(input$show_legend_diff %||% TRUE)) "legend" else "none") +
      scale_fill_manual(values = distinct_colours(np), drop = FALSE, guide = "none") +
      scale_x_continuous(breaks = if (length(ages) <= 15) ages else waiver()) +
      labs(x = "Age", y = paste("Difference in mean", trait_label(), "(higher \u2212 lower", a1_px(), "bin)"), colour = NULL,
           title = paste("Difference between", a1_px(), "bins at each age"),
           subtitle = paste0(if (pooled) "Bin numbers as in the trajectory legend; black line: least-squares trend across all pairs, with its 95% confidence band (grey). A rising or falling line indicates age-dependent selection." else "Bin numbers as in the trajectory legend; lines: least-squares trend of each difference against age, with 95% confidence bands. Trends that rise or fall indicate age-dependent selection.",
                             if (weighted) " Points (sized by weight) and lines are weighted by the inverse variance of each difference." else "")) +
      a1_facet_layer() + theme_disappR(13)
  })

  output$toy_card_visual <- renderUI({
    if (!is_toy()) return(NULL)
    txt <- toy_truth_text(toy_cfg())
    NULL
  })

  # ======================================================================
  # 3. Sampling, missingness and proxies
  # ======================================================================
  # The missingness window may open at the age at first trait expression; nothing else in the app uses it.
  miss_start_age <- reactive({
    if (identical(input$miss_start %||% "afr", "afe")) num_input(input$afe_age, NA_real_) else NA_real_
  })
  grid_r <- reactive({
    afe <- miss_start_age()
    if (identical(input$miss_start %||% "afr", "afe")) {
      a <- dat()$age
      a <- a[is.finite(a)]
      lo <- min(0, a)
      shiny::validate(
        shiny::need(isTRUE(is.finite(afe)), "Enter the age at first trait expression (AFE), or count missed occasions from AFR."),
        shiny::need(isTRUE(afe >= lo), sprintf("The age at first trait expression (AFE) cannot be earlier than %s: enter an age within the range of the data (%s to %s).",
                                              format_num(lo), format_num(min(a)), format_num(max(a)))),
        shiny::need(isTRUE(afe <= max(a)), sprintf("The age at first trait expression (AFE, %s) is later than every record (the last is at age %s).",
                                                  format_num(afe), format_num(max(a)))))
    }
    build_missing_grid(dat(), margin = 0,
                       start_mode = if (isTRUE(is.finite(afe))) "same" else "afr",
                       start_age = afe, step = age_step_manual())
  })
  msum <- reactive(missingness_summary(dat(), grid_r(), 0.05, life_known()))

  output$sampling_metrics <- renderUI({
    ms <- msum()
    tagList(
      metric_card("Missing expected occasions", if (is.finite(ms$percent)) sprintf("%.1f%%", ms$percent) else "NA",
                  ms$type, width = 3),
      metric_card(info_title("Detection p", "detection"), if (is.finite(ms$p)) sprintf("%.2f", ms$p) else "NA",
                  "Share of expected occasions with a trait record", width = 3),
      metric_card("r (mean age, ALR)", if (is.finite(ms$r_mean_alr)) sprintf("%.3f", ms$r_mean_alr) else "NA",
                  "Near 1 = the two proxies are interchangeable", width = 3),
      metric_card("r (ALR, LS) / r (mean age, LS)",
                  if (is.finite(ms$r_alr_ls)) sprintf("%.2f / %.2f", ms$r_alr_ls, ms$r_mean_ls) else "LS unknown",
                  "How well each proxy tracks known lifespan", width = 3)
    )
  })


  heat_n <- reactive(min(500, max(20, round(num_input(input$heat_n, 150)))))
  output$heatmap <- renderPlot(heatmap_obj(), height = function() {
    n <- safe_get(min(heat_n(), length(unique(grid_r()$id)))) %||% 150
    max(600, min(1700, round(2.8 * n) + 240))
  })
  output$heatmap_note <- renderUI({
    g <- safe_get(grid_r())
    if (is.null(g) || !nrow(g)) return(NULL)
    n_no <- attr(g, "n_no_trait") %||% 0
    im <- safe_get(imet())
    ids <- unique(g$id)
    n_no_alr <- if (is.null(im)) 0 else sum(!is.finite(im$alr[match(ids, im$id)]))
    # a mapped ALR earlier than a recorded age is an inconsistency in the data, shown rather than hidden
    d <- safe_get(dat())
    n_incons <- 0
    if (!is.null(d) && "alr" %in% names(d)) {
      dd <- d[d$id %in% ids, , drop = FALSE]
      mx <- tapply(dd$age, dd$id, max)
      al <- tapply(dd$alr, dd$id, finite_mean)
      st <- g$step[[1]]
      n_incons <- sum(is.finite(al) & is.finite(mx) & al < mx - st / 2)
    }
    parts <- c(if (n_no > 0) sprintf("%d individual%s with no %s record %s not shown: they enter no model.", n_no, if (n_no == 1) "" else "s",
                                     "trait", if (n_no == 1) "is" else "are"),
               if (n_incons > 0) sprintf("%d individual%s ha%s a recorded age later than %s ALR value, so %s records appear after the ALR marker: check the ALR column.",
                                         n_incons, if (n_incons == 1) "" else "s", if (n_incons == 1) "s" else "ve",
                                         if (n_incons == 1) "its" else "their", if (n_incons == 1) "its" else "their"),
               if (n_no_alr > 0 && identical(input$heat_order %||% "alr", "alr"))
                 sprintf("%d individual%s with no ALR value %s shown at the top.", n_no_alr, if (n_no_alr == 1) "" else "s", if (n_no_alr == 1) "is" else "are"))
    if (!length(parts)) return(NULL)
    p(class = "small-note", paste(parts, collapse = " "))
  })
  heatmap_obj <- reactive({
    g <- grid_r()
    shiny::validate(shiny::need(nrow(g) > 0, "Could not build the sampling grid: check the ages on the Data tab, or set the sampling interval."))
    im <- imet()
    ids <- unique(g$id)
    mi <- match(ids, im$id)
    # ALR and AFR for ordering are the values the models use (after missingness for simulated data, whose
    # missed records are removed); individuals with no ALR value are placed at the top
    # AFR order uses the AFR the grid's window opens at, i.e. the blue tile (0.24.2)
    afr_g <- if ("afr" %in% names(g)) as.numeric(tapply(g$afr, g$id, function(v) v[[1]])[ids]) else im$entry[mi]
    ord <- switch(input$heat_order %||% "alr",
                  alr = ids[order(im$alr[mi], afr_g, na.last = FALSE)],
                  afr = ids[order(afr_g, im$alr[mi], na.last = FALSE)],
                  trait = ids[order(im$trait_mean[mi], im$alr[mi])],
                  id = sort(ids),
                  random = ids[order(stats::runif(length(ids)))])
    n_show <- heat_n()
    if (length(ord) > n_show) ord <- ord[unique(round(seq(1, length(ord), length.out = n_show)))]
    x <- grid_display(g, ord, life_known = isTRUE(life_known()))
    shiny::validate(shiny::need(nrow(x) > 0, "Could not build the sampling grid: check the ages on the Data tab, or set the sampling interval."))
    n_shown <- attr(x, "n_shown") %||% length(ord)
    labs_map <- c("Observed" = "Observed", "Missed" = "Missed (between AFR and ALR, both included)",
                  "First record (AFR)" = "First record (AFR)", "Last record (ALR)" = "Last record (ALR)",
                  "AFR = ALR (one occasion)" = "AFR = ALR (one occasion)",
                  "Not expected" = "Outside the window (before the AFR or after the ALR)")
    x$id_f <- factor(x$id, levels = rev(ord))
    x$status <- factor(x$status, levels = names(HEAT_COLOURS))
    ggplot(x, aes(age, id_f, fill = status)) +
      geom_tile(width = g$step[[1]], colour = "#E9E1D6", linewidth = 0.15) +
      scale_fill_manual(values = HEAT_COLOURS, breaks = names(HEAT_COLOURS), labels = wrap_label(unname(labs_map[names(HEAT_COLOURS)]), 32), drop = FALSE) +
      labs(x = "Age (scheduled occasions)", y = NULL, fill = NULL,
           subtitle = if (n_shown < length(ids)) paste("Showing", n_shown, "of", length(ids), "individuals (evenly spaced in the chosen order)") else NULL) +
      theme_disappR(12) +
      theme(panel.grid = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            panel.background = element_rect(fill = "white", colour = NA), legend.position = "bottom",
            legend.text = element_text(size = 10)) +
      guides(fill = guide_legend(ncol = 2, byrow = TRUE))
  })

  output$missing_by_age <- renderPlot(missing_by_age_obj())
  missing_by_age_obj <- reactive({
    g <- grid_r()
    shiny::validate(shiny::need(nrow(g) > 0, "No sampling grid: check the ages on the Data tab, or set the sampling interval."))
    cv <- coverage_by_age(g)
    sc <- max(cv$Expected) / 100
    ggplot(cv, aes(Age)) +
      geom_col(aes(y = Observed), fill = "#E5D2BE", width = 0.8 * g$step[[1]]) +
      geom_line(aes(y = (100 - Percent_observed) * sc), colour = warm_palette[[1]], linewidth = 1.1) +
      geom_point(aes(y = (100 - Percent_observed) * sc), colour = warm_palette[[1]], size = 2.2) +
      scale_y_continuous("Individuals observed (bars)", sec.axis = sec_axis(~ . / sc, name = "% of expected occasions missing (line)")) +
      labs(x = "Age") + theme_disappR(12)
  })


  output$missing_vs_var <- renderPlot(missing_vs_var_obj())
  missing_vs_var_obj <- reactive({
    g <- grid_r()
    shiny::validate(shiny::need(nrow(g) > 0, "No sampling grid: check the ages on the Data tab, or set the sampling interval."))
    x <- missingness_by_variable(dat(), g, input$miss_var %||% "ALR")
    shiny::validate(shiny::need(nrow(x) > 0, "This variable is not available for these data: choose another."))
    if (identical(x$type[[1]], "categorical")) {
      ggplot(x, aes(stats::reorder(group, missing_rate), 100 * missing_rate)) + geom_col(fill = warm_palette[[1]], width = 0.65) +
        coord_flip() + labs(x = NULL, y = "% missing") + theme_disappR(12)
    } else {
      ggplot(x, aes(value, 100 * missing_rate)) + geom_line(colour = warm_palette[[1]], linewidth = 1) +
        geom_point(colour = warm_palette[[1]], size = 2.2) +
        labs(x = input$miss_var, y = "% missing", subtitle = if (identical(x$type[[1]], "binned")) "Quantile bins of individuals" else NULL) +
        theme_disappR(12)
    }
  })

  # A trait-dependent missingness signal mimics age-dependent selective disappearance, so it is flagged here
  output$missing_trait_note <- renderUI({
    d <- safe_get(msum())
    d <- if (is.null(d)) NULL else d$drivers
    if (is.null(d) || !nrow(d)) return(NULL)
    hit <- d[d$Predictor %in% c("Prior observed trait", "Condition") & d$Signal %in% "Associated", , drop = FALSE]
    if (!nrow(hit)) return(NULL)
    div(class = "diagnosis-card",
        div(class = "diagnosis-detail", strong("Missingness depends on the trait itself "),
            sprintf("(%s: %s, p = %s).", hit$Predictor[[1]], hit$Effect[[1]], format_p(hit$P_value[[1]])),
            " Occasions are missed according to the trait value, so individuals with low values are recorded less often and their records end earlier.",
            " This produces the same model ranking as age-dependent selective disappearance: Models 3, 4 and 5 can have much lower AICs than Model 1 even without selection.",
            " Lower AICs for those models are therefore not, on their own, indicative of selective disappearance here; the trait-before-death figure and this association are worth reporting alongside the model comparison."))
  })

  output$drivers_table <- renderTable({
    d <- msum()$drivers
    if (!nrow(d)) return(data.frame(Note = "Too few missing cells for association tests."))
    d$P_value <- vapply(d$P_value, format_p, character(1))
    d
  }, striped = TRUE, spacing = "xs")

  proxy_im <- reactive({
    im <- imet()
    im[im$n_trait > 0, , drop = FALSE]
  })
  output$afr_alr_plot <- renderPlot({
    d <- safe_get(dat())
    if (is.null(d)) return(NULL)
    im <- individual_metrics(d)
    im <- im[im$n_trait > 0, , drop = FALSE]
    df <- data.frame(AFR = ifelse(is.finite(im$entry), im$entry, im$first_recorded), ALR = im$alr)
    df <- df[is.finite(df$AFR) & is.finite(df$ALR), , drop = FALSE]
    shiny::validate(shiny::need(nrow(df) >= 5, "Fewer than 5 individuals with both AFR and ALR: this check needs more individuals."))
    shiny::validate(shiny::need(stats::sd(df$AFR) > 0, "AFR is the same for every individual, so it cannot be correlated with ALR (no selective appearance can be tested)."))
    r <- stats::cor(df$AFR, df$ALR)
    ggplot(df, aes(AFR, ALR)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55") +
      geom_count(alpha = 0.55, colour = warm_palette[[2]]) +
      geom_smooth(method = "lm", formula = y ~ x, colour = "black", fill = "grey55", alpha = 0.18, linewidth = 1) +
      scale_size_area(max_size = 6, guide = "none") +
      labs(x = "Age at first record (AFR)", y = "Age at last record (ALR)",
           subtitle = sprintf("r = %s; dashed line: 1:1 (points on it were recorded once); solid black line: linear regression with 95%% band", format_num(r))) +
      theme_disappR(12)
  })

  output$afr_alr_table <- renderTable({
    a <- safe_get(afr_alr_agreement(dat(), meta()))
    if (is.null(a) || !isTRUE(a$ok)) return(data.frame(Measure = "AFR vs ALR", Value = if (is.null(a)) "not available" else a$message))
    a$table
  }, striped = TRUE, spacing = "xs", width = "100%")

  output$proxy_plots_ui <- renderUI({
    if (life_known()) {
      fluidRow(column(4, plotOutput("proxy_alr_mean", height = 330)),
               column(4, plotOutput("proxy_alr_ls", height = 330)),
               column(4, plotOutput("proxy_mean_ls", height = 330)))
    } else {
      fluidRow(column(6, plotOutput("proxy_alr_mean", height = 340)),
               column(6, p(class = "small-note", style = "margin-top:20px;",
                           "Lifespan (LS) is not known for these data, so only ALR and mean age are compared. Map a known LS column on the Data tab to add ALR vs LS and mean age vs LS.")))
    }
  })
  proxy_alr_mean_obj <- reactive({
    im <- proxy_im()
    proxy_pair_plot(im$mean_age, im$alr, "Mean age", "ALR")
  })
  proxy_alr_ls_obj <- reactive({
    shiny::validate(shiny::need(life_known(), "Known lifespan is not mapped: map an LS column on the Data tab."))
    im <- proxy_im()
    proxy_pair_plot(im$lifespan, im$alr, "LS", "ALR")
  })
  proxy_mean_ls_obj <- reactive({
    shiny::validate(shiny::need(life_known(), "Known lifespan is not mapped: map an LS column on the Data tab."))
    im <- proxy_im()
    proxy_pair_plot(im$lifespan, im$mean_age, "LS", "Mean age")
  })
  output$proxy_alr_mean <- renderPlot(proxy_alr_mean_obj())
  output$proxy_alr_ls <- renderPlot(proxy_alr_ls_obj())
  output$proxy_mean_ls <- renderPlot(proxy_mean_ls_obj())

  a3_n_par <- function() {
    fn <- input$a3_function %||% "Quadratic"
    if (identical(fn, A3_NONLINEAR)) 2 else ncol(age_basis(1:3, make_age_params(1:10, fn, FALSE))) + 1
  }

  a3_ids <- reactive({
    im <- imet()
    im <- im[im$n_trait > 0 & im$id %in% unique(a3_data()$id), , drop = FALSE]
    k <- min(nrow(im), input$a3_n %||% 20)
    switch(input$a3_order %||% "random",
           longest = im$id[order(-im$alr)][seq_len(k)],
           shortest = im$id[order(im$alr)][seq_len(k)],
           im$id[order(stats::runif(nrow(im)))][seq_len(k)])
  })

  # Count traits are fitted on the log scale, the scale on which they are simulated and modelled; other traits on
  # the raw scale. "Automatic" decides from the trait, as the suggested error family does.
  a3_link <- reactive({
    sc <- input$a3_scale %||% "auto"
    if (sc %in% c("identity", "log")) return(sc)
    d <- safe_get(dat())
    if (is.null(d) || !nrow(d)) return("identity")
    fam <- tryCatch(suggest_family(d$trait)$family, error = function(e) "gaussian")
    if (isTRUE(fam %in% COUNT_FAMILIES)) "log" else "identity"
  })
  a3_min_records <- reactive({
    v <- suppressWarnings(as.numeric(input$a3_min_records))
    if (length(v) != 1 || !is.finite(v) || v <= 0) NULL else round(v)
  })
  # ---- Step 4 computation (0.20.5) ----
  # Each function's individual fits are cached (by data, function, scale and minimum records), so a function is fitted
  # once and the six-function comparison reuses the fits. The comparison runs only on its button, and opening the
  # Modelling tab no longer starts it. Both show a progress bar that moves with the individuals fitted, and timings are
  # logged to the R console, so a slow step can be told from a stalled one.
  A3_MAX_INDIVIDUALS <- 2000L
  step4_log <- function(...) message(sprintf("[disappR %s] step 4: ", format(Sys.time(), "%H:%M:%S")), sprintf(...))
  a3_cache <- new.env(parent = emptyenv())
  observeEvent(safe_get(data_sig()), {
    rm(list = ls(a3_cache, all.names = TRUE), envir = a3_cache)
  }, ignoreNULL = FALSE)
  observeEvent(input$tabs, {
    if (identical(input$tabs, "individual")) {
      d <- safe_get(dat())
      if (!is.null(d)) step4_log("tab opened: %d individuals, %d records", length(unique(d$id)), nrow(d))
    }
  })
  # the individuals step 4 uses: everyone, or an evenly spaced subset beyond A3_MAX_INDIVIDUALS
  a3_data <- reactive({
    d <- dat()
    ids <- unique(d$id)
    if (length(ids) <= A3_MAX_INDIVIDUALS) return(d)
    keep <- ids[unique(round(seq(1, length(ids), length.out = A3_MAX_INDIVIDUALS)))]
    out <- d[d$id %in% keep, , drop = FALSE]
    attr(out, "capped") <- c(used = length(keep), total = length(ids))
    out
  })
  output$a3_cap_warning <- renderUI({
    cp <- attr(safe_get(a3_data()), "capped")
    tagList(
      if (!is.null(cp)) div(class = "diagnosis-card", style = "border-left: 5px solid #C77C02;",
        div(class = "diagnosis-title", badge("caution"), " Step 4 uses a subset of individuals"),
        div(class = "diagnosis-detail",
            sprintf("To keep this page responsive, the individual fits use %s of the %s individuals (evenly spaced by ID). The mixed models on the Modelling tab use every individual.",
                    format(cp[["used"]], big.mark = ","), format(cp[["total"]], big.mark = ",")))) else NULL,
      if (!isTRUE(step4_isolated)) p(class = "small-note",
        "The individual fits run in the app's own R process. Install the 'callr' package to run them in a separate process, so that a failure on this page cannot stop the rest of the app.") else NULL)
  })
  # ---- Step 4 runs in a separate R process (0.20.12) ----
  # With the callr package installed, the individual fits run in a child R process. Whatever goes wrong there - an
  # error, a hang, even a failure that stops R - ends only that process: the page shows the reason, and every other
  # page keeps working. A job that runs longer than STEP4_TIMEOUT seconds is stopped. Without callr the fits run in
  # the app's own R process, as before.
  STEP4_TIMEOUT <- 180
  step4_isolated <- requireNamespace("callr", quietly = TRUE)
  step4_engine_dir <- tryCatch(if (nzchar(.disappr_engine_dir)) normalizePath(.disappr_engine_dir, mustWork = TRUE) else "",
                               error = function(e) "")
  step4_child <- function(engine_dir, files, what, args, progress_file) {
    if (nzchar(engine_dir)) {
      suppressPackageStartupMessages({ library(shiny); library(shinydashboard); library(ggplot2) })
      env <- new.env(parent = globalenv())
      for (f in files) sys.source(file.path(engine_dir, f), envir = env, keep.source = FALSE)
      fn <- get(what, envir = env)
    } else {
      fn <- utils::getFromNamespace(what, "disappR")
    }
    if (nzchar(progress_file)) args$progress <- function(f, detail) try(writeLines(format(f), progress_file), silent = TRUE)
    do.call(fn, args)
  }
  step4_run <- function(what, args, pr = NULL) {
    if (!step4_isolated) {
      if (is.function(pr)) args$progress <- pr
      return(do.call(get(what), args))
    }
    pf <- tempfile("disappr_step4_", fileext = ".txt")
    on.exit(unlink(pf), add = TRUE)
    p <- callr::r_bg(step4_child, args = list(engine_dir = step4_engine_dir, files = DISAPPR_ENGINE_FILES, what = what,
                                             args = args, progress_file = if (is.function(pr)) pf else ""),
                     supervise = TRUE)
    t0 <- Sys.time()
    while (p$is_alive()) {
      p$wait(250)
      if (is.function(pr) && file.exists(pf)) {
        f <- suppressWarnings(as.numeric(readLines(pf, n = 1L, warn = FALSE)))
        if (length(f) == 1L && is.finite(f)) pr(f, sprintf("%.0f%% of individuals", 100 * f))
      }
      if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > STEP4_TIMEOUT) {
        p$kill()
        stop(sprintf("the individual fits took longer than %d seconds and were stopped", STEP4_TIMEOUT), call. = FALSE)
      }
    }
    tryCatch(p$get_result(), error = function(e) {
      stop(paste("the separate R process running step 4 stopped:", conditionMessage(e)), call. = FALSE)
    })
  }
  a3_key <- function(fn) paste(safe_get(data_sig()) %||% "", fn, a3_link(), a3_min_records() %||% 0, sep = "|")
  # one function's individual fits, from the cache when possible; pr is an optional progress function(fraction, detail)
  a3_fit_for <- function(fn, pr = NULL) {
    key <- a3_key(fn)
    hit <- get0(key, envir = a3_cache, inherits = FALSE)
    if (!is.null(hit)) return(hit)
    d <- a3_data(); lk <- a3_link(); mr <- a3_min_records()
    t0 <- Sys.time()
    z <- tryCatch(step4_run("fit_individual_function", list(dat = d, fun = fn, min_resid_df = a3_min_df(), draw_ids = character(0),
                                                            link = lk, min_records = mr), pr),
                  error = function(e) {
                    message("disappR: step 4 individual fits failed: ", conditionMessage(e))
                    list(error = conditionMessage(e))
                  })
    step4_log("%s fitted to %d of %d individuals in %.2f s", fn, length(z$fitted_ids), length(unique(d$id)),
              as.numeric(difftime(Sys.time(), t0, units = "secs")))
    if (is.null(z$error)) assign(key, z, envir = a3_cache)
    z
  }
  a3_fit <- reactive({
    fn <- input$a3_function %||% "Quadratic"
    if (!inherits(shiny::getDefaultReactiveDomain(), "ShinySession")) return(a3_fit_for(fn))
    shiny::withProgress(message = paste("Fitting", fn, "to each individual"), value = 0,
                        a3_fit_for(fn, function(f, detail) shiny::setProgress(value = f, detail = detail)))
  })
  # counts modelled as negative binomial or zero-inflated are compared by QAICc (Poisson fits, dispersion-corrected)
  a3_quasi <- reactive(identical(a3_link(), "log") && isTRUE((input$model_family %||% "gaussian") %in% setdiff(COUNT_FAMILIES, "poisson")))
  a3_compare_sig <- function() paste(safe_get(data_sig()) %||% "", a3_link(), a3_min_records() %||% 0, a3_quasi(), sep = "|")
  a3_compare_res <- reactiveVal(NULL)
  observeEvent(input$run_a3_compare, disappr_guard("input$run_a3_compare", {
    d <- safe_get(a3_data())
    if (is.null(d)) {
      notify("The data are not ready: check the Data tab.", type = "error")
      return(invisible(NULL))
    }
    t0 <- Sys.time()
    fns <- A3_FUNCTIONS
    # the functions not yet fitted are fitted together, in one separate R process when callr is installed
    todo <- fns[vapply(fns, function(fn) is.null(get0(a3_key(fn), envir = a3_cache, inherits = FALSE)), logical(1))]
    fail <- NULL
    if (length(todo)) {
      new <- shiny::withProgress(message = "Comparing ageing functions across individuals", value = 0,
        tryCatch(step4_run("fit_individual_functions", list(dat = d, funs = todo, link = a3_link(), min_records = a3_min_records()),
                           function(f, detail) shiny::setProgress(value = f, detail = detail)),
                 error = function(e) {
                   message("disappR: step 4 individual fits failed: ", conditionMessage(e))
                   fail <<- conditionMessage(e)
                   list()
                 }))
      for (fn in names(new)) if (is.list(new[[fn]]) && is.null(new[[fn]]$error)) assign(a3_key(fn), new[[fn]], envir = a3_cache)
    }
    fits <- stats::setNames(lapply(fns, function(fn) get0(a3_key(fn), envir = a3_cache, inherits = FALSE) %||% list(error = "not fitted")), fns)
    tab <- tryCatch(compare_individual_functions(d, a3_min_df(), link = a3_link(), min_records = a3_min_records(),
                                                 quasi = a3_quasi(), fits = fits),
                    error = function(e) {
                      message("disappR: step 4 function comparison failed: ", conditionMessage(e))
                      structure(data.frame(), error = conditionMessage(e))
                    })
    if (!nrow(tab) && is.null(attr(tab, "error")) && !is.null(fail)) attr(tab, "error") <- fail
    attr(tab, "signature") <- a3_compare_sig()
    step4_log("six-function comparison finished in %.2f s", as.numeric(difftime(Sys.time(), t0, units = "secs")))
    a3_compare_res(tab)
  }))
  # the comparison, when it has been run for the current data and settings; NULL otherwise
  a3_compare_current <- reactive({
    tab <- a3_compare_res()
    if (is.null(tab) || !identical(attr(tab, "signature"), a3_compare_sig())) return(NULL)
    tab
  })
  a3_validate <- function(z) {
    shiny::validate(shiny::need(is.list(z) && is.null(z$error),
      paste0("The individual fits could not be computed. Error: ", if (is.list(z)) z$error %||% "unknown" else "unknown",
             ". Please report this message.")))
  }

  output$a3_metrics <- renderUI({
    z <- a3_fit()
    a3_validate(z)
    im <- imet()
    im <- im[im$n_trait > 0, , drop = FALSE]
    fitted <- im$id %in% z$fitted_ids
    alr_all <- mean(im$alr, na.rm = TRUE)
    alr_fit <- if (any(fitted)) mean(im$alr[fitted], na.rm = TRUE) else NA_real_
    sd_alr <- stats::sd(im$alr, na.rm = TRUE)
    biased <- is.finite(alr_fit) && isTRUE(sd_alr > 0) && (alr_fit - alr_all) / sd_alr > 0.25
    tagList(
      div(class = "metric-card", div(class = "metric-label", "Individuals fitted"),
          div(class = "metric-value", paste0(length(z$fitted_ids), " / ", z$n_total)),
          div(class = "metric-sub", paste("Need \u2265", min_records_for(input$a3_function %||% "Quadratic"), "records at different ages"))),
      div(class = "metric-card", div(class = "metric-label", "Mean ALR: fitted vs all"),
          div(class = "metric-value", if (is.finite(alr_fit)) paste0(format_num(alr_fit), " vs ", format_num(alr_all)) else "NA"),
          div(class = "metric-sub", if (biased) "Fitted individuals are longer-lived: the individual fits are survivor-biased here." else "No strong lifespan selection among fitted individuals.")),
      {
        rest <- !fitted
        cmp <- function(v) sprintf("%s vs %s", format_num(mean(v[fitted], na.rm = TRUE)), format_num(mean(v[rest], na.rm = TRUE)))
        div(class = "metric-card", div(class = "metric-label", "Fitted vs not fitted"),
            div(class = "metric-value", if (any(fitted) && any(rest)) paste("ALR", cmp(im$alr)) else if (any(fitted)) "all fitted" else "none fitted"),
            div(class = "metric-sub", if (any(fitted) && any(rest)) paste0("Trait mean ", cmp(im$trait_mean), "; records ", cmp(im$n_trait),
                                                                           ". Individuals with enough records are themselves a survivor-selected group.") else ""))
      },
      if (nrow(z$fit_stats)) div(class = "metric-card", div(class = "metric-label", "Median adjusted R\u00b2"),
          div(class = "metric-value", format_num(stats::median(z$fit_stats$Adjusted_R2, na.rm = TRUE))),
          div(class = "metric-sub", paste("Mean R\u00b2", format_num(mean(z$fit_stats$R2, na.rm = TRUE))))) else NULL
    )
  })

  output$a3_plot <- renderPlot({
    d <- dat()
    ids <- a3_ids()
    z <- a3_fit()
    a3_validate(z)
    step4_log("drawing the fits of %d individuals", length(ids))
    raw <- d[d$id %in% ids & is.finite(d$trait), , drop = FALSE]
    shiny::validate(shiny::need(nrow(raw) > 0, "No trait records for the selected individuals: change the selection."))
    ids <- ids[ids %in% raw$id]
    stat <- z$fit_stats
    lab <- stats::setNames(paste0(ids, "\n", "not estimable"), ids)
    if (nrow(stat)) {
      hit <- ids[ids %in% stat$id]
      s <- stat[match(hit, stat$id), , drop = FALSE]
      lab[hit] <- paste0(hit, "\nadj R\u00b2 ", ifelse(is.finite(s$Adjusted_R2), sprintf("%.2f", s$Adjusted_R2), "NA"))
    }
    raw$panel <- factor(lab[raw$id], levels = unique(lab[ids]))
    p <- ggplot(raw, aes(age, trait)) + geom_point(colour = "#8A7564", size = 1.3, alpha = 0.8)
    # an individual whose records are all equal (or single) gets a readable axis around its value, instead of a
    # vanishingly small range labelled with the same number repeated
    flat <- tapply(raw$trait, raw$id, function(v) diff(range(v)) == 0)
    flat_ids <- names(flat)[flat]
    if (length(flat_ids)) {
      fv <- raw[match(flat_ids, raw$id), , drop = FALSE]
      pad <- pmax(abs(fv$trait) * 0.05, 1)
      p <- p + geom_blank(data = rbind(transform(fv, trait = trait - pad), transform(fv, trait = trait + pad)))
    }
    cv <- tryCatch(individual_curves(z, d, ids), error = function(e) data.frame())
    if (nrow(cv)) {
      # each curve is kept near its own records, so an extreme fit cannot hand the device enormous coordinates
      rg <- tapply(raw$trait, raw$id, function(v) range(v, finite = TRUE), simplify = FALSE)
      lo <- vapply(cv$id, function(i) rg[[i]][[1]], numeric(1))
      hi <- vapply(cv$id, function(i) rg[[i]][[2]], numeric(1))
      w <- pmax(hi - lo, abs(hi + lo) * 0.05, 1e-8)
      cv$fitted <- pmin(pmax(cv$fitted, lo - 2 * w), hi + 2 * w)
      cv$panel <- factor(lab[cv$id], levels = levels(raw$panel))
      p <- p + geom_line(data = cv, aes(age, fitted), colour = warm_palette[[1]], linewidth = 0.9)
    }
    p + facet_wrap(~ panel, ncol = 5, scales = "free") +
      labs(x = "Age", y = current_map()$trait, title = paste(input$a3_function, "fits to individual trajectories"),
           subtitle = if (isTRUE(z$log_scale)) paste0("Fitted on the log scale (a Poisson fit for each individual) and drawn in the trait's units: ",
                                                      "a function that is straight on the log scale curves here (see the \u24d8 next to 'Scale of the individual fits').") else NULL) +
      theme_disappR(10) + theme(strip.text = element_text(size = 8, colour = "#5B493C"))
  }, height = function() 210 * ceiling(max(1, length(a3_ids())) / 5) + 70)

  output$a3_mean_plot <- renderPlot(a3_mean_obj())
  a3_mean_obj <- reactive({
    z <- a3_fit()
    a3_validate(z)
    step4_log("drawing the average trajectory")
    shiny::validate(shiny::need(nrow(z$mean_curve) > 0, "No individual has enough records to fit this function."))
    mc <- z$mean_curve
    # Linear-in-parameters functions give identical curves either way, so one is drawn; the non-linear
    # exponential gives two different curves, so both are drawn. Not a user choice.
    # the mean of individual curves is drawn when it differs from the mean of coefficients, unless it is switched off
    recon <- if ((isTRUE(z$nonlinear) || isTRUE(z$log_scale)) && isTRUE(input$a3_show_fun %||% FALSE)) "both" else "coef"
    lab_c <- if (isTRUE(z$log_scale)) "Mean of coefficients (typical individual)" else "Mean of coefficients"
    lab_f <- if (isTRUE(z$log_scale)) "Mean of individual curves (population average)" else "Mean of individual functions"
    lab_t <- truth_label(truth())
    obs <- observed_trajectory(dat())
    obs <- obs[obs$age >= min(mc$age) & obs$age <= max(mc$age), , drop = FALSE]
    lines <- data.frame(age = numeric(0), fitted = numeric(0), Method = character(0))
    bands <- data.frame(age = numeric(0), lo = numeric(0), hi = numeric(0), Method = character(0))
    if (recon %in% c("coef", "both")) {
      lines <- rbind(lines, data.frame(age = mc$age, fitted = mc$fitted, Method = lab_c))
      bands <- rbind(bands, data.frame(age = mc$age, lo = mc$lo, hi = mc$hi, Method = lab_c))
    }
    if (recon %in% c("fun", "both")) {
      lines <- rbind(lines, data.frame(age = mc$age, fitted = mc$fitted_fun, Method = lab_f))
      bands <- rbind(bands, data.frame(age = mc$age, lo = mc$lo_fun, hi = mc$hi_fun, Method = lab_f))
    }
    tc <- truth()
    if (!is.null(tc)) lines <- rbind(lines, data.frame(age = mc$age, fitted = toy_true_curve(tc, mc$age), Method = lab_t))
    lines <- lines[is.finite(lines$fitted), , drop = FALSE]
    bands <- bands[is.finite(bands$lo) & is.finite(bands$hi), , drop = FALSE]
    # With few records per individual, curves extrapolated to all ages (or exponentiated on the log scale) can be
    # astronomically large; drawn as they are, they crashed the graphics device and the app (0.20.13). Everything is
    # brought to within reach of the window that is shown.
    lim <- if (nrow(obs)) {
      sp <- diff(range(obs$fitted))
      if (!is.finite(sp) || sp <= 0) sp <- max(abs(obs$fitted), 1)
      range(obs$fitted) + c(-3, 3) * sp
    } else if (nrow(lines)) {
      q <- stats::quantile(lines$fitted, c(0.1, 0.9), names = FALSE)
      q + c(-3, 3) * max(diff(q), abs(mean(q)) * 0.1, 1e-8)
    } else c(NA_real_, NA_real_)
    lines$fitted <- squish_to(lines$fitted, lim)
    bands$lo <- squish_to(bands$lo, lim)
    bands$hi <- squish_to(bands$hi, lim)
    cols <- stats::setNames(c("#D55E00", "#0072B2", "black"), c(lab_c, lab_f, lab_t))
    note <- if (isTRUE(z$log_scale)) {
      "Fitted on the log scale: each curve is exp(...), non-linear in its coefficients, so the typical individual (mean of coefficients) and the population average (mean of curves) differ (Jensen's inequality)."
    } else if (isTRUE(z$nonlinear)) {
      "Non-linear in its parameters: the two reconstructions differ (Jensen's inequality)."
    } else {
      "Linear in its parameters: the two reconstructions are identical (lines overlap)."
    }
    p <- ggplot()
    if (nrow(bands)) p <- p + geom_ribbon(data = bands, aes(x = age, ymin = lo, ymax = hi, fill = Method), alpha = 0.15)
    p <- p + geom_point(data = obs, aes(age, fitted, size = n), colour = "grey55", alpha = 0.7) +
      geom_line(data = lines, aes(age, fitted, colour = Method, linetype = Method), linewidth = 1.15) +
      scale_colour_manual(values = cols, labels = function(x) wrap_label(x, 20)) + scale_fill_manual(values = cols, guide = "none") +
      scale_linetype_manual(values = stats::setNames(c("solid", "dotdash", "longdash"), c(lab_c, lab_f, lab_t)), labels = function(x) wrap_label(x, 20)) +
      scale_size_area(max_size = 4, guide = "none") +
      labs(x = "Age", y = current_map()$trait, colour = NULL, linetype = NULL,
           subtitle = paste0(note, " Bands: 95% intervals from ", length(z$fitted_ids),
                             sprintf(" individual fits; grey points: observed means; ages observed in \u2265 %d individuals", A3_MIN_IND_PER_AGE))) +
      theme_disappR(12) + theme(legend.position = "bottom", legend.text = element_text(size = 10)) +
      guides(colour = guide_legend(ncol = 2), linetype = guide_legend(ncol = 2))
    if (nrow(obs)) {
      # keep the plot readable if an extreme non-linear fit inflates the mean of functions
      span <- diff(range(obs$fitted))
      if (!is.finite(span) || span <= 0) span <- max(abs(obs$fitted), 1)
      lim <- range(obs$fitted) + c(-3, 3) * span
      yv <- c(obs$fitted, lines$fitted, bands$lo, bands$hi)
      yv <- yv[is.finite(yv) & yv >= lim[1] & yv <= lim[2]]
      if (length(yv)) p <- p + coord_cartesian(ylim = range(yv))
    }
    p
  })

  output$a3_compare_table <- renderTable({
    x <- a3_compare_current()
    if (is.null(x)) return(data.frame(Note = "Press 'Compare ageing functions across individuals' to fit all six functions to every individual and compare them."))
    if (!is.null(attr(x, "error"))) return(data.frame(Note = paste0("The function comparison could not be computed. Error: ", attr(x, "error"), ". Please report this message.")))
    if (!nrow(x)) return(data.frame(Note = "No function could be fitted to any individual."))
    x$Mean_R2 <- round(x$Mean_R2, 3)
    x$Mean_adj_R2 <- round(x$Mean_adj_R2, 3)
    x$Median_adj_R2 <- round(x$Median_adj_R2, 3)
    x$Mean_dAICc <- round(x$Mean_dAICc, 2)
    # label the column honestly when overdispersed counts were compared by QAICc
    ch <- attr(x, "chat")
    if (is.finite(ch %||% NA_real_) && ch > 1) names(x)[names(x) == "Mean_dAICc"] <- sprintf("Mean_dQAICc (dispersion %.2f)", ch)
    x
  }, striped = TRUE, spacing = "xs")

  output$a3_coef_table <- renderTable({
    z <- a3_fit()
    a3_validate(z)
    cf <- z$coefs
    if (!nrow(cf)) return(data.frame(Note = "No coefficients."))
    nms <- setdiff(names(cf), "id")
    data.frame(Coefficient = nms,
               Mean = vapply(cf[nms], function(v) mean(v, na.rm = TRUE), numeric(1)),
               SD = vapply(cf[nms], function(v) stats::sd(v, na.rm = TRUE), numeric(1)),
               N = vapply(cf[nms], function(v) sum(is.finite(v)), integer(1)), check.names = FALSE)
  }, digits = 5, striped = TRUE, spacing = "xs")

  # ======================================================================
  # 6. Model settings shared by B2 and B4
  # ======================================================================
  model_settings <- reactive({
    fam <- input$model_family %||% "gaussian"
    zi_vars <- input$zi_vars %||% character(0)
    ex <- stats::setNames(lapply(MODEL_IDS, function(mid) as.character(input[[paste0("extra_", mid)]] %||% character(0))), MODEL_IDS)
    ex <- ex[vapply(ex, length, integer(1)) > 0]
    list(family = fam,
         zi = if (fam %in% ZI_FAMILIES && length(zi_vars)) paste("~", paste(zi_vars, collapse = " + ")) else "~1",
         trials = if (fam %in% BINOMIAL_FAMILIES) (input$col_trials %||% "") else "",
         age_function = input$model_age_function %||% "Quadratic",
         models = MODEL_IDS[vapply(MODEL_IDS, function(mid) isTRUE(input[[paste0("use_", mid)]]), logical(1))],
         random_slope = normalise_slope(input$random_structure %||% "none"),
         standardise = isTRUE(input$standardise),
         among = if (input$among_order %in% c("same", "consistent")) input$among_order else "linear",
         include_invalid = isTRUE(input$include_invalid),
         extra = if (length(ex)) ex else NULL,
         disp = normalise_disp(input$disp_model))
  })
  model_sig <- reactive({
    s <- model_settings()
    paste(data_sig(), settings_sig(s), s$age_function, paste(sort(s$models), collapse = ","),
          paste(names(s$extra), vapply(s$extra, function(x) paste(sort(x), collapse = "+"), character(1)), sep = "=", collapse = ";"), sep = "||")
  })

  fn_default <- reactiveValues(sig = NULL, best = NULL, n = NA, nonlinear_best = FALSE, manual = NULL)
  output$use_a3_function_ui <- renderUI({
    fn <- input$a3_function %||% "Quadratic"
    tagList(
      actionButton("use_a3_function", paste0("Use this ageing function (", fn, ") for the mixed models"),
                   class = "btn-block-space use-function-btn", icon = icon("arrow-right"),
                   style = "white-space: normal; width: 100%;"),
      if (identical(isolate(fn_default$manual), fn)) p(class = "small-note", "The Modelling tab uses this function.") else NULL
    )
  })
  observeEvent(input$use_a3_function, disappr_guard("input$use_a3_function", {
    fn <- input$a3_function %||% "Quadratic"
    updateSelectInput(session, "model_age_function", selected = fn)
    fn_default$manual <- fn
    notify(paste0("The mixed models on the Modelling tab will use the ", fn, " ageing function."), duration = 4)
  }))
  # Opening the Modelling tab no longer runs the step-4 comparison (0.20.5): it uses the comparison if it was run.
  observeEvent(list(input$tabs, safe_get(data_sig()), a3_compare_res()), disappr_guard("list(input$tabs, safe_get(data_sig()))", {
    if (!identical(input$tabs, "models")) return(invisible(NULL))
    tab <- a3_compare_current()
    sig <- safe_get(data_sig())
    if (is.null(sig)) return(invisible(NULL))
    sig <- paste(sig, if (is.null(tab)) "not compared" else "compared")
    if (identical(fn_default$sig, sig)) return(invisible(NULL))
    fn_default$sig <- sig
    lin <- if (is.data.frame(tab) && nrow(tab)) tab$Function[tab$Function %in% AGE_FUNCTIONS] else character(0)
    if (!length(lin)) {
      fn_default$best <- NULL
      return(invisible(NULL))
    }
    fn_default$best <- lin[[1]]
    fn_default$n <- tab$N_common[[1]]
    fn_default$nonlinear_best <- identical(tab$Function[[1]], A3_NONLINEAR)
    if (!is.null(fn_default$manual)) return(invisible(NULL))
    # an empirical example keeps the ageing function of its published model
    if (identical(input$data_source %||% "toy", "example") && !is.null(current_example()$age_function)) return(invisible(NULL))
    updateSelectInput(session, "model_age_function", selected = lin[[1]])
  }))
  output$function_default_note <- renderUI({
    b <- fn_default$best
    if (!is.null(fn_default$manual)) {
      return(p(class = "small-note", sprintf("Chosen on tab 4 (individual fits): %s.%s You can still change it here.", fn_default$manual,
                                             if (!is.null(b)) sprintf(" Lowest mean \u0394AICc across individuals: %s.", b) else "")))
    }
    ex_fn <- if (identical(input$data_source %||% "toy", "example")) current_example()$age_function else NULL
    if (!is.null(ex_fn)) return(NULL)   # 0.24.7: no note for examples
    if (is.null(b)) {
      return(p(class = "small-note", "Default: Quadratic. To let the individual fits suggest a function, press 'Compare ageing functions across individuals' on tab 4."))
    }
    cur <- input$model_age_function %||% b
    p(class = "small-note", sprintf("Default: %s, from the individual-level comparison in step 4.", b))
  })

  output$zi_ui <- renderUI({
    m <- safe_get(meta())
    covs <- if (is.null(m) || !length(m$covars)) character(0) else stats::setNames(m$covars, unname(m$cov_labels[m$covars]))
    ch <- c("Age" = "f1", "ALR" = "ALR", "AFR (age at first observation)" = "AFR", covs)
    cur <- isolate(input$zi_vars)
    selectizeInput("zi_vars", "Zero inflation depends on", choices = ch, selected = intersect(cur %||% character(0), ch), multiple = TRUE,
                   options = list(placeholder = "constant (~1) if empty"))
  })
  try(outputOptions(output, "zi_ui", suspendWhenHidden = FALSE), silent = TRUE)
  # Binomial weights: the column holding the number of trials behind each proportion (binomial families only).
  output$trials_ui <- renderUI({
    df <- safe_get(raw_data())
    cols <- if (is.null(df)) character(0) else names(df)
    cur <- isolate(input$col_trials) %||% ""
    pre <- safe_get(isolate(preset_map())$trials) %||% ""
    sel <- if (cur %in% cols) cur else if (length(pre) == 1 && pre %in% cols) pre else ""
    tagList(
      selectInput("col_trials", "Weights: number of binomial trials", choices = c("None: binary 0/1 trait (one trial per record)" = "", cols), selected = sel),
      box_note("For a binary (0/1) trait keep 'None'. For a proportion (successes / trials), choose the column holding the number of trials (for example clutch size): each record is then weighted by its trials, so the model is fitted to successes out of trials. Records without a positive number of trials are dropped. The beta-binomial family needs proportions with trials."),
      box_note(strong("Why the number of trials matters. "), "A proportion of 0.6 carries very different information depending on what it came from: 3 out of 5 and 600 out of 1,000 are both 0.6, but the second pins the underlying probability far more tightly. Without a trials column every proportion is given the same weight, so a bird with one egg counts as much as a bird with twenty, standard errors are wrong and the model is over-confident about records based on few trials. With the column mapped, each record is weighted by its own number of trials.")
    )
  })
  try(outputOptions(output, "trials_ui", suspendWhenHidden = FALSE), silent = TRUE)
  observeEvent(input$model_family, disappr_guard("input$model_family", {
    if (isTRUE(input$model_family %in% BINOMIAL_FAMILIES)) {
      notify(paste(if (identical(input$model_family, "betabinomial")) "Beta-binomial family." else "Binomial family.",
                   "A binary (0/1) trait needs no weights. For proportions, choose the column with the number of trials under 'Weights' below the error family, so that each proportion is weighted by its number of trials."),
             duration = 12)
    }
  }), ignoreInit = TRUE)
  output$random_support_note <- renderUI({
    d <- safe_get(dat())
    if (is.null(d)) return(NULL)
    sup <- individual_data_support(d)
    adv <- random_slope_advice(sup)
    col <- switch(adv$level, good = "#1B7837", limited = "#B35806", "#A50026")
    warn <- data_support_warning(sup)
    rs_def <- if (!identical(input$data_source %||% "toy", "example")) default_random_structure(d) else NULL
    tagList(
      div(class = "small-note", style = paste0("border-left: 3px solid ", col, "; padding-left: 7px; margin-bottom: 8px;"), adv$text,
          if (!is.null(rs_def)) paste0(" Default for these data: ", names(RANDOM_STRUCTURES)[match(rs_def, RANDOM_STRUCTURES)], ".") else NULL),
      if (nzchar(warn)) div(class = "small-note", style = "border-left: 3px solid #A50026; padding-left: 7px; margin-bottom: 8px;", strong(warn))
    )
  })
  extra_term_label <- function(x, m) {
    ch <- extra_term_choices(m)
    lab <- names(ch)[match(x, ch)]
    built <- is.na(lab) & startsWith(as.character(x), "term:")
    if (any(built)) lab[built] <- vapply(x[built], function(k) extra_term_display(k, m), character(1))
    ifelse(is.na(lab), x, lab)
  }
  # Terms made with the term builder, per model; they are offered in that model's menu while their terms exist.
  built_terms <- reactiveValues()
  extra_choices_for <- function(mid, m) {
    b <- isolate(built_terms[[mid]]) %||% character(0)
    b <- b[vapply(b, built_term_ok, logical(1), covars = m$covars %||% character(0), USE.NAMES = FALSE)]
    c(extra_term_choices(m), stats::setNames(b, vapply(b, function(k) extra_term_display(k, m), character(1), USE.NAMES = FALSE)))
  }
  # An example opens with its own extra terms (e.g. lifespan added to Model 1 for the bee data): they are set when the
  # example changes or the app leaves the examples; otherwise the current selection is kept.
  example_extra_id <- reactiveVal("")
  observeEvent(safe_get(meta()$covars), disappr_guard("safe_get(meta()$covars)", {
    m <- safe_get(meta())
    ex_id <- if (identical(isolate(input$data_source) %||% "toy", "example")) (isolate(input$example_id) %||% "") else ""
    reset <- !identical(ex_id, isolate(example_extra_id()))
    if (reset) example_extra_id(ex_id)
    ex_extra <- if (nzchar(ex_id)) EXAMPLES[[ex_id]]$extra else NULL
    for (mid in MODEL_IDS) {
      ch <- extra_choices_for(mid, m)
      cur <- if (reset) (ex_extra[[mid]] %||% character(0)) else (isolate(input[[paste0("extra_", mid)]]) %||% character(0))
      updateSelectizeInput(session, paste0("extra_", mid), choices = ch, selected = intersect(cur, ch))
    }
  }), ignoreNULL = FALSE)

  # ---- Term builder: up to three terms joined by + (additive) or x (interaction), per model ----
  build_target <- reactiveVal(NULL)
  builder_ops <- c("\u00d7 (interaction)" = "*", "+ (additive)" = "+")
  builder_key <- function() {
    tk <- c(input$tb_t1 %||% "", input$tb_t2 %||% "", input$tb_t3 %||% "")
    op <- c(input$tb_o1 %||% "*", input$tb_o2 %||% "*")
    if (!nzchar(tk[[1]])) return(NULL)
    txt <- tk[[1]]
    if (nzchar(tk[[2]])) txt <- paste0(txt, if (identical(op[[1]], "+")) "+" else "*", tk[[2]])
    if (nzchar(tk[[3]])) txt <- paste0(txt, if (identical(op[[2]], "+")) "+" else "*", tk[[3]])
    comps <- parse_built_term(paste0("term:", txt))
    if (!length(comps)) return(NULL)
    paste0("term:", paste(vapply(comps, paste, character(1), collapse = "*"), collapse = "+"))
  }
  lapply(MODEL_IDS, function(mid) {
    observeEvent(input[[paste0("build_", mid)]], disappr_guard("input[[paste0('build_', mid)]]", {
      if (!is.function(session$sendModal)) return(invisible(NULL))
      toks <- builder_tokens(safe_get(meta()))
      build_target(mid)
      showModal(modalDialog(
        title = paste("Build a term for", model_label(mid)),
        p(class = "small-note", "Choose up to three terms and how each joins the previous one: \u00d7 fits an interaction together with its main effects (A * B in R), + adds the term on its own. For example ALR \u00d7 AFR \u00d7 age fits a three-way interaction. 'Age' stands for all ageing terms of the chosen function. The term is added to this model's menu, where it can be removed again."),
        fluidRow(
          column(3, selectInput("tb_t1", "Term 1", choices = toks)),
          column(2, selectInput("tb_o1", "Join", choices = builder_ops)),
          column(3, selectInput("tb_t2", "Term 2", choices = c("(none)" = "", toks), selected = "")),
          column(2, selectInput("tb_o2", "Join", choices = builder_ops)),
          column(2, selectInput("tb_t3", "Term 3", choices = c("(none)" = "", toks), selected = ""))
        ),
        uiOutput("tb_preview"),
        footer = tagList(modalButton("Cancel"), actionButton("tb_add", "Add to this model", class = "btn-primary", icon = icon("plus"))),
        easyClose = TRUE, size = "l"))
    }), ignoreInit = TRUE)
  })
  output$tb_preview <- renderUI({
    key <- builder_key()
    if (is.null(key)) return(p(class = "small-note", "Choose at least one term."))
    mid <- build_target() %||% "M1"
    m <- safe_get(meta())
    s <- model_settings()
    fn <- if (identical(s$age_function, A3_NONLINEAR)) "Linear" else s$age_function
    b <- switch(fn, Quadratic = c("f1", "f2"), Cubic = c("f1", "f2", "f3"), "f1")
    fr <- built_term_formula(key, b, s$among, mid %in% c("M3", "M5"))
    n_terms <- tryCatch(length(attr(stats::terms(stats::as.formula(paste("~", fr))), "term.labels")), error = function(e) NA_integer_)
    ok <- built_term_ok(key, m$covars %||% character(0))
    tagList(
      div(class = "truth-card small-note",
          strong("Term: "), extra_term_display(key, m), br(),
          strong("Added to the formula: "), tags$code(display_term(fr)),
          if (is.finite(n_terms)) tagList(br(), sprintf("%d fixed-effect terms when expanded (more coefficients for categorical covariates with more than two levels).", n_terms)) else NULL),
      if (!ok) div(class = "diagnosis-card", div(class = "diagnosis-detail", "This combination cannot be added: use at most three terms from the lists.")) else NULL,
      if (built_term_age_interaction(key)) div(class = "diagnosis-card", div(class = "diagnosis-detail", strong("Caution: interaction with age. "),
        "Every ageing term (e.g. age and age\u00b2 for a quadratic) gets its own coefficient for each interacting term, so interactions with age add parameters quickly, especially three-way interactions and categorical covariates with several levels. Such models can be over-fitted, unstable (check the fitting status and random-effect variances) and hard to interpret: compare them with simpler models.")) else NULL
    )
  })
  observeEvent(input$tb_add, disappr_guard("input$tb_add", {
    mid <- build_target()
    key <- builder_key()
    m <- safe_get(meta())
    if (is.null(mid) || is.null(key) || !built_term_ok(key, m$covars %||% character(0))) {
      notify("Choose at least one term from the lists (at most three).", type = "warning")
      return(invisible(NULL))
    }
    built_terms[[mid]] <- unique(c(built_terms[[mid]], key))
    cur <- input[[paste0("extra_", mid)]] %||% character(0)
    updateSelectizeInput(session, paste0("extra_", mid), choices = extra_choices_for(mid, m), selected = unique(c(cur, key)))
    if (is.function(session$sendModal)) removeModal()
    age_int <- built_term_age_interaction(key)
    notify(paste0("Added to ", model_label(mid), ": ", extra_term_display(key, m),
                  if (age_int) ". Caution: interactions with age add many parameters and can over-fit." else "."),
           type = if (age_int) "warning" else "message", duration = 8)
  }))
  lapply(MODEL_IDS, function(mid) {
    observeEvent(input[[paste0("info_model_", mid)]], disappr_guard("input[[paste0('info_model_', mid)]]", {
      s <- model_settings()
      m <- safe_get(meta())
      d <- safe_get(dat())
      z <- tryCatch(model_info_content(mid, s, m), error = function(e) NULL)
      if (is.null(z) || !is.function(session$sendModal)) return(invisible(NULL))
      lv <- list()
      if (!is.null(m) && !is.null(d) && length(m$covars)) {
        cat_cov <- m$covars[vapply(m$covars, function(cv) !is.null(d[[cv]]) && !is.numeric(d[[cv]]), logical(1))]
        for (cv in cat_cov) lv[[cv]] <- sort(unique(as.character(stats::na.omit(d[[cv]]))))
      }
      eqn <- tryCatch(model_equation(mid, s, m, lv, d), error = function(e) list(available = FALSE, message = paste("The equation could not be built:", conditionMessage(e))))
      body <- tagList(
        p(z$meaning),
        tags$table(class = "table table-condensed",
                   tags$tbody(lapply(names(z$attributes), function(k) tags$tr(tags$th(k), tags$td(z$attributes[[k]]))))),
        p(strong("Error family: "), z$family, br(), strong("Ageing function: "), z$ageing,
          if (length(z$extra)) tagList(br(), strong("Terms added to this model: "), paste(extra_term_label(z$extra, m), collapse = ", ")) else NULL),
        h5(strong("Model equation")),
        if (isTRUE(eqn$available)) tagList(
          div(class = "model-eq", lapply(eqn$equation, function(x) div(HTML(x)))),
          tags$ul(class = "small-note", lapply(eqn$distributions, function(x) tags$li(HTML(x)))),
          h5(strong("Coefficients (each term has its own coefficient)")),
          tags$table(class = "table table-condensed coef-eq",
                     tags$thead(tags$tr(tags$th("Coefficient"), tags$th("Multiplies"), tags$th("Name in the fitted R model"))),
                     tags$tbody(lapply(seq_len(nrow(eqn$coefficients)), function(k) {
                       tags$tr(tags$td(HTML(eqn$coefficients$Coefficient[[k]])), tags$td(HTML(eqn$coefficients$Multiplies[[k]])),
                               tags$td(tags$code(eqn$coefficients$R_term[[k]])))
                     }))),
          h5(strong("R syntax")),
          tags$pre(class = "code-out", paste(c(if (!is.null(eqn$basis)) paste("#", eqn$basis),
                                               "# as written in the app", eqn$r_compact,
                                               if (!is.null(eqn$r_expanded)) c("", "# fully expanded (A * B = A + B + A:B)", eqn$r_expanded),
                                               "", "# fitting call", eqn$r_call), collapse = "\n"))
        ) else p(eqn$message %||% "The equation could not be built."),
        p(class = "small-note", "i = individual, j = sampling occasion, k = group. age, ALR (age at last record), AFR (age at first observation) and LS (known lifespan) are centred and scaled when 'Standardise' is ticked. mean(age) is an individual's mean age over its records and \u0394age = age \u2212 mean(age). [x = level] is 1 when a categorical covariate takes that level and 0 otherwise; the first level is the reference. In R, f1 is the (transformed) age term, f2 and f3 its square and cube, and cv_ marks covariates."),
        p(class = "small-note", "Covariates and interactions are set on the Data tab; the other settings on this tab.")
      )
      showModal(modalDialog(title = z$title, body, easyClose = TRUE, footer = modalButton("Close"), size = "l"))
    }), ignoreInit = TRUE)
  })

  output$family_hint <- renderUI({
    fam <- safe_get(integrity()$family)
    s <- model_settings()
    msgs <- list()
    if (!is.null(fam)) {
      if (!identical(fam$family, "gaussian") && identical(s$family, "gaussian")) {
        msgs <- c(msgs, list(div(class = "diagnosis-card", div(class = "diagnosis-detail",
          strong("Integer data treated as Gaussian. "), "Check that the trait is continuous rather than a count, to avoid a misfit family."))))
      }
      if (identical(fam$family, "gaussian") && !s$family %in% DENSITY_FAMILIES) {
        msgs <- c(msgs, list(div(class = "diagnosis-card", div(class = "diagnosis-detail", "The trait is not a non-negative integer count: use the Gaussian family."))))
      }
    }
    if (!identical(s$family, "gaussian") && !HAS_GLMMTMB) {
      msgs <- c(msgs, list(div(class = "diagnosis-card", div(class = "diagnosis-detail", "Install glmmTMB to fit this family."))))
    }
    tagList(msgs)
  })

  output$structure_note <- renderUI({
    m <- safe_get(meta())
    if (is.null(m)) return(NULL)
    s <- model_settings()
    rstr <- random_display(m, s$random_slope, basis_label(s$age_function %||% "Quadratic"))
    cov <- if (length(m$covars)) {
      paste(vapply(m$covars, function(cv) paste0(m$cov_labels[[cv]], if (cv %in% (m$cov_age %||% character(0))) " (\u00d7 age)" else ""), character(1)), collapse = ", ")
    } else "none"
    div(class = "truth-card small-note",
        strong("Random effects: "), rstr, if (isTRUE(m$has_group) && isTRUE(m$nested)) (if (isTRUE(m$has_group2)) " (IDs nested in groups, nested in top-level groups)" else " (IDs nested in group)") else "", br(),
        strong("Covariates: "), cov, br(),
        strong("Among-individual terms: "), switch(s$among %||% "linear", same = "same polynomial order as the ageing function (powers of mean age)",
                                                     consistent = "same polynomial order, consistent decomposition (mean of each age term)", "linear"), br(),
        if (length(s$extra)) tagList(strong("Extra terms: "), paste(paste0(model_label(names(s$extra)), ": ",
                                     vapply(s$extra, function(x) paste(extra_term_label(x, m), collapse = ", "), character(1))), collapse = "; "), br()) else NULL,
        if (any(vapply(unlist(s$extra), built_term_age_interaction, logical(1)))) tagList(strong("Caution: "), "built terms that interact with age add many parameters and can over-fit the models they are added to.", br()) else NULL,
        "Change covariates and grouping on the Data tab.")
  })


  # ---- B2: population-level function comparison ----
  b2_res <- reactiveVal(NULL)
  observeEvent(input$run_functions, disappr_guard("input$run_functions", {
    d <- safe_get(dat())
    if (is.null(d)) {
      notify("The data are not ready: check the Data tab.", type = "error")
      return(invisible(NULL))
    }
    s <- model_settings()
    fns <- intersect(input$b2_functions %||% character(0), MODEL_FUNCTIONS)
    if (A3_NONLINEAR %in% fns && !identical(s$family, "gaussian")) {
      fns <- setdiff(fns, A3_NONLINEAR)
      notify("With a count family the non-linear exponential equals the Linear function on the log scale, so it was left out.", type = "warning", duration = 6)
    }
    if (!length(fns)) {
      notify("Tick at least one ageing function.", type = "warning")
      return(invisible(NULL))
    }
    res <- run_with_progress("Comparing ageing functions", function(pr) {
      compare_population_functions(d, meta(), s$family, s$random_slope, s$standardise, s$zi, progress = pr,
                                   functions = fns, model = "M1", among = s$among, include_invalid = s$include_invalid, disp = s$disp)
    })
    res$signature <- paste(data_sig(), settings_sig(s))
    b2_res(res)
  }))
  b2_current <- reactive({
    r <- b2_res()
    s <- model_settings()
    shiny::validate(shiny::need(!is.null(r), "Press 'Compare ageing functions'."))
    shiny::validate(shiny::need(identical(r$signature, paste(data_sig(), settings_sig(s))),
                  "Data or settings changed: press 'Compare ageing functions' again."))
    r
  })
  # Results for the currently ticked functions (delta AIC recomputed among them)
  b2_shown <- reactive({
    r <- b2_current()
    keep <- intersect(input$b2_functions %||% character(0), r$table$Function)
    shiny::validate(shiny::need(length(keep) > 0, "Press 'Compare ageing functions' to fit the ticked functions."))
    tab <- r$table[r$table$Function %in% keep, , drop = FALSE]
    if (any(is.finite(tab$AIC))) tab$Delta_AIC <- tab$AIC - min(tab$AIC, na.rm = TRUE)
    cv <- if (nrow(r$curves)) r$curves[r$curves$Function %in% keep, , drop = FALSE] else r$curves
    list(table = tab, curves = cv)
  })
  output$b2_table <- renderTable({
    tab <- b2_shown()$table
    tab$AIC <- round(tab$AIC, 1)
    tab$Delta_AIC <- round(tab$Delta_AIC, 1)
    tab
  }, striped = TRUE, spacing = "xs")
  output$b2_plot <- renderPlot(b2_plot_obj())
  b2_plot_obj <- reactive({
    r <- b2_shown()
    shiny::validate(shiny::need(nrow(r$curves) > 0, "No function could be fitted: lower the minimum records or choose a simpler function."))
    obs <- observed_trajectory(dat())
    ggplot() +
      geom_point(data = obs, aes(age, fitted, size = n), shape = 21, fill = "grey55", colour = "black", stroke = 0.6, alpha = 0.85) +
      geom_line(data = r$curves, aes(age, fitted, colour = Function), linewidth = 1.2) +
      scale_colour_manual(values = FUNCTION_COLOURS) +
      scale_size_area(max_size = 4, guide = "none") +
      labs(x = "Age", y = current_map()$trait, subtitle = "Lines: Model 1 predictions for each function; outlined points: observed means") +
      theme_disappR(13)
  })

  # ---- B4: comparative models ----
  model_res <- reactiveVal(NULL)
  # Rows before fitting: a dry run through fit_model_suite() with the current settings (nothing is fitted)
  output$prefit_rows <- renderUI({
    d <- safe_get(dat())
    s <- safe_get(model_settings())
    if (is.null(d) || is.null(s) || !length(s$models)) return(NULL)
    pv <- tryCatch(fit_model_suite(d, meta(), s$models, s$age_function, s$family, s$random_slope, s$standardise, s$zi,
                                   among = s$among, include_invalid = s$include_invalid, extra = s$extra, dry_run = TRUE, disp = s$disp),
                   error = function(e) NULL)
    if (is.null(pv)) return(NULL)
    if (!isTRUE(pv$ok)) return(div(class = "prefit-box", strong("Before fitting: "), pv$message))
    link <- family_link(pv$family)
    div(class = "prefit-box",
        p(strong("Before fitting. "),
          sprintf("The selected models share %d of %d records with a trait value, from %d of %d individuals.",
                  pv$n_rows_used, pv$n_rows, pv$n_ids_used, pv$n_ids)),
        if (length(pv$drop_by)) p(strong("Records not shared by every model: "), paste(pv$drop_by, collapse = "; "), ".") else NULL,
        p(strong("Settings. "),
          sprintf("%s with a %s link, %s ageing function, %s, random effects %s%s.", family_label(pv$family), link, pv$age_function,
                  if (isTRUE(pv$standardise)) "age and proxies standardised" else "raw scales", display_term(pv$random),
                  paste0(if (nzchar(pv$zi %||% "") && !identical(pv$zi, "~1")) paste0(", zero-inflation ", display_term(pv$zi)) else "",
                         if (!identical(pv$disp %||% "constant", "constant")) paste0("; ", disp_text(pv$disp)) else ""))),
        tags$ul(lapply(names(pv$formulas), function(m) tags$li(paste0(model_label(m), ": trait ~ ", display_term(pv$formulas[[m]]))))),
        p(class = "small-note", "The formulae above are general: they do not expand the terms of the ageing function you have chosen. For the exact formula of a model under the current settings, click the 'i' beside that model."),
        if (length(pv$alias_notes)) p(strong("Redundant terms (dropped when fitting): "), paste(pv$alias_notes, collapse = "; "), ".") else NULL,
        if (length(pv$fit_notes)) p(paste(pv$fit_notes, collapse = " ")) else NULL,
        NULL)   # 0.24.7: the random-effect caution is shown once, under the random-effect choice
  })

  observeEvent(input$fit_models, disappr_guard("input$fit_models", {
    d <- safe_get(dat())
    if (is.null(d)) {
      notify("The data are not ready: check the Data tab.", type = "error")
      return(invisible(NULL))
    }
    s <- model_settings()
    if (!length(s$models)) {
      notify("Select at least one model.", type = "warning")
      return(invisible(NULL))
    }
    res <- run_with_progress("Fitting models", function(pr) {
      fit_model_suite(d, meta(), s$models, s$age_function, s$family, s$random_slope, s$standardise, s$zi, progress = pr,
                      among = s$among, include_invalid = s$include_invalid, extra = s$extra, disp = s$disp)
    })
    res$signature <- model_sig()
    model_res(res)
    dharma_res(NULL)
    if (!isTRUE(res$ok)) notify(res$message, type = "error", duration = 8)
  }))

  current_models <- reactive({
    r <- model_res()
    shiny::validate(shiny::need(!is.null(r), "Choose settings and press 'Fit models'."))
    shiny::validate(shiny::need(identical(r$signature, model_sig()), "Data or model settings changed since the last fit: press 'Fit models' to update."))
    r
  })
  fitted_models <- reactive({
    r <- current_models()
    shiny::validate(shiny::need(isTRUE(r$ok), r$message))
    r
  })

  output$model_fit_note <- renderUI({
    r <- current_models()
    if (!isTRUE(r$ok)) {
      return(div(class = "diagnosis-card", div(class = "diagnosis-title", "Models not fitted"), div(class = "diagnosis-detail", r$message)))
    }
    aic <- r$aic
    best <- aic$Model[[1]]
    best_id <- sub("^Model ", "M", best)
    elig <- aic[aic$Eligible, , drop = FALSE]
    warn <- sum(unlist(r$validity) != "Valid")
    div(class = "diagnosis-card",
        div(class = "diagnosis-title", {
          supported <- if (nrow(elig)) elig$Model[is.finite(elig$Delta_AIC) & elig$Delta_AIC < 2] else best
          if (!length(supported)) supported <- best
          if (length(supported) > 1) paste0("Supported set (\u0394AIC < 2): ", paste(supported, collapse = ", "))
          else paste0("Supported model: ", supported,
                      if (nrow(elig) > 1) sprintf(" (next model \u0394AIC = %.1f)", elig$Delta_AIC[[2]]) else "")
        }),
        div(class = "diagnosis-detail", {
          supported <- if (nrow(elig)) elig$Model[is.finite(elig$Delta_AIC) & elig$Delta_AIC < 2] else best
          if (!length(supported)) supported <- best
          dfs <- if ("df" %in% names(elig)) elig$df[match(supported, elig$Model)] else rep(NA_real_, length(supported))
          simplest <- if (all(is.finite(dfs))) supported[dfs == min(dfs)] else supported[[1]]
          paste0(if (length(supported) > 1) paste0("Supported set (\u0394AIC < 2): ", paste(supported, collapse = ", "), ". ")
                 else paste0("Supported model: ", supported, ". "),
                 if (length(supported) > 1) paste0("Most parsimonious of these: ", paste(simplest, collapse = " or "), ".") else "")
        }),
        div(class = "diagnosis-detail", paste0(family_label(r$family), "; ", r$age_function, " ageing; random effects ",
                                               if (isTRUE(r$nonlinear)) r$random else random_display(meta(), r$random_slope),
                                               "; N = ", aic$N[[1]], " observations from ", length(unique(r$data$id)), " individuals.")),
        {
          # the fitted formula of every model in the supported set (0.24.7)
          sup_ids <- sub("^Model ", "M", aic$Model[is.finite(aic$Delta_AIC) & aic$Delta_AIC < 2])
          sup_ids <- intersect(if (length(sup_ids)) sup_ids else best_id, names(r$formulas))
          div(class = "diagnosis-detail small-note",
              if (length(sup_ids) > 1) "Fitted formulas of the supported models:" else "Fitted formula:",
              tags$ul(style = "margin: 2px 0 0; padding-left: 18px;",
                      lapply(sup_ids, function(m) tags$li(if (isTRUE(r$nonlinear)) paste0(model_label(m), ": ", r$formulas[[m]]) else
                        paste0(model_label(m), ": trait ~ ", display_term(r$formulas[[m]]), " + ", display_term(r$random))))))
        },
        if (nzchar(r$random_note %||% "")) div(class = "diagnosis-detail small-note", r$random_note) else NULL,
        if (length(r$alias_notes)) div(class = "diagnosis-detail small-note", paste0("Redundant terms dropped: ", paste(r$alias_notes, collapse = "; "), ".")) else NULL,
        if (length(r$fit_notes)) div(class = "diagnosis-detail small-note", paste(r$fit_notes, collapse = " ")) else NULL,
        if (isTRUE(r$n_excluded > 0)) div(class = "diagnosis-detail", strong(sprintf("%d fit(s) with an invalid Hessian are excluded from the ranking and interpretation.", r$n_excluded))) else NULL,
        if (r$n_dropped > 0) div(class = "diagnosis-detail", paste0(r$n_dropped, " rows with missing values in model variables were dropped from all models (", paste(r$drop_by, collapse = "; "), ").")) else NULL,
        if (warn > 0) div(class = "diagnosis-detail", strong(paste0(warn, " model(s) are classified Caution or Failed: see the fitting status below."))) else NULL)
  })

  output$aic_plot <- renderPlot({
    all_aic <- fitted_models()$aic
    aic <- all_aic[all_aic$Eligible, , drop = FALSE]
    shiny::validate(shiny::need(nrow(aic) > 0, "No eligible AIC values: check the fitting status and refit."))
    aic$Model <- factor(aic$Model, levels = rev(aic$Model))
    aic$lab <- paste0(sprintf("%.1f", aic$Delta_AIC), ifelse(aic$Fit == "Valid", "", paste0(" (", tolower(aic$Fit), ")")))
    excl <- all_aic$Model[!all_aic$Eligible]
    ggplot(aic, aes(Model, Delta_AIC, fill = Model, alpha = Fit)) + geom_col(width = 0.7) + coord_flip() +
      geom_hline(yintercept = 2, linetype = 2, colour = "grey45") +
      geom_text(aes(label = lab), hjust = -0.1, size = 3.5, colour = "#453A32", alpha = 1) +
      scale_fill_manual(values = METHOD_COLOURS, guide = "none") +
      scale_alpha_manual(values = c(Valid = 1, Caution = 0.4, Failed = 0.2), guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
      labs(x = NULL, y = "\u0394AIC", subtitle = paste0("Dashed line: \u0394AIC = 2; faded bars: Caution fits",
                                                       if (length(excl)) paste0("; excluded (invalid Hessian): ", paste(excl, collapse = ", ")) else "")) +
      theme_disappR(12)
  })

  # Models are compared on the rows every selected model can use. When a model could have used more, say so, name the
  # cost, and say what the user can do; when extra terms are added to some models only, say that too (0.20.18).
  output$model_rowset_warning <- renderUI({
    r <- safe_get(fitted_models())
    if (is.null(r) || !isTRUE(r$ok)) return(NULL)
    msgs <- list()
    rs <- r$row_sets
    if (is.data.frame(rs) && nrow(rs) && any(rs$Rows_usable > rs$Rows_used)) {
      short <- rs[rs$Rows_usable > rs$Rows_used, , drop = FALSE]
      short <- short[order(-short$Rows_usable), , drop = FALSE]
      msgs <- c(msgs, list(tagList(
        strong("The models share a smaller set of rows than some of them could use. "),
        sprintf("All models are fitted to the %s rows from %s individuals that every selected model can use, because AIC and the likelihood-ratio tests only compare models fitted to the same data. %s. %s",
                format(rs$Rows_used[[1]], big.mark = ","), format(rs$Individuals_used[[1]], big.mark = ","),
                paste(sprintf("%s could have used %s rows (%s individuals)", short$Model, format(short$Rows_usable, big.mark = ","),
                              format(short$Individuals_usable, big.mark = ",")), collapse = "; "),
                if (length(r$drop_by)) paste0("Rows were dropped for: ", paste(r$drop_by, collapse = "; "), ".") else ""),
        tags$br(), em("What you can do: fit the model on its own by deselecting the models that force the drop, fill in or unmap the variable causing it, or report the comparison on these shared rows and say so."))))
    }
    if (is.list(r$extra) && length(r$extra) && !setequal(names(r$extra), names(r$formulas))) {
      msgs <- c(msgs, list(tagList(
        strong("Extra terms were added to some models only. "),
        sprintf("%s carry terms the others do not, so the models no longer differ only in their selective-disappearance and appearance terms: AIC differences between them mix the two changes. Compare models with the same extra terms, or add the terms to all of them.",
                paste(vapply(intersect(names(r$formulas), names(r$extra)), model_label, character(1)), collapse = ", ")))))
    }
    if (any(vapply(c("M3", "M5"), function(m) m %in% names(r$formulas), logical(1))) && isTRUE(r$n_dropped > 0)) {
      msgs <- c(msgs, list(tagList(
        strong("Mean age and the within/between split. "),
        "Each individual's mean age, and the deviations from it, are computed from every row that individual has, not only from the shared rows, so that they describe the individual rather than the filter. Within the fitted rows the deviations therefore no longer average to exactly zero.")))
    }
    if (!length(msgs)) return(NULL)
    div(class = "diagnosis-card", style = "border-left: 5px solid #C77C02; margin-bottom: 8px;",
        div(class = "diagnosis-title", badge("caution"), " Read before comparing these models"),
        lapply(msgs, function(m) div(class = "diagnosis-detail", m)))
  })
  output$aic_table <- renderTable({
    aic <- fitted_models()$aic
    data.frame(Model = aic$Model, Fit = ifelse(aic$Eligible, aic$Fit, paste(aic$Fit, "(excluded)")), AIC = round(aic$AIC, 1),
               dAIC = round(aic$Delta_AIC, 1), Weight = round(aic$Weight, 3), df = aic$df, logLik = round(aic$logLik, 1), N = aic$N)
  }, striped = TRUE, spacing = "xs")

  output$lrt_table <- renderTable({
    l <- fitted_models()$lrt
    if (!nrow(l)) return(data.frame(Note = "Fit nested pairs (e.g. Models 1, 2 and 4) to obtain likelihood-ratio tests."))
    l$Chisq <- round(l$Chisq, 2)
    l$P_value <- vapply(l$P_value, format_p, character(1))
    l
  }, striped = TRUE, spacing = "xs")

  output$status_table <- renderTable({
    r <- current_models()
    st <- r$status
    if (!length(st)) return(NULL)
    out <- data.frame(Model = model_label(names(st)), Status = unlist(st, use.names = FALSE), check.names = FALSE, stringsAsFactors = FALSE)
    dg <- r$diagnostics
    if (is.data.frame(dg) && nrow(dg)) out <- cbind(out, dg[match(out$Model, dg$Model), setdiff(names(dg), c("Model", "Reported status")), drop = FALSE])
    out
  }, striped = TRUE, spacing = "xs", na = "")

  varcomp_display <- reactive({
    r <- fitted_models()
    vc <- r$varcomp
    shiny::validate(shiny::need(is.data.frame(vc) && nrow(vc) > 0, "No random-effect variances: this model has none to report."))
    m <- meta()
    grp_lab <- function(g) {
      out <- g
      glab <- if (isTRUE(m$has_group2) && isTRUE(m$nested)) paste0(m$map$group2, ":", m$map$group) else (m$map$group %||% "group")
      # uncorrelated slopes appear as separate terms "id.1", "id.2"...: they are still grouped by individual
      g <- sub("^id\\.[0-9]+$", "id", g)
      out[g == "id"] <- if (isTRUE(m$has_group) && isTRUE(m$nested)) paste0(glab, ":", m$map$id) else m$map$id
      out[g == "group"] <- glab
      out[g == "group2"] <- m$map$group2 %||% "group2"
      rt <- g %in% names(m$random_labels)
      out[rt] <- unname(m$random_labels[g[rt]])
      out
    }
    term_lab <- function(t) {
      t <- gsub("f1", "age (first ageing term)", t, fixed = TRUE)
      t <- gsub("f2", "age\u00b2 (second ageing term)", t, fixed = TRUE)
      gsub("f3", "age\u00b3 (third ageing term)", t, fixed = TRUE)
    }
    data.frame(Model = vc$Model, Group = grp_lab(vc$Group), Term = term_lab(vc$Term), Type = vc$Type,
               Variance = signif(vc$Variance %||% rep(NA_real_, nrow(vc)), 4),
               `SD / correlation / dispersion` = signif(vc$Estimate, 4), check.names = FALSE, stringsAsFactors = FALSE)
  })

  output$drop_note <- renderUI({
    r <- current_models()
    if (is.null(r$n_dropped) || r$n_dropped == 0) return(NULL)
    p(class = "small-note", paste0("Rows dropped for the common-data comparison: ", paste(r$drop_by, collapse = "; "),
                                   if (isTRUE(r$n_ids_dropped > 0)) sprintf("; %d individual(s) lost entirely", r$n_ids_dropped) else "", "."))
  })

  eligible_models <- reactive({
    r <- fitted_models()
    intersect(names(r$fits), sub("^Model ", "M", r$aic$Model[r$aic$Eligible]))
  })
  pred_curves <- reactive(pred_curves_for(fitted_models(), eligible_models()))
  # NULL = the tick boxes have not been used yet (draw every eligible model); character(0) = the user unticked all
  pred_sel <- reactiveVal(NULL)
  observeEvent(input$pred_models, disappr_guard("pred_models", pred_sel(as.character(input$pred_models %||% character(0)))),
               ignoreNULL = FALSE, ignoreInit = TRUE)
  pred_curves_shown <- reactive({
    r <- fitted_models()
    el <- eligible_models()
    chosen <- pred_sel()
    sel <- if (is.null(chosen)) el else intersect(chosen, el)
    by <- input$pred_by %||% ""
    # smooth curves on a fine age grid, or (tick box) predictions at the observed ages joined by straight lines
    ages <- if (isTRUE(input$pred_as_lines)) observed_prediction_ages(r$data$age, r$data$id, step = age_step_manual()) else smooth_prediction_ages(r$data$age)
    pred_curves_for(r, sel, if (nzchar(by)) by else NULL, ages = ages)
  })
  output$pred_models_ui <- renderUI({
    r <- safe_get(fitted_models())
    if (is.null(r)) return(NULL)
    ch <- safe_get(eligible_models()) %||% character(0)
    if (!length(ch)) return(NULL)
    chosen <- isolate(pred_sel())
    sel <- if (is.null(chosen)) ch else intersect(chosen, ch)
    if (!is.null(chosen) && length(chosen) && !length(sel)) sel <- ch   # after refitting with other models
    checkboxGroupInput("pred_models", "Models to draw", choices = stats::setNames(ch, model_label(ch)), selected = sel, inline = TRUE)
  })
  output$pred_by_ui <- renderUI({
    r <- safe_get(fitted_models())
    if (is.null(r)) return(NULL)
    cv <- intersect(r$covars %||% character(0), names(r$data))
    fac <- cv[!vapply(r$data[cv], is.numeric, logical(1))]
    if (!length(fac)) return(NULL)
    m <- meta()
    cur <- isolate(input$pred_by) %||% ""
    selectInput("pred_by", "Show predictions by", choices = c("(pooled)" = "", stats::setNames(fac, unname(m$cov_labels[fac]))),
                selected = if (cur %in% fac) cur else "")
  })
  obs_traj <- reactive(observed_trajectory(dat()))
  decomp_traj <- reactive(decomposition_trajectory(dat(), step = age_step_manual()))
  output$decomp_note <- renderUI({
    if (!isTRUE(input$show_decomp)) return(NULL)
    de <- safe_get(decomp_traj())
    cau <- if (is.data.frame(de)) attr(de, "caution") else NULL
    # an empty decomposition used to vanish without a word (0.22.7): say why
    if (is.data.frame(de) && !nrow(de))
      cau <- "not drawn: no individual is recorded at two consecutive occasions (for example cohorts sampled in alternate years). Setting the sampling interval does not change this."
    if (!length(cau) || !nzchar(cau[[1]])) return(NULL)
    div(class = "small-note", style = "border-left: 3px solid #B35806; padding-left: 7px; margin: 4px 0 8px;", strong("Decomposition: "), cau[[1]])
  })

  output$pred_plot <- renderPlot(pred_plot_obj())
  pred_plot_obj <- reactive({
    pc <- pred_curves_shown()
    shiny::validate(shiny::need(nrow(pc) > 0, "Choose at least one model to draw (or predictions could not be computed for the chosen models)."))
    by_on <- "level" %in% names(pc)
    p <- ggplot()
    if (isTRUE(input$show_raw_points)) {
      # every record's raw trait value, small and transparent, jittered slightly along age only
      dd <- dat()
      rp <- dd[is.finite(dd$trait) & is.finite(dd$age), , drop = FALSE]
      if (by_on) {
        byv <- input$pred_by
        rp <- rp[!is.na(rp[[byv]]) & as.character(rp[[byv]]) %in% unique(pc$level), , drop = FALSE]
        rp$level <- as.character(rp[[byv]])
      }
      if (nrow(rp)) {
        st <- resolve_age_step(rp$age, rp$id, age_step_manual())
        if (!is.finite(st) || st <= 0) st <- max(diff(range(rp$age)), 1) / 50
        p <- p + geom_point(data = rp, aes(age, trait), colour = "grey25", alpha = 0.15, size = 0.8,
                            position = position_jitter(width = 0.12 * st, height = 0, seed = 1))
      }
    }
    if (isTRUE(input$show_observed)) {
      ob <- if (by_on) {
        byv <- input$pred_by
        dd <- dat()
        parts <- lapply(unique(pc$level), function(lv) {
          o <- observed_trajectory(dd[!is.na(dd[[byv]]) & as.character(dd[[byv]]) == lv, , drop = FALSE])
          if (!nrow(o)) return(NULL)
          o$level <- lv
          o
        })
        parts <- Filter(Negate(is.null), parts)
        if (length(parts)) do.call(rbind, parts) else NULL
      } else obs_traj()
      if (!is.null(ob) && nrow(ob)) {
        ob$Method <- "Observed"
        p <- p + geom_point(data = ob, aes(age, fitted, size = n), shape = 21, fill = "grey55", colour = "black", stroke = 0.6, alpha = 0.85)
      }
    }
    as_lines <- isTRUE(input$pred_as_lines)
    p <- p + geom_line(data = pc, aes(age, fitted, colour = Method, linetype = Fit), linewidth = if (as_lines) 0.9 else 1.1)
    if (as_lines && length(unique(pc$age)) <= 60) p <- p + geom_point(data = pc, aes(age, fitted, colour = Method), size = 1.9)
    if (isTRUE(input$show_a3) && !by_on) {
      z <- safe_get(a3_fit())
      if (!is.null(z) && is.null(z$error) && isTRUE(nrow(z$mean_curve) > 0)) {
        mc <- z$mean_curve
        a3 <- data.frame(age = mc$age, fitted = mc$fitted, Method = "Individual fits: mean of coefficients")
        if (isTRUE(z$nonlinear)) a3 <- rbind(a3, data.frame(age = mc$age, fitted = mc$fitted_fun, Method = "Individual fits: mean of functions"))
        a3 <- a3[is.finite(a3$fitted), , drop = FALSE]
        # parts far outside the models' predictions are left out (a line break) rather than stretching the axis
        pr <- range(pc$fitted[is.finite(pc$fitted)])
        if (all(is.finite(pr))) {
          sp <- max(diff(pr), abs(mean(pr)) * 0.1, 1e-8)
          a3$fitted[a3$fitted < pr[1] - 3 * sp | a3$fitted > pr[2] + 3 * sp] <- NA_real_
        }
        if (any(is.finite(a3$fitted))) p <- p + geom_line(data = a3, aes(age, fitted, colour = Method), linewidth = 1.2, linetype = "dotdash", na.rm = TRUE)
      }
    }
    if (isTRUE(input$show_decomp) && !by_on) {
      de <- decomp_traj()
      if (nrow(de)) {
        de$Method <- "Decomposition"
        de$.seg <- if ("segment" %in% names(de)) de$segment else 1L
        p <- p + geom_line(data = de, aes(age, fitted, colour = Method, group = .seg), linewidth = 1.1, linetype = 2)
      }
    }
    tc <- truth()
    if (!is.null(tc)) {
      ag <- sort(unique(c(pc$age)))
      tr <- data.frame(age = ag, fitted = toy_true_curve(tc, ag), Method = "True (simulated)")
      p <- p + geom_line(data = tr, aes(age, fitted, colour = Method), linewidth = 1.6)
    }
    tl <- truth_label(truth())
    p + scale_colour_manual(values = METHOD_COLOURS, labels = function(x) wrap_label(ifelse(x == "True (simulated)", tl, x), 18)) +
      scale_size_area(max_size = 4, guide = "none") +
      scale_linetype_manual(values = c(Valid = "solid", Caution = "dashed", Failed = "dotted"), guide = "none") +
      (if (by_on) facet_wrap(~ level) else NULL) +
      labs(x = "Age", y = current_map()$trait, colour = NULL,
           subtitle = paste0(if (!is.null(tc)) "Black: simulated typical-individual trajectory; " else "",
                             if (!by_on) "dashed purple: decomposition; dot-dash: reconstruction from individual fits (if shown); " else "panels: levels of the chosen covariate; ",
                             if (isTRUE(input$show_raw_points)) "small grey dots: individual records (slightly jittered); " else "",
                             if (as_lines) "model predictions at the observed ages, joined by lines; " else "",
                             "points: observed means; dashed model lines: Caution fits")) +
      theme_disappR(13) + theme(legend.text = element_text(size = 10)) + guides(colour = guide_legend(ncol = 3, byrow = TRUE))
  })

  output$deviation_note <- renderUI({
    if (!is_toy()) return(p(class = "small-note", "The true trajectory is unknown for empirical data. Use a simulated dataset to see how each model and the decomposition recover it."))
    p(class = "small-note", "Relativised deviation D = 100 \u00d7 (estimate \u2212 truth) / truth (Methods Eq. 11), over ages with \u2265 10 observed individuals.")
  })

  output$deviation_table <- renderTable(deviation_tab(), striped = TRUE, spacing = "xs")
  deviation_tab <- reactive({
    dv <- deviation_raw()
    if (is.null(dv) || !nrow(dv)) return(NULL)
    dv$Mean_abs_D <- round(dv$Mean_abs_D, 1)
    dv$Max_abs_D <- round(dv$Max_abs_D, 1)
    dv$D_at_oldest_age <- round(dv$D_at_oldest_age, 1)
    names(dv) <- c("Method", "Mean |D| %", "Max |D| %", "D at oldest %", "Oldest age")
    dv
  })
  deviation_raw <- reactive({
    shiny::validate(shiny::need(is_toy(), ""))
    pc <- pred_curves()
    tc <- truth()
    ob <- obs_traj()
    parts <- list(pc[, c("age", "fitted", "Method"), drop = FALSE])
    ob2 <- ob[, c("age", "fitted")]
    ob2$Method <- "Observed"
    parts <- c(parts, list(ob2))
    de <- decomp_traj()
    if (nrow(de)) {
      de2 <- de[, c("age", "fitted")]
      de2$Method <- "Decomposition"
      parts <- c(parts, list(de2))
    }
    allc <- do.call(rbind, parts)
    deviation_from_truth(allc[, c("age", "fitted", "Method")], tc, data.frame(age = ob$age, n = ob$n))
  })
  output$consistency_ui <- renderUI({
    r <- safe_get(fitted_models())
    if (is.null(r)) return(NULL)
    dev <- if (isTRUE(safe_get(is_toy()))) safe_get(deviation_raw()) else NULL
    tr <- safe_get(a2_slope_trend(a2_slopes()))
    notes <- tryCatch(consistency_notes(r, dev, if (is.data.frame(tr) && nrow(tr) == 1) tr else NULL), error = function(e) character(0))
    tagList(
      tags$details(class = "info-more", tags$summary(info_title(strong("Internal consistency"), "consistency")),
      if (length(notes)) {
        div(class = "diagnosis-card", lapply(notes, function(x) div(class = "diagnosis-detail", icon("exclamation-triangle"), " ", x)))
      } else {
        p(class = "small-note", "No inconsistencies flagged between AIC, likelihood-ratio tests, fit validity, random-slope support",
          if (isTRUE(safe_get(is_toy()))) ", recovery of the simulated truth" else "", " and the trend of the lifespan\u2013trait coefficient across age bins. This does not prove that the evidence is coherent.")
      }))
  })

  output$coef_model_ui <- renderUI({
    r <- safe_get(fitted_models())
    if (is.null(r)) return(NULL)
    ch <- names(r$fits)
    cur <- isolate(input$coef_model)
    selectInput("coef_model", NULL, choices = stats::setNames(ch, model_label(ch)),
                selected = if (isTRUE(cur %in% ch)) cur else ch[[length(ch)]])
  })

  output$coef_table <- renderTable({
    r <- fitted_models()
    req(input$coef_model)
    x <- r$coefficients[r$coefficients$Model == model_label(input$coef_model), , drop = FALSE]
    if (!nrow(x)) return(data.frame(Note = "No coefficient table."))
    tab <- coef_display(x, r)
    tab <- tab[, c("Term", "Per", setdiff(names(tab), c("Term", "Per"))), drop = FALSE]
    names(tab)[names(tab) == "Per"] <- "Term description"
    # age terms and lifespan-proxy terms (ALR, AFR, LS, mean age; additive and interactive) in bold, easy to find
    chr <- vapply(tab, is.character, logical(1))
    tab[chr] <- lapply(tab[chr], function(v) as.character(htmltools::htmlEscape(v)))
    key <- key_age_proxy_terms(x$Raw_term)
    tab$Term[key] <- paste0("<strong>", tab$Term[key], "</strong>")
    tab
  }, striped = TRUE, spacing = "xs", sanitize.text.function = function(s) s)
  output$scaling_table <- renderTable({
    r <- fitted_models()
    sc <- scaling_constants(r)
    data.frame(Variable = sc$Variable, Centre = signif(sc$Centre, 5), `Scale (SD)` = signif(sc$Scale, 5), check.names = FALSE)
  }, striped = TRUE, spacing = "xs")
  coef_interpretation_text <- reactive({
    r <- fitted_models()
    req(input$coef_model)
    tryCatch(interpret_model_terms(r, input$coef_model), error = function(e) paste("Interpretation not available:", conditionMessage(e)))
  })
  output$coef_interpretation <- renderUI({
    txt <- safe_get(coef_interpretation_text())
    if (is.null(txt) || !length(txt)) return(NULL)
    div(class = "small-note", lapply(txt, p))
  })

  coef_re_tab <- reactive({
    r <- fitted_models()
    req(input$coef_model)
    vd <- safe_get(varcomp_display())
    if (is.null(vd)) return(data.frame(Note = "No random-effect estimates."))
    x <- vd[vd$Model == model_label(input$coef_model), setdiff(names(vd), "Model"), drop = FALSE]
    if (!nrow(x)) return(data.frame(Note = "No random-effect estimates."))
    rownames(x) <- NULL
    for (cl in intersect(c("Variance", "SD / correlation / dispersion"), names(x))) x[[cl]] <- sig_text(x[[cl]], 3)
    # terms that explain (next to) no variance are flagged, with advice below the table
    vc <- r$varcomp[r$varcomp$Model == model_label(input$coef_model), , drop = FALSE]
    fl <- negligible_random_terms(vc, data = r$data)
    if (nrow(fl) == nrow(x) && any(fl$flag)) x$Flag <- ifelse(fl$flag, "\u26a0 explains no variance", "")
    x
  })
  output$coef_re_table <- renderTable(coef_re_tab(), striped = TRUE, spacing = "xs")
  output$coef_re_flags <- renderUI({
    r <- safe_get(fitted_models())
    m <- input$coef_model
    if (is.null(r) || is.null(m) || !is.data.frame(r$varcomp)) return(NULL)
    vc <- r$varcomp[r$varcomp$Model == model_label(m), , drop = FALSE]
    fl <- negligible_random_terms(vc, data = r$data)
    if (!any(fl$flag)) return(NULL)
    vd <- safe_get(varcomp_display())
    if (is.null(vd)) return(NULL)
    vd <- vd[vd$Model == model_label(m), , drop = FALSE]
    if (nrow(vd) != nrow(vc)) return(NULL)
    msgs <- vapply(which(fl$flag), function(i) random_term_flag_text(vd$Group[[i]], vd$Term[[i]], identical(vc$Group[[i]], "id"),
                                                                     fl$share[[i]], fl$sd[[i]]), character(1))
    div(class = "diagnosis-card", style = "border-left: 5px solid #C77C02; margin-top: 6px;",
        div(class = "diagnosis-title", badge("caution"), " Random terms that explain no variance"),
        lapply(msgs, function(t) div(class = "diagnosis-detail", t)))
  })


  # ---- B1: family check ----
  family_res <- reactiveVal(NULL)
  observeEvent(input$run_family_check, disappr_guard("input$run_family_check", {
    d <- safe_get(dat())
    if (is.null(d)) return(invisible(NULL))
    s <- model_settings()
    res <- run_with_progress("Comparing error families", function(pr) {
      fams <- if ((s$family %||% "gaussian") %in% BINOMIAL_FAMILIES) BINOMIAL_FAMILIES
              else if ((s$family %||% "gaussian") %in% DENSITY_FAMILIES) DENSITY_FAMILIES else COUNT_FAMILIES
      compare_families(d, meta(), model = input$family_check_model %||% "M4", age_function = s$age_function,
                       random_slope = s$random_slope, standardise = s$standardise, zi_str = s$zi, progress = pr,
                       among = s$among, include_invalid = s$include_invalid, families = fams, disp = s$disp)
    })
    res$signature <- paste(data_sig(), input$family_check_model, s$age_function, settings_sig(s))
    family_res(res)
  }))
  output$family_table <- renderTable({
    r <- family_res()
    shiny::validate(shiny::need(!is.null(r), "Press 'Compare error families'."))
    s_now <- model_settings()
    shiny::validate(shiny::need(identical(r$signature, paste(data_sig(), input$family_check_model, s_now$age_function, settings_sig(s_now))),
                  "Data or settings changed since the comparison: press 'Compare error families' again."))
    shiny::validate(shiny::need(isTRUE(r$ok), r$message))
    tab <- r$table
    tab$AIC <- round(tab$AIC, 1)
    tab$Delta_AIC <- round(tab$Delta_AIC, 1)
    tab
  }, striped = TRUE, spacing = "xs")

  # ======================================================================
  # 2b. A4-A7 disappearance diagnostics
  # ======================================================================
  a4_ia <- reactive(disappearance_data(dat(), meta(), FALSE))
  a5_data <- reactive(terminal_data(dat(), meta(), 3))
  a5_plot_obj <- reactive({
    z <- a5_data()
    shiny::validate(shiny::need(is.data.frame(z) && nrow(z) > 0, "Too few individuals at each occasion before death: this figure needs more individuals with a known end."))
    known <- isTRUE(attr(z, "lifespan_known"))
    u <- sort(unique(z$before))
    ggplot(z, aes(before, trait, colour = end_bin, group = end_bin)) +
      geom_line(linewidth = 1.1) + geom_point(aes(size = n)) +
      scale_x_reverse(breaks = if (length(u) <= 15) u else waiver()) +
      scale_colour_manual(values = ordered_colours(nlevels(factor(z$end_bin)))) + scale_size_area(max_size = 4.5, guide = "none") +
      labs(x = if (known) "Occasions before death (0 = last occasion before death)" else "Occasions before the last record (0 = last record)",
           y = paste("Mean", trait_label()),
           colour = if (known) "Lifespan" else "ALR",
           title = "Trait before death", subtitle = "Individuals aligned on time before death, by lifespan tercile; point size = individuals") +
      theme_disappR(13)
  })
  output$a5_plot <- renderPlot(a5_plot_obj())
  a5_lines <- reactive({
    z <- a5_data()
    if (!is.data.frame(z) || !nrow(z)) return(character(0))
    parts <- lapply(split(z, z$end_bin), function(x) {
      last <- x$resid[x$before == 0]
      earlier <- x$resid[x$before >= 2]
      if (!length(last) || !length(earlier)) return(NULL)
      dd <- last[[1]] - mean(earlier)
      sprintf("%s: %s%s", as.character(x$end_bin[[1]]), if (dd >= 0) "+" else "\u2212", format_num(abs(dd)))
    })
    parts <- unlist(Filter(Negate(is.null), parts))
    if (!length(parts)) return("Too few occasions before death to compare the final occasion with earlier ones.")
    paste0("Change at the last occasion relative to occasions two or more before death (relative to the mean at the same age): ", paste(parts, collapse = "; "),
           ". Look for the same direction in every lifespan group: a rise at the last occasion may indicate terminal investment, a drop a terminal decline.",
           if (!isTRUE(attr(z, "lifespan_known"))) " Lifespan is unknown, so the last record may not be the last occasion before death." else "")
  })
  output$a5_note <- renderUI({
    txt <- safe_get(a5_lines())
    if (is.null(txt) || !length(txt)) return(NULL)
    p(class = "small-note", txt)
  })

  a6_data <- reactive(selection_differentials(a4_ia()))
  a6_values <- reactive({
    s <- a6_data()
    contrast <- identical(input$a6_compare, "contrast")
    if (nrow(s)) {
      s$y <- if (contrast) s$Contrast else s$Differential
      s$se <- if (contrast) s$Contrast_SE else s$Differential_SE
    }
    s
  })
  a6_trend <- reactive({
    s <- a6_values()
    if (nrow(s) < 3) return(NULL)
    ok <- is.finite(s$y) & is.finite(s$se) & s$se > 0
    if (sum(ok) < 3) return(NULL)
    x <- s[ok, , drop = FALSE]
    fit <- stats::lm(y ~ Age, data = x, weights = 1 / x$se^2)
    cf <- suppressWarnings(summary(fit)$coefficients)
    if (!"Age" %in% rownames(cf)) return(NULL)
    list(slope = cf["Age", 1], p = cf["Age", 4], mean = stats::weighted.mean(x$y, 1 / x$se^2), n = nrow(x))
  })
  a6_plot_obj <- reactive({
    s <- a6_values()
    shiny::validate(shiny::need(nrow(s) > 0, "No age with at least three survivors and three disappearing individuals: this figure needs more data at each age."))
    contrast <- identical(input$a6_compare, "contrast")
    s$w <- 1 / pmax(s$se, 1e-9)^2
    ggplot(s, aes(Age, y)) + geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
      geom_errorbar(aes(ymin = y - 1.96 * se, ymax = y + 1.96 * se), width = 0, colour = "#0072B2", alpha = 0.7) +
      geom_point(aes(size = N), colour = "#0072B2") +
      geom_smooth(aes(weight = w), method = "lm", formula = y ~ x, se = FALSE, colour = "#D55E00", linewidth = 1, na.rm = TRUE) +
      scale_size_area(max_size = 5, guide = "none") +
      labs(x = "Age", y = if (contrast) "Survivors \u2212 disappearing (within-age SD)" else "Selection differential (within-age SD)",
           title = "Selection differentials by age", subtitle = "Points \u00b1 95% intervals; orange line: inverse-variance weighted trend across age") +
      theme_disappR(13)
  })
  output$a6_plot <- renderPlot(a6_plot_obj())
  a6_lines <- reactive({
    tr <- a6_trend()
    if (is.null(tr)) return("Fewer than three ages with enough survivors and disappearing individuals to describe a trend.")
    paste0(sprintf("Weighted mean %s: %s within-age SD; trend across age %s per unit age (p = %s). ",
                   if (identical(input$a6_compare, "contrast")) "survivor\u2013disappearing difference" else "selection differential",
                   format_num(tr$mean), format_num(tr$slope), format_p(tr$p)),
           if (is.finite(tr$p) && tr$p < 0.05) "The differential changes with age, consistent with age-dependent selective disappearance."
           else "No clear trend across age, consistent with age-independent selective disappearance (or too little information).")
  })
  output$a6_note <- renderUI({
    txt <- safe_get(a6_lines())
    if (is.null(txt)) return(NULL)
    p(class = "small-note", txt)
  })

  a7_data <- reactive(life_table(dat(), meta()))
  a7_plot_obj <- reactive({
    z <- a7_data()
    shiny::validate(shiny::need(isTRUE(z$ok), z$message %||% "No hazard could be estimated."))
    lt <- z$table
    ggplot(lt, aes(Age, Hazard)) +
      geom_hline(yintercept = z$overall, linetype = 2, colour = "#D55E00", linewidth = 1) +
      geom_errorbar(aes(ymin = Lower_95, ymax = Upper_95), width = 0, colour = "#0072B2", alpha = 0.7) +
      geom_line(colour = "#0072B2", alpha = 0.6) + geom_point(aes(size = At_risk), colour = "#0072B2") +
      scale_size_area(max_size = 5, guide = "none") +
      scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
      labs(x = "Age", y = "Probability of disappearing before the next occasion",
           title = "Disappearance hazard", subtitle = "Points: age-specific hazard \u00b1 95% Wilson interval (size = at risk); dashed: overall, age-independent hazard") +
      theme_disappR(13)
  })
  output$a7_plot <- renderPlot(a7_plot_obj())
  a7_lines <- reactive({
    z <- a7_data()
    if (!isTRUE(z$ok)) return(z$message %||% "")
    c(sprintf("Overall (age-independent) hazard: %.1f%% per occasion (%s disappearances in %s individual-occasions at risk).",
              100 * z$overall, format(sum(z$table$Disappearances), big.mark = ","), format(sum(z$table$At_risk), big.mark = ",")),
      sprintf("Test of a constant hazard against age-specific hazards: p = %s%s.", format_p(z$p_constant),
              if (is.finite(z$p_constant) && z$p_constant < 0.05) ": the hazard changes with age" else ": no clear change with age"),
      if (!isTRUE(z$lifespan_known)) "Lifespan is unknown: disappearance is the last record, which includes emigration and missed detections." else NULL)
  })
  output$a7_note <- renderUI({
    txt <- safe_get(a7_lines())
    if (is.null(txt)) return(NULL)
    div(class = "small-note", lapply(txt, p))
  })
  # ---- sampling caveat and A3 data support
  output$sampling_caveat <- renderUI({
    sch <- safe_get(integrity()$schedule)
    if (is.null(sch)) return(NULL)
    if (isTRUE(sch$irregular)) {
      div(class = "diagnosis-card", style = "border-left: 5px solid #A50026;",
          div(class = "diagnosis-title", icon("exclamation-triangle"), " Sampling is irregular: read the missingness grid with caution"),
          div(class = "diagnosis-detail", sprintf("%.0f%% of records are not on a common sampling schedule (step %s). The missingness grid assumes regular, scheduled occasions and is most reliable under regular sampling; here expected occasions are approximate, so missingness percentages, patterns and drivers are indicative only. Round ages to a sampling resolution on the Data tab for a more reliable grid.",
                                                  100 * sch$off_share, format_num(sch$step))))
    } else if (isTRUE(sch$n_schedules > 1)) {
      p(class = "small-note", sprintf("Individuals follow %d offset sampling schedules; expected occasions are anchored on each individual's first record. The grid is most reliable under regular sampling.", sch$n_schedules))
    } else {
      p(class = "small-note", "The missingness grid assumes regular, scheduled sampling occasions, which these data appear to follow.")
    }
  })


  # ---- performance diagnostics
  perf_res <- reactiveVal(NULL)
  output$performance_model_ui <- renderUI({
    r <- safe_get(fitted_models())
    if (is.null(r)) return(p(class = "small-note", "Fit models first."))
    ch <- names(r$fits)
    selectInput("performance_model", "Model", choices = stats::setNames(ch, model_label(ch)), selected = sub("^Model ", "M", r$aic$Model[[1]]))
  })
  observeEvent(input$run_performance, disappr_guard("input$run_performance", {
    r <- safe_get(fitted_models())
    m <- input$performance_model
    if (is.null(r) || is.null(m) || is.null(r$fits[[m]])) {
      notify("Fit models first.", type = "warning")
      return(invisible(NULL))
    }
    res <- tryCatch(run_with_progress("Running performance checks", function(pr) run_performance(r$fits[[m]], r$family)),
                    error = function(e) list(ok = FALSE, message = paste("The performance checks failed for this model:", conditionMessage(e))))
    res$model <- m
    res$signature <- model_sig()
    perf_res(res)
    if (!isTRUE(res$ok)) notify(res$message, type = "error", duration = 8)
  }))
  output$performance_table <- renderTable({
    z <- perf_res()
    shiny::validate(shiny::need(!is.null(z), "Press 'Run performance checks'."))
    shiny::validate(shiny::need(isTRUE(z$ok), z$message %||% "The performance checks failed."))
    shiny::validate(shiny::need(identical(z$signature, model_sig()), "Models were refitted: run the checks again."))
    cbind(Model = model_label(z$model), z$table, stringsAsFactors = FALSE)
  }, striped = TRUE, spacing = "xs")
  output$performance_note <- renderUI({
    z <- perf_res()
    if (is.null(z) || !isTRUE(z$ok) || !nzchar(z$plot_message %||% "")) return(NULL)
    if (!identical(z$signature, model_sig())) return(NULL)
    p(class = "small-note", z$plot_message)
  })
  output$performance_plot <- renderPlot({
    z <- perf_res()
    shiny::validate(shiny::need(!is.null(z) && isTRUE(z$ok) && !is.null(z$check_model), ""))
    shiny::validate(shiny::need(identical(z$signature, model_sig()), ""))
    draw_check_model(z$check_model)()
  })

  script_data_lines <- function() {
    src <- input$data_source %||% "toy"
    lines <- "# ---- Data ----"
    if (identical(src, "upload")) {
      lines <- c(lines, paste0("raw <- read_user_csv(", dput_text(input$data_file$name %||% "your_data.csv"), ")$data  # the uploaded file, placed in the working directory"))
    } else if (identical(src, "example")) {
      lines <- c(lines, sprintf("raw <- utils::read.csv(system.file(\"app\", \"data\", %s, package = \"disappR\"), stringsAsFactors = FALSE, check.names = FALSE)",
                                dput_text(current_example()$file)))
    } else {
      lines <- c(lines, paste0("raw <- simulate_toy_data(", dput_text(toy_cfg()), ")"))
    }
    sv <- input$subset_var %||% ""
    if (nzchar(sv) && length(input$subset_levels)) {
      lines <- c(lines, sprintf("raw <- raw[!is.na(raw[[%s]]) & as.character(raw[[%s]]) %%in%% %s, , drop = FALSE]  # subset used in the app",
                                dput_text(sv), dput_text(sv), dput_text(as.character(input$subset_levels))))
    }
    c(lines,
      paste0("map <- ", dput_text(current_map())),
      paste0("prepared <- standardise_data(raw, map, ", dput_text(input$dup_action %||% "keep"), ")"),
      "dat <- prepared$data   # one row per individual x age",
      "meta <- prepared$meta",
      if (!is.null(age_step_manual())) paste0("meta$age_step <- ", format(age_step_manual(), digits = 10), "   # sampling interval set in the app (leave out to infer it from the ages)") else NULL,
      "im <- individual_metrics(dat)   # one row per individual: ALR, AFR, mean age, lifespan",
      "")
  }
  data_entries <- function(...) c("read_user_csv", "standardise_data", "individual_metrics", if (identical(input$data_source %||% "toy", "toy")) "simulate_toy_data", ...)

  section_code <- function(section) {
    lines <- switch(section,
      data = c(
        script_header(data_entries("data_integrity")),
        script_data_lines(),
        "# ---- Data integrity checks ----",
        "integrity <- data_integrity(dat, meta)",
        "print(integrity$table[, c(\"Check\", \"Result\", \"Status\")])",
        "",
        "# ---- Distribution of a variable ----",
        paste0("variable <- ", dput_text(input$dist_var %||% current_map()$trait)),
        "values <- raw[[variable]]",
        "num <- suppressWarnings(as.numeric(values))",
        "if (mean(is.finite(num[!is.na(values)])) > 0.9) {",
        "  print(ggplot(data.frame(x = num[is.finite(num)]), aes(x)) + geom_histogram(bins = 30, fill = \"#0072B2\", colour = \"white\") +",
        "          labs(x = variable, y = \"Rows\") + theme_minimal())",
        "} else {",
        "  tab <- sort(table(values), decreasing = TRUE)",
        "  print(ggplot(data.frame(level = names(tab), n = as.numeric(tab)), aes(stats::reorder(level, n), n)) + geom_col(fill = \"#0072B2\") +",
        "          coord_flip() + labs(x = variable, y = \"Rows\") + theme_minimal())",
        "}",
        "print(utils::head(raw, 10))"),
      visual = c(
        script_header(data_entries("mode_or_na", "individual_ids", "proxy_values", "binned_trajectory", "bin_differences", "bin_difference_trends",
                                   "trait_by_age_bins", "a2_bin_slopes", "a2_slope_trend", "disappearance_data", "terminal_data",
                                   "selection_differentials", "life_table")),
        script_data_lines(),
        "# ---- Settings (copied from the app) ----",
        paste0("proxy <- ", dput_text(safe_get(vis_px()) %||% "ALR"), "   # \"ALR\", \"Mean age\", \"LS\" or \"AFR\""),
        paste0("n_bins <- ", dput_text(as.numeric(input$n_bins %||% 4))),
        paste0("bin_method <- ", dput_text(input$bin_method %||% "quantile"), "   # \"equal\" or \"quantile\""),
        paste0("trait_scale <- ", dput_text(input$trait_scale %||% "raw"), "   # \"raw\" or \"log1p\""),
        paste0("facet_column <- ", dput_text(input$facet_var %||% ""), "   # \"\" for no panels (trait against the grouping variable)"),
        paste0("facet_column_trajectory <- ", dput_text(safe_get(a1_facet_var()) %||% ""), "   # panels of the trajectory and bin-difference figures"),
        "",
        "vis <- dat",
        "if (identical(trait_scale, \"log1p\")) vis$trait <- log1p(vis$trait)",
        "make_facet <- function(column) {",
        "  if (!nzchar(column)) return(NULL)",
        "  ids <- individual_ids(raw, map)   # the individual IDs used by the app (group + ID when nested)",
        "  lv <- as.character(raw[[column]])",
        "  ok <- !is.na(ids) & nzchar(ids) & !is.na(lv) & nzchar(lv)",
        "  per <- tapply(lv[ok], ids[ok], mode_or_na)",
        "  stats::setNames(paste0(column, \": \", as.character(per)), names(per))",
        "}",
        "facet <- make_facet(facet_column)",
        "facet_traj <- make_facet(facet_column_trajectory)",
        "panels <- if (is.null(facet)) NULL else facet_wrap(~ facet)",
        "panels_traj <- if (is.null(facet_traj)) NULL else facet_wrap(~ facet)",
        "pvals <- proxy_values(im, proxy)",
        "",
        "# ---- Trait trajectory within bins (at least 3 individuals per point) ----",
        "traj <- binned_trajectory(vis, pvals, n_bins, bin_method, 3, facet = facet_traj)",
        "print(ggplot(traj, aes(age, mean, colour = bin, group = bin)) + geom_line(linewidth = 1) + geom_point(aes(size = n)) + panels_traj +",
        "        labs(x = \"Age\", y = \"Mean trait\", colour = paste(proxy, \"bin\"), size = \"Individuals\") + theme_minimal())",
        "",
        "# ---- Difference between consecutive bins at each age (least-squares lines with 95% confidence bands) ----",
        "diffs <- bin_differences(traj, \"successive\")",
        "print(bin_difference_trends(diffs))",
        "print(ggplot(diffs, aes(age, difference, colour = pair)) + geom_hline(yintercept = 0, linetype = 2) + geom_point() +",
        "        geom_smooth(aes(fill = pair), method = \"lm\", formula = y ~ x, se = TRUE, alpha = 0.15) + panels_traj +",
        "        labs(x = \"Age\", y = \"Difference in mean trait\", colour = NULL, fill = NULL) + theme_minimal())",
        "",
        "# ---- Trait against the grouping variable within age bins (at least 3 individuals per bin) ----",
        "a2 <- trait_by_age_bins(vis, pvals, n_bins, facet = facet)",
        "a2 <- a2[stats::ave(rep(1, nrow(a2)), a2$facet, a2$age_bin, FUN = length) >= 3, , drop = FALSE]",
        "slopes <- a2_bin_slopes(a2)",
        "print(slopes)",
        "print(a2_slope_trend(slopes))",
        "print(ggplot(a2, aes(proxy, trait, colour = age_bin)) + geom_point(alpha = 0.35) +",
        "        geom_smooth(aes(group = age_bin, fill = age_bin), method = \"lm\", formula = y ~ x, se = TRUE, alpha = 0.15) + panels +",
        "        labs(x = proxy, y = \"Mean trait within the age bin\", colour = \"Age bin\", fill = \"Age bin\") + theme_minimal())",
        "",
        "# ---- Disappearance diagnostics ----",
        "terminal <- terminal_data(dat, meta, 3)",
        "if (nrow(terminal)) print(ggplot(terminal, aes(before, trait, colour = end_bin, group = end_bin)) + geom_line() + geom_point(aes(size = n)) +",
        "                         scale_x_reverse() + labs(x = \"Occasions before death\", y = \"Mean trait\", colour = \"Lifespan\") + theme_minimal())",
        "sel <- selection_differentials(disappearance_data(dat, meta, FALSE))",
        "print(sel)",
        "if (nrow(sel)) print(ggplot(sel, aes(Age, Differential)) + geom_hline(yintercept = 0, linetype = 2) +",
        "                    geom_errorbar(aes(ymin = Differential - 1.96 * Differential_SE, ymax = Differential + 1.96 * Differential_SE), width = 0) +",
        "                    geom_point(aes(size = N)) + labs(y = \"Selection differential (within-age SD)\") + theme_minimal())",
        "hazard <- life_table(dat, meta)",
        "if (isTRUE(hazard$ok)) {",
        "  print(hazard$table)",
        "  cat(\"Overall hazard:\", round(hazard$overall, 3), \"; test of a constant hazard: p =\", signif(hazard$p_constant, 3), \"\\n\")",
        "  print(ggplot(hazard$table, aes(Age, Hazard)) + geom_hline(yintercept = hazard$overall, linetype = 2) +",
        "          geom_errorbar(aes(ymin = Lower_95, ymax = Upper_95), width = 0) + geom_point(aes(size = At_risk)) +",
        "          labs(y = \"Probability of disappearing before the next occasion\") + theme_minimal())",
        "}"),
      sampling = c(
        script_header(data_entries("build_missing_grid", "missingness_summary", "missingness_by_variable", "grid_display")),
        script_data_lines(),
        "# ---- Settings (copied from the app) ----",
        paste0("start_age <- ", dput_text(miss_start_age()), "  # age at first trait expression (NA unless missingness is counted from AFE)"),
        "start_mode <- if (is.finite(start_age)) \"same\" else \"afr\"",
        paste0("n_show <- ", dput_text(as.numeric(safe_get(heat_n()) %||% 150))),
        "life_known <- isTRUE(meta$has_life) && !isTRUE(meta$life_auto)",
        "",
        "# ---- Expected occasions and missingness ----",
        "grid <- build_missing_grid(dat, margin = 0, start_mode = start_mode, start_age = start_age, step = meta$age_step)",
        "ms <- missingness_summary(dat, grid, 0.05, life_known)",
        "cat(sprintf(\"Missing expected occasions: %.1f%%\\n\", ms$percent))",
        "print(ms$drivers)",
        "cat(ms$guidance, \"\\n\")",
        "",
        "# ---- Sampling grid (individuals ordered by ALR) ----",
        "ids <- unique(grid$id)",
        "ids <- ids[order(im$alr[match(ids, im$id)], na.last = FALSE)]    # by the ALR the models use",
        "if (length(ids) > n_show) ids <- ids[unique(round(seq(1, length(ids), length.out = n_show)))]",
        "cells <- grid_display(grid, ids, life_known = any(is.finite(dat$life)))",
        "cells$id <- factor(cells$id, levels = rev(ids))",
        "print(ggplot(cells, aes(age, id, fill = status)) + geom_tile() +",
        "        scale_fill_manual(values = c(\"Observed\" = \"#7C8060\", \"Missed\" = \"#C0392B\", \"First record (AFR)\" = \"#2C6FAC\", \"Last record (ALR)\" = \"#E67E22\", \"AFR = ALR (one occasion)\" = \"#7B4F9E\", \"Not expected\" = \"#FFFFFF\")) +",
        "        labs(x = \"Age\", y = NULL, fill = NULL) + theme_minimal() + theme(axis.text.y = element_blank()))",
        "",
        "# ---- Missingness by age ----",
        "by_age <- missingness_by_variable(dat, grid, \"Age\")",
        "print(ggplot(by_age, aes(value, 100 * missing_rate)) + geom_line() + geom_point(aes(size = n)) +",
        "        labs(x = \"Age\", y = \"% of expected occasions missed\") + theme_minimal())",
        "",
        "# ---- Agreement between lifespan proxies ----",
        "cat(\"r(mean age, ALR) =\", round(stats::cor(im$mean_age, im$alr, use = \"complete.obs\"), 3), \"\\n\")",
        "print(ggplot(im, aes(mean_age, alr)) + geom_point(alpha = 0.5) + geom_abline(slope = 1, intercept = 0, linetype = 3) +",
        "        geom_smooth(method = \"lm\", formula = y ~ x, se = FALSE, colour = \"black\") +",
        "        labs(x = \"Mean age of the records\", y = \"ALR (age at last record)\", subtitle = \"Dotted: 1:1; solid black: linear regression\") + theme_minimal())",
        "if (life_known) {",
        "  cat(\"r(ALR, LS) =\", round(stats::cor(im$alr, im$lifespan, use = \"complete.obs\"), 3), \"\\n\")",
        "  print(ggplot(im, aes(lifespan, alr)) + geom_point(alpha = 0.5) + geom_abline(slope = 1, intercept = 0, linetype = 3) +",
        "          geom_smooth(method = \"lm\", formula = y ~ x, se = FALSE, colour = \"black\") + labs(x = \"Lifespan (LS)\", y = \"ALR\") + theme_minimal())",
        "  print(ggplot(im, aes(lifespan, mean_age)) + geom_point(alpha = 0.5) + geom_abline(slope = 1, intercept = 0, linetype = 3) +",
        "          geom_smooth(method = \"lm\", formula = y ~ x, se = FALSE, colour = \"black\") + labs(x = \"Lifespan (LS)\", y = \"Mean age\") + theme_minimal())",
        "}"),
      individual = {
        s <- model_settings()
        fns <- intersect(input$b2_functions %||% AGE_FUNCTIONS, MODEL_FUNCTIONS)
        c(
          script_header(data_entries("fit_individual_function", "compare_individual_functions", "compare_population_functions", "observed_trajectory"),
                        packages = c("ggplot2", "lme4")),
          script_data_lines(),
          "# ---- Settings (copied from the app) ----",
          paste0("fun <- ", dput_text(input$a3_function %||% "Quadratic"), "   # ageing function fitted to each individual"),
          paste0("functions <- ", dput_text(fns), "   # functions compared at the population level"),
          paste0("family <- ", dput_text(s$family)),
          paste0("random_slope <- ", dput_text(s$random_slope)),
          paste0("zero_inflation <- ", dput_text(s$zi)),
          "",
          "# ---- Individual fits ----",
          paste0("link <- ", dput_text(safe_get(a3_link()) %||% "identity"), "   # scale of the individual fits: \"log\" for counts"),
          paste0("min_records <- ", if (is.null(safe_get(a3_min_records()))) "NULL" else safe_get(a3_min_records())),
          "fits <- fit_individual_function(dat, fun, 1, draw_ids = unique(dat$id), link = link, min_records = min_records)",
          "cat(\"Individuals fitted:\", nrow(fits$coefs), \"of\", fits$n_total, \"\\n\")",
          "print(fits$mean_curve)",
          "obs <- observed_trajectory(dat)",
          "print(ggplot() + geom_point(data = obs, aes(age, fitted, size = n), shape = 21, fill = \"grey55\") +",
          "        geom_ribbon(data = fits$mean_curve, aes(age, ymin = lo, ymax = hi), alpha = 0.2) +",
          "        geom_line(data = fits$mean_curve, aes(age, fitted), colour = \"#D55E00\", linewidth = 1.2) +",
          "        labs(x = \"Age\", y = \"Trait\", subtitle = \"Line: mean of the individual coefficients (95% band); points: observed means\") + theme_minimal())",
          "show <- utils::head(unique(fits$curves$id), 20)",
          "print(ggplot(fits$curves[fits$curves$id %in% show, ], aes(age, fitted)) + geom_line(colour = \"#0072B2\") +",
          "        geom_point(data = dat[dat$id %in% show, ], aes(age, trait), size = 1) + facet_wrap(~ id, scales = \"free\") + theme_minimal())",
          "",
          "# ---- Ageing functions compared across individuals (mean delta AICc) ----",
          "print(compare_individual_functions(dat, 1, link = link, min_records = min_records))",
          "",
          "# ---- Ageing functions compared at the population level (Model 1) ----",
          paste0("population <- compare_population_functions(dat, meta, family = family, random_slope = random_slope, standardise = ",
                 dput_text(isTRUE(s$standardise)), ", zi_str = zero_inflation, functions = functions, model = \"M1\", among = ", dput_text(s$among),
                 if (!identical(normalise_disp(s$disp), "constant")) paste0(", disp = ", dput_text(normalise_disp(s$disp))) else "", ")"),
          "print(population$table)",
          "if (nrow(population$curves)) print(ggplot() + geom_point(data = obs, aes(age, fitted, size = n), shape = 21, fill = \"grey55\") +",
          "        geom_line(data = population$curves, aes(age, fitted, colour = Function), linewidth = 1.1) + labs(x = \"Age\", y = \"Trait\") + theme_minimal())")
      },
      character(0))
    paste(lines, collapse = "\n")
  }
  for (sec in c("data", "visual", "sampling", "individual")) {
    local({
      section <- sec
      output[[paste0("code_", section, "_text")]] <- renderText({
        tryCatch(section_code(section), shiny.silent.error = function(e) "",
                 error = function(e) paste("# The R code could not be generated:", conditionMessage(e)))
      })
      output[[paste0("download_code_", section)]] <- downloadHandler(
        filename = function() paste0("disappR_", section, "_section.R"),
        content = function(file) {
          txt <- tryCatch(section_code(section), error = function(e) paste("# The R code could not be generated:", conditionMessage(e)))
          con <- file(file, open = "w", encoding = "UTF-8")
          on.exit(close(con))
          writeLines(txt, con)
        })
    })
  }

  # 0.24.7: the suggested starting analysis and its "Apply these settings" button were removed

  # ---- guidance layer: progress strip and step navigation (display only) ----
  lapply(WORKFLOW_STEPS$tab, function(tb) {
    i <- match(tb, WORKFLOW_STEPS$tab)
    if (i > 1) observeEvent(input[[paste0("go_prev_", tb)]], {
      updateTabItems(session, "tabs", WORKFLOW_STEPS$tab[[i - 1]])
    }, ignoreInit = TRUE)
    if (i < nrow(WORKFLOW_STEPS)) observeEvent(input[[paste0("go_next_", tb)]], {
      updateTabItems(session, "tabs", WORKFLOW_STEPS$tab[[i + 1]])
    }, ignoreInit = TRUE)
  })
  observeEvent(input$journey_learn, { updateRadioButtons(session, "data_source", selected = "toy"); updateTabItems(session, "tabs", "data") })
  observeEvent(input$journey_explore, { updateRadioButtons(session, "data_source", selected = "example"); updateTabItems(session, "tabs", "data") })
  observeEvent(input$journey_analyse, { updateRadioButtons(session, "data_source", selected = "upload"); updateTabItems(session, "tabs", "data") })

  # Flag when the AICc comparison rests on far fewer individuals than the data contain: functions that need
  # more records are fitted to fewer individuals, and dAICc uses only the individuals common to every function.
  output$a3_common_warning <- renderUI({
    tab <- a3_compare_current()
    if (is.null(tab) || !nrow(tab) || !all(c("N_common", "N_considered") %in% names(tab))) return(NULL)
    # a function whose AICc could not be computed for most individuals is left out of the ranking
    ex <- attr(tab, "excluded")
    ex_card <- if (length(ex)) div(class = "diagnosis-card", style = "border-left: 5px solid #E0A526;",
      div(class = "diagnosis-title", badge("caution"), " Left out of the AICc ranking"),
      div(class = "diagnosis-detail", sprintf(
        "%s: too few records per individual to compute its AICc (%s). It is listed last, by adjusted R\u00b2 only.",
        paste(names(ex), collapse = ", "),
        paste(sprintf("%s%% of individuals", ex), collapse = "; "))))
    nc <- suppressWarnings(as.numeric(tab$N_common[[1]]))
    nt <- suppressWarnings(as.numeric(tab$N_considered[[1]]))
    if (!is.finite(nc) || !is.finite(nt) || nt <= 0 || nc >= nt) return(ex_card)
    drop <- 100 * (nt - nc) / nt
    if (drop < 20) return(ex_card)
    tagList(ex_card, div(class = "diagnosis-card", style = "border-left: 5px solid #A50026;",
        div(class = "diagnosis-title", badge("caution"), " Unreliable AICc comparison"),
        div(class = "diagnosis-detail",
            sprintf("N_common is %.0f%% lower than the total (%d of %d individuals have every function estimable). The \u0394AICc comparison uses only that common set, so with a drop this large it is unreliable and should not be used to choose an ageing function. Compare the mean adjusted R\u00b2 instead, which uses every individual each function can be fitted to.",
                    drop, as.integer(nc), as.integer(nt)))))
  })

  # The answer to the page's question, stated once at the top: which ageing shapes the individual fits support.
  output$shape_headline <- renderUI({
    tab <- a3_compare_current()
    if (is.null(tab) || !nrow(tab) || !"Mean_dAICc" %in% names(tab)) return(NULL)
    t <- tab[is.finite(tab$Mean_dAICc), , drop = FALSE]
    if (!nrow(t)) return(NULL)
    t <- t[order(t$Mean_dAICc), , drop = FALSE]
    sup <- t$Function[t$Mean_dAICc < 2]
    nc <- suppressWarnings(as.numeric(t$N_common[[1]])); nt <- suppressWarnings(as.numeric(t$N_considered[[1]]))
    thin <- is.finite(nc) && is.finite(nt) && nt > 0 && (nt - nc) / nt >= 0.2
    div(class = "shape-lead",
        div(
            "Suggested function: ", strong(t$Function[[1]]),
            if (length(sup) > 1) sprintf(" \u2014 within 2 AICc: %s.", paste(sup, collapse = ", "))
            else " \u2014 no other function is within 2 AICc.",
            # the caution box below gives the numbers when many individuals could not be fitted (0.24.7)
            if (!thin && is.finite(nc) && is.finite(nt)) sprintf(" Based on %d of %d fitted individuals.", as.integer(nc), as.integer(nt)) else ""),
        div(class = "small-note", style = "margin-top:4px;",
            "Also confirm it with the Ageing-function check in step 5, which compares different functions within the same model."))
  })

  # ---- summary of the workflow ------------------------------------------------------
  # Composed only from what the user saved, plus the fitted models. Phrased as what the analysis
  # suggests, never as a verdict: every line names the evidence it rests on.
  evidence_grade <- reactive({
    lst <- saved_results()
    tryCatch(grade_evidence(lst, trait = safe_get(trait_label()) %||% "the trait"),
             error = function(e) {
               message("disappR: evidence summary failed: ", conditionMessage(e))
               list(error = conditionMessage(e))
             })
  })
  evidence_grade_app <- reactive({
    tryCatch(grade_appearance(saved_results(), trait = safe_get(trait_label()) %||% "the trait"),
             error = function(e) {
               message("disappR: selective-appearance summary failed: ", conditionMessage(e))
               NULL
             })
  })
  output$evidence_summary <- renderUI({
    g <- evidence_grade()
    if (is.null(g)) return(NULL)
    # a failure is shown, never hidden: an empty panel here once concealed a broken feature
    if (!is.null(g$error)) return(div(class = "diagnosis-card", style = "border-left: 5px solid #A50026;",
      div(class = "diagnosis-title", "The evidence summary could not be built"),
      div(class = "diagnosis-detail", g$error),
      div(class = "diagnosis-detail small-note", "Please report this message. The saved results below are unaffected.")))
    if (!length(saved_results())) return(div(class = "evidence-card", style = "border-left: 6px solid #7d6d5e;",
      div(class = "evidence-headline", "No results saved yet."),
      p("The evidence summary grades the results you save. Use the 'Save to summary' buttons in steps 2 to 5; the summary updates as you save or remove results."),
      div(class = "evidence-pending", strong("Start with these:"),
          tags$ol(lapply(unname(PENDING_TEXT[c("visual", "models", "missingness")]), tags$li)))))
    icon_of <- c(supports = "\u2713", caveat = "!", contradicts = "\u2717", `not saved` = "\u2013")
    cls_of <- c(supports = "mark-yes", caveat = "mark-warn", contradicts = "mark-no", `not saved` = "mark-na")
    card <- function(g, title) {
      colour <- switch(g$level, strong = "#2f6b34", moderate = "#9a6b1e", weak = "#A50026", mixed = "#9a6b1e", "#7d6d5e")
      rows <- if (is.data.frame(g$lines) && nrow(g$lines)) lapply(seq_len(nrow(g$lines)), function(i) {
        st <- g$lines$Status[[i]]
        fn <- if ("Footnote" %in% names(g$lines)) g$lines$Footnote[[i]] else ""
        tags$tr(tags$td(class = paste("result-mark", cls_of[[st]]), icon_of[[st]]),
                tags$td(class = "ev-name", g$lines$Evidence[[i]]),
                tags$td(g$lines$Note[[i]],
                        if (length(fn) && !is.na(fn) && nzchar(fn)) tags$div(class = "small-note", style = "font-size: 85%; margin-top: 3px;", fn) else NULL))
      }) else NULL
      div(class = "evidence-card", style = paste0("border-left: 6px solid ", colour, ";"),
          div(class = "result-head", title),
          div(class = "evidence-headline", g$headline),
          if (!is.null(g$finding)) div(class = "evidence-finding", strong("Finding: "),
                                       paste0("the saved evidence suggests that ", g$finding, ".")) else NULL,
          if (!is.null(g$explanation)) div(class = "evidence-explain", g$explanation) else NULL,
          if (length(rows)) tags$table(class = "evidence-table", rows) else NULL,
          if (length(g$cautions)) tags$ul(class = "small-note", lapply(g$cautions, tags$li)) else NULL,
          if (length(g$pending)) div(class = "evidence-pending",
            strong("Complete the following checks for more robust evidence:"),
            tags$ol(lapply(unname(PENDING_TEXT[g$pending]), tags$li))) else NULL)
    }
    ga <- evidence_grade_app()
    tagList(
      card(g, "Evidence summary: selective disappearance"),
      if (!is.null(ga)) card(ga, "Evidence summary: selective appearance") else NULL,
      tags$details(class = "info-more", tags$summary("How this was established"),
        div(class = "info-more-body",
          p(strong("Data analysed: "), safe_get(data_description()) %||% ""),
          p("Read only from the results you saved. Figures saved with AFR as the grouping variable, and Models 7\u201310, are ",
            "evidence for selective appearance; figures grouped by lifespan (LS, ALR or mean age) and Models 1\u20136 for ",
            "selective disappearance. A figure's reading rests on the coefficient of the trait on the grouping variable ",
            "among the individuals present at each age: its average across ages (a difference from zero) and its change ",
            "with age, with bootstrap p-values over individuals. The level is 'strong' only when every check relevant to the ",
            "finding has been saved and passed; a check not yet saved caps it at 'moderate'; figures that contradict the models, ",
            "or a failed random-slope check or bootstrap test, cap it at 'weak' and change the explanation offered. Missing ",
            "records that depend on the trait lower it further. These are suggestions to weigh, not conclusions."))))
  })

  # ---- advanced: effect sizes and the null-model bootstrap ----
  effects_store <- reactiveVal(NULL)
  observeEvent(input$run_effects, disappr_guard("input$run_effects", {
    r <- safe_get(fitted_models())
    if (is.null(r) || !isTRUE(r$ok)) { showNotification("Fit the models first.", type = "warning"); return(invisible(NULL)) }
    effects_store(tryCatch(selection_effect_sizes(r, safe_get(dat())), error = function(e) NULL))
  }))
  output$effect_table <- renderTable({
    e <- effects_store()
    if (is.null(e) || !nrow(e)) return(data.frame(Note = "Press 'Compute effect sizes' after fitting models."))
    data.frame(Effect = e$Effect,
               Estimate = signif(e$Estimate, 4),
               `95% interval` = sprintf("%.4g to %.4g", e$Lower, e$Upper),
               Unit = e$Unit, Note = e$Note, check.names = FALSE, stringsAsFactors = FALSE)
  }, striped = TRUE, spacing = "xs")
  observeEvent(input$save_effects, disappr_guard("input$save_effects", {
    e <- effects_store()
    if (is.null(e) || !nrow(e)) return(nothing_to_save())
    add_saved("5 Modelling", "Effect sizes", verdict = "context",
              text = "How far the correction moved the ageing slope, and the size of the lifespan association, on the trait's own scale.",
              tables = list(`Effect sizes` = e))
  }))
  output$perm_model_ui <- renderUI({
    r <- safe_get(fitted_models())
    ch <- if (is.null(r) || !isTRUE(r$ok)) MODEL_IDS else MODEL_IDS[model_label(MODEL_IDS) %in% r$aic$Model]
    ch <- intersect(ch, BOOTSTRAP_MODELS)
    if (!length(ch)) ch <- "M4"
    # default to the model the finding rests on: the best-supported lifespan model
    best <- if (!is.null(r) && isTRUE(r$ok)) sub("^Model ", "M", r$aic$Model[is.finite(r$aic$Delta_AIC)]) else character(0)
    best <- intersect(best, ch)
    selectInput("perm_model", "Model to test", choices = stats::setNames(ch, model_label(ch)),
                selected = if (length(best)) best[[1]] else if ("M4" %in% ch) "M4" else ch[[1]])
  })
  perm_store <- reactiveVal(NULL)
  observeEvent(input$run_perm, disappr_guard("input$run_perm", {
    d <- safe_get(dat()); m <- safe_get(meta()); r <- safe_get(fitted_models())
    if (is.null(d) || is.null(r) || !isTRUE(r$ok)) { showNotification("Fit the models first.", type = "warning"); return(invisible(NULL)) }
    ms <- model_settings()
    n <- suppressWarnings(as.integer(input$perm_n))
    if (length(n) != 1L || is.na(n)) n <- 39L
    n <- max(1L, min(999L, n))
    withProgress(message = "Simulating data without a lifespan effect", value = 0, {
      out <- tryCatch(bootstrap_test(d, m, model = input$perm_model %||% "M4", against = "M1", n_boot = n,
                                       settings = list(family = ms$family, age_function = ms$age_function,
                                                       random_slope = ms$random_slope, standardise = ms$standardise,
                                                       zi = ms$zi, among = ms$among, extra = ms$extra, disp = ms$disp),
                                       progress = function(f, msg) setProgress(value = f, detail = msg)),
                      error = function(e) list(error = conditionMessage(e)))
      if (is.null(out$error)) {
        out$data_sig <- safe_get(data_sig())
        # the key comes from the test's own observed fit, so it matches the model comparison it tests (the requested
        # among option is recorded as "linear" by the fit whenever the ageing function is linear)
        if (is.null(out$settings_key)) out$settings_key <- paste(ms$age_function, ms$family, ms$among, isTRUE(ms$standardise), sep = "|")
      }
      perm_store(out)
    })
  }))
  output$perm_result <- renderUI({
    z <- perm_store()
    if (is.null(z)) return(p(class = "small-note", "Press 'Run bootstrap test' after fitting models."))
    if (!is.null(z$error)) return(div(class = "diagnosis-card", strong("Could not run: "), z$error))
    rd <- bootstrap_reading(z)
    if (is.null(rd)) return(div(class = "diagnosis-card", "No simulated dataset could be fitted, so there is nothing to compare against."))
    colour <- switch(rd$pattern, drops = "#2f6b34", partial = "#9a6b1e", no_drop = "#A50026", "#7d6d5e")
    div(class = "perm-card", style = paste0("border-left: 6px solid ", colour, ";"),
        div(class = "perm-headline", rd$headline),
        div(class = "perm-row", span(class = "perm-label", "What this suggests"), rd$meaning),
        div(class = "perm-row", span(class = "perm-label", "Should you trust the model?"), rd$trust),
        if (length(rd$alternatives)) div(class = "perm-row", span(class = "perm-label", "Other explanations to rule out"),
                                         tags$ul(lapply(rd$alternatives, tags$li))) else NULL,
        div(class = "small-note", style = "margin-top:8px;",
            sprintf("%s versus %s; %d of %d simulated datasets fitted; p = %s. Null model: %s ageing, %s random effects, %s, no lifespan term.",
                    model_label(z$model), model_label(z$against), z$n_ok, z$n_perm,
                    if (is.finite(z$p_gain)) format_p(z$p_gain) else "not available",
                    z$null_model$age_function %||% "", switch(z$null_model$random_slope %||% "none",
                      correlated = "correlated intercept and slope", uncorrelated = "intercept and slope", "intercept-only"),
                    disp_text(z$null_model$disp %||% "constant")),
            if (isTRUE((z$n_ok %||% 0L) < BOOTSTRAP_MIN_OK)) sprintf(" With %d refitted datasets the smallest possible p-value is %.3f, so this run cannot reach p < 0.05; run more draws so that at least 39 refit.",
                                       as.integer(z$n_ok %||% 0L), 1 / ((z$n_ok %||% 0L) + 1)) else ""))
  })
  perm_plot_obj <- reactive({
    z <- perm_store()
    shiny::validate(shiny::need(!is.null(z) && is.null(z$error) && length(z$null_gain) > 2,
                                "Run the bootstrap test to see the null distribution."))
    df <- data.frame(gain = z$null_gain)
    ggplot(df, aes(gain)) +
      geom_histogram(bins = 30, fill = "#c9b7a3", colour = "white") +
      geom_vline(xintercept = z$observed_gain, colour = "#A50026", linewidth = 1.2) +
      labs(x = sprintf("AIC advantage of %s over %s", model_label(z$model), model_label(z$against)),
           y = "Simulated datasets",
           title = "Null distribution from data simulated without a lifespan effect",
           subtitle = "Red line: the advantage observed in the real data") +
      theme_disappR(13)
  })
  output$perm_plot <- renderPlot(perm_plot_obj())
  observeEvent(input$save_perm, disappr_guard("input$save_perm", {
    z <- perm_store()
    if (is.null(z) || !is.null(z$error)) return(nothing_to_save())
    rd <- bootstrap_reading(z)
    if (is.null(rd)) return(nothing_to_save())
    add_saved(evidence = ev_perm(), "5 Modelling", "Null-model bootstrap", verdict = if (identical(rd$pattern, "drops")) "context" else "caution",
              text = c(rd$headline, paste("What this suggests:", rd$meaning), paste("Should you trust the model?", rd$trust),
                       if (length(rd$alternatives)) paste("Other explanations to rule out:", paste(rd$alternatives, collapse = " ")) else NULL,
                       sprintf("%s versus %s; %d of %d simulated datasets fitted; p = %s.", model_label(z$model), model_label(z$against),
                               z$n_ok, z$n_perm, if (is.finite(z$p_gain)) format_p(z$p_gain) else "NA")),
              plots = list(safe_get(perm_plot_obj())))
  }))

  # ---- automatic sensitivity flags -------------------------------------------------
  # Two mechanisms produce selective disappearance where none exists, at rates measured in the
  # package's own validation: a misspecified ageing function (an interaction model won 100% of
  # simulated replicates with no selection, median gap 1167 AIC) and omitted random slopes (60%).
  # Both are therefore checked automatically once models are fitted, and shown with the results
  # rather than behind a button.
  auto_check_cache <- new.env(parent = emptyenv())   # automatic checks already computed, by signature (0.23.1)
  auto_function_check <- reactive({
    r <- safe_get(fitted_models())
    d <- safe_get(dat())
    if (is.null(r) || !isTRUE(r$ok) || is.null(d)) return(NULL)
    if (nrow(d) > 40000) return(list(skipped = TRUE))
    ms <- model_settings()
    # Refit the best-supported model with each ageing function. Model 1 is the wrong reference: it omits lifespan,
    # and under age-dependent selective disappearance the population curve bends even when every individual ages
    # linearly, so a correct linear function would be flagged as misspecified.
    a <- r$aic[is.finite(r$aic$Delta_AIC), , drop = FALSE]
    mid <- if (nrow(a)) sub("^Model ", "M", a$Model[[1]]) else "M1"
    if (!mid %in% MODEL_IDS) mid <- "M1"
    # cached by data, settings and model (0.23.1): refitting the same models again returns the stored comparison
    key <- paste("fun", safe_get(data_sig()), settings_sig(ms), mid)
    res <- auto_check_cache[[key]]
    if (is.null(res)) {
      res <- tryCatch(compare_population_functions(d, meta(), family = ms$family, random_slope = ms$random_slope,
                                                   standardise = ms$standardise, zi_str = ms$zi, model = mid,
                                                   among = ms$among, include_invalid = FALSE, disp = ms$disp),
                      error = function(e) NULL)
      if (!is.null(res)) assign(key, res, envir = auto_check_cache)
    }
    if (is.null(res) || !nrow(res$table)) return(NULL)
    t <- res$table[is.finite(res$table$AIC), , drop = FALSE]
    if (!nrow(t)) return(NULL)
    list(skipped = FALSE, supported = t$Function[t$Delta_AIC < 2], table = t, model = mid)
  })
  # Automatic error-family check (0.22.6): the best-supported model refitted with each family of the trait's kind, as
  # the ageing-function check does for functions. Zero-inflated families only when zeros are common; skipped for very
  # large data and for the non-linear exponential.
  auto_family_check <- reactive({
    r <- safe_get(fitted_models())
    d <- safe_get(dat())
    if (is.null(r) || !isTRUE(r$ok) || is.null(d) || !HAS_GLMMTMB) return(NULL)
    if (nrow(d) > 40000) return(list(skipped = TRUE))
    ms <- model_settings()
    if (identical(ms$age_function, A3_NONLINEAR)) return(NULL)
    a <- r$aic[is.finite(r$aic$Delta_AIC), , drop = FALSE]
    mid <- if (nrow(a)) sub("^Model ", "M", a$Model[[1]]) else "M1"
    if (!mid %in% MODEL_IDS) mid <- "M1"
    fam <- ms$family %||% "gaussian"
    y <- d$trait[is.finite(d$trait)]
    fams <- if (fam %in% BINOMIAL_FAMILIES) BINOMIAL_FAMILIES else if (fam %in% DENSITY_FAMILIES) DENSITY_FAMILIES else
      c("poisson", "nbinom1", "nbinom2", if (length(y) && mean(y == 0) >= 0.1) c("zip", "zinb", "zinb1"))
    key <- paste("fam", safe_get(data_sig()), settings_sig(ms), ms$age_function, mid, paste(unique(c(fam, fams)), collapse = ","))
    res <- auto_check_cache[[key]]
    if (is.null(res)) {
      res <- tryCatch(compare_families(d, meta(), model = mid, age_function = ms$age_function, random_slope = ms$random_slope,
                                       standardise = ms$standardise, zi_str = ms$zi, families = unique(c(fam, fams)),
                                       among = ms$among, include_invalid = FALSE, disp = ms$disp),
                      error = function(e) NULL)
      if (!is.null(res)) assign(key, res, envir = auto_check_cache)
    }
    if (is.null(res) || !isTRUE(res$ok)) return(NULL)
    res$model <- mid
    res
  })
  sensitivity_flags <- reactive({
    r <- safe_get(fitted_models())
    if (is.null(r) || !isTRUE(r$ok)) return(list())
    out <- list()
    best_id <- {
      a <- r$aic[is.finite(r$aic$Delta_AIC), , drop = FALSE]
      m0 <- if (nrow(a)) sub("^Model ", "M", a$Model[[1]]) else "M1"
      if (m0 %in% names(r$fits)) m0 else names(r$fits)[[1]]
    }
    # error family: the AIC comparison from the manual check when it matches the current fit, otherwise from the
    # automatic check; if neither finds a better family, the instant screen of the fitted model
    fr <- safe_get(family_res())
    s_fam <- safe_get(model_settings())
    fresh <- !is.null(fr) && !is.null(s_fam) && isTRUE(fr$ok) &&
      identical(fr$signature %||% "", paste(safe_get(data_sig()), input$family_check_model, s_fam$age_function, settings_sig(s_fam)))
    fc_fam <- if (fresh) fr else safe_get(auto_family_check())
    compared <- is.list(fc_fam) && isTRUE(fc_fam$ok) && is.data.frame(fc_fam$table) && nrow(fc_fam$table) > 1
    if (compared) {
      ft <- fc_fam$table[is.finite(fc_fam$table$AIC), , drop = FALSE]
      ft <- ft[order(ft$AIC), , drop = FALSE]
      here <- family_label(r$family)
      mlab <- if (length(fc_fam$model) && nzchar(fc_fam$model[[1]])) model_label(fc_fam$model[[1]]) else "the checked model"
      if (nrow(ft) > 1 && here %in% ft$Family && !identical(ft$Family[[1]], here)) {
        gap <- ft$AIC[ft$Family == here][[1]] - ft$AIC[[1]]
        if (is.finite(gap) && gap > 2)
          out$error_family <- sprintf("Error family (%s): %s fits %s better than %s (\u0394AIC %s). Refit with it: a wrong family can inflate interaction terms.",
                                      if (fresh) "error-family check" else "automatic check", ft$Family[[1]], mlab, here, format_num(gap))
      }
    }
    if (is.null(out$error_family)) {
      scr <- tryCatch(family_screen(r, best_id), error = function(e) character(0))
      if (length(scr))
        out$error_family <- paste0("Error family: ", paste(scr, collapse = "; "), ". ",
                                   if (compared) "The AIC comparison found no better family: check the residuals." else "Run the error-family check (Checks tab).")
    }
    cur <- input$model_age_function %||% "Quadratic"
    fc <- safe_get(auto_function_check())
    if (!is.null(fc) && !isTRUE(fc$skipped) && length(fc$supported) && !cur %in% fc$supported) {
      gap <- fc$table$Delta_AIC[fc$table$Function == cur]
      out$function_form <- sprintf(
        "Ageing function (automatic check): refitting the best-supported model with each function, %s is %sbehind %s. Refit with a supported function: a misspecified shape can make interaction terms look supported without selective (dis)appearance.",
        cur, if (length(gap) && is.finite(gap[[1]])) paste0(format_num(gap[[1]]), " AIC ") else "",
        paste(fc$supported, collapse = ", "))
    }
    if (!is.null(fc) && isTRUE(fc$skipped)) {
      out$function_form_skipped <- "Ageing function: not checked automatically (large dataset). Run the ageing-function check (Checks tab): a misspecified shape can make the interaction models look supported."
    }
    adv <- tryCatch(random_slope_advice(individual_data_support(r$data)), error = function(e) NULL)
    # the structure actually fitted: 'Automatic' can resolve to an intercept only (0.23.1; review finding 1)
    slope_fitted <- !identical(normalise_slope(r$random_structure %||% "none"), "none")
    int_supported <- {
      a <- r$aic
      elig <- a[is.finite(a$Delta_AIC) & a$Delta_AIC < 2, , drop = FALSE]
      any(elig$Model %in% model_label(c("M4", "M5", "M6", "M8", "M9", "M10")))
    }
    if (!slope_fitted && int_supported) {
      lvl <- adv$level %||% "limited"
      strength <- switch(lvl, good = "the data support them", limited = "the data give limited support", "the data give weak support")
      out$random_slopes <- paste0("Random slopes (", strength, "): without them, an interaction model can win by absorbing ",
                                  "individual differences in ageing. Refit with random slopes, one per age term if the data allow.")
    }
    first <- c("random_slopes", "error_family", "function_form", "function_form_skipped")
    out[c(intersect(first, names(out)), setdiff(names(out), first))]
  })
  output$sensitivity_banner <- renderUI({
    fl <- sensitivity_flags()
    if (!length(fl)) return(NULL)
    tagList(lapply(names(fl), function(k) {
      div(class = "diagnosis-card", style = "border-left: 5px solid #A50026;",
          div(class = "diagnosis-title", badge("caution"), " Sensitivity check"),
          div(class = "diagnosis-detail", fl[[k]]))
    }))
  })

  # ---- ageing-function check: one model across every ageing function ----
  function_check <- reactiveVal(NULL)
  # Model 1 omits lifespan and bends under age-dependent selection, so it can flag a correct function; the manual
  # check therefore starts on the best-supported model once models are fitted.
  observeEvent(fitted_models(), disappr_guard("fitted_models()", {
    r <- safe_get(fitted_models())
    if (is.null(r) || !isTRUE(r$ok) || !nrow(r$aic)) return(invisible(NULL))
    a <- r$aic[is.finite(r$aic$Delta_AIC), , drop = FALSE]
    mid <- if (nrow(a)) sub("^Model ", "M", a$Model[[1]]) else "M1"
    if (mid %in% MODEL_IDS) updateSelectInput(session, "function_check_model", selected = mid)
  }))
  observeEvent(input$run_function_check, disappr_guard("input$run_function_check", {
    d <- safe_get(dat())
    if (is.null(d)) { showNotification("Load or simulate data first.", type = "warning"); return(invisible(NULL)) }
    m <- input$function_check_model %||% "M1"
    ms <- model_settings()
    res <- run_with_progress("Fitting this model with each ageing function", function(pr) {
      compare_population_functions(d, meta(), family = ms$family, random_slope = ms$random_slope,
                                   standardise = ms$standardise, zi_str = ms$zi, progress = pr, model = m,
                                   among = ms$among, include_invalid = isTRUE(ms$include_invalid), disp = ms$disp)
    })
    res$signature <- paste(data_sig(), m, ms$family, ms$random_slope, ms$standardise, ms$among, ms$disp)
    res$model_checked <- m
    res$data_sig <- safe_get(data_sig())
    function_check(res)
  }))
  output$function_check_table <- renderTable({
    r <- function_check()
    if (is.null(r) || !nrow(r$table)) return(NULL)
    t <- r$table
    out <- data.frame(Function = t$Function, AIC = format_num(t$AIC), dAIC = format_num(t$Delta_AIC),
                      df = t$df, N = t$N, Fit = t$Fit, check.names = FALSE, stringsAsFactors = FALSE)
    names(out)[names(out) == "dAIC"] <- "\u0394AIC"
    out
  }, striped = TRUE, spacing = "xs", width = "100%")
  output$function_check_note <- renderUI({
    r <- function_check()
    if (is.null(r) || !nrow(r$table)) return(p(class = "small-note", "Choose a model and press the button: it refits that model with each ageing function on the same rows."))
    t <- r$table[is.finite(r$table$AIC), , drop = FALSE]
    if (!nrow(t)) return(p(class = "small-note", "No ageing function could be fitted for this model."))
    supported <- t$Function[t$Delta_AIC < 2]
    cur <- input$model_age_function %||% "Quadratic"
    tagList(
      p(class = "small-note", if (length(supported) > 1)
        paste0("Supported set (\u0394AIC < 2): ", paste(supported, collapse = ", "), ". These shapes are not distinguished by these data.")
        else paste0("Only ", supported[[1]], " is within 2 AIC.")),
      if (!cur %in% supported) div(class = "small-note", style = "border-left: 3px solid #A50026; padding-left: 7px;",
        strong(sprintf("The ageing function currently used on this tab (%s) is not among the best-supported shapes (\u0394AIC = %s). ", cur, format_num(t$Delta_AIC[t$Function == cur][1]))),
        "A misspecified ageing function can make the interaction models win even when there is no selective disappearance, so check whether the disappearance result survives one of the supported shapes.") else NULL
    )
  })
  observeEvent(input$save_function_check, disappr_guard("input$save_function_check", {
    r <- safe_get(function_check())
    if (is.null(r) || !nrow(r$table)) return(nothing_to_save("Run the ageing-function check first."))
    t <- r$table[is.finite(r$table$AIC), , drop = FALSE]
    supported <- if (nrow(t)) t$Function[t$Delta_AIC < 2] else character(0)
    cur <- input$model_age_function %||% "Quadratic"
    txt <- c(sprintf("%s fitted with each ageing function on the same rows.", model_label(r$model)),
             if (length(supported)) sprintf("Supported set (\u0394AIC < 2): %s.", paste(supported, collapse = ", ")),
             if (length(supported) && !cur %in% supported) sprintf("The ageing function used for the reported models (%s) is NOT in the supported set: the selective-disappearance result may depend on the shape rather than on selection.", cur))
    add_saved(evidence = ev_shape(), "5 Modelling", paste("Ageing-function check with", model_label(r$model)), text = txt,
              tables = list(`Ageing functions` = r$table),
              verdict = if (length(supported) && !cur %in% supported) "caution" else "context")
  }))

  # ---- DHARMa ----
  dharma_res <- reactiveVal(NULL)
  output$dharma_model_ui <- renderUI({
    r <- safe_get(fitted_models())
    if (is.null(r)) return(p(class = "small-note", "Fit models first."))
    ch <- names(r$fits)
    best <- sub("Model ", "M", r$aic$Model[[1]])
    selectInput("dharma_model", "Model", choices = stats::setNames(ch, model_label(ch)), selected = best)
  })
  observeEvent(input$run_dharma, disappr_guard("input$run_dharma", {
    r <- safe_get(fitted_models())
    if (is.null(r) || is.null(input$dharma_model) || is.null(r$fits[[input$dharma_model]])) {
      notify("Fit models first.", type = "warning")
      return(invisible(NULL))
    }
    res <- run_with_progress("Simulating residuals", function(pr) run_dharma(r$fits[[input$dharma_model]], r$family))
    res$model <- input$dharma_model
    res$signature <- model_sig()
    dharma_res(res)
  }))
  output$dharma_table <- renderTable({
    z <- dharma_res()
    shiny::validate(shiny::need(!is.null(z), "Press 'Simulate residuals'."))
    shiny::validate(shiny::need(isTRUE(z$ok), z$message %||% "DHARMa failed."))
    shiny::validate(shiny::need(identical(z$signature, model_sig()), "Models were refitted: simulate residuals again."))
    data.frame(Model = model_label(z$model), Test = z$table$Test, P = vapply(z$table$P_value, format_p, character(1)))
  }, striped = TRUE, spacing = "xs")
  output$dharma_plot <- renderPlot({
    z <- dharma_res()
    shiny::validate(shiny::need(!is.null(z) && isTRUE(z$ok), ""))
    shiny::validate(shiny::need(identical(z$signature, model_sig()), ""))
    graphics::plot(z$sim)
  })

  # ---- R code ----
  code_text <- reactive({
    r <- fitted_models()
    src <- switch(input$data_source %||% "toy", upload = input$data_file$name %||% "your_data.csv",
                  example = current_example()$file, "disappR_simulated.csv")
    model_r_code(r, meta(), src, source_type = input$data_source %||% "toy")
  })
  output$code_ui <- renderUI(tags$pre(class = "code-out", code_text()))
  make_code_download <- function() {
    downloadHandler(
      filename = function() paste0("disappR_models_", Sys.Date(), ".R"),
      content = function(file) {
        con <- file(file, open = "w", encoding = "UTF-8")
        on.exit(close(con), add = TRUE)
        writeLines(code_text(), con)
      }
    )
  }
  output$download_code <- make_code_download()
  output$download_code2 <- make_code_download()
  make_data_download <- function() {
    downloadHandler(
      filename = function() "disappR_analysis_data.csv",
      content = function(file) {
        r <- safe_get(current_models())
        d <- if (is.null(r) || !isTRUE(r$ok) || isTRUE(r$nonlinear)) data.frame(Note = "Fit the models first (or refit after changing data or settings).") else analysis_data_export(r)
        utils::write.csv(d, file, row.names = FALSE)
      }
    )
  }
  output$download_analysis_data <- make_data_download()
  output$download_analysis_data2 <- make_data_download()

  # ======================================================================
  # 7. Summary and report
  # ======================================================================

  # ======================================================================
  # Saved results: every "Save to summary" button adds a snapshot here
  # ======================================================================
  saved_results <- reactiveVal(list())
  save_counter <- reactiveVal(0L)
  nothing_to_save <- function(msg = "Nothing to save yet: this result is not available for the current data and settings.") {
    notify(msg, type = "warning")
    invisible(NULL)
  }
  data_description <- function() {
    src <- input$data_source %||% "toy"
    d <- safe_get(dat())
    n_txt <- if (is.null(d)) "" else sprintf(" (%s rows, %s individuals)", format(nrow(d), big.mark = ","), format(length(unique(d$id)), big.mark = ","))
    base <- switch(src, example = current_example()$label, upload = paste("Uploaded file", input$data_file$name %||% ""),
                   paste("Simulated:", toy_truth_text(toy_cfg())$sim))
    paste0(base, n_txt)
  }
  # Verdict from the fitted models: which family of models is in the supported set (within 2 AIC).
  models_verdict <- function() {
    r <- safe_get(fitted_models())
    if (is.null(r) || !isTRUE(r$ok) || is.null(r$aic) || !nrow(r$aic)) return(NA_character_)
    a <- r$aic[is.finite(r$aic$Delta_AIC), , drop = FALSE]
    if (!nrow(a)) return(NA_character_)
    sup <- a$Model[a$Delta_AIC < 2]
    int <- model_label(c("M4", "M5", "M6", "M8", "M9", "M10"))
    add <- model_label(c("M2", "M3", "M7"))
    if (any(sup %in% int) && !model_label("M1") %in% sup) return("age_dependent")
    if (any(sup %in% add) && !model_label("M1") %in% sup) return("age_independent")
    if (model_label("M1") %in% sup && !any(sup %in% c(int, add))) return("none")
    if (model_label("M1") %in% sup) return("none")
    "context"
  }
  # Verdict from a visual panel: its own diagnosis sentence is built from the fitted bin trends,
  # so the wording is read once here rather than guessed from free text.
  visual_verdict <- function(txt) {
    tt <- tolower(paste(txt, collapse = " "))
    if (!nzchar(tt)) return(NA_character_)
    if (grepl("age-dependent|changes with age|widen|narrow|diverg|converg", tt)) return("age_dependent")
    if (grepl("age-independent|parallel|similar amount at all ages|constant", tt)) return("age_independent")
    if (grepl("no clear|no evidence|no difference|overlap", tt)) return("none")
    NA_character_
  }
  # Saved results are listed and exported in the order they appear in the app: by step, then top to bottom and
  # left to right within a step. This order is derived from the position of each save button in ui.R.
  SAVE_ORDER <- c(
    "Distribution of",
    "Data integrity checks",
    "Trait trajectory by",
    "Difference between",
    "Trait against",
    "Regression coefficient of the trait on",
    "Trait before death",
    "Selection differentials by age",
    "Disappearance hazard",
    "Sampling grid",
    "Sampling summary and proxy guidance",
    "Missingness and sample size by age",
    "Missingness against",
    "Agreement between ALR, mean age and lifespan",
    "Mean-coefficient trajectory",
    "Individual-level function comparison",
    "Population-level ageing functions",
    "Model comparison",
    "Cross-fit model comparison",
    "Model predictions trajectory",
    "Coefficients of",
    "Peak age and onset",
    "Among-individual covariance",
    "Residual checks for",
    "performance checks for",
    "Error-family check with",
    "Ageing-function check with",
    "Effect sizes",
    "Null-model bootstrap",
    "Reproducible R code")
  save_rank <- function(section, title) {
    sec <- suppressWarnings(as.integer(sub("^([0-9]+).*$", "\\1", section)))
    if (length(sec) != 1L || is.na(sec)) sec <- 9L
    hit <- which(vapply(SAVE_ORDER, function(p) startsWith(title, p), logical(1)))
    sec * 1000 + if (length(hit)) hit[[1]] else 999
  }
  # ---- structured evidence written with each saved result, read by grade_evidence() ----
  # the visual evidence uses the grouping variable and trait scale of the saved figure: AFR figures are evidence for
  # selective appearance, lifespan-proxy figures (LS, else ALR) for selective disappearance
  ev_visual <- function() tryCatch({
    px <- safe_get(vis_px()) %||% "ALR"
    vd <- safe_get(visual_dat())
    rec <- classify_visual_coef(vd, proxy = switch(px, LS = "life", AFR = "entry", "alr"),
                                process = if (identical(px, "AFR")) "appearance" else "disappearance")
    # the gap figure's reading, with the figure's own groups, as secondary evidence
    if (is.list(rec)) rec$gap <- tryCatch(
      visual_gap_stats(bin_differences(binned_trajectory(vd, proxy_values(safe_get(imet()), px), input$n_bins %||% 4,
                                                         input$bin_method %||% "quantile", 3), "successive")),
      error = function(e) NULL)
    rec
  }, error = function(e) NULL)
  ev_models <- function() tryCatch({
    r <- safe_get(fitted_models())
    fc <- safe_get(auto_function_check())
    ok <- if (!is.null(fc) && !isTRUE(fc$skipped) && length(fc$supported)) (r$age_function %in% fc$supported) else NA
    e <- models_evidence(r, models_verdict(), shape_ok = ok,
                         shape_supported = if (!is.null(fc) && length(fc$supported)) fc$supported else character(0))
    e$data_sig <- safe_get(data_sig())
    e
  }, error = function(e) NULL)
  ev_perm <- function() tryCatch({
    z <- perm_store(); rd <- bootstrap_reading(z)
    if (is.null(rd)) NULL else list(type = "permutation", method = "bootstrap", pattern = rd$pattern, model = z$model, p = z$p_gain,
                                    data_sig = z$data_sig, settings_key = z$settings_key)
  }, error = function(e) NULL)
  ev_shape <- function() tryCatch({
    r <- safe_get(function_check())
    t <- r$table[is.finite(r$table$AIC), , drop = FALSE]
    sup <- t$Function[t$Delta_AIC < 2]
    # the function actually fitted, not the dropdown, which may have changed since
    fm <- safe_get(fitted_models())
    cur <- if (!is.null(fm) && nzchar(fm$age_function %||% "")) fm$age_function else input$model_age_function %||% "Quadratic"
    list(type = "shape", ok = cur %in% sup, supported = sup, current = cur, model = r$model_checked %||% NA_character_,
         data_sig = r$data_sig)
  }, error = function(e) NULL)
  ev_missing <- function() tryCatch({
    ms <- safe_get(msum())
    dr <- if (is.list(ms)) ms$drivers else NULL
    tp <- if (is.data.frame(dr) && "Prior observed trait" %in% dr$Predictor) dr$P_value[dr$Predictor == "Prior observed trait"][[1]] else NA_real_
    list(type = "missingness", trait_p = tp, percent = if (is.list(ms) && is.numeric(ms$percent)) ms$percent else NA_real_)
  }, error = function(e) NULL)

  # Every analysis setting in force when a result is saved, with button click counts and navigation left out.
  # Two saves with identical settings and identical content are the same result.
  settings_snapshot <- function() {
    v <- tryCatch(shiny::reactiveValuesToList(input), error = function(e) NULL)
    if (is.null(v) || !length(v)) return(NULL)
    drop <- vapply(v, function(x) inherits(x, "shinyActionButtonValue"), logical(1)) |
            grepl("^(save_|info_|go_prev_|go_next_|saved_)", names(v)) |
            grepl("_(hover|click|dblclick|brush|rows_selected|rows_current|rows_all|state|search|cell_clicked)$", names(v)) |
            names(v) %in% c("tabs", "model_tabs", "sidebarCollapsed", "sidebarItemExpanded")
    v <- v[!drop]
    v[order(names(v))]
  }
  add_saved <- function(section, title, text = character(0), tables = list(), plots = list(), code = NULL, verdict = NA_character_,
                        evidence = NULL) {
    # Saving the same analysis twice would count it twice in the summary. The fingerprint covers the result and
    # every setting, but not the plots themselves: when the settings and tables match, the plot came from the
    # same inputs. A setting that changes only the figure (such as showing error bars) is in the snapshot, so it
    # still makes a new result.
    fp <- list(section = section, title = title, text = as.character(text),
               tables = Filter(function(t) is.data.frame(t) && nrow(t) > 0, tables),
               code = code, data = data_description(), settings = settings_snapshot())
    if (!is.null(fp$settings) &&
        any(vapply(saved_results(), function(e) identical(e$fingerprint, fp), logical(1)))) {
      showNotification("This result is already saved with exactly the same settings, so it was not added again.",
                       type = "message", duration = 6)
      return(invisible(NULL))
    }
    k <- save_counter() + 1L
    save_counter(k)
    id <- paste0("saved", k)
    tables <- Filter(function(t) is.data.frame(t) && nrow(t) > 0, tables)
    plots <- Filter(Negate(is.null), plots)
    sm <- saved_meaning(section, title, text, verdict)
    entry <- list(id = id, section = section, title = title, time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                  data = data_description(), text = as.character(text), tables = tables, plots = plots, code = code,
                  verdict = sm$verdict, meaning = sm$meaning, order = save_rank(section, title), fingerprint = fp,
                  evidence = if (is.list(evidence)) evidence else NULL,
                  methods = tryCatch(methods_context(), error = function(e) NULL))
    lst <- saved_results()
    lst[[id]] <- entry
    saved_results(lst)
    for (j in seq_along(plots)) {
      local({
        pl <- plots[[j]]
        output[[paste0(id, "_plot", j)]] <- renderPlot(draw_saved_plot(pl))
      })
    }
    for (j in seq_along(tables)) {
      local({
        tb <- tables[[j]]
        output[[paste0(id, "_table", j)]] <- renderTable(tb, striped = TRUE, spacing = "xs")
      })
    }
    notify(paste0("Saved to Summary & export: ", title), duration = 3)
  }
  start_text <- function() {
    if (isTRUE(safe_get(is_toy()))) return("set by the simulation")
    {
      afe <- miss_start_age()
      paste0(if (isTRUE(is.finite(afe))) paste0("missingness counted from age ", format_num(afe), " (first trait expression); ") else "",
             if (identical(input$afr_mode %||% "auto", "column")) "AFR from a mapped column" else "AFR from each individual's first record")
    }
  }

  observeEvent(input$save_integrity, disappr_guard("input$save_integrity", {
    ig <- safe_get(integrity())
    if (is.null(ig)) return(nothing_to_save())
    add_saved("1 Data", "Data integrity checks",
              text = c(ig$family$text, if (length(ig$bad_ls_ids)) paste("IDs with LS < last record:", paste(utils::head(ig$bad_ls_ids, 30), collapse = ", "))),
              tables = list(`Integrity checks` = ig$table))
  }))
  observeEvent(input$save_distribution, disappr_guard("input$save_distribution", {
    pl <- safe_get(dist_plot_obj())
    if (is.null(pl)) return(nothing_to_save())
    add_saved("1 Data", paste("Distribution of", input$dist_var),
              text = paste("One value per", if (identical(input$dist_unit, "individual")) "individual" else "row"), plots = list(pl))
  }))
  observeEvent(input$save_a1, disappr_guard("input$save_a1", {
    pl <- safe_get(a1_plot_obj())
    if (is.null(pl)) return(nothing_to_save())
    px <- safe_get(a1_px()) %||% ""
    a1_note <- c(safe_get(a2_trend_lines()) %||% "", a1_facet_text())
    add_saved(evidence = ev_visual(), "2 Visual diagnosis", paste("Trait trajectory by", px, "bin"),
              text = paste0(sprintf("%s %s bins (%s boundaries); at least 3 individuals per point; trait scale: %s",
                                    input$n_bins %||% 4, px, input$bin_method %||% "quantile", input$trait_scale %||% "raw"),
                            a1_facet_text()),
              verdict = visual_verdict(a1_note),
              plots = list(pl))
  }))
  observeEvent(input$save_a1_diff, disappr_guard("input$save_a1_diff", {
    pl <- safe_get(bin_diff_obj())
    dz <- safe_get(bin_diff_data())
    if (is.null(pl) || is.null(dz)) return(nothing_to_save())
    faceted <- !is.null(a1_facet_map())
    tab <- data.frame(Pair = as.character(dz$pair), Age = dz$age, Difference = signif(dz$difference, 4))
    if (faceted) tab <- cbind(Panel = dz$facet, tab, stringsAsFactors = FALSE)
    tr <- safe_get(bin_diff_trends())
    if (!is.null(tr) && nrow(tr)) {
      tr <- data.frame(Panel = tr$Facet, Pair = tr$Pair, Ages = tr$N_ages, From = tr$Age_from, To = tr$Age_to,
                       `Slope per unit age` = signif(tr$Slope_per_age, 3), P = vapply(tr$P, format_p, character(1)),
                       check.names = FALSE, stringsAsFactors = FALSE)
      if (!faceted) tr$Panel <- NULL
    }
    pooled <- NULL
    if (identical(input$diff_lines, "pooled")) {
      pz <- safe_get(bin_diff_pooled())
      if (!is.null(pz) && nrow(pz)) {
        pooled <- data.frame(Panel = pz$facet, `Slope per unit age (all pairs)` = signif(pz$Slope_per_age, 3), P = vapply(pz$P, format_p, character(1)),
                             Points = pz$N_points, check.names = FALSE, stringsAsFactors = FALSE)
        if (!faceted) pooled$Panel <- NULL
      }
    }
    add_saved(evidence = ev_visual(), "2 Visual diagnosis", paste("Difference between", safe_get(a1_px()) %||% "", "bins at each age"),
              text = paste0("Consecutive bin pairs; trend lines: ", if (identical(input$diff_lines, "pooled")) "one across all pairs" else "one per pair", "; shaded: 95% confidence bands", a1_facet_text()),
              plots = list(pl))
  }))
  observeEvent(input$save_a2, disappr_guard("input$save_a2", {
    pl <- safe_get(a2_plot_obj())
    if (is.null(pl)) return(nothing_to_save())
    px <- safe_get(a2_px()) %||% "ALR"
    stat <- safe_get(proxy_slopes_by_age(visual_dat(), proxy_values(imet(), px), "r"))
    sl <- safe_get(a2_slopes())
    add_saved(evidence = ev_visual(), "2 Visual diagnosis", paste("Trait against", px, "within age bins"),
              text = c(safe_get(a2_trend_lines()),
                       if (!is.null(stat)) paste("Correlation at each age:", a2_interpretation(stat, px))),
              plots = list(pl))
  }))
  observeEvent(input$save_heatmap, disappr_guard("input$save_heatmap", {
    pl <- safe_get(heatmap_obj())
    if (is.null(pl)) return(nothing_to_save())
    g <- safe_get(grid_r())
    between <- NA_real_
    if (!is.null(g) && "pattern" %in% names(g)) {
      nb <- sum(g$pattern == "Missed: between records")
      no <- sum(g$pattern == "Observed")
      if (nb + no > 0) between <- 100 * nb / (nb + no)
    }
    add_saved("3 Missingness and proxies", "Sampling grid",
              text = c(if (is.finite(between)) sprintf("%.1f%% of expected occasions between each individual's first and last recorded age were missed.", between),
                       paste("Start of sampling:", start_text()),
                       paste("Rows ordered by", switch(input$heat_order %||% "alr", alr = "ALR", afr = "AFR", trait = "mean trait value", id = "ID", "random order"))),
              plots = list(pl))
  }))
  observeEvent(input$save_sampling, disappr_guard("input$save_sampling", {
    ms <- safe_get(msum())
    if (is.null(ms)) return(nothing_to_save())
    add_saved(evidence = ev_missing(), "3 Missingness and proxies", "Sampling summary and proxy guidance",
              text = c(sprintf("Missed expected occasions: %.1f%%; detection p = %.2f", ms$percent, ms$p), ms$type, ms$guidance,
                       paste("Missed occasions:", ms$pattern),
                       sprintf("r(mean age, ALR) = %s; r(ALR, LS) = %s; r(mean age, LS) = %s", fmt_r(ms$r_mean_alr), fmt_r(ms$r_alr_ls), fmt_r(ms$r_mean_ls)),
                       paste("Start of sampling:", start_text())),
              tables = list(`Associations with missingness` = ms$drivers))
  }))
  observeEvent(input$save_missing_age, disappr_guard("input$save_missing_age", {
    pl <- safe_get(missing_by_age_obj())
    if (is.null(pl)) return(nothing_to_save())
    add_saved(evidence = ev_missing(), "3 Missingness and proxies", "Missingness and sample size by age",
              tables = list(`Coverage by age` = safe_get(coverage_by_age(grid_r()))), plots = list(pl))
  }))
  observeEvent(input$save_missing_var, disappr_guard("input$save_missing_var", {
    pl <- safe_get(missing_vs_var_obj())
    ms <- safe_get(msum())
    if (is.null(pl) && is.null(ms)) return(nothing_to_save())
    add_saved(evidence = ev_missing(), "3 Missingness and proxies", paste("Missingness against", input$miss_var %||% "ALR"),
              tables = list(`Associations with missingness` = if (!is.null(ms)) ms$drivers else NULL), plots = list(pl))
  }))
  observeEvent(input$save_proxies, disappr_guard("input$save_proxies", {
    lk <- isTRUE(safe_get(life_known()))
    pls <- list(safe_get(proxy_alr_mean_obj()), if (lk) safe_get(proxy_alr_ls_obj()), if (lk) safe_get(proxy_mean_ls_obj()))
    pls <- Filter(Negate(is.null), pls)
    if (!length(pls)) return(nothing_to_save())
    ms <- safe_get(msum())
    add_saved("3 Missingness and proxies", "Agreement between ALR, mean age and lifespan",
              text = if (!is.null(ms)) sprintf("r(mean age, ALR) = %s; r(ALR, LS) = %s; r(mean age, LS) = %s",
                                              fmt_r(ms$r_mean_alr), fmt_r(ms$r_alr_ls), fmt_r(ms$r_mean_ls)),
              plots = pls)
  }))
  observeEvent(input$save_a3, disappr_guard("input$save_a3", {
    pl <- safe_get(a3_mean_obj())
    z <- safe_get(a3_fit())
    if (is.null(pl) || is.null(z) || !is.null(z$error)) return(nothing_to_save())
    fn <- input$a3_function %||% "Quadratic"
    im <- safe_get(imet())
    txt <- c(sprintf("%s function fitted to %d of %d individuals", fn, length(z$fitted_ids), z$n_total),
             paste("Population curve:", if (isTRUE(z$nonlinear) || isTRUE(z$log_scale)) "mean of coefficients and mean of individual functions" else "mean of coefficients",
                   if (isTRUE(z$log_scale)) "(fitted on the log scale for a count trait: the two differ)"
                   else if (isTRUE(z$nonlinear)) "(non-linear function: the two differ)" else "(linear in parameters: the two are identical)"))
    if (!is.null(im) && length(z$fitted_ids)) {
      txt <- c(txt, sprintf("Mean ALR of fitted individuals %s vs %s for all individuals",
                            format_num(mean(im$alr[im$id %in% z$fitted_ids], na.rm = TRUE)), format_num(mean(im$alr, na.rm = TRUE))))
    }
    cf <- z$coefs
    nms <- setdiff(names(cf), "id")
    tab <- data.frame(Coefficient = nms,
                      Mean = signif(vapply(cf[nms], function(v) mean(v, na.rm = TRUE), numeric(1)), 5),
                      SD = signif(vapply(cf[nms], function(v) stats::sd(v, na.rm = TRUE), numeric(1)), 4))
    add_saved("4 Individual and population trajectories", "Mean-coefficient trajectory", text = txt, tables = list(`Individual coefficients` = tab), plots = list(pl))
  }))
  observeEvent(input$save_a3_compare, disappr_guard("input$save_a3_compare", {
    tab <- a3_compare_current()
    if (is.null(tab) || !nrow(tab)) return(nothing_to_save())
    tab[] <- lapply(tab, function(v) if (is.numeric(v)) signif(v, 4) else v)
    add_saved("4 Individual and population trajectories", "Individual-level function comparison", tables = list(`Function comparison` = tab))
  }))
  observeEvent(input$save_b2, disappr_guard("input$save_b2", {
    r <- safe_get(b2_shown())
    if (is.null(r)) return(nothing_to_save("Run 'Compare ageing functions' first."))
    s <- model_settings()
    add_saved("4 Individual and population trajectories", "Population-level ageing functions",
              text = paste0("Model 1 structure; ", family_label(s$family), "; ", slope_text(s$random_slope)),
              tables = list(`Function comparison` = r$table), plots = list(safe_get(b2_plot_obj())))
  }))
  observeEvent(input$save_models, disappr_guard("input$save_models", {
    r <- safe_get(fitted_models())
    if (is.null(r)) return(nothing_to_save("Fit models first."))
    aic <- r$aic
    best_id <- sub("^Model ", "M", aic$Model[[1]])
    elig <- aic[aic$Eligible, , drop = FALSE]
    txt <- c(sprintf("%s; %s ageing function; random effects %s", family_label(r$family), r$age_function,
                     if (isTRUE(r$nonlinear)) r$random else random_display(meta(), r$random_slope)),
             if (nzchar(r$random_note %||% "")) r$random_note,
             if (isTRUE(r$n_excluded > 0)) sprintf("%d fit(s) with an invalid Hessian excluded from the ranking", r$n_excluded),
             if (r$n_dropped > 0) sprintf("%d rows dropped from all models (%s)", r$n_dropped, paste(r$drop_by, collapse = "; ")))
    ms_now <- model_settings()
    txt <- c(txt, sprintf("Model settings: among-individual terms %s; age and proxies %s; zero-inflation %s; models compared: %s.",
                          switch(ms_now$among %||% "linear", consistent = "higher order (consistent decomposition)", same = "higher order (powers of the mean)", "linear"),
                          if (isTRUE(r$standardise)) "standardised" else "on their original scales",
                          if (nzchar(ms_now$zi %||% "") && !identical(ms_now$zi, "~1")) display_term(ms_now$zi) else "none",
                          paste(aic$Model, collapse = ", ")))
    aic_tab <- aic_display(aic)
    status_of <- stats::setNames(unlist(r$status, use.names = FALSE), model_label(names(r$status)))
    aic_tab$`Fit status` <- unname(status_of[aic_tab$Model])
    add_saved(evidence = ev_models(), "5 Modelling", "Model comparison", text = txt, verdict = models_verdict(),
              tables = list(`Model support` = aic_tab, `Likelihood-ratio tests` = lrt_display(r$lrt)))
  }))
  observeEvent(input$save_predictions, disappr_guard("input$save_predictions", {
    pl <- safe_get(pred_plot_obj())
    if (is.null(pl)) return(nothing_to_save("Fit models first."))
    dv <- if (isTRUE(safe_get(is_toy()))) safe_get(deviation_tab()) else NULL
    add_saved("5 Modelling", "Model predictions trajectory", verdict = models_verdict(),
              text = paste0("Models drawn: ", paste(model_label(intersect(input$pred_models %||% safe_get(eligible_models()), safe_get(eligible_models()) %||% character(0))), collapse = ", "),
                            if (nzchar(input$pred_by %||% "")) paste0("; by ", input$pred_by) else "",
                            if (isTRUE(input$show_a3)) paste0("; reconstruction from individual fits with the ", input$a3_function %||% "Quadratic", " function") else "",
                            if (isTRUE(input$show_raw_points)) "; individual records shown as jittered points" else ""),
              plots = list(pl))
  }))
  observeEvent(input$save_coefs, disappr_guard("input$save_coefs", {
    r <- safe_get(fitted_models())
    if (is.null(r)) return(nothing_to_save("Fit models first."))
    m <- input$coef_model %||% sub("Model ", "M", r$aic$Model[[1]])
    x <- r$coefficients[r$coefficients$Model == model_label(m), , drop = FALSE]
    if (!nrow(x)) return(nothing_to_save())
    tab <- coef_display(x, r)
    sc <- safe_get(scaling_constants(r))
    best <- r$aic$Model[is.finite(r$aic$Delta_AIC)][1]
    fitted_eq <- if (isTRUE(r$nonlinear)) paste(model_label(m), "fitted as:", r$formulas[[m]])
                 else paste0(model_label(m), " fitted as: trait ~ ", display_term(r$formulas[[m]]), " + ", display_term(r$random),
                             "  (", family_label(r$family), ", ", r$age_function, " ageing function)")
    add_saved("5 Modelling", paste("Coefficients of", model_label(m)),
              text = c(fitted_eq,
                       if (isTRUE(r$standardise)) "Estimates on the standardised scale." else "Estimates on the original scales.",
                       safe_get(term_verdict_line(r, m))),
              tables = list(`Fixed effects` = tab, `Scaling constants` = if (!is.null(sc)) data.frame(Variable = sc$Variable, Centre = signif(sc$Centre, 5), Scale = signif(sc$Scale, 5)) else NULL,
                            `Random effects` = safe_get(coef_re_tab())))
  }))
  observeEvent(input$save_family, disappr_guard("input$save_family", {
    fr <- family_res()
    if (is.null(fr) || !isTRUE(fr$ok)) return(nothing_to_save("Run the error-family check first."))
    add_saved("5 Modelling", paste("Error-family check with", model_label(fr$model)), text = if (nzchar(fr$skipped %||% "")) fr$skipped else NULL, tables = list(`Error families` = fr$table))
  }))
  observeEvent(input$save_dharma, disappr_guard("input$save_dharma", {
    z <- dharma_res()
    if (is.null(z) || !isTRUE(z$ok)) return(nothing_to_save("Simulate residuals first."))
    sim <- z$sim
    add_saved("5 Modelling", paste("Residual checks for", model_label(z$model)),
              tables = list(`DHARMa tests` = data.frame(Test = z$table$Test, P = vapply(z$table$P_value, format_p, character(1)))),
              plots = list(function() graphics::plot(sim)))
  }))
  observeEvent(input$save_a5, disappr_guard("input$save_a5", {
    pl <- safe_get(a5_plot_obj())
    if (is.null(pl)) return(nothing_to_save())
    add_saved("2 Visual diagnosis", "Trait before death", text = safe_get(a5_lines()),
              verdict = visual_verdict(safe_get(a5_lines())), plots = list(pl))
  }))
  observeEvent(input$save_a6, disappr_guard("input$save_a6", {
    pl <- safe_get(a6_plot_obj())
    s <- safe_get(a6_data())
    if (is.null(pl) || is.null(s) || !nrow(s)) return(nothing_to_save())
    add_saved("2 Visual diagnosis", "Selection differentials by age", text = safe_get(a6_lines()),
              tables = list(`Selection differentials` = data.frame(Age = s$Age, N = s$N, Disappearing = s$Disappearing,
                                                                   Differential = signif(s$Differential, 3), SE = signif(s$Differential_SE, 3),
                                                                   `Survivors - disappearing` = signif(s$Contrast, 3), check.names = FALSE)),
              plots = list(pl))
  }))
  observeEvent(input$save_a7, disappr_guard("input$save_a7", {
    pl <- safe_get(a7_plot_obj())
    if (is.null(pl)) return(nothing_to_save())
    add_saved("2 Visual diagnosis", "Disappearance hazard", text = safe_get(a7_lines()),
              verdict = visual_verdict(safe_get(a7_lines())), plots = list(pl))
  }))
  observeEvent(input$save_performance, disappr_guard("input$save_performance", {
    z <- perf_res()
    if (is.null(z) || !isTRUE(z$ok)) return(nothing_to_save("Run the performance checks first."))
    cm <- z$check_model
    add_saved("5 Modelling", paste("performance checks for", model_label(z$model)), text = if (nzchar(z$plot_message %||% "")) z$plot_message else NULL,
              tables = list(`performance checks` = z$table), plots = if (!is.null(cm)) list(draw_check_model(cm)) else list())
  }))
  observeEvent(input$save_code, disappr_guard("input$save_code", {
    code <- safe_get(code_text())
    if (is.null(code)) return(nothing_to_save("Fit models first."))
    add_saved("5 Modelling", "Reproducible R code", code = code)
  }))

  output$saved_controls <- renderUI({
    lst <- saved_results()
    if (!length(lst)) return(p(class = "small-note", "No saved results yet."))
    ch <- stats::setNames(names(lst), vapply(lst, function(e) paste0(e$section, " \u00b7 ", e$title, " (", e$time, ")"), character(1)))
    tagList(
      selectInput("saved_remove_id", "Remove a saved result", choices = ch),
      actionButton("saved_remove", "Remove", icon = icon("trash"), class = "btn-default btn-sm"),
      actionButton("saved_clear", "Clear all", icon = icon("broom"), class = "btn-default btn-sm")
    )
  })
  observeEvent(input$saved_remove, disappr_guard("input$saved_remove", {
    lst <- saved_results()
    id <- input$saved_remove_id
    if (!is.null(id) && id %in% names(lst)) {
      lst[[id]] <- NULL
      saved_results(lst)
    }
  }))
  observeEvent(input$saved_clear, disappr_guard("input$saved_clear", saved_results(list())))

  output$saved_ui <- renderUI({
    lst <- saved_results()
    if (!length(lst)) {
      return(div(class = "truth-card", "Nothing saved yet. Use the 'Save to summary' buttons on the other tabs; saved results appear here and make up the downloaded reports."))
    }
    cs <- concordance_state(lst)
    tagList(
      lapply(lst[order(vapply(lst, function(e) as.numeric(e$order %||% 9999), numeric(1)), seq_along(lst))], function(e) {
      div(class = "summary-block",
          h4(paste0(e$section, " \u00b7 ", e$title)),
          if (length(e$text)) tags$ul(lapply(e$text, tags$li)) else NULL,
          tagList(lapply(seq_along(e$tables), function(j) {
            cap <- names(e$tables)[j]
            tagList(if (!is.null(cap) && !is.na(cap) && nzchar(cap)) h5(strong(cap)) else NULL,
                    tableOutput(paste0(e$id, "_table", j)))
          })),
          tagList(lapply(seq_along(e$plots), function(j) plotOutput(paste0(e$id, "_plot", j), height = 380))),
          if (!is.null(e$code)) tags$pre(class = "code-out", e$code) else NULL)
    }))
  })

  # Reproducibility bundle: everything needed to re-run this analysis elsewhere.
  reproducibility_bundle <- reactive({
    r <- safe_get(fitted_models())
    p <- if (!is.null(r) && !is.null(r$provenance)) r$provenance else tryCatch(analysis_provenance(list(data = safe_get(dat()) %||% data.frame(), family = input$model_family %||% "", age_function = input$model_age_function %||% "", formulas = list(), random = "", zi = "", standardise = TRUE, age_params = NULL, proxy_params = NULL, status = list(), validity = list(), drop_by = character(0), alias_notes = character(0), fit_notes = character(0), among = input$among_order %||% "linear", extra = NULL, include_invalid = FALSE, random_request = normalise_slope(input$random_structure %||% "none")), safe_get(meta())), error = function(e) NULL)
    p
  })
  output$download_bundle <- downloadHandler(
    filename = function() paste0("disappR_reproducibility_", Sys.Date(), ".json"),
    content = function(file) {
      con <- file(file, open = "w", encoding = "UTF-8")
      on.exit(close(con))
      writeLines(as.character(provenance_json(reproducibility_bundle())), con)
    }
  )
  output$download_report_html <- downloadHandler(
    filename = function() paste0("disappR_report_", Sys.Date(), ".html"),
    content = function(file) {
      con <- file(file, open = "w", encoding = "UTF-8")
      on.exit(close(con))
      writeLines(html_report(saved_results(), "disappR report: saved results", trait = safe_get(trait_label()) %||% "the trait"), con)
    }
  )
  output$download_report <- downloadHandler(
    filename = function() paste0("disappR_report_", Sys.Date(), ".txt"),
    content = function(file) {
      con <- file(file, open = "w", encoding = "UTF-8")
      on.exit(close(con))
      writeLines(text_report(saved_results(), "disappR report: saved results", trait = safe_get(trait_label()) %||% "the trait"), con)
    }
  )
  # ======================================================================
  # 0.22.0: sampling interval, current data, stored models, peak and onset, random-effect covariance, methods
  # ======================================================================
  output$age_step_note <- renderUI({
    d <- safe_get(dat())
    if (is.null(d)) return(NULL)
    inferred <- infer_age_step(d$age, d$id)
    man <- age_step_manual()
    if (identical(input$age_step_auto, FALSE) && is.null(man)) {
      return(p(class = "small-note", style = "color:#A50026;",
               sprintf("Enter a positive interval; the inferred %s is used until then.", format_num(inferred))))
    }
    txt <- if (is.null(man)) sprintf("Inferred interval: %s, used app-wide.", format_num(inferred))
           else sprintf("Set to %s, used app-wide (inferred: %s).", format_num(man), format_num(inferred))
    warn <- NULL
    if (!is.null(man)) {
      rng <- diff(range(d$age[is.finite(d$age)]))
      if (is.finite(rng) && man > rng) {
        warn <- "Longer than the whole age range: records fall on one or two occasions."
      } else if (is.finite(inferred) && man < inferred / 1.5) {
        warn <- "Shorter than most gaps between records: missingness rises and the decomposition links fewer individuals."
      }
    }
    tagList(p(class = "small-note", txt), if (!is.null(warn)) p(class = "small-note", style = "color:#A50026;", warn))
  })
  # unticking 'infer' starts the manual entry from the inferred interval
  observeEvent(input$age_step_auto, disappr_guard("input$age_step_auto", {
    if (!identical(input$age_step_auto, FALSE)) return(invisible(NULL))
    if (isTRUE(is.finite(num_input(input$age_step_value, NA_real_)))) return(invisible(NULL))
    d <- safe_get(dat())
    if (is.null(d)) return(invisible(NULL))
    st <- infer_age_step(d$age, d$id)
    if (is.finite(st)) updateNumericInput(session, "age_step_value", value = signif(st, 6))
  }), ignoreInit = TRUE)

  # ---- current data, in the sidebar ----
  TOY_MISSING_LABELS <- c(complete = "complete", mcar = "missing completely at random (~25%)", mwo = "missing when old",
                          mwy = "missing when young", trait = "trait-dependent missingness", condition = "condition-dependent missingness")
  output$current_data_box <- renderUI({
    src <- input$data_source %||% "toy"
    d <- safe_get(dat())
    row <- function(lab, val) div(class = "cd-row", span(class = "cd-lab", paste0(lab, ": ")), val)
    recs <- if (!is.null(d)) sprintf("%s records, %s individuals", format(nrow(d), big.mark = ","), format(length(unique(d$id)), big.mark = ",")) else NULL
    rows <- list()
    if (identical(src, "toy")) {
      cfg <- utils::modifyList(TOY_DEFAULTS, toy_cfg())
      paper <- identical(cfg$trait, "paper")
      form <- if (paper) "Quadratic" else cfg$form
      sel <- function(x) names(SELECTION_TYPES)[SELECTION_TYPES == x][1]
      dir_txt <- function(x) if (isTRUE(as.numeric(x) < 0)) "negative" else "positive"
      rows <- list(
        div(class = "cd-head", "Current data: simulated"),
        row("Trait", names(TOY_TRAITS)[TOY_TRAITS == cfg$trait][1]),
        row("Sample size", paste0(format(cfg$n_id, big.mark = ","), " individuals", if (!is.null(recs)) paste0(" (", format(nrow(d), big.mark = ","), " records)") else "")),
        row("Average time steps", paste0(if (paper) 25 else cfg$mean_ls, " (mean lifespan)")),
        row("Functional form", paste0(form, if (!paper) paste0(": ", tolower(toy_shape_label(form, cfg$shape))) else "")),
        row("Selective disappearance", paste0(sel(cfg$sd_type), if (!identical(cfg$sd_type, "none")) paste0(", ", dir_txt(cfg$sd_dir)) else "")),
        row("Selective appearance", if (identical(cfg$afr_mode, "individual")) paste0(sel(cfg$sa_type), if (!identical(cfg$sa_type, "none")) paste0(", ", dir_txt(cfg$sa_dir)) else "", "; AFR varies")
                                    else "none (the same AFR for all)"),
        row("Strength", if (paper) "as in the manuscript" else if (identical(cfg$strength, "dramatic")) "dramatic" else "moderate"),
        row("Individual differences in ageing rate", if (identical(cfg$rate_var, "high")) "large" else "small"),
        row("Sampling", unname(TOY_MISSING_LABELS[cfg$missingness]) %||% cfg$missingness),
        if (isTRUE(cfg$diet) || isTRUE(cfg$groups)) row("Also", paste(c(if (isTRUE(cfg$diet)) "diet covariate", if (isTRUE(cfg$groups)) "25 families"), collapse = "; ")),
        row("Seed", cfg$seed))
      if (isTRUE(safe_get(toy_settings_pending()))) rows <- c(rows, list(div(class = "cd-warn", "Settings changed: not yet simulated.")))
    } else if (identical(src, "example")) {
      ex <- current_example()
      rows <- list(div(class = "cd-head", "Current data: published example"), div(class = "cd-row", ex$label),
                   if (!is.null(recs)) row("Size", recs))
    } else {
      rows <- list(div(class = "cd-head", "Current data: uploaded file"), div(class = "cd-row", input$data_file$name %||% "no file yet"),
                   if (!is.null(recs)) row("Size", recs))
    }
    if (!is.null(d)) {
      mp <- safe_get(current_map())
      st <- resolve_age_step(d$age, d$id, age_step_manual())
      rows <- c(rows, list(
        if (!identical(src, "toy") && !is.null(mp)) row("Trait column", mp$trait),
        row("Sampling interval", paste0(format_num(st), if (is.null(age_step_manual())) " (inferred)" else " (set)"))))
    }
    div(class = "current-data", Filter(Negate(is.null), rows))
  })

  # ---- stored models: compare models across fits ----
  model_store <- reactiveVal(list())
  store_counter <- reactiveVal(0L)
  output$store_pick_ui <- renderUI({
    r <- safe_get(fitted_models())
    if (is.null(r) || !isTRUE(r$ok)) return(p(class = "small-note", "Fit models, then tick the ones to store. Stored models stay when you refit."))
    ch <- intersect(sub("^Model ", "M", r$aic$Model[is.finite(r$aic$AIC)]), names(r$fits))
    checkboxGroupInput("store_pick", "Models from the current fit", choices = stats::setNames(ch, model_label(ch)), inline = TRUE)
  })
  observeEvent(input$store_models, disappr_guard("input$store_models", {
    r <- safe_get(fitted_models())
    if (is.null(r) || !isTRUE(r$ok)) { notify("Fit models first.", type = "warning"); return(invisible(NULL)) }
    pick <- intersect(input$store_pick %||% character(0), names(r$fits))
    if (!length(pick)) { notify("Tick at least one model to store.", type = "warning"); return(invisible(NULL)) }
    st <- model_store()
    keys <- vapply(st, function(e) e$key, character(1))
    added <- 0L; dup <- 0L
    for (m in pick) {
      e <- tryCatch(stored_model_entry(r, m, safe_get(data_sig()) %||% ""), error = function(err) NULL)
      if (is.null(e)) next
      # the model's predicted population trajectory, kept for the comparison plot (0.24.7)
      e$curve <- tryCatch({
        pc <- predict_population_curve(r$fits[[m]], r, smooth_prediction_ages(r$data$age))
        pc[is.finite(pc$fitted), c("age", "fitted"), drop = FALSE]
      }, error = function(err) NULL)
      if (e$key %in% keys) { dup <- dup + 1L; next }
      k <- store_counter() + 1L
      store_counter(k)
      e$id <- paste0("S", k)
      st[[e$id]] <- e
      keys <- c(keys, e$key)
      added <- added + 1L
    }
    model_store(st)
    notify(sprintf("%d model(s) stored%s.", added, if (dup > 0) sprintf("; %d already stored", dup) else ""), duration = 4)
  }))
  store_table_r <- reactive(stored_comparison_table(model_store()))
  output$store_table <- renderTable({
    tab <- store_table_r()
    if (!nrow(tab)) return(data.frame(Note = "No models stored yet."))
    tab$df <- NULL
    names(tab)[names(tab) == "dAIC"] <- "\u0394AIC"
    tab
  }, striped = TRUE, spacing = "xs")
  output$store_note <- renderUI({
    st <- model_store()
    if (!length(st)) return(NULL)
    cur <- safe_get(data_sig()) %||% ""
    other <- sum(vapply(st, function(e) !identical(e$data_sig, cur), logical(1)))
    tagList(
      p(class = "small-note", "\u0394AIC is within a set: same records, same likelihood type. Any two models in a set are comparable."),
      lapply(stored_set_notes(st), function(x) p(class = "small-note", style = "color:#8a5a00;", x)),
      if (other > 0) p(class = "small-note", sprintf("%d stored model(s) from other data or data settings.", other)) else NULL)
  })
  output$store_manage_ui <- renderUI({
    st <- model_store()
    if (!length(st)) return(NULL)
    tagList(
      selectizeInput("store_remove_ids", NULL, choices = names(st), multiple = TRUE, options = list(placeholder = "stored models to remove")),
      actionButton("store_remove", "Remove", class = "btn-default btn-sm"),
      actionButton("store_clear", "Clear all", class = "btn-default btn-sm"))
  })
  # predicted trajectories of the stored models, for the models ticked in 'store_plot_ids' (0.24.7)
  output$store_plot_pick_ui <- renderUI({
    st <- model_store()
    has <- names(st)[vapply(st, function(e) is.data.frame(e$curve) && nrow(e$curve) > 1, logical(1))]
    if (!length(has)) return(p(class = "small-note", "Store models to compare their predicted trajectories here."))
    lab <- vapply(st[has], function(e) paste0(e$id, ": ", e$label, " (", e$spec, ")"), character(1))
    checkboxGroupInput("store_plot_ids", "Stored models to draw", choices = stats::setNames(has, lab), selected = has)
  })
  output$store_pred_plot <- renderPlot({
    st <- model_store()
    ids <- intersect(input$store_plot_ids %||% character(0), names(st))
    shiny::validate(shiny::need(length(ids) > 0, "Store models and tick them above to draw their predicted trajectories."))
    df <- do.call(rbind, lapply(ids, function(k) {
      e <- st[[k]]
      if (!is.data.frame(e$curve) || !nrow(e$curve)) return(NULL)
      data.frame(age = e$curve$age, fitted = e$curve$fitted, Model = paste0(e$id, ": ", e$label), stringsAsFactors = FALSE)
    }))
    shiny::validate(shiny::need(!is.null(df) && nrow(df) > 0, "No predictions are available for the ticked models: store them again from a new fit."))
    ggplot(df, aes(age, fitted, colour = Model)) + geom_line(linewidth = 1) +
      labs(x = "Age", y = "Predicted trait (response scale)", colour = NULL) + theme_disappR(12) +
      theme(legend.position = "bottom", legend.direction = "vertical")
  }, height = 380)
  observeEvent(input$store_remove, disappr_guard("input$store_remove", {
    st <- model_store()
    model_store(st[setdiff(names(st), input$store_remove_ids %||% character(0))])
  }))
  observeEvent(input$store_clear, disappr_guard("input$store_clear", model_store(list())))
  observeEvent(input$save_store, disappr_guard("input$save_store", {
    tab <- store_table_r()
    if (!nrow(tab)) return(nothing_to_save("Store models first."))
    names(tab)[names(tab) == "dAIC"] <- "Delta AIC"
    add_saved("5 Modelling", "Cross-fit model comparison",
              text = c("Delta AIC is within a set (same data, records and likelihood type).",
                       stored_set_notes(model_store())),
              tables = list(`Stored models` = tab))
  }))

  # ---- peak age and onset of senescence, for every fitted model ----
  peak_all <- reactive({
    r <- fitted_models()
    lapply(stats::setNames(names(r$fits), names(r$fits)), function(m) {
      tryCatch(peak_onset_summary(r, m), error = function(e) list(ok = FALSE, model = m, message = conditionMessage(e)))
    })
  })
  peak_table_df <- reactive({
    z <- peak_all()
    ci <- function(l) if (length(l) == 2 && all(is.finite(l))) paste0(format_num(l[[1]]), "\u2013", format_num(l[[2]])) else ""
    rows <- lapply(names(z), function(m) {
      x <- z[[m]]
      if (!isTRUE(x$ok)) {
        return(data.frame(Model = model_label(m), `Peak age` = "not available", `Peak 95% CI` = "", `Onset of final decline` = "",
                          `Onset 95% CI` = "", check.names = FALSE, stringsAsFactors = FALSE))
      }
      tp <- x$turning
      data.frame(Model = model_label(m),
                 `Peak age` = if (isTRUE(tp$peak_interior)) format_num(tp$peak) else "none within the sampled ages",
                 `Peak 95% CI` = if (isTRUE(tp$peak_interior)) ci(x$ci$peak) else "",
                 `Onset of final decline` = switch(tp$onset_type, interior = format_num(tp$onset),
                                                   from_start = paste("at or before", format_num(x$age_range[[1]])),
                                                   no_final_decline = "no final decline", ""),
                 `Onset 95% CI` = if (identical(tp$onset_type, "interior")) ci(x$ci$onset) else "",
                 check.names = FALSE, stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
  })
  output$peak_table <- renderTable(peak_table_df(), striped = TRUE, spacing = "xs")
  output$peak_sentences <- renderUI({
    z <- peak_all()
    div(class = "small-note", lapply(z, function(x) p(peak_onset_sentence(x))))
  })
  observeEvent(input$save_peak, disappr_guard("input$save_peak", {
    tab <- safe_get(peak_table_df())
    z <- safe_get(peak_all())
    if (is.null(tab) || is.null(z)) return(nothing_to_save("Fit models first."))
    add_saved("5 Modelling", "Peak age and onset of senescence", text = vapply(z, peak_onset_sentence, character(1)),
              tables = list(`Peak and onset` = tab))
  }))

  # ---- among-individual covariance between the random effects and the lifespan proxies ----
  output$blup_model_ui <- renderUI({
    r <- safe_get(fitted_models())
    if (is.null(r)) return(NULL)
    ch <- names(r$fits)
    cur <- isolate(input$blup_model)
    selectInput("blup_model", "Random effects from", choices = stats::setNames(ch, model_label(ch)),
                selected = if (isTRUE(cur %in% ch)) cur else if ("M1" %in% ch) "M1" else ch[[1]])
  })
  blup_res <- reactive({
    r <- fitted_models()
    req(input$blup_model)
    tryCatch(blup_proxy_covariance(r, input$blup_model), error = function(e) list(ok = FALSE, message = conditionMessage(e)))
  })
  blup_table_df <- reactive({
    z <- blup_res()
    shiny::validate(shiny::need(isTRUE(z$ok), z$message %||% "Not available for this model."))
    t <- z$table
    data.frame(`Random effect` = t$Component, Proxy = t$Proxy, N = t$N, Covariance = sig_text(t$Covariance, 3), r = sprintf("%.3f", t$r),
               `95% CI of r` = paste0(round(t$Lower_95, 3), " to ", round(t$Upper_95, 3)), P = vapply(t$P, format_p, character(1)),
               check.names = FALSE, stringsAsFactors = FALSE)
  })
  output$blup_table <- renderTable(blup_table_df(), striped = TRUE, spacing = "xs")
  blup_plot_obj <- reactive({
    z <- blup_res()
    shiny::validate(shiny::need(isTRUE(z$ok), z$message %||% "Not available for this model."))
    ggplot(z$points, aes(proxy, blup)) +
      geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
      geom_point(alpha = 0.35, size = 1.3, colour = "#35637A") +
      geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = "black", linewidth = 0.8) +
      facet_grid(Component ~ Proxy, scales = "free") +
      labs(x = "Proxy (original units)", y = "Predicted random effect", subtitle = paste("Random effects from", model_label(z$model))) +
      theme_disappR(13)
  })
  output$blup_plot <- renderPlot(blup_plot_obj())
  blup_lines <- reactive({
    z <- blup_res()
    if (!isTRUE(z$ok)) return(character(0))
    c(if (length(z$in_model)) sprintf("%s includes %s: only the remaining association is shown (Model 1 shows all of it).",
                                      model_label(z$model), paste(z$in_model, collapse = " and ")),
      if (!isTRUE(z$slopes)) "Level only: fit random slopes to test the ageing rate.",
      "Descriptive: p-values are anticonservative (shrunken random effects; Hadfield et al. 2010).")
  })
  output$blup_note <- renderUI({
    txt <- safe_get(blup_lines())
    if (is.null(txt) || !length(txt)) return(NULL)
    div(class = "small-note", lapply(txt, p))
  })
  observeEvent(input$save_blup, disappr_guard("input$save_blup", {
    tab <- safe_get(blup_table_df())
    pl <- safe_get(blup_plot_obj())
    if (is.null(tab) || is.null(pl)) return(nothing_to_save("Fit models first."))
    add_saved("5 Modelling", paste0("Among-individual covariance with lifespan proxies (", model_label(input$blup_model), ")"),
              text = safe_get(blup_lines()), tables = list(`Random effects and proxies` = tab), plots = list(pl))
  }))

  # ---- auto-written methods ----
  methods_context <- function() {
    d <- safe_get(dat())
    m <- safe_get(meta())
    src <- input$data_source %||% "toy"
    ctx <- list(version = DISAPPR_VERSION)
    if (!is.null(d) && !is.null(m)) {
      ctx$data <- list(
        source = src,
        label = switch(src, example = current_example()$label, upload = input$data_file$name %||% "an uploaded file",
                       sub("^Simulated: ", "", toy_truth_text(toy_cfg())$sim)),
        trait = m$map$trait %||% "the trait", n_rows = nrow(d), n_ind = length(unique(d$id)), n_trait = sum(is.finite(d$trait)),
        age_min = min(d$age, na.rm = TRUE), age_max = max(d$age, na.rm = TRUE),
        step = resolve_age_step(d$age, d$id, m$age_step), step_manual = valid_age_step(m$age_step),
        alr_mapped = isTRUE(m$alr_mapped), afr_mapped = isTRUE(m$entry_mapped), trait_specific_ages = !isFALSE(m$trait_specific_ages),
        life_known = isTRUE(m$has_life) && !isTRUE(m$life_auto), n_censored = m$n_censored %||% 0,
        covars = unname(vapply(m$covars %||% character(0), function(cv) tryCatch(as.character(m$cov_labels[[cv]] %||% cv)[1], error = function(e) cv), character(1))),
        groups = c(if (isTRUE(m$has_group2)) paste("random intercepts for", m$map$group2),
                   if (isTRUE(m$has_group)) paste0("random intercepts for ", m$map$group, if (isTRUE(m$nested)) " (individuals nested within groups)" else ""),
                   if (length(m$random_terms)) paste("random intercepts for", paste(unlist(m$random_labels[m$random_terms]), collapse = ", "))),
        subset = if (!is.null(m$subset)) paste0(m$subset$var, " = ", paste(m$subset$levels, collapse = ", ")) else "")
    }
    ctx$visual <- list(proxy = safe_get(vis_px()) %||% "ALR", n_bins = as.numeric(input$n_bins %||% 4),
                       bin_method = input$bin_method %||% "quantile", trait_scale = input$trait_scale %||% "raw")
    ctx$sampling <- list(miss_start = input$miss_start %||% "afr", afe_age = num_input(input$afe_age, NA_real_))
    ctx$step4 <- list(fn = input$a3_function %||% "Quadratic", scale = safe_get(a3_link()) %||% "identity")
    r <- safe_get(fitted_models())
    if (!is.null(r) && isTRUE(r$ok)) {
      ctx$models <- list(family = r$family, age_function = r$age_function,
                         random = tryCatch(if (isTRUE(r$nonlinear)) r$random else random_display(m, r$random_slope), error = function(e) r$random),
                         among = r$among %||% "linear", standardise = isTRUE(r$standardise), disp = r$disp %||% "constant",
                         zi = if (isTRUE(r$family %in% ZI_FAMILIES)) (r$zi %||% "") else "",
                         models = names(r$fits), n_rows = nrow(r$data), n_ind = length(unique(r$data$id)),
                         decomposition = isTRUE(input$show_decomp), lmertest = isTRUE(HAS_LMERTEST))
      sw <- r$provenance$software
      if (length(sw)) ctx$software <- sw[intersect(c("R", "disappR", "lme4", "glmmTMB", "lmerTest", "DHARMa"), names(sw))]
    }
    z <- safe_get(perm_store())
    if (is.list(z) && is.null(z$error) && length(z$model)) {
      ctx$boot <- list(n = z$n_perm, model = model_label(z$model), null_function = z$null_model$age_function %||% "",
                       disp = z$null_model$disp %||% "constant")
    }
    if (length(input$blup_model)) ctx$blup_model <- model_label(input$blup_model)
    ctx
  }
  methods_seen <- new.env()
  methods_seen$ids <- character(0)
  saved_in_order <- function() {
    lst <- saved_results()
    if (!length(lst)) return(lst)
    lst[order(vapply(lst, function(e) as.numeric(e$order %||% 99999), numeric(1)))]
  }
  output$methods_pick_ui <- renderUI({
    lst <- saved_in_order()
    if (!length(lst)) return(p(class = "small-note", "Save results with 'Save to summary' to describe them here."))
    ids <- names(lst)
    cur <- isolate(input$methods_pick)
    sel <- if (is.null(cur) && !length(methods_seen$ids)) ids else c(intersect(cur %||% character(0), ids), setdiff(ids, methods_seen$ids))
    methods_seen$ids <- ids
    checkboxGroupInput("methods_pick", "Saved results to describe", selected = sel,
                       choices = stats::setNames(ids, vapply(lst, function(e) paste0(e$section, ": ", e$title), character(1))))
  })
  methods_res <- reactive({
    lst <- saved_in_order()
    pick <- input$methods_pick %||% character(0)
    methods_text(unname(lst[names(lst) %in% pick]))
  })
  output$methods_conflicts <- renderUI({
    z <- methods_res()
    if (!length(z$conflicts)) return(NULL)
    div(class = "diagnosis-card", style = "border-left: 5px solid #A50026;",
        div(class = "diagnosis-title", badge("caution"), " Conflicts to resolve"),
        lapply(z$conflicts, function(x) div(class = "diagnosis-detail", x)))
  })
  output$methods_text_ui <- renderUI({
    z <- methods_res()
    if (!length(z$paragraphs)) return(p(class = "small-note", "Tick saved results to draft the methods."))
    tagList(div(class = "methods-draft", style = "background:#fbf8f3; border:1px solid #e3d3bf; border-radius:6px; padding:10px 14px;",
                lapply(z$paragraphs, p)),
            h5(strong("References")), tags$ul(class = "small-note", lapply(z$references, tags$li)))
  })
  output$download_methods <- downloadHandler(
    filename = function() paste0("disappR_methods_", Sys.Date(), ".txt"),
    content = function(file) {
      z <- methods_res()
      con <- file(file, open = "w", encoding = "UTF-8")
      on.exit(close(con))
      writeLines(c("METHODS (draft written by disappR; check every statement before use)", "", z$paragraphs, "",
                   if (length(z$conflicts)) c("CONFLICTS TO RESOLVE", z$conflicts, "") else NULL,
                   "REFERENCES", z$references), con)
    }
  )
}
