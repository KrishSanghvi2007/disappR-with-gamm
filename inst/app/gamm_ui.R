# Step 7 - GAMMs (optional). The tab's interface, built by gamm_tab_ui() from ui.R. Every input and output id starts
# gamm_ (save buttons save_gamm_, help keys info_gamm_), so the tab shares nothing with steps 1-6 except the data.

gamm_tab_ui <- function() {
  tabItem(tabName = "gamm",
    page_title("Does a smooth model of age (GAMM) tell the same ageing story?"),
    guide_box("gamm",
      goal = "Fit generalized additive mixed models (GAMMs; mgcv): the ageing trajectory is a smooth curve whose shape the data choose, rather than a function you pick. Use them to describe the shape of ageing, to locate where the trait rises and falls, and to check the conclusions of the parametric models of step 5. This step reads the data of step 1 and changes nothing in steps 1\u20136.",
      start_with = "the settings on the Fit tab (the defaults suit most data) and press Fit GAMM. Read the curve, the plain-language summary and any cautions above it.",
      then = "open Ageing rate (where the trait changes significantly with age, the peak and the onset of decline), Selective disappearance (do ALR, lifespan or AFR terms matter, and how far do they move the curve?), Groups (do the sexes or treatments age differently?) and Checks (basis size, concurvity, residuals).",
      tip = "Save results to the summary as you go: each saved GAMM result is described in the methods draft of step 6.",
      further = "compare the smooth with linear, quadratic and cubic age (Checks), fit a random smooth per individual, or add a terminal-record effect."),
    fluidRow(box(width = 12, status = "info", solidHeader = FALSE, uiOutput("gamm_data_note"))),
    tabBox(width = 12, id = "gamm_tabs",
      # ------------------------------------------------------------------ Fit
      tabPanel("Fit", icon = icon("play"),
        fluidRow(
          box(width = 4, title = info_title("GAMM settings", "gamm_settings"), status = "primary", solidHeader = TRUE,
            selectInput("gamm_family", "Error family", GAMM_FAMILIES, "gaussian"),
            uiOutput("gamm_family_hint"),
            numericInput("gamm_k", "Basis size k for age (upper limit on wiggliness)", 10, min = 3, max = 40, step = 1),
            selectInput("gamm_individual", "Differences between individuals", GAMM_INDIVIDUAL, "ri"),
            uiOutput("gamm_individual_note"),
            h5(strong(info_title("Selective disappearance and appearance (how do I choose?)", "gamm_choose"))),
            selectInput("gamm_proxy", "Lifespan proxy", c("Age at last reproduction or record (ALR)" = "ALR", "Known lifespan (LS)" = "LS", "None" = "none"), "ALR"),
            conditionalPanel("input.gamm_proxy != 'none'",
              selectInput("gamm_proxy_form", "\u2026entered as", GAMM_PROXY_FORMS[-1], "linear")),
            uiOutput("gamm_proxy_hint"),
            selectInput("gamm_afr_form", "Age at first reproduction or record (AFR), entered as", GAMM_PROXY_FORMS, "none"),
            checkboxInput("gamm_terminal", "Terminal-record effect (last record before death)", FALSE),
            uiOutput("gamm_group_ui"),
            tags$details(class = "advanced-block",
              tags$summary("Advanced options (defaults are fine for most analyses)"),
              selectInput("gamm_bs", "Smooth basis for age", GAMM_BASES, "tp"),
              selectInput("gamm_engine", "Fitting engine", GAMM_ENGINES, "auto"),
              radioButtons("gamm_method", "Smoothing-parameter estimation", c("REML (recommended)" = "REML", "ML" = "ML"), "REML", inline = TRUE),
              checkboxInput("gamm_select", "Shrinkage selection: allow terms to be removed (select = TRUE)", FALSE),
              checkboxInput("gamm_covars", "Include the covariates mapped on the Data tab", TRUE),
              checkboxInput("gamm_cov_smooth", "Numeric covariates as smooths", FALSE),
              checkboxInput("gamm_random", "Include the random intercepts mapped on the Data tab", TRUE),
              checkboxInput("gamm_ar1", "AR1 errors within individuals (Gaussian traits; uses bam)", FALSE)),
            actionButton("gamm_fit", "Fit GAMM", class = "btn-primary", icon = icon("play")),
            uiOutput("gamm_formula_ui")),
          box(width = 8, title = info_title("What does the fitted ageing trajectory look like?", "gamm_curve"), status = "info", solidHeader = TRUE,
            uiOutput("gamm_reading_ui"),
            uiOutput("gamm_cautions_ui"),
            uiOutput("gamm_next_ui"),
            fluidRow(
              column(4, checkboxInput("gamm_show_means", "Observed means at each age", TRUE)),
              column(4, checkboxInput("gamm_show_points", "Individual records", FALSE)),
              column(4, uiOutput("gamm_truth_ui"))),
            radioButtons("gamm_interval", NULL, c("Simultaneous 95% intervals" = "simultaneous", "Pointwise 95% intervals" = "pointwise"),
                         "simultaneous", inline = TRUE),
            plotOutput("gamm_curve_plot", height = 400),
            plotOutput("gamm_support_plot", height = 120),
            look_for("the overall shape and where the interval is narrow; ignore wiggles where few individuals remain (the bars)."),
            save_button("save_gamm_curve"))
        )
      ),
      # ------------------------------------------------------------------ Ageing rate
      tabPanel("Ageing rate", icon = icon("arrow-trend-down"),
        fluidRow(
          box(width = 12, title = info_title("Where does the trait change with age?", "gamm_deriv"), status = "info", solidHeader = TRUE,
            p(class = "small-note", "Uses the interval type chosen on the Fit tab."),
            fluidRow(
              column(7, plotOutput("gamm_deriv_plot", height = 380)),
              column(5, h5(strong("Periods of significant change")), tableOutput("gamm_periods_table"),
                     h5(strong(info_title("Peak age and onset of the final decline", "peak_onset"))), tableOutput("gamm_peak_table"))),
            look_for("runs of red (significant decline) and blue (significant increase) points; the onset of senescence is where the final decline begins."),
            save_button("save_gamm_deriv"))
        )
      ),
      # ------------------------------------------------------------------ Selective disappearance
      tabPanel("Selective disappearance", icon = icon("user-minus"),
        fluidRow(
          box(width = 5, title = info_title("Does selective disappearance or appearance shape the curve?", "gamm_proxy"), status = "info", solidHeader = TRUE,
            h5(strong("Proxy terms in the fitted model")), tableOutput("gamm_proxy_terms_table"),
            actionButton("gamm_run_proxy", "Compare proxy forms (ML, same records)", class = "btn-default", icon = icon("scale-balanced")),
            uiOutput("gamm_proxy_verdict"),
            uiOutput("gamm_proxy_tables"),
            save_button("save_gamm_proxy")),
          box(width = 7, title = "How far do the proxy terms move the curve?", status = "info", solidHeader = TRUE,
            p(class = "small-note", "The population curve without any ALR, lifespan or AFR term (what a naive analysis shows) and the fitted model's curve, on the same records. A gap means that which individuals remain at each age shapes the naive curve. With a random intercept for each individual and many records per individual, a linear proxy changes the curve little, because the individual intercepts already absorb differences in level; an age-dependent proxy (or random slopes) is what removes a distortion of shape."),
            plotOutput("gamm_naive_plot", height = 380),
            look_for("whether the two curves differ in shape (age-dependent selective disappearance) or only in level (age-independent)."))
        )
      ),
      # ------------------------------------------------------------------ Groups
      tabPanel("Groups", icon = icon("people-group"),
        fluidRow(
          box(width = 12, title = info_title("Do groups age differently?", "gamm_groups"), status = "info", solidHeader = TRUE,
            uiOutput("gamm_groups_note"),
            actionButton("gamm_run_groups", "Compare group structures (ML, same records)", class = "btn-default", icon = icon("layer-group")),
            uiOutput("gamm_groups_verdict"),
            fluidRow(
              column(6, tableOutput("gamm_groups_table"),
                     h5(strong("Do the shapes differ from the reference group?")), tableOutput("gamm_diff_test_table"),
                     tableOutput("gamm_lrt_table")),
              column(6, plotOutput("gamm_diff_plot", height = 360))),
            save_button("save_gamm_groups"))
        )
      ),
      # ------------------------------------------------------------------ Checks
      tabPanel("Checks", icon = icon("stethoscope"),
        fluidRow(
          box(width = 6, title = info_title("Is the smooth well specified?", "gamm_check"), status = "info", solidHeader = TRUE,
            uiOutput("gamm_fit_stats"),
            h5(strong("Smooth terms")), tableOutput("gamm_smooth_table"),
            h5(strong("Parametric terms")), tableOutput("gamm_param_table"),
            h5(strong("Basis dimension (k) check")), tableOutput("gamm_kcheck_table"),
            h5(strong("Concurvity between fixed terms")), tableOutput("gamm_concurvity_table"),
            save_button("save_gamm_check")),
          box(width = 6, title = info_title("Do the residuals look right?", "gamm_resid"), status = "info", solidHeader = TRUE,
            uiOutput("gamm_resid_note"),
            plotOutput("gamm_resid_plot", height = 520))
        ),
        fluidRow(
          box(width = 12, title = info_title("Is a smooth better than a polynomial?", "gamm_poly"), status = "info", solidHeader = TRUE,
            actionButton("gamm_run_poly", "Compare with linear, quadratic and cubic age", class = "btn-default"),
            tableOutput("gamm_poly_table"),
            save_button("save_gamm_poly"))
        )
      ),
      # ------------------------------------------------------------------ Individuals
      tabPanel("Individuals", icon = icon("user"),
        fluidRow(
          box(width = 12, title = info_title("How much do individuals differ?", "gamm_indiv"), status = "info", solidHeader = TRUE,
            fluidRow(column(4, tableOutput("gamm_vcomp_table")), column(8, plotOutput("gamm_indiv_plot", height = 380))))
        )
      ),
      # ------------------------------------------------------------------ Code
      tabPanel("R code", icon = icon("code"),
        fluidRow(
          box(width = 12, title = info_title("Reproduce this GAMM in R", "gamm_code"), status = "info", solidHeader = TRUE,
            div(class = "code-toolbar",
                tags$button(type = "button", class = "btn btn-default btn-sm", onclick = "disapprCopy('gamm_code', this)", icon("copy"), " Copy"),
                downloadButton("download_gamm_code", "Download .R", class = "btn-default btn-sm"),
                downloadButton("download_gamm_data", "Download the model's data (.csv)", class = "btn-default btn-sm")),
            div(class = "section-code", verbatimTextOutput("gamm_code")))
        )
      )
    )
  )
}
