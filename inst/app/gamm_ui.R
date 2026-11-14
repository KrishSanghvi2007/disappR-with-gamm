# Step 7 - GAMMs (optional). The tab's interface, built by gamm_tab_ui() from ui.R. Every input and output id starts
# gamm_ (save buttons save_gamm_, help keys info_gamm_), so the tab shares nothing with steps 1-6 except the data.

gamm_tab_ui <- function() {
  tabItem(tabName = "gamm",
    page_title("Does a smooth model of age (GAMM) tell the same ageing story?"),
    guide_box("gamm",
      goal = "Fit generalized additive mixed models (GAMMs; mgcv): the ageing trajectory is a smooth curve whose shape the data choose, rather than a function you pick. Use them to describe the shape of ageing, to locate where the trait rises and falls, and to check the conclusions of the parametric models of step 5. This step reads the data of step 1 and changes nothing in steps 1\u20136.",
      start_with = "the settings on the Fit tab (the defaults suit most data) and press Fit GAMM. Read the curve, the plain-language summary and any cautions above it.",
      then = "read the estimates and p-values of every term on Model output, then open Ageing rate (where the trait changes significantly with age, the peak and the onset of decline), Selective disappearance (do ALR, lifespan or AFR terms matter, and how far do they move the curve?), Groups (do the sexes or treatments age differently?) and Checks (basis size, concurvity, residuals).",
      tip = "Save results to the summary as you go: each saved GAMM result is described in the methods draft of step 6.",
      further = "ask whether a smooth is needed at all (no age, linear, quadratic or cubic age against the smooth, on Model output), fit a random smooth per individual, or add a terminal-record effect."),
    fluidRow(box(width = 12, status = "info", solidHeader = FALSE, uiOutput("gamm_data_note"))),
    tabBox(width = 12, id = "gamm_tabs",
      # ------------------------------------------------------------------ Fit
      tabPanel("Fit", icon = icon("play"),
        fluidRow(
          box(width = 4, title = info_title("GAMM settings", "gamm_settings"), status = "primary", solidHeader = TRUE,
            selectInput("gamm_family", "Error family", GAMM_FAMILIES, "gaussian"),
            uiOutput("gamm_family_hint"),
            conditionalPanel("input.gamm_family == 'poisson' || input.gamm_family == 'binomial'",
              checkboxInput("gamm_olre", "Observation-level random effect (for overdispersed counts or proportions)", FALSE)),
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
              selectInput("gamm_engine", "Fitting engine", GAMM_ENGINES, "auto"),
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
              column(4, selectInput("gamm_show_data", "Show the data as", c("Observed means at each age" = "means", "Observed means in age bins" = "bins",
                                                                          "Individual records" = "points", "Nothing" = "none"), "means")),
              column(3, conditionalPanel("input.gamm_show_data == 'bins'", numericInput("gamm_age_bins", "Number of age bins", 8, min = 3, max = 25, step = 1))),
              column(3, uiOutput("gamm_n_proxy_ui")),
              column(2, uiOutput("gamm_truth_ui"))),
            uiOutput("gamm_scale_ui"),
            plotOutput("gamm_curve_plot", height = 400),
            plotOutput("gamm_support_plot", height = 120),
            look_for("the overall shape and where the interval is narrow; ignore wiggles where few individuals remain (the bars)."),
            save_button("save_gamm_curve"))
        )
      ),
      # ------------------------------------------------------------------ Model output
      tabPanel("Model output", icon = icon("table-list"),
        fluidRow(
          box(width = 12, title = info_title("Reading this GAMM, step by step", "gamm_guide"), status = "primary", solidHeader = TRUE,
            p(class = "small-note", "The questions to ask of a GAMM, in order, answered for this fit. Each points to the table, comparison or tab that answers it in full."),
            uiOutput("gamm_guide_ui"))
        ),
        fluidRow(
          box(width = 12, title = info_title("What does each term of the GAMM estimate?", "gamm_output"), status = "primary", solidHeader = TRUE,
            uiOutput("gamm_output_reading"),
            fluidRow(
              column(4, h5(strong("Model statistics")), tableOutput("gamm_out_stats")),
              column(8,
                h5(strong("Smooth terms (fixed)")), tableOutput("gamm_out_smooth"),
                h5(strong("Parametric terms (fixed)")), tableOutput("gamm_out_param"))),
            p(class = "small-note", uiOutput("gamm_out_formula", inline = TRUE)),
            save_button("save_gamm_output"))
        ),
        fluidRow(
          box(width = 12, title = info_title("Is a smooth needed? No age, straight line, polynomial or smooth", "gamm_poly"), status = "primary", solidHeader = TRUE,
            p(class = "small-note", "Refits the model with no age term, with linear, quadratic and cubic age (the functions of step 5) and with the smooth, all by maximum likelihood on the same records, and compares them by AIC. With many individuals the fits use gamm4 and take seconds to minutes; the progress bar shows the elapsed time."),
            actionButton("gamm_run_poly", "Compare: no age, linear, quadratic, cubic and smooth", class = "btn-default", icon = icon("scale-balanced")),
            uiOutput("gamm_engine_note_poly"),
            uiOutput("gamm_poly_reading"),
            tableOutput("gamm_poly_table"),
            save_button("save_gamm_poly"))
        ),
        fluidRow(
          box(width = 12, title = info_title("Random effects", "gamm_random"), status = "primary", solidHeader = TRUE,
            tableOutput("gamm_out_random"),
            actionButton("gamm_run_re_test", "Test the random effects (mgcv; can take a while)", class = "btn-default", icon = icon("vial")),
            uiOutput("gamm_re_test_note"),
            tableOutput("gamm_re_test_table"))
        )
      ),
      # ------------------------------------------------------------------ Ageing rate
      tabPanel("Ageing rate", icon = icon("arrow-trend-down"),
        fluidRow(
          box(width = 12, title = info_title("Where does the trait change with age?", "gamm_deriv"), status = "info", solidHeader = TRUE,
            p(class = "small-note", "Simultaneous 95% intervals: they hold for the whole curve at once, so a period where the interval excludes zero is a reliable period of change."),
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
            uiOutput("gamm_engine_note_proxy"),
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
            tags$details(class = "info-more", open = NA, tags$summary("Which group structure? G, GS, GI, S and I explained"),
              div(class = "info-more-body", tags$table(class = "table table-condensed small-note",
                tags$thead(tags$tr(tags$th("Structure"), tags$th("What it assumes"), tags$th("In mgcv"), tags$th("Choose it when"))),
                tags$tbody(
                  tags$tr(tags$td(strong("G")), tags$td("One ageing curve for all groups; groups differ only in overall level."), tags$td(code("group + s(age)")), tags$td("The groups age the same way (the parametric analogue: group as an additive term).")),
                  tags$tr(tags$td(strong("GS")), tags$td("A common curve plus a deviation for each group; all deviations share one wiggliness."), tags$td(code('s(age) + s(age, group, bs = "sz")')), tags$td("Groups share a basic pattern and differ in similar, modest ways. The deviations are shrunk towards the common curve, which helps with small groups.")),
                  tags$tr(tags$td(strong("GI")), tags$td("A common curve plus a deviation for each group, each deviation with its own wiggliness."), tags$td(code("s(age) + s(age, by = group, m = 1)")), tags$td("Groups share a pattern, but one group departs from it in a more complex way than another.")),
                  tags$tr(tags$td(strong("S")), tags$td("A separate curve per group, no common curve, all with the same wiggliness."), tags$td(code("s(age, by = group, id = 1)")), tags$td("No shared pattern is expected, but the curves should be similarly smooth.")),
                  tags$tr(tags$td(strong("I")), tags$td("Fully separate curves, each with its own wiggliness."), tags$td(code("s(age, by = group)")), tags$td("Groups may age in completely different ways (the parametric analogue: age \u00d7 group with every term interacting)."))
                )),
                p(class = "small-note", "How to decide: compare all five (button below). If G is within 2 AIC of the best, there is no clear evidence that the groups age differently: report one curve. Otherwise the best structure says how they differ: GS or GI, around a shared pattern; S or I, independently. Within 2 AIC, prefer the simpler structure (G, then GS, S, GI, I). In every structure the group main effect is included, so the curves can differ in level as well.")
              )),
            actionButton("gamm_run_groups", "Compare group structures (ML, same records)", class = "btn-default", icon = icon("layer-group")),
            uiOutput("gamm_engine_note_groups"),
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
          box(width = 12, title = info_title("Is the model adequate?", "gamm_check"), status = "info", solidHeader = TRUE,
            uiOutput("gamm_check_overall"),
            tableOutput("gamm_check_summary"),
            tags$details(class = "info-more", tags$summary("Details: basis dimension (k) check"),
              div(class = "info-more-body",
                p(class = "small-note", "For each smooth: k' (the largest edf allowed), the edf used, the k-index (below 1 suggests pattern left in the residuals along that term) and its p-value. 'k may be too low' needs all three: a small p-value, a k-index below 1 and an edf close to k'."),
                tableOutput("gamm_kcheck_table"))),
            tags$details(class = "info-more", tags$summary("Details: concurvity between fixed terms"),
              div(class = "info-more-body",
                p(class = "small-note", "For each smooth, how far it can be reproduced by the most similar other fixed term (0 = not at all, 1 = completely). 'Estimate' is the typical value, 'Worst' the worst case. Above 0.8 the two terms' separate effects are poorly identified."),
                tableOutput("gamm_concurvity_table"))),
            p(class = "small-note", "The estimates, test statistics and p-values of every term are on the Model output tab."),
            save_button("save_gamm_check"))
        ),
        fluidRow(
          box(width = 12, title = info_title("Do the residuals look right?", "gamm_resid"), status = "info", solidHeader = TRUE,
            uiOutput("gamm_resid_note"),
            plotOutput("gamm_resid_plot", height = 520))
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
