# Step 8 - Bayesian models (optional). The tab's interface, built by bayes_tab_ui() from ui.R. Every input and output id
# starts bayes_ (save buttons save_bayes_, help keys info_bayes_). The step refits one model of step 5 with brms; it
# reads the step-5 fit and changes nothing in steps 1-7.

bayes_tab_ui <- function() {
  tabItem(tabName = "bayes",
    page_title("Does a Bayesian fit of the same model tell the same story?"),
    guide_box("bayes",
      goal = "Refit one of the step-5 models (Models 1–10) in a Bayesian framework with brms and Stan, on the same records and with the same formula. The step walks through the Bayesian workflow: choose priors, see what they imply, fit, check that the sampler worked, then read the posterior and check the model against the data. It changes nothing in steps 1–7.",
      start_with = "fitting the models on the Modelling tab (step 5). Then, on Set up, check your installation and choose the model (the best-ranked one is preselected).",
      then = "look at the ageing curves your priors allow (Priors), press Fit on 'Fit and convergence' and read the traffic lights, then compare the posterior with step 5 (Results) and check the model against the data (Posterior checks).",
      tip = "Start with a draft run (about a minute after the first compilation): it is for exploring. Use the full run before saving results for a paper.",
      further = "refit with a narrow and a wide prior: if the conclusions do not change, the data, not the priors, are speaking."),
    fluidRow(box(width = 12, status = "info", solidHeader = FALSE, uiOutput("bayes_status_note"))),
    tabBox(width = 12, id = "bayes_tabs",
      # ------------------------------------------------------------------ Set up
      tabPanel("Set up", icon = icon("screwdriver-wrench"),
        fluidRow(
          box(width = 5, title = info_title("Is brms ready on this computer?", "bayes_setup"), status = "primary", solidHeader = TRUE,
            uiOutput("bayes_env_ui"),
            actionButton("bayes_check_env", "Check my set-up (packages and C++ compiler)", class = "btn-default", icon = icon("stethoscope")),
            tableOutput("bayes_env_table")),
          box(width = 7, title = info_title("Which model is refitted?", "bayes_model"), status = "primary", solidHeader = TRUE,
            uiOutput("bayes_model_ui"),
            uiOutput("bayes_spec_ui"))
        )
      ),
      # ------------------------------------------------------------------ Priors
      tabPanel("Priors", icon = icon("sliders"),
        fluidRow(
          box(width = 5, title = info_title("What do I believe before seeing the data?", "bayes_priors"), status = "primary", solidHeader = TRUE,
            radioButtons("bayes_width", "Width of the priors on the fixed effects", BAYES_WIDTHS, "default"),
            uiOutput("bayes_prior_meaning"),
            h5(strong("The priors used")),
            tableOutput("bayes_prior_table")),
          box(width = 7, title = info_title("Which ageing curves do these priors allow? (prior predictive check)", "bayes_prior_check"), status = "info", solidHeader = TRUE,
            uiOutput("bayes_prior_reading"),
            plotOutput("bayes_prior_plot", height = 420),
            look_for("whether the grey curves span the plausible range without piling up at impossible values. The orange band marks the observed values; the curves need not stay inside it, but most should not be absurd."))
        )
      ),
      # ------------------------------------------------------------------ Fit and convergence
      tabPanel("Fit and convergence", icon = icon("gears"),
        fluidRow(
          box(width = 4, title = info_title("How long should the sampler run?", "bayes_fit"), status = "primary", solidHeader = TRUE,
            radioButtons("bayes_mode", "Run", c("Draft: 2 chains × 1,000 iterations (exploring)" = "draft",
                                                "Full: 4 chains × 2,000 iterations (reporting)" = "full"), "draft"),
            tags$details(class = "advanced-block",
              tags$summary("Advanced sampler settings (defaults are fine for most analyses)"),
              numericInput("bayes_iter", "Iterations per chain (blank: from the run choice)", NA, min = 200, step = 500),
              numericInput("bayes_adapt_delta", "adapt_delta (raise towards 0.99 if there are divergent transitions)", 0.95, min = 0.8, max = 0.999, step = 0.01),
              numericInput("bayes_treedepth", "Maximum tree depth", 10, min = 5, max = 20, step = 1),
              numericInput("bayes_seed", "Random seed", 2024, min = 1, step = 1)),
            actionButton("bayes_fit", "Fit with brms", class = "btn-primary", icon = icon("play")),
            uiOutput("bayes_fit_note")),
          box(width = 8, title = info_title("Did the sampler work? (convergence checks)", "bayes_diag"), status = "info", solidHeader = TRUE,
            uiOutput("bayes_overall_ui"),
            tableOutput("bayes_diag_table"),
            save_button("save_bayes_diag"))
        ),
        fluidRow(
          box(width = 12, title = info_title("Do the chains mix? (trace plots)", "bayes_trace"), status = "info", solidHeader = TRUE,
            plotOutput("bayes_trace_plot", height = 420),
            look_for("overlapping, fuzzy 'caterpillars' with no trends and no chain sitting apart from the others."))
        )
      ),
      # ------------------------------------------------------------------ Results
      tabPanel("Results", icon = icon("chart-area"),
        fluidRow(
          box(width = 12, title = info_title("What does the posterior say, and does it agree with step 5?", "bayes_results"), status = "info", solidHeader = TRUE,
            uiOutput("bayes_reading_ui"),
            h5(strong("Fixed effects: Bayesian refit beside the step-5 fit")),
            tableOutput("bayes_compare_table"),
            fluidRow(
              column(6, h5(strong("Random-effect and residual SDs")), tableOutput("bayes_var_table")),
              column(6, h5(strong(info_title("Peak age and onset of the final decline", "bayes_peak"))), tableOutput("bayes_peak_table"))),
            save_button("save_bayes_results"))
        ),
        fluidRow(
          box(width = 12, title = info_title("The ageing curve with its credible band", "bayes_curve"), status = "info", solidHeader = TRUE,
            checkboxInput("bayes_show_freq", "Show the step-5 curve", TRUE),
            plotOutput("bayes_curve_plot", height = 400),
            look_for("whether the Bayesian curve and its 95% credible band agree with the step-5 curve; differences point to the priors or to convergence."),
            save_button("save_bayes_curve"))
        )
      ),
      # ------------------------------------------------------------------ Posterior predictive checks
      tabPanel("Posterior checks", icon = icon("magnifying-glass-chart"),
        fluidRow(
          box(width = 12, title = info_title("Could the model have produced these data? (posterior predictive checks)", "bayes_ppc"), status = "info", solidHeader = TRUE,
            uiOutput("bayes_ppc_reading"),
            fluidRow(
              column(6, plotOutput("bayes_ppc_dist_plot", height = 360)),
              column(6, plotOutput("bayes_ppc_age_plot", height = 360))),
            h5(strong("Summary statistics of the data against the replicated data")),
            tableOutput("bayes_ppc_stats"),
            look_for("the dark observed values sitting among the light replicates; systematic misses at some ages or in the spread point to a missing term or the wrong family."),
            save_button("save_bayes_ppc"))
        )
      ),
      # ------------------------------------------------------------------ Code
      tabPanel("R code", icon = icon("code"),
        fluidRow(
          box(width = 12, title = info_title("Reproduce this Bayesian fit in R", "bayes_code"), status = "info", solidHeader = TRUE,
            div(class = "code-toolbar",
                tags$button(type = "button", class = "btn btn-default btn-sm", onclick = "disapprCopy('bayes_code', this)", icon("copy"), " Copy"),
                downloadButton("download_bayes_code", "Download .R", class = "btn-default btn-sm"),
                downloadButton("download_bayes_data", "Download the model's data (.csv)", class = "btn-default btn-sm")),
            div(class = "section-code", verbatimTextOutput("bayes_code")),
            h5(strong("Methods paragraph (BARG checklist; Kruschke 2021)")),
            uiOutput("bayes_methods_ui"))
        )
      )
    )
  )
}
