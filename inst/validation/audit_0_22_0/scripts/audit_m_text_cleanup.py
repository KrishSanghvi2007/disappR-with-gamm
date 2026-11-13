"""Part M of the audit (0.24.7): the interface clean-up. Every requested removal is gone, every revised text is in
place, the example cards have their short summary and folded details, and the new stored-model plot is wired."""
import os, re, sys
sys.path.insert(0, os.path.dirname(__file__))
import port022 as P
ROOT = P.ROOT
fails = []
def check(label, ok, detail=""):
    print(("PASS " if ok else "FAIL ") + label + (": " + detail if detail else ""))
    if not ok: fails.append(label)
src = lambda f: open(os.path.join(ROOT, f), encoding="utf-8").read()
ui, server, helpers, imp = src("inst/app/ui.R"), src("inst/app/server.R"), src("R/ui-helpers.R"), src("R/import.R")
allsrc = ui + server + helpers + src("R/model-fit.R") + src("R/model-comparison.R")
gone = {
  "pre-filled mapping note": "Column mapping, covariates, nesting, random effects, the error family and the model selection are pre-filled",
  "binomial weights note": "Binomial weights (the number of trials behind each proportion)",
  "AFR 'enters the dataset' sentence": "AFR is the age when the individual enters the dataset.",
  "bins sentence on page 2": 'box_note("These settings apply to all figures below.',
  "frailty clause of the hazard test": "individual frailty not estimable, so occasions treated as independent",
  "'changes the missingness figures on this tab only' note": "This setting changes the missingness figures on this tab only",
  "proxy-choice card": '"Proxy choice (ALR or mean age)"',
  "'Missing cells' line": 'strong("Missing cells: ")',
  "data-support box on page 4": 'output$a3_support',
  "example ageing-function sentence": "Set by the example to the ageing function of the published model",
  "pre-fit random-effects warning": "These settings often end in singular fits",
  "'rule of thumb' caution": "Caution (rule of thumb, not a threshold)",
  "suggested starting analysis": "suggested_settings",
  "Apply these settings button": '"apply_suggested"',
  "outlined AFR = ALR tile": "linewidth = 0.7,\n",
  "AIC values in example notes": "AICc weight",
}
for k, v in gone.items():
    check("removed: " + k, v not in allsrc and v not in imp)
present = {
  "exclusion label": "Exclude individuals who have more than one ALR, lifespan or AFR value.",
  "rounding help (records not averaged)": "Records that round to the same age for one individual are not averaged",
  "trait-scale wording": "Suggested visualisation is on the %s scale, which can be changed in the 'Trait scale' option.",
  "terminal-change wording": "a rise at the last occasion may indicate terminal investment",
  "suggested-function wording": '"Suggested function: "',
  "function-comparison title": "How do different functions fit the population-level data?",
  "fitted-curve help without the decomposition": "b2_curves = info_entry(\"Fitted population trajectories\"",
  "'Further' step in the modelling guide": 'further = "permute the data to test whether the lifespan effect reflects a real association (Advanced tab)."',
  "formulas of every supported model": "Fitted formulas of the supported models:",
  "random-term wording": "Try refitting without this term.",
  "misspecified-shape wording": "can make interaction terms look supported without selective (dis)appearance",
  "random-effects covariance title": "Do individuals' model-predicted random effects covary with lifespan, ALR or AFR?",
  "invalid-Hessian wording": "are excluded from the ranking and interpretation.",
  "short caution wording": "Random slopes, if fitted, will be imprecise here.",
  "standardising wording": "When they are uncorrelated, centring decides the age at which they are assumed uncorrelated",
  "variance question label": "Is the variance in the data changing across age?",
  "AFR = ALR legend entry": '"AFR = ALR (one occasion)" = "#7B4F9E"',
  "stored-model trajectory plot": 'output$store_pred_plot <- renderPlot' ,
  "set comparison wording": "its AIC cannot be compared with set",
}
for k, v in present.items():
    check("in place: " + k, v in allsrc)
n_about = len(re.findall(r"\n    about = list\(species = ", imp))
check("every bundled example has a short summary (species, trait, default model, covariates, main result, other traits)", n_about == 13, "%d of 13" % n_about)
check("the example card shows the summary and folds the full note under 'More details'",
      'tags$summary(style = "cursor: pointer;", "More details")' in server and 'item("Default model", a$model)' in server)
check("the new plot and its choices are placed in the interface and rendered by the app test",
      'plotOutput("store_pred_plot"' in ui and 'uiOutput("store_plot_pick_ui")' in ui and '"store_pred_plot"' in src("tests/scripts/app_test.R"))
mf = src("R/model-fit.R")
check("0.24.8: well-supported random slopes are the default for simulated and uploaded data; examples keep their published structure",
      "default_random_structure <- function(dat)" in mf and 'if (!identical(adv$level, "good")) return("none")' in mf
      and "else default_random_structure(safe_get(dat()))" in server and "normalise_slope(ex$random_structure" in server
      and "Default for these data: " in server and "default_random_structure(rich)" in src("tests/testthat/test-0-24-8.R"))
print("\n%d failure(s)%s" % (len(fails), (": " + "; ".join(fails)) if fails else ""))
