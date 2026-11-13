# Validation 2: the evidence summary. Every line of evidence is produced by the app's own functions, as a user who
# saved each result would produce it, and grade_evidence() is applied. Known answers:
#   no selection                    -> "consistent with none"
#   age-independent selection       -> strong, age-independent
#   age-dependent selection         -> strong, age-dependent
#   no selection, trait-dependent missing records -> not strong, observation bias flagged
source(file.path("inst", "validation", "v0_20", "setup.R"))
scen <- expand.grid(case = c("none", "independent", "dependent", "trait_missing"), seed = seq_len(N_SEEDS),
                    stringsAsFactors = FALSE)
expected <- c(none = "none", independent = "strong / age_independent", dependent = "strong / age_dependent",
              trait_missing = "not strong, observation bias")
grade_case <- function(case, seed) {
  sd <- switch(case, independent = "independent", dependent = "dependent", "none")
  miss <- if (case == "trait_missing") "trait" else "complete"
  b <- simulate_prepared(form = "Quadratic", sd_type = sd, missingness = miss, seed = seed, n_id = 200)
  e <- list()
  add <- function(ev) if (!is.null(ev)) e[[length(e) + 1]] <<- list(evidence = ev)
  add(classify_visual_coef(b$data))
  fit <- function(rs) fit_model_suite(b$data, b$meta, models = c("M1", "M2", "M4"), age_function = "Quadratic", random_slope = rs)
  r0 <- fit("none")
  best <- sub("^Model ", "M", r0$aic$Model[is.finite(r0$aic$Delta_AIC)][[1]])
  fc <- compare_population_functions(b$data, b$meta, model = best)
  t <- fc$table[is.finite(fc$table$AIC), , drop = FALSE]
  shape_ok <- "Quadratic" %in% t$Function[t$Delta_AIC < 2]
  add(models_evidence(r0, "none", shape_ok = shape_ok))
  add(models_evidence(fit("uncorrelated"), "none", shape_ok = shape_ok))
  mv <- e[[length(e)]]$evidence
  tested <- if (identical(mv$kind, "age_independent")) "M2" else "M4"
  z <- tryCatch(bootstrap_test(b$data, b$meta, model = tested, n_boot = 19, settings = list(age_function = "Quadratic")),
                error = function(err) NULL)
  rd <- bootstrap_reading(z)
  if (!is.null(rd)) add(list(type = "permutation", pattern = rd$pattern, model = tested))
  g <- build_missing_grid(b$data)
  ms <- missingness_summary(b$data, g, 0.05, TRUE)
  dr <- ms$drivers
  tp <- if (is.data.frame(dr) && "Prior observed trait" %in% dr$Predictor) dr$P_value[dr$Predictor == "Prior observed trait"][[1]] else NA_real_
  add(list(type = "missingness", trait_p = tp, percent = ms$percent))
  gr <- grade_evidence(e)
  kind <- if (!is.null(mv)) mv$kind else NA_character_
  data.frame(case = case, seed = seed, expected = expected[[case]], level = gr$level, kind = kind,
             bootstrap = if (is.null(rd)) NA_character_ else rd$pattern,
             observation_bias = any(grepl("Observation bias", gr$lines$Evidence) & gr$lines$Status != "supports"))
}
res <- do.call(rbind, lapply(seq_len(nrow(scen)), function(i) grade_case(scen$case[[i]], scen$seed[[i]])))
write_result(res, "grader_calibration")
print(res)
