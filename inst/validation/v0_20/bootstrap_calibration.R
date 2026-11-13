# Validation 3: the null-model bootstrap. False positives where there is no selective disappearance - including the
# cases that defeated the permutation test (a misspecified ageing function, individuals ageing at different rates) -
# and power where there is. The permutation test was significant in 3 of 3 misspecified-function datasets without
# selection (0.17.0 checks); the bootstrap should be close to the nominal 5% in every null case.
source(file.path("inst", "validation", "v0_20", "setup.R"))
seeds <- seq_len(max(N_SEEDS, 10))
scen <- rbind(
  data.frame(case = "null, correct function",        sd_type = "none",        form = "Quadratic", fitted = "Quadratic", rate_var = "low",  model = "M4"),
  data.frame(case = "null, misspecified function",   sd_type = "none",        form = "Quadratic", fitted = "Linear",    rate_var = "low",  model = "M4"),
  data.frame(case = "null, individuals age at different rates", sd_type = "none", form = "Quadratic", fitted = "Quadratic", rate_var = "high", model = "M4"),
  data.frame(case = "age-dependent selection",       sd_type = "dependent",   form = "Quadratic", fitted = "Quadratic", rate_var = "low",  model = "M4"),
  data.frame(case = "age-independent selection",     sd_type = "independent", form = "Quadratic", fitted = "Quadratic", rate_var = "low",  model = "M2"))
rows <- list()
for (i in seq_len(nrow(scen))) for (sd in seeds) {
  s <- scen[i, ]
  b <- simulate_prepared(form = s$form, sd_type = s$sd_type, rate_var = s$rate_var, seed = sd, n_id = 200)
  z <- tryCatch(bootstrap_test(b$data, b$meta, model = s$model, n_boot = 19, settings = list(age_function = s$fitted)),
                error = function(e) list(error = conditionMessage(e)))
  rows[[length(rows) + 1]] <- data.frame(s, seed = sd, p = if (is.null(z$error)) z$p_gain else NA_real_,
                                         significant = if (is.null(z$error)) isTRUE(z$p_gain < 0.05) else NA,
                                         error = if (is.null(z$error)) "" else z$error)
}
res <- do.call(rbind, rows)
write_result(res, "bootstrap_calibration")
print(stats::aggregate(significant ~ case, res, function(x) mean(x, na.rm = TRUE)))
