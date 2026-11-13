# Validation 1: step 4. Does the mean-of-coefficients reconstruction from individual fits recover the simulated
# average within-individual trajectory? Continuous traits are fitted on the raw scale, counts on the log scale
# (0.19.0), with and without age-dependent selective disappearance and missing records.
# Expected (from the independent Python check): continuous traits within about 1-2%; Poisson counts within about
# 2-9% for linear and quadratic forms; cubic counts noisy unless well-sampled individuals only are fitted.
source(file.path("inst", "validation", "v0_20", "setup.R"))
scen <- expand.grid(trait = c("mass", "count_pois"), form = c("Linear", "Quadratic", "Cubic"),
                    sd_type = c("none", "dependent"), missingness = c("complete", "mcar", "mwo"),
                    seed = seq_len(N_SEEDS), stringsAsFactors = FALSE)
rows <- lapply(seq_len(nrow(scen)), function(i) {
  s <- scen[i, ]
  b <- simulate_prepared(trait = s$trait, form = s$form, sd_type = s$sd_type, missingness = s$missingness, seed = s$seed)
  link <- if (suggest_family(b$data$trait)$family %in% COUNT_FAMILIES) "log" else "identity"
  z <- fit_individual_function(b$data, s$form, 1, link = link)
  mc <- z$mean_curve
  tru <- toy_true_curve(b$truth, mc$age)
  ok <- is.finite(tru) & abs(tru) > 0.05 * max(abs(tru), na.rm = TRUE)
  err <- abs(mc$fitted[ok] / tru[ok] - 1)
  data.frame(s, link = link, fitted = length(z$fitted_ids), individuals = z$n_total,
             mean_abs_error_pct = 100 * mean(err), max_abs_error_pct = 100 * max(err))
})
res <- do.call(rbind, rows)
write_result(res, "step4_reconstruction")
print(stats::aggregate(mean_abs_error_pct ~ trait + form + sd_type + missingness, res, mean))
