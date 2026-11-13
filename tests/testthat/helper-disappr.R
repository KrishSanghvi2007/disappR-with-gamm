# Small simulated data sets shared by the unit tests.
toy_prepared <- function(n_id = 80, seed = 1, ...) {
  raw <- simulate_toy_data(c(list(n_id = n_id, seed = seed), list(...)))
  list(raw = raw, prep = standardise_data(raw, toy_mapping(raw)))
}
