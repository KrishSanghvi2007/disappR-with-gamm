# disappR engine - Public R interface: the same engine functions the Shiny app calls, for scripted analyses.
#
#   x   <- disappr_prepare(my_data, disappr_mapping(id = "ID", age = "age", trait = "mass", life = "lifespan"))
#   fit <- disappr_fit(x, models = c("M1", "M2", "M4"), age_function = "Quadratic", family = "gaussian")
#   disappr_compare(fit); disappr_diagnose(fit); disappr_interpret(fit)
#   disappr_predict(fit, "M4")
#   cat(disappr_code(fit, x, "my_data.csv"))

#' Scripted disappR analyses
#'
#' The functions the Shiny app uses, available for analyses in R scripts: build a column mapping, prepare the data,
#' fit and compare Models 1-10, predict population trajectories, and read the diagnostics, the interpretation notes
#' and the reproducible code.
#'
#' @param ... For `disappr_mapping()`: column roles (`id`, `age`, `trait`, `alr`, `life`, `entry`, `covars`,
#'   `cov_factor`, `cov_age`, `group`, `group2`, `random`, ...) and `trait_specific_ages` (default `TRUE`: automatic
#'   ALR and AFR are the last and first ages with a trait value; `FALSE`: of any record). For `disappr_fit()`: further settings passed to the
#'   model suite (`random_slope`, `standardise`, `zi_str`, `among`, `include_invalid`, `extra`). For
#'   `disappr_simulate()`: simulation settings (`n_id`, `seed`, `sd_type`, `missingness`, `form`, ...).
#' @param data A data frame with one row per record.
#' @param mapping A column mapping from `disappr_mapping()`.
#' @param dup_action What to do with repeated individual-by-age records: `"keep"` (default) keeps every row; `"mean"` collapses
#'   rows identical in every mapped column into one (nothing is averaged; the name is kept for compatibility).
#' @param x Prepared data from `disappr_prepare()`.
#' @param models Model identifiers, `"M1"` to `"M10"`.
#' @param age_function Ageing function: `"Linear"`, `"Quadratic"`, `"Cubic"`, `"Logarithmic"` or
#'   `"Asymptotic exponential"`.
#' @param family Error family, e.g. `"gaussian"`, `"poisson"`, `"nbinom2"`, `"binomial"`.
#' @param fit A fitted comparison from `disappr_fit()`.
#' @param model One model identifier, e.g. `"M4"`.
#' @param ages Ages at which to predict (default: a fine grid over the observed ages).
#' @param by Optional categorical covariate whose levels get separate predictions.
#' @param source_label File name the exported script reads.
#' @return `disappr_mapping()`: a mapping list. `disappr_prepare()`: a list with `data` and `meta`.
#'   `disappr_fit()`: the model comparison (AIC table, coefficients, fits, validity). `disappr_compare()`: the AIC
#'   table. `disappr_predict()`: predicted population trajectory. `disappr_diagnose()`: validity, status, dropped
#'   rows and notes. `disappr_interpret()`: interpretation notes. `disappr_code()`: an R script as a string.
#'   `disappr_simulate()`: a simulated data set.
#' @name disappr_api
#' @examples
#' \dontrun{
#' sim <- disappr_simulate(n_id = 150, seed = 1, sd_type = "both")
#' map <- disappr_mapping(id = "ID", age = "age", trait = "body_mass", life = "lifespan")
#' x <- disappr_prepare(sim, map)
#' fit <- disappr_fit(x, models = c("M1", "M2", "M4"))
#' disappr_compare(fit)
#' }
NULL

#' @rdname disappr_api
#' @export
disappr_mapping <- function(...) utils::modifyList(example_map(trait_specific_ages = TRUE), list(...))   # trait-specific ALR/AFR by default (0.24.2)

#' @rdname disappr_api
#' @export
disappr_prepare <- function(data, mapping, dup_action = "keep") {
  if (!is.data.frame(data)) stop("'data' must be a data frame.", call. = FALSE)
  standardise_data(data, mapping, dup_action = dup_action)
}

#' @rdname disappr_api
#' @export
disappr_fit <- function(x, models = MODEL_IDS, age_function = "Quadratic", family = "gaussian", ...) {
  if (!is.list(x) || is.null(x$data) || is.null(x$meta)) stop("'x' must come from disappr_prepare().", call. = FALSE)
  fit_model_suite(x$data, x$meta, models, age_function, family, ...)
}

#' @rdname disappr_api
#' @export
disappr_compare <- function(fit) fit$aic

#' @rdname disappr_api
#' @export
disappr_predict <- function(fit, model, ages = NULL, by = NULL) {
  if (is.null(fit$fits[[model]])) stop("Model ", model, " was not fitted.", call. = FALSE)
  if (is.null(ages)) ages <- smooth_prediction_ages(fit$data$age)
  predict_population_curve(fit$fits[[model]], fit, ages, by = by)
}

#' @rdname disappr_api
#' @export
disappr_diagnose <- function(fit) {
  list(validity = fit$validity, status = fit$status, rows_dropped = fit$drop_by,
       redundant_terms = fit$alias_notes, notes = fit$fit_notes)
}

#' @rdname disappr_api
#' @export
disappr_interpret <- function(fit) consistency_notes(fit)

#' @rdname disappr_api
#' @export
disappr_code <- function(fit, x, source_label = "your_data.csv") model_r_code(fit, x$meta, source_label)

#' @rdname disappr_api
#' @export
disappr_simulate <- function(...) simulate_toy_data(list(...))

#' @rdname disappr_api
#' @param path Path to a CSV or other delimited text file.
#' @export
disappr_read <- function(path) {
  r <- read_user_csv(path, basename(path))
  if (is.null(r$data)) stop(r$note, call. = FALSE)
  r$data
}

#' @rdname disappr_api
#' @param key Name of a bundled example, e.g. `"bouwhuis"` (see `names(disappR:::EXAMPLES)`).
#' @export
disappr_example <- function(key) {
  ex <- EXAMPLES[[key]]
  if (is.null(ex)) stop("Unknown example '", key, "'. Available: ", paste(names(EXAMPLES), collapse = ", "), call. = FALSE)
  app <- system.file("app", package = "disappR")
  if (!nzchar(app) && dir.exists(file.path("inst", "app"))) app <- file.path("inst", "app")
  if (!nzchar(app) || !dir.exists(file.path(app, "data"))) stop("The bundled example data were not found.", call. = FALSE)
  owd <- setwd(app)
  on.exit(setwd(owd), add = TRUE)
  raw <- load_example_file(ex$file)
  if (is.null(raw)) stop("The data file for example '", key, "' was not found.", call. = FALSE)
  if (!is.null(ex$subset)) raw <- raw[!is.na(raw[[ex$subset$var]]) & as.character(raw[[ex$subset$var]]) %in% ex$subset$levels, , drop = FALSE]
  list(data = raw, mapping = ex$mapping,
       settings = list(models = ex$models %||% MODEL_IDS, age_function = ex$age_function %||% "Quadratic",
                       family = ex$family %||% "gaussian", extra = ex$extra))
}

#' @rdname disappr_api
#' @export
disappr_provenance <- function(fit) fit$provenance

