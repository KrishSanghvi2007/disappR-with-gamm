# disappR engine - Provenance and runtime state: the disappR version, optional packages and their versions, computed when the
# engine is loaded (disappr_runtime_init(), called by .onLoad() and by the app's global.R).
# Engine code shared by the app and the R interface; changes that alter results must pass the golden tests.

# Optional packages: what each adds. Checked without loading them (fast), reported on the Start page and at start-up.
OPTIONAL_PACKAGES <- c(
  glmmTMB = "non-Gaussian families (Poisson, negative binomial, zero-inflated, binomial, beta-binomial)",
  lmerTest = "Satterthwaite t-test p-values for Gaussian models (otherwise asymptotic Wald z)",
  DHARMa = "simulation-based residual checks",
  nlme = "the population a\u00b7exp(b\u00b7age) model",
  performance = "performance-based model checks",
  see = "plots for the performance checks",
  codetools = "complete helper functions in exported code")

package_version_or_na <- function(p) {
  if (!nzchar(system.file(package = p))) return(NA_character_)
  tryCatch(as.character(utils::packageDescription(p, fields = "Version")), error = function(e) NA_character_)
}

optional_package_status <- function() {
  v <- vapply(names(OPTIONAL_PACKAGES), package_version_or_na, character(1))
  data.frame(Package = names(OPTIONAL_PACKAGES), Version = unname(v), Installed = !is.na(v),
             Needed_for = unname(OPTIONAL_PACKAGES), stringsAsFactors = FALSE)
}

# One comment line recording the versions in the current session (for exported code).
dependency_versions_line <- function(pkgs = c("lme4", "glmmTMB", "lmerTest")) {
  v <- vapply(pkgs, package_version_or_na, character(1))
  paste0("# Versions in the app session: disappR ", DISAPPR_VERSION, ", R ", R.version$major, ".", R.version$minor,
         if (any(!is.na(v))) paste0(", ", paste(paste(pkgs[!is.na(v)], v[!is.na(v)]), collapse = ", ")) else "")
}

# Runtime state. Placeholders keep the names defined when the package is built; the values are set when the engine is
# loaded: by .onLoad() for an installed package and by the app's global.R when the engine is sourced from a source
# checkout, so they reflect the packages installed now rather than when disappR was installed.
HAS_GLMMTMB <- FALSE
HAS_LMERTEST <- FALSE
HAS_DHARMA <- FALSE
DISAPPR_VERSION <- NA_character_
OPTIONAL_STATUS <- NULL

disappr_runtime_init <- function(env) {
  assign("HAS_GLMMTMB", requireNamespace("glmmTMB", quietly = TRUE), envir = env)
  assign("HAS_LMERTEST", requireNamespace("lmerTest", quietly = TRUE), envir = env)
  assign("HAS_DHARMA", requireNamespace("DHARMa", quietly = TRUE), envir = env)
  # disappR version: DESCRIPTION of a source checkout (inst/app -> ../../) or of an installed package (app -> ../),
  # then the installed package, then a fallback.
  assign("DISAPPR_VERSION", local({
    from_desc <- function(f) tryCatch({
      d <- read.dcf(f, fields = c("Package", "Version"))
      if (identical(unname(d[1, "Package"]), "disappR")) unname(d[1, "Version"]) else NA_character_
    }, error = function(e) NA_character_, warning = function(w) NA_character_)
    # The loaded package is asked first: it is the code that is actually running. The DESCRIPTION files are relative
    # to the app's own directory and are only reached when the app runs from a source checkout (0.20.18).
    v <- tryCatch(as.character(utils::packageVersion("disappR")), error = function(e) NA_character_)
    if (is.na(v)) v <- from_desc(file.path("..", "..", "DESCRIPTION"))
    if (is.na(v)) v <- from_desc(file.path("..", "DESCRIPTION"))
    if (is.na(v)) "unknown" else v
  }), envir = env)
  assign("OPTIONAL_STATUS", optional_package_status(), envir = env)
  invisible(env)
}

.onLoad <- function(libname, pkgname) {
  disappr_runtime_init(environment(disappr_runtime_init))
}

# ---- Moved unchanged from inst/app/server.R (0.9.11): pure helpers that use no reactive state ----

settings_sig <- function(s) {
  sig <- paste(s$family, s$zi, s$random_slope, s$standardise, s$among, s$include_invalid, s$trials %||% "", sep = "||")
  # the variance model joins the signature only when it is not the default, so earlier signatures are unchanged
  if (!identical(normalise_disp(s$disp), "constant")) sig <- paste0(sig, "||disp=", normalise_disp(s$disp))
  sig
}

# ---- Provenance of results (0.9.11) ----

# Provenance of a model comparison: what was analysed (data fingerprint, rows and individuals), how (formulas, family,
# random effects, transformations and settings), with which software, and the warnings raised.
analysis_provenance <- function(res, meta = NULL) {
  d <- res$data
  md5_of <- function(x) tryCatch({
    f <- tempfile(fileext = ".csv")
    on.exit(unlink(f), add = TRUE)
    utils::write.csv(x, f, row.names = FALSE)
    unname(tools::md5sum(f))
  }, error = function(e) NA_character_)
  plain <- if (is.data.frame(d)) {
    x <- d
    x[] <- lapply(x, function(v) if (is.factor(v)) as.character(v) else v)
    x[, !vapply(x, is.list, logical(1)), drop = FALSE]
  } else data.frame()
  ids <- if (is.data.frame(d) && "id" %in% names(d)) sort(unique(as.character(d$id))) else character(0)
  pkgs <- c("lme4", "glmmTMB", "lmerTest", "DHARMa", "nlme", "ggplot2", "shiny", "Matrix", "TMB")
  blas <- tryCatch({
    si <- utils::sessionInfo()
    c(BLAS = si$BLAS %||% NA_character_, LAPACK = si$LAPACK %||% NA_character_)
  }, error = function(e) c(BLAS = NA_character_, LAPACK = NA_character_))
  list(
    created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    created_utc = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
    software = c(disappR = DISAPPR_VERSION, R = paste(R.version$major, R.version$minor, sep = "."),
                 vapply(pkgs, package_version_or_na, character(1))),
    platform = c(os = R.version$platform, system = R.version$system, sysname = unname(Sys.info()[["sysname"]]),
                 release = unname(Sys.info()[["release"]]), locale = Sys.getlocale("LC_COLLATE"),
                 blas = unname(blas[["BLAS"]]), lapack = unname(blas[["LAPACK"]])),
    source = list(file = meta$source_file %||% NA_character_, file_md5 = meta$source_md5 %||% NA_character_,
                  rows_imported = meta$n_rows_raw %||% NA_integer_),
    mapping = meta$map %||% list(),
    data = list(fingerprint_md5 = md5_of(plain), rows = nrow(plain), individuals = length(ids),
                ids_md5 = md5_of(data.frame(id = ids))),
    model = list(family = res$family, age_function = res$age_function, formulas = res$formulas,
                 random = res$random, zero_inflation = res$zi, dispersion = res$disp_formula %||% "~1"),
    transformations = list(standardise = isTRUE(res$standardise), age = res$age_params, proxies = res$proxy_params,
                           age_rounding = meta$age_round, subset = meta$subset, duplicates = meta$dup_action),
    settings = list(models = names(res$status), among = res$among, extra = res$extra,
                    include_invalid = isTRUE(res$include_invalid), random_request = res$random_request),
    warnings = list(status = res$status, validity = res$validity, rows_dropped = res$drop_by,
                    redundant_terms = res$alias_notes, notes = res$fit_notes))
}

# The whole provenance record as JSON: the reproducibility bundle shipped with every export.
provenance_json <- function(p) {
  if (is.null(p)) return("{}")
  tryCatch(jsonlite::toJSON(p, auto_unbox = TRUE, null = "null", na = "string", pretty = TRUE, force = TRUE),
           error = function(e) "{}")
}

# Short text lines summarising a provenance record (for saved results, reports and exported code).
provenance_lines <- function(p) {
  if (is.null(p) || !is.list(p)) return(character(0))
  sw <- p$software[!is.na(p$software)]
  c(sprintf("Analysed data: %s rows from %s individuals (md5 %s; IDs md5 %s)", p$data$rows, p$data$individuals,
            p$data$fingerprint_md5, p$data$ids_md5),
    sprintf("Model settings: %s family, %s ageing, %s; random effects %s", p$model$family %||% "", p$model$age_function %||% "",
            if (isTRUE(p$transformations$standardise)) "age and proxies standardised" else "raw scales", p$model$random %||% ""),
    paste("Software:", paste(names(sw), sw, collapse = ", ")),
    if (!is.null(p$platform)) sprintf("Platform: %s (%s); BLAS %s", p$platform[["os"]] %||% "", p$platform[["sysname"]] %||% "",
                                      p$platform[["blas"]] %||% "unknown") else NULL,
    if (!is.null(p$source) && !is.na(p$source$file %||% NA)) sprintf("Input file: %s (md5 %s, %s rows read)",
      p$source$file, p$source$file_md5 %||% "NA", p$source$rows_imported %||% "NA") else NULL,
    paste("Created:", p$created))
}

