# disappR engine - Model formulas: Models 1-10, extra and built terms, covariate terms, random-effect structures and model equations.
# Engine code shared by the app and the R interface; changes that alter results must pass the golden tests.

# Human-readable random-effect structure using the user's column names.
# Individual-level random-effect structures. TRUE/FALSE (older settings) map to correlated/none.
RANDOM_STRUCTURES <- c("Random intercept: (1 | ID)" = "none",
                       "Automatic: chosen from the data support" = "auto",
                       "Uncorrelated intercept and slope: (1 | ID) + (0 + age | ID)" = "uncorrelated",
                       "Correlated intercept and slope: (1 + age | ID)" = "correlated",
                       "Uncorrelated intercept and every age term: (1 | ID) + (0 + age | ID) + (0 + age\u00b2 | ID)" = "uncorrelated_all",
                       "Correlated intercept and every age term: (1 + age + age\u00b2 | ID)" = "correlated_all")

normalise_slope <- function(x) {
  if (isTRUE(x)) return("correlated")
  if (is.null(x) || !length(x) || isFALSE(x) || is.na(x[[1]])) return("none")
  x <- as.character(x[[1]])
  if (x %in% c("none", "auto", "uncorrelated", "correlated", "uncorrelated_all", "correlated_all")) x else "none"
}

slope_text <- function(x) {
  switch(normalise_slope(x), correlated = "correlated random intercept and slope", uncorrelated = "uncorrelated random intercept and slope",
         correlated_all = "correlated random intercept and slopes for every age term",
         uncorrelated_all = "uncorrelated random intercept and slopes for every age term",
         auto = "random slope chosen automatically", "random intercept only")
}

random_display <- function(meta, random_slope = FALSE, slope_label = "age") {
  map <- meta$map
  g2 <- isTRUE(meta$has_group2)
  grp_lab <- if (g2 && isTRUE(meta$nested)) paste0(map$group2, ":", map$group) else map$group
  id_lab <- if (isTRUE(meta$has_group) && isTRUE(meta$nested)) paste0(grp_lab, ":", map$id) else map$id
  rs <- normalise_slope(random_slope)
  # slope_label may arrive as one string covering every age term ("age + age\u00b2"); split it so each term is named
  sl <- strsplit(paste(slope_label, collapse = " + "), " + ", fixed = TRUE)[[1]]
  terms <- c(
    if (g2) paste0("(1 | ", map$group2, ")"),
    if (isTRUE(meta$has_group)) paste0("(1 | ", grp_lab, ")"),
    switch(rs,
      correlated = paste0("(1 + ", sl[[1]], " | ", id_lab, ")"),
      uncorrelated = paste0("(1 | ", id_lab, ") + (0 + ", sl[[1]], " | ", id_lab, ")"),
      correlated_all = paste0("(1 + ", paste(sl, collapse = " + "), " | ", id_lab, ")"),
      uncorrelated_all = paste0("(1 | ", id_lab, ")", paste0(" + (0 + ", sl, " | ", id_lab, ")", collapse = "")),
      auto = paste0("(1 | ", id_lab, ") with a random slope if the data support it"),
      paste0("(1 | ", id_lab, ")")),
    if (length(meta$random_terms)) paste0("(1 | ", unname(meta$random_labels[meta$random_terms]), ")")
  )
  paste(terms, collapse = " + ")
}

# ---------------------------------------------------------------------------
# Comparative Models 1-8
# ---------------------------------------------------------------------------
# among = "linear": ALR, LS, AFR and mean age enter linearly (default, as in the manuscript).
# among = "same": powers of the proxy (ALR + ALR2 with a quadratic) and powers of the MEAN AGE
# (mean_f1 + mean_f1_2) in the centring models. Note that mean_f1_2 is (mean age)^2, which is not the
# individual mean of age^2: they differ by Var_i(age). Paired with delta_f2 = age^2 - mean(age^2), the
# two columns therefore do not reconstruct age^2. Kept because it mirrors ALR + ALR2, but it is not a
# decomposition of the age basis.
# among = "consistent": powers of the proxy as above, but the centring models use the individual MEAN OF
# age^k (mean_f2, mean_f3) as the among-individual terms. Those pair exactly with delta_fk = f_k - mean_fk,
# so among + within reconstructs the age basis term by term. This is the internally consistent
# higher-order decomposition.
# cov_age: covariates that interact with the ageing terms (covariate x age), in addition to their main effect.
model_formula_strings <- function(b, covars = character(0), among = "linear", cov_age = character(0), cov_pairs = character(0)) {
  delta <- paste0("delta_", b)
  poly_among <- among %in% c("same", "consistent") && length(b) > 1
  # "same": powers of mean age. "consistent": the individual means of age^k, which pair exactly with delta_fk.
  mean_terms <- if (!poly_among) "mean_f1"
                else if (identical(among, "consistent")) paste0("mean_", b)
                else c("mean_f1", paste0("mean_f1_", seq_along(b)[-1]))
  if (poly_among) {
    prox <- function(v) paste(c(v, paste0(v, seq_along(b)[-1])), collapse = " + ")
    bs <- paste(b, collapse = " + ")
    core <- c(
      M1 = bs,
      M2 = paste(bs, "+", prox("ALR")),
      M3 = paste(c(mean_terms, delta), collapse = " + "),
      M4 = paste0("(", bs, ") * (", prox("ALR"), ")"),
      M5 = paste0("(", paste(mean_terms, collapse = " + "), ") * (", paste(delta, collapse = " + "), ")"),
      M6 = paste0("(", bs, ") * (", prox("LS"), ")"),
      M7 = paste(bs, "+", prox("ALR"), "+", prox("AFR")),
      M8 = paste0("(", bs, ") * (", prox("ALR"), ") + (", bs, ") * (", prox("AFR"), ")"),
      M9 = paste0("(", bs, ") * (", prox("ALR"), ") + ", prox("AFR")),
      M10 = paste0("(", bs, ") * (", prox("AFR"), ") + ", prox("ALR"))
    )
    return(add_covariate_terms(core, b, covars, cov_age, mean_terms, cov_pairs))
  }
  core <- c(
    M1 = paste(b, collapse = " + "),
    M2 = paste(c(b, "ALR"), collapse = " + "),
    M3 = paste(c("mean_f1", delta), collapse = " + "),
    M4 = paste(paste0(b, " * ALR"), collapse = " + "),
    M5 = paste0("mean_f1 * (", paste(delta, collapse = " + "), ")"),
    M6 = paste(paste0(b, " * LS"), collapse = " + "),
    M7 = paste(c(b, "ALR", "AFR"), collapse = " + "),
    M8 = paste(c(paste0(b, " * ALR"), paste0(b, " * AFR")), collapse = " + "),
    M9 = paste(c(paste0(b, " * ALR"), "AFR"), collapse = " + "),
    M10 = paste(c(paste0(b, " * AFR"), "ALR"), collapse = " + ")
  )
  add_covariate_terms(core, b, covars, cov_age, mean_terms, cov_pairs)
}

# Extra terms added to chosen models: extra = list(models = c("M5"), terms = c("ALR", "AFR_x_age", "cv_diet")).
# Keys: ALR, AFR, LS (additive), mean_age (additive), ALR_x_age, AFR_x_age, LS_x_age (proxy x ageing terms,
# with main effects), a covariate's internal name (additive) or "<covariate>:age" (covariate x ageing terms).
# Built terms (term builder): "term:" + up to three tokens (age, ALR, AFR, LS, mean_age or a covariate's internal
# name) joined by "+" (additive) or "*" (interaction with main effects, as in R), e.g. "term:ALR*AFR*age".
EXTRA_TERM_KEYS <- c("ALR (additive)" = "ALR", "AFR (additive)" = "AFR", "LS (additive)" = "LS", "Mean age (additive)" = "mean_age",
                     "ALR \u00d7 age" = "ALR_x_age", "AFR \u00d7 age" = "AFR_x_age", "LS \u00d7 age" = "LS_x_age")

BUILT_TERM_TOKENS <- c("age", "ALR", "AFR", "LS", "mean_age")

# Additive components of a built term, each a vector of interacting tokens: "term:ALR*AFR+cv_diet" gives
# list(c("ALR", "AFR"), "cv_diet"). NULL when the key is not a built term.
parse_built_term <- function(k) {
  if (!is.character(k) || length(k) != 1 || is.na(k) || !startsWith(k, "term:")) return(NULL)
  comps <- strsplit(strsplit(substring(k, 6), "+", fixed = TRUE)[[1]], "*", fixed = TRUE)
  comps <- lapply(comps, function(x) unique(trimws(x[nzchar(trimws(x))])))
  comps[vapply(comps, length, integer(1)) > 0]
}

built_term_ok <- function(k, covars = character(0)) {
  comps <- parse_built_term(k)
  toks <- unlist(comps)
  length(comps) > 0 && length(toks) <= 3 && all(toks %in% c(BUILT_TERM_TOKENS, covars))
}

built_term_age_interaction <- function(k) {
  comps <- parse_built_term(k)
  length(comps) > 0 && any(vapply(comps, function(cp) length(cp) > 1 && "age" %in% cp, logical(1)))
}

# Fixed-effect terms of a built key: age stands for every ageing term of the function (for the centring Models 3 and
# 5, the individual mean and within-individual deviation terms), ALR/AFR/LS for their linear or polynomial
# among-individual terms; components are joined as written (A * B = A + B + A:B).
built_term_formula <- function(k, b, among = "linear", centred = FALSE) {
  comps <- parse_built_term(k)
  if (!length(comps)) return("")
  poly_among <- among %in% c("same", "consistent") && length(b) > 1
  prox <- function(v) if (poly_among) paste(c(v, paste0(v, seq_along(b)[-1])), collapse = " + ") else v
  mean_terms <- if (!poly_among) "mean_f1"
                else if (identical(among, "consistent")) paste(paste0("mean_", b), collapse = " + ")
                else paste(c("mean_f1", paste0("mean_f1_", seq_along(b)[-1])), collapse = " + ")
  age_terms <- if (isTRUE(centred)) paste(c(strsplit(mean_terms, " + ", fixed = TRUE)[[1]], paste0("delta_", b)), collapse = " + ") else paste(b, collapse = " + ")
  wrap <- function(x) if (grepl("+", x, fixed = TRUE)) paste0("(", x, ")") else x
  piece <- function(tok) wrap(switch(tok, age = age_terms, ALR = prox("ALR"), AFR = prox("AFR"), LS = prox("LS"), mean_age = mean_terms, tok))
  paste(vapply(comps, function(cp) paste(vapply(cp, piece, character(1)), collapse = " * "), character(1)), collapse = " + ")
}

# Plain-language label of an extra-term key (menus, summaries).
extra_term_display <- function(k, meta = NULL) {
  labs <- meta$cov_labels %||% character(0)
  tok <- function(t) if (t %in% names(labs)) unname(labs[[t]]) else switch(t, age = "age", mean_age = "mean age", display_term(t))
  comps <- parse_built_term(k)
  if (length(comps)) return(paste(vapply(comps, function(cp) paste(vapply(cp, tok, character(1)), collapse = " \u00d7 "), character(1)), collapse = " + "))
  if (k %in% EXTRA_TERM_KEYS) return(names(EXTRA_TERM_KEYS)[match(k, EXTRA_TERM_KEYS)])
  if (grepl(":age$", k)) return(paste0(tok(sub(":age$", "", k)), " \u00d7 age"))
  if (k %in% names(labs)) return(paste0(labs[[k]], " (additive)"))
  k
}

apply_extra_terms <- function(fs, b, extra = NULL, among = "linear") {
  ex <- per_model_extra(extra)
  if (!length(ex)) return(fs)
  poly_among <- among %in% c("same", "consistent") && length(b) > 1
  prox <- function(v) if (poly_among) paste(c(v, paste0(v, seq_along(b)[-1])), collapse = " + ") else v
  mean_terms <- if (!poly_among) "mean_f1"
                else if (identical(among, "consistent")) paste(paste0("mean_", b), collapse = " + ")
                else paste(c("mean_f1", paste0("mean_f1_", seq_along(b)[-1])), collapse = " + ")
  for (m in intersect(names(ex), names(fs))) {
    age_terms <- if (m %in% c("M3", "M5")) paste(c(strsplit(mean_terms, " + ", fixed = TRUE)[[1]], paste0("delta_", b)), collapse = " + ") else paste(b, collapse = " + ")
    add <- vapply(ex[[m]], function(k) {
      if (k %in% c("ALR", "AFR", "LS")) return(prox(k))
      if (identical(k, "mean_age")) return(mean_terms)
      if (k %in% c("ALR_x_age", "AFR_x_age", "LS_x_age")) return(paste0("(", age_terms, ") * (", prox(sub("_x_age$", "", k)), ")"))
      if (startsWith(k, "term:")) return(built_term_formula(k, b, among, m %in% c("M3", "M5")))
      if (grepl(":age$", k)) return(paste0(sub(":age$", "", k), " * (", age_terms, ")"))
      k
    }, character(1))
    fs[[m]] <- paste(fs[[m]], "+", paste(add, collapse = " + "))
  }
  fs
}

# Extra terms are stored per model: list(M5 = "ALR", M6 = c("ALR", "cv_diet:age")). The older form
# list(models = ..., terms = ...) (the same terms for several models) is converted.
per_model_extra <- function(extra) {
  if (is.null(extra) || !length(extra)) return(list())
  if (!is.null(extra$models) && !is.null(extra$terms)) {
    return(stats::setNames(rep(list(unique(extra$terms)), length(extra$models)), extra$models))
  }
  extra[intersect(names(extra), MODEL_IDS)]
}

# Keep only extra-term keys that are known or refer to covariates in the analysed data.
clean_extra_terms <- function(extra, covars) {
  ex <- per_model_extra(extra)
  out <- list()
  for (m in names(ex)) {
    k <- unique(as.character(ex[[m]]))
    keep <- k[k %in% EXTRA_TERM_KEYS | k %in% covars | (grepl(":age$", k) & sub(":age$", "", k) %in% covars) |
              vapply(k, built_term_ok, logical(1), covars = covars, USE.NAMES = FALSE)]
    if (length(keep)) out[[m]] <- keep
  }
  if (length(out)) out else NULL
}

# TRUE if every term of the smaller model is also in the larger one (so a likelihood-ratio test is valid).
formula_nested <- function(small, large) {
  tl <- function(s) {
    lab <- tryCatch(attr(stats::terms(stats::as.formula(paste("y ~", s))), "term.labels"), error = function(e) NULL)
    if (is.null(lab)) return(NULL)
    vapply(strsplit(lab, ":", fixed = TRUE), function(p) paste(sort(p), collapse = ":"), character(1))
  }
  a <- tl(small)
  b <- tl(large)
  !is.null(a) && !is.null(b) && all(a %in% b) && length(b) > length(a)
}

add_covariate_terms <- function(core, b, covars, cov_age, mean_terms = "mean_f1", cov_pairs = character(0)) {
  cov_age <- intersect(cov_age, covars)
  cov_pairs <- cov_pairs[vapply(strsplit(cov_pairs, ":", fixed = TRUE), function(p) length(p) == 2 && all(p %in% covars), logical(1))]
  if (length(cov_pairs)) core[] <- paste(core, "+", paste(cov_pairs, collapse = " + "))
  if (length(cov_age)) {
    age_default <- paste(b, collapse = " + ")
    age_centred <- paste(c(mean_terms, paste0("delta_", b)), collapse = " + ")
    for (m in names(core)) {
      at <- if (m %in% c("M3", "M5")) age_centred else age_default
      core[[m]] <- paste0(core[[m]], " + ", paste0(cov_age, ":(", at, ")", collapse = " + "))
    }
  }
  if (length(covars)) core[] <- paste0(paste(covars, collapse = " + "), " + ", core)
  core
}

# Individual IDs are already made unique within groups when nesting is selected
# (standardise_data), so (1 | group) + (1 | id) is the nested (1 | group/id) structure;
# without nesting the same terms give crossed random effects. With a second grouping level the groups are also made
# unique within top-level groups, so (1 | group2) + (1 | group) + (1 | id) is (1 | group2/group/id).
# b1: the basis columns of the ageing function. The "_all" structures put a slope on every one of them, which is
# what covers among-individual differences in curvature; with a one-term function they collapse to the plain forms.
random_term_string <- function(b1 = "f1", random_slope = FALSE, has_group = FALSE, extra = character(0), has_group2 = FALSE) {
  b1 <- as.character(b1)
  id_term <- switch(normalise_slope(random_slope),
    correlated = paste0("(1 + ", b1[[1]], " | id)"),
    uncorrelated = paste0("(1 | id) + (0 + ", b1[[1]], " | id)"),
    correlated_all = paste0("(1 + ", paste(b1, collapse = " + "), " | id)"),
    uncorrelated_all = paste0("(1 | id)", paste0(" + (0 + ", b1, " | id)", collapse = "")),
    "(1 | id)")
  terms <- c(if (isTRUE(has_group2)) "(1 | group2)", if (isTRUE(has_group)) "(1 | group)", id_term, if (length(extra)) paste0("(1 | ", extra, ")"))
  paste(terms, collapse = " + ")
}

model_definition_table <- function(age_function, covars = character(0), random_str = "(1 | id)", cov_labels = character(0),
                                   among = "linear", cov_age = character(0), cov_pairs = character(0)) {
  raw <- basis_label(age_function)
  poly <- among %in% c("same", "consistent") && age_function %in% c("Quadratic", "Cubic")
  pw <- if (identical(age_function, "Cubic")) c("", "\u00b2", "\u00b3") else c("", "\u00b2")
  px <- function(v) if (poly) paste0("(", paste0(v, pw, collapse = " + "), ")") else v
  first <- switch(age_function, Logarithmic = "log(age)", "Asymptotic exponential" = "exp(\u2212z_age)", "age")
  deltas <- switch(age_function,
    Linear = "\u0394age", Quadratic = "\u0394age + \u0394age\u00b2", Cubic = "\u0394age + \u0394age\u00b2 + \u0394age\u00b3",
    Logarithmic = "\u0394log(age)", "Asymptotic exponential" = "\u0394exp(\u2212z_age)", "\u0394age")
  cv_txt <- if (length(covars)) paste0(paste(unname(cov_labels[covars] %||% display_term(covars)), collapse = " + "), " + ") else ""
  cv_age_txt <- if (length(intersect(cov_age, covars))) paste0(" + ", paste(unname(cov_labels[intersect(cov_age, covars)] %||% display_term(cov_age)), collapse = ", "), " \u00d7 age terms") else ""
  if (length(cov_pairs)) cv_age_txt <- paste0(cv_age_txt, " + ", paste(readable_terms(cov_pairs, age_function, cov_labels), collapse = " + "))
  mean_txt <- if (!poly) paste0("mean(", first, ")")
              else if (identical(among, "consistent")) paste0("mean(", paste0(first, pw, collapse = ") + mean("), ")")   # mean of each age term
              else paste(paste0("mean(", first, ")", pw), collapse = " + ")                                              # powers of mean age
  data.frame(
    Model = model_label(MODEL_IDS),
    Name = vapply(MODEL_IDS, function(m) MODEL_MEANING[[m]]$name, character(1), USE.NAMES = FALSE),   # one source of model names
    Fixed_effects = paste0(cv_txt, c(
      raw, paste(raw, "+", px("ALR")), paste0(mean_txt, " + ", deltas), paste0("(", raw, ") \u00d7 ", px("ALR")),
      paste0(if (poly) paste0("(", mean_txt, ")") else mean_txt, " \u00d7 (", deltas, ")"), paste0("(", raw, ") \u00d7 ", px("LS")),
      paste(raw, "+", px("ALR"), "+", px("AFR")), paste0("(", raw, ") \u00d7 ", px("ALR"), " + (", raw, ") \u00d7 ", px("AFR")),
      paste0("(", raw, ") \u00d7 ", px("ALR"), " + ", px("AFR")), paste0("(", raw, ") \u00d7 ", px("AFR"), " + ", px("ALR"))), cv_age_txt),
    Random = random_str,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Per-model "i" help: meaning of each model and its exact specification under the current settings
# ---------------------------------------------------------------------------
MODEL_MEANING <- list(
  M1 = list(name = "Naive", proxy = "none", dis = "not accounted for", app = "not accounted for",
       purpose = "Negative control: no selective disappearance term.",
            text = "No selective disappearance or appearance term. The trajectory mixes within-individual ageing with changes in which individuals are still being sampled, so it is biased when individuals that disappear early differ from the others (negative control)."),
  M2 = list(name = "Additive ALR", proxy = "ALR (age at last record in the data)", dis = "age-independent", app = "not accounted for",
       purpose = "Age-independent selective disappearance (van de Pol & Verhulst 2006).",
            text = "ALR-based. ALR enters as a main effect, so longer- and shorter-lived individuals may differ by the same amount at every age (van de Pol & Verhulst 2006)."),
  M3 = list(name = "Mean-age centring", proxy = "individual mean age", dis = "age-independent", app = "not accounted for",
       purpose = "Within/among-individual separation (van de Pol & Wright 2009; Fay et al. 2022).",
            text = "Centring-based. Each individual's age is split into its mean age (among-individual term) and the deviation from that mean (\u0394age, within-individual ageing; \u0394age\u00b2 is age\u00b2 minus the individual's mean of age\u00b2), so among-individual differences such as selective disappearance do not bias the within-individual ageing terms (van de Pol & Wright 2009; Fay et al. 2022)."),
  M4 = list(name = "ALR interaction", proxy = "ALR (age at last record in the data)", dis = "age-dependent (includes the age-independent ALR effect)", app = "not accounted for",
       purpose = "Age-dependent selective disappearance through ALR \u00d7 ageing terms.",
            text = "ALR-based. ALR interacts with the ageing terms, so the difference between longer- and shorter-lived individuals can change with age."),
  M5 = list(name = "Centring interaction", proxy = "individual mean age", dis = "age-dependent (includes the age-independent mean-age effect)", app = "not accounted for",
       purpose = "Age-dependent selective disappearance through mean age \u00d7 \u0394age terms.",
            text = "Centring-based. Individual mean age interacts with the within-individual ageing terms, so individuals sampled at older ages (usually the longer-lived) can age differently."),
  M6 = list(name = "LS interaction (positive control)", proxy = "known lifespan (LS)", dis = "age-dependent (includes the age-independent LS effect)", app = "not accounted for",
       purpose = "Known latent lifespan as the interacting covariate (simulation benchmark).",
            text = "Known lifespan interacts with the ageing terms. Only available when lifespan is truly known; a benchmark for Models 4 and 5."),
  M7 = list(name = "Additive ALR + AFR", proxy = "ALR and AFR (age at first observation)", dis = "age-independent", app = "age-independent",
       purpose = "Age-independent disappearance (ALR) and appearance (AFR).",
            text = "ALR-based. ALR and AFR enter as main effects: individuals sampled until later, or first observed later, may differ by constant amounts at all ages."),
  M8 = list(name = "Interactive ALR + AFR", proxy = "ALR and AFR (age at first observation)", dis = "age-dependent", app = "age-dependent",
       purpose = "Age-dependent disappearance and appearance.",
            text = "ALR-based. ALR and AFR both interact with the ageing terms."),
  M9 = list(name = "Interactive ALR + additive AFR", proxy = "ALR and AFR (age at first observation)", dis = "age-dependent", app = "age-independent",
       purpose = "Age-dependent disappearance (ALR) with age-independent appearance (AFR).",
            text = "ALR-based. ALR interacts with the ageing terms; AFR enters as a main effect only."),
  M10 = list(name = "Additive ALR + interactive AFR", proxy = "ALR and AFR (age at first observation)", dis = "age-independent", app = "age-dependent",
       purpose = "Age-independent disappearance (ALR) with age-dependent appearance (AFR).",
             text = "ALR-based. AFR interacts with the ageing terms; ALR enters as a main effect only.")
)

# Internal term names in plain language, e.g. "cv_diet:f1" -> "diet x age"
readable_terms <- function(x, age_function = "Quadratic", cov_labels = character(0)) {
  age_lab <- switch(age_function, Logarithmic = "log(age)", "Asymptotic exponential" = "exp(\u2212z_age)", "age")
  s <- x
  for (cv in names(cov_labels)) {
    s <- gsub(paste0("\\b", gsub(".", "\\.", cv, fixed = TRUE), "\\b"), cov_labels[[cv]], s, perl = TRUE)
  }
  s <- gsub("\\bmean_f1\\b", paste0("mean(", age_lab, ")"), s, perl = TRUE)
  s <- gsub("\\bmean_f2\\b", "mean(age\u00b2)", s, perl = TRUE)
  s <- gsub("\\bmean_f3\\b", "mean(age\u00b3)", s, perl = TRUE)
  s <- gsub("\\bdelta_f1\\b", paste0("\u0394", age_lab), s, perl = TRUE)
  s <- gsub("\\bdelta_f2\\b", "\u0394age\u00b2", s, perl = TRUE)
  s <- gsub("\\bdelta_f3\\b", "\u0394age\u00b3", s, perl = TRUE)
  s <- gsub("\\bf1\\b", age_lab, s, perl = TRUE)
  s <- gsub("\\bf2\\b", "age\u00b2", s, perl = TRUE)
  s <- gsub("\\bf3\\b", "age\u00b3", s, perl = TRUE)
  s <- gsub("\\b(ALR|AFR|LS)2\\b", "\\1\u00b2", s, perl = TRUE)
  s <- gsub("\\b(ALR|AFR|LS)3\\b", "\\1\u00b3", s, perl = TRUE)
  s <- gsub(" * ", " \u00d7 ", s, fixed = TRUE)
  gsub(":", " \u00d7 ", s, fixed = TRUE)
}

model_info_content <- function(m, s, meta = NULL) {
  mm <- MODEL_MEANING[[m]]
  fn <- s$age_function %||% "Quadratic"
  fam <- s$family %||% "gaussian"
  covars <- meta$covars %||% character(0)
  cov_labels <- meta$cov_labels %||% character(0)
  cov_age <- intersect(meta$cov_age %||% character(0), covars)
  cov_pairs <- meta$cov_pairs %||% character(0)
  extra <- clean_extra_terms(s$extra, covars)
  rhs <- function(x) if (length(x)) paste(x, collapse = " + ") else "1"
  r_formula <- NULL
  if (identical(fn, A3_NONLINEAR) && identical(fam, "gaussian")) {
    if (m %in% c("M3", "M5")) {
      spec <- "Not available with the non-linear exponential function (the centring models need a linear-in-parameters age function)."
    } else {
      parts <- nonlinear_extra_parts(NONLINEAR_PARTS[[m]], extra[[m]] %||% character(0), covars)
      a <- union(c(covars, cov_pairs), parts$a)
      b <- union(cov_age, parts$b)
      spec <- paste0("trait = a \u00b7 exp(b \u00b7 z_age); level a ~ ", readable_terms(rhs(a), "Linear", cov_labels),
                     "; rate b ~ ", readable_terms(rhs(b), "Linear", cov_labels))
    }
  } else {
    fn2 <- if (identical(fn, A3_NONLINEAR)) "Linear" else fn
    b <- switch(fn2, Quadratic = c("f1", "f2"), Cubic = c("f1", "f2", "f3"), "f1")
    among <- s$among %||% "linear"
    fs <- apply_extra_terms(model_formula_strings(b, covars, among, cov_age, cov_pairs), b, extra, among)
    spec <- paste("trait ~", readable_terms(fs[[m]], fn2, cov_labels))
    r_formula <- paste("trait ~", fs[[m]], "+", random_term_string(b, s$random_slope, isTRUE(meta$has_group), meta$random_terms %||% character(0), has_group2 = isTRUE(meta$has_group2)))
  }
  list(title = paste0(model_label(m), " \u00b7 ", mm$name), meaning = mm$text,
       attributes = c(Purpose = mm$purpose %||% "", `Lifespan proxy` = mm$proxy,
                      `Selective disappearance` = mm$dis, `Selective appearance` = mm$app),
       spec = spec, random = if (is.null(meta)) "(1 | ID)" else random_display(meta, s$random_slope),
       family = paste0(family_label(fam), if (fam %in% ZI_FAMILIES) paste0("; zero inflation ", s$zi %||% "~1") else "",
                       if (!identical(normalise_disp(s$disp), "constant") && fam %in% DISPERSION_FAMILIES) paste0("; ", disp_text(s$disp)) else ""),
       ageing = fn, extra = extra[[m]] %||% character(0), r_formula = r_formula)
}

# ---------------------------------------------------------------------------
# Exact model equation for the per-model "i": the fixed effects fully expanded (A * B written as A + B + A:B),
# each term with its own coefficient, the random effects and error distribution, and the R syntax as fitted.
# ---------------------------------------------------------------------------
html_esc <- function(x) htmltools::htmlEscape(as.character(x))

# One component of a model term in mathematical notation (HTML). i = individual, j = sampling occasion.
math_part <- function(p, age_function = "Quadratic", meta = NULL, level = NA_character_) {
  age <- switch(age_function, Logarithmic = "ln(age)", "Asymptotic exponential" = "exp(\u2212age)", "age")
  pw <- function(k) if (k > 1) paste0("<sup>", k, "</sup>") else ""
  if (grepl("^f[1-3]$", p)) {
    k <- as.integer(substring(p, 2))
    return(paste0(age, "<sub>ij</sub>", pw(k)))
  }
  if (grepl("^mean_f[1-3]$", p)) {
    k <- as.integer(substring(p, 7))
    return(paste0("mean(", age, pw(k), ")<sub>i</sub>"))
  }
  if (grepl("^delta_f[1-3]$", p)) {
    k <- as.integer(substring(p, 8))
    return(if (k > 1) paste0("\u0394(", age, pw(k), ")<sub>ij</sub>") else paste0("\u0394", age, "<sub>ij</sub>"))
  }
  if (grepl("^(ALR|AFR|LS)[23]?$", p)) {
    v <- sub("[23]$", "", p)
    k <- if (grepl("[23]$", p)) as.integer(substring(p, nchar(p))) else 1L
    return(paste0(v, "<sub>i</sub>", pw(k)))
  }
  lab <- html_esc((meta$cov_labels %||% character(0))[p] %|NA|% display_term(p))
  if (!is.na(level)) paste0("[", lab, " = ", html_esc(level), "]<sub>ij</sub>") else paste0(lab, "<sub>ij</sub>")
}

# Terms as R expands them (terms() order, as in the coefficient table), with one row per coefficient:
# categorical covariates get one indicator per non-reference level.
expand_model_terms <- function(rhs, factor_levels = list()) {
  if (!nzchar(trimws(rhs %||% ""))) return(list())
  tl <- attr(stats::terms(stats::as.formula(paste("~", rhs))), "term.labels")
  out <- list()
  for (t in tl) {
    parts <- strsplit(t, ":", fixed = TRUE)[[1]]
    choices <- lapply(parts, function(p) {
      lv <- factor_levels[[p]]
      if (length(lv) >= 2) as.character(lv[-1]) else NA_character_
    })
    combos <- expand.grid(choices, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
    for (r in seq_len(nrow(combos))) {
      lev <- vapply(seq_along(parts), function(q) as.character(combos[r, q]), character(1))
      out[[length(out) + 1]] <- list(term = t, parts = parts, levels = lev,
                                     r_name = paste(ifelse(is.na(lev), parts, paste0(parts, lev)), collapse = ":"))
    }
  }
  out
}

model_equation <- function(m, s, meta = NULL, factor_levels = list(), dat = NULL) {
  fn <- s$age_function %||% "Quadratic"
  fam <- s$family %||% "gaussian"
  covars <- meta$covars %||% character(0)
  cov_age <- intersect(meta$cov_age %||% character(0), covars)
  cov_pairs <- meta$cov_pairs %||% character(0)
  extra <- clean_extra_terms(s$extra, covars)
  rs_req <- normalise_slope(s$random_slope)
  rs <- rs_req
  rs_note <- ""
  if (identical(rs_req, "auto")) {
    adv <- if (!is.null(dat)) tryCatch(random_slope_advice(individual_data_support(dat)), error = function(e) NULL) else NULL
    rs <- adv$recommended %||% "none"
    rs_note <- paste0("Automatic random effects: with the current data this is the ", slope_text(rs), " structure.")
  }
  sb <- function(x, k) paste0(x, "<sub>", k, "</sub>")
  bta <- function(k) sb("\u03b2", k)
  nonlinear <- identical(fn, A3_NONLINEAR) && identical(fam, "gaussian")
  fn_math <- if (identical(fn, A3_NONLINEAR)) "Linear" else fn
  slope_var <- math_part("f1", fn_math, meta)
  # ---- random effects (shared by both model types)
  re_terms <- character(0); re_dist <- character(0)
  if (isTRUE(meta$has_group)) {
    re_terms <- c(re_terms, sb("g", "k"))
    re_dist <- c(re_dist, paste0(sb("g", "k"), " ~ N(0, \u03c3<sup>2</sup><sub>group</sub>)", if (isTRUE(meta$nested)) ": individuals i are nested in groups k" else ": groups k crossed with individuals"))
  }
  if (isTRUE(meta$has_group2)) {
    re_terms <- c(re_terms, sb("h", "l"))
    re_dist <- c(re_dist, paste0(sb("h", "l"), " ~ N(0, \u03c3<sup>2</sup><sub>group2</sub>)", if (isTRUE(meta$nested)) ": groups k are nested in top-level groups l" else ": top-level groups l crossed with groups k and individuals"))
  }
  for (rt in meta$random_terms %||% character(0)) {
    lab <- html_esc(display_term(rt))
    re_terms <- c(re_terms, paste0("v<sub>", lab, "</sub>"))
    re_dist <- c(re_dist, paste0("v<sub>", lab, "</sub> ~ N(0, \u03c3<sup>2</sup><sub>", lab, "</sub>): one intercept per level of ", lab))
  }
  # ---- error distribution and link
  resp <- switch(fam, gaussian = sb("y", "ij"), poisson = , zip = paste0("log(", sb("\u03bb", "ij"), ")"),
                 binomial = , betabinomial = paste0("logit(", sb("p", "ij"), ")"), beta = paste0("logit(", sb("\u03bc", "ij"), ")"),
                 paste0("log(", sb("\u03bc", "ij"), ")"))
  fam_dist <- switch(fam,
    gaussian = paste0(sb("\u03b5", "ij"), " ~ N(0, \u03c3<sup>2</sup>)"),
    poisson = paste0(sb("y", "ij"), " ~ Poisson(", sb("\u03bb", "ij"), ")"),
    nbinom2 = paste0(sb("y", "ij"), " ~ NegBin(", sb("\u03bc", "ij"), ", \u03b8), Var(y) = \u03bc + \u03bc<sup>2</sup>/\u03b8"),
    nbinom1 = paste0(sb("y", "ij"), " ~ NegBin(", sb("\u03bc", "ij"), ", \u03c6), Var(y) = \u03bc(1 + \u03c6)"),
    zip = paste0(sb("y", "ij"), " = 0 with probability ", sb("\u03c0", "ij"), ", otherwise ", sb("y", "ij"), " ~ Poisson(", sb("\u03bb", "ij"), ")"),
    zinb = paste0(sb("y", "ij"), " = 0 with probability ", sb("\u03c0", "ij"), ", otherwise ", sb("y", "ij"), " ~ NegBin(", sb("\u03bc", "ij"), ", \u03b8)"),
    zinb1 = paste0(sb("y", "ij"), " = 0 with probability ", sb("\u03c0", "ij"), ", otherwise ", sb("y", "ij"), " ~ NegBin(", sb("\u03bc", "ij"), ", \u03c6), Var(y) = \u03bc(1 + \u03c6)"),
    binomial = paste0(sb("y", "ij"), " ~ Binomial(", sb("n", "ij"), ", ", sb("p", "ij"), "), with ", sb("n", "ij"), " = 1 for binary data and the trials column otherwise"),
    betabinomial = paste0(sb("y", "ij"), " ~ Beta-binomial(", sb("n", "ij"), ", ", sb("p", "ij"), ", \u03c6): binomial with extra variation between observations"),
    gamma = paste0(sb("y", "ij"), " ~ Gamma(mean ", sb("\u03bc", "ij"), ", dispersion \u03c6): strictly positive, variance proportional to \u03bc<sup>2</sup>"),
    lognormal = paste0(sb("y", "ij"), " ~ lognormal(mean ", sb("\u03bc", "ij"), ", dispersion \u03c6): strictly positive and right-skewed"),
    beta = paste0(sb("y", "ij"), " ~ Beta(mean ", sb("\u03bc", "ij"), ", precision \u03c6): continuous values strictly between 0 and 1"),
    paste0(sb("y", "ij"), " ~ ", family_label(fam)))
  disp_on <- !identical(normalise_disp(s$disp), "constant") && fam %in% DISPERSION_FAMILIES && !identical(fn, A3_NONLINEAR)
  disp_line <- NULL
  if (disp_on) {
    db <- if (identical(normalise_disp(s$disp), "age_terms")) switch(fn_math, Quadratic = c("f1", "f2"), Cubic = c("f1", "f2", "f3"), "f1") else "f1"
    disp_line <- paste0("log(", sb("\u03c6", "ij"), ") = ", sb("\u03b4", 0),
                        paste0(vapply(seq_along(db), function(k) paste0(" + ", sb("\u03b4", k), " \u00b7 ", math_part(db[[k]], fn_math, meta)), character(1)), collapse = ""),
                        ": the residual variance (Gaussian) or dispersion changes with age")
  }
  zi_line <- NULL; zi_coef <- NULL
  if (fam %in% ZI_FAMILIES) {
    zv <- tryCatch(all.vars(stats::as.formula(s$zi %||% "~1")), error = function(e) character(0))
    zt <- expand_model_terms(paste(zv, collapse = " + "), factor_levels)
    zi_line <- paste0("logit(", sb("\u03c0", "ij"), ") = ", sb("\u03b3", 0),
                      paste0(vapply(seq_along(zt), function(k) paste0(" + ", sb("\u03b3", k), " \u00b7 ",
                             paste(vapply(seq_along(zt[[k]]$parts), function(q) math_part(zt[[k]]$parts[[q]], fn_math, meta, zt[[k]]$levels[[q]]), character(1)), collapse = " \u00d7 ")),
                             character(1)), collapse = ""))
    zi_coef <- data.frame(Coefficient = c(sb("\u03b3", 0), vapply(seq_along(zt), function(k) sb("\u03b3", k), character(1))),
                          Multiplies = c("1 (zero-inflation intercept)", vapply(zt, function(z) paste(vapply(seq_along(z$parts), function(q) math_part(z$parts[[q]], fn_math, meta, z$levels[[q]]), character(1)), collapse = " \u00d7 "), character(1))),
                          R_term = c("zi~(Intercept)", vapply(zt, function(z) paste0("zi~", z$r_name), character(1))), stringsAsFactors = FALSE)
  }
  if (nonlinear) {
    if (m %in% c("M3", "M5")) return(list(available = FALSE, message = "Not available with the non-linear exponential function (the centring models need a linear-in-parameters age function)."))
    parts <- nonlinear_extra_parts(NONLINEAR_PARTS[[m]], extra[[m]] %||% character(0), covars)
    a_rhs <- paste(c("1", union(c(covars, cov_pairs), parts$a)), collapse = " + ")
    b_rhs <- paste(c("1", union(cov_age, parts$b)), collapse = " + ")
    ta <- expand_model_terms(a_rhs, factor_levels)
    tb <- expand_model_terms(b_rhs, factor_levels)
    lin <- function(sym, tt) paste0(sb(sym, 0), paste0(vapply(seq_along(tt), function(k) paste0(" + ", sb(sym, k), " \u00b7 ",
                    paste(vapply(seq_along(tt[[k]]$parts), function(q) math_part(tt[[k]]$parts[[q]], "Linear", meta, tt[[k]]$levels[[q]]), character(1)), collapse = " \u00d7 ")),
                    character(1)), collapse = ""))
    ua <- c(sb("u", "a,i"), re_terms)
    ub <- if (rs %in% c("correlated", "uncorrelated", "correlated_all", "uncorrelated_all")) sb("u", "b,i") else character(0)
    eq <- c(paste0(sb("y", "ij"), " = ", sb("a", "i"), " \u00b7 exp(", sb("b", "i"), " \u00b7 age<sub>ij</sub>) + ", sb("\u03b5", "ij")),
            paste0(sb("a", "i"), " = ", lin("\u03b1", ta), paste0(" + ", ua, collapse = "")),
            paste0(sb("b", "i"), " = ", lin("\u03b2", tb), if (length(ub)) paste0(" + ", ub) else ""))
    dist <- c(paste0(sb("\u03b5", "ij"), " ~ N(0, \u03c3<sup>2</sup>)"),
              switch(rs,
                correlated = paste0("(", sb("u", "a,i"), ", ", sb("u", "b,i"), ") ~ MVN(0, \u03a3): SDs \u03c3<sub>a</sub>, \u03c3<sub>b</sub> and correlation \u03c1"),
                uncorrelated = c(paste0(sb("u", "a,i"), " ~ N(0, \u03c3<sup>2</sup><sub>a</sub>)"), paste0(sb("u", "b,i"), " ~ N(0, \u03c3<sup>2</sup><sub>b</sub>), independent of ", sb("u", "a,i"))),
                paste0(sb("u", "a,i"), " ~ N(0, \u03c3<sup>2</sup><sub>a</sub>)")),
              re_dist)
    coef <- data.frame(Coefficient = c(sb("\u03b1", 0), vapply(seq_along(ta), function(k) sb("\u03b1", k), character(1)),
                                       sb("\u03b2", 0), vapply(seq_along(tb), function(k) sb("\u03b2", k), character(1))),
                       Multiplies = c("level a: intercept", vapply(ta, function(z) paste("level a:", paste(vapply(seq_along(z$parts), function(q) math_part(z$parts[[q]], "Linear", meta, z$levels[[q]]), character(1)), collapse = " \u00d7 ")), character(1)),
                                      "rate b: intercept", vapply(tb, function(z) paste("rate b:", paste(vapply(seq_along(z$parts), function(q) math_part(z$parts[[q]], "Linear", meta, z$levels[[q]]), character(1)), collapse = " \u00d7 ")), character(1))),
                       R_term = c("a.(Intercept)", vapply(ta, function(z) paste0("a.", z$r_name), character(1)), "b.(Intercept)", vapply(tb, function(z) paste0("b.", z$r_name), character(1))),
                       stringsAsFactors = FALSE)
    rand <- switch(rs, correlated = , correlated_all = "pdSymm(a + b ~ 1)",
                   uncorrelated = , uncorrelated_all = "pdDiag(a + b ~ 1)", "pdSymm(a ~ 1)")
    rhs_or_1 <- function(x) {
      y <- sub("^1 \\+ ", "", x)
      if (!nzchar(y)) "1" else y
    }
    r_call <- paste0("nlme::nlme(trait ~ a * exp(b * f1), data = dat,\n           fixed = list(a ~ ", rhs_or_1(a_rhs), ", b ~ ", rhs_or_1(b_rhs), "),\n           random = ",
                     if (isTRUE(meta$has_group2)) paste0("list(group2 = pdSymm(a ~ 1), group = pdSymm(a ~ 1), id = ", rand, ")") else
                       if (isTRUE(meta$has_group)) paste0("list(group = pdSymm(a ~ 1), id = ", rand, ")") else rand,
                     ", groups = ", if (isTRUE(meta$has_group2)) "~ group2/group/id" else if (isTRUE(meta$has_group)) "~ group/id" else "~ id",
                     ", method = \"ML\", start = <starting values>)")
    return(list(available = TRUE, equation = eq, distributions = c(dist, if (nzchar(rs_note)) rs_note), coefficients = coef,
                basis = "age on the scale set by 'Standardise age and proxies' (standardised by default); a (level) and b (rate) are estimated, so the curve is not linear in its coefficients",
                r_compact = paste0("trait ~ a * exp(b * f1); a ~ ", rhs_or_1(a_rhs), "; b ~ ", rhs_or_1(b_rhs)), r_expanded = NULL, r_call = r_call))
  }
  b <- switch(fn_math, Quadratic = c("f1", "f2"), Cubic = c("f1", "f2", "f3"), "f1")
  basis_note <- paste0(switch(fn_math,
                              Logarithmic = "f1 = log(age)",
                              "Asymptotic exponential" = "f1 = exp(\u2212age)",
                              Quadratic = "f1 = age, f2 = age\u00b2",
                              Cubic = "f1 = age, f2 = age\u00b2, f3 = age\u00b3",
                              "f1 = age"),
                       if (isTRUE(s$standardise)) ", centred and scaled across the sampled ages" else "")
  among <- s$among %||% "linear"
  fixed <- apply_extra_terms(model_formula_strings(b, covars, among, cov_age, cov_pairs), b, extra, among)[[m]]
  tt <- expand_model_terms(fixed, factor_levels)
  random_str <- random_term_string(b, rs, isTRUE(meta$has_group), meta$random_terms %||% character(0), has_group2 = isTRUE(meta$has_group2))
  id_re <- switch(rs, correlated = , uncorrelated = c(sb("u", "0i"), paste0(sb("u", "1i"), " \u00b7 ", slope_var)),
                  correlated_all = , uncorrelated_all = c(sb("u", "0i"),
                    vapply(seq_along(b), function(j) paste0(sb("u", paste0(j, "i")), " \u00b7 ", b[[j]]), character(1))),
                  sb("u", "0i"))
  terms_math <- vapply(tt, function(z) paste(vapply(seq_along(z$parts), function(q) math_part(z$parts[[q]], fn_math, meta, z$levels[[q]]), character(1)), collapse = " \u00d7 "), character(1))
  eq <- paste0(resp, " = ", bta(0), paste0(vapply(seq_along(tt), function(k) paste0(" + ", bta(k), " \u00b7 ", terms_math[[k]]), character(1)), collapse = ""),
               paste0(" + ", c(id_re, re_terms), collapse = ""), if (identical(fam, "gaussian")) paste0(" + ", sb("\u03b5", "ij")) else "")
  id_dist <- switch(rs,
    correlated = paste0("(", sb("u", "0i"), ", ", sb("u", "1i"), ") ~ MVN(0, \u03a3): SDs \u03c3<sub>u0</sub>, \u03c3<sub>u1</sub> and correlation \u03c1"),
    uncorrelated = c(paste0(sb("u", "0i"), " ~ N(0, \u03c3<sup>2</sup><sub>u0</sub>)"), paste0(sb("u", "1i"), " ~ N(0, \u03c3<sup>2</sup><sub>u1</sub>), independent of ", sb("u", "0i"))),
    paste0(sb("u", "0i"), " ~ N(0, \u03c3<sup>2</sup><sub>u0</sub>)"))
  coef <- data.frame(Coefficient = c(bta(0), vapply(seq_along(tt), function(k) bta(k), character(1))),
                     Multiplies = c("1 (intercept: all standardised variables at their centre, categorical covariates at their reference level)", terms_math),
                     R_term = c("(Intercept)", vapply(tt, function(z) z$r_name, character(1))), stringsAsFactors = FALSE)
  if (!is.null(zi_coef)) coef <- rbind(coef, zi_coef)
  tl <- vapply(tt, function(z) z$term, character(1))
  r_expanded <- paste0("trait ~ 1", paste0(" + ", unique(tl), collapse = ""), " + ", random_str)
  fam_call <- family_r_call(fam)
  r_call <- if (identical(fam, "gaussian") && !disp_on) {
    paste0("lme4::lmer(", r_expanded, ",\n           data = dat, REML = FALSE)")
  } else {
    paste0("glmmTMB::glmmTMB(", r_expanded, ",\n                 data = dat, family = ", fam_call, ", ziformula = ",
           if (fam %in% ZI_FAMILIES) (s$zi %||% "~1") else "~0",
           if (fam %in% BINOMIAL_FAMILIES && isTRUE(meta$has_trials)) ", weights = .trials" else "",
           if (disp_on) paste0(", dispformula = ", disp_formula_string(s$disp, b)) else "", ", REML = FALSE)")
  }
  if (disp_on && identical(fam, "gaussian")) fam_dist <- paste0(sb("\u03b5", "ij"), " ~ N(0, ", sb("\u03c3", "ij"), "<sup>2</sup>), with the variance changing with age")
  list(available = TRUE, equation = c(eq, zi_line, disp_line), distributions = c(if (identical(fam, "gaussian")) fam_dist else paste(fam_dist, "with the linear predictor above"),
                                                                     id_dist, re_dist, if (nzchar(rs_note)) rs_note),
       coefficients = coef, basis = basis_note, r_compact = paste("trait ~", fixed, "+", random_str), r_expanded = r_expanded, r_call = r_call)
}

# ---------------------------------------------------------------------------
# Non-linear exponential mixed models (Gaussian): trait = a * exp(b * z_age), fitted with nlme by ML
# ---------------------------------------------------------------------------
# The level a and the rate b have their own linear predictors, so selective disappearance can act on the
# level (age-independent: a ~ ALR) or on the rate (age-dependent: b ~ ALR). Individuals have random levels
# (and optionally random rates). Age is always standardised (z_age) for numerical stability. For count
# families a * exp(b * age) is linear in age on the log link, so the Linear function is fitted instead.
NONLINEAR_PARTS <- list(
  M1 = list(a = character(0), b = character(0)),
  M2 = list(a = "ALR", b = character(0)),
  M4 = list(a = "ALR", b = "ALR"),
  M6 = list(a = "LS", b = "LS"),
  M7 = list(a = c("ALR", "AFR"), b = character(0)),
  M8 = list(a = c("ALR", "AFR"), b = c("ALR", "AFR")),
  M9 = list(a = c("ALR", "AFR"), b = "ALR"),
  M10 = list(a = c("ALR", "AFR"), b = "AFR")
)

# Adds extra-term keys to the level (a) and rate (b) parts of a non-linear model. Terms without age act on the
# level; terms that interact with age act on the level and the rate (as ALR x age does). Mean age is not used.
nonlinear_extra_parts <- function(p, keys, covars = character(0)) {
  for (k in keys %||% character(0)) {
    if (k %in% c("ALR", "AFR", "LS")) {
      p$a <- union(p$a, k)
    } else if (k %in% c("ALR_x_age", "AFR_x_age", "LS_x_age")) {
      v <- sub("_x_age$", "", k)
      p$a <- union(p$a, v)
      p$b <- union(p$b, v)
    } else if (grepl(":age$", k) && sub(":age$", "", k) %in% covars) {
      p$b <- union(p$b, sub(":age$", "", k))
    } else {
      for (cp in parse_built_term(k) %||% list()) {
        rest <- setdiff(cp, c("age", "mean_age"))
        if (!length(rest)) next
        lab <- attr(stats::terms(stats::as.formula(paste("~", paste(rest, collapse = " * ")))), "term.labels")
        p$a <- union(p$a, lab)
        if ("age" %in% cp) p$b <- union(p$b, lab)
      }
    }
  }
  p
}

# ---- Moved unchanged from inst/app/server.R (0.9.11): pure helpers that use no reactive state ----

extra_term_choices <- function(m) {
  covs <- if (is.null(m) || !length(m$covars)) character(0) else {
    labs <- unname(m$cov_labels[m$covars])
    c(stats::setNames(m$covars, paste0(labs, " (additive)")), stats::setNames(paste0(m$covars, ":age"), paste0(labs, " \u00d7 age")))
  }
  c(EXTRA_TERM_KEYS, covs)
}

builder_tokens <- function(m) {
  covs <- if (is.null(m) || !length(m$covars)) character(0) else stats::setNames(m$covars, unname(m$cov_labels[m$covars]))
  lk <- isTRUE(m$has_life) && !isTRUE(m$life_auto)
  c("Age (the ageing terms)" = "age", "ALR (age at last record)" = "ALR", "AFR (age at first observation)" = "AFR",
    if (lk) c("LS (known lifespan)" = "LS"), "Mean age of the individual" = "mean_age", covs)
}
