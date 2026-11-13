# disappR engine - Teaching simulations with known answers: simulated ageing trajectories, selection, sampling and the true curves.
# Engine code shared by the app and the R interface; changes that alter results must pass the golden tests.

# ---------------------------------------------------------------------------
# Teaching simulator
# ---------------------------------------------------------------------------
# Individual ageing trajectories F_i(age) = sum_k b_ki g_k(u), with u = age / (mean lifespan / 20) so
# that the trajectory shape is the same for 3 or 30 sampling occasions, and g_k the basis of the
# chosen form (linear, quadratic, cubic, logarithmic, exponential; the same bases as the models).
# Each individual's coefficients = population means
#   + an individual component (heterogeneity in level and ageing rate),
#   + a lifespan-linked component (selective disappearance): on the intercept for age-independent,
#     on the first ageing coefficient ("rate") for age-dependent selective disappearance,
#   + an AFR-linked component (selective appearance), defined in the same way.
# The lifespan and AFR latent variables are independent, so AFR and ALR are uncorrelated by
# construction; with individual-specific AFR, individuals are included only if they outlive the
# latest possible AFR, which depends on lifespan alone. Count traits use a log link.
# "paper" reproduces the manuscript's multivariate-normal quadratic parameterisation exactly.
TOY_TRAITS <- c(
  "Body mass \u2014 continuous (Gaussian)" = "mass",
  "Fecundity \u2014 counts (Poisson)" = "count_pois",
  "Fecundity \u2014 overdispersed counts (negative binomial)" = "count_nb",
  "Fecundity \u2014 zero-inflated overdispersed counts (ZINB)" = "count_zinb",
  "Paper simulation \u2014 continuous fecundity (Methods Eq. 1\u20134)" = "paper"
)

SELECTION_TYPES <- c("None" = "none", "Age-independent" = "independent", "Age-dependent" = "dependent",
                     "Age-independent and age-dependent" = "both")

TOY_DEFAULTS <- list(trait = "mass", form = "Quadratic", shape = "default", strength = "dramatic", sd_type = "both", sd_dir = 1,
                     rate_var = "low", mean_ls = 20, missingness = "complete",
                     afr_mode = "same", sa_type = "none", sa_dir = 1, diet = FALSE, groups = FALSE,
                     n_id = 300, seed = 42)

# Parameters of each simulated form on the scaled time u. mu: population coefficients (intercept
# first); s_rate: lifespan- or AFR-linked SD of the first ageing coefficient at dramatic strength;
# u_small / u_large: individual SDs of the ageing coefficients (the intercept SD is fixed).
# The small values were tuned so that, without selective disappearance, random-intercept models
# usually do not favour the interaction models (verified by simulation for every form).
toy_form_spec <- function(form, count = FALSE) {
  if (!isTRUE(count)) {
    switch(form,
      Linear      = list(mu = c(50, -0.8), s_rate = 0.425, u_small = 0.04, u_large = 0.5),
      Cubic       = list(mu = c(40, 1.2, 0.06, -0.0035), s_rate = 0.425,
                         u_small = c(0.04, 0.0012, 0.00003), u_large = c(0.5, 0.015, 0.0005)),
      Logarithmic = list(mu = c(30, 8), s_rate = 2.8, u_small = 0.27, u_large = 3.3),
      "Asymptotic exponential" = list(mu = c(28, 6), s_rate = 2.0, u_small = 0.17, u_large = 2.3),
      list(mu = c(40, 2, -0.06), s_rate = 0.425, u_small = c(0.04, 0.0012), u_large = c(0.5, 0.015)))
  } else {
    switch(form,
      Linear      = list(mu = c(log(40), -0.06), s_rate = 0.0255, u_small = 0.003, u_large = 0.03),
      Cubic       = list(mu = c(log(20), 0.08, 0.004, -0.0003), s_rate = 0.0255,
                         u_small = c(0.003, 0.00015, 0.000005), u_large = c(0.03, 0.0015, 0.00005)),
      Logarithmic = list(mu = c(log(10), 0.5), s_rate = 0.17, u_small = 0.02, u_large = 0.2),
      "Asymptotic exponential" = list(mu = c(log(12), 0.35), s_rate = 0.12, u_small = 0.012, u_large = 0.14),
      list(mu = c(log(30), 0.12, -0.007), s_rate = 0.0255, u_small = c(0.003, 0.00015), u_large = c(0.03, 0.0015)))
  }
}

# Basis of a simulated form (without intercept). The exponential basis is exp(-(u - m) / s) (a decline that is
# steep early in life and flattens later, with a positive coefficient), with m and s
# the mean and SD of all expected (pre-missingness) scaled ages, mirroring the app's asymptotic exponential function.
toy_form_basis <- function(form, u, ref = c(0, 1)) {
  switch(form,
    Linear = cbind(u),
    Cubic = cbind(u, u^2, u^3),
    Logarithmic = cbind(log(pmax(u, 1e-9))),
    "Asymptotic exponential" = cbind(exp(-(u - ref[1]) / ref[2])),
    cbind(u, u^2))
}

# Biologically motivated shapes within each simulated ageing form: the population mean coefficients (intercept
# first) on the scaled time u (u = 20 at the mean lifespan), for continuous traits (mu) and on the log scale for
# counts (mu_count). "default" is the form's original shape (toy_form_spec()), so default simulations are
# unchanged. Shapes change the mean coefficients only: the lifespan- and AFR-linked components (selection) and the
# individual variation stay the same, so selective disappearance and appearance keep their strength. Continuous
# shapes stay positive to about 1.75 times the mean lifespan.
TOY_SHAPES <- list(
  Linear = list(
    default = list(label = "Moderate senescence (steady decline)"),
    slow = list(label = "Slow senescence", mu = c(50, -0.4), mu_count = c(log(40), -0.03)),
    fast = list(label = "Fast senescence", mu = c(50, -1.25), mu_count = c(log(40), -0.1)),
    none = list(label = "No senescence (flat)", mu = c(50, 0), mu_count = c(log(40), 0)),
    improve = list(label = "Improvement with age (e.g. growth or experience)", mu = c(50, 0.5), mu_count = c(log(40), 0.03))),
  Quadratic = list(
    default = list(label = "Improvement, then senescence"),
    early = list(label = "Early peak, then fast senescence", mu = c(40, 1.3, -0.065), mu_count = c(log(30), 0.1, -0.008)),
    late = list(label = "Late peak, then slow senescence", mu = c(40, 1.6, -0.04), mu_count = c(log(30), 0.112, -0.004)),
    ushape = list(label = "Decline, then improvement (U-shape)", mu = c(50, -1.4, 0.04), mu_count = c(log(40), -0.1, 0.0028))),
  Cubic = list(
    default = list(label = "Improvement, plateau, then accelerating late-life decline"),
    twophase = list(label = "Early decline, mid-life plateau, then late-life decline",
                    mu = c(45, -0.864, 0.072, -0.002), mu_count = c(log(40), -0.0864, 0.0072, -0.0002)),
    terminal = list(label = "Stable, then late-life increase (e.g. terminal investment)",
                    mu = c(40, 0.45, -0.045, 0.0015), mu_count = c(log(20), 0.03, -0.003, 0.0001))),
  Logarithmic = list(
    default = list(label = "Rapid early improvement, then levelling off"),
    gradual_up = list(label = "Gradual improvement, then levelling off", mu = c(40, 4), mu_count = c(log(20), 0.25)),
    rapid_down = list(label = "Rapid early decline, then slower decline", mu = c(55, -8), mu_count = c(log(40), -0.5)),
    gradual_down = list(label = "Gradual decline, then levelling off", mu = c(50, -4), mu_count = c(log(30), -0.25))),
  "Asymptotic exponential" = list(
    default = list(label = "Rapid early decline, then plateau"),
    gradual_down = list(label = "Gradual decline, then plateau", mu = c(34, 3), mu_count = c(log(14), 0.18)),
    rapid_up = list(label = "Rapid early improvement, then plateau (growth to an asymptote)", mu = c(55, -6), mu_count = c(log(30), -0.35)),
    gradual_up = list(label = "Gradual improvement, then plateau", mu = c(47, -3), mu_count = c(log(24), -0.18)))
)

toy_shape_choices <- function(form) {
  sh <- TOY_SHAPES[[form %||% "Quadratic"]] %||% TOY_SHAPES[["Quadratic"]]
  stats::setNames(names(sh), vapply(names(sh), function(k) paste0(sh[[k]]$label, if (identical(k, "default")) " (default)" else ""), character(1)))
}

# Mean coefficients of a non-default shape; NULL for the default (or an unknown) shape.
toy_shape_mu <- function(form, shape, count = FALSE) {
  shape <- shape %||% "default"
  sh <- TOY_SHAPES[[form %||% ""]][[shape]]
  if (is.null(sh) || identical(shape, "default")) return(NULL)
  if (isTRUE(count)) sh$mu_count else sh$mu
}

toy_shape_label <- function(form, shape) {
  sh <- TOY_SHAPES[[form %||% ""]]
  sh[[shape %||% "default"]]$label %||% sh[["default"]]$label %||% ""
}

simulate_toy_data <- function(cfg = list()) {
  cfg <- utils::modifyList(TOY_DEFAULTS, cfg)
  for (nm in c("sd_dir", "sa_dir", "mean_ls", "n_id", "seed")) cfg[[nm]] <- as.numeric(cfg[[nm]])
  # use the configured seed without leaving R's random-number stream changed for other code or other users
  old_seed <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
    } else assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)
  set.seed(as.integer(cfg$seed))
  type <- cfg$trait
  paper <- identical(type, "paper")
  is_count <- grepl("^count", type)
  form <- if (paper || !isTRUE(cfg$form %in% AGE_FUNCTIONS)) "Quadratic" else cfg$form
  dramatic <- identical(cfg$strength, "dramatic") && !paper
  n <- max(30L, as.integer(cfg$n_id))
  mean_ls <- if (paper) 25 else min(30, max(3, round(cfg$mean_ls)))
  cc <- mean_ls / 20
  individual_afr <- identical(cfg$afr_mode, "individual")
  sd_level <- cfg$sd_type %in% c("independent", "both")
  sd_rate <- cfg$sd_type %in% c("dependent", "both")
  sa_level <- individual_afr && cfg$sa_type %in% c("independent", "both")
  sa_rate <- individual_afr && cfg$sa_type %in% c("dependent", "both")
  sd_dir <- if (isTRUE(cfg$sd_dir < 0)) -1 else 1
  sa_dir <- if (isTRUE(cfg$sa_dir < 0)) -1 else 1

  z_ls <- stats::rnorm(n)
  z_afr <- stats::rnorm(n)
  if (paper) {
    mu_b <- c(600, 9, -0.3)
    sd_b <- abs(mu_b) * 0.2
    rho <- c(if (sd_level) 0.5 * sd_dir else 0, if (sd_rate) 0.5 * sd_dir else 0, 0)
    E <- matrix(stats::rnorm(n * 3), n, 3)
    B <- sapply(1:3, function(j) mu_b[j] + sd_b[j] * (rho[j] * z_ls + sqrt(1 - rho[j]^2) * E[, j]))
    B[, 1] <- B[, 1] + (if (sa_level) sa_dir * 0.5 * sd_b[1] else 0) * z_afr
    B[, 2] <- B[, 2] + (if (sa_rate) sa_dir * 0.5 * sd_b[2] else 0) * z_afr
    LS_raw <- 25 + 5 * z_ls
    sigma_e <- 5
    shift_diet <- -60
    u_sd <- 40
  } else {
    sp <- toy_form_spec(form, is_count)
    # biologically motivated shape within the form (population mean coefficients only; default = toy_form_spec)
    shape_mu <- toy_shape_mu(form, cfg$shape, is_count)
    if (length(shape_mu) == length(sp$mu)) sp$mu <- shape_mu
    k_str <- if (dramatic) 1 else 0.35
    s_level <- if (is_count) 0.2125 else 5.1
    np <- length(sp$mu)
    s_U <- c(if (is_count) 0.25 else 4, if (identical(cfg$rate_var, "high")) sp$u_large else sp$u_small)
    E <- matrix(stats::rnorm(n * np), n, np)
    B <- matrix(sp$mu, n, np, byrow = TRUE) + sweep(E, 2, s_U, "*")
    B[, 1] <- B[, 1] + k_str * s_level * ((if (sd_level) sd_dir * z_ls else 0) + (if (sa_level) sa_dir * z_afr else 0))
    B[, 2] <- B[, 2] + k_str * sp$s_rate * ((if (sd_rate) sd_dir * z_ls else 0) + (if (sa_rate) sa_dir * z_afr else 0))
    mu_b <- sp$mu
    LS_raw <- mean_ls + 0.3 * mean_ls * z_ls
    sigma_e <- 1.5
    shift_diet <- if (is_count) -0.3 else -5
    u_sd <- if (is_count) 0.25 else 4
  }

  diet <- if (isTRUE(cfg$diet)) stats::rbinom(n, 1, 0.5) else rep(0, n)
  n_groups <- 25L
  grp <- if (isTRUE(cfg$groups)) sample(seq_len(n_groups), n, replace = TRUE) else rep(1L, n)
  u_tr <- if (isTRUE(cfg$groups)) stats::rnorm(n_groups, 0, u_sd)[grp] else rep(0, n)
  B[, 1] <- B[, 1] + shift_diet * diet + u_tr
  LS <- pmax(if (paper) 1 else 2, round(LS_raw))

  if (individual_afr) {
    span <- max(2, round(0.4 * mean_ls))
    AFR <- 1 + floor(stats::pnorm(0.9 * z_afr + sqrt(1 - 0.81) * stats::rnorm(n)) * span)
    alive <- which(LS >= span)            # inclusion depends on lifespan only, not on AFR
  } else {
    AFR <- rep(1, n)
    alive <- seq_len(n)
  }
  cond <- as.numeric(scale(0.7 * z_ls + sqrt(1 - 0.49) * stats::rnorm(n)))

  n_age <- as.integer(LS[alive] - AFR[alive] + 1)
  idx <- rep(alive, n_age)
  age <- AFR[idx] + sequence(n_age) - 1
  u <- age / cc
  ref <- c(mean(u), stats::sd(u))
  if (!is.finite(ref[2]) || ref[2] <= 0) ref[2] <- 1
  X <- if (paper) cbind(1, age, age^2) else cbind(1, toy_form_basis(form, u, ref))
  eta <- rowSums(B[idx, , drop = FALSE] * X)
  m <- length(age)
  trait <- switch(type,
    paper = pmax(0, eta) + stats::rnorm(m, 0, sigma_e),
    mass = pmax(eta + stats::rnorm(m, 0, sigma_e), 0.02 * mean(eta)),
    count_pois = stats::rpois(m, exp(pmin(eta, log(2000)))),
    count_nb = stats::rnbinom(m, size = 3, mu = exp(pmin(eta, log(2000)))),
    count_zinb = ifelse(stats::runif(m) < 0.25, 0, stats::rnbinom(m, size = 3, mu = exp(pmin(eta, log(2000)))))
  )

  s <- if (dramatic || paper) 1 else 0.6
  zt <- as.numeric(scale(if (is_count) log1p(trait) else trait))
  keep_p <- switch(cfg$missingness,
    complete = rep(1, m),
    mcar = rep(if (paper) 0.5 else 0.75, m),
    mwo = if (paper) 1 / (1 + exp(0.25 * (age - 40))) else 1 / (1 + exp(0.5 * s / cc * (age - 0.7 * mean_ls))),
    mwy = if (paper) 1 - 1 / (1 + exp(0.25 * (age - 10))) else 1 - 1 / (1 + exp(0.5 * s / cc * (age - 0.35 * mean_ls))),
    trait = stats::plogis(1.6 + 2 * s * zt),
    condition = stats::plogis(1.6 + 2 * s * cond[idx]),
    rep(1, m)
  )
  keep <- stats::runif(m) < keep_p

  trait_col <- if (identical(type, "mass")) "body_mass" else "fecundity"
  out <- data.frame(ID = sprintf("ID%04d", idx), age = age, trait = trait, lifespan = LS[idx],
                    condition = round(cond[idx], 3), stringsAsFactors = FALSE)
  names(out)[3] <- trait_col
  if (individual_afr) out$AFR <- AFR[idx]
  if (isTRUE(cfg$diet)) out$diet <- ifelse(diet[idx] == 1, "Poor", "Standard")
  if (isTRUE(cfg$groups)) out$family <- sprintf("F%02d", grp[idx])
  out <- out[keep, , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "truth") <- list(type = type, form = form, paper = paper, mu_b = mu_b, cc = cc, ref = ref,
                             shift_T = shift_diet, mean_T = mean(diet[alive]),
                             mean_expT = mean(exp(shift_diet * diet[alive])),
                             zi = if (identical(type, "count_zinb")) 0.25 else 0,
                             shape = if (paper) "default" else (cfg$shape %||% "default"), cfg = cfg)
  out
}

# Typical-individual trajectory implied by the simulation (population mean coefficients),
# marginalised over the diet covariate in the same way as the app's model predictions.
toy_true_curve <- function(truth, age) {
  X <- if (isTRUE(truth$paper) || is.null(truth$form)) cbind(1, age, age^2) else cbind(1, toy_form_basis(truth$form, age / truth$cc, truth$ref))
  base <- as.numeric(X %*% truth$mu_b)
  if (truth$type %in% c("paper", "mass")) {
    base + truth$shift_T * truth$mean_T
  } else {
    (1 - truth$zi) * exp(base) * truth$mean_expT
  }
}

toy_mapping <- function(df) {
  cols <- names(df)
  afr_ind <- "AFR" %in% cols
  list(id = "ID", age = "age", trait = if ("body_mass" %in% cols) "body_mass" else "fecundity",
       # AFR is the observed one (the first observed record), like ALR (the last); the true AFR column stays in the
       # data for comparison (0.24.2)
       alr = "__AUTO_LAST__", life = "lifespan", entry = "__AUTO_FIRST__",
       condition = "condition", covars = intersect("diet", cols), cov_factor = intersect("diet", cols),
       group = if ("family" %in% cols) "family" else "", nested = TRUE, random = character(0),
       censor = "", censor_value = "", start_mode = if (afr_ind) "afr" else "same", start_age = 1, age_round = NA_real_,
       cov_age = character(0), cov_int = character(0), trait_specific_ages = TRUE)
}

toy_family <- function(type) {
  switch(type, mass = "gaussian", paper = "gaussian", count_pois = "poisson", count_nb = "nbinom2",
         count_zinb = "zinb", "gaussian")
}

# Expectations verified by simulation (random-intercept models, several seeds per scenario).
toy_truth_text <- function(cfg) {
  cfg <- utils::modifyList(TOY_DEFAULTS, cfg)
  cfg$mean_ls <- as.numeric(cfg$mean_ls)
  paper <- identical(cfg$trait, "paper")
  form <- if (paper) "Quadratic" else cfg$form
  trait <- names(TOY_TRAITS)[TOY_TRAITS == cfg$trait]
  mag <- if (paper) "|\u03c1| = 0.5 as in the Methods" else if (identical(cfg$strength, "dramatic")) "dramatic strength" else "moderate strength"
  L <- if (paper) 25 else cfg$mean_ls
  dir_txt <- function(d) if (isTRUE(as.numeric(d) < 0)) "negative" else "positive"
  sel_lab <- function(x) tolower(names(SELECTION_TYPES)[SELECTION_TYPES == x])
  individual_afr <- identical(cfg$afr_mode, "individual")
  sim <- paste0("Simulated: ", trait, "; ", tolower(form), " ageing",
                if (!paper) paste0(" (", tolower(toy_shape_label(form, cfg$shape)), ")") else "",
                "; selective disappearance: ", sel_lab(cfg$sd_type),
                if (cfg$sd_type != "none") paste0(" (", dir_txt(cfg$sd_dir), ")") else "",
                "; AFR ", if (individual_afr) paste0("individual-specific, selective appearance: ", sel_lab(cfg$sa_type),
                                                     if (cfg$sa_type != "none") paste0(" (", dir_txt(cfg$sa_dir), ")") else "")
                else "the same for everyone",
                "; ", mag, "; mean lifespan ", L, " occasions",
                if (!paper) paste0("; ", if (identical(cfg$rate_var, "high")) "large" else "small", " individual differences in ageing rates") else "",
                ".")
  expect <- character(0)
  if (cfg$sd_type %in% c("dependent", "both")) {
    expect <- c(expect,
      "Trajectory plot: lifespan (ALR/LS) bins diverge with age, so bin differences and their trend lines change with age.",
      "Lifespan plot: the LS\u2013trait regression coefficient changes across age bins (see the coefficient table).",
      "Models 4, 5 and 6 are expected to have much lower AICs than Models 1\u20133; the decomposition drifts from the truth at late ages.")
  } else if (identical(cfg$sd_type, "independent")) {
    expect <- c(expect,
      "Trajectory plot: bins are offset but parallel, so bin differences are similar at every age (flat trend lines away from zero).",
      "Lifespan plot: similar non-zero LS\u2013trait coefficients in every age bin.",
      "Model 2 is expected to have a much lower AIC than Model 1; the interaction models add little.")
  } else {
    expect <- c(expect,
      "Trajectory plot: bins overlap and their differences scatter around zero; lifespan plot: coefficients near zero in every age bin.",
      "")
  }
  expect <- expect[nzchar(expect)]
  flat <- !paper && identical(form, "Linear") && identical(cfg$shape, "none")
  expect <- c(expect, if (flat) {
    "No senescence: the simulated typical trajectory is flat, so any change of the observed means with age comes from selective disappearance (or appearance). A flat trajectory is a special case of every ageing function, so the ageing-function comparison need not single out one of them."
  } else paste0("The population-level ageing-function comparison is expected to favour the ",
    form, " function", if (identical(form, "Asymptotic exponential")) " (approximately: the exponential basis is scaled by the SD of the sampled ages, which changes under age-dependent missingness)" else "",
    "."))
  if (!paper && !identical(cfg$shape %||% "default", "default")) {
    expect <- expect
  }
  miss <- switch(cfg$missingness,
    complete = "Complete sampling.",
    mcar = paste0("MCAR (", if (paper) "retention 0.5, as in the Methods" else "about 75% of occasions sampled", "): with age-dependent selection, Model 4 is expected to have a lower AIC than Model 5, as ALR is the less error-prone lifespan proxy."),
    mwo = "Missing when old: late ages are thinly sampled and ALR underestimates lifespan; Model 6 (true LS) is expected to have much the lowest AIC, and Model 4 a lower AIC than Model 5.",
    mwy = "Missing when young: first records are delayed, so AFR (first record) varies; with age-dependent selection Model 4 is expected to have a lower AIC than Model 5.",
    trait = "Trait-dependent missingness (low values more often missing): step 3 (Missingness and proxies) should flag the prior-trait association, and observed means are inflated.",
    condition = "Condition-dependent missingness (condition covaries with lifespan): step 3 (Missingness and proxies) should flag condition and LS.")
  expect <- c(expect, miss)
  if (individual_afr) {
    expect <- c(expect, switch(cfg$sa_type,
      none = "AFR varies at random: AFR bins overlap in the trajectory plot (selective appearance question) and Models 7\u20138 are not expected to improve on Models 2 and 4 (within about 2 AIC).",
      independent = "Age-independent selective appearance: AFR bins are offset but parallel in the trajectory plot, and Models 7 and 8 are expected to have clearly lower AICs than Models 2 and 4.",
      dependent = "Age-dependent selective appearance: AFR bins diverge with age in the trajectory plot, and Model 8 is expected to have a clearly lower AIC than Model 4.",
      "Age-independent and age-dependent selective appearance: AFR bins are offset and diverge with age, and Models 7 and 8 are expected to have clearly lower AICs than Models 2 and 4."),
      "AFR is independent of lifespan by construction, so AFR and ALR are expected to be nearly uncorrelated.")
    if (L < 8) expect <- c(expect, "With short lifespans and individual-specific AFR, individuals that die before the latest possible AFR are not sampled at all.")
  }
  if (isTRUE(cfg$diet)) expect <- c(expect, "Diet: the 'Poor' diet lowers the trait at all ages but does not affect lifespan. It is mapped as a covariate (and can be used to split the lifespan-group figures into panels); removing it adds unexplained variation but should not create selective-disappearance signals.")
  if (isTRUE(cfg$groups)) expect <- c(expect, "Families: 25 families share trait effects, mapped as a nested random effect (1 | family) + (1 | family:ID).")
  if (grepl("^count", cfg$trait)) expect <- c(expect, "Counts: use the matching family (Poisson / negative binomial / ZINB); compare with Gaussian to see how rankings can change.")
  if (L < 6) expect <- c(expect, "Few occasions per individual: individual fits are very imprecise and interaction models have little within-individual information.")
  list(sim = sim, expect = expect)
}

# Relativised deviation from the simulated truth (Methods Eq. 11), restricted to ages
# with at least min_n observed individuals and a non-negligible true value.
deviation_from_truth <- function(curves, truth, obs_n, min_n = 10) {
  if (!nrow(curves) || !nrow(obs_n)) return(data.frame())
  ok_ages <- obs_n$age[obs_n$n >= min_n]
  if (!length(ok_ages)) return(data.frame())
  z <- curves[curves$age >= min(ok_ages) & curves$age <= max(ok_ages), , drop = FALSE]
  if (!nrow(z)) return(data.frame())
  z$true <- toy_true_curve(truth, z$age)
  z <- z[abs(z$true) > 0.05 * max(abs(z$true)), , drop = FALSE]
  if (!nrow(z)) return(data.frame())
  z$D <- 100 * (z$fitted - z$true) / z$true
  out <- do.call(rbind, lapply(split(z, z$Method), function(x) {
    x <- x[order(x$age), , drop = FALSE]
    data.frame(Method = x$Method[[1]], Mean_abs_D = mean(abs(x$D)), Max_abs_D = max(abs(x$D)),
               D_at_oldest_age = x$D[nrow(x)], Oldest_age = x$age[nrow(x)], stringsAsFactors = FALSE)
  }))
  out[order(out$Mean_abs_D), , drop = FALSE]
}
