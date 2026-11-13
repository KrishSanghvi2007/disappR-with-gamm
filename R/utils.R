# disappR engine - Shared helpers and constants: operators, formatting, labels, colours and small numerical utilities.
# Engine code shared by the app and the R interface; changes that alter results must pass the golden tests.

`%||%` <- function(a, b) if (is.null(a)) b else a

AGE_FUNCTIONS <- c("Linear", "Quadratic", "Cubic", "Logarithmic", "Asymptotic exponential")

# A3 only: a function that is non-linear in its parameters, a * exp(b * age), fitted by nls per individual
A3_NONLINEAR <- "Exponential (a \u00b7 exp(b \u00b7 age))"

A3_FUNCTIONS <- c(AGE_FUNCTIONS, A3_NONLINEAR)

# Colour-blind-safe, clearly distinct colours for ageing functions (Okabe-Ito)
FUNCTION_COLOURS <- stats::setNames(c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9"), A3_FUNCTIONS)

MODEL_IDS <- paste0("M", 1:10)

model_label <- function(m) sub("^M", "Model ", m)

# Colours follow the manuscript figures (Models 1-6) where possible.
METHOD_COLOURS <- c(
  "Model 1" = "darkgrey", "Model 2" = "hotpink1", "Model 3" = "darkgreen",
  "Model 4" = "orange", "Model 5" = "steelblue2", "Model 6" = "red",
  "Model 7" = "#8C510A", "Model 8" = "#01665E", "Model 9" = "#762A83", "Model 10" = "#1B7837",
  "Observed" = "grey35", "Decomposition" = "purple", "True (simulated)" = "black",
  "Individual fits: mean of coefficients" = "#E7298A", "Individual fits: mean of functions" = "#66A61E"
)

# Clearly distinguishable colours: Okabe-Ito for up to 7 levels, then an HCL qualitative palette.
distinct_colours <- function(n) {
  base <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#000000")
  if (n <= length(base)) base[seq_len(n)] else grDevices::hcl.colors(n, "Dark 3")
}

# Ordered but clearly distinguishable colours (for ordered bins): the viridis HCL palette without its palest end.
ordered_colours <- function(n) {
  n <- max(1L, as.integer(n))
  grDevices::hcl.colors(n + 1, "Viridis")[seq_len(n)]
}

warm_palette <- c("#A85F4A", "#C8944C", "#7C8060", "#B77C75", "#6D5A4A", "#D4AA7D")

bin_palette <- c("#3B4CC0", "#7B9FF9", "#C0D4F5", "#F2CBB7", "#EE8468", "#B40426")

theme_disappR <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#FBF7F1", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#FBF7F1", colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#E7DDD0", linewidth = 0.35),
      axis.title = ggplot2::element_text(colour = "#4B4037", face = "bold"),
      axis.text = ggplot2::element_text(colour = "#62564C"),
      # In-plot titles, subtitles and captions are suppressed: every figure sits under a box heading that
      # poses its question, and its info panel explains it, so text inside the plot only repeats them.
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      plot.caption = ggplot2::element_blank(),
      legend.position = "top",
      # Legends are the one place a reader decodes the figure, so they are larger and heavier than axis text.
      legend.title = ggplot2::element_text(face = "bold", colour = "#2F2620", size = ggplot2::rel(1.05)),
      legend.text = ggplot2::element_text(colour = "#2F2620", face = "bold", size = ggplot2::rel(1.0)),
      legend.key.size = ggplot2::unit(1.1, "lines")
    )
}

empty_plot <- function(msg) {
  ggplot2::ggplot() + ggplot2::annotate("text", x = 0, y = 0, label = msg, colour = "#75665A", size = 5) +
    ggplot2::theme_void()
}

# ---------------------------------------------------------------------------
# Small helpers
# ---------------------------------------------------------------------------
# A numeric column is kept as it is. Converting it through text is not safe: as.character() follows
# options(OutDec), so under OutDec = "," every non-integer value came back as NA (0.21.12).
safe_numeric <- function(x) if (is.numeric(x)) as.numeric(x) else suppressWarnings(as.numeric(as.character(x)))

# Numbers recovered from table() names: factor levels are made with as.character(), which uses OutDec.
names_num <- function(x) {
  nm <- names(x)
  od <- getOption("OutDec", ".")
  if (!identical(od, ".")) nm <- gsub(od, ".", nm, fixed = TRUE)
  suppressWarnings(as.numeric(nm))
}

mode_or_na <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[[1]]
}

finite_mean <- function(v) {
  v <- v[is.finite(v)]
  if (length(v)) mean(v) else NA_real_
}

cor_safe <- function(a, b) {
  ok <- is.finite(a) & is.finite(b)
  if (sum(ok) < 4) return(NA_real_)
  if (!isTRUE(stats::sd(a[ok]) > 0) || !isTRUE(stats::sd(b[ok]) > 0)) return(NA_real_)
  stats::cor(a[ok], b[ok])
}

format_p <- function(x) {
  if (length(x) != 1 || !is.finite(x)) return("NA")
  if (x < 0.001) "<0.001" else sprintf("%.3f", x)
}

format_num <- function(x, digits = 3) format(signif(x, digits), trim = TRUE, scientific = FALSE)

# Least-squares line of y on x with its 95% confidence band on a grid (trend lines drawn in figures).
# A line through only two distinct x values has no residual degrees of freedom and therefore no band (NA).
# Weight for a point that pools two groups of n individuals in total: log2(n). Proportional weights give an
# identical least-squares trend, so the base of the logarithm does not change the fit, only its label.
log_n_weight <- function(n) {
  w <- suppressWarnings(log2(as.numeric(n)))
  w[!is.finite(w) | w <= 0] <- NA_real_
  w
}

lm_band <- function(x, y, n = 50, w = NULL) {
  ok <- is.finite(x) & is.finite(y)
  if (!is.null(w)) ok <- ok & is.finite(w) & w > 0
  x <- x[ok]
  y <- y[ok]
  if (!is.null(w)) w <- w[ok]
  if (length(unique(x)) < 2) return(data.frame())
  fit <- if (is.null(w)) stats::lm(y ~ x) else stats::lm(y ~ x, weights = w)
  grid <- seq(min(x), max(x), length.out = n)
  pr <- suppressWarnings(stats::predict(fit, newdata = data.frame(x = grid), interval = "confidence", level = 0.95))
  out <- data.frame(x = grid, fit = as.numeric(pr[, "fit"]), lo = as.numeric(pr[, "lwr"]), hi = as.numeric(pr[, "upr"]))
  out$lo[!is.finite(out$lo)] <- NA_real_
  out$hi[!is.finite(out$hi)] <- NA_real_
  out
}

# Sampling step: the smallest interval that is common (>= 5% of intervals) among within-individual
# intervals between consecutive records. Staggered cohorts, floating-point noise in ages and occasional
# extra records therefore do not shrink it. Without repeated records it falls back to gaps between
# distinct ages; when no interval is common (irregular sampling) it is the median interval. A smallest
# common interval that is close to (>= 0.75 times) but not a divisor of a clearly dominant interval (>= 60%)
# is an unequal first or last interval, e.g. a day-1 record followed by weekly sampling on days 7, 14, 21,
# not a finer schedule: the dominant interval is then the step.
infer_age_step <- function(age, id = NULL) {
  ok <- is.finite(age)
  if (!is.null(id)) ok <- ok & !is.na(id)
  if (sum(ok) < 2) return(NA_real_)
  a <- age[ok]
  tol <- 1e-6 * max(1, diff(range(a)))
  d <- numeric(0)
  if (!is.null(id)) {
    g <- as.character(id[ok])
    o <- order(g, a)
    a2 <- a[o]
    g2 <- g[o]
    same <- g2[-1] == g2[-length(g2)]
    d <- diff(a2)[same]
  }
  d <- d[is.finite(d) & d > tol]
  if (!length(d)) {
    u <- sort(unique(a))
    d <- diff(u)
    d <- d[d > tol]
    if (!length(d)) return(NA_real_)
  }
  d <- signif(d, 6)
  tab <- table(d)
  vals <- names_num(tab)
  share <- as.numeric(tab) / length(d)
  common <- vals[share >= 0.05]
  if (!length(common)) return(stats::median(d))
  step <- min(common)
  dom <- vals[which.max(share)]
  if (max(share) >= 0.6 && step / dom >= 0.75 && abs(dom / step - round(dom / step)) > 0.01) step <- dom
  step
}

# Numbers for display tables with a fixed number of significant digits (0.22.3). shiny::renderTable rounds numeric
# columns to two decimals, which shows small coefficients (for example those of age^2 and age^3 when age is not
# standardised) as 0.00; formatting them as text first keeps them readable.
sig_text <- function(v, digits = 3) {
  vapply(as.numeric(v), function(x) {
    if (!is.finite(x)) return("NA")
    if (x == 0) return("0")
    if (abs(x) >= 1e6 || abs(x) < 1e-4) return(formatC(x, digits = digits - 1, format = "e"))
    formatC(signif(x, digits), digits = digits, format = "fg")
  }, character(1))
}

# Tables drawn by shiny::renderTable show numeric columns with two decimals, so a value smaller than 0.005 in absolute
# value appears as 0.00 (0.22.4). A numeric column holding such a value is converted to text: values of 0.005 or more
# keep the two decimals, smaller ones get three significant digits. Columns without small values, whole-number columns
# and anything that is not a data frame are returned unchanged, so ordinary tables look as before.
display_numbers <- function(x) {
  if (!is.data.frame(x) || !ncol(x)) return(x)
  for (j in seq_along(x)) {
    v <- x[[j]]
    if (!is.double(v)) next
    fin <- is.finite(v)
    if (!any(fin & v != 0 & abs(v) < 0.005)) next
    x[[j]] <- ifelse(!fin, "NA", ifelse(v == 0 | abs(v) >= 0.005, sprintf("%.2f", v), sig_text(v, 3)))
  }
  x
}

# The sampling interval used throughout the app (0.22.0): a number set by the user (the 'Sampling interval' control on
# the Missingness tab) or, when none is set, the interval inferred from the ages by infer_age_step(). Only a single
# finite positive number counts as set; anything else falls back to the inferred interval.
valid_age_step <- function(step) is.numeric(step) && length(step) == 1L && is.finite(step) && step > 0
resolve_age_step <- function(age, id = NULL, step = NULL) {
  if (valid_age_step(step)) return(as.numeric(step))
  infer_age_step(age, id)
}

# Values clamped to a window widened by `pad` times its width on each side (0.20.13). coord_cartesian() hides what lies
# outside the window, but the graphics device still receives those coordinates, and astronomically large ones - sparse
# individuals' curves extrapolated, or exponentials on the log scale - can crash it, and the app with it.
squish_to <- function(x, lim, pad = 0.5) {
  if (length(lim) != 2 || !all(is.finite(lim))) return(x)
  w <- diff(lim)
  if (!is.finite(w) || w <= 0) w <- max(abs(lim), 1)
  pmin(pmax(x, lim[1] - pad * w), lim[2] + pad * w)
}

# Legend labels wrapped onto several lines, so long labels stay inside the figure.
# strwrap() breaks so that lines stay strictly under `width`; the label should be allowed to fill it
wrap_label <- function(x, width = 26) vapply(as.character(x), function(s) paste(strwrap(s, width + 1L), collapse = "\n"), character(1), USE.NAMES = FALSE)

display_term <- function(x) gsub("(^|[^A-Za-z0-9_])(cv|re)_", "\\1", x)

`%|NA|%` <- function(a, b) if (length(a) != 1 || is.na(a)) b else a

# ---------------------------------------------------------------------------
# performance package checks (optional). Every check, including the formatting of its result, runs inside its
# own error handler and time limit, so a check that fails or runs too long is reported instead of stopping the app.
# ---------------------------------------------------------------------------
with_time_limit <- function(expr, seconds) {
  setTimeLimit(elapsed = seconds, transient = TRUE)
  on.exit(setTimeLimit(elapsed = Inf, transient = FALSE), add = TRUE)
  expr
}

# columns referred to inside ggplot2 aesthetics and data.table-style expressions
utils::globalVariables(c("w", "wt", ".wts"))
