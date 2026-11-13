## =============================================================================
## Launch the NEWEST disappR on this computer.
## Unzipping a new build next to an old one makes macOS create "disappR 2", "disappR 3", ... - a launcher with a
## fixed path then keeps opening the old copy. This finds every copy in Downloads, Desktop and Documents, lists
## them with their versions, and opens the newest.
## =============================================================================
find_disappR <- function(roots = c("~/Downloads", "~/Desktop", "~/Documents")) {
  hits <- character(0)
  for (r in path.expand(roots)) {
    if (!dir.exists(r)) next
    d1 <- list.dirs(r, recursive = FALSE)
    d2 <- unlist(lapply(d1, list.dirs, recursive = FALSE))
    for (d in c(d1, d2)) {
      f <- file.path(d, "DESCRIPTION")
      if (!file.exists(f) || !file.exists(file.path(d, "inst", "app", "ui.R"))) next
      dd <- tryCatch(read.dcf(f), error = function(e) NULL)
      if (!is.null(dd) && identical(unname(dd[1, "Package"]), "disappR")) hits[d] <- unname(dd[1, "Version"])
    }
  }
  if (!length(hits)) stop("No disappR folder found in Downloads, Desktop or Documents.", call. = FALSE)
  hits <- hits[order(numeric_version(hits), decreasing = TRUE)]
  cat("disappR copies found (newest first):\n")
  for (i in seq_along(hits)) cat(sprintf("  %-8s %s\n", hits[[i]], names(hits)[[i]]))
  if (length(hits) > 1) cat("  -> older copies can be deleted to avoid confusion.\n")
  names(hits)[[1]]
}
disappr_dir <- find_disappR()
cat("\nLaunching", disappr_dir, "\n")
shiny::runApp(file.path(disappr_dir, "inst", "app"), launch.browser = TRUE)
