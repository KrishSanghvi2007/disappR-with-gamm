# Record the exact versions of R and of every package disappR uses, so that published numbers can be reproduced.
# Run from the package root (the folder containing DESCRIPTION):
#   source("inst/scripts/lock_dependencies.R")
# Writes dependency_versions.csv (always) and renv.lock (when the renv package is installed).
local({
  if (!file.exists("DESCRIPTION")) stop("Run this from the disappR package root (the folder containing DESCRIPTION).")
  desc <- read.dcf("DESCRIPTION")
  field <- function(f) if (f %in% colnames(desc)) desc[1, f] else ""
  pkgs <- trimws(sub("\\(.*$", "", unlist(strsplit(paste(field("Imports"), field("Suggests"), sep = ","), ","))))
  pkgs <- setdiff(unique(pkgs[nzchar(pkgs)]), "R")
  ver <- vapply(pkgs, function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_), character(1))
  rec <- data.frame(package = c("R", "disappR", pkgs),
                    version = c(paste(R.version$major, R.version$minor, sep = "."), unname(desc[1, "Version"]), unname(ver)),
                    stringsAsFactors = FALSE)
  rec$installed <- !is.na(rec$version)
  rec$recorded <- format(Sys.time(), "%Y-%m-%d %H:%M")
  rec$platform <- R.version$platform
  utils::write.csv(rec, "dependency_versions.csv", row.names = FALSE)
  message("Wrote dependency_versions.csv: ", sum(!is.na(ver)), " of ", length(pkgs), " dependencies installed.")
  if (requireNamespace("renv", quietly = TRUE)) {
    ok <- tryCatch({
      renv::snapshot(project = ".", lockfile = "renv.lock", type = "explicit", prompt = FALSE)
      TRUE
    }, error = function(e) {
      message("renv::snapshot() failed: ", conditionMessage(e))
      FALSE
    })
    if (ok) message("Wrote renv.lock; renv::restore() reinstalls these versions on another machine.")
  } else {
    message("Install the renv package to also write renv.lock: install.packages('renv').")
  }
})
