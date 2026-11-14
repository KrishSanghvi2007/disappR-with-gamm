# Builds the bundled files of the Cooper et al. 2021 (superb fairy-wren) and Lalande et al. 2022 (Asian elephant)
# examples from the archived originals. Run from the package root with the folders of the downloaded deposits:
#   Rscript dev/prepare_gamm_examples.R <fairywren_dir> <elephant_csv>
# fairywren_dir: dryad_female_surv.csv, dryad_female_repro.csv, dryad_male_repro.csv (Dryad 10.5061/dryad.2547d7wnq)
# elephant_csv:  sex-specific-dataset.csv (Dryad 10.5061/dryad.5dv41ns59; semicolon-separated)
a <- commandArgs(TRUE); fw <- a[[1]]; el <- a[[2]]
out <- file.path("inst", "app", "data")
s <- utils::read.csv(file.path(fw, "dryad_female_surv.csv"))
r <- utils::read.csv(file.path(fw, "dryad_female_repro.csv"))
m <- utils::read.csv(file.path(fw, "dryad_male_repro.csv"))
# Females: every year a female was alive (the survival records), with her independent offspring where she bred.
# Survival to the next breeding season is derived: a female survived the year unless it was the age at which she died
# (age equals her longevity in years).
s$survived <- as.integer(s$age < s$long_years)
# the deposit repeats one bird-year (A30037-T__, 2015) four times with identical values: kept once
r <- unique(r[, c("ID", "year", "offspring_inde", "jul_incubeZY", "final_breed")])
f <- merge(s[, c("ID", "year", "age", "long_years", "survived")], r, by = c("ID", "year"), all.x = TRUE)
f <- f[order(f$ID, f$year), ]
utils::write.csv(f, file.path(out, "cooper_2021_fairywren_females.csv"), row.names = FALSE, na = "")
# Males: offspring counts; the individual is the part of ID.yr before "--".
m$ID <- sub("--[0-9]+$", "", m$ID.yr)
m <- m[order(m$ID, m$year), c("ID", "year", "age", "dom", "long_years", "final_breed", "offspring", "offspring_WG", "offspring_EG")]
utils::write.csv(m, file.path(out, "cooper_2021_fairywren_males.csv"), row.names = FALSE, na = "")
# Elephants: log body mass; lifespan (age at death) for the elephants known to have died.
e <- utils::read.csv2(el, dec = ".", stringsAsFactors = FALSE)
e$log_bm <- round(log(e$bm), 6)
e$lifespan <- ifelse(e$alive == "N", round(e$age + e$y_before_death, 4), NA)
e$lifespan <- stats::ave(e$lifespan, e$id, FUN = function(x) if (any(is.finite(x))) x[is.finite(x)][1] else NA_real_)
e$sex <- ifelse(e$sex == "A", "F", "M")                     # A = females, B = males in the deposit's legend
e <- e[order(e$id, e$age), c("id", "sex", "age", "log_bm", "bm", "lifespan", "alive", "terminal", "Township", "year_birth", "measure")]
e$age <- round(e$age, 4)
utils::write.csv(e, file.path(out, "lalande_2022_elephant_body_mass.csv"), row.names = FALSE, na = "")
cat("females", nrow(f), length(unique(f$ID)), "| males", nrow(m), length(unique(m$ID)), "| elephants", nrow(e), length(unique(e$id)), "\n")
