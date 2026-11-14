# disappR engine - Import: bundled empirical examples, uploaded files (separators, decimal commas, encodings) and column guessing.
# Engine code shared by the app and the R interface; changes that alter results must pass the golden tests.

load_fly_example <- function() {
  f <- file.path("data", "fly_fecundity.csv")
  if (!file.exists(f)) return(NULL)
  utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
}

FLY_MAPPING <- list(
  id = "F1_ID", age = "F1_Age", trait = "F2_Count", alr = "ALR", life = "LS",
  entry = "__AUTO_FIRST__", condition = "", covars = c("Rep", "Paternal_age", "Paternal_sperm_age"),
  cov_factor = c("Rep", "Paternal_age", "Paternal_sperm_age"), cov_int = character(0),
  group = "F0_ID", nested = TRUE, random = character(0), censor = "Censored", censor_value = "0",
  start_mode = "same", start_age = 4, age_round = NA_real_, cov_age = character(0),
  trait_specific_ages = FALSE   # documented results use ALR and AFR from every record (0.24.2)
)

# ---------------------------------------------------------------------------
# Bundled empirical examples. Each entry carries the published data, the mapping
# and the model settings that reproduce (or come close to) the published analysis.
# ---------------------------------------------------------------------------
load_example_file <- function(file) {
  f <- file.path("data", file)
  if (!file.exists(f)) return(NULL)
  utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
}

example_map <- function(...) {
  base <- list(id = "", age = "", trait = "", alr = "__AUTO_LAST__", life = "", entry = "__AUTO_FIRST__",
               condition = "", covars = character(0), cov_factor = character(0), cov_int = character(0),
               group = "", nested = TRUE, random = character(0), censor = "", censor_value = "",
               start_mode = "afr", start_age = NA_real_, age_round = NA_real_, cov_age = character(0),
               inconsistent = "error",
               # the documented example results use ALR and AFR from every record (0.24.2)
               trait_specific_ages = FALSE)
  utils::modifyList(base, list(...))
}

EXAMPLES <- list(
  fly = list(
    label = "Sanghvi et al. 2025, American Naturalist \u2014 Drosophila melanogaster (daily fecundity)",
    about = list(species = "Fruit fly (Drosophila melanogaster), laboratory population",
                 trait = "Daily fecundity of daughters: offspring counted in 14-day assays until death",
                 model = "Quadratic age with the daughters' lifespan as the selective-disappearance term; zero-inflated negative binomial; daughters nested in fathers",
                 covariates = "Rep: replicate population; Paternal_age: father's age treatment; Paternal_sperm_age: how long the father's sperm had been stored before fertilisation",
                 finding = "Longer-lived daughters were more fecund throughout life (age-independent selective disappearance).",
                 other = "None in this file"),
    file = "fly_fecundity.csv", mapping = FLY_MAPPING,
    family = "zinb", age_function = "Quadratic", models = c("M1", "M2", "M3", "M4", "M5"),
    note = paste("Lifetime fecundity of laboratory female Drosophila melanogaster, assayed every 14 days until death, with lifespan,",
                 "replicate and paternal age treatments. The paper fitted zero-inflated negative binomial mixed models with",
                 "offspring nested in fathers, and found age-independent selective disappearance. Their model fits a quadratic age",
                 "term with the daughters' lifespan added on its own, alongside paternal age, sperm storage and replicate, so",
                 "the published selective-disappearance term is lifespan rather than ALR: tick Model 6 to use lifespan itself,",
                 "which restricts every model to the 99% of flies with a known lifespan, because models must be compared on the same",
                 "rows throughout. ")),
  bichet = list(
    label = "Bichet et al. 2022, Journal of Animal Ecology \u2014 common tern (immune parameters)",
    about = list(species = "Common tern (Sterna hirundo), wild colony",
                 trait = "Haemagglutination titre (HA), a measure of innate immunity",
                 model = "Linear age with age at first and last measurement (Model 7); Gaussian; random intercepts for individual and sampling year",
                 covariates = "sex; storage_time_HAHL: time samples were stored before the assay; initial_lysis: lysis at the start of the assay; assay_batch_HAHL: assay batch",
                 finding = "Haemagglutination rose with age within individuals; no selective appearance or disappearance.",
                 other = "Haptoglobin (hapto)"),
    file = "bichet_2022_tern_immunity.csv",
    mapping = example_map(id = "ID", age = "age", trait = "HA",
                          covars = c("sex", "storage_time_HAHL", "initial_lysis", "assay_batch_HAHL"),
                          cov_factor = c("sex", "assay_batch_HAHL"), random = "year_sampling"),
    family = "gaussian", age_function = "Linear", models = c("M1", "M2", "M3", "M4", "M7"),
    note = paste("Innate immunity (haemagglutination titre and haptoglobin) in common terns of known age, sampled over many years;",
                 "age is derived as sampling year minus birth year. The paper split age into among- and within-individual",
                 "components and found haemagglutination rising within individuals, haptoglobin unchanged, and no selective",
                 "(dis)appearance in either. Alongside the centring models they fitted a second set with age, age at first",
                 "measurement and age at last measurement, which is Model 7 here, and that is where their conclusion about",
                 "selective appearance and disappearance comes from; Model 7 is therefore ticked. They also compared random",
                 "intercepts with random intercepts and slopes, and random intercepts fitted both traits better, so the example keeps a random intercept. Switch the trait to hapto for the",
                 "second measure. ")),
  bichet_marmot = list(
    label = "Bichet et al. 2022, Ecology and Evolution \u2014 Alpine marmot (immune parameters)",
    about = list(species = "Alpine marmot (Marmota marmota), wild population",
                 trait = "Lymphocyte count per blood sample",
                 model = "Linear age with age at last observation and age at first observation (Model 7); Poisson",
                 covariates = "sex; mass: body mass; capture_date: day of capture; year_of_sampling: sampling year",
                 finding = "Marmots last observed at older ages had more lymphocytes and fewer neutrophils (selective disappearance).",
                 other = "Neutrophil, monocyte and eosinophil counts; leukocyte concentration (log scale, Gaussian)"),
    file = "bichet_2022_marmot_immunity.csv",
    mapping = example_map(id = "id", age = "age", trait = "lymphocyte_count", alr = "ALO",
                          covars = c("sex", "mass", "capture_date", "year_of_sampling"),
                          cov_factor = c("sex", "year_of_sampling")),
    family = "poisson", age_function = "Linear", models = c("M1", "M2", "M7"),
    note = paste("Leukocyte counts and concentration in wild Alpine marmots of known age, with age at last observation",
                 "supplied by the authors. The paper partitioned age into average and delta components, then replaced",
                 "them with actual age, age at access to dominance (selective appearance) and age at last observation",
                 "(selective disappearance): older ages at last observation went with more lymphocytes and fewer",
                 "neutrophils, and age at access to dominance mattered for neither. Model 7 with ALO mapped as the proxy",
                 "is that published model; the app supplies age at first observation for AFR, since age at access to dominance",
                 "is not in the archived file. Switch the trait to neutrophil_count, monocyte_count, eosinophil_count or",
                 "log_leukocyte_concentration (the last with the Gaussian family) for the other four responses. ")),
  moullec_swift = list(
    label = "Moullec et al. 2023, Frontiers in Ecology and Evolution \u2014 Alpine swift (reproduction)",
    about = list(species = "Alpine swift (Tachymarptis melba), wild colonies",
                 trait = "Laying date",
                 model = "Quadratic age with age at first reproduction and lifespan (Models 6-8); Gaussian; random intercept for year",
                 covariates = "sex; colony",
                 finding = "Reproductive senescence in females but not males, and selective appearance in both sexes.",
                 other = "Clutch size, brood size at hatching and at fledging (counts, Poisson)"),
    file = "moullec_2023_alpine_swift_reproduction.csv",
    mapping = example_map(id = "ring", age = "age", trait = "laying_date", life = "lifespan", entry = "AFR",
                          covars = c("sex", "colony"), cov_factor = c("sex", "colony"), random = "year", inconsistent = "exclude"),
    family = "gaussian", age_function = "Quadratic",
    models = c("M1", "M2", "M4", "M6", "M7", "M8"),
    integrity_note = "One bird, Frontiers_000304, is excluded: its lifespan column repeats its age (2, 3 and 4), so it has no single lifespan.",
    note = paste("Twenty years of reproduction by Alpine swifts of known age, with age at first reproduction and lifespan",
                 "supplied for birds followed from first breeding to death. The paper compared no-age, linear, quadratic",
                 "and threshold (breakpoint) models and averaged the best-supported ones, finding reproductive senescence in",
                 "females but not males, selective appearance in both sexes and selective disappearance of long-tailed",
                 "males. The quadratic ageing function set here is the closest this app can come to their threshold",
                 "models, which fit separate slopes either side of a fitted breakpoint age: it captures the same rise-",
                 "then-fall shape with a smooth curve rather than a hinge, so the onset age they report has no direct",
                 "equivalent in these results. Switch the trait to clutch_size, brood_size_hatching or",
                 "brood_size_fledging (counts, Poisson) for the other three reproductive measures; the biometric traits",
                 "they also analysed are not in this file. Their selective-disappearance term is lifespan and their appearance",
                 "term is age at first reproduction, both entered additively beside a within-individual age effect: Model 7",
                 "here is the same shape with ALR in place of lifespan, and Model 6 uses lifespan itself, interacting with",
                 "age. ")),
  pasztor_apollo = list(
    label = "P\u00e1sztor et al. 2022, Ecology and Evolution \u2014 Clouded Apollo butterfly (body size)",
    about = list(species = "Clouded Apollo butterfly (Parnassius mnemosyne), wild population",
                 trait = "Body mass (log scale), from mark-recapture",
                 model = "Quadratic age (days since first capture), including the centring models; Gaussian; correlated random intercept and slope per butterfly, random intercept for year",
                 covariates = "sex; first_capture: day of the flight period on which the butterfly was first caught; wing_length: mean forewing length",
                 finding = "Body mass declined non-linearly with age in both sexes; butterflies first caught later in the season were lighter.",
                 other = "Thorax width; body mass on the raw scale"),
    file = "pasztor_2022_clouded_apollo_body_size.csv",
    mapping = example_map(id = "id", age = "age_days", trait = "log_body_mass",
                          covars = c("sex", "first_capture", "wing_length"), cov_factor = "sex",
                          random = "year"),
    family = "gaussian", age_function = "Quadratic", models = c("M1", "M2", "M3", "M4", "M5"),
    random_structure = "correlated",
    note = paste("Seven flight periods of mark-recapture on a wild Clouded Apollo population, with repeated body mass and",
                 "thorax width and a single wing length per butterfly. IMPORTANT: several columns are DERIVED here and are",
                 "not in the archived files. age_days is days elapsed since an individual's first capture, which the paper",
                 "uses as a minimum estimate of true age; first_capture is the day of that year's flight period on which",
                 "the individual was first caught, counted from the flight-period start dates in the paper's Table 1;",
                 "mean_age, mean_age_sq and mean_first_capture are the individual (or annual) averages the paper defines;",
                 "wing_length is the mean of the two forewings; thorax_width is the mean of the two measurements per",
                 "occasion; log_body_mass is the natural log, as the paper models it. The paper found body mass declining",
                 "non-linearly with age in both sexes, lighter butterflies among those caught later in the season, and a",
                 "positive effect of wing length. Fitting their model here reproduces every coefficient to within about one",
                 "published standard error. Random-slope models fitted better than random-intercept ones there, so the example",
                 "opens with a correlated random slope. Switch the trait to thorax_width or body_mass for the other",
                 "responses. Because",
                 "age is counted from first capture, every individual starts at age 0, so age at first observation carries no",
                 "information and the appearance models are unavailable; age at last record is the number of days a",
                 "butterfly was still being recaught, and 31% of individuals were caught only once. ")),
  wynn = list(
    label = "Wynn et al. 2025, Journal of Animal Ecology \u2014 common tern (navigational efficiency)",
    about = list(species = "Common tern (Sterna hirundo), tracked on migration",
                 trait = "Navigational deflection: the angle between each track step and the direction of the goal (lower is more efficient)",
                 model = "Linear age split into mean age and within-individual change (Model 3); Gaussian; tracks nested in individuals",
                 covariates = "season: autumn or spring migration",
                 finding = "Older birds navigated more efficiently among individuals but not within them, read as selective disappearance of poor navigators.",
                 other = "None in this file"),
    file = "wynn_2025_tern_navigation.csv",
    mapping = example_map(id = "individual", age = "age", trait = "deflection",
                          covars = "season", cov_factor = "season", random = "track_id"),
    family = "gaussian", age_function = "Linear", models = c("M1", "M2", "M3", "M4", "M5"),
    note = paste("Navigational efficiency of migrating common terns: the deflection of each track from the direction of the goal.",
                 "This response is not in the archived file and is derived here from the archived geolocator positions (the angle",
                 "between the bearing to the next fix and the bearing to the goal, with short steps removed as stopovers; see",
                 "data/PROVENANCE.md). The paper fitted linear mixed models splitting age into among- and within-individual",
                 "components, track nested in individual, and found older birds navigating more efficiently among individuals",
                 "but not within them, which they read as selective disappearance. Track is mapped as an extra random intercept,",
                 "not as the individual. Splitting age that way is Model 3 here, so Model 3 is the published model and Models 2",
                 "and 4 go beyond it. ")),
  sanghvi_female = list(
    label = "Sanghvi et al. 2022, Evolution \u2014 seed beetle (female fecundity)",
    about = list(species = "Seed beetle (Callosobruchus maculatus), laboratory",
                 trait = "Daily egg count of females",
                 model = "Quadratic age interacting with developmental and adult temperature, with adult lifespan (Model 6); negative binomial; random slopes for females within families",
                 covariates = "DevT: developmental temperature; AdultT: adult temperature; Block: experimental block",
                 finding = "Hot developmental and hot adult temperatures each accelerated the decline in fecundity with age.",
                 other = "None in this file"),
    file = "sanghvi_2022_beetle_female_fecundity.csv",
    mapping = example_map(id = "individual", age = "Adult_age", trait = "Daily_Eggs", life = "Adult_lifespan",
                          covars = c("DevT", "AdultT", "Block"), cov_factor = c("DevT", "AdultT", "Block"),
                          cov_int = "DevT|||AdultT", cov_age = c("DevT", "AdultT"),
                          group = "Family", nested = TRUE),
    family = "nbinom2", age_function = "Quadratic", random_structure = "correlated", models = c("M1", "M2", "M3", "M4", "M5", "M6"),
    integrity_note = "The individual identifier is DERIVED: Block, Family and ID joined, because some original IDs lost a character when the file was re-encoded and 15 of them each covered more than one beetle.",
    note = paste("Daily egg counts of female seed beetles from a 2 \u00d7 2 temperature experiment. The paper fitted negative binomial",
                 "mixed models with the temperature treatments interacting with age and age\u00b2, adult lifespan as a fixed effect for",
                 "selective disappearance, and random slopes of age for females and families, which this example switches on;",
                 "hot developmental and hot adult temperatures each",
                 "accelerated the decline in fecundity with age. ")),
  allain = list(
    label = "Allain et al. 2023, Oikos \u2014 eastern chipmunk (reproduction)",
    about = list(species = "Eastern chipmunk (Tamias striatus), wild population",
                 trait = "Number of juveniles weaned in a breeding season",
                 model = "Quadratic age with age at first reproduction (interacting with age) and lifespan; Poisson",
                 covariates = "season: breeding season before or after a tree-seed mast; sex; site",
                 finding = "Reproductive ageing depended on when individuals started breeding.",
                 other = "weaned_juv: whether any juvenile was weaned (0/1, binomial)"),
    file = "allain_2023_chipmunk_reproduction.csv",
    mapping = example_map(id = "ID", age = "age", trait = "nb_juv", entry = "AFR", life = "lifespan",
                          covars = c("season", "sex", "site"), cov_factor = c("season", "sex", "site"),
                          cov_age = "season", inconsistent = "exclude"),
    family = "poisson", age_function = "Quadratic", models = c("M1", "M2", "M4", "M6", "M7", "M8", "M9", "M10"),
    integrity_note = "Three chipmunks (D037, D114 and D165) are excluded: their AFR column repeats their age (7 and 10), so they have no single age at first reproduction.",
    note = paste("Reproduction of wild eastern chipmunks, with age and age at first reproduction in months and lifespan known for",
                 "individuals that died. The paper fitted generalised linear mixed models (binomial for the probability of",
                 "weaning, Poisson for the number of juveniles) and found that age at first reproduction",
                 "and its interaction with age improved the models substantially: reproductive ageing depended on when",
                 "individuals started breeding. weaned_juv is a binary alternative trait for the binomial family. Lifespan is",
                 "unknown for some individuals, so keeping Model 6 ticked restricts every model to records with a known",
                 "lifespan, because models must be compared on the same rows throughout. Their supplementary model tables give the",
                 "best-supported model for females as age + age\u00b2 + season + AFR + season \u00d7 age + season \u00d7 age\u00b2 +",
                 "AFR \u00d7 age + AFR \u00d7 age\u00b2 + lifespan: age-dependent selective appearance through the",
                 "AFR interactions, with lifespan additive, which is Model 10 here except that their disappearance term is",
                 "lifespan rather than ALR. ")),
  bouwhuis = list(
    label = "Bouwhuis et al. 2009, Proc. R. Soc. B \u2014 great tit (recruit production)",
    about = list(species = "Great tit (Parus major), Wytham Woods",
                 trait = "Laying date of each breeding attempt",
                 model = "Quadratic age interacting with ALR (Model 4); Gaussian; random intercepts for year and wood sector",
                 covariates = "YR_FL: year quality (population fledgling production); loc_density: local breeding density; f_status: female immigrant or locally born; pred: nest-box type",
                 finding = "In the original study, selective disappearance of poorer breeders hid part of the within-individual decline, so senescence began earlier than population-level patterns suggest. Laying date was not analysed there; our paper analyses it.",
                 other = "Recruits, clutch size (CS), brood size (BS), fledglings (FL)"),
    file = "bouwhuis_2009_great_tit_recruitment.csv",
    mapping = example_map(id = "female", age = "f_min_age", trait = "LD", alr = "f_ALR",
                          covars = c("YR_FL", "loc_density", "f_status", "pred"),
                          cov_factor = c("f_status", "pred"), random = c("year", "area")),
    family = "gaussian", age_function = "Quadratic", models = c("M1", "M2", "M3", "M4", "M5"),
    note = paste("Nearly five decades of breeding by female great tits in Wytham Woods: annual recruits, clutch size, brood size",
                 "and fledgling number, with year quality, breeding density, female status and nest-box type as covariates and",
                 "year and wood sector as random effects. The paper fitted cross-classified mixed models and showed that",
                 "selective disappearance of poorer breeders masks part of the within-individual decline, so senescence begins",
                 "earlier than a population-level analysis suggests, while the component traits show age effects without a",
                 "lifespan effect. The trait opens on laying date (LD); recruits, CS, BS and FL,",
                 "the traits analysed in the paper, are in the trait dropdown on the Data tab.",
                 "Laying date data from this study are what our paper (Sanghvi et al. 2026) analyses. Their model set fitted ALR,",
                 "ALR\u00b2 and ALR \u00d7 age together with terminal effects, so the published specification is Model 4 with a",
                 "quadratic proxy rather than an additive one. They modelled recruits as counts with cross-classified random",
                 "effects: switch the family to Poisson if you change the trait to recruits, CS, BS or FL. ")),
  warner = list(
    label = "Warner et al. 2016, PNAS \u2014 painted turtle (reproduction)",
    about = list(species = "Painted turtle (Chrysemys picta), wild population",
                 trait = "Mean egg mass per clutch",
                 model = "Quadratic reproductive age with plastron length (Model 1); Gaussian; random intercepts for female and year",
                 covariates = "Plastron Length (mm): female body size",
                 finding = "Egg mass increased with reproductive age; clutch size did not.",
                 other = "Clutch size and hatching success"),
    file = "warner_2016_turtle_reproduction.csv",
    mapping = example_map(id = "Female ID", age = "Reproductive Age", trait = "Avg Egg Mass (g)",
                          covars = "Plastron Length (mm)", random = "Year"),
    family = "gaussian", age_function = "Quadratic", models = c("M1", "M2", "M3", "M4", "M5"),
    note = paste("Two decades of nesting by wild painted turtles: egg mass, clutch size and hatching success by reproductive age,",
                 "with plastron length as a covariate and maternal identity as a random effect. The paper fitted linear mixed",
                 "models and found egg mass increasing with reproductive age while clutch size did not.",
                 "Records marked UNK are read as missing, and females nest several",
                 "times per season, so duplicate ID \u00d7 age records are expected. Their models fitted reproductive age with",
                 "plastron length and maternal identity only, with no lifespan or ALR term, so the published specification is",
                 "Model 1 with a covariate and every other model here goes beyond their analysis. ")),
  mckennaell_breeding = list(
    label = "McKenna-Ell et al. 2023, Biology Letters \u2014 Soay sheep (breeding probability, offspring survival)",
    about = list(species = "Soay sheep (Ovis aries), St Kilda",
                 trait = "Fecundity: whether a female aged 5 or older gave birth to a live lamb (0/1)",
                 model = "Linear age with age at last observation (Model 2); binomial; random intercepts for female, year and birth cohort",
                 covariates = "BredYearling: whether she bred as a yearling; EarlyLifeRec: her early-life recruitment (both also interact with age)",
                 finding = "Breeding probability declined with age, with selective disappearance, and declined faster in females that bred as yearlings.",
                 other = "OffspringRecruitment: whether the lamb survived its first winter (0/1)"),
    file = "mckennaell_2023_soay_breeding_survival.csv",
    mapping = example_map(id = "FemaleID", age = "Age", trait = "Fecundity", alr = "AgeLastObs",
                          covars = c("BredYearling", "EarlyLifeRec"), cov_factor = "BredYearling",
                          cov_age = c("BredYearling", "EarlyLifeRec"), random = c("ObsYear", "FemaleCohort")),
    family = "binomial", age_function = "Linear", models = c("M1", "M2", "M3", "M4", "M5"),
    note = paste("Annual reproduction of known-age female Soay sheep on St Kilda from age 5 onwards, with two binary traits:",
                 "whether a female gave birth to a live lamb (Fecundity) and, for those that did, whether a lamb survived its",
                 "first winter (OffspringRecruitment). The paper fitted binomial mixed models with a linear age effect, age at",
                 "last observation for selective disappearance, whether the female bred as a yearling and her early-life",
                 "recruitment (each also interacting with age), and random intercepts for female, year and birth cohort. It found",
                 "senescence and selective disappearance in both traits, a faster decline in breeding probability in females that",
                 "bred as yearlings, and no effect of early-life reproduction on the rate of ageing in offspring survival.",
                 "Model 2 with these terms is the published model. Switch the trait to OffspringRecruitment for offspring",
                 "survival: records without a lamb are then blank and are not modelled. With Standardise ticked the early-life",
                 "main effects refer to the mean age rather than age 0; untick it to compare coefficients with the paper. ")),
  mckennaell_weight = list(
    label = "McKenna-Ell et al. 2023, Biology Letters \u2014 Soay sheep (offspring birth weight)",
    about = list(species = "Soay sheep (Ovis aries), St Kilda",
                 trait = "Lamb birth weight, for mothers aged 5 and older",
                 model = "Linear maternal age with age at last observation (Model 2); Gaussian; random intercepts for mother, year and birth cohort",
                 covariates = "OffspringCaptureAge: lamb's age when weighed; OffspringSex; OffspringTwinStatus; BredYearling and EarlyLifeRec: the mother's early-life reproduction (both also interact with age)",
                 finding = "Birth weight declined with maternal age, and mothers observed to older ages had heavier lambs (selective disappearance).",
                 other = "None in this file"),
    file = "mckennaell_2023_soay_offspring_weight.csv",
    mapping = example_map(id = "FemaleID", age = "Age", trait = "OffspringBirthWt", alr = "AgeLastObs",
                          covars = c("OffspringCaptureAge", "OffspringSex", "OffspringTwinStatus", "BredYearling", "EarlyLifeRec"),
                          cov_factor = c("OffspringSex", "OffspringTwinStatus", "BredYearling"),
                          cov_age = c("BredYearling", "EarlyLifeRec"), random = c("ObsYear", "FemaleCohort")),
    family = "gaussian", age_function = "Linear", models = c("M1", "M2", "M3", "M4", "M5"),
    note = paste("Birth weight of lambs born to known-age female Soay sheep aged 5 and older, one row per lamb. The paper fitted a",
                 "Gaussian mixed model with the lamb\u0027s age at capture, sex and twin status, the mother\u0027s age, her age at last",
                 "observation for selective disappearance, whether she bred as a yearling and her early-life recruitment (both",
                 "also interacting with age), and random intercepts for mother, year and her birth cohort. Birth weight declined",
                 "with maternal age, mothers observed to older ages had heavier lambs (selective disappearance), and early-life",
                 "reproduction did not change that decline. Model 2 with these terms is the published model.",
                 "Age at last observation",
                 "is later than the last weighed lamb for mothers that stopped breeding, which the integrity table flags. With",
                 "Standardise ticked the early-life main effects refer to the mean age; untick it to match the published table. ")),
  szejnersigal_activity = list(
    label = "Szejner-Sigal et al. 2025, Proc. R. Soc. B \u2014 alfalfa leafcutting bee (locomotor activity)",
    about = list(species = "Alfalfa leafcutting bee (Megachile rotundata), laboratory",
                 trait = "Locomotor activity of females: beam breaks in four hours, measured weekly",
                 model = "Quadratic age with lifespan (age at death) added; Gaussian; random intercept per bee",
                 covariates = "None besides lifespan",
                 finding = "Activity rose to a mid-life peak then declined, earlier and lower in males, with little link between early activity and lifespan.",
                 other = "Males (choose sex = m in the subset)"),
    file = "szejnersigal_2025_bee_activity.csv",
    mapping = example_map(id = "id", age = "age", trait = "total.act", life = "age.death"),
    family = "gaussian", age_function = "Quadratic", models = c("M1", "M2", "M4", "M6"),
    extra = list(M1 = "LS"), subset = list(var = "sex", levels = "f"),
    note = paste("Locomotor activity (beam breaks in four hours) of individually marked alfalfa leafcutting bees (Megachile",
                 "rotundata), measured weekly from emergence until death (days 1, 7, 14, 21 and so on), with each bee\u0027s age at",
                 "death. The paper fitted linear mixed models separately by sex, with age as a quadratic, lifespan as a covariate",
                 "and a random intercept per bee: activity rose to a mid-life peak then declined, earlier and lower in males, with",
                 "little link between early activity and lifespan. The example opens on females (Subset: sex = f); choose m for",
                 "males. Age at death is mapped as lifespan (LS) and added to Model 1, so Model 1 is the published model (age,",
                 "age\u00b2 and lifespan); Model 2 uses the age at the last weekly record instead, and Model 6 lets lifespan interact",
                 "with age. age.group20 holds the paper\u0027s short-, average- and long-lived groups for panels. The first interval is",
                 "six days (day 1 to day 7), so the integrity table reports ages off the weekly schedule; the sampling grid",
                 "assigns each record to the nearest weekly occasion. ")),
  tsui_mass = list(
    label = "Tsui et al. 2026, Evolution Letters \u2014 Seychelles warbler (body mass; GAMMs)",
    about = list(species = "Seychelles warbler (Acrocephalus sechellensis), wild population on Cousin Island",
                 trait = "Body mass, z-scored (BodyMass_z; raw grams in BodyMass)",
                 model = "GAMM (step 7): smooth of age (k = 15) with known lifespan as a linear term; Gaussian; random intercepts for bird, birth year and observer",
                 covariates = "RightTarsus_z: tarsus length (body size); newstat: dominant breeder or subordinate; SexEstimate: sex (0/1 as coded in the source data); CatchTime_mins: time of capture; avg_invert: insect abundance",
                 finding = "Body mass declines in late life within individuals (onset of decline at about 4.7 years from the derivative of the age smooth), after controlling for lifespan; an age \u00d7 lifespan interaction was also supported.",
                 other = "Wing length (WingLength_z), tarsus (RightTarsus_z) and fat score (FatScore, a count)"),
    file = "tsui_2026_warbler_body_mass.csv",
    mapping = example_map(id = "BirdID", age = "age_year", trait = "BodyMass_z", life = "lifespan_years",
                          covars = c("RightTarsus_z", "newstat", "SexEstimate", "CatchTime_mins", "avg_invert"),
                          cov_factor = c("newstat", "SexEstimate"), random = c("birthyear", "Observer")),
    family = "gaussian", age_function = "Quadratic", models = c("M1", "M2", "M4", "M6"),
    gamm = list(k = 15, proxy = "LS", proxy_form = "linear", individual = "ri", afr_form = "none"),
    note = paste("Morphology of individually ringed Seychelles warblers caught 1981-2022, with each bird\u0027s lifespan (in years",
                 "here; days in newlifespan). The paper fitted generalized additive mixed models in mgcv to 13 traits, with a smooth of",
                 "age, lifespan as a linear term, these covariates and random intercepts for bird, birth cohort and observer, and",
                 "located the onset of ageing from the derivative of the age smooth. Open step 7 (GAMMs): the example sets the",
                 "published settings (k = 15, known lifespan, linear). Sex-specific curves were compared as models G, GS and S.",
                 "Lifespan is known for every bird in this file. Data: Zenodo 10.5281/zenodo.21101620 (CC BY 4.0). ")),
  brown_haematocrit = list(
    label = "Brown et al. 2021, Ecology and Evolution \u2014 Seychelles warbler (haematocrit; GAMM)",
    about = list(species = "Seychelles warbler (Acrocephalus sechellensis), wild population on Cousin Island",
                 trait = "Haematocrit: the proportion of blood volume made up of red cells",
                 model = "GAMM (step 7): smooth of age; Gaussian; sex, status, catch time and sex \u00d7 status; random intercepts for bird, breed group and catch year",
                 covariates = "Sex; Status: dominant breeder or subordinate; DecimalCatchTime: time of day (hours)",
                 finding = "Haematocrit rises steeply to a peak at about 1.5 years, then changes little; no clear selective disappearance.",
                 other = "SurviveOneYear (0/1) and YearsLeft (time to last sighting)"),
    file = "brown_2021_warbler_haematocrit.csv",
    mapping = example_map(id = "BirdID", age = "AgeYears", trait = "Haematocrit", alr = "AgeLastSeen",
                          covars = c("Sex", "Status", "DecimalCatchTime"), cov_factor = c("Sex", "Status"), cov_int = "Sex|||Status",
                          random = c("BreedGroupID", "CatchYear")),
    family = "gaussian", age_function = "Quadratic", models = c("M1", "M2", "M4"),
    gamm = list(k = 10, proxy = "none", proxy_form = "linear", individual = "ri", afr_form = "none"),
    note = paste("Haematocrit of Seychelles warblers sampled 2003-2016, with continuous age at capture. The paper fitted a GAMM",
                 "(gamm4) with a smooth of age, sex, status, catch time and a sex \u00d7 status interaction, and random intercepts",
                 "for bird, breed group and catch year, and found a peak at about 1.5 years (age smooth edf 7.8). Open step 7",
                 "(GAMMs): the example sets the published model (no lifespan term). AgeLastSeen is the age at the last sighting,",
                 "used as ALR; birds still alive at the end of the study are right-censored, so it is a minimum lifespan.",
                 "Four records without a catch time are left out, as in the paper. Data: Dryad 10.5061/dryad.7wm37pvrp (CC0). ")),
  cooper_wren_females = list(
    label = "Cooper et al. 2021, American Naturalist \u2014 superb fairy-wren females (offspring and survival; GAMMs)",
    about = list(species = "Superb fairy-wren (Malurus cyaneus), wild population, Canberra",
                 trait = "Independent offspring per year (count); annual survival (survived, 0/1) in the same file",
                 model = "GAMM (step 7): smooth of age; Poisson for offspring, binomial for survival; longevity as a linear term; final-breeding-season effect; random intercepts for female and year",
                 covariates = "final_breed: the female's last breeding season (terminal effect); jul_incubeZY: start of incubation relative to the year's median",
                 finding = "Reproductive output rose then declined with age (age smooth edf 4.5); survival declined linearly from age 1 (edf 1.0).",
                 other = "survived (annual survival, 0/1): choose it as the trait with the binomial family"),
    file = "cooper_2021_fairywren_females.csv",
    mapping = example_map(id = "ID", age = "age", trait = "offspring_inde", life = "long_years",
                          covars = "final_breed", cov_factor = "final_breed", random = "year", inconsistent = "exclude"),
    family = "poisson", age_function = "Quadratic", models = c("M1", "M2", "M4", "M6"),
    gamm = list(k = 8, proxy = "LS", proxy_form = "linear", individual = "ri", afr_form = "none"),
    note = paste("Females of a long-term study of superb fairy-wrens, one row per female per year she was alive (1988-2017), with",
                 "her longevity in years, annual survival (derived: she survived the year unless it was the age at which she died)",
                 "and, for years she bred, her number of independent offspring. The paper fitted GAMMs (mgcv, gamm4) with a",
                 "thin-plate smooth of age, longevity as a linear term, a final-breeding-season term and random intercepts for",
                 "female and year (plus an observation-level random effect for counts). Open step 7: the example sets these",
                 "settings. For survival, choose 'survived' as the trait, the binomial family and no lifespan proxy. One",
                 "bird-year repeated four times in the deposit is kept once. Three females whose longevity differs between their",
                 "records in the deposit (725573-RBM, 898242-gbAN, 982725-rgNN) are left out by the integrity check.",
                 "Data: Dryad 10.5061/dryad.2547d7wnq (CC0). ")),
  cooper_wren_males = list(
    label = "Cooper et al. 2021, American Naturalist \u2014 superb fairy-wren males (offspring; GAMMs)",
    about = list(species = "Superb fairy-wren (Malurus cyaneus), wild population, Canberra",
                 trait = "Offspring sired per year (count): total, within-group (offspring_WG) and extra-group (offspring_EG)",
                 model = "GAMM (step 7): smooth of age; Poisson; longevity as a linear term; final-breeding-season effect; dominance status; random intercepts for male and year",
                 covariates = "dom: dominant breeder (1) or subordinate helper (0); final_breed: the male's last breeding season",
                 finding = "Total and extra-group siring success rose then declined with age (edf 3.6 and 3.7); within-group success did not change significantly.",
                 other = "offspring_WG and offspring_EG"),
    file = "cooper_2021_fairywren_males.csv",
    mapping = example_map(id = "ID", age = "age", trait = "offspring", life = "long_years",
                          covars = c("dom", "final_breed"), cov_factor = c("dom", "final_breed"), random = "year"),
    family = "poisson", age_function = "Quadratic", models = c("M1", "M2", "M4", "M6"),
    gamm = list(k = 10, proxy = "LS", proxy_form = "linear", individual = "ri", afr_form = "none"),
    note = paste("Males of the superb fairy-wren study, one row per male per breeding season with offspring sired in total,",
                 "within his group and outside it, his dominance status and longevity. The paper fitted Poisson GAMMs with a",
                 "thin-plate smooth of age, longevity as a linear term, final breeding season, dominance, and random intercepts",
                 "for male and year (plus an observation-level random effect). Open step 7: the example sets these settings.",
                 "Data: Dryad 10.5061/dryad.2547d7wnq (CC0). ")),
  lalande_elephants = list(
    label = "Lalande et al. 2022, Journal of Evolutionary Biology \u2014 Asian elephant (body mass; GAMMs by sex)",
    about = list(species = "Asian elephant (Elephas maximus), semi-captive timber elephants, Myanmar",
                 trait = "Body mass, log-transformed (log_bm; kg in bm), adults aged 18-72",
                 model = "GAMM (step 7): smooth of age by sex (separate curves); age at last measurement as the selective-disappearance term; terminal year; random intercepts for elephant and township",
                 covariates = "sex; terminal: measured within the year before death; Township: region of measurement",
                 finding = "Body mass rose then declined in both sexes, with different trajectories: separate sex curves beat a common one by 65.7 AICc.",
                 other = "Lifespan (age at death) for the elephants known to have died"),
    file = "lalande_2022_elephant_body_mass.csv",
    mapping = example_map(id = "id", age = "age", trait = "log_bm", covars = c("sex", "terminal"), cov_factor = c("sex", "terminal"),
                          random = "Township"),
    family = "gaussian", age_function = "Quadratic", models = c("M1", "M2", "M4"),
    gamm = list(k = 15, proxy = "ALR", proxy_form = "linear", individual = "ri", afr_form = "none"),
    note = paste("Repeated body-mass measurements of adult Asian timber elephants with known ages (18-72 years). The paper fitted",
                 "GAMMs (mgcv::gamm) of log body mass with a cubic regression smooth of within-individual age by sex (k = 15),",
                 "age at last measurement (linear and quadratic) for selective disappearance, a terminal-year term, and random",
                 "intercepts for elephant and township. Here the smooth is of age with age at last measurement (ALR) as the",
                 "proxy, which separates within- from between-individual change in the same way. Open step 7 and choose sex",
                 "under 'Separate curves for a group'. Lifespan is the age at death for elephants that died (others alive or",
                 "unknown). Data: Dryad 10.5061/dryad.5dv41ns59 (CC0); code Zenodo 10.5281/zenodo.6460236. "))
)

# Listed by first-author surname, as in the manuscript's reference list. Labels begin "Surname et al. YEAR,",
# so sorting them is the surname order; radix keeps it identical on every locale.
EXAMPLE_CHOICES <- local({
  labs <- vapply(EXAMPLES, function(e) e$label, character(1))
  ch <- stats::setNames(names(EXAMPLES), labs)
  ch[order(labs, method = "radix")]
})

# ---------------------------------------------------------------------------
# Column guessing for uploaded data
# ---------------------------------------------------------------------------
guess_mapping <- function(df) {
  cols <- names(df)
  low <- gsub("[^a-z0-9]+", "_", tolower(cols))
  n <- nrow(df)
  is_num <- vapply(df, function(x) {
    ch <- as.character(x)
    present <- !is.na(x) & nzchar(ch)
    if (!any(present)) return(FALSE)
    v <- suppressWarnings(as.numeric(ch[present]))
    isTRUE(mean(is.finite(v)) > 0.95)
  }, logical(1))
  nu <- vapply(df, function(x) length(unique(x[!is.na(x)])), integer(1))
  find_col <- function(patterns, pool) {
    for (p in patterns) {
      h <- which(pool & low == p)
      if (length(h)) return(cols[[h[[1]]]])
    }
    for (p in patterns) {
      h <- which(pool & grepl(p, low, fixed = TRUE))
      if (length(h)) return(cols[[h[[1]]]])
    }
    ""
  }

  id_hits <- which(nu >= 2 & nu < n & grepl("(^|_)id($|_)|individual|animal|subject|ring|(^|_)ind($|_)", low))
  id <- if (length(id_hits)) cols[[id_hits[[which.max(nu[id_hits])]]]] else cols[[1]]

  excl_age <- grepl("mean|delta|centr|age2|age_2|_sq|sq_|squared|paternal|maternal|sperm|alr|afr|first|last|entry|death|lifespan", low)
  age <- find_col(c("age", "time", "occasion", "year"), is_num & !excl_age & cols != id)
  if (!nzchar(age)) age <- cols[[min(2, length(cols))]]

  excl_trait <- grepl("(^|_)ls($|_)|lifespan|alr|afr|mean|delta|age|censor|(^|_)rep($|_)|obs|(^|_)id($|_)|year", low)
  trait <- find_col(c("trait", "count", "fecund", "fertil", "offspring", "egg", "clutch", "value", "phenotype",
                      "mass", "weight", "size", "date", "score"), is_num & !excl_trait & cols != id & cols != age)
  if (!nzchar(trait)) {
    h <- which(is_num & !excl_trait & cols != id & cols != age & nu > 2)
    trait <- if (length(h)) cols[[h[[1]]]] else cols[[min(3, length(cols))]]
  }

  life <- find_col(c("ls", "lifespan", "life_span", "longevity", "age_at_death", "death_age"), is_num)
  alr <- find_col(c("alr", "age_last_record", "last_record", "age_at_last"), is_num)
  afr <- find_col(c("afr", "age_first_record", "first_record", "age_at_first", "entry_age"), is_num)
  cond <- find_col(c("condition", "state", "body_condition", "quality"), rep(TRUE, length(cols)))
  list(
    id = id, age = age, trait = trait,
    alr = if (nzchar(alr)) alr else "__AUTO_LAST__",
    life = life,
    entry = if (nzchar(afr)) afr else "__AUTO_FIRST__",
    condition = cond, covars = character(0), cov_factor = character(0), cov_int = character(0), group = "", nested = TRUE, random = character(0),
    censor = "", censor_value = "", start_mode = "afr", start_age = NA_real_, age_round = NA_real_, cov_age = character(0),
    trait_specific_ages = TRUE
  )
}

# ---------------------------------------------------------------------------
# Robust reading of uploaded delimited files
# ---------------------------------------------------------------------------
# Detects the separator (comma, semicolon, tab, pipe) and decimal commas, strips a UTF-8 byte-order
# mark, treats common missing-value codes as NA, trims white space, keeps every column as text (so IDs
# such as "007" are not turned into 7; numbers are converted where they are used), repairs blank or
# duplicated column names and drops empty rows and columns. Falls back to latin1 for non-UTF-8 files.
read_user_csv <- function(path, file_name = "") {
  ext <- tolower(tools::file_ext(if (nzchar(file_name %||% "")) file_name else path))
  if (ext %in% c("xls", "xlsx", "xlsm", "ods")) {
    return(list(data = NULL, note = "This is a spreadsheet workbook: save the sheet as CSV (comma- or semicolon-separated) and upload that file."))
  }
  magic <- tryCatch(readBin(path, "raw", n = 8), error = function(e) raw(0))
  if (length(magic) >= 4 && (identical(magic[1:4], as.raw(c(0x50, 0x4b, 0x03, 0x04))) ||
                             identical(magic[1:4], as.raw(c(0xd0, 0xcf, 0x11, 0xe0))))) {
    return(list(data = NULL, note = "This file is a spreadsheet workbook (Excel), not a CSV, even if it is named .csv: open it and save the sheet as CSV."))
  }
  first <- tryCatch(suppressWarnings(readLines(path, n = 60, warn = FALSE, encoding = "UTF-8")), error = function(e) character(0))
  # invalid UTF-8 (e.g. latin1 files) would make the regular expressions below fail: replace bad bytes for sniffing only
  first <- iconv(first, from = "UTF-8", to = "UTF-8", sub = "?")
  first[is.na(first)] <- ""
  first <- first[nzchar(trimws(first))]
  if (!length(first)) return(list(data = NULL, note = "The file is empty or could not be read."))
  hdr <- sub("^\ufeff", "", first[[1]])
  seps <- c(",", ";", "\t", "|")
  cnt <- vapply(seps, function(s) {
    m <- gregexpr(s, hdr, fixed = TRUE)[[1]]
    as.numeric(sum(m > 0))
  }, numeric(1))
  sep <- if (max(cnt) > 0) seps[[which.max(cnt)]] else ","
  dec <- "."
  if (!identical(sep, ",") && length(first) > 1) {
    fields <- trimws(gsub("\"", "", unlist(strsplit(first[-1], sep, fixed = TRUE))))
    if (sum(grepl("^-?[0-9]+,[0-9]+$", fields)) > sum(grepl("^-?[0-9]+\\.[0-9]+$", fields))) dec <- ","
  }
  na_codes <- c("NA", "", ".", "-", "NaN", "N/A", "n/a", "na", "#N/A", "NULL", "null")
  # Read the bytes once, decide the encoding ourselves, convert to UTF-8, then parse from text.
  # Relying on read.csv(fileEncoding = ) makes success depend on the session locale and on which
  # platform the app runs, and it leaves invalid bytes in the returned strings, where later calls
  # such as trimws() fail. Converting first means the rest of the pipeline only ever sees valid UTF-8.
  read_text <- function() {
    raw_bytes <- tryCatch(readBin(path, "raw", n = file.info(path)$size %||% 0), error = function(e) raw(0))
    if (!length(raw_bytes)) return(NULL)
    # strip a UTF-8 byte-order mark; flag the UTF-16 marks, which read.csv cannot handle as text
    if (length(raw_bytes) >= 3 && identical(raw_bytes[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) raw_bytes <- raw_bytes[-(1:3)]
    if (length(raw_bytes) >= 2 && (identical(raw_bytes[1:2], as.raw(c(0xff, 0xfe))) ||
                                   identical(raw_bytes[1:2], as.raw(c(0xfe, 0xff))))) return(NA_character_)
    # Embedded NUL bytes silently truncate a string in R, so a file could lose data without any message.
    # Remove them here and report how many were dropped rather than relying on a warning.
    n_nul <- sum(raw_bytes == as.raw(0))
    if (n_nul > 0) raw_bytes <- raw_bytes[raw_bytes != as.raw(0)]
    txt <- rawToChar(raw_bytes)
    # NOTE: do not declare this string as "bytes". R refuses to pass byte-declared strings to strsplit()
    # and most other string functions, and iconv() returns the declaration unchanged when the input is
    # already valid UTF-8 - which broke reading of every valid file in 0.12.2 to 0.12.5.
    for (enc in c("UTF-8", "latin1", "windows-1252")) {
      conv <- suppressWarnings(iconv(txt, from = enc, to = "UTF-8", sub = NA))
      if (length(conv) == 1L && !is.na(conv)) {
        conv <- as.character(conv)
        return(list(text = conv, encoding = enc, n_nul = n_nul))
      }
    }
    # nothing decoded cleanly: replace what cannot be converted, so parsing still works
    conv <- suppressWarnings(iconv(txt, from = "latin1", to = "UTF-8", sub = "?"))
    if (length(conv) != 1L || is.na(conv)) conv <- txt
    list(text = as.character(conv), encoding = "latin1 (with unconvertible bytes replaced)", n_nul = n_nul)
  }
  decoded <- read_text()
  if (is.null(decoded)) return(list(data = NULL, note = "The file is empty or could not be read."))
  if (is.character(decoded) && length(decoded) == 1L && is.na(decoded)) {
    return(list(data = NULL, note = "This file looks like UTF-16 text (often produced by 'Unicode text' exports): save it as UTF-8 CSV and upload that."))
  }
  enc_used <- decoded$encoding %||% "UTF-8"
  n_nul <- decoded$n_nul %||% 0
  lines_all <- tryCatch(strsplit(decoded$text, "\r\n|\n|\r")[[1]], error = function(e) NULL)
  read_from_text <- function() {
    if (is.null(lines_all) || !length(lines_all)) return(NULL)
    tryCatch(withCallingHandlers(
      utils::read.csv(text = lines_all, sep = sep, stringsAsFactors = FALSE, check.names = FALSE,
                      na.strings = na_codes, strip.white = TRUE, colClasses = "character",
                      comment.char = "", blank.lines.skip = TRUE),
      warning = function(w) invokeRestart("muffleWarning")), error = function(e) NULL)
  }
  # Fall back to reading the file directly if anything about the decoded text is unusable, so that a
  # problem in the decoding step can never stop a readable file from loading.
  read_from_file <- function() {
    tryCatch(withCallingHandlers(
      utils::read.csv(path, sep = sep, stringsAsFactors = FALSE, check.names = FALSE,
                      na.strings = na_codes, strip.white = TRUE, colClasses = "character",
                      comment.char = "", blank.lines.skip = TRUE),
      warning = function(w) invokeRestart("muffleWarning")), error = function(e) NULL)
  }
  x <- read_from_text()
  if (is.null(x) || !ncol(x)) x <- read_from_file()
  if (is.null(x) || !ncol(x)) return(list(data = NULL, note = "The file could not be parsed as a delimited text (CSV) file."))
  nm <- trimws(sub("^\ufeff", "", names(x)))
  blank <- is.na(nm) | !nzchar(nm)
  nm[blank] <- paste0("column_", which(blank))
  names(x) <- make.unique(nm)
  x[] <- lapply(x, function(col) {
    col <- trimws(col)
    col[!is.na(col) & col %in% na_codes] <- NA
    if (identical(dec, ",")) {
      ok <- !is.na(col)
      if (any(ok) && mean(grepl("^-?[0-9]+(,[0-9]+)?$", col[ok])) > 0.9) col <- gsub(",", ".", col, fixed = TRUE)
    }
    col
  })
  if (ncol(x)) x <- x[, vapply(x, function(col) any(!is.na(col)), logical(1)), drop = FALSE]
  if (ncol(x)) x <- x[rowSums(!is.na(x)) > 0, , drop = FALSE]
  rownames(x) <- NULL
  sep_name <- switch(sep, `;` = "semicolon", `\t` = "tab", `|` = "pipe", "comma")
  note <- sprintf("Read %d rows and %d columns (%s-separated%s%s%s).", nrow(x), ncol(x), sep_name,
                  if (identical(dec, ",")) ", decimal commas converted" else "",
                  if (!identical(enc_used, "UTF-8")) sprintf(", read as %s and converted to UTF-8", enc_used) else "",
                  if (n_nul > 0) sprintf("; %d embedded NUL byte(s) removed before reading, which would otherwise have truncated the text silently", n_nul) else "")
  list(data = x, note = note)
}

# Numeric if at least 95% of the non-missing values are numbers, otherwise text.
maybe_numeric <- function(x) {
  if (is.numeric(x) || is.logical(x)) return(x)
  ch <- trimws(as.character(x))
  present <- !is.na(ch) & nzchar(ch)
  if (!any(present)) return(x)
  v <- suppressWarnings(as.numeric(ch))
  if (mean(is.finite(v[present])) >= 0.95) {
    v[!is.finite(v)] <- NA_real_
    v
  } else {
    ch[!present] <- NA_character_
    ch
  }
}
