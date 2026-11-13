# Stress-test datasets

Realistic and adversarial out-of-sample data used by `tests/stress_test.R`. None of these were used to build the app.

| File | What it stresses |
|---|---|
| wild_bird_annual.csv | Annual breeding records; recruitment age varies with quality; imperfect recapture; birds alive at study end (censoring coded `alive_at_end = yes`); Poisson clutch size; habitat, cohort, observer (rare levels), sex |
| mammal_irregular_days.csv | Body mass at irregular intervals in days (no common schedule); death ages; censored animals |
| ages_from_dates_decimal_years.csv | Ages computed from dates in decimal years (irregular, floating-point ages) |
| staggered_cohorts_lab.csv | Assays every 14 days in two cohorts offset by 7 days; fly IDs nested in vials; zero-inflated counts |
| few_individuals.csv | Seven individuals |
| single_records_heavy.csv | 65% of individuals recorded once |
| messy_semicolon_decimal_comma.csv | Semicolon separator, decimal commas, byte-order mark, NA codes (`NA`, blank, `.`, `-`, `n/a`), trailing spaces in IDs, special characters in column names, `-999` codes |
| covariate_edge_cases.csv | Constant covariate, names that collide after `make.names` (`a b` and `a.b`), a rare level, numeric covariate with 30% missing, logical covariate, ID-like covariate with >300 levels |
| age_edge_cases.csv | Negative and zero ages, text ages, lifespans before the first record, AFR column inconsistent with records |
| count_edge_cases.csv | 60% zeros, individuals with only zeros, extreme counts (100000) |
| two_ages_only.csv, single_age_cross_sectional.csv | Too few distinct ages for quadratic (and linear) functions |
| grouping_timevarying_edge.csv | Missing group labels, singleton groups, IDs repeated across groups, censoring coded TRUE/FALSE, time-varying covariate |
| latin1_semicolon.csv | Latin-1 encoded file (degree sign in a header) |
| tab_with_commas_in_quotes.tsv | Tab separator with commas inside quoted fields |
| leading_zero_ids.csv | IDs `007`, `07` and `7` must stay distinct |
| blank_duplicate_headers.csv | Blank and duplicated column names, empty columns |
| dates_in_age_column.csv | Calendar dates instead of ages (the app must stop with a clear message) |
| header_only.csv | No data rows |
| inf_and_huge_traits.csv | `Inf` and `1e300` trait values |
| all_na_covariate.csv | A covariate with no values |
| excel_renamed.csv | An Excel workbook renamed to .csv |
| a4_selection_censored.csv | Annual mortality depends on the current trait (odds ratio 0.45 per within-age SD); 30% censored |
| a4_age_dependent_selection.csv | Trait-dependent mortality that strengthens with age (A4 trait × age, A6 trend) |
| terminal_decline.csv | Trait drops by 5 units at the last occasion before death; constant 10% annual hazard |
| rising_hazard.csv | Gompertz-like rising hazard with no trait effect (A7) |

A 6,000-individual dataset is generated inside the script for performance checks.
