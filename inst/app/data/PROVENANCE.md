# Provenance of the bundled empirical examples

Every file below was checked cell by cell against the archived original.

## Sources, DOIs and licences

The bundled data files are redistributed under the terms of their sources; they are not covered by the package's
MIT licence. Dryad publishes all data under a CC0 public-domain dedication, so the Dryad files may be redistributed
without restriction. Cite the paper and the dataset when you use an example.

| File | Paper (DOI) | Data source (DOI) | Licence |
|---|---|---|---|
| fly_fecundity.csv | Sanghvi et al. 2025, *American Naturalist* (DOI to add) | supplied by the authors | **to confirm** |
| bichet_2022_tern_immunity.csv | Bichet et al. 2022, *J. Anim. Ecol.* 91: 458-469 (10.1111/1365-2656.13642) | Dryad 10.5061/dryad.63xsj3v3q | CC0 1.0 |
| bichet_2022_marmot_immunity.csv | Bichet et al. 2022, *Ecol. Evol.* (10.1002/ece3.9094) | Dryad 10.5061/dryad.bvq83bk5d | CC0 1.0 |
| moullec_2023_alpine_swift_reproduction.csv | Moullec, Reichert & Bize 2023, *Front. Ecol. Evol.* 11: 983266 (10.3389/fevo.2023.983266) | supplementary material of the article | CC BY 4.0 (the article's licence; **confirm** that the file came from the article's supplement) |
| pasztor_2022_clouded_apollo_body_size.csv | Pásztor et al. 2022, *Ecol. Evol.* (10.1002/ece3.9668) | Dryad 10.5061/dryad.jq2bvq8bp | CC0 1.0 |
| wynn_2025_tern_navigation.csv | Wynn, Kürten, Moiron & Bouwhuis 2025, *J. Anim. Ecol.* 94: 535-544 (10.1111/1365-2656.14231) | Dryad 10.5061/dryad.m63xsj4cj | CC0 1.0 |
| sanghvi_2022_beetle_female_fecundity.csv | Sanghvi et al. 2022, *Evolution* 76: 1868-1882 (10.1111/evo.14567) | Dryad 10.5061/dryad.nvx0k6dvt | CC0 1.0 |
| allain_2023_chipmunk_reproduction.csv | Allain et al., *Oikos* 2024, online 2023 (10.1111/oik.09944) | Dryad 10.5061/dryad.pg4f4qrwr | CC0 1.0 |
| bouwhuis_2009_great_tit_recruitment.csv | Bouwhuis et al. 2009, *Proc. R. Soc. B* 276: 2769-2777 (10.1098/rspb.2009.0457) | no public archive found; supplied by the authors | **permission to confirm** |
| warner_2016_turtle_reproduction.csv | Warner et al. 2016, *PNAS* 113: 6502-6507 (10.1073/pnas.1600035113) | Dryad 10.5061/dryad.g2r87 | CC0 1.0 |
| mckennaell_2023_soay_breeding_survival.csv, mckennaell_2023_soay_offspring_weight.csv | McKenna-Ell et al. 2023, *Biol. Lett.* (10.1098/rsbl.2023.0050) | Dryad 10.5061/dryad.stqjq2c7s | CC0 1.0 |
| szejnersigal_2025_bee_activity.csv | Szejner-Sigal et al. 2025, *Proc. R. Soc. B* (10.1098/rspb.2024.2637) | Dryad 10.5061/dryad.j9kd51cq3 | CC0 1.0 |

Three entries still need the authors' input before a public release: the fly data (licence and DOI), the great tit
data (written permission, or a public deposit), and the swift file's source.

## Checks against the archived originals

| File | Rows vs original | Columns | Values | Derived columns |
|---|---|---|---|---|
| fly_fecundity.csv | unchanged | unchanged | unchanged | none (as supplied with the app) |
| bichet_2022_tern_immunity.csv | 1097 / 1097 | all kept; `year of sampling` renamed `year_sampling` | identical | `age` = year_sampling - year_of_birth |
| sanghvi_2022_beetle_female_fecundity.csv | 6001 / 6001 | all kept | identical | re-saved as UTF-8; derived column `individual` added (see below) |
| allain_2023_chipmunk_reproduction.csv | 487 / 487 | all kept | identical | none |
| warner_2016_turtle_reproduction.csv | 2205 / 2205 | all kept | identical | none |
| bouwhuis_2009_great_tit_recruitment.csv | 7126 / 7126 | all kept | identical (md5 match) | none |
| wynn_2025_tern_navigation.csv | **2689 / 126483** | subset + derived | identical for retained fields | `deflection`, `season`, `step_km`, `move_bearing`, `goal_bearing`, `goal_lat`, `goal_lon`, `sex` |

## The tern navigation file is the only transformed dataset

The response variable analysed by Wynn et al. (2025), instantaneous deflection from the goal, is not in the
archived file, which contains raw geolocator positions. It is computed here as:

1. each track is split into autumn and spring migration using the departure and arrival dates already in the file;
2. the goal is the individual's mean position between arrival at and departure from the wintering area (autumn)
   or the breeding colony at 53.5111 N, 8.1056 E (spring);
3. deflection is the absolute angle between the bearing to the next fix and the bearing to the goal;
4. steps shorter than 100 km are dropped, following the paper's stopover filter.

This keeps 2,689 fixes from 186 tracks and 78 of the 102 birds; tracks without wintering dates cannot be
classified and are lost. The published estimates are an among-individual effect of -0.870 degrees per year and
a within-individual effect of +0.851; this derivation gives -0.769 and +0.903. The sign pattern is unchanged at
50 km (-0.66 / +1.24) and 200 km (-0.55 / +1.58) thresholds, but disappears with no distance filter
(-0.06 / +3.61), so the stopover filter matters, exactly as the paper states.

`sex_raw` preserves the original coding. `sex` harmonises the numeric codes to letters: nineteen individuals
appear under both codings, and code 1 always coincides with m (nine individuals) and 2 with f (nine), never the
reverse, so the mapping is supported by the data rather than assumed.

## Added in 0.9.5

Supplied by Krish from the papers' data archives (Soay sheep: doi:10.5061/dryad.stqjq2c7s; bees:
doi:10.5061/dryad.j9kd51cq3) and bundled byte for byte under new file names.

| File | Rows | Columns | Values | Derived columns |
|---|---|---|---|---|
| mckennaell_2023_soay_breeding_survival.csv (`fecundityoffsurv.csv`) | 3173 records of 762 females (the paper's breeding-probability sample; 2573 non-blank offspring-survival records of 714 females) | unchanged | unchanged | none |
| mckennaell_2023_soay_offspring_weight.csv (`offspringwt.csv`) | 2317 lambs of 649 mothers (the paper's sample) | unchanged | unchanged | none |
| szejnersigal_2025_bee_activity.csv (`Activity.csv`) | 808 records of 184 bees (414 female, 394 male records); the paper's text reports 199 bees in the activity experiment | unchanged | unchanged | none |
| bichet_2022_marmot_immunity.csv | 173 / 173 | all kept; `age_at_last_observation` renamed `ALO`, spaces in names replaced by underscores | identical | `log_leukocyte_concentration` = natural log of `leukocyte_concentration` |
| moullec_2023_alpine_swift_reproduction.csv | 2087 / 2087 | the `reproduction` sheet only; `CSi.day`, `CS`, `BSH`, `BSF` renamed `laying_date`, `clutch_size`, `brood_size_hatching`, `brood_size_fledging` | identical | none |
| pasztor_2022_clouded_apollo_body_size.csv | 3888 rows merged from three archived files (mass 3132, thorax 2847, wing 2144) | mass, thorax and wing joined on individual and measurement date | identical where supplied | SEVERAL - see the note below |


## Derived columns in pasztor_2022_clouded_apollo_body_size.csv

This is the only bundled example whose analysis columns are derived rather than archived. The three archived files
(`mass_data_2014_2020`, `thorax_data_2014_2020`, `wing_data_2014_2020`) hold measurement dates, not ages, so the
variables the paper models had to be rebuilt from them. Each follows the definition given in the paper's Section 2.3:

| Column | How it is derived |
|---|---|
| `age_days` | Days elapsed between a measurement and that individual's first capture. The paper uses this as a minimum estimate of true age. |
| `first_capture` | The day of that year's flight period on which the individual was first caught, counted from the flight-period start dates in the paper's Table 1 (17 Apr 2014, 26 Apr 2015, 22 Apr 2016, 25 Apr 2017, 29 Apr 2018, 21 Apr 2019, 21 Apr 2020). |
| `mean_age` | The individual's mean `age_days` across its measurements. |
| `mean_age_sq` | The mean of `age_days` SQUARED across an individual's measurements - the paper's "mean (age2)". Note this is the mean of the squares, not the square of the mean; the two differ by the within-individual variance of age, and for these data the difference is immaterial (median 4.1 day-squared, and the two forms differ by 0.3 AIC when fitted). |
| `mean_first_capture` | The annual mean of `first_capture`, which the paper uses as a year-level covariate correlated with flight-period length. |
| `wing_length` | Mean of the two forewing measurements (`wfl`, `wfr`); one value per individual, as wing length does not change with age. |
| `thorax_width` | Mean of the two caliper measurements per occasion (`tw1`, `tw2`). |
| `log_body_mass` | Natural log of `body_mass`, as the paper models it. |

Reconstruction checks against the paper: 1,190 individuals with body mass (paper 1,191); 70.2% measured at least
twice (paper 69.35%); 1,313 individuals with thorax width (paper 1,312) and 57.1% repeatedly (paper 56.86%);
once-measured butterflies averaged 0.186 g against 0.201 g for repeatedly measured ones (paper 0.187 and 0.205).
Fitting the paper's best-supported body-mass model reproduces every coefficient to within about one published
standard error, wing length included.


## Derived identifier: sanghvi_2022_beetle_female_fecundity.csv (disappR 0.20.0)
Some IDs in this file contain "?" where a character was lost when the file was re-encoded, so different beetles
share an ID: 15 IDs each covered more than one beetle (for example "B?15" holds one beetle from family Bpi on
treatment AH with lifespan 6, and one from family Btr on treatment AA with lifespan 12). The added column
`individual` joins Block, Family and ID, which identifies every beetle uniquely: 637 individuals, with no
conflicting lifespans or treatments and no two records at the same age. No original value was changed.
