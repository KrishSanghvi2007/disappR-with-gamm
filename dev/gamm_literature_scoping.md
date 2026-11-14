# Open-data GAMM ageing studies: scoping (2026-11-14)

The search ran two ways:

- citation chasing from Brown et al. 2021 and Tsui et al. 2026 (backward through Crossref, 102 and 127 references;
  forward through Semantic Scholar and Europe PMC);
- independent scoping through Europe PMC, Dryad, Zenodo and web search.

Inclusion: a GAM or GAMM of a trait against age, checked in the methods or the authors' code; repeated measures of
individuals; open data.

## Chosen for bundling

| Study | Data | Licence | Traits |
|---|---|---|---|
| Cooper, Bonnet, Osmond, Cockburn & Kruuk 2021, *Am. Nat.* 197: 111-127 (10.1086/711755), superb fairy-wren | Dryad 10.5061/dryad.2547d7wnq (Zenodo mirror 3997828) | CC0 | female survival (Bernoulli), female independent offspring (Poisson), male total, extra-group and within-group offspring (Poisson) |
| Lalande, Lummaa, Aung, Htut, Nyein, Berger & Briga 2022, *J. Evol. Biol.* (preprint 10.1101/2020.12.17.423208), Asian elephants | Dryad 10.5061/dryad.5dv41ns59 (Zenodo 6462306); code Zenodo 6460236 (MIT) | CC0 | log body mass, both sexes, ages 18-72 |

Cooper et al. fitted mgcv/gamm4 thin-plate smooths of age (REML) with random effects for individual and year
(observation-level for Poisson). Lifespan was a linear term, and a final-breeding-season term was included. Table 1
age-smooth edf:

| Trait | n (individuals) | Age edf |
|---|---|---|
| Female survival | 1,896 (751) | 1.000 |
| Female independent offspring | 1,372 (606) | 4.540 |
| Male total offspring | 2,310 (780) | 3.567 |
| Male extra-group offspring | 2,310 (780) | 3.731 |
| Male within-group offspring | 2,310 (780) | 2.019 (n.s.) |

The deposited male survival file (2,896 rows) does not match the paper's sample (2,747), so male survival is not used.

Lalande et al. fitted `gamm(log(bm) ~ terminal + age_last + age_last^2 + s(delta_age, by = sex, bs = "cr", k = 15) +
sex, random = id, Township)`.

- Table S2: by-sex smooth AICc -8,809.1 against a common smooth -8,743.4, a difference of 65.7.
- Table S5: delta-age smooth edf 6.096 (males) and 6.899 (females).

## Promising, not yet verified

Tholstrup, Halvorsen & Dahlgren 2021, *J. Ecol.* (10.1111/1365-2745.13717), the moss *Polytrichastrum formosum*:
Dryad 10.5061/dryad.pg4f4qrpn, CC0. Binary survival and reproduction. The data summary says GAMMs were used, but the
article was not accessible.

## Not suitable

- Hämäläinen et al. 2014 (mouse lemur): no age column in the deposit.
- Worsley et al. 2024 (gut microbiome): the trait must be computed from raw sequence data.
- Hammers et al. 2016 (malaria): no public data.
- Torres et al. 2011: SAS, no data.

Checked and found not to use GAMMs: albatross (Zenodo 5017630), northern elephant seal, Alpine swift, roe deer,
zebra finch metabolic rate, Soay sheep immunity, wild rodent immunity, chimpanzee dysregulation, Briga & Verhulst
2021, Brown 2022 "silver spoons", Raj Pant 2020, Cheynel 2017, Fay 2021, Hammers 2021, Spurgin 2018, Beirne 2015,
Kroeger 2018, Rodríguez-Muñoz 2019 and others.

Unresolved (methods paywalled): meerkat (Thorley), bottlenose dolphin, tern and tit immunosenescence, Daphnia
senescence (R. Soc. Open Sci. 2020).
