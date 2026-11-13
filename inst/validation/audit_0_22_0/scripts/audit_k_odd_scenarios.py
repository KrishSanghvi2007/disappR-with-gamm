"""Part K of the audit (0.24.3): odd missingness x trait x age scenarios, with lifespan (LS) mapped and ALR either
automatic (following the tick box, so depending on the trait) or mapped as a column that differs from LS.
K1 the tick option reaches everything it should: grid, AFR-ALR and ALR-mean age correlations, proxy tables, data
   checks; and nothing it should not (hazard, life table, the LS-before-last-record check)
K2 odd scenarios (independent implementation): two traits in one file sampled on different schedules, trait values
   missing not at random (low values missed more when old), sightings after the last measurement, a mapped ALR that is
   the last sighting (different from LS and from the last trait value), and an inconsistent mapped ALR earlier than
   the last trait value. Invariants checked on every dataset; proxies and models compared
Outputs k_odd_scenarios.txt and k_scenarios.csv (QUICK=1 for a short run)."""
import os, re, sys, time, numpy as np, pandas as pd
sys.path.insert(0, os.path.dirname(__file__))
import port022 as P
from lmm022 import lmm_ri
QUICK = os.environ.get("QUICK") == "1"
ROOT = P.ROOT; OUT = os.path.join(os.path.dirname(__file__), "..")
fails = []
def check(label, ok, detail=""):
    print(("PASS " if ok else "FAIL ") + label + (": " + detail if detail else ""))
    if not ok: fails.append(label)
src = lambda f: open(os.path.join(ROOT, f), encoding="utf-8").read()

print("== K1 what the tick option reaches")
server, val, dis, miss, prep = src("inst/app/server.R"), src("R/validation.R"), src("R/disappearance.R"), src("R/missingness.R"), src("R/data-preparation.R")
check("grid window: AFR and ALR from the prepared data (which follow the tick)", "afr_ref <- ifelse(is.finite(entry), pmin(entry, first_trait_age), first_trait_age)" in miss
      and "alr_i <- as.numeric(tapply(if (\"alr\" %in% names(d)) d$alr" in miss)
check("AFR-ALR agreement uses the prepared AFR and ALR", "afr <- ifelse(is.finite(im$entry), im$entry, im$first_recorded)\n  alr <- im$alr" in dis)
check("ALR-mean age and ALR-LS correlations use the prepared ALR; mean age always from ages with a trait value",
      "r_mean_alr = cor_safe(im$mean_age, im$alr)" in miss and "out$mean_age <- as.numeric(tapply(dat$age[okt], fo, mean))" in prep)
check("proxy tables (ALR, mean age, LS, AFR) use the prepared values", "v <- switch(proxy, ALR = im$alr, `Mean age` = im$mean_age, LS = im$lifespan, AFR = im$entry, im$alr)" in dis)
check("data check 'mapped ALR vs last age' compares with the last age the tick implies", "last_ref <- if (trait_specific && !is.null(im$last_observed)) im$last_observed else im$last_recorded" in val)
check("data check 'LS - ALR' uses the ALR the models use when ticked", "(if (trait_specific) im$alr else im$last_recorded)" in val)
check("the ALR order of the grid breaks ties by the same AFR", "alr = ids[order(im$alr[mi], afr_g, na.last = FALSE)]" in server)
check("unchanged by design: LS earlier than the last record of any kind is still an impossible value",
      "im$lifespan < im$last_recorded - 1e-8" in val)
check("unchanged by design: hazard and life table use each individual's last record of any kind",
      "end <- if (known) ifelse(is.finite(im$lifespan), im$lifespan, im$last_recorded) else im$last_recorded" in dis)
app = src("tests/scripts/app_test.R")
check("the app server test toggles the box and redraws the grid, the proxies and the checks under each option",
      "session$setInputs(trait_ages = FALSE)" in app and "session$setInputs(trait_ages = TRUE, heat_order = \"alr\")" in app and "trait_ages_note: no benchmark note" in app)

# ------------------------------------------------------------------ independent implementation
def prepare(df, trait, specific, alr_col=None):
    d = df[["id", "age", trait, "LS"] + ([alr_col] if alr_col else [])].rename(columns={trait: "trait", "LS": "life"}).copy()
    use = d.age.where(np.isfinite(d.trait)) if specific else d.age
    g = use.groupby(d.id)
    d["alr"] = d[alr_col] if alr_col else d.id.map(g.max())
    d["entry"] = d.id.map(g.min())
    return d
def window(d):
    """AFR to ALR, both included (step 1); an observed trait record after a mapped ALR stays inside."""
    ok = d[np.isfinite(d.trait)]
    has = ok.id.unique()
    d = d[d.id.isin(has)]
    ft = ok.groupby("id").age.min(); lt = ok.groupby("id").age.max()
    ent = d.groupby("id").entry.min(); alr = d.groupby("id").alr.first()
    start = np.minimum(ent.reindex(has), ft.reindex(has)); end = np.fmax(alr.reindex(has), lt.reindex(has))
    obs = set(zip(ok.id, ok.age))
    cells = [(i, a) for i in has for a in range(int(start[i]), int(end[i]) + 1)]
    return start, end, cells, obs

def simulate(sel, seed, n_id=300):
    """Two traits, LS known, sightings every year with p = 0.85 until death. Trait A: measured at 70% of sightings,
    but low values are missed more often when old (missing not at random x age). Trait B: measured only from age 2 to
    age 7 (a life-stage trait). The mapped ALR column is the last sighting, which can be earlier than LS."""
    r = np.random.default_rng(seed)
    ls = np.clip(np.round(r.normal(9, 3, n_id)), 2, 16).astype(int); zl = (ls - ls.mean()) / ls.std()
    lev = (0.8 * zl if sel == "age-independent" else 0) + r.normal(0, 0.8, n_id)
    slo = (0.8 * zl if sel == "age-dependent" else 0) + r.normal(0, 0.3, n_id)
    rows = []
    for i in range(n_id):
        for a in range(1, ls[i] + 1):
            if r.random() > 0.85: continue
            ya = 0.8 * (a - 5) - 0.08 * (a - 5) ** 2 + lev[i] + slo[i] * (a - 5) / 5 + r.normal(0, 1)
            yb = 0.5 * (a - 4) + 0.5 * lev[i] + r.normal(0, 1)
            pa = 0.7 / (1 + np.exp(-(1.5 + 1.5 * (ya - 0) - 0.25 * (a - 5))))      # low values missed more when old
            rows.append((i, a, ya if r.random() < pa else np.nan, yb if (2 <= a <= 7 and r.random() < 0.8) else np.nan, ls[i]))
    d = pd.DataFrame(rows, columns=["id", "age", "A", "B", "LS"])
    d["ALR_sighting"] = d.groupby("id").age.transform("max")
    # an inconsistent mapped ALR for 5% of individuals: one step before their last sighting
    bad = r.choice(d.id.unique(), max(1, n_id // 20), replace=False)
    d["ALR_bad"] = np.where(d.id.isin(bad), d.ALR_sighting - 1, d.ALR_sighting)
    return d, set(bad)

print("\n== K2 odd scenarios")
reps = 5 if QUICK else 25
rows = []; inv = dict(inside=True, end_at_alr=True, order=True, mapped_same=True, alr_le_ls=True, bad_kept=True); t0 = time.time()
for sel in ("none", "age-independent", "age-dependent"):
    for rep in range(reps):
        raw, bad = simulate(sel, 7000 + 100 * ["none", "age-independent", "age-dependent"].index(sel) + rep)
        for trait in ("A", "B"):
            for variant, alr_col in (("automatic ALR", None), ("mapped ALR = last sighting", "ALR_sighting"), ("mapped ALR, 5% inconsistent", "ALR_bad")):
                res = {}
                for specific in (True, False):
                    d = prepare(raw, trait, specific, alr_col)
                    start, end, cells, obs = window(d)
                    inv["inside"] &= all(start[i] <= a <= end[i] for i, a in obs)
                    ind = d[np.isfinite(d.trait)].groupby("id").agg(alr=("alr", "first"), life=("life", "first"), mean_age=("age", "mean"), entry=("entry", "first"))
                    if alr_col is None:
                        inv["end_at_alr"] &= bool((end == d.groupby("id").alr.first().reindex(end.index)).all())
                        inv["alr_le_ls"] &= bool((ind.alr <= ind.life).all())
                    if alr_col == "ALR_bad":
                        inv["bad_kept"] &= bool(all(end[i] >= d[(d.id == i) & np.isfinite(d.trait)].age.max() for i in bad if i in end.index))
                    dm = d[np.isfinite(d.trait)].copy()
                    zA = lambda v: (v - v.mean()) / v.std()
                    a = zA(dm.age.values); A = zA(dm.alr.values); L = zA(dm.life.values); one = np.ones(len(dm))
                    X = {"M1": np.column_stack([one, a, a ** 2]), "M2": np.column_stack([one, a, a ** 2, A]),
                         "M4": np.column_stack([one, a, a ** 2, A, a * A, a ** 2 * A]), "M2_LS": np.column_stack([one, a, a ** 2, L]),
                         "M6": np.column_stack([one, a, a ** 2, L, a * L, a ** 2 * L])}
                    f = {k: lmm_ri(v, dm.trait.values, dm.id.values) for k, v in X.items()}
                    res[specific] = dict(r_alr_ls=np.corrcoef(ind.alr, ind.life)[0, 1], r_mean_ls=np.corrcoef(ind.mean_age, ind.life)[0, 1],
                                         r_alr_mean=np.corrcoef(ind.alr, ind.mean_age)[0, 1], r_afr_alr=np.corrcoef(ind.entry, ind.alr)[0, 1] if ind.entry.std() > 0 else np.nan,
                                         miss=1 - sum(1 for c in cells if c in obs) / len(cells),
                                         agedep=f["M2"]["aic"] - f["M4"]["aic"] > 2, agedep_ls=f["M2_LS"]["aic"] - f["M6"]["aic"] > 2,
                                         term=f["M1"]["aic"] - f["M2"]["aic"] > 2, term_ls=f["M1"]["aic"] - f["M2_LS"]["aic"] > 2,
                                         alr_mean=ind.alr.mean())
                if alr_col is not None:
                    inv["mapped_same"] &= abs(res[True]["alr_mean"] - res[False]["alr_mean"]) < 1e-12
                inv["order"] &= res[True]["alr_mean"] <= res[False]["alr_mean"] + 1e-12 or alr_col is not None
                for specific in (True, False):
                    rows.append(dict(selection=sel, trait=trait, variant=variant, option="ticked" if specific else "unticked", **{k: v for k, v in res[specific].items() if k != "alr_mean"}))
K = pd.DataFrame(rows); K.to_csv(os.path.join(OUT, "k_scenarios.csv"), index=False)
print("   %d datasets x 2 traits x 3 ALR variants x 2 options, %.0f s" % (3 * reps, time.time() - t0))
agg = K.groupby(["trait", "variant", "selection", "option"]).agg(
    r_ALR_LS=("r_alr_ls", "mean"), r_meanage_LS=("r_mean_ls", "mean"), r_ALR_meanage=("r_alr_mean", "mean"), r_AFR_ALR=("r_afr_alr", "mean"),
    missing_pct=("miss", lambda x: 100 * x.mean()), ALR_term=("term", lambda x: 100 * x.mean()), LS_term=("term_ls", lambda x: 100 * x.mean()),
    agedep_ALR=("agedep", lambda x: 100 * x.mean()), agedep_LS=("agedep_ls", lambda x: 100 * x.mean())).reset_index()
pd.set_option("display.width", 230); pd.set_option("display.max_rows", 300)
print(agg.round(2).to_string(index=False))
check("every observed trait record lies inside its window (all traits, variants and options)", inv["inside"])
check("with automatic ALR the window ends exactly at the ALR", inv["end_at_alr"])
check("automatic ALR never exceeds LS", inv["alr_le_ls"])
check("ticked automatic ALR is never later than unticked", inv["order"])
check("a mapped ALR is the same under both options", inv["mapped_same"])
check("an inconsistent mapped ALR (earlier than the last trait value) does not push observed records out of the window", inv["bad_kept"])
na = agg[(agg.selection == "none")]
ad = agg[agg.selection == "age-dependent"]
check("age-dependent selection is detected with ALR as often as with known LS for trait A (every variant and option)",
      bool((ad[ad.trait == "A"].agedep_ALR >= ad[ad.trait == "A"].agedep_LS - 10).all()))
print("   false age-dependent calls with no selection, ALR against known LS (percent of datasets):")
print(na[["trait", "variant", "option", "agedep_ALR", "agedep_LS"]].round(0).to_string(index=False))
print("\n%d failure(s)%s" % (len(fails), (": " + "; ".join(fails)) if fails else ""))
