"""Part J of the audit (0.24.2): trait-specific automatic ALR and AFR, and the missingness window from AFR to ALR.
J1 wiring: the rule, the tick box and its help, the defaults (examples unticked; uploads, simulations and the R
   interface ticked), the fingerprint, the exported script, the methods draft, the window, the AFR tile and order
J2 worked examples and edge cases, by an independent implementation of the rule and of the window
J3 the bundled examples: before (0.24.1) and after (0.24.2, unticked and ticked): AFR and ALR, missingness, and the
   documented models refitted (Gaussian examples; Python mixed models, no covariates)
J4 files with several traits: each trait gets its own AFR and ALR when ticked, and all share one when unticked
J5 simulations with records kept when the trait is missing (the app's own simulator deletes them): which ALR is the
   better lifespan proxy, and how the models behave, under five sampling designs
Outputs j_trait_ages.txt and j_snapshot_before_after.md (with QUICK=1 a short run)."""
import os, re, sys, time, numpy as np, pandas as pd
sys.path.insert(0, os.path.dirname(__file__))
import port022 as P
from lmm022 import lmm_ri
QUICK = os.environ.get("QUICK") == "1"
ROOT = P.ROOT; APP = os.path.join(ROOT, "inst", "app")
OUT = os.path.join(os.path.dirname(__file__), "..")
fails = []
snap = []   # markdown lines for the before/after snapshot
def check(label, ok, detail=""):
    print(("PASS " if ok else "FAIL ") + label + (": " + detail if detail else ""))
    if not ok: fails.append(label)
src = lambda f: open(os.path.join(ROOT, f), encoding="utf-8").read()

# ------------------------------------------------------------------ J1 wiring
print("== J1 wiring")
prep, miss, exp_, imp, sim, api = src("R/data-preparation.R"), src("R/missingness.R"), src("R/export.R"), src("R/import.R"), src("R/simulation.R"), src("R/api.R")
server, ui, helpers = src("inst/app/server.R"), src("inst/app/ui.R"), src("R/ui-helpers.R")
check("engine: automatic ALR/AFR from ages with a trait value unless trait_specific_ages is FALSE",
      "trait_specific <- !isFALSE(map$trait_specific_ages)" in prep and "ifelse(is.finite(out$trait), out$age, NA_real_)" in prep)
check("engine: the choice is recorded in meta", "trait_specific_ages = trait_specific," in prep)
check("bundled examples are dataset-wide (the base example mapping and the fly mapping)",
      imp.count("trait_specific_ages = FALSE") == 2 and "FLY_MAPPING" in imp)
check("uploads (guess_mapping), simulations (toy_mapping) and the R interface (disappr_mapping) are trait-specific",
      "trait_specific_ages = TRUE\n  )" in imp and "trait_specific_ages = TRUE)" in sim and "example_map(trait_specific_ages = TRUE)" in api)
check("simulations use the observed AFR (first observed record)", 'entry = "__AUTO_FIRST__",' in sim and 'entry = if (afr_ind) "AFR"' not in sim)
check("the tick box, its help link and the example note are in the Data tab",
      'checkboxInput("trait_ages", "Trait-specific ALR/AFR (default)"' in server and 'actionLink("info_trait_ages"' in server and "output$trait_ages_note" in server)
check("the help panel exists", "trait_ages = info_entry(\"Trait-specific ALR/AFR (default)\"" in helpers)
check("the mapping passes the choice on; before the box renders, the preset decides",
      "trait_specific_ages = if (is.null(input$trait_ages)) !isFALSE(safe_get(preset_map())$trait_specific_ages) else isTRUE(input$trait_ages)" in server)
check("the choice is part of the data fingerprint (no stale stored results)", "isTRUE(m$has_group2), !isFALSE(m$trait_specific_ages)," in server)
check("the exported script computes ALR and AFR the same way", "trait-specific ALR and AFR: only ages with a trait value" in exp_ and "dataset-wide ALR and AFR: every record" in exp_)
check("the methods draft states the definition", "oldest age with a trait value" in exp_ and "its AFR" in exp_)
check("the missingness window opens at the AFR and closes at the ALR the models use",
      "afr_ref <- ifelse(is.finite(entry), pmin(entry, first_trait_age), first_trait_age)" in miss and "end_k <- pmax(alr_k, last_tk, na.rm = TRUE)" in miss)
check("the AFR tile, its outline when it is also the ALR, and the AFR order", '"First record (AFR)"' in miss and "afr_outline" in miss
      and 'afr = ids[order(afr_g, im$alr[mi], na.last = FALSE)]' in server)
check("the disappearance hazard and life table still use the last record of any kind",
      "last <- stats::ave(ia$age, ia$id, FUN = max)" in src("R/disappearance.R") and "im$last_recorded" in src("R/disappearance.R"))
check("the unit tests cover the rule, two traits, zeros, mapped columns, lifespan, defaults, the window and the script",
      all(x in src("tests/testthat/test-0-24-2.R") for x in ("two_traits", "zero is a value", "ALRcol", "__AUTO_LAST__", "EXAMPLES", "build_missing_grid", "model_r_code")))

# ------------------------------------------------------------------ independent implementation
def std_ages(df, idc, agec, trc, alrc=None, entc=None, lifec=None, specific=True):
    d = pd.DataFrame(dict(id=df[idc].astype(str).str.strip(), age=pd.to_numeric(df[agec], errors="coerce"),
                          trait=pd.to_numeric(df[trc], errors="coerce")))
    if alrc: d["alr_col"] = pd.to_numeric(df[alrc], errors="coerce")
    if entc: d["entry_col"] = pd.to_numeric(df[entc], errors="coerce")
    if lifec and lifec in df: d["life"] = pd.to_numeric(df[lifec], errors="coerce")
    d = d[d.id.notna() & (d.id != "") & (d.id != "nan") & np.isfinite(d.age)].copy()
    use = d.age.where(np.isfinite(d.trait)) if specific else d.age
    g = use.groupby(d.id)
    d["alr"] = d.alr_col if alrc else d.id.map(g.max())
    d["entry"] = d.entry_col if entc else d.id.map(g.min())
    return d
def grid(d, rule="new", step=None):
    """Expected occasions per individual (margin 0, AFR start). rule 'new' (0.24.2): AFR to ALR, both included.
    rule 'old' (0.24.1): first trait record to the later of the last record of any kind and the ALR."""
    d = d[np.isfinite(d.age)]
    has = d.groupby("id").trait.apply(lambda v: np.isfinite(v).any())
    d = d[d.id.isin(has.index[has])].copy()
    st = P.resolve_age_step(d.age.values, d.id.values, step)
    if not np.isfinite(st) or st <= 0: st = 1.0
    ids = pd.unique(d.id); pos = {v: i for i, v in enumerate(ids)}; fi = d.id.map(pos).values
    first_age = d.groupby("id").age.min().reindex(ids).values
    ft = d[np.isfinite(d.trait)].groupby("id").age.min().reindex(ids).values
    ent = d.groupby("id").entry.min().reindex(ids).values
    alr = d.groupby("id").alr.mean().reindex(ids).values
    amax = np.nanmax(np.r_[d.age.values, alr[np.isfinite(alr)]])
    afr_ref = np.where(np.isfinite(ent), np.minimum(ent, ft), ft) if rule == "new" else ft
    n_before = np.maximum(0, np.floor((first_age - afr_ref) / st + 1e-9)); anchor = first_age - n_before * st
    k = np.round((d.age.values - anchor[fi]) / st)
    last_k = pd.Series(k).groupby(fi).max().reindex(range(len(ids))).values
    kt = np.where(np.isfinite(d.trait.values), k, np.nan)
    last_tk = pd.Series(kt).groupby(fi).max().reindex(range(len(ids))).values
    alr_k = np.where(np.isfinite(alr), np.round((alr - anchor) / st), np.nan)
    if rule == "new":
        end_k = np.fmax(alr_k, last_tk)
        end_k = np.minimum(end_k, np.floor((amax - anchor) / st + 1e-9)); end_k = np.fmax(end_k, last_tk)
    else:
        end_k = np.fmax(last_k, alr_k)
        end_k = np.minimum(end_k, np.floor((amax - anchor) / st + 1e-9)); end_k = np.maximum(end_k, last_k)
    first_k = np.round((afr_ref - anchor) / st)
    start_k = np.maximum.reduce([np.zeros(len(ids)), first_k, end_k - 4999])
    okt = np.isfinite(d.trait.values)
    obs = set(zip(fi[okt], k[okt]))
    cells = [(i, kk) for i in range(len(ids)) for kk in range(int(start_k[i]), int(end_k[i]) + 1)]
    have = sum(1 for c in cells if c in obs)
    return dict(step=st, cells=len(cells), missing=1 - have / max(1, len(cells)), ids=ids,
                window={ids[i]: (anchor[i] + start_k[i] * st, anchor[i] + end_k[i] * st) for i in range(len(ids))})

# ------------------------------------------------------------------ J2 worked examples
print("\n== J2 worked examples and edge cases (independent implementation)")
ex = pd.DataFrame(dict(id=["P"] * 5, age=[1, 2, 3, 4, 5], A=[np.nan, 5, 7, 6, np.nan], B=[3, 4, 0, np.nan, np.nan]))
t = std_ages(ex, "id", "age", "A", specific=True); u = std_ages(ex, "id", "age", "A", specific=False)
check("your example, ticked: AFR 2, ALR 4", (t.entry.iloc[0], t.alr.iloc[0]) == (2, 4))
check("your example, unticked: AFR 1, ALR 5", (u.entry.iloc[0], u.alr.iloc[0]) == (1, 5))
b = std_ages(ex, "id", "age", "B", specific=True)
check("a second trait of the same file (sampled at 1-3, with a zero at 3): AFR 1, ALR 3", (b.entry.iloc[0], b.alr.iloc[0]) == (1, 3))
gt, gu = grid(t), grid(u)
check("window ticked: ages 2-4, nothing missed", gt["window"]["P"] == (2, 4) and gt["missing"] == 0)
check("window unticked: ages 1-5, two of five missed (records without a trait value at 1 and 5)", gu["window"]["P"] == (1, 5) and abs(gu["missing"] - 0.4) < 1e-9)
go = grid(u, "old")
check("0.24.1 window for comparison: first trait record (2) to the last record (5), one of four missed", go["window"]["P"] == (2, 5) and abs(go["missing"] - 0.25) < 1e-9)
never = pd.DataFrame(dict(id=["N", "N", "P", "P"], age=[1, 2, 1, 2], A=[np.nan, np.nan, 1, 2]))
nt = std_ages(never, "id", "age", "A", specific=True)
check("an individual never measured has no automatic ALR when ticked and is left out of the grid", nt[nt.id == "N"].alr.isna().all() and "N" not in grid(nt)["ids"])
mapped = ex.assign(ALRc=9)
check("a mapped ALR column is used as supplied under either option", std_ages(mapped, "id", "age", "A", alrc="ALRc", specific=True).alr.iloc[0] == 9
      and std_ages(mapped, "id", "age", "A", alrc="ALRc", specific=False).alr.iloc[0] == 9)
gap = pd.DataFrame(dict(id=["G"] * 4, age=[1, 2, 3, 4], A=[1.0, np.nan, 3.0, np.nan]))
gg = grid(std_ages(gap, "id", "age", "A", specific=True))
check("a gap between trait values counts as missed; a trailing record without one is outside the ticked window", gg["window"]["G"] == (1, 3) and abs(gg["missing"] - 1 / 3) < 1e-9)
single = pd.DataFrame(dict(id=["S"], age=[3], A=[2.0]))
gs = grid(std_ages(single, "id", "age", "A", specific=True))
check("one record: AFR = ALR, one expected occasion, observed", gs["window"]["S"] == (3, 3) and gs["missing"] == 0)
rt = src("tests/testthat/test-0-24-2.R")
check("the R unit tests assert the same values", all(s in rt for s in ('"alr")[["P"]]), 4)', '"entry")[["P"]]), 2)', '"alr")[["P"]]), 5)', 'sum(g_u$missing[g_u$id == "P"]), 2)')))

# ------------------------------------------------------------------ J3 bundled examples
print("\n== J3 bundled examples: before (0.24.1) and after (0.24.2)")
def example_specs():
    i = imp.index("EXAMPLES <- list("); blk = imp[i:]
    out = []
    ms = list(re.finditer(r"\n  ([a-z_0-9]+) = list\(", blk))
    for j, m in enumerate(ms):
        seg = blk[m.end():ms[j + 1].start() if j + 1 < len(ms) else m.end() + 3500]   # this example's entry only
        f = re.search(r'file = "([^"]+)"', seg)
        if not f: continue
        if "mapping = FLY_MAPPING" in seg:
            mp = imp[imp.index("FLY_MAPPING <- list("):imp.index("FLY_MAPPING <- list(") + 600]
        else:
            mp = seg[seg.index("example_map("):]
            mp = mp[:mp.index("\n    family") if "\n    family" in mp else 900]
        get = lambda k: (re.search(r'\b%s = "([^"]*)"' % k, mp) or [None, None])[1]
        fam = (re.search(r'family = "([a-z0-9]+)"', seg) or [None, "gaussian"])[1]
        fun = (re.search(r'age_function = "([^"]+)"', seg) or [None, "Quadratic"])[1]
        models = re.findall(r'"(M\d+)"', (re.search(r"models = c\(([^)]*)\)", seg) or [None, ""])[1])
        sub = re.search(r'subset = list\(var = "([^"]+)", levels = "([^"]+)"\)', seg)
        alr = get("alr"); ent = get("entry")
        out.append(dict(key=m.group(1), file=f.group(1), id=get("id"), age=get("age"), trait=get("trait"),
                        alr=None if alr in (None, "", "__AUTO_LAST__") else alr, entry=None if ent in (None, "", "__AUTO_FIRST__") else ent,
                        life=get("life"), family=fam, fun=fun, models=models or ["M1", "M2", "M3", "M4", "M5"], subset=sub.groups() if sub else None))
    return out
def z(v): v = np.asarray(v, float); s = np.nanstd(v); return (v - np.nanmean(v)) / (s if s > 0 else 1)
def design(d, fun, model):
    a = d.age.values; zz = z(a)
    B = [zz] if fun == "Linear" else [zz, zz ** 2]
    one = np.ones(len(a))
    A = z(d.alr.values); F = z(d.entry.values)
    if model in ("M3", "M5"):
        g = d.id.values
        mb = [pd.Series(bb).groupby(g).transform("mean").values for bb in B]
        W = [bb - m for bb, m in zip(B, mb)]
        cols = [one, mb[0]] + W
        if model == "M5": cols += [mb[0] * w for w in W]
        return np.column_stack(cols)
    cols = [one] + B
    if model in ("M2", "M4", "M7", "M8"): cols.append(A)
    if model in ("M4", "M8"): cols += [bb * A for bb in B]
    if model in ("M7", "M8"): cols.append(F)
    if model == "M8": cols += [bb * F for bb in B]
    return np.column_stack(cols)
def fit_all(d, fun, models):
    dm = d[np.isfinite(d.trait) & np.isfinite(d.alr) & np.isfinite(d.entry)]
    out = {}
    for m in models:
        if m not in ("M1", "M2", "M3", "M4", "M5", "M7", "M8"): continue
        X = design(dm, fun, m)
        if np.linalg.matrix_rank(X) < X.shape[1]: continue
        f = lmm_ri(X, dm.trait.values, dm.id.values); out[m] = f
    return out, len(dm)
rows = []; mrows = []
specs = example_specs()
for sp in specs:
    p = os.path.join(APP, "data", sp["file"])
    if not os.path.exists(p) or not sp["id"]: continue
    df = pd.read_csv(p)
    if sp["subset"]: df = df[df[sp["subset"][0]].astype(str) == sp["subset"][1]]
    if not all(c in df for c in (sp["id"], sp["age"], sp["trait"])): continue
    res = {}
    for mode in ("wide", "specific"):
        d = std_ages(df, sp["id"], sp["age"], sp["trait"], sp["alr"], sp["entry"], sp["life"], specific=(mode == "specific"))
        res[mode] = d
    w, s = res["wide"], res["specific"]
    per = lambda d, c: d.groupby("id")[c].first()
    ids = per(s, "alr").dropna().index
    d_alr = int((per(w, "alr")[ids] != per(s, "alr")[ids]).sum()); d_afr = int((per(w, "entry")[ids] != per(s, "entry")[ids]).sum())
    g_old, g_w, g_s = grid(w, "old"), grid(w, "new"), grid(s, "new")
    rows.append(dict(example=sp["key"], family=sp["family"], ALR=("column " + sp["alr"]) if sp["alr"] else "automatic",
                     AFR=("column " + sp["entry"]) if sp["entry"] else "automatic", individuals=len(ids), ALR_changed=d_alr, AFR_changed=d_afr,
                     miss_0241=100 * g_old["missing"], miss_unticked=100 * g_w["missing"], miss_ticked=100 * g_s["missing"]))
    if sp["family"] == "gaussian" and (d_alr or d_afr):
        fw, nw = fit_all(w, sp["fun"], sp["models"]); fs, ns = fit_all(s, sp["fun"], sp["models"])
        for m in sorted(set(fw) | set(fs), key=lambda x: int(x[1:])):
            mrows.append(dict(example=sp["key"], model=m, dAIC_unticked=fw[m]["aic"] - min(v["aic"] for v in fw.values()) if m in fw else np.nan,
                              dAIC_ticked=fs[m]["aic"] - min(v["aic"] for v in fs.values()) if m in fs else np.nan, n_unticked=nw, n_ticked=ns))
    print("   %-24s done" % sp["key"])
E = pd.DataFrame(rows); M = pd.DataFrame(mrows)
pd.set_option("display.width", 220); pd.set_option("display.max_rows", 200)
print(E.round(1).to_string(index=False)); print(M.round(1).to_string(index=False))
E.to_csv(os.path.join(OUT, "j_examples.csv"), index=False); M.to_csv(os.path.join(OUT, "j_example_models.csv"), index=False)
check("every bundled example was processed", len(E) == len(specs), "%d of %d" % (len(E), len(specs)))
unt_same = E[(E.ALR_changed == 0) & (E.AFR_changed == 0)]
check("examples without records lacking a trait value have identical AFR, ALR and missingness under both options",
      bool((abs(unt_same.miss_unticked - unt_same.miss_ticked) < 1e-9).all()))
snap += ["## Bundled examples", "",
         "Models of the bundled examples are unchanged in 0.24.2: they open unticked, so ALR and AFR are computed from every record as in 0.24.1. "
         "What changes unticked is the missingness window, which now opens at the AFR (0.24.1: at the first trait record) and closes at the ALR "
         "(0.24.1: at the later of the ALR and the last record of any kind). Ticking the box changes AFR and ALR where records lack a trait value.", "",
         "| Example | ALR | AFR | Individuals | ALR changes when ticked | AFR changes when ticked | Missing % 0.24.1 | Missing % 0.24.2 unticked | Missing % 0.24.2 ticked |",
         "|---|---|---|---|---|---|---|---|---|"]
for r in rows:
    snap.append("| %s | %s | %s | %d | %d | %d | %.1f | %.1f | %.1f |" % (r["example"], r["ALR"], r["AFR"], r["individuals"], r["ALR_changed"], r["AFR_changed"],
                                                                   r["miss_0241"], r["miss_unticked"], r["miss_ticked"]))
snap += ["", "Refitted documented models (Gaussian examples whose AFR or ALR changes; Python mixed models without covariates, so not the app's exact numbers):", "",
         "| Example | Model | ΔAIC unticked | ΔAIC ticked |", "|---|---|---|---|"]
for r in mrows:
    snap.append("| %s | %s | %s | %s |" % (r["example"], r["model"], "%.1f" % r["dAIC_unticked"] if np.isfinite(r["dAIC_unticked"]) else "-",
                                          "%.1f" % r["dAIC_ticked"] if np.isfinite(r["dAIC_ticked"]) else "-"))

# ------------------------------------------------------------------ J4 several traits in one file
print("\n== J4 files with several traits")
multi = [("bichet_2022_tern_immunity.csv", "ID", "age", ["HA", "hapto", "initial_lysis"]),
         ("bouwhuis_2009_great_tit_recruitment.csv", "female", "f_min_age", ["LD", "HD", "ACW", "CS", "FL"])]
trows = []; inv_ok = True; order_ok = True
for f, idc, agec, traits in multi:
    df = pd.read_csv(os.path.join(APP, "data", f))
    wide_alr = None
    for tr in traits:
        s = std_ages(df, idc, agec, tr, specific=True); w = std_ages(df, idc, agec, tr, specific=False)
        sa = s.groupby("id").alr.first().dropna(); wa = w.groupby("id").alr.first()
        sf = s.groupby("id").entry.first().dropna(); wf = w.groupby("id").entry.first()
        if wide_alr is None: wide_alr = wa
        inv_ok &= bool((wa.reindex(wide_alr.index) == wide_alr).all())          # unticked: one ALR for every trait
        order_ok &= bool((sa <= wa[sa.index]).all() and (sf >= wf[sf.index]).all())
        trows.append(dict(file=f.split("_")[0] + " " + f.split("_")[2], trait=tr, measured=int(np.isfinite(pd.to_numeric(df[tr], errors="coerce")).sum()),
                          individuals=len(sa), ALR_earlier=int((sa < wa[sa.index]).sum()), AFR_later=int((sf > wf[sf.index]).sum()),
                          mean_ALR_ticked=sa.mean(), mean_ALR_unticked=wa[sa.index].mean()))
T = pd.DataFrame(trows); print(T.round(2).to_string(index=False))
check("unticked, every trait of a file shares one ALR per individual", inv_ok)
check("ticked, ALR is never later and AFR never earlier than unticked", order_ok)
check("ticked, traits sampled at different ages get different ALR (at least one file)", bool((T.groupby("file").ALR_earlier.nunique() > 1).any()))
snap += ["", "## Several traits in one file", "", "| File | Trait | Records with a value | Individuals | ALR earlier when ticked | AFR later when ticked | Mean ALR ticked | Mean ALR unticked |",
         "|---|---|---|---|---|---|---|---|"]
for r in trows:
    snap.append("| %s | %s | %d | %d | %d | %d | %.2f | %.2f |" % (r["file"], r["trait"], r["measured"], r["individuals"], r["ALR_earlier"], r["AFR_later"], r["mean_ALR_ticked"], r["mean_ALR_unticked"]))

# ------------------------------------------------------------------ J5 simulations
print("\n== J5 simulations with records kept when the trait is missing")
def simulate(design, sel, seed, n_id=250):
    r = np.random.default_rng(seed)
    ls = np.clip(np.round(r.normal(10, 3, n_id)), 2, 18).astype(int); zl = (ls - ls.mean()) / ls.std()
    lev = (0.8 * zl if sel == "age-independent" else 0) + r.normal(0, 0.8, n_id)
    slo = (0.8 * zl if sel == "age-dependent" else 0) + r.normal(0, 0.3, n_id)
    rows = []
    for i in range(n_id):
        for a in range(1, ls[i] + 1):
            y = 0.8 * (a - 6) - 0.08 * (a - 6) ** 2 + lev[i] + slo[i] * (a - 6) / 6 + r.normal(0, 1)
            seen = r.random() < 0.9
            if design == "trait MCAR 50%": meas = r.random() < 0.5
            elif design == "trait stops 2 before death": meas = a <= ls[i] - 2 and r.random() < 0.8
            elif design == "trait starts at 3": meas = a >= 3 and r.random() < 0.8
            elif design == "trait missing when old": meas = r.random() < 1 / (1 + np.exp(0.6 * (a - 9)))
            else: meas = True
            if design == "no records without the trait" and not meas: continue
            if seen or meas:
                rows.append((i, a, y if meas else np.nan, ls[i]))
    return pd.DataFrame(rows, columns=["id", "age", "trait", "life"])
TRUE = lambda a: 0.8 * (a - 6) - 0.08 * (a - 6) ** 2
designs = ["no records without the trait", "trait MCAR 50%", "trait stops 2 before death", "trait starts at 3", "trait missing when old"]
reps = 6 if QUICK else 30
srows = []; t0 = time.time()
for di, des in enumerate(designs):
    for sel in ("none", "age-independent", "age-dependent"):
        for rep in range(reps):
            raw = simulate(des, sel, 50000 + 1000 * di + 100 * ["none", "age-independent", "age-dependent"].index(sel) + rep)
            out = {}
            for mode in ("specific", "wide"):
                d = std_ages(raw, "id", "age", "trait", lifec="life", specific=(mode == "specific"))
                dm = d[np.isfinite(d.trait)]
                ind = dm.groupby("id").agg(alr=("alr", "first"), life=("life", "first"), mean_age=("age", "mean"))
                fits = {m: lmm_ri(design(dm, "Quadratic", m), dm.trait.values, dm.id.values) for m in ("M1", "M2", "M4")}
                grid_age = np.arange(1, 13, dtype=float)
                a = dm.age.values; mu, sd = a.mean(), a.std()
                zz = (grid_age - mu) / sd
                b = fits["M4"]["beta"]; pred = b[0] + b[1] * zz + b[2] * zz ** 2
                tru = TRUE(grid_age); err = np.sqrt(np.mean(((pred - pred.mean()) - (tru - tru.mean())) ** 2)) / tru.std()
                out[mode] = dict(r_ls=np.corrcoef(ind.alr, ind.life)[0, 1], agedep=fits["M2"]["aic"] - fits["M4"]["aic"] > 2,
                                 alr_effect=fits["M1"]["aic"] - fits["M2"]["aic"] > 2, err=err,
                                 miss=grid(d)["missing"])
                out["r_mean_age"] = np.corrcoef(ind.mean_age, ind.life)[0, 1]
            srows.append(dict(design=des, selection=sel, r_ALR_LS_ticked=out["specific"]["r_ls"], r_ALR_LS_unticked=out["wide"]["r_ls"],
                              r_meanage_LS=out["r_mean_age"], agedep_ticked=out["specific"]["agedep"], agedep_unticked=out["wide"]["agedep"],
                              alr_ticked=out["specific"]["alr_effect"], alr_unticked=out["wide"]["alr_effect"],
                              err_ticked=out["specific"]["err"], err_unticked=out["wide"]["err"],
                              miss_ticked=out["specific"]["miss"], miss_unticked=out["wide"]["miss"]))
S = pd.DataFrame(srows); S.to_csv(os.path.join(OUT, "j_simulations.csv"), index=False)
print("   %d datasets, %.0f s" % (len(S), time.time() - t0))
agg = S.groupby(["design", "selection"]).agg(
    r_ALR_LS_ticked=("r_ALR_LS_ticked", "mean"), r_ALR_LS_unticked=("r_ALR_LS_unticked", "mean"), r_meanage_LS=("r_meanage_LS", "mean"),
    agedep_ticked=("agedep_ticked", lambda x: 100 * x.mean()), agedep_unticked=("agedep_unticked", lambda x: 100 * x.mean()),
    ALR_term_ticked=("alr_ticked", lambda x: 100 * x.mean()), ALR_term_unticked=("alr_unticked", lambda x: 100 * x.mean()),
    err_ticked=("err_ticked", "mean"), err_unticked=("err_unticked", "mean"),
    miss_ticked=("miss_ticked", lambda x: 100 * x.mean()), miss_unticked=("miss_unticked", lambda x: 100 * x.mean())).reset_index()
print(agg.round(2).to_string(index=False))
same = S[S.design == "no records without the trait"]
check("without records lacking the trait (the app's simulator), both options are identical",
      bool((abs(same.r_ALR_LS_ticked - same.r_ALR_LS_unticked) < 1e-12).all() and (same.agedep_ticked == same.agedep_unticked).all()))
check("every simulated dataset fitted under both options", len(S) == len(designs) * 3 * reps)
snap += ["", "## Simulations with records kept when the trait is missing", "",
         "Mean over %d datasets per row. r = correlation with true lifespan. 'Age-dependent' and 'ALR term' = %% of datasets where Model 4 beats Model 2, "
         "or Model 2 beats Model 1, by more than 2 AIC. Error = shape error of Model 4's trajectory (relative to the true curve's SD)." % reps, "",
         "| Design | Selection | r(ALR, LS) ticked | r(ALR, LS) unticked | r(mean age, LS) | Age-dependent ticked | Age-dependent unticked | ALR term ticked | ALR term unticked | Error ticked | Error unticked | Missing % ticked | Missing % unticked |",
         "|---|---|---|---|---|---|---|---|---|---|---|---|---|"]
for _, r in agg.iterrows():
    snap.append("| %s | %s | %.2f | %.2f | %.2f | %.0f | %.0f | %.0f | %.0f | %.3f | %.3f | %.1f | %.1f |" % tuple(r.values))
with open(os.path.join(OUT, "j_snapshot_before_after.md"), "w", encoding="utf-8") as fh:
    fh.write("# 0.24.1 against 0.24.2: trait-specific ALR/AFR and the missingness window\n\nGenerated by scripts/audit_j_trait_ages.py. "
             "The code changes are in j_code_diff_0_24_1_to_0_24_4.patch.\n\n" + "\n".join(snap) + "\n")
print("\n%d failure(s)%s" % (len(fails), (": " + "; ".join(fails)) if fails else ""))
