"""Part G of the audit (0.23.1).
G1  the review's ten correctness findings and the easy recommendations: each fix checked in the source
G2  GAMMs against GLMMs on simulated data: six true ageing shapes x three kinds of selective disappearance (none,
    age-independent, age-dependent), complete and incomplete sampling. The same datasets go through
      GLMM-Q  the parametric models with a quadratic ageing function (the app's default),
      GLMM-F  the same with the ageing function chosen by AIC (as the app's ageing-function check does),
      GAMM    a penalised smooth of age (Python port: gamm_py), with ALR additive and age-dependent (s(age, by = ALR)).
    Each approach fits its version of Models 1 (no proxy), 2 (additive ALR) and 4 (age-dependent ALR) by ML.
    Scored: which model wins; false and true detection of age-dependent selective disappearance; how well the
    population curve at the mean ALR recovers the true within-individual trajectory (shape error); peak age.
G3  the same comparison on the eight bundled Gaussian examples: which model each approach prefers, and how far the
    GLMM and GAMM trajectories differ.
Set QUICK=1 for a short run."""
import os, re, sys, time, numpy as np, pandas as pd
sys.path.insert(0, os.path.dirname(__file__))
import port022 as P
import gamm_py as GP
from lmm022 import lmm_ri
QUICK = os.environ.get("QUICK") == "1"
PART = os.environ.get("PART", "all")
ROOT = P.ROOT; APP = os.path.join(ROOT, "inst", "app")
fails = []
def check(label, ok, detail=""):
    print(("PASS " if ok else "FAIL ") + label + (": " + detail if detail else ""))
    if not ok: fails.append(label)

if PART in ("all", "G1"):
    print("== G1 the review's findings, fixed in 0.23.1")
    server = open(os.path.join(APP, "server.R"), encoding="utf-8").read()
    glob = open(os.path.join(APP, "global.R"), encoding="utf-8").read()
    runapp = open(os.path.join(ROOT, "R", "run_app.R"), encoding="utf-8").read()
    diag = open(os.path.join(ROOT, "R", "diagnostics.R"), encoding="utf-8").read()
    export = open(os.path.join(ROOT, "R", "export.R"), encoding="utf-8").read()
    modelfit = open(os.path.join(ROOT, "R", "model-fit.R"), encoding="utf-8").read()
    api = open(os.path.join(ROOT, "R", "api.R"), encoding="utf-8").read()
    formulas = open(os.path.join(ROOT, "R", "formulas.R"), encoding="utf-8").read()
    uitest = open(os.path.join(ROOT, "tests", "testthat", "test-ui-builds.R"), encoding="utf-8").read()
    check("1 the random-slope warning reads the fitted structure", "normalise_slope(r$random_structure" in server and "slope_fitted <- !identical(normalise_slope(input$random_structure" not in server)
    check("2 the suggested analysis offers Model 6 only with a known lifespan", 'has_life = isTRUE(m$has_life) && !isTRUE(m$life_auto)' in server and 'if (isTRUE(m$has_life) && !isTRUE(m$life_auto)) "M6"' in server)
    check("3 the bootstrap caution prints the number that refitted", "1 / ((z$n_ok %||% 0L) + 1)" in server and "z$n_perm, 1 / (z$n_perm + 1)" not in server)
    check("4 the checksum is of the file the app loaded", 'f_loaded <- file.path("data", current_example()$file)' in server)
    check("5 one upload limit, set in one place", "50 * 1024^2" in glob and "maxRequestSize" not in runapp)
    check("6 the random-slope support check covers the _all structures", '"correlated_all", "uncorrelated_all"' in diag)
    check("7 no message says duplicates were averaged", "averaged duplicate" not in export and "Averaging duplicate" not in modelfit and "nothing is averaged" in api)
    check("8 the mean-age note is for Models 3 and 5 only", 'c("M3", "M5"), function(m) m %in% names(r$formulas)' in server)
    check("9 the model script is written as UTF-8", 'file(file, open = "w", encoding = "UTF-8")' in server)
    check("10 the layout test skips without the app sources", 'file.exists(file.path(pkg_root, "inst", "app", "global.R"))' in uitest)
    check("unused server code removed (summary_parts, overview_text, d45)", all(x not in server for x in ("summary_parts", "overview_text", "d45 <-")))
    check("automatic checks cached by signature", "auto_check_cache[[key]]" in server and server.count("assign(key, res, envir = auto_check_cache)") == 2)
    check("exported scripts use the public reader", "disappR:::read_user_csv" not in export and "disappR::disappr_read(" in export)
    check("model names from one source", "MODEL_MEANING[[m]]$name" in formulas)
    check("stale 'moved verbatim' headers gone", not any("Moved verbatim" in open(os.path.join(ROOT, "R", f), encoding="utf-8").read() for f in os.listdir(os.path.join(ROOT, "R")) if f.endswith(".R")))
    check(".Rhistory removed and ignored", not os.path.exists(os.path.join(ROOT, ".Rhistory")) and ".Rhistory" in open(os.path.join(ROOT, ".gitignore")).read())

# ------------------------------------------------------------------ model sets
def z(v): v = np.asarray(v, float); s = v.std(); return (v - v.mean()) / (s if s > 0 else 1.0)
FUNS = ["Linear", "Quadratic", "Cubic", "Logarithmic", "Asymptotic exponential"]
def age_basis(age, fun, ref=None):
    ref = age if ref is None else ref
    m, s = ref.mean(), ref.std()
    zz = (age - m) / s
    if fun == "Linear": return [zz]
    if fun == "Quadratic": return [zz, zz ** 2]
    if fun == "Cubic": return [zz, zz ** 2, zz ** 3]
    if fun == "Logarithmic":
        lo = np.log(age - ref.min() + 1); lr = np.log(ref - ref.min() + 1); return [(lo - lr.mean()) / lr.std()]
    ex = np.exp(-zz); er = np.exp(-(ref - m) / s); return [(ex - er.mean()) / er.std()]
def glmm_design(age, A, fun, model, ref):
    b = age_basis(age, fun, ref)
    cols = [np.ones(len(age))] + b
    if model in ("M2", "M4"): cols.append(A)
    if model == "M4": cols += [c * A for c in b]
    return np.column_stack(cols)
def fit_glmm(d, fun):
    A = z(d.ALR); age = d.age.values
    fits = {m: lmm_ri(glmm_design(age, A, fun, m, age), d.trait.values, d.id.values) for m in ("M1", "M2", "M4")}
    return fits
def glmm_curve(fits, model, fun, grid, ref):
    X = glmm_design(grid, np.zeros(len(grid)), fun, model, ref)
    return X @ fits[model]["beta"]
def fit_gamm(d):
    age = d.age.values; A = z(d.ALR)
    S = GP.Smooth(age)
    fx = S.fixed(age); Zs = S.random(age)
    one = np.ones(len(age))
    fits = {"M1": GP.fit(np.column_stack([one, fx]), [Zs], d.trait.values, d.id.values),
            "M2": GP.fit(np.column_stack([one, fx, A]), [Zs], d.trait.values, d.id.values),
            "M4": GP.fit(np.column_stack([one, fx, A, fx[:, 0] * A]), [Zs, Zs * A[:, None]], d.trait.values, d.id.values)}
    return fits, S
def gamm_curve(fits, S, model, grid):
    one = np.ones(len(grid)); fx = S.fixed(grid); Zs = S.random(grid); zero = np.zeros(len(grid))
    if model == "M1": return GP.predict(fits[model], np.column_stack([one, fx]), [Zs])
    if model == "M2": return GP.predict(fits[model], np.column_stack([one, fx, zero]), [Zs])
    return GP.predict(fits[model], np.column_stack([one, fx, zero, zero]), [Zs, Zs * 0])
def verdict(fits):
    a = {m: fits[m]["aic"] for m in fits}
    best = min(a, key=a.get)
    agedep = a["M2"] - a["M4"] > 2
    return best, agedep, a
def shape_err(est, tru):
    d = (est - est.mean()) - (tru - tru.mean())
    return float(np.sqrt(np.mean(d ** 2)) / (tru.std() if tru.std() > 0 else 1.0))

# ------------------------------------------------------------------ simulation
SHAPES = {
    "quadratic": lambda a: 0.6 * (a - 8) - 0.05 * (a - 8) ** 2,
    "cubic": lambda a: 0.0045 * (a - 1) * (a - 9) * (15 - a) * -1 + 0.0 * a,
    "plateau": lambda a: 3 * (1 - np.exp(-0.45 * (a - 1))),
    "threshold": lambda a: np.where(a <= 9, 0.35 * a, 0.35 * 9 - 0.5 * (a - 9)),
    "logarithmic": lambda a: 2 * np.log(a),
    "late collapse": lambda a: -0.003 * np.exp(0.4 * a),
}
def simulate(shape, sel, seed, n_id=150, mcar=0.0):
    r = np.random.default_rng(seed)
    ls = np.clip(np.round(r.normal(9, 3, n_id)), 3, 16).astype(int)
    zz = (ls - ls.mean()) / ls.std()
    lev = (0.8 * zz if sel == "age-independent" else 0) + r.normal(0, 0.8, n_id)
    slo = (0.8 * zz if sel == "age-dependent" else 0) + r.normal(0, 0.3, n_id)
    rows = []
    for i in range(n_id):
        a = np.arange(1, ls[i] + 1, dtype=float)
        keep = r.random(len(a)) >= mcar
        keep[0] = True
        a = a[keep]
        y = SHAPES[shape](a) + lev[i] + slo[i] * (a - 8) / 8 + r.normal(0, 1, len(a))
        rows.append(pd.DataFrame(dict(id=i, age=a, trait=y)))
    d = pd.concat(rows, ignore_index=True)
    d["ALR"] = d.groupby("id").age.transform("max")
    return d
def peak(grid, curve):
    j = int(np.argmax(curve))
    return grid[j] if 0 < j < len(grid) - 1 else np.nan

def run_dataset(d, truth=None):
    grid = np.linspace(d.age.min(), np.quantile(d.age, 0.9), 60)
    out = {}
    gq = fit_glmm(d, "Quadratic")
    b, ad, a = verdict(gq)
    out["GLMM-Q"] = dict(best=b, agedep=ad, curve=glmm_curve(gq, b, "Quadratic", grid, d.age.values), aic=a)
    fsel = min(FUNS, key=lambda f: lmm_ri(glmm_design(d.age.values, z(d.ALR), f, "M2", d.age.values), d.trait.values, d.id.values)["aic"])
    gf = gq if fsel == "Quadratic" else fit_glmm(d, fsel)
    b, ad, a = verdict(gf)
    out["GLMM-F"] = dict(best=b, agedep=ad, curve=glmm_curve(gf, b, fsel, grid, d.age.values), aic=a, fun=fsel)
    gm, S = fit_gamm(d)
    b, ad, a = verdict(gm)
    out["GAMM"] = dict(best=b, agedep=ad, curve=gamm_curve(gm, S, b, grid), aic=a)
    if truth is not None:
        tru = truth(grid)
        for k in out:
            out[k]["err"] = shape_err(out[k]["curve"], tru)
            out[k]["peak_err"] = abs(peak(grid, out[k]["curve"]) - peak(grid, tru)) if np.isfinite(peak(grid, tru)) else np.nan
    return out, grid

if PART in ("all", "G2"):
    print("\n== G2 simulations: GLMM-Q (quadratic), GLMM-F (function chosen by AIC) and GAMM on the same datasets")
    reps = 6 if QUICK else 40
    scen = [(s, sel, 0.0) for s in SHAPES for sel in ("none", "age-independent", "age-dependent")]
    scen += [(s, sel, 0.3) for s in ("quadratic", "threshold") for sel in ("none", "age-dependent")]
    rows = []
    t0 = time.time()
    for si, (shape, sel, mc) in enumerate(scen):
        for rep in range(reps):
            d = simulate(shape, sel, 10000 * si + rep, mcar=mc)
            res, _ = run_dataset(d, SHAPES[shape])
            for k, v in res.items():
                rows.append(dict(shape=shape, selection=sel, sampling="30% missing" if mc else "complete", approach=k, best=v["best"],
                                 agedep=v["agedep"], err=v["err"], peak_err=v["peak_err"], fun=v.get("fun", "")))
    R = pd.DataFrame(rows)
    R.to_csv(os.path.join(os.path.dirname(__file__), "..", "g_simulations.csv"), index=False)
    print("   %d datasets, %.0f s" % (len(R) // 3, time.time() - t0))
    summ = R.groupby(["shape", "selection", "sampling", "approach"]).agg(
        M1=("best", lambda b: 100 * np.mean(b == "M1")), M2=("best", lambda b: 100 * np.mean(b == "M2")),
        M4=("best", lambda b: 100 * np.mean(b == "M4")), agedep=("agedep", lambda x: 100 * np.mean(x)),
        shape_err=("err", "mean"), peak_err=("peak_err", "mean")).reset_index()
    pd.set_option("display.width", 200); pd.set_option("display.max_rows", 500)
    print(summ.round(2).to_string(index=False))
    # headline numbers
    def rate(sel, appr, col="agedep"):
        x = R[(R.selection == sel) & (R.approach == appr)]
        return 100 * x[col].mean()
    print("\n   Age-dependent selection declared (M4 better than M2 by > 2 AIC), all shapes and sampling:")
    for appr in ("GLMM-Q", "GLMM-F", "GAMM"):
        print("   %-7s  none: %5.1f%%   age-independent: %5.1f%%   age-dependent (power): %5.1f%%   mean shape error %.3f" % (
            appr, rate("none", appr), rate("age-independent", appr), rate("age-dependent", appr), R[R.approach == appr].err.mean()))
    fq = rate("none", "GLMM-Q"); fg = rate("none", "GAMM"); pg = rate("age-dependent", "GAMM")
    check("G2 the GAMM makes fewer false age-dependent calls than the quadratic GLMM when there is no selection", fg < fq, "%.1f%% vs %.1f%%" % (fg, fq))
    check("G2 the GAMM keeps power to detect age-dependent selection (>= 80%)", pg >= 80, "%.1f%%" % pg)
    eg = R[R.approach == "GAMM"].err.mean(); eq = R[R.approach == "GLMM-Q"].err.mean()
    check("G2 the GAMM recovers the true trajectory shape at least as well as the quadratic GLMM on average", eg <= eq, "%.3f vs %.3f" % (eg, eq))
    agree = R.pivot_table(index=["shape", "selection", "sampling"], columns="approach", values="best", aggfunc=lambda b: b.mode().iloc[0])
    print("\n   Most frequent best model per scenario:")
    print(agree.to_string())

if PART in ("all", "G3"):
    print("\n== G3 empirical examples (Gaussian traits)")
    imp = P.SRC["import.R"]
    def example_spec(key):
        i = imp.index("\n  %s = list(" % key); seg = imp[i:i + 3000]
        f = re.search(r'file = "([^"]+)"', seg).group(1)
        mp = seg[seg.index("example_map("):]
        get = lambda k: (re.search(r'\b%s = "([^"]+)"' % k, mp) or [None, None])[1]
        return f, get("id"), get("age"), get("trait"), get("alr")
    keys = ["bichet", "moullec_swift", "pasztor_apollo", "wynn", "bouwhuis", "warner", "mckennaell_weight", "szejnersigal_activity"]
    erows = []
    for key in keys:
        f, idc, agec, trc, alrc = example_spec(key)
        d0 = pd.read_csv(os.path.join(APP, "data", f))
        d = pd.DataFrame(dict(id=d0[idc].astype(str), age=pd.to_numeric(d0[agec], errors="coerce"), trait=pd.to_numeric(d0[trc], errors="coerce")))
        if alrc and alrc in d0: d["ALR"] = pd.to_numeric(d0[alrc], errors="coerce")
        d = d.dropna(subset=["age", "trait"])
        if "ALR" not in d or d.ALR.isna().all(): d["ALR"] = d.groupby("id").age.transform("max")
        d["ALR"] = d.groupby("id").ALR.transform(lambda v: v.dropna().iloc[0] if v.notna().any() else np.nan)
        d = d.dropna().reset_index(drop=True)
        if d.age.nunique() < 5: continue
        res, grid = run_dataset(d)
        cg = res["GAMM"]["curve"]; cq = res["GLMM-Q"]["curve"]; cf = res["GLMM-F"]["curve"]
        sd = cg.std() if cg.std() > 0 else 1.0
        diff = lambda c: float(np.sqrt(np.mean(((c - c.mean()) - (cg - cg.mean())) ** 2)) / sd)
        fmt = lambda a: ", ".join("%s %+.1f" % (m, a[m] - min(a.values())) for m in ("M1", "M2", "M4"))
        erows.append(dict(example=key, n=len(d), individuals=d.id.nunique(), ages=d.age.nunique(),
                          GLMM_Q=res["GLMM-Q"]["best"], GLMM_F=res["GLMM-F"]["best"] + " (" + res["GLMM-F"]["fun"] + ")", GAMM=res["GAMM"]["best"],
                          agedep_Q=res["GLMM-Q"]["agedep"], agedep_F=res["GLMM-F"]["agedep"], agedep_GAMM=res["GAMM"]["agedep"],
                          curve_diff_Q=diff(cq), curve_diff_F=diff(cf),
                          peak_Q=peak(grid, cq), peak_F=peak(grid, cf), peak_GAMM=peak(grid, cg),
                          dAIC_Q=fmt(res["GLMM-Q"]["aic"]), dAIC_GAMM=fmt(res["GAMM"]["aic"])))
        print("   %-22s done (%d records)" % (key, len(d)))
    E = pd.DataFrame(erows)
    E.to_csv(os.path.join(os.path.dirname(__file__), "..", "g_empirical.csv"), index=False)
    pd.set_option("display.width", 250)
    print(E[["example", "n", "individuals", "GLMM_Q", "GLMM_F", "GAMM", "agedep_Q", "agedep_F", "agedep_GAMM"]].to_string(index=False))
    print(E[["example", "curve_diff_Q", "curve_diff_F", "peak_Q", "peak_F", "peak_GAMM"]].round(2).to_string(index=False))
    print(E[["example", "dAIC_Q", "dAIC_GAMM"]].to_string(index=False))
    check("G3 every Gaussian example ran through all three approaches", len(E) == len(keys), "%d of %d" % (len(E), len(keys)))
print("\n%d failure(s)%s" % (len(fails), (": " + "; ".join(fails)) if fails else ""))
