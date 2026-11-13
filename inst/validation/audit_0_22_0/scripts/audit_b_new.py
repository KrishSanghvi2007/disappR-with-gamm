"""Part B of the 0.22.0 audit: stress tests of the 0.22.0 additions with data built to exercise them.
B1 sampling interval   B2 families   B3 age-dependent variance   B4 stored models   B5 share of wins
B6 peak and onset      B7 random effects against lifespan proxies   B8 methods writer   B9 current-data box
B10 new wiring         B11 the expectations of tests/testthat/test-0-22.R"""
import os, re, sys, hashlib, time, numpy as np, pandas as pd
from scipy import stats
sys.path.insert(0, os.path.dirname(__file__))
import port022 as P
from lmm022 import lmm_ri, lmm_ri_hetero
ROOT = P.ROOT; APP = os.path.join(ROOT, "inst", "app")
server = open(os.path.join(APP, "server.R"), encoding="utf-8").read()
ui = open(os.path.join(APP, "ui.R"), encoding="utf-8").read()
ENG = "".join(P.SRC.values())
QUICK = os.environ.get("QUICK") == "1"
fails = []
def check(label, ok, detail=""):
    print(("PASS " if ok else "FAIL ") + label + (": " + detail if detail else ""))
    if not ok: fails.append(label)
t0 = time.time()

def design_data(kind, n=300, seed=2):
    r = np.random.default_rng(seed); rows = []
    for i in range(n):
        L = r.integers(3, 10)
        if kind == "annual_midyear":
            a = list(np.arange(1, L + 1, 1.0))
            if r.random() < 0.2: a.append(r.integers(1, L) + 0.5)
        elif kind == "half_yearly": a = list(np.arange(1, L + 1, 0.5))
        elif kind == "biennial_extras":
            a = list(np.arange(1, 2 * L, 2.0))
            if r.random() < 0.1: a.append(a[0] + 1)
        elif kind == "irregular": a = list(np.sort(1 + np.cumsum(r.uniform(0.6, 1.4, L))))
        a = np.array(sorted(a))
        rows.append(pd.DataFrame(dict(id="i%03d" % i, age=a, trait=10 - 0.3 * a + r.normal(0, 1, len(a)))))
    return pd.concat(rows, ignore_index=True)

print("== B1 sampling interval: inferred against set")
print("   %-18s %-8s %6s %8s %5s %8s %6s" % ("design", "interval", "step", "miss %", "segs", "maxpair", "off"))
for kind, true_step in [("annual_midyear", 1.0), ("half_yearly", 0.5), ("biennial_extras", 2.0), ("irregular", 1.0)]:
    d = design_data(kind)
    res = {}
    for lab, st in [("auto", None), ("set", true_step)]:
        g = P.missing_grid(d, st); de = P.decomposition_trajectory(d, st)
        mp = int(de["rows"].n_pairs.max()) if len(de["rows"]) else 0
        res[lab] = (g, de, mp)
        print("   %-18s %-8s %6.3g %8.1f %5d %8d %6d" % (kind, lab, g["step"], 100 * g["missing"], de["segments"], mp, de["n_off"]))
    if kind == "annual_midyear":
        check("annual + mid-year extras: auto infers 0.5 (the fault the setting fixes)", res["auto"][0]["step"] == 0.5)
        check("setting 1 restores the annual links (max pairs >= 5x auto)", res["set"][2] >= 5 * res["auto"][2], "%d vs %d" % (res["set"][2], res["auto"][2]))
        check("setting 1 lowers reported missingness", res["set"][0]["missing"] < res["auto"][0]["missing"] / 3)
    if kind == "half_yearly":
        check("true half-yearly design: auto and set agree", res["auto"][0]["step"] == 0.5 and abs(res["auto"][0]["missing"] - res["set"][0]["missing"]) < 1e-12)
d = design_data("annual_midyear")
g = P.missing_grid(d, 0.001)
check("a tiny set interval is coarsened, not a runaway grid", g["cells"] <= 300000, "step used %.3g for requested 0.001; %d cells" % (g["step"], g["cells"]))
de = P.decomposition_trajectory(d, 100.0)
check("an interval longer than the age range returns an empty or single-link decomposition without error", len(de["rows"]) <= 2)
for bad in [0, -1, float("nan"), None]:
    check("invalid set interval %r falls back to the inferred one" % (bad,), P.resolve_age_step(d.age.values, d.id.values, bad) == P.infer_age_step(d.age.values, d.id.values))

print("\n   examples: decomposition with the interval set to the inferred value, against auto")
imp = P.SRC["import.R"]; body = imp[imp.index("EXAMPLES <- list("):]
keys = [(m.group(1), m.start()) for m in re.finditer(r"\n  ([a-z0-9_]+) = list\(", body)]
fly_map = dict(re.findall(r'(\w+) = "([^"]*)"', imp[imp.index("FLY_MAPPING <- list"):imp.index("FLY_MAPPING <- list") + 800]))
EXD = {}
for i, (k, pos) in enumerate(keys):
    seg = body[pos: keys[i + 1][1] if i + 1 < len(keys) else len(body)]
    f = re.search(r'file = "([^"]+)"', seg).group(1)
    mp = fly_map if k == "fly" else dict(re.findall(r'(\w+) = "([^"]*)"', seg[seg.index("mapping = example_map"):seg.index("mapping = example_map") + 700]))
    d0 = pd.read_csv(os.path.join(APP, "data", f))
    sub = re.search(r'subset = list\(var = "([^"]+)", levels = c\(([^)]*)\)', seg)
    if sub: d0 = d0[d0[sub.group(1)].astype(str).isin(re.findall(r'"([^"]*)"', sub.group(2)))]
    dd = pd.DataFrame(dict(id=d0[mp["id"]].astype(str), age=pd.to_numeric(d0[mp["age"]], errors="coerce"),
                           trait=pd.to_numeric(d0[mp["trait"]], errors="coerce")))
    for extra in ("alr", "life", "entry"):
        if mp.get(extra) and mp[extra] in d0.columns: dd[extra] = pd.to_numeric(d0[mp[extra]], errors="coerce").values
    EXD[k] = (dd, re.search(r'family = "([^"]+)"', seg).group(1) if re.search(r'family = "([^"]+)"', seg) else "gaussian",
              (re.search(r'age_function = "([^"]+)"', seg) or [None, "Quadratic"])[1])
same_all = True
for k, (dd, fam, fn) in EXD.items():
    st = P.infer_age_step(dd.age.values, dd.id.values)
    a = P.decomposition_trajectory(dd); b = P.decomposition_trajectory(dd, st)
    ra, rb = a["rows"], b["rows"]
    same = len(ra) == len(rb) and (len(ra) == 0 or (np.allclose(ra.age, rb.age) and np.allclose(ra.fitted, rb.fitted) and (ra.n_pairs.values == rb.n_pairs.values).all()))
    same_all &= same
    print("   %-24s step %6.4g  auto %3d pts/%d segs  set %3d pts/%d segs  off-grid %4d  %s" % (k, st, len(ra), a["segments"], len(rb), b["segments"], b["n_off"], "identical" if same else "DIFFERS"))
check("setting the interval to the inferred value never changes the decomposition", same_all)
print("   examples: decomposition with the interval set to a DIFFERENT value (the next finer or coarser), against auto")
for k, (dd, fam, fn) in EXD.items():
    st = P.infer_age_step(dd.age.values, dd.id.values)
    for alt in (2 * st,):
        b = P.decomposition_trajectory(dd, alt)
        ages_real = set(np.round(dd.age.dropna().unique(), 8))
        moved = [x for x in b["rows"].age if not any(abs(x - r_) <= 0.25 * alt for r_ in ages_real)] if len(b["rows"]) else []
        print("   %-24s set %6.4g: %3d pts/%d segs, off-grid %4d, occasions away from any real age %d" % (k, alt, len(b["rows"]), b["segments"], b["n_off"], len(moved)))
bee = pd.DataFrame(dict(id=np.repeat(np.arange(40), 5), age=np.tile([1, 7, 14, 21, 28], 40).astype(float)))
bee["trait"] = 5 + 0.1 * bee.age + np.random.default_rng(1).normal(0, 1, len(bee))
b1 = P.decomposition_trajectory(bee, 14.0)
check("an offset first occasion (day 1, then weekly) keeps its real ages when the interval is set", set(np.round(b1["rows"].age, 6)) <= {1.0, 7.0, 14.0, 21.0, 28.0}, str(list(b1["rows"].age)))

print("\n   server wiring of the interval")
for fn in ["build_missing_grid(", "decomposition_trajectory(", "observed_prediction_ages("]:
    uses = [m.start() for m in re.finditer(re.escape(fn), server)]
    ok = all("age_step" in server[u:u + 400] for u in uses)
    check("server passes the interval to every %s" % fn[:-1], ok, "%d call(s)" % len(uses))
left = [server[:m.start()].count("\n") + 1 for m in re.finditer(r"infer_age_step\(", server)]
print("   infer_age_step() left in server.R at lines %s (the note and the prefill, which must show the inferred value)" % left)
check("meta carries the interval", "m$age_step <- age_step_manual()" in server)
for fn in ["disappearance_data", "life_table", "terminal_data", "data_integrity"]:
    check("%s defaults to meta$age_step" % fn, "step = meta$age_step" in P.r_body(fn).strip().split("\n")[0])

print("\n== B2 families")
mf = re.search(r"MODEL_FAMILIES <- c\((.*?)\n\)", P.SRC["model-fit.R"], re.S).group(1)
fams = re.findall(r'= "([a-z0-9]+)"', mf)
fit1 = P.r_body("fit_one_model"); lab = P.r_body("family_label"); rc = P.r_body("family_r_call")
for f in fams:
    how = lambda body: "explicit" if re.search(r"\b%s = " % f, body) else "default branch"
    print("   family %-12s fit_one_model %-14s family_label %-14s family_r_call %s" % (f, how(fit1), how(lab), how(rc)))
    if f in ("gamma", "lognormal", "beta"):
        check("new family %s is explicit in fit_one_model, family_label and family_r_call" % f, all(how(b) == "explicit" for b in (fit1, lab, rc)))
print("   (the default branches: nbinom2 for zinb in fitting and code, which with the zero-inflation formula is the zinb model;")
print("    family_label labels binomial and betabinomial explicitly from 0.22.2)")
eq = P.r_body("model_equation")
for f in ["gamma", "lognormal", "beta"]:
    check("equation panel describes %s" % f, re.search(r"\b%s = paste0" % f, eq) is not None)
print("   support of each example's trait (share of values allowed):")
for k, (dd, fam, fn) in EXD.items():
    v = dd.trait[np.isfinite(dd.trait)]
    print("   %-24s family %-10s Gamma/lognormal %s  beta %s" % (k, fam, "yes" if (v > 0).all() else "no (%d <= 0)" % (v <= 0).sum(),
                                                            "yes" if ((v > 0) & (v < 1)).all() else "no"))
# AIC comparability of continuous densities: lognormal and Gamma data, GLMs by ML
r = np.random.default_rng(3); x = r.uniform(-1, 1, 3000)
def fit_aic(y, fam):
    Xm = np.column_stack([np.ones_like(x), x])
    if fam == "gaussian":
        b = np.linalg.lstsq(Xm, y, rcond=None)[0]; s2 = np.mean((y - Xm @ b) ** 2)
        return -2 * np.sum(stats.norm.logpdf(y, Xm @ b, np.sqrt(s2))) + 2 * 3
    if fam == "lognormal":   # a density of y: the log-scale normal density minus log(y) (the Jacobian)
        ly = np.log(y); b = np.linalg.lstsq(Xm, ly, rcond=None)[0]; s2 = np.mean((ly - Xm @ b) ** 2)
        return -2 * (np.sum(stats.norm.logpdf(ly, Xm @ b, np.sqrt(s2))) - np.sum(ly)) + 2 * 3
    if fam == "gamma":
        from scipy.optimize import minimize
        def nll(p): mu = np.exp(p[0] + p[1] * x); k = np.exp(p[2]); return -np.sum(stats.gamma.logpdf(y, k, scale=mu / k))
        return 2 * minimize(nll, [np.log(y.mean()), 0, 0], method="Nelder-Mead", options=dict(maxiter=5000)).fun + 2 * 3
for truth in ["lognormal", "gamma"]:
    y = np.exp(1 + 0.5 * x + r.normal(0, 0.6, len(x))) if truth == "lognormal" else r.gamma(2.0, np.exp(1 + 0.5 * x) / 2.0)
    aics = {f: fit_aic(y, f) for f in ["gaussian", "lognormal", "gamma"]}
    best = min(aics, key=aics.get)
    print("   %-9s data: AIC Gaussian %.0f, lognormal %.0f, Gamma %.0f" % (truth, aics["gaussian"], aics["lognormal"], aics["gamma"]))
    check("AIC on the densities of the same values picks the true family (%s)" % truth, best == truth)

print("\n== B3 residual variance or dispersion that changes with age")
dfs = P.r_body("disp_formula_string")
check("constant -> ~1, age -> ~ f1, age_terms -> every basis term", '"~1"' in dfs and '"~ f1"' in dfs and 'paste(b, collapse = " + ")' in dfs)
fms = P.r_body("fit_model_suite")
check("families without a dispersion parameter ignore the option with a note", "no dispersion parameter" in fms and 'disp <- "constant"' in fms)
check("the non-linear exponential refuses an age-dependent variance", "not available with the non-linear exponential" in fms)
bt = P.r_body("bootstrap_test")
check("bootstrap_test carries the analysis's variance model to the null model and every refit (0.22.2)",
      bt.count("disp = s$disp") == 2 and "normalise_disp(s$disp)" in bt)
def sim_het(n, d1, sd_level, seed):
    r = np.random.default_rng(seed); rows = []
    ls = np.clip(np.round(r.normal(7, 2, n)), 2, 14).astype(int); zls = (ls - ls.mean()) / ls.std()
    for i in range(n):
        a = np.arange(1, ls[i] + 1.0)
        u = r.normal(0, 1) + sd_level * zls[i]
        rows.append(pd.DataFrame(dict(id=i, age=a, u=u, ls=ls[i])))
    d = pd.concat(rows, ignore_index=True)
    az = (d.age - d.age.mean()) / d.age.std()
    d["trait"] = 10 + 0.8 * az - 0.4 * az ** 2 + d.u + r.normal(0, 1, len(d)) * np.exp(0.5 * (d1 * az))
    d["alr"] = d.groupby("id").age.transform("max"); return d
def design(d, model):
    az = ((d.age - d.age.mean()) / d.age.std()).values; A = ((d.alr - d.alr.mean()) / d.alr.std()).values
    X = [np.ones(len(d)), az, az ** 2]
    if model in ("M2", "M4"): X.append(A)
    if model == "M4": X += [az * A, az ** 2 * A]
    return np.column_stack(X), az
# calibration of the audit's own fitter: with no selection and a constant variance, 2 x the log-likelihood gain of
# Model 4 over Model 1 (3 extra terms) should follow a chi-square with 3 df
lr = []
for s in range(100 if QUICK else 300):
    d = sim_het(150, 0.0, 0.0, 1000 + s); X1, az = design(d, "M1"); X4, _ = design(d, "M4")
    lr.append(2 * (lmm_ri(X4, d.trait.values, d.id.values)["loglik"] - lmm_ri(X1, d.trait.values, d.id.values)["loglik"]))
lr = np.array(lr); ks = stats.kstest(lr, "chi2", args=(3,)).pvalue
print("   fitter calibration: %d null datasets, mean LR %.2f (chi2(3): 3), share with delta AIC > 2 %.1f%% (expected 4.6%%), KS p %.2f" % (len(lr), lr.mean(), 100 * (lr > 8).mean(), ks))
check("the audit's mixed-model fitter gives chi-square(3) likelihood ratios under the null (KS p > 0.01)", ks > 0.01)
reps = 40 if QUICK else 150
print("   %-40s %11s %11s %11s" % ("scenario (%d replicates, 150 individuals)" % reps, "var model", "M4-M1 >2", "hetero >2"))
for lab, d1, sdl in [("constant variance, no selection", 0.0, 0.0), ("variance rising with age, no selection", 0.8, 0.0),
                     ("variance rising, level-linked selection", 0.8, 0.8)]:
    fp_c = fp_h = het = 0
    for s in range(reps):
        d = sim_het(150, d1, sdl, 100 + s); g = d.id.values; y = d.trait.values
        X1, az = design(d, "M1"); X4, _ = design(d, "M4")
        c1, c4 = lmm_ri(X1, y, g), lmm_ri(X4, y, g)
        h1, h4 = lmm_ri_hetero(X1, y, g, az), lmm_ri_hetero(X4, y, g, az)
        fp_c += (c1["aic"] - c4["aic"]) > 2; fp_h += (h1["aic"] - h4["aic"]) > 2; het += (c1["aic"] - h1["aic"]) > 2
    print("   %-40s %11s %10.0f%% %10s" % (lab, "constant", 100 * fp_c / reps, ""))
    print("   %-40s %11s %10.0f%% %10.0f%%" % ("", "age", 100 * fp_h / reps, 100 * het / reps))
    if d1 == 0 and sdl == 0: check("with a constant variance the age-dependent model is rarely preferred (<= 20%)", het / reps <= 0.2, "%.0f%%" % (100 * het / reps))
    if d1 > 0 and sdl == 0:
        check("a variance rising with age is detected (>= 80%)", het / reps >= 0.8, "%.0f%%" % (100 * het / reps))
        check("with a variance rising with age, modelling it lowers the false preference for Model 4", fp_h < fp_c, "%.0f%% -> %.0f%%" % (100 * fp_c / reps, 100 * fp_h / reps))

print("\n== B4 stored models")
mk = lambda i, sig, rows, cls, aic: dict(id="S%d" % i, data_sig=sig, rows=rows, family_class=cls, AIC=aic)
st = [mk(1, "a", "r1", "continuous", 100), mk(2, "a", "r1", "continuous", 95), mk(3, "a", "r2", "continuous", 50), mk(4, "a", "r1", "discrete", 10), mk(5, "b", "r1", "continuous", 5)]
tab = P.stored_comparison_table(st)
check("sets split by rows, likelihood type and data", sorted(tab.Set.unique()) == ["A", "B", "C", "D"])
check("delta AIC within a set only", float(tab.loc[tab.ID == "S1", "dAIC"].iloc[0]) == 5 and float(tab.loc[tab.ID == "S3", "dAIC"].iloc[0]) == 0)
check("no Akaike weights in the stored table", "Weight" not in P.r_body("stored_comparison_table"))
many = [mk(i, "a", "r%d" % i, "continuous", 10 + i) for i in range(1, 31)]
labs = P.stored_set_labels(many)
check("more than 26 sets are labelled without error", labs[25] == "Z" and labs[26] == "S27")
na = P.stored_comparison_table([mk(1, "a", "r1", "continuous", float("nan")), mk(2, "a", "r1", "continuous", 3)])
check("a missing AIC is kept but not compared", np.isnan(na.loc[na.ID == "S1", "dAIC"].iloc[0]) and na.loc[na.ID == "S2", "dAIC"].iloc[0] == 0)
def rows_key(d, trials=None):
    x = pd.DataFrame(dict(id=d.id.astype(str), age=d.age.map(lambda v: float("%.10g" % v)), trait=d.trait.map(lambda v: float("%.10g" % v))))
    if trials is not None: x["trials"] = trials
    x = x.sort_values(["id", "age", "trait"]); return hashlib.md5(x.to_csv(index=False).encode()).hexdigest()
d = design_data("annual_midyear", n=50)
check("row key ignores row order", rows_key(d) == rows_key(d.sample(frac=1, random_state=1)))
d2 = d.copy(); d2.loc[0, "trait"] += 1e-3
check("row key changes when one value changes", rows_key(d) != rows_key(d2))
check("row key drops one record -> a different set", rows_key(d) != rows_key(d.iloc[1:]))
ent = P.r_body("stored_model_entry")
for fld in ["res$age_function", "res$family", "res$random", "res$among", "disp", "res$zi", "res$standardise", "res$formulas[[m]]", "data_sig", "rk"]:
    check("stored key includes %s" % fld, fld in ent.split("list(key = paste(")[1].split("sep =")[0])
# the user's example: quadratic M4 and logarithmic model on the same records are comparable; a lifespan model on fewer rows is not
dd = EXD["bouwhuis"][0].dropna(subset=["trait", "age"]).copy()
az = (dd.age - dd.age.mean()) / dd.age.std(); A = dd.groupby("id").age.transform("max"); A = (A - A.mean()) / A.std()
la = np.log(dd.age - dd.age.min() + 1); la = (la - la.mean()) / la.std()
q4 = lmm_ri(np.column_stack([np.ones(len(dd)), az, az ** 2, A, az * A, az ** 2 * A]), dd.trait.values, dd.id.values)
l4 = lmm_ri(np.column_stack([np.ones(len(dd)), la, A, la * A]), dd.trait.values, dd.id.values)
print("   great tit recruits, same %d records: quadratic Model 4 AIC %.1f, logarithmic Model 4 AIC %.1f -> delta %.1f (one set)" % (len(dd), q4["aic"], l4["aic"], abs(q4["aic"] - l4["aic"])))

print("\n== B5 share of wins in the individual-level comparison")
per = pd.DataFrame(dict(id=np.repeat(["a", "b", "c", "d"], 3), Function=["L", "Q", "C"] * 4,
                        AICc=[10, 12, 13, 20, 18, 19, 5, 5, 9, 7, np.nan, np.nan]))
sh, comp, nc = P.share_best(per, ["L", "Q", "C"])
check("a tie splits the individual's win; incomplete individuals leave the common set", nc == 3 and sh == {"L": 50.0, "Q": 50.0, "C": 0.0}, str(sh))
per2 = pd.DataFrame(dict(id=np.repeat(list("abcdef"), 3), Function=["L", "Q", "C"] * 6, AICc=[1, 2, np.nan] * 5 + [3, 1, 2]))
sh2, comp2, nc2 = P.share_best(per2, ["L", "Q", "C"])
check("a function estimable for under half of the individuals is excluded (share NA)", np.isnan(sh2["C"]) and nc2 == 6 and abs(sh2["L"] + sh2["Q"] - 100) < 1e-9, str(sh2))
def indiv_aicc(dd, fn):
    out = []
    for i, g in dd.groupby("id"):
        a = g.age.values; y = g.trait.values
        cols = {"Linear": [a], "Quadratic": [a, a ** 2], "Cubic": [a, a ** 2, a ** 3], "Logarithmic": [np.log(a - dd.age.min() + 1)]}[fn]
        X = np.column_stack([np.ones_like(a)] + cols); n, p = X.shape
        if n <= p + 2: out.append((i, np.nan)); continue
        b = np.linalg.lstsq(X, y, rcond=None)[0]; rss = np.sum((y - X @ b) ** 2)
        if rss <= 1e-12: out.append((i, np.nan)); continue
        k = p + 1; ll = -0.5 * n * (np.log(2 * np.pi * rss / n) + 1)
        out.append((i, -2 * ll + 2 * k + 2 * k * (k + 1) / (n - k - 1) if n - k - 1 > 0 else np.nan))
    return pd.DataFrame(out, columns=["id", "AICc"]).assign(Function=fn)
print("   Gaussian examples, polynomial and logarithmic functions fitted per individual (share of wins, %):")
for k in ["bichet", "wynn", "bouwhuis", "warner", "mckennaell_weight", "moullec_swift", "szejnersigal_activity"]:
    dd = EXD[k][0].dropna(subset=["trait", "age"])
    funs = ["Linear", "Quadratic", "Cubic", "Logarithmic"]
    per = pd.concat([indiv_aicc(dd, f) for f in funs], ignore_index=True)
    sh, comp, nc = P.share_best(per, funs)
    tot = sum(v for v in sh.values() if np.isfinite(v))
    print("   %-24s common %4d  %s  (sum %.1f)" % (k, nc, "  ".join("%s %s" % (f[:4], "NA" if not np.isfinite(sh[f]) else "%.1f" % sh[f]) for f in funs), tot))
    if nc: check("%s: shares of the comparable functions sum to 100%%" % k, abs(tot - 100) <= 0.3)

print("\n== B6 peak age and onset of senescence")
a = np.linspace(0, 10, 201)
tp = P.curve_turning_points(a, -(a - 4) ** 2); check("quadratic: peak = onset = 4", tp["peak_interior"] and abs(tp["peak"] - 4) < 1e-9 and abs(tp["onset"] - 4) < 1e-9)
cub = -(a - 2) ** 2 * (a - 2) * 0 + (-0.05 * (a - 3) ** 3 + 0.3 * (a - 3) ** 2 * (a < 3))
tp = P.curve_turning_points(a, np.where(a < 3, a, 3 + 0.0 * a) - np.where(a > 6, (a - 6) ** 2, 0))
check("rise, plateau, late decline: onset at the start of the final decline, not at the plateau start", tp["onset_type"] == "interior" and abs(tp["onset"] - 6) < 0.06, "onset %.2f, peak %.2f" % (tp["onset"], tp["peak"]))
check("monotone decline: onset at or before the first age", P.curve_turning_points(a, -a)["onset_type"] == "from_start")
check("monotone rise: no final decline", P.curve_turning_points(a, a)["onset_type"] == "no_final_decline")
tp = P.curve_turning_points(a, (a - 5) ** 2); check("U-shape: interior trough, no interior peak", tp["trough_interior"] and not tp["peak_interior"])
check("flat curve: no interior peak and no decline", (lambda t: not t["peak_interior"] and t["onset_type"] == "no_final_decline")(P.curve_turning_points(a, np.zeros_like(a))))
# confidence limits from draws of the fixed effects: coverage of the true population peak
reps = 60 if QUICK else 200; cover = 0; widths = []; interior = 0
rng = np.random.default_rng(11)
for s in range(reps):
    n = 150; ls = np.clip(np.round(rng.normal(8, 2, n)), 3, 15).astype(int)
    g = np.repeat(np.arange(n), ls); age = np.concatenate([np.arange(1, l + 1.0) for l in ls])
    y = 2 + 0.9 * age - 0.09 * age ** 2 + np.repeat(rng.normal(0, 1, n), ls) + rng.normal(0, 1, len(age))
    X = np.column_stack([np.ones_like(age), age, age ** 2]); f = lmm_ri(X, y, g)
    grid = np.linspace(age.min(), age.max(), 200)
    Xg = np.column_stack([np.ones_like(grid), grid, grid ** 2])
    L = np.linalg.cholesky(f["vcov"] + 1e-12 * np.eye(3)); B = f["beta"][:, None] + L @ rng.normal(size=(3, 1000))
    curves = Xg @ B; pk = grid[np.argmax(curves, axis=0)]; inn = (np.argmax(curves, axis=0) > 0) & (np.argmax(curves, axis=0) < 199)
    lo, hi = np.quantile(pk[inn], [0.025, 0.975]); true = 0.9 / (2 * 0.09)
    cover += lo <= true <= hi; widths.append(hi - lo); interior += inn.mean() > 0.95
print("   true peak 5.0: coverage of the 95%% limits %.1f%% over %d datasets; mean width %.2f" % (100 * cover / reps, reps, np.mean(widths)))
check("peak confidence limits cover the true peak about 95% of the time (90-99%)", 0.90 <= cover / reps <= 0.99)
# great tit recruits (the recruits column, the paper's covariates, individual random intercept only)
gt = pd.read_csv(os.path.join(APP, "data", "bouwhuis_2009_great_tit_recruitment.csv")).dropna(subset=["recruits", "f_min_age"])
pk = {}
for lab, with_alr in [("Model 1", False), ("Model 2 (ALR)", True)]:
    X = [np.ones(len(gt)), gt.f_min_age, gt.f_min_age ** 2, gt.YR_FL, gt.loc_density, gt.f_status, gt.pred]
    if with_alr: X.append((gt.f_ALR - gt.f_ALR.mean()) / gt.f_ALR.std())
    f = lmm_ri(np.column_stack(X).astype(float), gt.recruits.values.astype(float), gt.female.values)
    grid = np.linspace(gt.f_min_age.min(), gt.f_min_age.max(), 400)
    pk[lab] = P.curve_turning_points(grid, f["beta"][1] * grid + f["beta"][2] * grid ** 2)["peak"]
    print("   great tit recruits, %-14s peak %.2f (analytic -b1/2b2 %.2f)" % (lab, pk[lab], -f["beta"][1] / (2 * f["beta"][2])))
print("   published: 3.45 (cross-sectional) and 2.80 (individual level); REPLICATION.md: app 3.48 and 2.80 with year and area random effects")
check("great tit recruits: correcting with ALR moves the peak younger, as published", pk["Model 2 (ALR)"] < pk["Model 1"] - 0.5)

print("\n== B7 random effects against lifespan proxies")
reps = 30 if QUICK else 100
for lab, sdl in [("no selective disappearance", 0.0), ("level-linked selection", 0.6)]:
    rs = []; sig = 0; rs2 = []
    for s in range(reps):
        d = sim_het(150, 0.0, sdl, 500 + s); X1, az = design(d, "M1"); X2, _ = design(d, "M2")
        f1 = lmm_ri(X1, d.trait.values, d.id.values); f2 = lmm_ri(X2, d.trait.values, d.id.values)
        ind = d.groupby("id").agg(ls=("ls", "first"))
        b1 = ind.index.map(f1["blup"]).values.astype(float); b2 = ind.index.map(f2["blup"]).values.astype(float)
        r_, p_ = stats.pearsonr(ind.ls.values, b1); rs.append(r_); sig += p_ < 0.05; rs2.append(stats.pearsonr(ind.ls.values, b2)[0])
    print("   %-28s Model 1 r(BLUP, LS) mean %+.3f, p < 0.05 in %3.0f%%; Model 2 r mean %+.3f" % (lab, np.mean(rs), 100 * sig / reps, np.mean(rs2)))
    if sdl == 0: check("no selection: Model 1 BLUPs uncorrelated with lifespan (|mean r| < 0.05; rate <= 10%)", abs(np.mean(rs)) < 0.05 and sig / reps <= 0.10)
    else: check("level-linked selection: Model 1 BLUPs rise with lifespan (detected >= 90%)", sig / reps >= 0.9 and np.mean(rs) > 0.2)
print("   Gaussian examples, Model 1 (quadratic, individual random intercept) BLUPs against ALR:")
for k in ["bichet", "bouwhuis", "warner", "mckennaell_weight", "wynn", "moullec_swift"]:
    dd = EXD[k][0].dropna(subset=["trait", "age"]).copy()
    alr = dd.groupby("id").age.transform("max") if "alr" not in dd or dd.alr.isna().all() else dd.alr
    dd["alr_"] = alr.values
    az = (dd.age - dd.age.mean()) / dd.age.std()
    f = lmm_ri(np.column_stack([np.ones(len(dd)), az, az ** 2]), dd.trait.values, dd.id.values)
    ind = dd.groupby("id").alr_.first(); b = ind.index.map(f["blup"]).values.astype(float); ok = np.isfinite(ind.values) & np.isfinite(b)
    r_, p_ = stats.pearsonr(ind.values[ok], b[ok])
    print("   %-24s n %4d  r %+.3f  p %.2g" % (k, ok.sum(), r_, p_))

print("\n== B8 methods writer: prefixes against saved titles, references, context fields")
mt = P.r_body("methods_text")
used = set(re.findall(r'has\("([^"]+)"\)', mt)) | set(re.findall(r'pick\("([^"]+)"\)', mt))
used |= set(re.findall(r'"([^"]+)"', re.search(r"vis_titles <- c\((.*?)\)", mt).group(1)))
used |= set(re.findall(r'"([^"]+)"', re.search(r"model_titles <- c\((.*?)\)\n", mt, re.S).group(1)))
title_texts = re.findall(r'add_saved\(\s*"[^"]+",\s*(.*?)(?:,\s*(?:text|tables|plots|evidence|code)\s*=|\)\s*\n)', server, re.S)
lits = []
for t in title_texts: lits += re.findall(r'"([^"]*)"', t)[:1]
extra_titles = ["Trait trajectory by", "Difference between", "Trait against", "Missingness against", "Model comparison", "Null-model bootstrap",
                "Ageing-function check with", "Missingness and sample size by age", "Sampling summary and proxy guidance"]
def matches(pfx): return any(t.startswith(pfx) or pfx.startswith(t) for t in lits if t) or pfx in server
missing_pfx = sorted(p for p in used if not matches(p))
check("every title prefix the methods writer looks for occurs in the server's saves", not missing_pfx, ", ".join(missing_pfx))
refs_used = set(re.findall(r'refs <- c\(refs, (?:if \([^)]*\) )?"([a-z0-9A-Z]+)"\)', mt)) | set(re.findall(r'"([a-zA-Z0-9]+)"', re.search(r'refs <- c\("sanghvi", "disappr"\)', mt).group(0)))
refs_used |= set(re.findall(r'if \(gauss_lme4\) "(\w+)" else "(\w+)"', mt)[0]) if re.search(r'if \(gauss_lme4\) "(\w+)" else "(\w+)"', mt) else set()
refs_def = set(re.findall(r"\n  (\w+) = \"", P.r_body("methods_text") and ENG[ENG.index("METHODS_REFS <- c("):ENG.index("METHODS_REFS <- c(") + 4000]))
check("every cited reference key is defined", refs_used <= refs_def, str(sorted(refs_used - refs_def)))
ctx = server[server.index("methods_context <- function()"):server.index("methods_seen <- new.env()")]
for grp, fields in [("data", ["source", "label", "trait", "n_trait", "n_ind", "age_min", "age_max", "step", "step_manual", "alr_mapped", "afr_mapped", "life_known", "n_censored", "covars", "groups", "subset"]),
                    ("models", ["family", "age_function", "random", "among", "standardise", "disp", "zi", "models", "n_rows", "n_ind", "decomposition", "lmertest"]),
                    ("boot", ["n", "model", "null_function"])]:
    miss = [f for f in fields if not re.search(r"\b%s = " % f, ctx)]
    check("methods_context supplies every %s field the writer reads" % grp, not miss, ", ".join(miss))

print("\n== B9 current-data box")
box = server[server.index("output$current_data_box <- renderUI"):server.index("# ---- stored models")]
toyd = P.SRC["simulation.R"][P.SRC["simulation.R"].index("TOY_DEFAULTS <- list("):]
toyd = toyd[:toyd.index("\n)") + 2]
cfg_fields = set(re.findall(r"cfg\$(\w+)", box))
missing_cfg = sorted(f for f in cfg_fields if not re.search(r"\b%s\s*=" % f, toyd))
check("every simulation setting the box reads has a default", not missing_cfg, ", ".join(missing_cfg))
mblk = ui[ui.index('selectInput("toy_missingness"'):]; mblk = mblk[:mblk.index("selected")]
miss_choices = re.findall(r'= "([a-z]+)"', mblk)
labels = set(re.findall(r"(\w+) = \"", re.search(r"TOY_MISSING_LABELS <- c\((.*?)\)\n", server, re.S).group(1)))
check("every sampling design offered in the simulator has a label", set(miss_choices) <= labels, "choices %s" % miss_choices)
check("the box is placed in the sidebar", 'uiOutput("current_data_box")' in ui)

print("\n== B10 new inputs, outputs and observers")
new_in = ["age_step_auto", "age_step_value", "disp_model", "store_pick", "store_models", "store_remove_ids", "store_remove", "store_clear",
          "save_store", "save_peak", "blup_model", "save_blup", "methods_pick"]
for i in new_in:
    created = re.search(r'"%s"' % i, ui) or re.search(r'Input\("%s"|checkboxGroupInput\("%s"|selectizeInput\("%s"|selectInput\("%s"|actionButton\("%s"' % ((i,) * 5), server)
    check("input %s is created" % i, created is not None)
new_out = ["current_data_box", "age_step_note", "store_pick_ui", "store_note", "store_table", "store_manage_ui", "peak_table", "peak_sentences",
           "blup_model_ui", "blup_table", "blup_plot", "blup_note", "methods_pick_ui", "methods_conflicts", "methods_text_ui", "download_methods"]
for o in new_out:
    check("output %s is defined and placed" % o, ("output$%s <-" % o) in server and ('"%s"' % o) in ui)
for ev in ["store_models", "store_remove", "store_clear", "save_store", "save_peak", "save_blup", "age_step_auto"]:
    m = re.search(r"observeEvent\(input\$%s, disappr_guard" % ev, server)
    check("observer for %s is guarded" % ev, m is not None)

print("\n== B11 expectations of tests/testthat/test-0-22.R, re-derived with the ports")
def half_year(seed=2, n=300):
    r = np.random.default_rng(seed); rows = []
    for i in range(n):
        L = r.integers(3, 10); a = list(np.arange(1, L + 1.0))
        if r.random() < 0.2: a.append(r.integers(1, L) + 0.5)
        a = np.sort(a); rows.append(pd.DataFrame(dict(id="i%03d" % i, age=a, trait=10 - 0.3 * a + r.normal(0, 1, len(a)))))
    return pd.concat(rows, ignore_index=True)
ok_all = True
for seed in range(1, 21):
    d = half_year(seed)
    au = P.decomposition_trajectory(d); s1 = P.decomposition_trajectory(d, 1.0)
    ok = P.infer_age_step(d.age.values, d.id.values) == 0.5 and s1["rows"].n_pairs.max() > 5 * au["rows"].n_pairs.max() and \
         P.missing_grid(d, 1.0)["missing"] < P.missing_grid(d)["missing"]
    ok_all &= ok
check("step, decomposition and grid expectations hold for 20 seeds of the test's design", ok_all)
print("\n%d failure(s)%s   [%.0f s]" % (len(fails), (": " + "; ".join(fails)) if fails else "", time.time() - t0))
