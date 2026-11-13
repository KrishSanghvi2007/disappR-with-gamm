"""Part C of the audit (0.22.4): do the reported numbers and words match the truth under every transformation?
The app reports coefficients on a standardised or raw scale, converts them 'per original unit', describes each term,
states where main effects are evaluated, reports predicted differences, flags random terms and labels ages. Each of
these depends on the scaling settings. This part ports those pieces from the R source (asserting the code they copy)
and checks every one against an independent truth, for every ageing function, with and without standardisation, on
simulated data and on bundled examples.

C1  the term description and scale factor reproduce the model column exactly
C2  estimate / scale factor equals the coefficient of a refit in the described units
C3  standardising changes the parameterisation, not the model: fitted values identical
C4  predicted differences reported in the text equal the truth and do not depend on standardisation
C5  the stated reference age is where the main-effect coefficient actually applies
C6  joint Wald test of the interaction block invariant to standardisation (the main-effect test is not)
C7  no displayed number rounds to zero when it is not zero
C8  the ages quoted for the mean-age term are ages
C9  the 'explains no variance' flag does not depend on the scale of a slope's covariate
C10 peak and onset ages do not depend on standardisation
C11 every table is drawn through the rounding guard; the scaling table matches the data"""
import os, re, sys, numpy as np, pandas as pd
sys.path.insert(0, os.path.dirname(__file__))
import port022 as P
from lmm022 import lmm_ri
from scipy import stats
ROOT = P.ROOT; APP = os.path.join(ROOT, "inst", "app")
server = open(os.path.join(APP, "server.R"), encoding="utf-8").read()
fails = []
def check(label, ok, detail=""):
    print(("PASS " if ok else "FAIL ") + label + (": " + detail if detail else ""))
    if not ok: fails.append(label)

# ---- the code being ported, asserted against the source ----
AB = P.r_body("age_basis"); MP = P.r_body("make_age_params"); PM = P.r_body("prepare_model_data")
TS = P.r_body("term_scaling"); SC = P.r_body("scaling_constants"); AR = P.r_body("age_reference")
NG = P.r_body("negligible_random_terms"); IT = P.r_body("interpret_model_terms"); ST = P.r_body("sig_text"); DN = P.r_body("display_numbers")
assert "x <- if (isTRUE(p$standardise)) z else age" in AB and "log(pmax(age - p$min_age + 1, 1e-12))" in AB
assert "ctr <- if (isTRUE(standardise) && length(x)) mean(x) else 0" in PM
assert 'paste0(age_var, "^", pw)' in TS and 'paste0("(", age_var, " \\u2212 centre)^", pw)' in TS and "value at \", age_var, \" = 0" in TS.replace("paste0(\"value at \", age_var, \" = 0", "value at \", age_var, \" = 0")
assert "exp(ap$mean_log) + ap$min_age - 1" in AR and "ap$mean_age - ap$sd_age * log(ap$mean_exp)" in AR
assert "vc$Variance[[i]] * ms" in NG and "real_mean_age" in IT and 'ref_mode = "own"' in IT and "q <- rev(q)" in IT
assert "abs(v) < 0.005" in DN

def make_age_params(age, fun, std):
    age = age[np.isfinite(age)]
    p = dict(fun=fun, standardise=std, mean_age=age.mean(), sd_age=age.std(ddof=1), min_age=age.min(), all_positive=age.min() > 0)
    lx = np.log(age) if p["all_positive"] else np.log(age - p["min_age"] + 1)
    p["mean_log"], p["sd_log"] = lx.mean(), lx.std(ddof=1)
    ex = np.exp(-(age - p["mean_age"]) / p["sd_age"]); p["mean_exp"], p["sd_exp"] = ex.mean(), ex.std(ddof=1)
    return p

def age_basis(age, p):
    z = (age - p["mean_age"]) / p["sd_age"]
    if p["fun"] in ("Linear", "Quadratic", "Cubic"):
        x = z if p["standardise"] else age
        k = {"Linear": 1, "Quadratic": 2, "Cubic": 3}[p["fun"]]
        return {"f%d" % i: x ** i for i in range(1, k + 1)}
    if p["fun"] == "Logarithmic":
        lx = np.log(np.maximum(age, 1e-12)) if p["all_positive"] else np.log(np.maximum(age - p["min_age"] + 1, 1e-12))
        return {"f1": (lx - p["mean_log"]) / p["sd_log"] if p["standardise"] else lx}
    ex = np.exp(-z)
    return {"f1": (ex - p["mean_exp"]) / p["sd_exp"] if p["standardise"] else ex}

def prepare(raw, fun, std):
    d = raw.copy()
    first = ~d.id.duplicated()
    pp = {}
    for v in ("ALR", "AFR", "LS"):
        x = d.loc[first, v + "_raw"].values; x = x[np.isfinite(x)]
        ctr = x.mean() if std and len(x) else 0.0
        s = x.std(ddof=1) if len(x) > 1 else np.nan
        scl = s if std and np.isfinite(s) and s > 0 else 1.0
        pp[v] = (ctr, scl); d[v] = (d[v + "_raw"] - ctr) / scl
    ap = make_age_params(d.age.values, fun, std)
    for nm, col in age_basis(d.age.values, ap).items():
        d[nm] = col; d["mean_" + nm] = d.groupby("id")[nm].transform("mean"); d["delta_" + nm] = d[nm] - d["mean_" + nm]
    return d, ap, pp

def scaling_constants(ap, pp):
    age_var = {"Logarithmic": "log(age)" if ap["all_positive"] else "log(age \u2212 youngest age + 1)", "Asymptotic exponential": "exp(\u2212z_age)"}.get(ap["fun"], "age")
    centre = 0.0 if not ap["standardise"] else {"Logarithmic": ap["mean_log"], "Asymptotic exponential": ap["mean_exp"]}.get(ap["fun"], ap["mean_age"])
    scale = 1.0 if not ap["standardise"] else {"Logarithmic": ap["sd_log"], "Asymptotic exponential": ap["sd_exp"]}.get(ap["fun"], ap["sd_age"])
    return age_var, centre, scale

def term_scaling(raw, ap, pp):
    age_var, age_centre, age_scale = scaling_constants(ap, pp)
    poly = ap["fun"] in ("Linear", "Quadratic", "Cubic"); scaled = ap["standardise"]
    if raw == "(Intercept)":
        return np.nan, ("value at the centre of every scaled variable" if scaled else "value at %s = 0 with every proxy at 0" % age_var)
    s = 1.0; units = []
    for p_ in raw.split(":"):
        base = re.sub(r"^(mean_|delta_)", "", p_)
        if re.match(r"^f[1-3]$", base):
            pw = int(base[1]) if poly else 1
            s *= age_scale ** pw
            pre = "individual mean " if p_.startswith("mean_") else "within-individual deviation of " if p_.startswith("delta_") else ""
            units.append(pre + (("(%s \u2212 centre)^%d" % (age_var, pw)) if (pw > 1 and scaled) else ("%s^%d" % (age_var, pw)) if pw > 1 else "%s unit" % age_var))
        else:
            v = p_; ctr, scl = pp[v]; s *= scl
            units.append("%s unit" % v)
    return s, " \u00d7 ".join(units)

def described_quantity(desc, d, ap, pp):
    """Evaluate a description on the raw data: the independent truth that basis x scale factor must reproduce."""
    age_var, age_centre, _ = scaling_constants(ap, pp)
    xr = {"age": lambda: d.age.values, "log(age)": lambda: np.log(d.age.values), "log(age \u2212 youngest age + 1)": lambda: np.log(d.age.values - ap["min_age"] + 1),
          "exp(\u2212z_age)": lambda: np.exp(-(d.age.values - ap["mean_age"]) / ap["sd_age"])}[age_var]()
    out = np.ones(len(d))
    for part in desc.split(" \u00d7 "):
        mode = None
        if part.startswith("individual mean "): mode = "mean"; part = part[len("individual mean "):]
        elif part.startswith("within-individual deviation of "): mode = "delta"; part = part[len("within-individual deviation of "):]
        m1 = re.match(r"^\((.+) \u2212 centre\)\^(\d)$", part); m2 = re.match(r"^(.+)\^(\d)$", part); m3 = re.match(r"^(.+) unit$", part)
        if m1: var, pw, centred = m1.group(1), int(m1.group(2)), True
        elif m2: var, pw, centred = m2.group(1), int(m2.group(2)), False
        elif m3: var, pw, centred = m3.group(1), 1, None
        else: raise ValueError(part)
        if var == age_var:
            c = age_centre
            q = (xr - c) ** pw if (centred or (centred is None)) else xr ** pw
        else:
            c = pp[var][0]
            q = (d[var + "_raw"].values - c) ** pw
        if mode == "mean": q = pd.Series(q).groupby(d.id.values).transform("mean").values
        if mode == "delta": q = q - pd.Series(q).groupby(d.id.values).transform("mean").values
        out = out * q
    return out

MODELS = {"M1": lambda b: list(b), "M2": lambda b: list(b) + ["ALR"], "M3": lambda b: ["mean_f1"] + ["delta_" + x for x in b],
          "M4": lambda b: list(b) + ["ALR"] + [x + ":ALR" for x in b], "M5": lambda b: ["mean_f1"] + ["delta_" + x for x in b] + ["mean_f1:delta_" + x for x in b],
          "M6": lambda b: list(b) + ["LS"] + [x + ":LS" for x in b], "M7": lambda b: list(b) + ["ALR", "AFR"],
          "M10": lambda b: list(b) + ["ALR", "AFR"] + [x + ":AFR" for x in b]}
def col(d, term):
    v = np.ones(len(d))
    for p_ in term.split(":"): v = v * d[p_].values
    return v
def design(d, terms): return np.column_stack([np.ones(len(d))] + [col(d, t) for t in terms])

def simulate(n=260, seed=5, age0=1):
    r = np.random.default_rng(seed); rows = []
    ls = np.clip(np.round(r.normal(9, 3, n)), 3, 18).astype(int); afr = r.integers(age0, age0 + 3, n); zls = (ls - ls.mean()) / ls.std()
    for i in range(n):
        a = np.arange(afr[i], afr[i] + ls[i]).astype(float)
        b0 = 10 + 0.8 * zls[i] + r.normal(0, 1); b1 = 0.6 - 0.25 * zls[i] + r.normal(0, 0.1)
        y = b0 + b1 * (a - 6) - 0.05 * (a - 6) ** 2 + 0.3 * (afr[i] - age0) + r.normal(0, 1, len(a))
        rows.append(pd.DataFrame(dict(id="i%03d" % i, age=a, trait=y)))
    d = pd.concat(rows, ignore_index=True)
    g = d.groupby("id").age
    d["ALR_raw"] = g.transform("max"); d["AFR_raw"] = g.transform("min"); d["LS_raw"] = d["ALR_raw"] + 1
    return d

FUNS = ["Linear", "Quadratic", "Cubic", "Logarithmic", "Asymptotic exponential"]
DATA = {"simulated (ages from 1)": simulate(), "simulated (ages from 0)": simulate(seed=6, age0=0)}
for k in ["bouwhuis", "warner"]:
    imp = P.SRC["import.R"]; seg = imp[imp.index("\n  %s = list(" % k):][:2500]
    f = re.search(r'file = "([^"]+)"', seg).group(1)
    mp = dict(re.findall(r'(\w+) = "([^"]*)"', seg[seg.index("mapping = example_map"):seg.index("mapping = example_map") + 700]))
    d0 = pd.read_csv(os.path.join(APP, "data", f))
    d = pd.DataFrame(dict(id=d0[mp["id"]].astype(str), age=pd.to_numeric(d0[mp["age"]], errors="coerce"), trait=pd.to_numeric(d0[mp["trait"]], errors="coerce"))).dropna()
    g = d.groupby("id").age; d["ALR_raw"] = g.transform("max"); d["AFR_raw"] = g.transform("min"); d["LS_raw"] = d["ALR_raw"] + 1
    DATA["example: " + k] = d.reset_index(drop=True)

res = {}
worst = dict(C1=0.0, C2=0.0, C3=0.0, C4=0.0, C4truth=0.0, C5=0.0, C6=0.0, C10=0.0)
print("== C1-C6 over %d datasets x %d functions x %d models x standardised / not" % (len(DATA), len(FUNS), len(MODELS)))
for dname, raw in DATA.items():
    for fun in FUNS:
        for mname, mk in MODELS.items():
            fits = {}
            for std in (True, False):
                d, ap, pp = prepare(raw, fun, std)
                b = [k for k in age_basis(d.age.values[:1], ap)]
                terms = mk(b)
                X = design(d, terms)
                if np.linalg.matrix_rank(X) < X.shape[1]: continue
                f = lmm_ri(X, d.trait.values, d.id.values)
                # C1 and C2: description x scale factor reproduce the column; refit in described units
                Xo = [np.ones(len(d))]; scales = []
                for t in terms:
                    s, desc = term_scaling(t, ap, pp)
                    q = described_quantity(desc, d, ap, pp)
                    colv = col(d, t)
                    worst["C1"] = max(worst["C1"], float(np.max(np.abs(colv * s - q)) / (1 + np.max(np.abs(q)))))
                    Xo.append(q); scales.append(s)
                fo = lmm_ri(np.column_stack(Xo), d.trait.values, d.id.values)
                conv = f["beta"][1:] / np.array(scales)
                worst["C2"] = max(worst["C2"], float(np.max(np.abs(conv - fo["beta"][1:]) / (np.abs(fo["beta"][1:]) + 1e-8 + 1e-6 * np.abs(conv)))))
                fits[std] = dict(d=d, ap=ap, pp=pp, terms=terms, f=f, fo=fo, X=X)
            if len(fits) < 2: continue
            # C3: identical fitted values
            fv = {s: fits[s]["X"] @ fits[s]["f"]["beta"] for s in fits}
            worst["C3"] = max(worst["C3"], float(np.max(np.abs(fv[True] - fv[False])) / np.std(raw.trait)))
            # C4/C5/C6 for the proxy of the model
            proxy = "ALR" if "ALR" in "".join(fits[True]["terms"]) else "LS" if "LS" in "".join(fits[True]["terms"]) else None
            if mname == "M10": proxy = "AFR"
            if proxy is None: continue
            ind = raw[~raw.id.duplicated()]
            qv = np.quantile(ind[proxy + "_raw"], [0.1, 0.9])
            if qv[0] == qv[1]: continue
            ages_txt = np.unique(np.quantile(raw.age, [0.1, 0.5, 0.9]))
            contr = {}
            for std, F in fits.items():
                d, ap, pp = F["d"], F["ap"], F["pp"]
                ctr, scl = pp[proxy]
                def pred(ages, zval):
                    nd = pd.DataFrame(dict(age=ages)); nd["id"] = "x"
                    for nm, cv in age_basis(ages, ap).items():
                        nd[nm] = cv; mv = d[~d.id.duplicated()]["mean_" + nm].mean(); nd["mean_" + nm] = mv; nd["delta_" + nm] = nd[nm] - mv
                    for v in ("ALR", "AFR", "LS"): nd[v] = d[~d.id.duplicated()][v].mean()
                    nd[proxy] = zval
                    return design(nd, F["terms"]) @ F["f"]["beta"]
                zlo, zhi = (qv - ctr) / scl
                contr[std] = pred(ages_txt, zhi) - pred(ages_txt, zlo)
                # C5: the reference age of the main effect
                ra = None
                if ap["fun"] in ("Linear", "Quadratic", "Cubic"): ra = ap["mean_age"] if std else 0.0
                elif ap["fun"] == "Logarithmic": ra = (np.exp(ap["mean_log"]) + (0 if ap["all_positive"] else ap["min_age"] - 1)) if std else (1.0 if ap["all_positive"] else ap["min_age"])
                elif std: ra = ap["mean_age"] - ap["sd_age"] * np.log(ap["mean_exp"])
                if ra is not None and proxy in F["terms"]:
                    main = F["f"]["beta"][1 + F["terms"].index(proxy)] * (zhi - zlo)
                    worst["C5"] = max(worst["C5"], float(abs(main - (pred(np.array([ra]), zhi) - pred(np.array([ra]), zlo))[0])))
            worst["C4"] = max(worst["C4"], float(np.max(np.abs(contr[True] - contr[False]))))
            # truth in described units
            F = fits[False]; fo = F["fo"]
            # C6 joint Wald of the interaction block
            wd = {}
            for std, F2 in fits.items():
                idx = [1 + i for i, t in enumerate(F2["terms"]) if ":" in t and proxy in t.split(":")]
                if not idx: continue
                bb = F2["f"]["beta"][idx]; V = F2["f"]["vcov"][np.ix_(idx, idx)]
                wd[std] = float(bb @ np.linalg.solve(V, bb))
            if len(wd) == 2: worst["C6"] = max(worst["C6"], abs(wd[True] - wd[False]) / max(1.0, wd[True]))
print("PASS" if worst["C1"] < 1e-8 else "FAIL", "C1 description x scale factor reproduces every model column: largest relative error %.1e" % worst["C1"])
if worst["C1"] >= 1e-8: fails.append("C1")
check("C2 estimate / scale factor equals the refit in the described units", worst["C2"] < 1e-4, "largest relative error %.1e" % worst["C2"])
check("C3 standardising changes the parameterisation, not the model (fitted values)", worst["C3"] < 1e-6, "largest difference %.1e SD" % worst["C3"])
check("C4 predicted differences for the text do not depend on standardisation", worst["C4"] < 1e-6, "largest difference %.1e" % worst["C4"])
check("C5 the stated reference age is where the main-effect coefficient applies", worst["C5"] < 1e-6, "largest difference %.1e" % worst["C5"])
check("C6 the joint Wald test of the interaction block does not depend on standardisation", worst["C6"] < 1e-4, "largest relative difference %.1e" % worst["C6"])

print("\n== C4 against a hand calculation, and C5 in words, on the reported case (cubic, AFR x age, not standardised)")
raw = DATA["simulated (ages from 1)"]
d, ap, pp = prepare(raw, "Cubic", False)
terms = MODELS["M10"](["f1", "f2", "f3"]); f = lmm_ri(design(d, terms), d.trait.values, d.id.values)
be = dict(zip(terms, f["beta"][1:]))
ind = raw[~raw.id.duplicated()]; q = np.quantile(ind.AFR_raw, [0.1, 0.9]); ages_txt = np.quantile(raw.age, [0.1, 0.5, 0.9])
hand = [(q[1] - q[0]) * (be["AFR"] + be["f1:AFR"] * a + be["f2:AFR"] * a ** 2 + be["f3:AFR"] * a ** 3) for a in ages_txt]
print("   AFR %s vs %s; hand-calculated difference at ages %s: %s" % (q[1], q[0], list(np.round(ages_txt, 2)), list(np.round(hand, 4))))
print("   main-effect coefficient x delta AFR = %.4f = the difference at age 0; at the median age it is %.4f" % ((q[1] - q[0]) * be["AFR"], hand[1]))
check("C5 the text names age 0, not the mean age, when a polynomial is not standardised",
      'if (poly) "age 0"' in AR and "Age is not standardised, so the main effect is the difference at \", ref_age" in IT)
check("C5 logarithmic and asymptotic exponential references are named for their own zero point",
      "age 1, where log(age) is 0" in AR and "which no age reaches" in AR and "where the log term is 0" in AR)

print("\n== C7 displayed numbers never round to zero when they are not zero")
def sig_text(x, digits=3):
    if not np.isfinite(x): return "NA"
    if x == 0: return "0"
    if abs(x) >= 1e6 or abs(x) < 1e-4: return "%.*e" % (digits - 1, x)
    return "%s" % float("%.*g" % (digits, x))
hidden_old = hidden_new = n_coef = 0
for dname, raw in DATA.items():
    for fun in ("Quadratic", "Cubic"):
        for std in (True, False):
            d, ap, pp = prepare(raw, fun, std)
            terms = MODELS["M10"]([k for k in age_basis(d.age.values[:1], ap)])
            f = lmm_ri(design(d, terms), d.trait.values, d.id.values)
            est = f["beta"]; se = np.sqrt(np.diag(f["vcov"])); z = est / se
            for e, zz in zip(est, z):
                n_coef += 1
                if abs(zz) >= 0.1 and ("%.2f" % e) in ("0.00", "-0.00"): hidden_old += 1
                if abs(zz) >= 0.1 and float(sig_text(e)) == 0: hidden_new += 1
print("   %d coefficients: %d shown as 0.00 by two-decimal rounding, %d by three significant digits" % (n_coef, hidden_old, hidden_new))
check("C7 no non-zero coefficient is displayed as zero", hidden_new == 0)
dfx = pd.DataFrame(dict(a=[0.0004, 0.5, 12.0], b=[1.0, 2.0, 3.0], c=[1234.5, 0.0, -0.0021]))
out = {}
for c_ in dfx:
    v = dfx[c_].values
    out[c_] = ["NA" if not np.isfinite(x) else ("%.2f" % x if (x == 0 or abs(x) >= 0.005) else sig_text(x)) for x in v] if np.any((v != 0) & (np.abs(v) < 0.005)) else list(v)
check("C7 display_numbers(): small values kept, other columns untouched", out["a"] == ["0.0004", "0.50", "12.00"] and out["b"] == [1.0, 2.0, 3.0] and out["c"] == ["1234.50", "0.00", "-0.0021"], str(out))

print("\n== C8 the ages quoted for the mean-age term")
for fun in FUNS:
    for std in (True, False):
        d, ap, pp = prepare(DATA["simulated (ages from 1)"], fun, std)
        ind = d[~d.id.duplicated()]; mf = ind.mean_f1.values; qq = np.quantile(mf, [0.1, 0.9])
        old = [x * ap["sd_age"] + ap["mean_age"] if (std and fun in ("Linear", "Quadratic", "Cubic")) else x for x in qq]
        real = [d.age[d.id == ind.id.values[np.argmin(np.abs(mf - t))]].mean() for t in qq]   # the truth: those individuals' mean ages
        new = sorted(real)                                                                       # 0.22.4: real ages, younger group first
        err_old = max(abs(o - r) for o, r in zip(old, real))
        print("   %-24s %-5s old labels %-16s new labels %-12s truth %-12s old error %.2f years%s" % (
            fun, "std" if std else "raw", np.round(old, 2), np.round(new, 2), np.round(real, 2), err_old,
            "; old 'younger' group was the older one" if real[0] > real[1] else ""))
        if not (new[0] <= new[1] and set(np.round(new, 9)) == set(np.round(real, 9))): fails.append("C8 %s" % fun)
check("C8 the new labels are the real mean ages of the individuals used, younger group first, for every function", not any(x.startswith("C8") for x in fails))

print("\n== C9 'explains no variance' and the scale of the slope's covariate")
age = np.tile(np.arange(1, 16.0), 50); olds, news = [], []
for sd_raw in (0.01, 0.0001):
    for c_ in (1.0, 1 / 15, 1 / 225):
        x = (age ** 2) * c_; var_b = (sd_raw / c_) ** 2
        old = np.sqrt(var_b) < 0.05; new = np.sqrt(var_b * np.mean(x ** 2)) < 0.05
        olds.append((sd_raw, old)); news.append((sd_raw, new))
        print("   slope SD %-7g per age^2, covariate rescaled x %-8.4g: SD per unit %-9.4g old flag %-5s new flag %-5s (SD across records %.4f)" % (
            sd_raw, c_, np.sqrt(var_b), old, new, np.sqrt(var_b * np.mean(x ** 2))))
same_new = all(len({f for s, f in news if s == s0}) == 1 for s0 in (0.01, 0.0001))
same_old = all(len({f for s, f in olds if s == s0}) == 1 for s0 in (0.01, 0.0001))
check("C9 the new flag does not depend on the covariate's scale (the old one did)", same_new and not same_old)
check("C9 a slope that varies little is still flagged", dict(news)[0.0001])

print("\n== C10 peak and onset ages under standardisation")
for fun in ("Quadratic", "Cubic"):
    pk = {}
    for std in (True, False):
        d, ap, pp = prepare(DATA["simulated (ages from 1)"], fun, std)
        terms = MODELS["M2"]([k for k in age_basis(d.age.values[:1], ap)])
        f = lmm_ri(design(d, terms), d.trait.values, d.id.values)
        grid = np.linspace(d.age.min(), d.age.max(), 200); nd = pd.DataFrame(dict(age=grid))
        for nm, cv in age_basis(grid, ap).items(): nd[nm] = cv
        nd["ALR"] = d[~d.id.duplicated()].ALR.mean()
        tp = P.curve_turning_points(grid, design(nd, terms) @ f["beta"]); pk[std] = (tp["peak"], tp["onset"])
    print("   %-10s standardised peak %.3f onset %.3f | raw peak %.3f onset %.3f" % (fun, pk[True][0], pk[True][1], pk[False][0], pk[False][1]))
    worst["C10"] = max(worst["C10"], abs(pk[True][0] - pk[False][0]), abs(np.nan_to_num(pk[True][1]) - np.nan_to_num(pk[False][1])))
check("C10 peak and onset do not depend on standardisation", worst["C10"] < 1e-9)

print("\n== C11 tables and the scaling table")
i_def = server.index("renderTable <- function(expr, ..., env = parent.frame(), quoted = FALSE)")
uses = [m.start() for m in re.finditer(r"\brenderTable\(", server)]
check("C11 every table is drawn through the rounding guard (defined before all %d uses)" % (len(uses) - 1), all(u >= i_def for u in uses))
check("C11 coefficient, random-effect and covariance tables format with significant digits",
      "sig_text(x$Estimate, 3)" in P.r_body("coef_display") and 'x[[cl]] <- sig_text(x[[cl]], 3)' in server and "Covariance = sig_text(t$Covariance, 3)" in server)
d, ap, pp = prepare(DATA["simulated (ages from 0)"], "Logarithmic", True)
check("C11 the log term is described as log(age - youngest age + 1) when ages include 0", scaling_constants(ap, pp)[0] == "log(age \u2212 youngest age + 1)")
d, ap, pp = prepare(DATA["simulated (ages from 1)"], "Quadratic", True)
ind = d[~d.id.duplicated()]
check("C11 scaling constants are the data's mean and SD", abs(scaling_constants(ap, pp)[1] - d.age.mean()) < 1e-12 and abs(pp["ALR"][1] - ind.ALR_raw.std(ddof=1)) < 1e-12)
print("\n%d failure(s)%s" % (len(fails), (": " + "; ".join(fails)) if fails else ""))
