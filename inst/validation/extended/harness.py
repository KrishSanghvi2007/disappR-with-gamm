"""Validation harness for disappR 0.9.13.

prepare() reproduces R/data-preparation.R exactly (proxy centring over individuals present in the
analysed rows; age basis per R/formulas.R make_age_params/age_basis), formulas per
R/formulas.R model_formula_strings. Fitting uses the ML LMM / GH-quadrature GLMMs already verified
against dense likelihoods and brute-force quadrature.
"""
import numpy as np, pandas as pd
from scipy.optimize import minimize
from scipy.special import gammaln, expit
from emu import standardise, individual_metrics, LMM, design, integrity, availability, num

def prepare(dat, fun="Quadratic", standardise_flag=True):
    d = dat[np.isfinite(dat.trait) & np.isfinite(dat.age)].copy()
    if len(d) < 2: return None, None
    im = individual_metrics(dat)
    afr = im.entry.where(np.isfinite(im.entry), im.first_recorded)
    raw = {"ALR": im.alr, "LS": im.lifespan, "AFR": afr}
    first_ids = d.id.drop_duplicates()                    # individuals present in the analysed rows
    for v, s in raw.items():
        x = s.reindex(first_ids).values
        x = x[np.isfinite(x)]
        ctr = x.mean() if (standardise_flag and len(x)) else 0.0
        sd = x.std(ddof=1) if len(x) > 1 else np.nan
        scl = sd if (standardise_flag and np.isfinite(sd) and sd > 0) else 1.0
        d[v] = (d.id.map(s) - ctr) / scl
        d[v + "2"] = d[v] ** 2
        d[v + "3"] = d[v] ** 3
    a = d.age.values.astype(float)
    mean_age, sd_age = a.mean(), (a.std(ddof=1) if len(a) > 1 else 1.0)
    if not np.isfinite(sd_age) or sd_age <= 0: sd_age = 1.0
    z = (a - mean_age) / sd_age
    if fun in ("Linear", "Quadratic", "Cubic"):
        x = z if standardise_flag else a
        d["f1"] = x; basis = ["f1"]
        if fun in ("Quadratic", "Cubic"): d["f2"] = x ** 2; basis.append("f2")
        if fun == "Cubic": d["f3"] = x ** 3; basis.append("f3")
    elif fun == "Logarithmic":
        lx = np.log(np.maximum(a, 1e-12)) if a.min() > 0 else np.log(np.maximum(a - a.min() + 1, 1e-12))
        ml, sl = lx.mean(), (lx.std(ddof=1) if len(lx) > 1 else 1.0)
        if not np.isfinite(sl) or sl <= 0: sl = 1.0
        d["f1"] = (lx - ml) / sl if standardise_flag else lx; basis = ["f1"]
    else:
        ex = np.exp(-z); me, se = ex.mean(), (ex.std(ddof=1) if len(ex) > 1 else 1.0)
        if not np.isfinite(se) or se <= 0: se = 1.0
        d["f1"] = (ex - me) / se if standardise_flag else ex; basis = ["f1"]
    for bnm in basis:
        m = d.groupby("id")[bnm].transform("mean")
        d["mean_" + bnm] = m; d["delta_" + bnm] = d[bnm] - m
    return d, basis

def formulas(b, among="linear"):
    B = [(x,) for x in b]; dl = [("delta_" + x,) for x in b]
    inter = lambda v: [(x, v) for x in b]
    poly = among == "same" and len(b) > 1
    if not poly:
        prox = lambda v: [(v,)]
        pint = lambda v: inter(v)
        mean_terms = [("mean_f1",)]
    else:
        prox = lambda v: [(v,)] + [(f"{v}{i}",) for i in range(2, len(b) + 1)]
        pint = lambda v: [(x, p[0]) for x in b for p in prox(v)]
        mean_terms = [("mean_" + x,) for x in b]
    return {
        "M1": B, "M2": B + prox("ALR"), "M3": mean_terms + dl,
        "M4": B + prox("ALR") + pint("ALR"),
        "M5": mean_terms + dl + [(m[0], dd[0]) for m in mean_terms for dd in dl],
        "M6": B + prox("LS") + pint("LS"),
        "M7": B + prox("ALR") + prox("AFR"),
        "M8": B + prox("ALR") + prox("AFR") + pint("ALR") + pint("AFR"),
        "M9": B + prox("ALR") + pint("ALR") + prox("AFR"),
        "M10": B + prox("AFR") + pint("AFR") + prox("ALR"),
    }

x_gh, w_gh = np.polynomial.hermite_e.hermegauss(25); w_gh = w_gh / np.sqrt(2 * np.pi)

def glmm_aic(y, X, ids, family="gaussian", ntrials=None, slope=None, slope_type="none"):
    if family == "gaussian":
        f = LMM(y, X, ids, slope, slope_type).fit(); return f.aic, dict(beta=f.beta, sing=f.singular)
    o = np.argsort(ids, kind="stable"); y, X, ids = y[o], X[o], np.asarray(ids)[o]
    nt = (np.ones(len(y)) if ntrials is None else np.asarray(ntrials, float))[o]
    _, st, _ = np.unique(ids, return_index=True, return_counts=True); p = X.shape[1]
    lch = gammaln(nt + 1) - gammaln(y + 1) - gammaln(nt - y + 1) if family == "binomial" else 0.0
    def nll(th):
        b, s = th[:p], abs(th[p]); eta = np.clip(X @ b[:, None] + s * x_gh[None, :], -30, 30)
        if family == "binomial": ll = lch[:, None] + y[:, None] * eta - nt[:, None] * np.log1p(np.exp(eta))
        else: ll = y[:, None] * eta - np.exp(eta) - gammaln(y + 1)[:, None]
        per = np.add.reduceat(ll, st, axis=0); m = per.max(axis=1, keepdims=True)
        return -float(np.sum(m.ravel() + np.log(np.maximum(np.exp(per - m) @ w_gh, 1e-300))))
    # start from the GLM (IRLS) fit, then optimise jointly - far faster than Nelder-Mead from cold
    bg = np.zeros(p)
    bg[0] = np.log(max(y.mean(), .01)) if family == "poisson" else 0.0
    for _ in range(40):
        eta = np.clip(X @ bg, -25, 25)
        mu = np.exp(eta) if family == "poisson" else nt / (1 + np.exp(-eta))
        W = np.maximum(mu if family == "poisson" else mu * (1 - mu / np.maximum(nt, 1e-9)), 1e-9)
        z = eta + (y - mu) / W
        nb = np.linalg.lstsq(X * np.sqrt(W)[:, None], z * np.sqrt(W), rcond=None)[0]
        if np.max(np.abs(nb - bg)) < 1e-9: bg = nb; break
        bg = nb
    best = None
    for s0 in (0.25, 0.7):
        b0 = np.append(bg, s0)
        r = minimize(nll, b0, method="BFGS", options=dict(maxiter=600, gtol=1e-5))
        if best is None or r.fun < best.fun: best = r
    return 2 * best.fun + 2 * (p + 1), dict(beta=best.x[:p], sing=abs(best.x[p]) < 1e-4)

def suite(df, models=("M1","M2","M3","M4","M5","M6"), fun="Quadratic", family="gaussian",
          among="linear", life="", entry="__AUTO_FIRST__", alr="__AUTO_LAST__",
          covars=(), factors=(), group=None, nested=True, slope_type="none", trials=None):
    m = dict(id="id", age="age", trait="trait", alr=alr, life=life, entry=entry,
             covars=list(covars), factors=list(factors))
    if group: m.update(group=group, nested=nested)
    dat, meta = standardise(df, m)
    d, b = prepare(dat, fun)
    if d is None: return None, None, None
    fs = formulas(b, among)
    ok, default, why = availability(dat, meta)
    models = [x for x in models if ok[x]]
    if not models: return None, None, None
    cvn = ["cv_" + c for c in covars]
    need = {"id", "trait"}
    for mm in models:
        for t in fs[mm]: need.update(t)
    need.update(cvn)
    cc = np.ones(len(d), bool)
    for v in need:
        if v not in d.columns: continue
        col = d[v]
        if v in ("id",) or col.dtype == object or str(col.dtype) == "string" or v in cvn:
            cc &= col.notna().values
        else:
            cc &= np.isfinite(col.values.astype(float))
    dd = d[cc].copy()
    if trials is not None:
        dd["ntrials"] = dat.loc[dd.index, trials].values if trials in dat.columns else 1.0
    out = {}
    for mm in models:
        terms = [(c,) for c in cvn] + fs[mm]
        X, names = design(dd, terms, tuple("cv_" + f for f in factors))
        y = dd.trait.values.astype(float)
        nt = dd.ntrials.values.astype(float) if trials is not None else None
        if family == "binomial" and nt is not None: y = np.round(y * nt)
        aic, info = glmm_aic(y, X, dd.id.values, family, nt, dd.f1.values, slope_type)
        out[mm] = dict(aic=aic, beta=dict(zip(names, info["beta"])), sing=info["sing"])
    tab = pd.Series({k: v["aic"] for k, v in out.items()}).sort_values()
    return (tab - tab.min()).round(1), out, dict(n=len(dd), n_id=dd.id.nunique(), avail=ok, default=default)

# ---------------------------------------------------------------- simulators
def lifespans(r, n, mean_ls, cv=0.3, lo=2):
    return np.clip(np.round(r.normal(mean_ls, cv * mean_ls, n)), lo, None).astype(int)

def simulate(n=400, mean_ls=14, seed=1, fun="quadratic", sd="none", strength=1.0, family="gaussian",
             afr_var=0, miss=0.0, miss_mode="mcar", censor=0.0, groups=0, cov=False, seed_cv=None,
             slope_sd=0.0, trials=None, sigma=1.0):
    """sd: none | level | rate | shape | threshold | ushaped | appearance"""
    r = np.random.default_rng(seed)
    ls = lifespans(r, n, mean_ls)
    z = (ls - ls.mean()) / ls.std(ddof=1)
    b0 = 10 + r.normal(0, 1, n)
    b1 = np.full(n, -0.25); b2 = np.full(n, 0.0)
    if fun == "quadratic": b1[:] = 0.5; b2[:] = -0.035
    if sd == "level":      b0 += strength * 1.5 * z
    if sd == "rate":       b1 += strength * 0.12 * z
    if sd == "shape":      b2 += strength * 0.012 * z
    if sd == "threshold":  b1 += np.where(ls >= np.median(ls), 0.10, -0.30) * strength
    if sd == "ushaped":    b0 += strength * 3.0 * (1 - z ** 2)
    b1 += r.normal(0, slope_sd, n)
    afr = 1 + (r.integers(0, afr_var + 1, n) if afr_var else np.zeros(n, int))
    if sd == "appearance":
        # selective APPEARANCE: better individuals (higher intercept) enter the study earlier
        q = b0 - 10.0
        afr = np.clip(1 + np.round(1.5 * (-q) + r.normal(0, .5, n)), 1, 6).astype(int)
    afr = np.minimum(afr, np.maximum(ls - 1, 1))
    grp = r.integers(0, groups, n) if groups else np.zeros(n, int)
    ge = r.normal(0, 1.0, max(groups, 1))[grp]
    diet = r.choice(["A", "B"], n) if cov else np.array([""] * n)
    rows = []
    for i in range(n):
        a = np.arange(afr[i], ls[i] + 1).astype(float)
        if len(a) < 1: continue
        if fun == "log":
            base = b1[i] * np.log(a)
        elif fun == "exp":
            base = b1[i] * np.exp(-(a - mean_ls) / (0.3 * mean_ls))
        else:
            base = b1[i] * a + b2[i] * a ** 2
        eta = b0[i] + ge[i] + base + (-0.15 * a if (cov and diet[i] == "B") else 0)
        if family == "gaussian":
            y = eta + r.normal(0, sigma, len(a))
        elif family == "poisson":
            y = r.poisson(np.exp(np.clip(eta / 6, -5, 6)))
        elif family == "binomial":
            nt = np.full(len(a), trials or 1)
            p = expit((eta - 10) / 3)
            k = r.binomial(nt.astype(int), p)
            y = k / nt
        if miss > 0:
            pk = (1 - miss) * (np.clip(1 - 0.05 * (a - a[0]), 0.2, 1) if miss_mode == "age" else 1.0)
            if miss_mode == "mnar": pk = expit((y - np.median(y)) * 1.5) * (1 - miss) + miss * 0.2
            keep = r.random(len(a)) < np.maximum(pk, 0.05); keep[0] = True
        else:
            keep = np.ones(len(a), bool)
        cens = r.random() < censor
        for j in range(len(a)):
            if not keep[j]: continue
            row = dict(id=f"i{i}", age=float(a[j]), trait=float(y[j]), LS=float(np.nan if cens else ls[i]),
                       AFR=float(afr[i]), grp=f"g{grp[i]}", diet=diet[i], censored=int(cens))
            if family == "binomial": row["ntrials"] = float(trials or 1)
            rows.append(row)
    return pd.DataFrame(rows)
