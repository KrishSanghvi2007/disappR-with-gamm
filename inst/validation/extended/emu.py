"""Emulation of disappR v0.7.0 data pipeline (global.R) and an ML linear mixed model equivalent to
lmer(REML = FALSE) with (1|id), optional (0+f1|id) or (1+f1|id), and optional one extra intercept factor
(nested group or crossed factor). Used because R is unavailable in this container."""
import numpy as np, pandas as pd, re
from scipy.optimize import minimize

NA_CODES = ["NA", "", ".", "-", "NaN", "N/A", "n/a", "na", "#N/A", "NULL", "null"]
MODEL_IDS = [f"M{i}" for i in range(1, 11)]

def read_user_csv(path, enc="utf-8-sig"):
    try:
        x = pd.read_csv(path, dtype=str, keep_default_na=False, encoding=enc)
    except UnicodeDecodeError:
        x = pd.read_csv(path, dtype=str, keep_default_na=False, encoding="latin1")
    x.columns = [c.strip() for c in x.columns]
    for c in x.columns:
        v = x[c].str.strip()
        v[v.isin(NA_CODES)] = np.nan
        x[c] = v
    x = x.loc[:, x.notna().any()]
    return x

def num(s):
    return pd.to_numeric(s, errors="coerce")

def guess_mapping(df):
    cols = list(df.columns)
    low = [re.sub("[^a-z0-9]+", "_", c.lower()) for c in cols]
    n = len(df)
    is_num = []
    for c in cols:
        v = df[c]; pres = v.notna() & (v.astype(str) != "")
        if not pres.any(): is_num.append(False); continue
        is_num.append(np.isfinite(num(v[pres])).mean() > 0.95)
    is_num = np.array(is_num)
    nu = np.array([df[c].dropna().nunique() for c in cols])
    def find_col(pats, pool):
        for p in pats:
            h = [i for i in range(len(cols)) if pool[i] and low[i] == p]
            if h: return cols[h[0]]
        for p in pats:
            h = [i for i in range(len(cols)) if pool[i] and p in low[i]]
            if h: return cols[h[0]]
        return ""
    idre = re.compile(r"(^|_)id($|_)|individual|animal|subject|ring|(^|_)ind($|_)")
    idh = [i for i in range(len(cols)) if nu[i] >= 2 and nu[i] < n and idre.search(low[i])]
    idc = cols[max(idh, key=lambda i: nu[i])] if idh else cols[0]
    exa = re.compile("mean|delta|centr|age2|age_2|_sq|sq_|squared|paternal|maternal|sperm|alr|afr|first|last|entry|death|lifespan")
    pool = np.array([is_num[i] and not exa.search(low[i]) and cols[i] != idc for i in range(len(cols))])
    age = find_col(["age", "time", "occasion", "year"], pool) or cols[min(1, len(cols) - 1)]
    ext = re.compile(r"(^|_)ls($|_)|lifespan|alr|afr|mean|delta|age|censor|(^|_)rep($|_)|obs|(^|_)id($|_)|year")
    pool = np.array([is_num[i] and not ext.search(low[i]) and cols[i] not in (idc, age) for i in range(len(cols))])
    trait = find_col(["trait", "count", "fecund", "fertil", "offspring", "egg", "clutch", "value", "phenotype",
                      "mass", "weight", "size", "date", "score"], pool)
    if not trait:
        h = [i for i in range(len(cols)) if pool[i] and nu[i] > 2]
        trait = cols[h[0]] if h else cols[min(2, len(cols) - 1)]
    life = find_col(["ls", "lifespan", "life_span", "longevity", "age_at_death", "death_age"], is_num)
    alr = find_col(["alr", "age_last_record", "last_record", "age_at_last"], is_num)
    afr = find_col(["afr", "age_first_record", "first_record", "age_at_first", "entry_age"], is_num)
    return dict(id=idc, age=age, trait=trait, alr=alr or "__AUTO_LAST__", life=life, entry=afr or "__AUTO_FIRST__")

def standardise(df, m):
    """m: id, age, trait, alr, life, entry, censor, censor_value, group, nested, covars(list), factors(list), random(list), dup"""
    out = pd.DataFrame({"id": df[m["id"]].astype(str).str.strip(), "age": num(df[m["age"]]),
                        "trait": num(df[m["trait"]]), "row": np.arange(len(df))})
    if m.get("age_round"): out["age"] = np.round(out.age / m["age_round"]) * m["age_round"]
    keep = df[m["id"]].notna().values & np.isfinite(out.age.values)
    out = out[keep].copy(); rows = out.row.values
    meta = dict(n_raw=len(df), n_dropped=int((~keep).sum()))
    g = m.get("group", "")
    meta["has_group"] = bool(g)
    out["group"] = df[g].values[rows] if g else np.nan
    if g and m.get("nested", True):
        out["id"] = np.where(pd.isna(out.group), out.id, out.group.astype(str) + "/" + out.id)
    last = out.groupby("id").age.transform("max"); first = out.groupby("id").age.transform("min")
    meta["alr_mapped"] = m["alr"] not in ("", "__AUTO_LAST__")
    out["alr"] = num(df[m["alr"]]).values[rows] if meta["alr_mapped"] else last
    meta["life_auto"] = m["life"] == "__AUTO_LAST__"
    meta["has_life"] = meta["life_auto"] or bool(m["life"])
    out["life"] = last if meta["life_auto"] else (num(df[m["life"]]).values[rows] if m["life"] else np.nan)
    meta["n_censored"] = 0
    if m.get("censor") and not meta["life_auto"]:
        cr = (df[m["censor"]].astype(str).values[rows] == str(m["censor_value"]))
        cids = out.id[cr].unique(); meta["n_censored"] = len(cids)
        out.loc[out.id.isin(cids), "life"] = np.nan
    meta["entry_mapped"] = m["entry"] not in ("", "__AUTO_FIRST__")
    out["entry"] = num(df[m["entry"]]).values[rows] if meta["entry_mapped"] else first
    covs, types = [], {}
    for c in m.get("covars", []):
        cn = "cv_" + re.sub("[^A-Za-z0-9_.]", ".", c)
        if c in m.get("factors", []):
            out[cn] = df[c].astype("string").str.strip().values[rows]
        else:
            out[cn] = num(df[c]).values[rows]
        covs.append(cn); types[cn] = "factor" if c in m.get("factors", []) else "num"
    res = []
    for r in m.get("random", []):
        rn = "re_" + re.sub("[^A-Za-z0-9_.]", ".", r); out[rn] = df[r].astype(str).values[rows]; res.append(rn)
    meta.update(covars=covs, cov_types=types, random_terms=res)
    meta["n_dup"] = int(out.duplicated(["id", "age"]).sum())
    if m.get("dup") == "mean" and meta["n_dup"]:
        tm = out.groupby(["id", "age"]).trait.transform(lambda v: v[np.isfinite(v)].mean() if np.isfinite(v).any() else np.nan)
        out["trait"] = tm; out = out.drop_duplicates(["id", "age"])
    out = out.sort_values(["id", "age"]).reset_index(drop=True)
    return out, meta

def infer_age_step(age, idv=None):
    age = np.asarray(age, float); ok = np.isfinite(age)
    a = age[ok]
    if len(a) < 2: return np.nan
    tol = 1e-6 * max(1, np.ptp(a))
    d = np.array([])
    if idv is not None:
        g = np.asarray(idv)[ok]; o = np.lexsort((a, g)); a2 = a[o]; g2 = g[o]
        same = g2[1:] == g2[:-1]; d = np.diff(a2)[same]
    d = d[np.isfinite(d) & (d > tol)]
    if not len(d):
        u = np.unique(a); d = np.diff(u); d = d[d > tol]
        if not len(d): return np.nan
    d = np.array([float(f"{x:.6g}") for x in d])
    vals, cnt = np.unique(d, return_counts=True)
    common = vals[cnt / len(d) >= 0.05]
    return common.min() if len(common) else np.median(d)

def individual_metrics(dat):
    okt = np.isfinite(dat.trait)
    g = dat.groupby("id", sort=False)
    im = pd.DataFrame({"n_rows": g.size(), "first_recorded": g.age.min(), "last_recorded": g.age.max(),
                       "alr": g.alr.mean(), "lifespan": g.life.mean(), "entry": g.entry.mean()})
    t = dat[okt].groupby("id")
    im["n_trait"] = t.size().reindex(im.index).fillna(0).astype(int)
    im["mean_age"] = t.age.mean().reindex(im.index)
    im["trait_mean"] = t.trait.mean().reindex(im.index)
    return im

def suggest_family(trait, age):
    ok = np.isfinite(trait) & np.isfinite(age); v = trait[ok]; a = age[ok]
    if len(v) < 10: return "gaussian", "too few"
    if not (np.all(v >= 0) and np.all(np.abs(v - np.round(v)) < 1e-8)): return "gaussian", "continuous"
    zeros = np.mean(v == 0); disp = zr = np.nan
    if len(np.unique(a)) >= 3 and v.mean() > 0:
        X = np.column_stack([np.ones_like(a), a, a ** 2])
        mu = poisson_glm(X, v)
        disp = np.sum((v - mu) ** 2 / mu) / max(1, len(v) - 3)
        ez = np.mean(np.exp(-mu)); zr = zeros / ez if ez > 0 else np.inf
    fam = "zinb" if (zr > 1.5 and zeros > 0.1) else ("nbinom2" if disp > 2 else "poisson")
    return fam, f"zeros {100*zeros:.0f}%, zero ratio {zr:.2f}, dispersion {disp:.2f}"

def poisson_glm(X, y, it=50):
    s = X.std(0); s[s == 0] = 1; m = X.mean(0); m[0] = 0; s[0] = 1
    Xs = (X - m) / s
    b = np.zeros(X.shape[1]); b[0] = np.log(y.mean())
    for _ in range(it):
        eta = np.clip(Xs @ b, -30, 30); mu = np.exp(eta)
        z = eta + (y - mu) / mu; W = mu
        bn = np.linalg.lstsq(Xs * np.sqrt(W)[:, None], z * np.sqrt(W), rcond=None)[0]
        if np.max(np.abs(bn - b)) < 1e-9: b = bn; break
        b = bn
    return np.exp(np.clip(Xs @ b, -30, 30))

def integrity(dat, meta):
    im = individual_metrics(dat); step = infer_age_step(dat.age.values, dat.id.values); r = {}
    r["rows_used"] = f"{meta['n_raw']-meta['n_dropped']}/{meta['n_raw']}"
    r["trait_NA_rows"] = int((~np.isfinite(dat.trait)).sum())
    r["dup_id_age"] = meta["n_dup"]
    r["single_record_%"] = round(100 * (im.n_trait == 1).mean(), 1)
    r["n_individuals"] = len(im)
    if meta["alr_mapped"]: r["ALR_mismatch"] = int((np.abs(im.alr - im.last_recorded) > 1e-8).sum())
    if meta["has_life"] and not meta["life_auto"]:
        r["LS_missing"] = int((~np.isfinite(im.lifespan)).sum())
        r["LS<ALR"] = int((np.isfinite(im.lifespan) & (im.lifespan < im.last_recorded - 1e-8)).sum())
        gap = ((im.lifespan - im.last_recorded) / step).dropna()
        if len(gap): r["LS-ALR steps"] = f"median {gap.median():.2f}; {100*(gap>=1-1e-8).mean():.0f}% >=1"
    r["step"] = step
    if np.isfinite(step):
        fa = dat.groupby("id").age.transform("min"); rel = (dat.age - fa) / step
        off = np.abs(rel - np.round(rel)) > 0.01
        r["off_schedule_share"] = round(off.mean(), 3); r["irregular"] = off.mean() > 0.2
    r["distinct_ages"] = dat.age.nunique()
    r["n_afr_values"] = im.entry.nunique()
    r["AFR-ALR r"] = round(np.corrcoef(im.entry, im.alr)[0, 1], 2) if im.entry.nunique() > 1 and im.alr.nunique() > 1 else np.nan
    r["median_records"] = im.n_trait.median()
    r["family"] = suggest_family(dat.trait.values, dat.age.values)
    r["age_range"] = (dat.age.min(), dat.age.max())
    return r, im

def availability(dat, meta):
    im = individual_metrics(dat); im = im[im.n_trait > 0]
    ok = {m: True for m in MODEL_IDS}; why = {}
    if not im.alr.std() > 0:
        for m in ["M2", "M4", "M7", "M8", "M9", "M10"]: ok[m] = False
    if not meta["has_life"] or meta["life_auto"] or not (im.lifespan.std() > 0): ok["M6"] = False
    afr = im.entry.fillna(im.first_recorded)
    if not afr.std() > 0:
        for m in ["M7", "M8", "M9", "M10"]: ok[m] = False
    default = dict(ok)
    af = afr.dropna()
    if ok["M7"] and len(af):
        mode = af.value_counts().index[0]; nd = int((np.abs(af - mode) > 1e-8).sum())
        if nd < max(5, 0.05 * len(af)):
            for m in ["M7", "M8", "M9", "M10"]: default[m] = False
            why["M7-10"] = f"only {nd} AFR differ from mode"
    if ok["M6"] and (~np.isfinite(im.lifespan)).any():
        default["M6"] = False; why["M6"] = f"LS missing for {(~np.isfinite(im.lifespan)).sum()}"
    return ok, default, why

# ------------------------------------------------------------------ model data and formulas
def prepare(dat, fun="Quadratic", standardise=True):
    d = dat[np.isfinite(dat.trait) & np.isfinite(dat.age)].copy()
    im = individual_metrics(dat)
    afr = im.entry.where(np.isfinite(im.entry), im.first_recorded)
    raw = {"ALR": im.alr, "LS": im.lifespan, "AFR": afr}
    for v, s in raw.items():
        x = s.values[np.isfinite(s.values)]
        c = x.mean() if standardise and len(x) else 0
        sc = x.std(ddof=1) if standardise and len(x) > 1 and x.std(ddof=1) > 0 else 1
        d[v] = (d.id.map(s) - c) / sc
    a = d.age.values; mu, sd = a.mean(), a.std(ddof=1)
    z = (a - mu) / sd
    if fun in ("Linear", "Quadratic", "Cubic"):
        x = z if standardise else a
        d["f1"] = x; basis = ["f1"]
        if fun != "Linear": d["f2"] = x ** 2; basis.append("f2")
        if fun == "Cubic": d["f3"] = x ** 3; basis.append("f3")
    elif fun == "Logarithmic":
        lx = np.log(a) if a.min() > 0 else np.log(a - a.min() + 1)
        d["f1"] = (lx - lx.mean()) / lx.std(ddof=1) if standardise else lx; basis = ["f1"]
    else:
        ex = np.exp(-z); d["f1"] = (ex - ex.mean()) / ex.std(ddof=1) if standardise else ex; basis = ["f1"]
    for b in basis:
        d["mean_" + b] = d.groupby("id")[b].transform("mean"); d["delta_" + b] = d[b] - d["mean_" + b]
    return d, basis

def formulas(b, covars=(), cov_age=(), cov_pairs=()):
    """linear 'among' (app default). Terms are tuples of variable names."""
    B = [(x,) for x in b]; dl = [("delta_" + x,) for x in b]
    inter = lambda v: [(x, v) for x in b]
    core = {
        "M1": B, "M2": B + [("ALR",)], "M3": [("mean_f1",)] + dl,
        "M4": B + [("ALR",)] + inter("ALR"),
        "M5": [("mean_f1",)] + dl + [("mean_f1", "delta_" + x) for x in b],
        "M6": B + [("LS",)] + inter("LS"),
        "M7": B + [("ALR",), ("AFR",)],
        "M8": B + [("ALR",), ("AFR",)] + inter("ALR") + inter("AFR"),
        "M9": B + [("ALR",)] + inter("ALR") + [("AFR",)],
        "M10": B + [("AFR",)] + inter("AFR") + [("ALR",)],
    }
    for m in core:
        t = [(c,) for c in covars] + core[m] + [tuple(p.split(":")) for p in cov_pairs]
        at = ([("mean_f1",)] + dl) if m in ("M3", "M5") else B
        for c in cov_age: t += [(c,) + x for x in at]
        core[m] = t
    return core

def design(d, terms, factor_cols=()):
    cols = [np.ones(len(d))]; names = ["(Intercept)"]
    dummies = {}
    for f in factor_cols:
        lv = sorted(d[f].dropna().unique()); dummies[f] = {l: (d[f] == l).astype(float).values for l in lv[1:]}
    for t in terms:
        parts = [[("", np.ones(len(d)))]]
        for v in t:
            if v in dummies: parts.append([(f"{v}{l}", c) for l, c in dummies[v].items()])
            else: parts.append([(v, d[v].values.astype(float))])
        combos = [("", np.ones(len(d)))]
        for p in parts[1:]:
            combos = [((a + ":" + b).strip(":"), ca * cb) for a, ca in combos for b, cb in p]
        for nm, c in combos: cols.append(c); names.append(nm)
    X = np.column_stack(cols)
    # drop exactly collinear columns like lme4 (rank-deficient)
    q, r = np.linalg.qr(X); keep = np.abs(np.diag(r)) > 1e-7 * np.abs(np.diag(r)).max()
    return X[:, keep], [n for n, k in zip(names, keep) if k]

# ------------------------------------------------------------------ ML LMM
class LMM:
    """y = Xb + Z_id u + Z_g v + e; u_j ~ N(0, s2*D) (1x1 or 2x2), v ~ N(0, s2*tau^2 I)."""
    def __init__(self, y, X, ids, slope=None, slope_type="none", gfac=None):
        o = np.argsort(ids, kind="stable")
        self.y = y[o]; self.X = X[o]; ids = np.asarray(ids)[o]
        self.n, self.p = X.shape
        _, self.start, self.cnt = np.unique(ids, return_index=True, return_counts=True)
        self.gidx = np.repeat(np.arange(len(self.start)), self.cnt)
        self.st = slope_type
        self.x1 = slope[o] if slope is not None else np.zeros(self.n)
        if gfac is not None:
            _, gi = np.unique(np.asarray(gfac)[o], return_inverse=True)
            self.G = np.zeros((self.n, gi.max() + 1)); self.G[np.arange(self.n), gi] = 1
        else: self.G = None
        self.S1 = np.add.reduceat(self.x1, self.start); self.S11 = np.add.reduceat(self.x1 ** 2, self.start)
        self.ntheta = {"none": 1, "uncorrelated": 2, "correlated": 3}[slope_type] + (self.G is not None)

    def _Ainv_apply(self, M, L):
        """A = I + Z D Z' per individual; D = L L'. returns A^{-1} M and log|A|"""
        k = 1 if self.st == "none" else 2
        red = lambda v: np.add.reduceat(v, self.start, axis=0)
        if k == 1:
            d = L[0, 0] ** 2
            c = 1 + d * self.cnt  # |I + d 11'|
            ZtM = red(M)
            corr = (d / c)[:, None] * ZtM
            AiM = M - corr[self.gidx]
            return AiM, np.log(c).sum()
        # 2x2: A^{-1} = I - Z (D^{-1} + Z'Z)^{-1} Z'  ; use form with L to avoid D^{-1}: 
        # (I + Z L L' Z')^{-1} = I - Z L (I + L' Z'Z L)^{-1} L' Z'
        m = len(self.start)
        ZZ = np.empty((m, 2, 2)); ZZ[:, 0, 0] = self.cnt; ZZ[:, 0, 1] = ZZ[:, 1, 0] = self.S1; ZZ[:, 1, 1] = self.S11
        Mm = np.eye(2)[None] + np.einsum("ji,mjk,kl->mil", L, ZZ, L)
        Minv = np.linalg.inv(Mm); logdet = np.log(np.linalg.det(Mm)).sum()
        Mcol = M if M.ndim == 2 else M[:, None]
        Zt0 = red(Mcol); Zt1 = red(self.x1[:, None] * Mcol)
        ZtM = np.stack([Zt0, Zt1], axis=1)  # m x 2 x c
        W = np.einsum("ij,mjk,kl,mlc->mic", L, Minv, L.T, ZtM)
        corr = W[self.gidx, 0, :] + self.x1[:, None] * W[self.gidx, 1, :]
        return Mcol - corr, logdet

    def _L(self, th):
        if self.st == "none": return np.array([[th[0]]])
        if self.st == "uncorrelated": return np.diag([th[0], th[1]])
        return np.array([[th[0], 0], [th[1], th[2]]])

    def dev(self, th):
        L = self._L(th)
        M = np.column_stack([self.X, self.y] + ([self.G] if self.G is not None else []))
        AiM, logdet = self._Ainv_apply(M, L)
        p = self.p
        if self.G is not None:
            tau2 = th[-1] ** 2
            Gt = M[:, p + 1:]; AiG = AiM[:, p + 1:]
            C = np.eye(Gt.shape[1]) + tau2 * Gt.T @ AiG
            sign, ld = np.linalg.slogdet(C); logdet += ld
            B = M[:, :p + 1]; AiB = AiM[:, :p + 1]
            Q = B.T @ AiB - tau2 * (B.T @ AiG) @ np.linalg.solve(C, AiG.T @ B)
        else:
            Q = M.T @ AiM
        XtX = Q[:p, :p]; Xty = Q[:p, p]; yty = Q[p, p]
        try: beta = np.linalg.solve(XtX, Xty)
        except np.linalg.LinAlgError: return 1e15, None
        rss = yty - Xty @ beta
        if rss <= 0: return 1e15, None
        s2 = rss / self.n
        return self.n * np.log(2 * np.pi * s2) + logdet + self.n, (beta, s2)

    def fit(self):
        k = {"none": 1, "uncorrelated": 2, "correlated": 3}[self.st]
        x0 = [1.0] * k if self.st != "correlated" else [1.0, 0.0, 0.5]
        if self.G is not None: x0.append(0.5)
        bnds = []
        for i in range(k): bnds.append((None, None) if (self.st == "correlated" and i == 1) else (0, None))
        if self.G is not None: bnds.append((0, None))
        best = None
        for start in (x0, [0.3] * len(x0)):
            r = minimize(lambda t: self.dev(t)[0], start, method="L-BFGS-B", bounds=bnds)
            if best is None or r.fun < best.fun: best = r
        self.theta = best.x; self.deviance = best.fun
        self.beta, self.s2 = self.dev(best.x)[1]
        self.df = self.p + 1 + self.ntheta
        self.aic = self.deviance + 2 * self.df
        self.singular = bool(np.any(np.abs(best.x[[i for i, b in enumerate(bnds) if b[0] == 0]]) < 1e-4))
        return self

def fit_suite(dat, meta, fun="Quadratic", models=MODEL_IDS, slope_type="none", covars=(), factors=(), cov_age=(),
              cov_pairs=(), gcol=None, extra_re=None):
    d, b = prepare(dat, fun)
    fs = formulas(b, covars, cov_age, cov_pairs)
    ok, default, why = availability(dat, meta)
    models = [m for m in models if ok[m]]
    need = set(["id", "trait"])
    for m in models:
        for t in fs[m]: need.update(t)
    if gcol: need.add(gcol)
    cc = np.ones(len(d), bool)
    for v in need:
        x = d[v]
        cc &= (np.isfinite(x.values.astype(float)) if v not in factors and v not in ("id", gcol) else x.notna().values)
    dd = d[cc].copy()
    res = {}
    for m in models:
        X, names = design(dd, fs[m], factors)
        mod = LMM(dd.trait.values, X, dd.id.values, dd.f1.values, slope_type,
                  dd[gcol].values if gcol else None).fit()
        res[m] = dict(AIC=mod.aic, df=mod.df, beta=dict(zip(names, mod.beta)), theta=mod.theta, sing=mod.singular)
    tab = pd.DataFrame({m: {"AIC": r["AIC"], "df": r["df"], "sing": r["sing"]} for m, r in res.items()}).T
    tab["dAIC"] = tab.AIC - tab.AIC.min()
    return tab.sort_values("AIC"), res, dict(n=len(dd), n_id=dd.id.nunique(), dropped=int((~cc).sum()), default=default, why=why)
