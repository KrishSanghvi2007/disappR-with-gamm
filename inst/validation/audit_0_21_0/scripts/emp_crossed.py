"""Refit the Gaussian examples with every random intercept their mapping specifies (individual plus the extra
crossed terms), not just the individual. Woodbury on the stacked design of random effects."""
import re, numpy as np, pandas as pd, warnings
from scipy.optimize import minimize
warnings.filterwarnings("ignore")
exec(open("emp_defaults.py").read().split("FAM = {")[0])

def lmm_crossed(X, y, Zblocks):
    """ML for y = Xb + sum_k Z_k u_k + e, u_k ~ N(0, s_k^2 I), profiled over sigma^2."""
    q = [Z.shape[1] for Z in Zblocks]
    Z = np.hstack(Zblocks)
    ZtZ, ZtX, Zty = Z.T @ Z, Z.T @ X, Z.T @ y
    XtX, Xty, yty, N, p = X.T @ X, X.T @ y, float(y @ y), len(y), X.shape[1]
    def parts(par):
        th = np.exp(np.clip(par, -12, 12))
        d = np.concatenate([np.repeat(t, qi) for t, qi in zip(th, q)])
        M = np.diag(1.0 / d) + ZtZ
        L = np.linalg.cholesky(M)
        solve = lambda B: np.linalg.solve(L.T, np.linalg.solve(L, B))
        XVX = XtX - ZtX.T @ solve(ZtX)
        XVy = Xty - ZtX.T @ solve(Zty)
        yVy = yty - Zty @ solve(Zty)
        beta = np.linalg.solve(XVX, XVy)
        rss = yVy - 2 * beta @ XVy + beta @ XVX @ beta
        logdet = 2 * np.sum(np.log(np.diag(L))) + float(np.sum(np.log(d)))   # log|I + Z D Z'|
        return beta, rss, XVX, logdet
    def nll(par):
        try: beta, rss, _, ld = parts(par)
        except np.linalg.LinAlgError: return 1e12
        if rss <= 0 or not np.isfinite(rss): return 1e12
        return 0.5 * (N * np.log(2 * np.pi * rss / N) + ld + N)
    r = minimize(nll, np.full(len(Zblocks), -1.0), method="Nelder-Mead",
                 options=dict(maxiter=600, xatol=1e-3, fatol=1e-3))
    beta, rss, XVX, _ = parts(r.x)
    se = np.sqrt(np.diag(np.linalg.inv(XVX)) * rss / N)
    return 2 * r.fun + 2 * (p + 1 + len(Zblocks)), beta, se

def dummies(v):
    lev = pd.factorize(v)[0]
    Z = np.zeros((len(v), lev.max() + 1)); Z[np.arange(len(v)), lev] = 1.0
    return Z

TARGET = {"bichet": ["M2", "M3", "M4", "M7"], "warner": ["M1", "M2", "M4"], "wynn": ["M2", "M3", "M4"],
          "pasztor_apollo": ["M3", "M5"], "moullec_swift": ["M7", "M2"], "bouwhuis": ["M2", "M4"],
          "mckennaell_weight": ["M2"]}
for k, models in TARGET.items():
    e = EX[k]; mp = e["map"]
    d = pd.read_csv(ROOT + "inst/app/data/" + e["file"])
    if e["subset"]: d = d[d[e["subset"].group(1)].astype(str).isin(re.findall(r'"([^"]*)"', e["subset"].group(2)))]
    d = d.rename(columns={mp["id"]: "id", mp["age"]: "age", mp["trait"]: "trait"})
    for c in ("age", "trait"): d[c] = pd.to_numeric(d[c], errors="coerce")
    d = d[d["age"].notna() & d["trait"].notna()].copy()
    a, l, en = mp.get("alr", ""), mp.get("life", ""), mp.get("entry", "")
    d["alr"] = pd.to_numeric(d[a], errors="coerce") if a in d.columns else d.groupby("id")["age"].transform("max")
    d["afr"] = pd.to_numeric(d[en], errors="coerce") if en in d.columns else d.groupby("id")["age"].transform("min")
    has_ls = l in d.columns
    if has_ls: d["life"] = pd.to_numeric(d[l], errors="coerce")
    rnd = mp.get("random", []); rnd = [rnd] if isinstance(rnd, str) and rnd else list(rnd)
    rnd = [c for c in rnd if c in d.columns]
    cov = [c for c in ([mp.get("covars")] if isinstance(mp.get("covars"), str) else list(mp.get("covars", []))) if c in d.columns]
    fac = [mp.get("cov_factor")] if isinstance(mp.get("cov_factor"), str) else list(mp.get("cov_factor", []))
    for c in cov:
        num = pd.to_numeric(d[c], errors="coerce")
        d[c] = d[c].astype(str) if (c in fac or num.notna().mean() < 0.5) else num
    d = d.loc[d[["id", "age", "trait", "alr", "afr"] + (["life"] if has_ls else []) + cov + rnd].dropna().index].copy()
    cols, cn = [], []
    for c in cov:
        v = d[c]
        if not pd.api.types.is_numeric_dtype(v):
            for lev in sorted(v.unique())[1:]: cols.append((v == lev).astype(float).values); cn.append(f"{c}={lev}")
        else:
            x = v.values.astype(float); cols.append((x - x.mean()) / (x.std() or 1)); cn.append(c)
    covX = np.column_stack(cols) if cols else np.zeros((len(d), 0))
    y = d["trait"].values.astype(float)
    Zid = dummies(d["id"].values)
    Zs = [Zid] + [dummies(d[c].values) for c in rnd]
    print(f"## {k}: individual + {', '.join(rnd) if rnd else '(none)'} | n={len(d)}")
    for m in models:
        try:
            X, nm = design_named(d, e["fun"], m, covX, cn)
            if np.linalg.matrix_rank(X) < X.shape[1]: print(f"   {m}: rank-deficient"); continue
            a1, b1, s1 = lmm_beta(X, y, pd.factorize(d["id"])[0])
            a2, b2, s2 = lmm_crossed(X, y, Zs)
            key = [n for n in nm if n.startswith(("age", "dage", "mean_age")) or n in ("ALR", "AFR", "LS") or ":" in n][:4]
            f = lambda b, s: " ".join(f"{n}={b[nm.index(n)]:+.3f}({abs(b[nm.index(n)]/s[nm.index(n)]):.1f}sd)" for n in key)
            print(f"   {m}: id-only AIC {a1:.1f} -> full AIC {a2:.1f}")
            print(f"      id only : {f(b1, s1)}")
            print(f"      full    : {f(b2, s2)}")
        except Exception as ex:
            print(f"   {m}: failed ({type(ex).__name__}: {ex})")
