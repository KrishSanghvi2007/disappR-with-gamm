"""Fit the paper-matched specification for each example (as the app documents it) and print the terms the paper reports."""
import re, numpy as np, pandas as pd, warnings
warnings.filterwarnings("ignore")
exec(open("emp_defaults.py").read().split("FAM = {")[0])
FAM = {"gaussian": "gaussian", "poisson": "poisson", "nbinom2": "nb", "zinb": "nb", "binomial": "binomial"}
WANT = {"fly": ["M2", "M6"], "bichet": ["M3", "M7"], "bichet_marmot": ["M7"], "moullec_swift": ["M7"],
        "pasztor_apollo": ["M3"], "wynn": ["M3"], "sanghvi_female": ["M2", "M6"], "allain": ["M10"],
        "bouwhuis": ["M4"], "warner": ["M1"], "mckennaell_breeding": ["M2"], "mckennaell_weight": ["M2"],
        "szejnersigal_activity": ["M1", "M6"]}
for k, e in EX.items():
    fam = FAM.get(e["family"]); fun = e["fun"]; mp = e["map"]
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
    cov = [c for c in ([mp.get("covars")] if isinstance(mp.get("covars"), str) else list(mp.get("covars", []))) if c in d.columns]
    fac = [mp.get("cov_factor")] if isinstance(mp.get("cov_factor"), str) else list(mp.get("cov_factor", []))
    cols, cn = [], []
    for c in cov:                                   # numeric covariates coerced, so "UNK" becomes missing
        num = pd.to_numeric(d[c], errors="coerce")
        if c in fac or num.notna().mean() < 0.5:
            d[c] = d[c].astype(str)
        else:
            d[c] = num
    keep = d[["id", "age", "trait", "alr", "afr"] + (["life"] if has_ls else []) + cov].dropna()
    d = d.loc[keep.index].copy()
    if has_ls and d["life"].nunique() < 2: has_ls = False
    for c in cov:
        v = d[c]
        if not pd.api.types.is_numeric_dtype(v):
            for lev in sorted(v.unique())[1:]: cols.append((v == lev).astype(float).values); cn.append(f"{c}={lev}")
        else:
            x = v.values.astype(float); cols.append((x - x.mean()) / (x.std() or 1)); cn.append(c)
    covX = np.column_stack(cols) if cols else np.zeros((len(d), 0))
    y = d["trait"].values.astype(float); gid = pd.factorize(d["id"])[0]
    out = []
    for m in WANT.get(k, []):
        if m == "M6" and not has_ls: continue
        try:
            X, nm = design_named(d, fun, m, covX, cn)
            if np.linalg.matrix_rank(X) < X.shape[1]: out.append(f"{m}: rank-deficient"); continue
            if fam == "gaussian":
                aic, b, se = lmm_beta(X, y, gid)
                key = [n for n in nm if n.startswith(("age", "dage", "mean_age")) or n in ("ALR", "AFR", "LS") or ":" in n]
                out.append(f"{m} " + " ".join(f"{n}={b[nm.index(n)]:+.3f}({abs(b[nm.index(n)]/se[nm.index(n)]):.1f}sd)" for n in key[:6]))
            else:
                aic, b = glmm_beta(X, y, gid, fam)
                key = [n for n in nm if n.startswith(("age", "dage", "mean_age")) or n in ("ALR", "AFR", "LS") or ":" in n]
                out.append(f"{m} " + " ".join(f"{n}={b[nm.index(n)]:+.3f}" for n in key[:6]))
        except Exception as ex:
            out.append(f"{m}: failed ({type(ex).__name__})")
    print(f"## {k} [{e['family']}/{fun}] n={len(d)} ids={d['id'].nunique()}")
    for o in out: print("   ", o)

# ---- random-slope refits for the two Gaussian examples whose papers used them ----
print("\n=== with a correlated random slope (as the papers fitted) ===")
exec(open("emp_extra.py").read().split("def prep(")[0].split("exec(open")[2].split(")\n", 1)[1]) if False else None
def lmm_slope_beta(X, y, gid, zage):
    from scipy.optimize import minimize
    o = np.argsort(gid, kind="stable"); X, y, z, g = X[o], y[o], zage[o], gid[o]
    idx = np.r_[0, np.where(np.diff(g) != 0)[0] + 1]; ends = np.r_[idx[1:], len(g)]
    Z = np.column_stack([np.ones(len(y)), z])
    A = np.stack([X[a:b].T @ Z[a:b] for a, b in zip(idx, ends)])
    B = np.stack([Z[a:b].T @ Z[a:b] for a, b in zip(idx, ends)])
    Zty = np.stack([Z[a:b].T @ y[a:b] for a, b in zip(idx, ends)])
    XtX, Xty, yty, N, p = X.T @ X, X.T @ y, float(y @ y), len(y), X.shape[1]
    def parts(par):
        s0, s1, rho = np.exp(par[0]), np.exp(par[1]), np.tanh(par[2])
        P = np.array([[s0 ** 2, rho * s0 * s1], [rho * s0 * s1, s1 ** 2]])
        M = np.linalg.inv(np.linalg.inv(P + 1e-10 * np.eye(2))[None, :, :] + B)
        XVX = XtX - np.einsum("kpi,kij,kqj->pq", A, M, A)
        XVy = Xty - np.einsum("kpi,kij,kj->p", A, M, Zty)
        yVy = yty - np.einsum("ki,kij,kj->", Zty, M, Zty)
        beta = np.linalg.solve(XVX, XVy)
        rss = yVy - 2 * beta @ XVy + beta @ XVX @ beta
        _, ld = np.linalg.slogdet(np.eye(2)[None, :, :] + np.einsum("ij,kjl->kil", P, B))
        return beta, rss, XVX, float(ld.sum())
    def nll(par):
        try: beta, rss, _, ld = parts(par)
        except np.linalg.LinAlgError: return 1e12
        if rss <= 0: return 1e12
        return 0.5 * (N * np.log(2 * np.pi * rss / N) + ld + N)
    r = minimize(nll, np.array([-0.5, -1.0, 0.0]), method="Nelder-Mead", options=dict(maxiter=400, xatol=1e-3, fatol=1e-3))
    beta, rss, XVX, _ = parts(r.x)
    se = np.sqrt(np.diag(np.linalg.inv(XVX)) * rss / N)
    return 2 * r.fun + 2 * (p + 4), beta, se

for k, models in [("bichet", ["M3", "M7", "M2", "M4"]), ("pasztor_apollo", ["M3", "M5"])]:
    e = EX[k]; mp = e["map"]
    d = pd.read_csv(ROOT + "inst/app/data/" + e["file"])
    d = d.rename(columns={mp["id"]: "id", mp["age"]: "age", mp["trait"]: "trait"})
    for c in ("age", "trait"): d[c] = pd.to_numeric(d[c], errors="coerce")
    d = d[d["age"].notna() & d["trait"].notna()].copy()
    d["alr"] = d.groupby("id")["age"].transform("max"); d["afr"] = d.groupby("id")["age"].transform("min")
    cov = [c for c in ([mp.get("covars")] if isinstance(mp.get("covars"), str) else list(mp.get("covars", []))) if c in d.columns]
    fac = [mp.get("cov_factor")] if isinstance(mp.get("cov_factor"), str) else list(mp.get("cov_factor", []))
    for c in cov:
        num = pd.to_numeric(d[c], errors="coerce")
        d[c] = d[c].astype(str) if (c in fac or num.notna().mean() < 0.5) else num
    d = d.loc[d[["id", "age", "trait", "alr", "afr"] + cov].dropna().index].copy()
    cols, cn = [], []
    for c in cov:
        v = d[c]
        if not pd.api.types.is_numeric_dtype(v):
            for lev in sorted(v.unique())[1:]: cols.append((v == lev).astype(float).values); cn.append(f"{c}={lev}")
        else:
            x = v.values.astype(float); cols.append((x - x.mean()) / (x.std() or 1)); cn.append(c)
    covX = np.column_stack(cols) if cols else np.zeros((len(d), 0))
    y = d["trait"].values.astype(float); gid = pd.factorize(d["id"])[0]
    zage = ((d["age"] - d["age"].mean()) / d["age"].std()).values
    print(f"## {k}")
    for m in models:
        X, nm = design_named(d, e["fun"], m, covX, cn)
        a_ri, b_ri, se_ri = lmm_beta(X, y, gid)
        a_rs, b_rs, se_rs = lmm_slope_beta(X, y, gid, zage)
        key = [n for n in nm if n.startswith(("age", "dage", "mean_age")) or n in ("ALR", "AFR") or ":" in n][:4]
        f = lambda b, se, n: f"{n}={b[nm.index(n)]:+.3f}({abs(b[nm.index(n)]/se[nm.index(n)]):.1f}sd)"
        print(f"   {m}: intercept-only AIC {a_ri:.1f} | slope AIC {a_rs:.1f} ({'slope better' if a_rs < a_ri - 2 else 'no gain'})")
        print(f"      intercept-only: " + " ".join(f(b_ri, se_ri, n) for n in key))
        print(f"      with slope    : " + " ".join(f(b_rs, se_rs, n) for n in key))
