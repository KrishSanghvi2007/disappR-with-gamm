"""Fit each bundled example with the app's own default settings (family, ageing function, model set, covariates),
rank the preset models by AIC, and report the key coefficients of the winner."""
import re, time, numpy as np, pandas as pd, warnings
from scipy.optimize import minimize, minimize_scalar
from scipy.special import gammaln, logsumexp
warnings.filterwarnings("ignore")
ROOT = "../../../"
exec(open("emp_common.py").read().replace('../../../', ROOT))

def design_named(d, fun, model, covX, covn):
    b = basis(fun, d["age"].values); k = b.shape[1]
    z = lambda v: (v - np.nanmean(v)) / (np.nanstd(v) if np.nanstd(v) > 0 else 1)
    ALR, AFR = z(d["alr"].values), z(d["afr"].values)
    LS = z(d["life"].values) if "life" in d else None
    mb = np.column_stack([pd.Series(b[:, j]).groupby(d["id"].values).transform("mean").values for j in range(k)])
    dl = b - mb
    bn = [f"age{j+1}" for j in range(k)]
    I = lambda A, v, nm, an: (np.column_stack([A[:, j] * v for j in range(A.shape[1])]), [f"{a}:{nm}" for a in an])
    if model == "M1": M, nm = b, bn
    elif model == "M2": M, nm = np.column_stack([b, ALR]), bn + ["ALR"]
    elif model == "M3": M, nm = np.column_stack([mb[:, 0], dl]), ["mean_age"] + [f"d{n}" for n in bn]
    elif model == "M4":
        X2, n2 = I(b, ALR, "ALR", bn); M, nm = np.column_stack([b, ALR, X2]), bn + ["ALR"] + n2
    elif model == "M5":
        X2, n2 = I(dl, mb[:, 0], "mean_age", [f"d{n}" for n in bn])
        M, nm = np.column_stack([mb[:, 0], dl, X2]), ["mean_age"] + [f"d{n}" for n in bn] + n2
    elif model == "M6":
        X2, n2 = I(b, LS, "LS", bn); M, nm = np.column_stack([b, LS, X2]), bn + ["LS"] + n2
    elif model == "M7": M, nm = np.column_stack([b, ALR, AFR]), bn + ["ALR", "AFR"]
    elif model == "M8":
        X2, n2 = I(b, ALR, "ALR", bn); X3, n3 = I(b, AFR, "AFR", bn)
        M, nm = np.column_stack([b, ALR, AFR, X2, X3]), bn + ["ALR", "AFR"] + n2 + n3
    elif model == "M9":
        X2, n2 = I(b, ALR, "ALR", bn); M, nm = np.column_stack([b, ALR, AFR, X2]), bn + ["ALR", "AFR"] + n2
    elif model == "M10":
        X3, n3 = I(b, AFR, "AFR", bn); M, nm = np.column_stack([b, ALR, AFR, X3]), bn + ["ALR", "AFR"] + n3
    X = np.column_stack([np.ones(len(d)), M] + ([covX] if covX.shape[1] else []))
    return X, ["(int)"] + nm + list(covn)

def lmm_beta(X, y, gid):
    o = np.argsort(gid, kind="stable"); X, y, g = X[o], y[o], gid[o]
    idx = np.r_[0, np.where(np.diff(g) != 0)[0] + 1]
    n_i = np.diff(np.r_[idx, len(g)]).astype(float)
    Sx = np.add.reduceat(X, idx, axis=0); Sy = np.add.reduceat(y, idx)
    XtX, Xty, yty, N, p = X.T @ X, X.T @ y, float(y @ y), len(y), X.shape[1]
    def parts(lt):
        th = np.exp(lt); c = th / (1 + n_i * th)
        A = XtX - Sx.T @ (c[:, None] * Sx); b = Xty - Sx.T @ (c * Sy)
        beta = np.linalg.solve(A, b); Sr = Sy - Sx @ beta
        rss = yty - 2 * beta @ Xty + beta @ XtX @ beta - float(np.sum(c * Sr ** 2))
        return beta, rss, A
    def nll(lt):
        try: beta, rss, _ = parts(lt)
        except np.linalg.LinAlgError: return 1e12
        if rss <= 0: return 1e12
        th = np.exp(lt)
        return 0.5 * (N * np.log(2 * np.pi * rss / N) + float(np.sum(np.log1p(n_i * th))) + N)
    r = minimize_scalar(nll, bounds=(-9, 9), method="bounded")
    beta, rss, A = parts(r.x)
    se = np.sqrt(np.diag(np.linalg.inv(A)) * rss / N)
    return 2 * r.fun + 2 * (p + 2), beta, se

Q = 9; gh_x, gh_w = np.polynomial.hermite.hermgauss(Q); log_w = np.log(gh_w) - 0.5 * np.log(np.pi)
def glmm_beta(X, y, gid, family):
    o = np.argsort(gid, kind="stable"); X, y, g = X[o], y[o], gid[o]
    idx = np.r_[0, np.where(np.diff(g) != 0)[0] + 1]; n, p = X.shape
    def fg(par):
        beta, ls = par[:p], par[p]; th = np.exp(par[p + 1]) if family == "nb" else None
        u = np.sqrt(2.0) * np.exp(ls) * gh_x
        eta = (X @ beta)[:, None] + u[None, :]
        if family == "poisson":
            mu = np.exp(np.clip(eta, -30, 30)); lp = y[:, None] * eta - mu - gammaln(y + 1)[:, None]; dd = y[:, None] - mu
        elif family == "binomial":
            m = np.clip(eta, -30, 30); lp = y[:, None] * m - np.log1p(np.exp(m)); dd = y[:, None] - 1 / (1 + np.exp(-m))
        else:
            mu = np.exp(np.clip(eta, -30, 30))
            lp = (gammaln(y[:, None] + th) - gammaln(th) - gammaln(y + 1)[:, None] + th * np.log(th / (th + mu)) + y[:, None] * np.log(mu / (th + mu)))
            dd = (y[:, None] - mu) * th / (th + mu)
        S = np.add.reduceat(lp, idx, axis=0) + log_w[None, :]
        ll_i = logsumexp(S, axis=1); pw = np.exp(S - ll_i[:, None])
        w = np.repeat(pw, np.diff(np.r_[idx, n]), axis=0)
        gb = X.T @ (dd * w).sum(axis=1); gs = float(((dd * w) * u[None, :]).sum())
        gr = np.r_[gb, gs, 0.0] if family == "nb" else np.r_[gb, gs]
        return -ll_i.sum(), -gr
    p0 = np.r_[np.zeros(p), -0.5] if family != "nb" else np.r_[np.zeros(p), -0.5, 0.5]
    p0[0] = np.log(max(y.mean(), 0.1)) if family in ("poisson", "nb") else 0.0
    r = minimize(fg, p0, jac=True, method="L-BFGS-B", options=dict(maxiter=400))
    k = p + (2 if family == "nb" else 1)
    return 2 * r.fun + 2 * k, r.x[:p]

FAM = {"gaussian": "gaussian", "poisson": "poisson", "nbinom2": "nb", "zinb": "nb", "binomial": "binomial"}
rows = []
for k, e in EX.items():
    fam = FAM.get(e["family"]); fun = e["fun"]; models = e["models"] or ["M1", "M2", "M3", "M4", "M5"]
    mp = e["map"]; d = pd.read_csv(ROOT + "inst/app/data/" + e["file"])
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
    keep = d[["id", "age", "trait", "alr", "afr"] + (["life"] if has_ls else []) + cov].dropna()
    d = d.loc[keep.index].copy()
    if has_ls and d["life"].nunique() < 2: has_ls = False
    cols, cn = [], []
    for c in cov:
        v = d[c]
        if c in fac or v.dtype == object:
            for lev in sorted(v.astype(str).unique())[1:]: cols.append((v.astype(str) == lev).astype(float).values); cn.append(f"{c}={lev}")
        else:
            x = pd.to_numeric(v, errors="coerce").values; cols.append((x - np.nanmean(x)) / (np.nanstd(x) or 1)); cn.append(c)
    covX = np.column_stack(cols) if cols else np.zeros((len(d), 0))
    y = d["trait"].values.astype(float); gid = pd.factorize(d["id"])[0]
    afr_var = d.groupby("id")["afr"].first().nunique() > 1
    use = [m for m in models if not (m == "M6" and not has_ls) and not (m in ("M7", "M8", "M9", "M10") and not afr_var)]
    res = {}
    for m in use:
        try:
            X, nm = design_named(d, fun, m, covX, cn)
            if np.linalg.matrix_rank(X) < X.shape[1]: continue
            if fam == "gaussian":
                aic, beta, se = lmm_beta(X, y, gid); res[m] = (aic, dict(zip(nm, beta)), dict(zip(nm, se)))
            else:
                aic, beta = glmm_beta(X, y, gid, fam); res[m] = (aic, dict(zip(nm, beta)), None)
        except Exception as ex:
            continue
    if not res: print(f"{k}: no fit"); continue
    best = min(res, key=lambda m: res[m][0])
    aics = sorted(res.items(), key=lambda kv: kv[1][0])
    gap = aics[1][1][0] - aics[0][1][0] if len(aics) > 1 else np.nan
    b, se = res[best][1], res[best][2]
    key = {n: v for n, v in b.items() if n.startswith("age") or n in ("ALR", "AFR", "LS", "mean_age") or ":" in n}
    fmt = lambda n: f"{n}={b[n]:+.3f}" + (f"({abs(b[n]/se[n]):.1f}sd)" if se and se.get(n, 0) > 0 else "")
    print(f"## {k} [{e['family']}/{fun}] best {best}  next +{gap:.1f} AIC | " +
          "; ".join(fmt(n) for n in list(key)[:6]) + f" | n={len(d)}, ids={d['id'].nunique()}")
