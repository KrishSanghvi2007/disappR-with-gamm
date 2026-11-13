"""Independent simulation verification and validation of disappR's models and decomposition.
Independent implementation in Python (numpy/scipy) of the app's simulation design (simulate_toy_data), its model
definitions (Models 1-6, quadratic, standardised age, linear among-individual terms, random intercept, ML) and its
decomposition (0.9.6 algorithm). It does not execute the R app: agreement here supports the method and the
specification; the R smoke test checks the app's own fits."""
import time, numpy as np, pandas as pd, scipy.sparse as sp
from scipy.optimize import minimize_scalar, minimize
from scipy.linalg import cho_factor, cho_solve
from scipy.stats import chi2, norm
from scipy.special import gammaln
from simdec import simulate, id_age_means, decomp, survivor_target
MODELS = ["M1", "M2", "M3", "M4", "M5", "M6"]
def ri_lmm(y, X, g):
    """Exact ML linear mixed model with one random intercept (profiled; closed-form per group)."""
    n, p = X.shape; G = int(g.max()) + 1
    ni = np.bincount(g, minlength=G).astype(float)
    O = sp.csr_matrix((np.ones(n), (g, np.arange(n))), shape=(G, n))
    S = np.asarray(O @ X); t = np.asarray(O @ y).ravel()
    XtX, Xty, yty = X.T @ X, X.T @ y, y @ y
    def prof(lg, full=False):
        gam = np.exp(lg); c = gam / (1 + ni * gam)
        Sxx = XtX - S.T @ (S * c[:, None]); Sxy = Xty - S.T @ (c * t)
        try: cf = cho_factor(Sxx)
        except np.linalg.LinAlgError: return np.inf
        beta = cho_solve(cf, Sxy); q = yty - np.sum(c * t * t) - Sxy @ beta
        if q <= 0: return np.inf
        dev = n * np.log(2 * np.pi * q / n) + n + np.sum(np.log1p(ni * gam))
        return (dev, beta, q / n, cho_solve(cf, np.eye(p)) * q / n, gam) if full else dev
    lo, hi = np.log(1e-8), np.log(1e4)
    o = minimize_scalar(prof, bounds=(lo, hi), method="bounded", options=dict(xatol=1e-7))
    dev, beta, s2, V, gam = prof(o.x, True)
    return dict(dev=dev, aic=dev + 2 * (p + 2), beta=beta, V=V, s2=s2, s2u=gam * s2, singular=o.x < lo + 1e-3)
def ri_poisson(y, X, g):
    """Laplace (exact per-group 1-D) Poisson GLMM with one random intercept, ML over (beta, log sd)."""
    n, p = X.shape; G = int(g.max()) + 1; st = {"u": np.zeros(G)}
    def obj(par):
        beta, s = par[:p], np.exp(par[p]); off = X @ beta; u = st["u"].copy()
        for _ in range(60):
            eta = off + s * u[g]; mu = np.exp(np.clip(eta, -30, 30))
            gr = s * np.bincount(g, y - mu, minlength=G) - u; h = s * s * np.bincount(g, mu, minlength=G) + 1
            stp = gr / h; u += stp
            if np.max(np.abs(stp)) < 1e-10: break
        st["u"] = u; eta = off + s * u[g]; mu = np.exp(np.clip(eta, -30, 30)); h = s * s * np.bincount(g, mu, minlength=G) + 1
        return -2 * (np.sum(y * eta - mu - gammaln(y + 1)) - 0.5 * u @ u) + np.sum(np.log(h))
    b0 = np.linalg.lstsq(X, np.log(y + 0.5), rcond=None)[0]
    o = minimize(obj, np.concatenate([b0, [np.log(0.3)]]), method="L-BFGS-B", options=dict(maxiter=400))
    return dict(dev=o.fun, aic=o.fun + 2 * (p + 1), beta=o.x[:p], ok=o.success)
def censor(df, rng, share=0.3):
    # an array, not a set: iterating a set of strings follows Python's per-process hash seed, which made the censored
    # cells irreproducible across runs (fixed in disappR 0.20.2; those cells were regenerated)
    ids = df.id.unique(); cens = rng.choice(ids, int(share * len(ids)), replace=False)
    first = df.groupby("id").age.transform("min"); last = df.groupby("id").LS.transform("first")
    cut = df.id.map({i: None for i in ids})
    cuts = {i: rng.uniform(0, 1) for i in cens}
    c = np.array([first.iloc[k] + cuts[i] * (last.iloc[k] - first.iloc[k]) if i in cuts else np.inf for k, i in enumerate(df.id.values)])
    return df[df.age.values <= np.maximum(c, first.values)].reset_index(drop=True)
def model_designs(df):
    a = df.age.values; ma, sa = a.mean(), a.std(ddof=1); f1 = (a - ma) / sa; f2 = f1 ** 2
    g, _ = pd.factorize(df.id); first = ~df.id.duplicated().values
    grp = lambda v: np.bincount(g, v) / np.bincount(g)
    alr = df.groupby("id").age.transform("max").values; ls = df.LS.values.astype(float)
    z = lambda v: (v - v[first].mean()) / (v[first].std(ddof=1) if v[first].std(ddof=1) > 0 else 1)
    ALR, LS = z(alr), z(ls); m1, m2 = grp(f1)[g], grp(f2)[g]; d1, d2 = f1 - m1, f2 - m2
    one = np.ones(len(a))
    X = {"M1": [one, f1, f2], "M2": [one, f1, f2, ALR], "M3": [one, m1, d1, d2], "M4": [one, f1, f2, ALR, f1 * ALR, f2 * ALR],
         "M5": [one, m1, d1, d2, m1 * d1, m1 * d2], "M6": [one, f1, f2, LS, f1 * LS, f2 * LS]}
    mb1, mb2 = grp(f1).mean(), grp(f2).mean()
    def P(ages):
        q1 = (ages - ma) / sa; q2 = q1 ** 2; o = np.ones(len(ages)); z0 = np.zeros(len(ages)); e1, e2 = q1 - mb1, q2 - mb2
        return {"M1": [o, q1, q2], "M2": [o, q1, q2, z0], "M3": [o, o * mb1, e1, e2], "M4": [o, q1, q2, z0, z0, z0],
                "M5": [o, o * mb1, e1, e2, mb1 * e1, mb1 * e2], "M6": [o, q1, q2, z0, z0, z0]}
    return {k: np.column_stack(v) for k, v in X.items()}, (lambda ages: {k: np.column_stack(v) for k, v in P(ages).items()}), g
def one_rep(cell, seed):
    kw = {k: cell[k] for k in ("form", "count", "strength", "sd_type", "sd_dir", "rate_var", "mean_ls", "missingness", "n_id", "mu") if k in cell}
    df, truth, lat = simulate(seed=seed, **kw); rng = np.random.default_rng(seed + 7)
    if cell.get("sampling") == "censored": df = censor(df, rng)
    if cell.get("sampling") == "irregular": df = df.assign(age=df.age + rng.uniform(-0.35, 0.35, len(df)))
    df = df[np.isfinite(df.trait)].reset_index(drop=True)
    X, P, g = model_designs(df); y = df.trait.values.astype(float)
    nobs = pd.Series(np.round(df.age.values)).value_counts(); ev = np.sort(nobs[nobs >= 10].index.values.astype(float))
    ev = ev[ev >= 1]; tr = truth(ev); late = ev[int(0.9 * (len(ev) - 1))]; mid = ev[len(ev) // 2]; early = ev[min(1, len(ev) - 1)]
    PX = P(ev); out = {"seed": seed}
    fits = {}
    for m in (MODELS if not cell.get("count") else ["M1", "M2", "M4"]):
        f = ri_poisson(y, X[m], g) if cell.get("count") else ri_lmm(y, X[m], g); fits[m] = f
        eta = PX[m] @ f["beta"]; pred = np.exp(eta) if cell.get("count") else eta
        dev = 100 * (pred - tr) / np.abs(tr)
        out[f"{m}_aic"] = f["aic"]; out[f"{m}_mape"] = np.mean(np.abs(dev)); out[f"{m}_bias"] = np.mean(dev); out[f"{m}_late"] = dev[ev == late][0]
        if not cell.get("count"):
            se = np.sqrt(np.einsum("ij,jk,ik->i", PX[m], f["V"], PX[m])); k = np.isin(ev, [early, mid, late])
            out[f"{m}_cover"] = np.mean(np.abs(eta[k] - tr[k]) <= 1.96 * se[k]); out[f"{m}_singular"] = bool(f["singular"])
    if not cell.get("count"):
        b, V = fits["M4"]["beta"][4:6], fits["M4"]["V"][4:6, 4:6]
        out["M4_wald_p"] = chi2.sf(b @ np.linalg.solve(V, b), 2)
    ia = id_age_means(df[["id", "age", "trait"]]); dc, cau = decomp(ia)
    if len(dc):
        ages_c = np.round(dc.age.values); sel = np.isin(ages_c, ev)
        out["decomp_mape"] = np.mean(np.abs(dc.fitted.values[sel] - truth(ages_c[sel])) / np.abs(truth(ages_c[sel]))) * 100 if sel.any() else np.nan
        tg = survivor_target(lat, int(ages_c[0]), int(ages_c[-1])); lvl = np.mean(np.abs(list(tg.values())))
        vv = [abs(v - tg[int(a)]) for a, v in zip(ages_c, dc.fitted.values) if int(a) in tg and a in ev]
        out["decomp_vs_target"] = 100 * np.mean(vv) / lvl if vv else np.nan; out["decomp_caution"] = bool(cau)
    obs = ia.assign(a=np.round(ia.age)).groupby("a").trait.mean(); obs = obs[obs.index.isin(ev)]
    out["observed_mape"] = np.mean(np.abs(obs.values - truth(obs.index.values)) / np.abs(truth(obs.index.values))) * 100
    out["n_rows"] = len(df); out["n_ids"] = df.id.nunique()
    return out
def verify_lmm():
    import sys; sys.path.insert(0, "/home/claude")
    from lmm import lmm_ml, dummies
    df, truth, lat = simulate(seed=5, n_id=150); X, P, g = model_designs(df); y = df.trait.values
    worst = 0
    for m in MODELS:
        a = ri_lmm(y, X[m], g); b = lmm_ml(y, X[m], [dummies(df.id)])
        worst = max(worst, abs(a["aic"] - b["aic"]), np.max(np.abs(a["beta"] - b["beta"])))
    return worst
