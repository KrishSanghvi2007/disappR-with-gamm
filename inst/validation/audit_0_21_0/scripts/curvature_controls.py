"""Controls for a random slope on every age term.
  negative control : no selective disappearance, individuals differ in curvature  -> interaction must NOT win
  null-2           : no selective disappearance, no curvature heterogeneity        -> extra terms must not help
  positive control : real age-dependent selective disappearance (rho(LS, b2) != 0) -> interaction must still win
"""
import numpy as np
from scipy.optimize import minimize
rng = np.random.default_rng(1)

def lmm(X, y, Z):
    q = [z.shape[1] for z in Z]; Zc = np.hstack(Z)
    ZtZ, ZtX, Zty = Zc.T @ Zc, Zc.T @ X, Zc.T @ y
    XtX, Xty, yty, N, p = X.T @ X, X.T @ y, float(y @ y), len(y), X.shape[1]
    def nll(par):
        d = np.concatenate([np.repeat(t, qi) for t, qi in zip(np.exp(np.clip(par, -12, 12)), q)])
        try: L = np.linalg.cholesky(np.diag(1.0 / d) + ZtZ)
        except np.linalg.LinAlgError: return 1e12
        sol = lambda B: np.linalg.solve(L.T, np.linalg.solve(L, B))
        XVX = XtX - ZtX.T @ sol(ZtX); XVy = Xty - ZtX.T @ sol(Zty); yVy = yty - Zty @ sol(Zty)
        beta = np.linalg.solve(XVX, XVy)
        rss = yVy - 2 * beta @ XVy + beta @ XVX @ beta
        if rss <= 0: return 1e12
        return 0.5 * (N * np.log(2 * np.pi * rss / N) + 2 * np.sum(np.log(np.diag(L))) + float(np.sum(np.log(d))) + N)
    r = minimize(nll, np.full(len(Z), -1.0), method="Nelder-Mead", options=dict(maxiter=400, fatol=1e-3, xatol=1e-3))
    return 2 * r.fun + 2 * (p + 1 + len(Z))

def sim(n=180, mu_ls=16, sd_ls=4, curv_var=True, rho2=0.0, seed=0):
    r = np.random.default_rng(seed)
    mb = np.array([600.0, 9.0, -0.3]); sb = np.abs(mb) * 0.2
    if not curv_var: sb[2] = 1e-8
    z = r.normal(size=n)                                   # standardised lifespan
    ls = np.clip(np.round(mu_ls + sd_ls * z), 3, None)
    e = r.normal(size=(n, 3))
    b = np.column_stack([mb[0] + sb[0] * e[:, 0], mb[1] + sb[1] * e[:, 1],
                         mb[2] + sb[2] * (rho2 * z + np.sqrt(max(1 - rho2 ** 2, 0)) * e[:, 2])])
    afr = r.integers(1, 5, n)
    ids, age, y = [], [], []
    for i in range(n):
        a = np.arange(afr[i], ls[i] + 1)
        if len(a) < 3: continue
        ids += [i] * len(a); age += list(a)
        y += list(b[i, 0] + b[i, 1] * a + b[i, 2] * a ** 2 + r.normal(0, 5, len(a)))
    return np.array(ids), np.array(age, float), np.array(y)

def design(age, ids, model):
    z = (age - age.mean()) / age.std(); f = np.column_stack([z, z ** 2])
    alr = np.array([age[ids == i].max() for i in ids]); alr = (alr - alr.mean()) / alr.std()
    one = np.ones((len(age), 1))
    return {"M1": np.column_stack([one, f]), "M2": np.column_stack([one, f, alr]),
            "M4": np.column_stack([one, f, alr, f * alr[:, None]])}[model]

def Z(ids, age, k):
    lev, inv = np.unique(ids, return_inverse=True)
    Z0 = np.zeros((len(ids), len(lev))); Z0[np.arange(len(ids)), inv] = 1.0
    z = (age - age.mean()) / age.std()
    return [Z0] + ([Z0 * z[:, None]] if k >= 2 else []) + ([Z0 * (z ** 2)[:, None]] if k >= 3 else [])

R = 6
for label, kw in [("negative control: no selection, curvature varies", dict(curv_var=True, rho2=0.0)),
                  ("null-2: no selection, no curvature heterogeneity", dict(curv_var=False, rho2=0.0)),
                  ("positive control: age-dependent selection via curvature", dict(curv_var=True, rho2=0.6))]:
    win = {1: {}, 2: {}, 3: {}}; gap = {1: [], 2: [], 3: []}
    for s in range(R):
        ids, age, y = sim(seed=200 + s, **kw)
        for k in (1, 2, 3):
            Zs = Z(ids, age, k)
            a = {m: lmm(design(age, ids, m), y, Zs) for m in ("M1", "M2", "M4")}
            b = min(a, key=a.get); win[k][b] = win[k].get(b, 0) + 1
            gap[k].append(a["M1"] - a["M4"])
    print(f"\n{label} ({R} replicates)")
    for k, nm in [(1, "(1|id)"), (2, "(1+age|id)"), (3, "(1+age+age^2|id)")]:
        w = win[k]
        print(f"  {nm:20s} wins: " + " ".join(f"{m} {w.get(m,0):2d}" for m in ("M1", "M2", "M4")) +
              f" | mean AIC(M1)-AIC(M4) = {np.mean(gap[k]):+6.1f}")
