"""How often does an interaction model win on null data (no selective disappearance), with and without a random slope?"""
import numpy as np
from scipy.optimize import minimize, minimize_scalar
exec(open("null_randomslope.py").read().split("def lmm(")[0])
exec("def sim" + open("null_randomslope.py").read().split("def sim")[1].split("def design")[0])
exec("def design" + open("null_randomslope.py").read().split("def design")[1].split("def Zof")[0])

def lmm_ri(X, y, gid):                                    # random intercept, profiled exactly
    o = np.argsort(gid, kind="stable"); X, y, g = X[o], y[o], gid[o]
    idx = np.r_[0, np.where(np.diff(g) != 0)[0] + 1]; n_i = np.diff(np.r_[idx, len(g)]).astype(float)
    Sx = np.add.reduceat(X, idx, axis=0); Sy = np.add.reduceat(y, idx)
    XtX, Xty, yty, N, p = X.T @ X, X.T @ y, float(y @ y), len(y), X.shape[1]
    def nll(lt):
        th = np.exp(lt); c = th / (1 + n_i * th)
        A = XtX - Sx.T @ (c[:, None] * Sx); b = Xty - Sx.T @ (c * Sy)
        beta = np.linalg.solve(A, b); Sr = Sy - Sx @ beta
        rss = yty - 2 * beta @ Xty + beta @ XtX @ beta - float(np.sum(c * Sr ** 2))
        return 1e12 if rss <= 0 else 0.5 * (N * np.log(2 * np.pi * rss / N) + float(np.sum(np.log1p(n_i * th))) + N)
    r = minimize_scalar(nll, bounds=(-9, 9), method="bounded")
    return 2 * r.fun + 2 * (p + 2)

def lmm_rs(X, y, gid, zage):                              # correlated random intercept and slope
    o = np.argsort(gid, kind="stable"); X, y, z, g = X[o], y[o], zage[o], gid[o]
    idx = np.r_[0, np.where(np.diff(g) != 0)[0] + 1]; ends = np.r_[idx[1:], len(g)]
    Z = np.column_stack([np.ones(len(y)), z])
    A = np.stack([X[a:b].T @ Z[a:b] for a, b in zip(idx, ends)])
    B = np.stack([Z[a:b].T @ Z[a:b] for a, b in zip(idx, ends)])
    Zty = np.stack([Z[a:b].T @ y[a:b] for a, b in zip(idx, ends)])
    XtX, Xty, yty, N, p = X.T @ X, X.T @ y, float(y @ y), len(y), X.shape[1]
    def nll(par):
        s0, s1, rho = np.exp(par[0]), np.exp(par[1]), np.tanh(par[2])
        P = np.array([[s0 ** 2, rho * s0 * s1], [rho * s0 * s1, s1 ** 2]])
        try: M = np.linalg.inv(np.linalg.inv(P + 1e-10 * np.eye(2))[None] + B)
        except np.linalg.LinAlgError: return 1e12
        XVX = XtX - np.einsum("kpi,kij,kqj->pq", A, M, A); XVy = Xty - np.einsum("kpi,kij,kj->p", A, M, Zty)
        yVy = yty - np.einsum("ki,kij,kj->", Zty, M, Zty)
        try: beta = np.linalg.solve(XVX, XVy)
        except np.linalg.LinAlgError: return 1e12
        rss = yVy - 2 * beta @ XVy + beta @ XVX @ beta
        if rss <= 0: return 1e12
        _, ld = np.linalg.slogdet(np.eye(2)[None] + np.einsum("ij,kjl->kil", P, B))
        return 0.5 * (N * np.log(2 * np.pi * rss / N) + float(ld.sum()) + N)
    r = minimize(nll, np.array([-0.5, -1.0, 0.0]), method="Nelder-Mead", options=dict(maxiter=300, fatol=1e-3, xatol=1e-3))
    return 2 * r.fun + 2 * (p + 4)

for label, sv in [("curvature varies too", True), ("level and slope only", False)]:
    wins = {"(1|id)": {"M1": 0, "M2": 0, "M4": 0}, "(1+age|id)": {"M1": 0, "M2": 0, "M4": 0}}
    gaps = {"(1|id)": [], "(1+age|id)": []}
    R = 15
    for s in range(R):
        ids, age, y = sim(n=300, shape_var=sv, seed=100 + s)
        gid = np.unique(ids, return_inverse=True)[1]
        zage = (age - age.mean()) / age.std()
        for name, f in [("(1|id)", lambda X: lmm_ri(X, y, gid)), ("(1+age|id)", lambda X: lmm_rs(X, y, gid, zage))]:
            a = {m: f(design(age, ids, m)) for m in ("M1", "M2", "M4")}
            b = min(a, key=a.get); wins[name][b] += 1
            gaps[name].append(a["M1"] - a["M4"])
    print(f"\nno selective disappearance, {label} ({R} replicates, 300 individuals)")
    for name in wins:
        w = wins[name]
        print(f"  {name:12s} wins: M1 {w['M1']:2d}  M2 {w['M2']:2d}  M4 {w['M4']:2d} | mean AIC(M1) - AIC(M4) = {np.mean(gaps[name]):+.1f}")
