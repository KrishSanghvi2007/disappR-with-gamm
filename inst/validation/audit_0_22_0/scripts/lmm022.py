"""Small maximum-likelihood mixed models used by the audit: a random-intercept linear mixed model (profiled over the
variance ratio, returning coefficients, their covariance and the predicted random effects) and the same model with a
residual variance that changes log-linearly with a covariate (the 0.22.0 'Changes with age' option)."""
import numpy as np
from scipy.optimize import minimize_scalar, minimize

def _groups(g):
    o = np.argsort(g, kind="stable"); gs = g[o]
    idx = np.r_[0, np.where(gs[1:] != gs[:-1])[0] + 1]
    return o, idx, gs[idx]

def lmm_ri(X, y, g):
    o, idx, labels = _groups(np.asarray(g))
    X = X[o]; y = y[o]
    n_i = np.diff(np.r_[idx, len(y)]).astype(float)
    Sx = np.add.reduceat(X, idx, axis=0); Sy = np.add.reduceat(y, idx)
    XtX, Xty, yty, N, p = X.T @ X, X.T @ y, float(y @ y), len(y), X.shape[1]
    def parts(lt):
        th = np.exp(lt); c = th / (1 + n_i * th)
        A = XtX - Sx.T @ (c[:, None] * Sx); b = Xty - Sx.T @ (c * Sy)
        beta = np.linalg.solve(A, b); Sr = Sy - Sx @ beta
        rss = yty - 2 * beta @ Xty + beta @ XtX @ beta - float(np.sum(c * Sr ** 2))
        return th, c, A, beta, Sr, rss
    def nll(lt):
        try: th, c, A, beta, Sr, rss = parts(lt)
        except np.linalg.LinAlgError: return 1e12
        if rss <= 0: return 1e12
        return 0.5 * (N * np.log(2 * np.pi * rss / N) + float(np.sum(np.log1p(n_i * th))) + N)
    r = minimize_scalar(nll, bounds=(-12, 8), method="bounded", options=dict(xatol=1e-6))
    th, c, A, beta, Sr, rss = parts(r.x)
    s2 = rss / N
    return dict(loglik=-r.fun, aic=2 * r.fun + 2 * (p + 2), beta=beta, vcov=s2 * np.linalg.inv(A), sigma2=s2,
                sigma_u2=th * s2, blup=dict(zip(labels, c * Sr)), n=N)

def lmm_ri_hetero(X, y, g, z):
    """Random intercept, residual variance exp(d0 + d1*z). Returns loglik and AIC (p + 3 variance parameters)."""
    o, idx, labels = _groups(np.asarray(g))
    X = X[o]; y = y[o]; z = z[o]; N, p = X.shape
    def nll(par):
        lsu, d0, d1 = par
        su2 = np.exp(2 * lsu); dv = np.exp(d0 + d1 * z); w = 1 / dv
        Sw = np.add.reduceat(w, idx); cc = su2 / (1 + su2 * Sw)
        Xw = X * w[:, None]; SXw = np.add.reduceat(Xw, idx, axis=0); Syw = np.add.reduceat(y * w, idx)
        A = X.T @ Xw - SXw.T @ (cc[:, None] * SXw); b = Xw.T @ y - SXw.T @ (cc * Syw)
        try: beta = np.linalg.solve(A, b)
        except np.linalg.LinAlgError: return 1e12
        r = y - X @ beta; Srw = np.add.reduceat(r * w, idx)
        quad = float(np.sum(r * r * w) - np.sum(cc * Srw ** 2))
        logdet = float(np.sum(np.log(dv)) + np.sum(np.log1p(su2 * Sw)))
        return 0.5 * (N * np.log(2 * np.pi) + logdet + quad)
    best = None
    for start in ([0.0, 0.0, 0.0], [np.log(np.std(y)) - 0.5, 2 * np.log(np.std(y)), 0.0]):
        r = minimize(nll, start, method="Nelder-Mead", options=dict(maxiter=4000, xatol=1e-6, fatol=1e-8))
        if best is None or r.fun < best.fun: best = r
    return dict(loglik=-best.fun, aic=2 * best.fun + 2 * (p + 3), par=best.x)

def lmm_ri_const_full(X, y, g, z):
    """The constant-variance model fitted with the same code path as the heterogeneous one (d1 fixed at 0), as a
    cross-check that the two likelihoods are on the same scale."""
    o, idx, labels = _groups(np.asarray(g))
    X = X[o]; y = y[o]; z = z[o]; N, p = X.shape
    def nll(par):
        lsu, d0 = par
        su2 = np.exp(2 * lsu); dv = np.exp(d0 + 0 * z); w = 1 / dv
        Sw = np.add.reduceat(w, idx); cc = su2 / (1 + su2 * Sw)
        Xw = X * w[:, None]; SXw = np.add.reduceat(Xw, idx, axis=0); Syw = np.add.reduceat(y * w, idx)
        A = X.T @ Xw - SXw.T @ (cc[:, None] * SXw); b = Xw.T @ y - SXw.T @ (cc * Syw)
        beta = np.linalg.solve(A, b)
        r = y - X @ beta; Srw = np.add.reduceat(r * w, idx)
        quad = float(np.sum(r * r * w) - np.sum(cc * Srw ** 2))
        logdet = float(np.sum(np.log(dv)) + np.sum(np.log1p(su2 * Sw)))
        return 0.5 * (N * np.log(2 * np.pi) + logdet + quad)
    r = minimize(nll, [0.0, 2 * np.log(np.std(y))], method="Nelder-Mead", options=dict(maxiter=4000, xatol=1e-7, fatol=1e-9))
    return dict(loglik=-r.fun)
