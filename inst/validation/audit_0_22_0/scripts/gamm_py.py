"""A small GAMM fitter for the audit (0.23.1): penalised cubic B-splines (second-order difference penalty) written as
mixed models (Wood 2017, section 5.8; Ruppert, Wand & Carroll 2003), with an individual random intercept, fitted by
maximum likelihood. The smooth's null space (intercept and linear age) is fixed; its penalised part is a random
effect with its own variance. A numeric 'by' variable gives a varying-coefficient smooth, s(age, by = ALR), used here
for age-dependent selective disappearance. The individual random intercept is absorbed analytically, so a fit costs
little more than a least-squares solve. ML log-likelihoods are marginal (smooths integrated out), as in gamm4, so
AICs are comparable with those of the linear mixed models fitted by lmm022.lmm_ri."""
import numpy as np
from scipy.interpolate import BSpline
from scipy import optimize


class Smooth:
    def __init__(self, x, nseg=8, deg=3):
        self.xl, self.xr = float(np.min(x)), float(np.max(x))
        pad = 1e-6 * (self.xr - self.xl)
        self.xl -= pad; self.xr += pad
        dx = (self.xr - self.xl) / nseg
        self.t = self.xl + dx * np.arange(-deg, nseg + deg + 1)
        self.deg = deg
        self.n = len(self.t) - deg - 1
        Dm = np.diff(np.eye(self.n), 2, axis=0)
        w, U = np.linalg.eigh(Dm.T @ Dm)
        pos = w > 1e-8
        self.U, self.w = U[:, pos], w[pos]
        self.xm, self.xs = float(np.mean(x)), float(np.std(x))

    def basis(self, x):
        x = np.clip(np.asarray(x, float), self.xl, self.xr)
        B = BSpline.design_matrix(x, self.t, self.deg).toarray()
        return B

    def fixed(self, x):          # null space of the penalty: linear in (standardised) x
        return ((np.asarray(x, float) - self.xm) / self.xs)[:, None]

    def random(self, x):         # penalised part, iid coefficients
        return self.basis(x) @ self.U / np.sqrt(self.w)


def fit(Xf, blocks, y, g, start=None):
    """Xf: fixed-effect matrix (with intercept); blocks: list of penalised design blocks (each an iid random effect
    with its own variance); g: individual labels (random intercept). ML by profiling the residual variance and
    optimising the log variance ratios. Returns beta (fixed and penalised coefficients), sigma2, loglik, aic."""
    y = np.asarray(y, float)
    n = len(y)
    W = np.column_stack([Xf] + blocks) if blocks else Xf
    p = Xf.shape[1]
    qs = [b.shape[1] for b in blocks]
    m = W.shape[1]
    _, gi = np.unique(g, return_inverse=True)
    nid = gi.max() + 1
    cnt = np.bincount(gi, minlength=nid).astype(float)
    Wsum = np.zeros((nid, m)); np.add.at(Wsum, gi, W)
    ysum = np.bincount(gi, weights=y, minlength=nid)
    WtW = W.T @ W; Wty = W.T @ y; yty = y @ y

    def parts(theta):
        tb = np.exp(theta[0]); ts = np.exp(theta[1:]) if len(theta) > 1 else np.array([])
        D = cnt + 1.0 / tb
        P = np.zeros(m)
        pos = p
        for q, t in zip(qs, ts):
            P[pos:pos + q] = 1.0 / t
            pos += q
        A = WtW + np.diag(P) - (Wsum.T / D) @ Wsum
        c = Wty - Wsum.T @ (ysum / D)
        try:
            L = np.linalg.cholesky(A)
        except np.linalg.LinAlgError:
            return None
        th = np.linalg.solve(A, c)
        r = yty - np.sum(ysum ** 2 / D) - c @ th
        if r <= 0:
            return None
        s2 = r / n
        ld_G = nid * np.log(tb) + sum(q * np.log(t) for q, t in zip(qs, ts))
        ld_R = np.sum(np.log(D)) + (2 * np.sum(np.log(np.diag(L[p:, p:]))) if m > p else 0.0)
        # log|A_rr| uses the random block after absorbing the fixed part is not needed: marginal ML uses the random
        # block of the mixed-model matrix before profiling the fixed effects, computed below exactly
        if m > p:
            Arr = A[p:, p:]
            sign, ld_rr = np.linalg.slogdet(Arr)
            ld_R = np.sum(np.log(D)) + ld_rr
        m2ll = n * np.log(2 * np.pi * s2) + n + ld_G + ld_R
        return m2ll, th, s2, A

    k = 1 + len(blocks)
    x0 = np.zeros(k) if start is None else np.asarray(start, float)
    best = None
    for s0 in ([x0] + [np.full(k, v) for v in (-4.0, 3.0)]):
        res = optimize.minimize(lambda t: (parts(t) or (1e300,))[0], s0, method="L-BFGS-B", bounds=[(-20, 15)] * k)
        if best is None or res.fun < best.fun:
            best = res
    m2ll, th, s2, A = parts(best.x)
    npar = p + k + 1
    return dict(beta=th, sigma2=s2, loglik=-m2ll / 2, aic=m2ll + 2 * npar, theta=best.x, A=A, p=p, qs=qs, npar=npar)


def predict(f, Xf_new, blocks_new):
    W = np.column_stack([Xf_new] + blocks_new) if blocks_new else Xf_new
    return W @ f["beta"]
