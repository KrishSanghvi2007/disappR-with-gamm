
import re, numpy as np, pandas as pd, warnings
from scipy.optimize import minimize_scalar
warnings.filterwarnings("ignore")
ROOT = "../../../"
s = open(ROOT + "R/import.R", encoding="utf-8").read()
def parse_map(t):
    o = {}
    for m in re.finditer(r'(\w+)\s*=\s*(c\([^)]*\)|"[^"]*")', t):
        v = m.group(2); o[m.group(1)] = re.findall(r'"([^"]*)"', v) if v.startswith("c(") else v.strip('"')
    return o
def block(t, i0):
    i = t.index("(", i0); d = 0
    for j in range(i, len(t)):
        d += (t[j] == "(") - (t[j] == ")")
        if d == 0: return t[i:j]
fly_map = parse_map(block(s, s.index("FLY_MAPPING <- list")))
body = s[s.index("EXAMPLES <- list("):]
keys = [(m.group(1), m.start()) for m in re.finditer(r"\n  ([a-z0-9_]+) = list\(", body)]
EX = {}
for i, (k, pos) in enumerate(keys):
    seg = body[pos: keys[i+1][1] if i+1 < len(keys) else len(body)]
    mp = fly_map if k == "fly" else parse_map(block(seg, re.search(r"mapping = example_map", seg).end() - 1))
    EX[k] = dict(map=mp, file=re.search(r'file = "([^"]+)"', seg).group(1),
                 family=(re.search(r'family = "([^"]+)"', seg) or [None, "gaussian"])[1],
                 fun=(re.search(r'age_function = "([^"]+)"', seg) or [None, "Quadratic"])[1],
                 models=re.sub(r'[" ]', '', (re.search(r'models = c\(([^)]*)\)', seg) or [None, ""])[1]).split(",") if re.search(r'models = c\(', seg) else None,
                 subset=re.search(r'subset = list\(var = "([^"]+)", levels = c\(([^)]*)\)', seg))

def basis(fun, age):
    z = (age - age.mean()) / age.std()
    if fun == "Linear": return np.column_stack([z])
    if fun == "Quadratic": return np.column_stack([z, z ** 2])
    if fun == "Cubic": return np.column_stack([z, z ** 2, z ** 3])
    if fun == "Logarithmic":
        l = np.log(age - age.min() + 1); return np.column_stack([(l - l.mean()) / l.std()])
    if fun == "Asymptotic exponential": return np.column_stack([np.exp(-z)])
    raise ValueError(fun)

def lmm_ml(X, y, gid):
    """ML log-likelihood and AIC for y = Xb + (1|gid) + e, profiled over the variance ratio."""
    o = np.argsort(gid, kind="stable"); X, y, g = X[o], y[o], gid[o]
    idx = np.r_[0, np.where(np.diff(g) != 0)[0] + 1]
    n_i = np.diff(np.r_[idx, len(g)]).astype(float)
    Sx = np.add.reduceat(X, idx, axis=0); Sy = np.add.reduceat(y, idx)
    XtX, Xty, yty, N, p = X.T @ X, X.T @ y, float(y @ y), len(y), X.shape[1]
    def nll(lt):
        th = np.exp(lt); c = th / (1 + n_i * th)
        A = XtX - Sx.T @ (c[:, None] * Sx); b = Xty - Sx.T @ (c * Sy)
        try: beta = np.linalg.solve(A, b)
        except np.linalg.LinAlgError: return 1e12
        Sr = Sy - Sx @ beta
        rss = yty - 2 * beta @ Xty + beta @ XtX @ beta - float(np.sum(c * Sr ** 2))
        if rss <= 0: return 1e12
        s2 = rss / N
        return 0.5 * (N * np.log(2 * np.pi * s2) + float(np.sum(np.log1p(n_i * th))) + N)
    r = minimize_scalar(nll, bounds=(-9, 9), method="bounded", options=dict(xatol=1e-4))
    return -r.fun, 2 * r.fun + 2 * (p + 2)

def design(d, fun, model, covX):
    b = basis(fun, d["age"].values); k = b.shape[1]
    z = lambda v: (v - np.nanmean(v)) / (np.nanstd(v) if np.nanstd(v) > 0 else 1)
    ALR, AFR, LS = z(d["alr"].values), z(d["afr"].values), z(d["life"].values) if "life" in d else None
    mean_b = np.column_stack([pd.Series(b[:, j]).groupby(d["id"].values).transform("mean").values for j in range(k)])
    delta = b - mean_b
    one = np.ones((len(d), 1))
    inter = lambda A, v: np.column_stack([A[:, j] * v for j in range(A.shape[1])])
    if model == "M1": M = b
    elif model == "M2": M = np.column_stack([b, ALR])
    elif model == "M3": M = np.column_stack([mean_b[:, 0], delta])
    elif model == "M4": M = np.column_stack([b, ALR, inter(b, ALR)])
    elif model == "M5": M = np.column_stack([mean_b[:, 0], delta, inter(delta, mean_b[:, 0])])
    elif model == "M6": M = np.column_stack([b, LS, inter(b, LS)])
    elif model == "M7": M = np.column_stack([b, ALR, AFR])
    elif model == "M8": M = np.column_stack([b, ALR, AFR, inter(b, ALR), inter(b, AFR)])
    elif model == "M9": M = np.column_stack([b, ALR, AFR, inter(b, ALR)])
    elif model == "M10": M = np.column_stack([b, ALR, AFR, inter(b, AFR)])
    return np.column_stack([one, M, covX]) if covX is not None and covX.shape[1] else np.column_stack([one, M])

