"""Numerical checks of three statistical choices in disappR 0.20.x, in Python (does not execute the R package).

1. Step 4 compares ageing functions across individual count fits by QAICc when the models use an overdispersed or
   zero-inflated family: Poisson fits, dispersion c-hat estimated once from the cubic fits (pooled Pearson
   chi-square / pooled residual df), QAICc = -2 logL / c-hat + 2K + 2K(K+1)/(n-K-1) with K = k + 1. Does it pick the
   true function more often than the plain Poisson AICc, and does it leave Poisson data alone?
2. The direction of the finding: the sign of the first age coefficient against the slope of the predictions, for
   every ageing basis as R/data-preparation.R builds it.
3. Standardising age changes no AIC with a random intercept or correlated random slopes, but does with uncorrelated
   random slopes.
Writes stats_checks.md next to this file.     Usage: python3 stats_checks.py [datasets_per_case]
"""
import os, sys, time, warnings
import numpy as np
from scipy.special import gammaln
warnings.filterwarnings("ignore")
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "..", "extended"))
from harness import simulate   # noqa: E402
from emu import LMM            # noqa: E402
NDS = int(sys.argv[1]) if len(sys.argv) > 1 else 100
out = []

# ---------------------------------------------------------------- 1. QAICc in step 4
# Three criteria for ranking the functions across individual count fits:
#   Poisson AICc - the Poisson likelihood as it stands (step 4 up to 0.19.6);
#   QAICc        - disappR 0.20.x for overdispersed or zero-inflated families (c-hat from the cubic fits);
#   oracle AICc  - the correct likelihood with the true dispersion or zero-inflation held fixed (not available to the
#                  app; the benchmark for what an honest criterion should choose with these data).
from scipy.optimize import minimize

def irls(X, y, var_w, maxit=100):
    if np.linalg.matrix_rank(X) < X.shape[1]:
        return None
    mu = y + 0.5
    eta = np.log(mu)
    beta = np.zeros(X.shape[1])
    for _ in range(maxit):
        z = eta + (y - mu) / mu
        W = var_w(mu)
        XtW = X.T * W
        try:
            new = np.linalg.solve(XtW @ X, XtW @ z)
        except np.linalg.LinAlgError:
            return None
        eta = X @ new
        if not np.all(np.isfinite(eta)) or np.max(np.abs(eta)) > 50:
            return None
        mu = np.exp(eta)
        if np.max(np.abs(new - beta)) < 1e-10 * (1 + np.max(np.abs(new))):
            return new, mu
        beta = new
    return None

def zip_fit(X, y, pz, start):
    def nll(b):
        eta = np.clip(X @ b, -50, 50); mu = np.exp(eta)
        l0 = np.log(pz + (1 - pz) * np.exp(-mu)); l1 = np.log1p(-pz) + y * eta - mu - gammaln(y + 1)
        return -np.sum(np.where(y == 0, l0, l1))
    def grad(b):
        eta = np.clip(X @ b, -50, 50); mu = np.exp(eta)
        g0 = -(1 - pz) * np.exp(-mu) * mu / (pz + (1 - pz) * np.exp(-mu))
        return -X.T @ np.where(y == 0, g0, y - mu)
    o = minimize(nll, start, jac=grad, method="BFGS")
    return (-o.fun if o.success or o.status == 2 else None)

FUNS = {"Linear": 1, "Quadratic": 2, "Cubic": 3}
def stats_one(age, y, fun, kind, nuis):
    k = FUNS[fun] + 1
    n = len(y)
    if n < max(k, {1: 2, 2: 3, 3: 4}[FUNS[fun]]) or len(np.unique(age)) < k:
        return None
    X = np.column_stack([((age - 6.0) / 3.0) ** p for p in range(k)])   # a linear reparameterisation of raw ages
    y = y.astype(float)
    f = irls(X, y, lambda mu: mu)
    if f is None:
        return None
    beta, mu = f
    ll = np.sum(y * np.log(np.maximum(mu, 1e-300)) - mu - gammaln(y + 1))
    pen = lambda kk: 2 * kk + 2 * kk * (kk + 1) / (n - kk - 1) if n - kk - 1 > 0 else np.nan
    if kind.startswith("negative"):
        th = nuis
        g = irls(X, y, lambda m: m / (1 + m / th))
        oll = None if g is None else np.sum(gammaln(y + th) - gammaln(th) - gammaln(y + 1) + th * np.log(th / (th + g[1])) + y * np.log(g[1] / (th + g[1])))
    elif kind.startswith("zero"):
        oll = zip_fit(X, y, nuis, beta)
    else:
        oll = ll
    return dict(n=n, k=k, ll=ll, aicc=-2 * ll + pen(k), oracle=(-2 * oll + pen(k)) if oll is not None else np.nan,
                pearson=np.sum((y - mu) ** 2 / np.maximum(mu, 1e-12)), df=n - k)

def winner(per, which):
    chat = np.nan
    if which == "QAICc":
        cub = [s for s in per["Cubic"].values() if s["df"] > 0]
        chat = max(1.0, sum(s["pearson"] for s in cub) / sum(s["df"] for s in cub)) if cub else np.nan
    crit = {}
    for f, d in per.items():
        crit[f] = {}
        for i, s in d.items():
            if which == "QAICc" and chat > 1:
                kq = s["k"] + 1
                crit[f][i] = -2 * s["ll"] / chat + 2 * kq + 2 * kq * (kq + 1) / (s["n"] - kq - 1) if s["n"] - kq - 1 > 0 else np.nan
            else:
                crit[f][i] = s["aicc"] if which in ("AICc", "QAICc") else s["oracle"]
    common = set.intersection(*[{i for i, v in crit[f].items() if np.isfinite(v)} for f in crit])
    means = {f: np.mean([crit[f][i] - min(crit[g][i] for g in crit) for i in common]) for f in crit}
    return min(means, key=means.get), chat

DESIGNS = {"A: 5-12 records, mild curvature": (6, 14, 0.35), "B: 10-16 records, strong curvature": (11, 18, 0.8)}
KINDS = {"Poisson": None, "negative binomial (theta = 2)": 2.0, "zero-inflated Poisson (30% zeros)": 0.3}
def sim_counts(rng, kind, lo, hi, curv, n_id=80):
    data = []
    for i in range(n_id):
        age = np.arange(1, rng.integers(lo, hi))
        z = (age - 6.0) / 3.0
        mu = np.exp(np.log(8) + rng.normal(0, 0.3) + 0.25 * z - curv * z ** 2)   # quadratic on the log scale
        if kind == "Poisson":
            y = rng.poisson(mu)
        elif kind.startswith("negative"):
            y = rng.negative_binomial(2, 2 / (2 + mu))
        else:
            y = rng.poisson(mu) * rng.binomial(1, 0.7, len(mu))
        data.append((age, y))
    return data

t0 = time.time()
rows = []
for dname, (lo, hi, curv) in DESIGNS.items():
    for kind, nuis in KINDS.items():
        wins = {c: {f: 0 for f in FUNS} for c in ("AICc", "QAICc", "oracle")}
        chats = []
        for ds in range(NDS):
            rng = np.random.default_rng(90000 + ds)
            per = {f: {} for f in FUNS}
            for i, (age, y) in enumerate(sim_counts(rng, kind, lo, hi, curv)):
                for f in FUNS:
                    st = stats_one(age, y, f, kind, nuis)
                    if st is not None:
                        per[f][i] = st
            for c in wins:
                w, ch = winner(per, c)
                wins[c][w] += 1
                if c == "QAICc": chats.append(ch)
        for c in ("oracle", "AICc", "QAICc"):
            rows.append((dname, kind, {"oracle": "oracle (true likelihood)", "AICc": "Poisson AICc (0.19.6)", "QAICc": "QAICc (0.20.x)"}[c],
                         *(wins[c][f] / NDS for f in FUNS), np.nanmean(chats) if c == "QAICc" else np.nan))
out.append("## 1. Step 4: which ageing function wins - the true likelihood, Poisson AICc and QAICc\n")
out.append(f"{NDS} simulated datasets per row, 80 individuals each; the true function is quadratic on the log scale. "
           "Share of datasets in which each function has the lowest mean difference across individuals (the ranking "
           "of the step-4 comparison table). 'Oracle' uses the correct likelihood with the true dispersion or zero-"
           "inflation: the benchmark a criterion should match.\n")
out.append("| Design | Counts | Criterion | Linear | Quadratic (true) | Cubic | mean c-hat |\n|---|---|---|---|---|---|---|")
for d, k, c, l, q, cu, ch in rows:
    out.append(f"| {d} | {k} | {c} | {l:.2f} | {q:.2f} | {cu:.2f} | {'' if not np.isfinite(ch) else f'{ch:.2f}'} |")
print(f"QAICc check done ({time.time() - t0:.0f} s)", flush=True)

# ---------------------------------------------------------------- 2. direction of the finding, per ageing basis
def make_params(age):
    p = dict(mean_age=age.mean(), sd_age=age.std(ddof=1), min_age=age.min(), all_pos=age.min() > 0)
    lx = np.log(age) if p["all_pos"] else np.log(age - p["min_age"] + 1)
    p.update(mean_log=lx.mean(), sd_log=lx.std(ddof=1))
    ex = np.exp(-(age - p["mean_age"]) / p["sd_age"])
    p.update(mean_exp=ex.mean(), sd_exp=ex.std(ddof=1))
    return p

def basis(age, p, fun):          # R/data-preparation.R age_basis(), standardise = TRUE
    z = (age - p["mean_age"]) / p["sd_age"]
    if fun in ("Linear", "Quadratic", "Cubic"):
        return np.column_stack([z ** j for j in range(1, {"Linear": 2, "Quadratic": 3, "Cubic": 4}[fun])])
    if fun == "Logarithmic":
        lx = np.log(np.maximum(age, 1e-12)) if p["all_pos"] else np.log(np.maximum(age - p["min_age"] + 1, 1e-12))
        return ((lx - p["mean_log"]) / p["sd_log"])[:, None]
    return ((np.exp(-z) - p["mean_exp"]) / p["sd_exp"])[:, None]

rng = np.random.default_rng(4)
age = rng.uniform(1, 12, 3000)
out.append("\n## 2. Direction of ageing: first coefficient's sign against the predictions' slope\n")
out.append("Trait = 10 -/+ 3 (1 - exp(-age / 3)) + noise, 3000 records, ages 1-12; ordinary least squares with each basis. "
           "The predictions' slope is taken between the 25th and 75th age percentiles (as the 0.20.2 evidence summary does).\n")
out.append("| Basis | Truth | Sign of f1 coefficient | Sign of predicted slope | Coefficient rule right? | Prediction rule right? |\n|---|---|---|---|---|---|")
wrong_coef = []
for fun in ("Linear", "Quadratic", "Cubic", "Logarithmic", "Asymptotic exponential"):
    for truth, sgn in (("declining", -1), ("increasing", 1)):
        y = 10 + sgn * 3 * (1 - np.exp(-age / 3)) + rng.normal(0, 0.3, len(age))
        p = make_params(age)
        X = np.column_stack([np.ones_like(age), basis(age, p, fun)])
        b = np.linalg.lstsq(X, y, rcond=None)[0]
        q = np.quantile(age, [0.25, 0.75])
        Xq = np.column_stack([np.ones(2), basis(q, p, fun)])
        slope = (Xq @ b)[1] - (Xq @ b)[0]
        c_ok, p_ok = np.sign(b[1]) == sgn, np.sign(slope) == sgn
        if not c_ok: wrong_coef.append(fun)
        out.append(f"| {fun} | {truth} | {'+' if b[1] > 0 else '-'} | {'+' if slope > 0 else '-'} | {'yes' if c_ok else '**no**'} | {'yes' if p_ok else '**no**'} |")
print("direction check done", flush=True)

# ---------------------------------------------------------------- 3. standardisation and AIC
res = {"none": [], "correlated": [], "uncorrelated": []}
for s in range(20):
    df = simulate(n=150, mean_ls=10, seed=500 + s, fun="linear", sd="none", slope_sd=0.12)
    y = df.trait.values.astype(float); a = df.age.values.astype(float); ids = df.id.values
    z = (a - a.mean()) / a.std(ddof=1)
    for st in res:
        raw = LMM(y, np.column_stack([np.ones_like(a), a]), ids, a, st).fit().aic
        std = LMM(y, np.column_stack([np.ones_like(z), z]), ids, z, st).fit().aic
        res[st].append(abs(raw - std))
out.append("\n## 3. Does standardising age change the AIC?\n")
out.append("20 simulated datasets (150 individuals, individual differences in ageing rate), Model 1 with linear ageing, "
           "maximum likelihood; |AIC(raw age) - AIC(standardised age)|.\n")
out.append("| Random effects | median | max |\n|---|---|---|")
for st, v in res.items():
    out.append(f"| {st} | {np.median(v):.2g} | {np.max(v):.2g} |")
with open(os.path.join(HERE, "stats_checks.md"), "w") as fh:
    fh.write("# Numerical checks for disappR 0.20.2 (Python; not the R package)\n\n" + "\n".join(out) + "\n")
print("\n".join(out))
print(f"coefficient rule wrong for: {sorted(set(wrong_coef))}; total {time.time() - t0:.0f} s")
