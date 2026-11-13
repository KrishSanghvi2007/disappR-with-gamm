"""Binomial-trait checks for disappR, with the fits written directly (IRLS): weights (number of trials),
two records at one age, selective-disappearance recovery, and overdispersion."""
import numpy as np, warnings
warnings.filterwarnings("ignore")

def glm_binom(X, y, k):
    """Binomial GLM by IRLS. y = successes (or a proportion when k = 1), k = trials."""
    beta = np.zeros(X.shape[1]); p = y / k
    for _ in range(60):
        eta = X @ beta
        mu = 1 / (1 + np.exp(-eta))
        v = np.maximum(mu * (1 - mu), 1e-10)
        W = k * v
        z = eta + (p - mu) / v
        beta_new = np.linalg.solve(X.T @ (X * W[:, None]), X.T @ (W * z))
        if np.max(np.abs(beta_new - beta)) < 1e-11:
            beta = beta_new; break
        beta = beta_new
    eta = X @ beta; mu = 1 / (1 + np.exp(-eta)); v = np.maximum(mu * (1 - mu), 1e-10); W = k * v
    cov = np.linalg.inv(X.T @ (X * W[:, None]))
    pearson = np.sum(k * (p - mu) ** 2 / v) / (len(y) - X.shape[1])
    return beta, np.sqrt(np.diag(cov)), pearson, mu, cov

def cluster_se(X, y, k, beta, cov, groups):
    mu = 1 / (1 + np.exp(-(X @ beta)))
    u = X * (k * (y / k - mu))[:, None]
    B = np.zeros((X.shape[1], X.shape[1]))
    for g in np.unique(groups):
        s = u[groups == g].sum(0)
        B += np.outer(s, s)
    return np.sqrt(np.diag(cov @ B @ cov))

out = []
# ---- 1. weights: the number of trials ----
res = []
for seed in range(80):
    r = np.random.default_rng(seed)
    n = 400
    x = r.normal(size=n); k = r.integers(1, 21, n).astype(float)
    p = 1 / (1 + np.exp(-(-0.5 + 0.35 * x)))
    y = r.binomial(k.astype(int), p).astype(float)
    X = np.column_stack([np.ones(n), x])
    bw, sew, _, _, _ = glm_binom(X, y, k)
    bn, sen, _, _, _ = glm_binom(X, y / k, np.ones(n))
    res.append((bw[1], sew[1], bn[1], sen[1]))
res = np.array(res)
out.append(("weights: slope with trials", f"mean {res[:,0].mean():+.3f} (true +0.350), SD {res[:,0].std():.3f}, mean SE {res[:,1].mean():.3f}"))
out.append(("weights: slope without trials", f"mean {res[:,2].mean():+.3f}, SD {res[:,2].std():.3f}, mean SE {res[:,3].mean():.3f}"))
out.append(("weights: 95% CI coverage", f"with trials {np.mean(np.abs(res[:,0]-0.35) < 1.96*res[:,1]):.2f}; without {np.mean(np.abs(res[:,2]-0.35) < 1.96*res[:,3]):.2f}"))
r = np.random.default_rng(99); n = 400
x = r.normal(size=n); k = np.full(n, 8.0); y = r.binomial(8, 1/(1+np.exp(-(-0.5+0.35*x)))).astype(float)
X = np.column_stack([np.ones(n), x])
a = glm_binom(X, y, k)[0][1]; b = glm_binom(X, y / k, np.ones(n))[0][1]
out.append(("weights: equal trials, same estimate", f"with {a:+.4f} vs without {b:+.4f} (difference {abs(a-b):.4f})"))

# ---- 2. two records at the same individual and age ----
keep, pool, avg = [], [], []
for seed in range(80):
    r = np.random.default_rng(1000 + seed); n = 300
    x = r.normal(size=n); p = 1 / (1 + np.exp(-(-0.5 + 0.35 * x)))
    k1 = r.integers(1, 4, n).astype(float); k2 = r.integers(10, 26, n).astype(float)
    y1 = r.binomial(k1.astype(int), p).astype(float); y2 = r.binomial(k2.astype(int), p).astype(float)
    X = np.column_stack([np.ones(n), x]); X2 = np.column_stack([np.ones(2 * n), np.r_[x, x]])
    keep.append(glm_binom(X2, np.r_[y1, y2], np.r_[k1, k2])[0][1])
    pool.append(glm_binom(X, y1 + y2, k1 + k2)[0][1])
    ak = np.round((k1 + k2) / 2); ap = (y1 / k1 + y2 / k2) / 2
    avg.append(glm_binom(X, np.round(ap * ak), ak)[0][1])
out.append(("two records at one age: keep both", f"mean slope {np.mean(keep):+.3f} (true +0.350), SD {np.std(keep):.3f}"))
out.append(("two records at one age: pool successes and trials", f"mean slope {np.mean(pool):+.3f}, SD {np.std(pool):.3f}"))
out.append(("two records at one age: average the proportions", f"mean slope {np.mean(avg):+.3f} (bias {np.mean(avg)-0.35:+.3f}, SD {np.std(avg):.3f})"))

# ---- 3. selective disappearance in a binomial trait ----
def sim_sd(kind, n=400, beta_age=-0.30, seed=0):
    r = np.random.default_rng(seed)
    ls = np.clip(np.round(3 + r.gamma(4, 1.2, n)), 2, 18).astype(int)
    z = (ls - ls.mean()) / ls.std()
    a0 = 0.6 * z + r.normal(0, 0.4, n)
    slope = beta_age + (0.18 * z if kind == "age_dependent" else np.zeros(n))
    id_, age_, y_, k_, alr_ = [], [], [], [], []
    for i in range(n):
        for age in range(1, ls[i] + 1):
            k = r.integers(2, 13)
            eta = a0[i] + slope[i] * (age - 1) / 5
            y_.append(r.binomial(k, 1 / (1 + np.exp(-eta)))); k_.append(k); id_.append(i); age_.append(age); alr_.append(ls[i])
    id_, age_, y_, k_, alr_ = map(np.asarray, (id_, age_, y_, k_, alr_))
    ac = (age_ - age_.mean()) / 5.0
    az = (alr_ - alr_.mean()) / alr_.std()
    return id_, ac, y_.astype(float), k_.astype(float), az
for kind in ["none", "age_dependent"]:
    naive, adj, inter, se_i, m4_age = [], [], [], [], []
    for s in range(20):
        g, ac, y, k, az = sim_sd(kind, seed=s)
        X1 = np.column_stack([np.ones(len(y)), ac])
        X2 = np.column_stack([np.ones(len(y)), ac, az])
        X4 = np.column_stack([np.ones(len(y)), ac, az, ac * az])
        naive.append(glm_binom(X1, y, k)[0][1])
        adj.append(glm_binom(X2, y, k)[0][1])
        b4, _, _, _, cov4 = glm_binom(X4, y, k)
        inter.append(b4[3]); se_i.append(cluster_se(X4, y, k, b4, cov4, g)[3]); m4_age.append(b4[1])
    hits = np.mean(np.abs(np.array(inter)) > 1.96 * np.array(se_i))
    out.append((f"selective disappearance ({kind}): within-individual age slope",
                f"naive {np.mean(naive):+.3f}, additive ALR {np.mean(adj):+.3f}, ALR interaction {np.mean(m4_age):+.3f} (true -0.300)"))
    out.append((f"selective disappearance ({kind}): ALR x age term",
                f"mean {np.mean(inter):+.3f} (true {'+0.180' if kind == 'age_dependent' else '0.000'}), detected in {hits:.0%} of runs"))

# ---- 4. overdispersion ----
r = np.random.default_rng(3); n = 600
x = r.normal(size=n); k = r.integers(5, 20, n).astype(float)
p = 1 / (1 + np.exp(-(-0.3 + 0.4 * x)))
rho = 0.25; a = p * (1 / rho - 1); b = (1 - p) * (1 / rho - 1)
y = r.binomial(k.astype(int), r.beta(a, b)).astype(float)
X = np.column_stack([np.ones(n), x])
_, se, pearson, _, _ = glm_binom(X, y, k)
_, se2, pearson2, _, _ = glm_binom(X, r.binomial(k.astype(int), p).astype(float), k)
out.append(("overdispersion: beta-binomial data read as binomial", f"Pearson chi2/df = {pearson:.2f}; clean binomial data = {pearson2:.2f}"))
w = max(len(a) for a, _ in out)
for a, b in out: print(f"{a:<{w}}  {b}")
