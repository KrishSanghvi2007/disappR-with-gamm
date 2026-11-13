"""Port of disappR's decomposition_trajectory(), run on the manuscript's generating process, and compared with
the analytic solutions of Supplementary S1 (Eq. 9 for the observed deviation, Eq. 15 for the decomposition's bias)."""
import numpy as np
from scipy import stats
rng_global = np.random.default_rng(0)

def decomposition_trajectory(ids, ages, trait):
    """Faithful port: occasions = distinct ages (merged when closer than a quarter step); a link needs the next
    occasion about one step away and at least one individual recorded at both ends; the chain restarts at a break,
    anchored on the survivor-restricted mean of the earlier age of the resumed link."""
    order = np.argsort(ages, kind="stable")
    ids, ages, trait = ids[order], ages[order], trait[order]
    uniq = np.unique(ages)
    if len(uniq) < 2: return np.empty((0, 2)), 0
    step = np.median(np.diff(uniq))
    occ_id = np.cumsum(np.r_[True, np.diff(uniq) > 0.25 * step])
    occ_age = np.array([uniq[occ_id == o].mean() for o in np.unique(occ_id)])
    occ_of = dict(zip(uniq, occ_id))
    o = np.array([occ_of[a] for a in ages])
    k = len(occ_age)
    by = {j: {i: t for i, t in zip(ids[o == j + 1], trait[o == j + 1])} for j in range(k)}
    inc = np.full(k - 1, np.nan); anchor = np.full(k - 1, np.nan); npair = np.zeros(k - 1, int)
    for j in range(k - 1):
        gap = occ_age[j + 1] - occ_age[j]
        if not (0.5 * step <= gap < 1.5 * step): continue
        both = set(by[j]) & set(by[j + 1])
        if not both: continue
        ta = np.array([by[j][i] for i in both]); tb = np.array([by[j + 1][i] for i in both])
        ok = np.isfinite(ta) & np.isfinite(tb)
        if not ok.any(): continue
        inc[j] = np.mean(tb[ok] - ta[ok]); anchor[j] = np.mean(ta[ok]); npair[j] = ok.sum()
    if not np.isfinite(inc).any(): return np.empty((0, 2)), 0
    out = []; seg = 0; j = 0
    while j <= k - 2:
        if not np.isfinite(inc[j]): j += 1; continue
        seg += 1; cur = anchor[j]; out.append((occ_age[j], cur))
        while j <= k - 2 and np.isfinite(inc[j]):
            cur += inc[j]; out.append((occ_age[j + 1], cur)); j += 1
    return np.array(out), seg

def simulate(rho, n=250, mu_ls=25, mu=(600.0, 9.0, -0.3), seed=0, retain=1.0, scale=1.0):
    r = np.random.default_rng(seed)
    b = np.array([mu[0], mu[1] * scale, mu[2] * scale ** 2])
    sd = np.abs(np.r_[mu_ls, b]) * 0.2
    R = np.eye(4)
    for i, rr in enumerate(rho): R[0, i + 1] = R[i + 1, 0] = rr
    S = np.outer(sd, sd) * R
    th = r.multivariate_normal(np.r_[mu_ls, b], S, size=n)
    ls = np.clip(np.round(th[:, 0]), 1, None).astype(int)
    ids, ages, trait = [], [], []
    for i in range(n):
        a = np.arange(1, ls[i] + 1)
        f = th[i, 1] + th[i, 2] * a + th[i, 3] * a ** 2 + r.normal(0, 5, len(a))
        keep = r.random(len(a)) < retain if retain < 1 else np.ones(len(a), bool)
        ids += [i] * keep.sum(); ages += list(a[keep]); trait += list(f[keep])
    return np.array(ids), np.array(ages, float), np.array(trait), (mu_ls, sd, b)

def lam(t, mu_ls, sd_ls):
    a = (t - mu_ls) / sd_ls
    return stats.norm.pdf(a) / np.maximum(1 - stats.norm.cdf(a), 1e-12)

def run(label, rho, reps=25, retain=1.0, n=250, mu_ls=25, scale=1.0, mu=(600.0, 9.0, -0.3)):
    dev_obs, dev_dec, err_obs_analytic, err_inc_analytic, segs = [], [], [], [], []
    for s in range(reps):
        ids, ages, trait, (m_ls, sd, b) = simulate(rho, n=n, mu_ls=mu_ls, mu=mu, seed=100 + s, retain=retain, scale=scale)
        if len(ages) < 20: continue
        grid = np.arange(1, int(np.floor(m_ls)) + 1)                      # ages up to the mean lifespan
        truth = b[0] + b[1] * grid + b[2] * grid ** 2
        obs = np.array([trait[ages == t].mean() if (ages == t).any() else np.nan for t in grid])
        dec, seg = decomposition_trajectory(ids, ages, trait); segs.append(seg)
        dmap = {a: v for a, v in dec}
        dv = np.array([dmap.get(t, np.nan) for t in grid])
        dev_obs.append(np.nanmean(np.abs((obs - truth) / truth)) * 100)
        dev_dec.append(np.nanmean(np.abs((dv - truth) / truth)) * 100)
        # Eq. 9: observed deviation
        D = lam(grid, m_ls, sd[0]) * (rho[0] * sd[1] + grid * rho[1] * sd[2] + grid ** 2 * rho[2] * sd[3])
        err_obs_analytic.append(np.nanmean(np.abs(obs - truth - D)) / np.nanmean(np.abs(truth)) * 100)
        # Eq. 15: the decomposition's per-step change
        if len(dec) > 2:
            a0 = dec[:, 0]; f0 = dec[:, 1]
            keep = (a0[:-1] >= 2) & (a0[1:] <= m_ls) & (np.diff(a0) > 0)
            t = a0[:-1][keep]
            emp = np.diff(f0)[keep]
            pred = b[1] + b[2] * (2 * t + 1) + lam(t + 1, m_ls, sd[0]) * (rho[1] * sd[2] + (2 * t + 1) * rho[2] * sd[3])
            err_inc_analytic.append(np.nanmean(np.abs(emp - pred)))
    return (label, np.mean(dev_obs), np.mean(dev_dec), np.mean(err_obs_analytic), np.mean(err_inc_analytic), np.mean(segs))

rows = [
    run("no selective disappearance", (0, 0, 0)),
    run("intercept only (+0.5)", (0.5, 0, 0)),
    run("slope only (+0.5)", (0, 0.5, 0)),
    run("shape only (-0.5)", (0, 0, -0.5)),
    run("intercept + slope + shape", (0.5, 0.5, 0.5)),
    run("intercept only, MCAR 50%", (0.5, 0, 0), retain=0.5, n=500),
    run("slope only, MCAR 50%", (0, 0.5, 0), retain=0.5, n=500),
    run("short-lived (mean LS 5), complete", (0, 0.5, 0), mu_ls=5, scale=5.0),
    run("short-lived (mean LS 5), MCAR 50%", (0, 0.5, 0), mu_ls=5, scale=5.0, retain=0.5, n=500),
]
print(f"{'scenario':38s} {'obs vs true':>11s} {'decomp vs true':>14s} {'obs vs Eq.9':>12s} {'step vs Eq.15':>14s} {'segments':>9s}")
for lab, a, b_, c, d, e in rows:
    print(f"{lab:38s} {a:10.1f}% {b_:13.1f}% {c:11.2f}% {d:14.2f} {e:9.1f}")
