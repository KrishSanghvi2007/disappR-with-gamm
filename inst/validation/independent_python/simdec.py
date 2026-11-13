import numpy as np, pandas as pd
from scipy.stats import norm
SPEC_G = {"Linear": ([50, -0.8], 0.425, [0.04], [0.5]), "Quadratic": ([40, 2, -0.06], 0.425, [0.04, 0.0012], [0.5, 0.015]),
          "Cubic": ([40, 1.2, 0.06, -0.0035], 0.425, [0.04, 0.0012, 0.00003], [0.5, 0.015, 0.0005]),
          "Logarithmic": ([30, 8], 2.8, [0.27], [3.3]), "Asymptotic exponential": ([28, 6], 2.0, [0.17], [2.3])}
SPEC_C = {"Linear": ([np.log(40), -0.06], 0.0255, [0.003], [0.03]), "Quadratic": ([np.log(30), 0.12, -0.007], 0.0255, [0.003, 0.00015], [0.03, 0.0015])}
def basis(form, u, ref):
    return {"Linear": [u], "Cubic": [u, u**2, u**3], "Logarithmic": [np.log(np.maximum(u, 1e-9))],
            "Asymptotic exponential": [np.exp(-(u - ref[0]) / ref[1])]}.get(form, [u, u**2])
def simulate(form="Quadratic", count=False, strength="dramatic", sd_type="both", sd_dir=1, rate_var="low", mean_ls=20,
             missingness="complete", afr_mode="same", sa_type="none", sa_dir=1, n_id=300, seed=1, mu=None):
    rng = np.random.default_rng(seed); n = max(30, n_id); mean_ls = min(30, max(3, round(mean_ls))); cc = mean_ls / 20
    ind = afr_mode == "individual"
    sdl, sdr = sd_type in ("independent", "both"), sd_type in ("dependent", "both")
    sal, sar = ind and sa_type in ("independent", "both"), ind and sa_type in ("dependent", "both")
    z_ls, z_afr = rng.standard_normal(n), rng.standard_normal(n)
    mu0, s_rate, us, ul = (SPEC_C if count else SPEC_G)[form]
    mu = np.array(mu if mu is not None else mu0, float); k = 1 if strength == "dramatic" else 0.35
    sU = np.concatenate([[0.25 if count else 4], ul if rate_var == "high" else us])
    B = mu + rng.standard_normal((n, len(mu))) * sU
    B[:, 0] += k * (0.2125 if count else 5.1) * ((sd_dir * z_ls if sdl else 0) + (sa_dir * z_afr if sal else 0))
    B[:, 1] += k * s_rate * ((sd_dir * z_ls if sdr else 0) + (sa_dir * z_afr if sar else 0))
    LS = np.maximum(2, np.round(mean_ls + 0.3 * mean_ls * z_ls))
    if ind:
        span = max(2, round(0.4 * mean_ls)); AFR = 1 + np.floor(norm.cdf(0.9 * z_afr + np.sqrt(0.19) * rng.standard_normal(n)) * span)
        alive = np.where(LS >= span)[0]
    else:
        AFR = np.ones(n); alive = np.arange(n)
    cond = 0.7 * z_ls + np.sqrt(0.51) * rng.standard_normal(n); cond = (cond - cond.mean()) / cond.std(ddof=1)
    n_age = (LS[alive] - AFR[alive] + 1).astype(int); idx = np.repeat(alive, n_age)
    age = AFR[idx] + np.concatenate([np.arange(m) for m in n_age]); u = age / cc; ref = (u.mean(), u.std(ddof=1))
    X = np.column_stack([np.ones(len(u))] + basis(form, u, ref)); eta = (B[idx] * X).sum(1)
    trait = rng.poisson(np.exp(np.minimum(eta, np.log(2000)))).astype(float) if count else eta + rng.normal(0, 1.5, len(eta))
    s = 1 if strength == "dramatic" else 0.6
    zt = np.log1p(trait) if count else trait; zt = (zt - zt.mean()) / zt.std(ddof=1)
    kp = {"complete": np.ones(len(age)), "mcar": np.full(len(age), 0.75), "mwo": 1 / (1 + np.exp(0.5 * s / cc * (age - 0.7 * mean_ls))),
          "mwy": 1 - 1 / (1 + np.exp(0.5 * s / cc * (age - 0.35 * mean_ls))), "trait": 1 / (1 + np.exp(-(1.6 + 2 * s * zt))),
          "condition": 1 / (1 + np.exp(-(1.6 + 2 * s * cond[idx])))}[missingness]
    keep = rng.random(len(age)) < kp
    df = pd.DataFrame({"id": idx.astype(str), "age": age.astype(float), "trait": trait, "LS": LS[idx], "AFR": AFR[idx], "cond": cond[idx]})[keep].reset_index(drop=True)
    f = lambda i, t: (np.column_stack([np.ones(len(np.atleast_1d(t)))] + basis(form, np.atleast_1d(t) / cc, ref)) @ B[i].T)
    truth = lambda t: np.column_stack([np.ones(len(np.atleast_1d(t)))] + basis(form, np.atleast_1d(np.asarray(t, float)) / cc, ref)) @ mu
    lat = dict(B=B, LS=LS, AFR=AFR, alive=alive, cc=cc, ref=ref, form=form, count=count)
    return df, (lambda t: np.exp(truth(t)) if count else truth(t)), lat
def survivor_target(lat, t0, t_end):
    """Latent survivor-restricted chain: baseline = mean f_i(t0) over individuals present at t0 and t0+1; each step adds the
    mean latent change among individuals present at both ages."""
    a = lat["alive"]; AFR, LS, B = lat["AFR"][a], lat["LS"][a], lat["B"][a]
    def fv(t):
        X = np.concatenate([[1.0], np.concatenate([np.atleast_1d(b) for b in basis(lat["form"], np.array([t / lat["cc"]]), lat["ref"])])])
        e = B @ X
        return np.exp(e) if lat["count"] else e
    out = {}; both = (AFR <= t0) & (LS >= t0 + 1); cur = fv(t0)[both].mean(); out[t0] = cur; t = t0
    while t < t_end:
        both = (AFR <= t) & (LS >= t + 1)
        if not both.any(): break
        cur += (fv(t + 1)[both] - fv(t)[both]).mean(); t += 1; out[t] = cur
    return out
# ---------- decomposition ports
def signif(x, d=6):
    x = np.asarray(x, float); o = np.zeros_like(x); nz = x != 0
    m = np.floor(np.log10(np.abs(x[nz]))); o[nz] = np.round(x[nz] / 10**(m - d + 1)) * 10**(m - d + 1); return o
def id_age_means(df):
    d = df[np.isfinite(df.trait) & np.isfinite(df.age)]
    return d.groupby(["id", "age"], as_index=False).trait.mean().sort_values(["id", "age"]).reset_index(drop=True)
def infer_step(ia):
    a, g = ia.age.values, ia.id.values; tol = 1e-6 * max(1, a.max() - a.min())
    d = np.diff(a)[g[1:] == g[:-1]]; d = d[np.isfinite(d) & (d > tol)]
    if not len(d):
        d = np.diff(np.unique(a)); d = d[d > tol]
        if not len(d): return np.nan
    d = signif(d); v, c = np.unique(d, return_counts=True); sh = c / len(d); common = v[sh >= 0.05]
    if not len(common): return float(np.median(d))
    st = common.min(); dom = v[sh.argmax()]
    if sh.max() >= 0.6 and st / dom >= 0.75 and abs(dom / st - round(dom / st)) > 0.01: st = dom
    return float(st)
def linkwalk(ia, st):
    tol = 0.01 * st; gap = np.diff(ia.age.values); same = ia.id.values[1:] == ia.id.values[:-1]
    j = np.where(same & (gap >= 0.5 * st) & (gap < 1.5 * st))[0]
    if not len(j): return pd.DataFrame()
    P = pd.DataFrame({"k": np.round(ia.age.values[j] / tol), "e": np.round(ia.age.values[j + 1] / tol), "a": ia.age.values[j], "b": ia.age.values[j + 1],
                      "d": ia.trait.values[j + 1] - ia.trait.values[j], "s": ia.trait.values[j]})
    L = P.groupby(["k", "e"]).agg(a=("a", "mean"), b=("b", "mean"), inc=("d", "mean"), s=("s", "mean"), n=("d", "size")).reset_index()
    L = L.sort_values(["k", "n", "e"], ascending=[True, False, True]).drop_duplicates("k").reset_index(drop=True)
    i = 0; cur = L.s[0]; rows = [(L.a[0], cur, L.n[0])]
    while True:
        cur += L.inc[i]; rows.append((L.b[i], cur, L.n[i])); nx = np.where(np.abs(L.a.values - L.b[i]) <= tol)[0]
        if not len(nx): break
        i = nx[0]
    return pd.DataFrame(rows, columns=["age", "fitted", "n_pairs"])
def decomp(ia, version="0.9.6"):
    """0.9.5: link-walk only. 0.9.6: link-walk on a regular schedule; on an irregular one, records are placed on a common grid
    spaced by the median interval between an individual's successive records and chained over successive occasions."""
    if len(ia) < 2: return pd.DataFrame(), ""
    st = infer_step(ia)
    if not np.isfinite(st) or st <= 0: return pd.DataFrame(), ""
    first = ia.groupby("id").age.transform("min").values; rel = (ia.age.values - first) / st
    irregular = np.mean(np.abs(rel - np.round(rel)) > 0.01) > 0.2
    if version == "0.9.5" or not irregular: return linkwalk(ia, st), ""
    a, g = ia.age.values, ia.id.values; tol = 1e-6 * max(1, a.max() - a.min())
    d = np.diff(a)[g[1:] == g[:-1]]; d = d[d > tol]
    grid = float(np.median(d)) if len(d) else st
    occ = np.round((a - a.min()) / grid)
    occ_age = pd.Series(a).groupby(occ).mean()
    merged = bool((pd.Series(a).groupby(occ).agg(lambda x: x.max() - x.min()) > 0.01 * grid).any())
    ob = pd.DataFrame({"id": g, "occ": occ, "trait": ia.trait.values}).groupby(["id", "occ"], as_index=False).trait.mean().sort_values(["id", "occ"])
    same = ob.id.values[1:] == ob.id.values[:-1]; j = np.where(same & (np.diff(ob.occ.values) == 1))[0]
    if not len(j): return pd.DataFrame(), ""
    P = pd.DataFrame({"k": ob.occ.values[j], "d": ob.trait.values[j + 1] - ob.trait.values[j], "s": ob.trait.values[j]})
    inc = P.groupby("k").d.mean(); npr = P.groupby("k").d.size(); k = inc.index.min()
    cur = P.s[P.k == k].mean(); rows = [(occ_age[k], cur, npr[k])]
    while True:
        cur += inc[k]; rows.append((occ_age[k + 1], cur, npr[k])); k += 1
        if k not in inc.index: break
    return pd.DataFrame(rows, columns=["age", "fitted", "n_pairs"]), ("merged" if merged else "")
