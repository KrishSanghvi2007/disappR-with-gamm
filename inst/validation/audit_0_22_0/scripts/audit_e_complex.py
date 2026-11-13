"""Part E of the audit (0.22.7): complicated data structures and combinations not tested before, run through the
ports of the engine (port022.py) and the audit's mixed models (lmm022.py). The same scenarios run through the real R
engine in tests/scripts/complex_scenarios.R.
E1 monthly ages (decimal interval 1/12) with a rounded interval typed by the user
E2 staggered biennial cohorts (half the individuals sampled in odd years, half in even years)
E3 sampling that changes frequency with age (annual, then biennial)
E4 a study window with a calendar-year trend: left truncation, censoring and cohort confounding, no selection
E5 selective disappearance through lifespan, with AFR correlated with lifespan but no selective appearance
E6 an unstandardised cubic on ages in days
E7 tiny and degenerate data: few individuals, singletons, two distinct ages
E8 calibration of the instant error-family screen: false alarms and detection
E9 continuous proportions: beta against Gaussian"""
import os, sys, time, numpy as np, pandas as pd
from scipy import stats, optimize, special
sys.path.insert(0, os.path.dirname(__file__))
import port022 as P
from lmm022 import lmm_ri
QUICK = os.environ.get("QUICK") == "1"
fails = []
def check(label, ok, detail=""):
    print(("PASS " if ok else "FAIL ") + label + (": " + detail if detail else ""))
    if not ok: fails.append(label)
t0 = time.time()
rng = np.random.default_rng(2207)

def frame(rows): return pd.concat(rows, ignore_index=True)

print("== E1 monthly ages, interval typed as 0.0833")
rows = []
for i in range(200):
    n = rng.integers(12, 121); a = np.round(np.arange(1, n + 1) / 12.0, 10)
    keep = rng.random(n) > 0.1
    rows.append(pd.DataFrame(dict(id="m%03d" % i, age=a[keep], trait=5 + 0.2 * a[keep] + rng.normal(0, 1, keep.sum()))))
d = frame(rows)
st = P.infer_age_step(d.age.values, d.id.values)
au = P.decomposition_trajectory(d); se = P.decomposition_trajectory(d, 0.0833)
ga = P.missing_grid(d); gs = P.missing_grid(d, 0.0833)
print("   inferred %.6f; auto: %d links, %d segments; typed 0.0833: %d links, %d segments, %d records dropped" % (st, len(au["rows"]), au["segments"], len(se["rows"]), se["segments"], se["n_off"]))
print("   missing: auto %.1f%%, typed %.1f%%" % (100 * ga["missing"], 100 * gs["missing"]))
check("E1 a rounded monthly interval keeps every record and the same missingness", se["n_off"] == 0 and abs(ga["missing"] - gs["missing"]) < 0.01)
drift = 120 * abs(1 / 12 - 0.0833) / 0.0833
print("   drift of the typed grid after 10 years: %.3f of an interval (records are dropped beyond 0.25, i.e. after ~%.0f years)" % (drift, 0.25 * 0.0833 / abs(1 / 12 - 0.0833) / 12))

print("\n== E2 staggered biennial cohorts")
rows = []
for i in range(300):
    start = 1 + (i % 2); n = rng.integers(3, 8); a = start + 2.0 * np.arange(n)
    rows.append(pd.DataFrame(dict(id="s%03d" % i, age=a, trait=10 - 0.3 * a + rng.normal(0, 1, n))))
d = frame(rows)
st = P.infer_age_step(d.age.values, d.id.values); de = P.decomposition_trajectory(d); g = P.missing_grid(d)
print("   inferred interval %.0f, missing %.1f%%, decomposition: %d points, %d segments" % (st, 100 * g["missing"], len(de["rows"]), de["segments"]))
check("E2 the interval (2) and missingness (0%) are right for staggered cohorts", st == 2 and g["missing"] < 0.01)
print("   finding: the decomposition links neighbouring occasions (1 year apart), which no individual shares, so it is empty (%s)" % ("confirmed" if len(de["rows"]) == 0 else "not confirmed"))

print("\n== E3 annual sampling to age 5, then biennial")
rows = []
for i in range(300):
    L = rng.integers(3, 16); a = np.array([x for x in range(1, L + 1) if x <= 5 or (x - 5) % 2 == 0], float)
    rows.append(pd.DataFrame(dict(id="f%03d" % i, age=a, trait=10 - 0.2 * a + rng.normal(0, 1, len(a)))))
d = frame(rows)
g = P.missing_grid(d); st = P.infer_age_step(d.age.values, d.id.values)
print("   inferred interval %.0f; missing %.1f%% overall, by design: the grid counts the skipped years after age 5 as missed" % (st, 100 * g["missing"]))
check("E3 a planned change of frequency shows as missingness (documented in the help: counted against the interval)", g["missing"] > 0.1)

print("\n== E4 study window with a calendar-year trend, no selective disappearance")
def window_data(seed, year_eff):
    r = np.random.default_rng(seed); rows = []
    for i in range(400):
        birth = r.integers(0, 40); ls = int(np.clip(round(r.normal(8, 3)), 2, 18))
        ages = np.arange(1, ls + 1); years = birth + ages
        inw = (years >= 15) & (years <= 35)
        if inw.sum() < 1: continue
        a = ages[inw].astype(float); y = years[inw]
        tr = 10 + 0.6 * a - 0.04 * a ** 2 + r.normal(0, 1) + year_eff * (y - 25) + r.normal(0, 1, len(a))
        rows.append(pd.DataFrame(dict(id=i, age=a, year=y, trait=tr)))
    d = frame(rows); d["alr"] = d.groupby("id").age.transform("max"); return d
def design(d, model, year=False):
    az = ((d.age - d.age.mean()) / d.age.std()).values; A = ((d.alr - d.alr.mean()) / d.alr.std()).values
    X = [np.ones(len(d)), az, az ** 2]
    if model in ("M2", "M4"): X.append(A)
    if model == "M4": X += [az * A, az ** 2 * A]
    if year: X += [(d.year == y).astype(float).values for y in sorted(d.year.unique())[1:]]
    return np.column_stack(X)
reps = 25 if QUICK else 80
for ye in (0.0, -0.08):
    fp2 = fp4 = fp2y = 0
    for s in range(reps):
        d = window_data(3000 + s, ye); g = d.id.values; y = d.trait.values
        a1, a2, a4 = (lmm_ri(design(d, m), y, g)["aic"] for m in ("M1", "M2", "M4"))
        fp2 += (a1 - a2) > 2; fp4 += (a1 - a4) > 2
        b1, b2 = (lmm_ri(design(d, m, True), y, g)["aic"] for m in ("M1", "M2"))
        fp2y += (b1 - b2) > 2
    print("   year trend %+.2f/yr: Model 2 wrongly preferred %3.0f%%, Model 4 %3.0f%%; with year in the model, Model 2 %3.0f%%" % (ye, 100 * fp2 / reps, 100 * fp4 / reps, 100 * fp2y / reps))
    if ye == 0: check("E4 window truncation and censoring alone do not create selective disappearance (Model 2 <= 15%)", fp2 / reps <= 0.15)
    else:
        check("E4 a calendar trend under a study window creates false selective disappearance", fp2 / reps >= 0.3, "%.0f%%" % (100 * fp2 / reps))
        check("E4 adding year (a random intercept in the app) removes it (<= 15%)", fp2y / reps <= 0.15, "%.0f%%" % (100 * fp2y / reps))

print("\n== E5 selective disappearance with AFR correlated with lifespan, no effect of AFR itself")
def afr_data(seed, on):
    """on = 'death': the rate of ageing depends on age at death (ALR); on = 'duration': on the length of the adult life
    (ALR - AFR + 1). In both, AFR has no effect of its own and is correlated with lifespan."""
    r = np.random.default_rng(seed); ind = []
    for i in range(300):
        z = r.normal(); ls = int(np.clip(round(10 + 3 * z), 3, 20)); afr = int(np.clip(round(3 - 0.8 * z + r.normal(0, 0.6)), 1, 6))
        ind.append((i, afr, ls, afr + ls - 1))
    ind = pd.DataFrame(ind, columns=["id", "afr", "ls", "death"])
    key = ind.death if on == "death" else ind.ls
    ind["zk"] = (key - key.mean()) / key.std()
    rows = []
    for _, q in ind.iterrows():
        a = np.arange(q.afr, q.afr + q.ls, dtype=float)
        tr = 10 + (-0.3 + 0.15 * q.zk) * (a - 8) + r.normal(0, 1) + r.normal(0, 1, len(a))
        rows.append(pd.DataFrame(dict(id=int(q.id), age=a, trait=tr, afr=float(q.afr))))
    d = frame(rows); d["alr"] = d.groupby("id").age.transform("max"); return d
def dz(v): return ((v - v.mean()) / v.std()).values
reps = 20 if QUICK else 60
for on in ("death", "duration"):
    sig10 = sig8 = 0; rs = []
    for s in range(reps):
        d = afr_data(4000 + s, on); g = d.id.values; y = d.trait.values
        az, A, F = dz(d.age), dz(d.alr), dz(d.afr)
        rs.append(np.corrcoef(d.groupby("id").afr.first(), d.groupby("id").alr.first())[0, 1])
        base = [np.ones(len(d)), az]
        m10 = lmm_ri(np.column_stack(base + [A, F, az * F]), y, g)
        m8 = lmm_ri(np.column_stack(base + [A, F, az * A, az * F]), y, g)
        sig10 += abs(m10["beta"][4] / np.sqrt(m10["vcov"][4, 4])) > 1.96
        sig8 += abs(m8["beta"][5] / np.sqrt(m8["vcov"][5, 5])) > 1.96
    print("   selection on %-8s (AFR-ALR r %.2f): AFR x age 'significant' in Model 10 %3.0f%%, in Model 8 %3.0f%%" % (
        "age at death" if on == "death" else "adult lifespan", np.mean(rs), 100 * sig10 / reps, 100 * sig8 / reps))
    if on == "death":
        check("E5 confounded AFR makes Model 10 report selective appearance", sig10 / reps >= 0.3, "%.0f%%" % (100 * sig10 / reps))
        check("E5 Model 8, with ALR x age, removes it (<= 20%)", sig8 / reps <= 0.2, "%.0f%%" % (100 * sig8 / reps))
    else:
        print("   finding: when selection acts on adult lifespan (ALR - AFR), AFR x age carries part of it even in Model 8: an 'appearance' term then reflects lifespan measured from entry, not an effect of entry age")

print("\n== E6 unstandardised cubic on ages in days")
rows = []
for i in range(150):
    a = np.sort(rng.choice(np.arange(0, 2000, 30), size=rng.integers(4, 12), replace=False)).astype(float)
    rows.append(pd.DataFrame(dict(id=i, age=a, trait=5 + 0.002 * a - 1e-6 * a ** 2 + rng.normal(0, 1) + rng.normal(0, 0.5, len(a)))))
d = frame(rows)
Xr = np.column_stack([np.ones(len(d)), d.age, d.age ** 2, d.age ** 3]); z = dz(d.age); Xs = np.column_stack([np.ones(len(d)), z, z ** 2, z ** 3])
fr, fs = lmm_ri(Xr, d.trait.values, d.id.values), lmm_ri(Xs, d.trait.values, d.id.values)
cr, cs = np.linalg.cond(Xr), np.linalg.cond(Xs)
diff = np.max(np.abs(Xr @ fr["beta"] - Xs @ fs["beta"]))
print("   condition number: raw %.1e, standardised %.1e; largest difference in fitted values %.2e" % (cr, cs, diff))
print("   raw coefficients: %s (two-decimal display: %s)" % (", ".join("%.3g" % b for b in fr["beta"]), ", ".join("%.2f" % b for b in fr["beta"])))
check("E6 the raw cubic on days is ill-conditioned (condition number > 1e9)", cr > 1e9, "%.1e" % cr)
check("E6 its fit still matches the standardised one here (< 1e-3)", diff < 1e-3, "%.1e" % diff)

print("\n== E7 tiny and degenerate data")
tiny = frame([pd.DataFrame(dict(id=i, age=np.arange(1, 1 + (1 if i < 8 else 3), dtype=float), trait=rng.normal(5, 1, 1 if i < 8 else 3))) for i in range(12)])
res = {}
for lab, fn in [("step", lambda: P.infer_age_step(tiny.age.values, tiny.id.values)), ("grid", lambda: P.missing_grid(tiny)["missing"]),
                ("decomposition", lambda: len(P.decomposition_trajectory(tiny)["rows"])),
                ("share of wins, no common set", lambda: P.share_best(pd.DataFrame(dict(id=["a", "b"], Function=["L", "Q"], AICc=[np.nan, np.nan])), ["L", "Q"])[2]),
                ("peak on three ages", lambda: P.curve_turning_points(np.array([1.0, 2, 3]), np.array([1.0, 2, 1]))["peak"]),
                ("stored table, one model", lambda: len(P.stored_comparison_table([dict(id="S1", data_sig="a", rows="r", family_class="continuous", AIC=10.0)])))]:
    try: res[lab] = fn(); ok = True
    except Exception as e: res[lab] = "ERROR %s" % e; ok = False
    print("   %-32s %s" % (lab, res[lab])); check("E7 %s runs on degenerate data" % lab, ok)
two = frame([pd.DataFrame(dict(id=i, age=np.array([1.0, 2.0]), trait=rng.normal(5, 1, 2))) for i in range(30)])
X = np.column_stack([np.ones(len(two)), two.age, two.age ** 2])
print("   two distinct ages, quadratic: design rank %d of %d (the app drops the unidentifiable term and marks the fit 'Caution')" % (np.linalg.matrix_rank(X), X.shape[1]))

print("\n== E8 calibration of the instant error-family screen")
def poisson_glm(X, y):
    b = np.zeros(X.shape[1]); b[0] = np.log(max(y.mean(), 1e-3))
    for _ in range(50):
        mu = np.exp(X @ b); W = mu; z = X @ b + (y - mu) / mu
        b_new = np.linalg.solve(X.T @ (W[:, None] * X), X.T @ (W * z))
        if np.max(np.abs(b_new - b)) < 1e-8: b = b_new; break
        b = b_new
    return np.exp(X @ b)
def screen_counts(y, X, sims=50, seed=1):
    mu = poisson_glm(X, y); ratio = np.sum((y - mu) ** 2 / mu) / (len(y) - X.shape[1])
    r = np.random.default_rng(seed); zs = np.array([np.mean(r.poisson(mu) == 0) for _ in range(sims)])
    zero = np.mean(y == 0) > np.quantile(zs, 0.975) and np.mean(y == 0) - zs.mean() > 0.02
    return ratio > 1.5, zero
reps = 60 if QUICK else 200
for lab, gen in [("Poisson (no problem)", lambda r, mu: r.poisson(mu)),
                 ("negative binomial, theta 2", lambda r, mu: r.negative_binomial(2, 2 / (2 + mu))),
                 ("Poisson + 25% extra zeros", lambda r, mu: np.where(r.random(len(mu)) < 0.25, 0, r.poisson(mu)))]:
    od = zz = 0
    for s in range(reps):
        r = np.random.default_rng(5000 + s); age = r.uniform(1, 10, 600); X = np.column_stack([np.ones(600), age, age ** 2])
        mu = np.exp(0.8 + 0.25 * age - 0.02 * age ** 2); y = gen(r, mu).astype(float)
        a, b = screen_counts(y, X, seed=s); od += a; zz += b
    print("   %-28s overdispersion flagged %3.0f%%, excess zeros flagged %3.0f%%" % (lab, 100 * od / reps, 100 * zz / reps))
    if lab.startswith("Poisson ("): check("E8 few false alarms on Poisson data (each <= 5%)", od / reps <= 0.05 and zz / reps <= 0.05)
    elif lab.startswith("negative"): check("E8 overdispersion detected (>= 90%)", od / reps >= 0.9)
    else: check("E8 excess zeros detected (>= 90%)", zz / reps >= 0.9)
sk_fa = sk_hit = 0
for s in range(reps):
    r = np.random.default_rng(6000 + s); e1 = r.normal(0, 1, 600); e2 = np.exp(r.normal(0, 0.5, 600)); e2 = e2 - e2.mean()
    sk_fa += stats.skew(e1) > 1; sk_hit += stats.skew(e2) > 1
print("   skewed-residual screen: flagged on Gaussian residuals %.0f%%, on lognormal (sigma 0.5) residuals %.0f%%" % (100 * sk_fa / reps, 100 * sk_hit / reps))
check("E8 the skew screen does not flag Gaussian residuals", sk_fa == 0)
sk_mod = 0
for s in range(reps):
    r = np.random.default_rng(7000 + s); e = np.exp(r.normal(0, 0.3, 600)); sk_mod += stats.skew(e) > 1
print("   finding: with mild skew (lognormal sigma 0.3) the screen flags %.0f%%; the automatic AIC check covers these cases" % (100 * sk_mod / reps))

print("\n== E9 continuous proportions: beta against Gaussian")
reps = 20 if QUICK else 60; wins = 0
for s in range(reps):
    r = np.random.default_rng(8000 + s); age = r.uniform(1, 10, 500); X = np.column_stack([np.ones(500), age])
    mu = special.expit(-1 + 0.3 * age); phi = 8.0; y = r.beta(mu * phi, (1 - mu) * phi)
    def nll(p): m = special.expit(X @ p[:2]); f = np.exp(p[2]); return -np.sum(stats.beta.logpdf(y, m * f, (1 - m) * f))
    fb = optimize.minimize(nll, [0, 0, 1], method="Nelder-Mead", options=dict(maxiter=4000, xatol=1e-8, fatol=1e-10))
    bg = np.linalg.lstsq(X, y, rcond=None)[0]; s2 = np.mean((y - X @ bg) ** 2)
    aic_b = 2 * fb.fun + 6; aic_g = -2 * np.sum(stats.norm.logpdf(y, X @ bg, np.sqrt(s2))) + 6
    wins += aic_b < aic_g - 2
print("   beta preferred over Gaussian by more than 2 AIC in %.0f%% of %d datasets of beta-distributed proportions" % (100 * wins / reps, reps))
check("E9 the beta family is recognised on beta data (>= 90%)", wins / reps >= 0.9)
y01 = np.array([0.0, 0.3, 1.0]); print("   values of exactly 0 or 1 are refused by the beta family (%d here), as the error message states" % np.sum((y01 <= 0) | (y01 >= 1)))
print("\n%d failure(s)%s   [%.0f s]" % (len(fails), (": " + "; ".join(fails)) if fails else "", time.time() - t0))
