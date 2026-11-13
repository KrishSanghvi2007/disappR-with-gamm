"""Part H of the audit (0.23.2).
H1 continuous integration: workflow files, what they run, and the server test wired into the unit tests
H2 the individual frailty in the discrete-time hazard models: does it change type I error and power, and how often
   can the frailty be estimated? (logistic GLM against a random-intercept logistic GLMM by Gauss-Hermite quadrature)
H3 Models 3 and 5: the preprint's, the revised manuscript's and the app's specifications of the within-individual
   quadratic term, on the preprint's own simulation design (quadratic ageing, lifespan correlated with intercept,
   slope or shape; complete sampling and 50% missing at random)
       mean of squares   age^2 - mean_i(age^2)      revised manuscript, Table 1; the app (delta_f2 = f2 - mean_i(f2))
       square of mean    age^2 - mean_i(age)^2      Fay et al. 2022 eq. 3; one reading of the preprint's notation
       squared deviation (age - mean_i(age))^2      Fay et al. 2022 eq. 4 (shown there to be biased for ageing)
   and the app's standardised parameterisation against the raw one. Set QUICK=1 for a short run."""
import os, re, sys, time, numpy as np, pandas as pd, yaml
from scipy import optimize, special
sys.path.insert(0, os.path.dirname(__file__))
import port022 as P
from lmm022 import lmm_ri
QUICK = os.environ.get("QUICK") == "1"
PART = os.environ.get("PART", "all")
ROOT = P.ROOT
fails = []
def check(label, ok, detail=""):
    print(("PASS " if ok else "FAIL ") + label + (": " + detail if detail else ""))
    if not ok: fails.append(label)

if PART in ("all", "H1"):
    print("== H1 continuous integration")
    wf = os.path.join(ROOT, ".github", "workflows")
    files = sorted(os.listdir(wf)) if os.path.isdir(wf) else []
    docs = {f: yaml.safe_load(open(os.path.join(wf, f))) for f in files}
    check("three workflows, all valid YAML", files == ["R-CMD-check.yaml", "script-tiers.yaml", "test-coverage.yaml"], ", ".join(files))
    rc = docs.get("R-CMD-check.yaml", {})
    oses = {c["os"] for c in rc["jobs"]["R-CMD-check"]["strategy"]["matrix"]["config"]}
    trig = rc.get(True) or rc.get("on")
    check("R CMD check on macOS, Windows and Linux, on every push and pull request", oses == {"macos-latest", "windows-latest", "ubuntu-latest"} and "push" in trig and "pull_request" in trig)
    steps = " ".join(str(s) for s in rc["jobs"]["app-server"]["steps"])
    check("the app server test and the source-tree unit tests run on every push", "tests/scripts/app_test.R" in steps and "test_local" in steps)
    tc = docs.get("test-coverage.yaml", {})
    check("coverage computed on every push; upload cannot fail the build", "covr::package_coverage" in str(tc) and "continue-on-error" in str(tc))
    st = docs.get("script-tiers.yaml", {})
    trig = st.get(True) or st.get("on")
    check("all script tiers run weekly and on demand", "schedule" in trig and "workflow_dispatch" in trig and "run_all.R" in str(st))
    check(".github is left out of the built package", "^\\.github$" in open(os.path.join(ROOT, ".Rbuildignore")).read())
    app = open(os.path.join(ROOT, "tests", "scripts", "app_test.R"), encoding="utf-8").read()
    check("the app test now fails with a non-zero status when an output errors", "quit(status = 1)" in app)
    srv = open(os.path.join(ROOT, "inst", "app", "server.R"), encoding="utf-8").read()
    i0 = app.index("all_outputs <- c("); dep = 0
    for k0 in range(app.index("(", i0), len(app)):
        dep += (app[k0] == "(") - (app[k0] == ")")
        if dep == 0: break
    listed = re.findall(r'"([A-Za-z0-9_]+)"', app[i0:k0])
    defined = set(re.findall(r"output\$([A-Za-z0-9_]+)\s*<-", srv)) | {"code_%s_text" % s for s in ("data", "visual", "sampling", "individual")}
    stale = [n for n in listed if n not in defined]
    check("every output the app test renders is defined by the server (0.24.6)", not stale, ", ".join(stale))
    ut = open(os.path.join(ROOT, "tests", "testthat", "test-app-server.R"), encoding="utf-8").read()
    check("the server test is part of the unit tests (skipped on CRAN and outside a source tree)", "skip_on_cran()" in ut and "app_test.R" in ut and 'inst", "app", "global.R' in ut)

# ---------------------------------------------------------------- H2 hazard models
def logit_glm(X, y):
    b = np.zeros(X.shape[1])
    for _ in range(60):
        eta = X @ b; p = special.expit(eta); W = p * (1 - p) + 1e-12
        H = X.T @ (W[:, None] * X); g = X.T @ (y - p)
        step = np.linalg.solve(H, g); b += step
        if np.max(np.abs(step)) < 1e-9: break
    p = special.expit(X @ b); W = p * (1 - p)
    V = np.linalg.inv(X.T @ (W[:, None] * X))
    ll = np.sum(y * np.log(p + 1e-300) + (1 - y) * np.log(1 - p + 1e-300))
    return b, V, ll
GH_X, GH_W = np.polynomial.hermite.hermgauss(25)
def logit_glmm(X, y, g):
    """Random-intercept logistic GLMM by adaptive-free Gauss-Hermite quadrature (25 nodes)."""
    _, gi = np.unique(g, return_inverse=True); nid = gi.max() + 1
    b0, _, _ = logit_glm(X, y)
    def nll(th):
        b = th[:-1]; s = np.exp(th[-1])
        eta = X @ b
        u = np.sqrt(2) * s * GH_X
        E = eta[:, None] + u[None, :]
        ll_obs = y[:, None] * E - np.log1p(np.exp(E))
        L = np.zeros((nid, len(u))); np.add.at(L, gi, ll_obs)
        m = L.max(axis=1, keepdims=True)
        li = m[:, 0] + np.log(np.exp(L - m) @ GH_W / np.sqrt(np.pi))
        return -np.sum(li)
    best = None
    for s0 in (np.log(0.5), np.log(1.5)):
        res = optimize.minimize(nll, np.r_[b0, s0], method="L-BFGS-B", bounds=[(None, None)] * len(b0) + [(-8, 3)])
        if best is None or res.fun < best.fun: best = res
    th = best.x
    H = optimize.approx_fprime  # numerical Hessian by finite differences of the gradient
    eps = 1e-4; k = len(th); Hm = np.zeros((k, k))
    g0 = optimize.approx_fprime(th, nll, eps)
    for j in range(k):
        e = np.zeros(k); e[j] = eps
        Hm[:, j] = (optimize.approx_fprime(th + e, nll, eps) - g0) / eps
    Hm = (Hm + Hm.T) / 2
    try: V = np.linalg.inv(Hm)
    except np.linalg.LinAlgError: V = np.full((k, k), np.nan)
    return th[:-1], V[:-1, :-1], np.exp(th[-1]), -best.fun
def sim_hazard(seed, trait_effect=0.0, frailty_sd=1.0, n_id=300, max_age=15):
    r = np.random.default_rng(seed)
    u = r.normal(0, frailty_sd, n_id); a = r.normal(0, 1, n_id)
    rows = []
    for i in range(n_id):
        for t in range(1, max_age + 1):
            tr = a[i] + r.normal(0, 1)
            p = special.expit(-2.6 + 0.08 * t + u[i] + trait_effect * tr)
            ev = r.random() < p
            rows.append((i, t, tr, int(ev)))
            if ev: break
    d = pd.DataFrame(rows, columns=["id", "age", "trait", "event"])
    d["trait_z"] = (d.trait - d.trait.mean()) / d.trait.std(); d["age_c"] = (d.age - d.age.mean()) / d.age.std()
    return d
if PART in ("all", "H2"):
    print("\n== H2 individual frailty in the hazard models (null: the trait does not affect disappearance)")
    reps = 40 if QUICK else 150
    res = {"GLM": [], "GLMM": []}; sd_est = []; pw = {"GLM": [], "GLMM": []}
    t0 = time.time()
    for rep in range(reps):
        for eff, store in ((0.0, res), (0.25, pw)):
            d = sim_hazard(7000 + rep + (100000 if eff else 0), trait_effect=eff)
            X = np.column_stack([np.ones(len(d)), d.age_c, d.trait_z]); y = d.event.values.astype(float)
            b, V, _ = logit_glm(X, y); z1 = b[2] / np.sqrt(V[2, 2])
            bm, Vm, s, _ = logit_glmm(X, y, d.id.values); z2 = bm[2] / np.sqrt(Vm[2, 2]) if np.isfinite(Vm[2, 2]) and Vm[2, 2] > 0 else np.nan
            store["GLM"].append(abs(z1) > 1.96); store["GLMM"].append(abs(z2) > 1.96 if np.isfinite(z2) else False)
            if eff == 0.0: sd_est.append(s)
    sd_est = np.array(sd_est)
    print("   %d datasets per condition (%.0f s): 300 individuals, one terminal event each, frailty SD 1, age as a covariate" % (reps, time.time() - t0))
    print("   type I error of the trait effect: GLM %.1f%%, GLMM %.1f%%   |   power (log-odds 0.25 per SD): GLM %.1f%%, GLMM %.1f%%" % (
        100 * np.mean(res["GLM"]), 100 * np.mean(res["GLMM"]), 100 * np.mean(pw["GLM"]), 100 * np.mean(pw["GLMM"])))
    print("   frailty SD estimated (true 1): median %.2f, at the lower bound (< 0.05) in %.0f%% of datasets" % (np.median(sd_est), 100 * np.mean(sd_est < 0.05)))
    check("H2 the GLM's type I error is near 5% even with unmodelled frailty (<= 8%)", np.mean(res["GLM"]) <= 0.08, "%.1f%%" % (100 * np.mean(res["GLM"])))
    check("H2 the frailty GLMM keeps type I error near 5% (<= 8%)", np.mean(res["GLMM"]) <= 0.08, "%.1f%%" % (100 * np.mean(res["GLMM"])))

# ---------------------------------------------------------------- H3 Models 3 and 5
def sim_preprint(seed, rho, n_id=250, mcar=False):
    """The preprint's design: LS, intercept, slope and shape from a multivariate normal (CV 0.2), lifespan correlated
    with one or more fecundity variables; F = b0 + b1 age + b2 age^2 + e (sd 5), ages 1..LS."""
    r = np.random.default_rng(seed)
    mu = np.array([25, 600, 9, -0.3]); sd = 0.2 * np.abs(mu)
    C = np.eye(4); C[0, 1:] = C[1:, 0] = rho
    Sig = C * np.outer(sd, sd)
    n = 2 * n_id if mcar else n_id
    T = r.multivariate_normal(mu, Sig, n)
    rows = []
    for i in range(n):
        ls = max(1, int(round(T[i, 0])))
        a = np.arange(1, ls + 1, dtype=float)
        f = T[i, 1] + T[i, 2] * a + T[i, 3] * a ** 2 + r.normal(0, 5, len(a))
        keep = r.random(len(a)) < 0.5 if mcar else np.ones(len(a), bool)
        if keep.sum() == 0: continue
        rows.append(pd.DataFrame(dict(id=i, age=a[keep], F=f[keep])))
    d = pd.concat(rows, ignore_index=True)
    g = d.groupby("id")
    d["m1"] = g.age.transform("mean"); d["m2"] = g.age.transform(lambda v: np.mean(v ** 2)); d["ALR"] = g.age.transform("max")
    return d
def design_centring(d, version, model, m1=None, m2=None, age=None):
    age = d.age.values if age is None else age
    m1 = d.m1.values if m1 is None else m1
    m2 = d.m2.values if m2 is None else m2
    da = age - m1
    dq = {"mean of squares": age ** 2 - m2, "square of mean": age ** 2 - m1 ** 2, "squared deviation": (age - m1) ** 2}[version]
    cols = [np.ones(len(age)), da, dq, m1]
    if model == "M5": cols += [da * m1, dq * m1]
    return np.column_stack(cols)
TRUE = lambda a: 600 + 9 * a - 0.3 * a ** 2
if PART in ("all", "H3"):
    print("\n== H3 Models 3 and 5: three specifications of the within-individual quadratic term")
    reps = 6 if QUICK else 30
    scen = {"no selective disappearance": (0, 0, 0), "intercept (+0.5)": (0.5, 0, 0), "slope (+0.5)": (0, 0.5, 0), "shape (-0.5)": (0, 0, -0.5),
            "intercept, slope, shape (+0.5)": (0.5, 0.5, 0.5)}
    versions = ["mean of squares", "square of mean", "squared deviation"]
    rows = []; t0 = time.time()
    grid = np.arange(1, 41, dtype=float)
    for si, (sname, rho) in enumerate(scen.items()):
        for mcar in (False, True):
            for rep in range(reps):
                d = sim_preprint(900 * si + rep + (50000 if mcar else 0), np.array(rho), mcar=mcar)
                y = d.F.values; g = d.id.values
                ind = d.drop_duplicates("id")
                m1s, m2s = ind.m1.mean(), ind.m2.mean()
                aics = {}
                for v in versions:
                    for m in ("M3", "M5"):
                        f = lmm_ri(design_centring(d, v, m), y, g)
                        Xg = design_centring(None, v, m, m1=np.full(len(grid), m1s), m2=np.full(len(grid), m2s), age=grid) if False else \
                             design_centring(pd.DataFrame({"age": grid}), v, m, m1=np.full(len(grid), m1s), m2=np.full(len(grid), m2s), age=grid)
                        pred = Xg @ f["beta"]
                        dev = 100 * (pred - TRUE(grid)) / TRUE(grid)
                        aics[(v, m)] = f["aic"]
                        rows.append(dict(scenario=sname, sampling="50% missing" if mcar else "complete", version=v, model=m,
                                         aic=f["aic"], dev_all=np.mean(np.abs(dev)), dev_late=np.mean(np.abs(dev[grid >= 30]))))
                # the app's standardised parameterisation against the raw 'mean of squares' version (M5)
                z = (d.age - d.age.mean()) / d.age.std(); f1 = z.values; f2 = f1 ** 2
                mf1 = pd.Series(f1).groupby(g).transform("mean").values; mf2 = pd.Series(f2).groupby(g).transform("mean").values
                Xapp = np.column_stack([np.ones(len(d)), mf1, f1 - mf1, f2 - mf2, mf1 * (f1 - mf1), mf1 * (f2 - mf2)])
                rows.append(dict(scenario=sname, sampling="50% missing" if mcar else "complete", version="app (standardised)", model="M5",
                                 aic=lmm_ri(Xapp, y, g)["aic"], dev_all=np.nan, dev_late=np.nan, raw_equiv=abs(lmm_ri(Xapp, y, g)["aic"] - aics[("mean of squares", "M5")])))
    R = pd.DataFrame(rows)
    R.to_csv(os.path.join(os.path.dirname(__file__), "..", "h_models35.csv"), index=False)
    print("   %d datasets, %.0f s" % (len(scen) * 2 * reps, time.time() - t0))
    eq = R[R.version == "app (standardised)"].raw_equiv.max()
    check("H3 the app's standardised Model 5 is the revised manuscript's Model 5 (identical AIC)", eq < 1e-6, "largest AIC difference %.1e" % eq)
    S = R[R.version != "app (standardised)"].groupby(["scenario", "sampling", "model", "version"]).agg(
        AIC=("aic", "mean"), dev_all=("dev_all", "mean"), dev_late=("dev_late", "mean")).reset_index()
    best = S.loc[S.groupby(["scenario", "sampling", "model"]).AIC.idxmin()][["scenario", "sampling", "model", "version"]]
    pd.set_option("display.width", 220)
    S["dAIC"] = S.AIC - S.groupby(["scenario", "sampling", "model"]).AIC.transform("min")
    print(S[["scenario", "sampling", "model", "version", "dAIC", "dev_all", "dev_late"]].round(1).to_string(index=False))
    tot = S.groupby("version").agg(dev_all=("dev_all", "mean"), dev_late=("dev_late", "mean"), dAIC=("dAIC", "mean")).round(2)
    print("\n   averaged over scenarios and sampling:"); print(tot.to_string())
    ms = S[S.version == "mean of squares"].set_index(["scenario", "sampling", "model"]); sm = S[S.version == "square of mean"].set_index(["scenario", "sampling", "model"])
    sd = S[S.version == "squared deviation"].set_index(["scenario", "sampling", "model"])
    gap = (ms.dev_all - sm.dev_all).abs().max(); daic = (ms.AIC - sm.AIC).abs().max()
    check("H3 'mean of squares' (app, revised manuscript) and 'square of mean' give nearly the same trajectories (within 0.5 points)", gap <= 0.5, "largest gap %.2f points; AIC gap up to %.1f" % (gap, daic))
    m5 = [i for i in sd.index if i[2] == "M5"]
    check("H3 in Model 5, 'squared deviation' (Fay eq. 4) fits worse than the other two in every scenario",
          bool(((sd.loc[m5].AIC - ms.loc[m5].AIC) > 2).all() and ((sd.loc[m5].AIC - sm.loc[m5].AIC) > 2).all()))
    m3s = [i for i in sd.index if i[2] == "M3" and i[0] in ("slope (+0.5)", "intercept, slope, shape (+0.5)") and i[1] == "complete"]
    print("   note: in Model 3 under age-dependent selection, 'squared deviation' fits far better (dAIC %s): (age - mean)^2 contains -2 age x mean age,"
          " an age-by-mean-age interaction, so it partly models age-dependent selective disappearance that Model 3 otherwise omits" %
          ", ".join("%.0f" % (ms.loc[i].AIC - sd.loc[i].AIC) for i in m3s))
print("\n%d failure(s)%s" % (len(fails), (": " + "; ".join(fails)) if fails else ""))
