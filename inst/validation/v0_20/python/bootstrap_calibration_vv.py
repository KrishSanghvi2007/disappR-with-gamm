"""Calibration of two null distributions for the lifespan-term test (disappR 0.20.0).

Compares, on simulated Gaussian data with a known answer, the permutation null used up to 0.19.6 (ALR shuffled
between individuals) with the parametric-bootstrap null that replaces it in 0.20.0 (responses simulated from a
lifespan-free model that carries the nuisance structure - a cubic ageing curve and correlated random intercepts
and slopes - with every individual's ages and ALR held fixed).

The test statistic is the one the app reports: the AIC advantage of Model 4 over Model 1, both fitted with the
analyst's specification (here: linear ageing, random intercept only, maximum likelihood).

Evidence type: independent implementation (numpy/scipy). The mixed model is the ML LMM of ../extended/emu.py,
already checked against dense likelihoods. The R package is NOT executed, so agreement supports the method, not
the R code; tests/testthat/test-nulltest.R checks the R code.

Usage:  python3 bootstrap_calibration_vv.py [reps_per_scenario] [n_boot]
"""
import os, sys, time
import numpy as np, pandas as pd
from scipy.stats import chi2

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "..", "extended"))
from harness import simulate  # noqa: E402
from emu import LMM           # noqa: E402

REPS = int(sys.argv[1]) if len(sys.argv) > 1 else 80
B = int(sys.argv[2]) if len(sys.argv) > 2 else 39
N_ID, MEAN_LS = 100, 10

SCENARIOS = [
    # name, simulator settings, analyst's ageing function, what a valid test should do
    ("clean null", dict(fun="linear", sd="none"), "Linear", "reject at the nominal rate"),
    ("misspecified ageing function, no selection", dict(fun="quadratic", sd="none"), "Linear", "reject at the nominal rate"),
    ("heterogeneous ageing rates, no selection", dict(fun="linear", sd="none", slope_sd=0.12), "Linear", "reject at the nominal rate"),
    ("rate-linked selection (strength 1)", dict(fun="linear", sd="rate", strength=1.0), "Linear", "reject often (power)"),
    ("rate-linked selection (strength 0.5)", dict(fun="linear", sd="rate", strength=0.5), "Linear", "reject often (power)"),
    ("misspecified function and heterogeneous rates, no selection", dict(fun="quadratic", sd="none", slope_sd=0.12), "Linear", "reject at the nominal rate"),
]


def basis(z, fun):
    return np.column_stack([np.ones_like(z), z] + ([z ** 2] if fun == "Quadratic" else []))


def gain(y, ids, z, za, fun, st="none"):
    X1 = basis(z, fun)
    X4 = np.column_stack([X1, za[:, None] * X1])      # (ageing terms) * ALR, main effects included
    a1 = LMM(y, X1, ids, z, st).fit().aic
    a4 = LMM(y, X4, ids, z, st).fit().aic
    return a1 - a4, X4.shape[1] - X1.shape[1]


def one(df, fun, rng):
    ids = df.id.values
    age = df.age.values.astype(float)
    y = df.trait.values.astype(float)
    z = (age - age.mean()) / age.std(ddof=1)
    alr_i = df.groupby("id").age.max()
    za_i = (alr_i - alr_i.mean()) / alr_i.std(ddof=1)
    za = df.id.map(za_i).values.astype(float)
    obs, dk = gain(y, ids, z, za, fun)
    # permutation null: ALR shuffled between individuals (Model 1 does not change)
    X1 = basis(z, fun)
    a1 = LMM(y, X1, ids, z, "none").fit().aic
    perm = np.empty(B)
    for b in range(B):
        m = pd.Series(rng.permutation(za_i.values), index=za_i.index)
        zp = df.id.map(m).values.astype(float)
        perm[b] = a1 - LMM(y, np.column_stack([X1, zp[:, None] * X1]), ids, z, "none").fit().aic
    # parametric-bootstrap nulls: lifespan-free Model 1 with a cubic curve, design fixed. 'uncorrelated' is what
    # bootstrap_test() uses when the analysis has a random intercept only (0.20.1+); 'correlated' when the analysis
    # uses correlated slopes (and what the 0.20.0 draft used throughout).
    Xn = np.column_stack([np.ones_like(z), z, z ** 2, z ** 3])
    uniq, inv = np.unique(ids, return_inverse=True)
    pvals = {}
    for st in ("uncorrelated", "correlated"):
        nul = LMM(y, Xn, ids, z, st).fit()
        L = nul._L(nul.theta)
        boot = np.empty(B)
        for b in range(B):
            u = np.sqrt(nul.s2) * (rng.standard_normal((len(uniq), 2)) @ L.T)
            ys = Xn @ nul.beta + u[inv, 0] + u[inv, 1] * z + rng.normal(0.0, np.sqrt(nul.s2), len(y))
            boot[b] = gain(ys, ids, z, za, fun)[0]
        pvals[st] = (1 + np.sum(boot >= obs)) / (1 + B)
    lrt = obs + 2 * dk
    return dict(observed_gain=obs, aic_prefers_m4=obs > 2, lrt_p=chi2.sf(lrt, dk),
                p_permutation=(1 + np.sum(perm >= obs)) / (1 + B),
                p_boot_uncorrelated=pvals["uncorrelated"], p_boot_correlated=pvals["correlated"])


def main():
    # resumable: scenarios already in the replicates file are kept, so the study can run in pieces
    # (python3 bootstrap_calibration_vv.py 60 39 [first_scenario] [last_scenario])
    out_rows = os.path.join(HERE, "bootstrap_calibration_replicates.csv")
    rows, t0 = [], time.time()
    if os.path.exists(out_rows):
        rows = pd.read_csv(out_rows).to_dict("records")
    have = {r["scenario"] for r in rows}
    lo = int(sys.argv[3]) if len(sys.argv) > 3 else 0
    hi = int(sys.argv[4]) if len(sys.argv) > 4 else len(SCENARIOS) - 1
    for si, (name, sim, fun, expect) in enumerate(SCENARIOS):
        if name in have or not (lo <= si <= hi):
            continue
        for r in range(REPS):
            seed = 7000 + 1000 * si + r
            df = simulate(n=N_ID, mean_ls=MEAN_LS, seed=seed, **sim)
            res = one(df, fun, np.random.default_rng(seed + 17))
            res.update(scenario=name, rep=r, seed=seed, analyst_function=fun, expectation=expect)
            rows.append(res)
        pd.DataFrame(rows).to_csv(out_rows, index=False)
        print(f"{name}: done ({time.time() - t0:.0f} s)", flush=True)
    d = pd.DataFrame(rows)
    if set(d.scenario) != {n for n, _, _, _ in SCENARIOS}:
        print("partial run: summary written when every scenario is done", flush=True)
        return
    alpha_boot = 1.0 / (1 + B)          # with B draws, p < 0.05 means 'beats every draw' when B < 39
    s = d.groupby("scenario", sort=False).agg(
        datasets=("rep", "size"),
        aic_prefers_model4=("aic_prefers_m4", "mean"),
        lrt_rejects=("lrt_p", lambda p: np.mean(p < 0.05)),
        permutation_rejects=("p_permutation", lambda p: np.mean(p < 0.05)),
        bootstrap_uncorrelated_rejects=("p_boot_uncorrelated", lambda p: np.mean(p < 0.05)),
        bootstrap_correlated_rejects=("p_boot_correlated", lambda p: np.mean(p < 0.05)))
    s = s.reindex([n for n, _, _, _ in SCENARIOS])
    s.insert(1, "expectation", [e for (n, _, _, e) in SCENARIOS])
    s.to_csv(os.path.join(HERE, "bootstrap_calibration_summary.csv"))
    with open(os.path.join(HERE, "bootstrap_calibration_summary.md"), "w") as fh:
        fh.write(f"N = {N_ID} individuals, mean lifespan {MEAN_LS}, {REPS} datasets per scenario, {B} null draws per "
                 f"dataset; the nominal rejection rate of a valid test is {1 / (1 + B):.3f} "
                 f"(p < 0.05 requires beating all {B} draws).\n\n")
        fh.write(s.to_markdown(floatfmt=".3f") if hasattr(s, "to_markdown") else s.to_string())
        fh.write("\n")
    print(s.to_string())


if __name__ == "__main__":
    main()
