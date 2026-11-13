"""Held-out validation: scenarios and seeds never used during development, with expectations fixed in advance
(from the development results) and checked afterwards. Independent implementation; the R app is not executed."""
import os, time, numpy as np, pandas as pd, warnings
warnings.filterwarnings("ignore")
from vv import one_rep
CELLS = [  # new shapes, sample sizes, lifespans and combinations; seeds start at 900000 (development seeds are below 60000)
    dict(name="steep peak, N 200, lifespan 12, age-dependent SD, complete", sd_type="dependent", sampling="complete", n_id=200, mean_ls=12, mu=[40, 3, -0.1]),
    dict(name="steep peak, N 200, lifespan 12, no SD, MCAR", sd_type="none", sampling="mcar", missingness="mcar", n_id=200, mean_ls=12, mu=[40, 3, -0.1]),
    dict(name="late peak, N 500, lifespan 25, both SD, missing when old", sd_type="both", sampling="mwo", missingness="mwo", n_id=500, mean_ls=25, mu=[40, 1, -0.04]),
    dict(name="late peak, N 150, age-dependent SD, censored", sd_type="dependent", sampling="censored", n_id=150, mu=[40, 1, -0.04]),
    dict(name="mid peak, lifespan 16, age-independent SD, irregular ages", sd_type="independent", sampling="irregular", n_id=300, mean_ls=16, mu=[35, 2.5, -0.08]),
    dict(name="mid peak, lifespan 16, both SD, missing when young", sd_type="both", sampling="mwy", missingness="mwy", n_id=300, mean_ls=16, mu=[35, 2.5, -0.08]),
    dict(name="subtle SD, high slope variance, age-dependent, MCAR", sd_type="dependent", sampling="mcar", missingness="mcar", n_id=300, strength="subtle", rate_var="high"),
    dict(name="negative direction, age-independent SD, censored", sd_type="independent", sampling="censored", n_id=250, sd_dir=-1),
    dict(name="Poisson counts, age-dependent SD, MCAR", sd_type="dependent", sampling="mcar", missingness="mcar", n_id=300, count=True, reps=40),
    dict(name="linear senescence, N 400, lifespan 6, both SD, complete", sd_type="both", sampling="complete", n_id=400, mean_ls=6, form="Linear", mu=[50, -1.0])]
def expectations(c, s):
    """Fixed before running (see VALIDATION.md): returns (label, passed) pairs."""
    out, M = [], [m for m in ["M1", "M2", "M3", "M4", "M5"] if f"{m}_mape" in s]
    if c["sd_type"] in ("none", "independent") and not c.get("count"):
        out.append(("E1 no age-dependent selection: every model within 2%", max(s[f"{m}_mape"] for m in M) < 2))
    if c["sd_type"] in ("dependent", "both"):
        lim = 6 if c.get("count") else 3
        out.append((f"E2 age-dependent selection: Model 4 within {lim}% and closer than Model 1", s["M4_mape"] < lim and s["M4_mape"] < s["M1_mape"]))
        if c["sampling"] in ("complete", "mcar") and not c.get("count"):
            out.append(("E3 Models 4 or 5 have the lowest AIC in at least 80% of datasets", s["M45_lowest"] >= 0.8))
    if c["sampling"] in ("complete", "mcar", "mwo") and not c.get("count"):
        out.append(("E4 decomposition within 2% of its survivor-restricted target", s["decomp_vs_target"] < 2))
    return out
rows, t0 = [], time.time()
for i, c in enumerate(CELLS):
    reps = [one_rep(c, seed=900000 + 1000 * i + r) for r in range(c.get("reps", 60))]
    d = pd.DataFrame(reps)
    s = {k: d[k].mean() for k in d.columns if k.endswith(("_mape", "_cover")) or k in ("decomp_vs_target", "decomp_mape", "observed_mape")}
    cand = [m for m in ["M1", "M2", "M3", "M4", "M5"] if f"{m}_aic" in d]
    A = d[[f"{m}_aic" for m in cand]].values; win = np.array(cand)[A.argmin(axis=1)]
    s["M45_lowest"] = np.mean(np.isin(win, ["M4", "M5"]))
    if "M4_wald_p" in d: s["M4_wald_reject"] = (d["M4_wald_p"] < 0.05).mean()
    ex = expectations(c, s)
    rows.append(dict(scenario=c["name"], reps=len(d), **{k: round(v, 3) for k, v in s.items()},
                     expectations="; ".join(f"{lab}: {'PASS' if ok else 'FAIL'}" for lab, ok in ex)))
    print(f"{c['name']:62s} " + " | ".join(f"{lab.split(' ')[0]} {'PASS' if ok else 'FAIL'}" for lab, ok in ex)
          + f" | M1 {s['M1_mape']:.1f}% M4 {s['M4_mape']:.1f}%" + (f" | deco-target {s['decomp_vs_target']:.1f}%" if 'decomp_vs_target' in s else "") + f" ({time.time() - t0:.0f}s)", flush=True)
os.makedirs("../results", exist_ok=True)
pd.DataFrame(rows).to_csv("../results/heldout_summary.csv", index=False)
