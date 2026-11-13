"""Runs the verification-and-validation matrix; appends one row per replicate to ../results/vv_replicates.csv."""
import sys, os, time, json, numpy as np, pandas as pd, warnings
warnings.filterwarnings("ignore")
from vv import one_rep
SD = ["none", "independent", "dependent", "both"]
def cells():
    out = []
    for sd in SD:
        for smp in ["complete", "mcar", "mwo", "mwy", "censored", "irregular"]:
            out.append(dict(block="core", sd_type=sd, sampling=smp, n_id=300, missingness=smp if smp in ("mcar", "mwo", "mwy") else "complete"))
    for sd in SD:
        for smp in ["complete", "mcar"]: out.append(dict(block="N=100", sd_type=sd, sampling=smp, n_id=100, missingness=smp))
        out.append(dict(block="subtle selection", sd_type=sd, sampling="complete", n_id=300, strength="subtle"))
        out.append(dict(block="high slope variance", sd_type=sd, sampling="complete", n_id=300, rate_var="high"))
        out.append(dict(block="Poisson counts", sd_type=sd, sampling="complete", n_id=300, count=True, reps=30))
    for sd in ["dependent", "both"]: out.append(dict(block="negative direction", sd_type=sd, sampling="complete", n_id=300, sd_dir=-1))
    for sd in ["none", "both"]:
        out.append(dict(block="linear ageing", sd_type=sd, sampling="complete", n_id=300, form="Linear"))
        out.append(dict(block="short lifespans (mean 8)", sd_type=sd, sampling="complete", n_id=300, mean_ls=8))
    return out
if __name__ == "__main__":
    which = sys.argv[1] if len(sys.argv) > 1 else "all"; reps = 100
    path = "../results/vv_replicates.csv"; os.makedirs("../results", exist_ok=True)
    M = ["M1", "M2", "M3", "M4", "M5", "M6"]
    COLS = (["cell_id", "block", "sd_type", "sampling", "seed", "error", "n_rows", "n_ids"] +
            [f"{m}_{k}" for m in M for k in ("aic", "mape", "bias", "late", "cover", "singular")] +
            ["M4_wald_p", "decomp_mape", "decomp_vs_target", "decomp_caution", "observed_mape"])
    done = set(pd.read_csv(path).cell_id.unique()) if os.path.exists(path) else set()
    t0 = time.time()
    for i, c in enumerate(cells()):
        cid = f"{c['block']} | {c['sd_type']} | {c['sampling']}"
        if cid in done or (which == "core" and c["block"] != "core") or (which == "extra" and c["block"] == "core"): continue
        rows = []
        for r in range(c.get("reps", reps)):
            try: o = one_rep(c, seed=1000 * i + r); o["error"] = ""
            except Exception as e: o = {"seed": 1000 * i + r, "error": f"{type(e).__name__}: {e}"}
            o.update(cell_id=cid, block=c["block"], sd_type=c["sd_type"], sampling=c["sampling"]); rows.append(o)
        pd.DataFrame(rows).reindex(columns=COLS).to_csv(path, mode="a", header=not os.path.exists(path), index=False)
        print(f"{cid:55s} {len(rows)} reps, errors {sum(1 for x in rows if x['error'])} ({time.time() - t0:.0f}s)", flush=True)
