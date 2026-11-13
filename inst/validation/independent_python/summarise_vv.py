import numpy as np, pandas as pd
d = pd.read_csv("../results/vv_replicates.csv.gz")
M = ["M1", "M2", "M3", "M4", "M5", "M6"]; rows = []
B = lambda x: x.map(lambda v: 1.0 if v in (True, "True", 1, 1.0) else (0.0 if v in (False, "False", 0, 0.0) else np.nan)).astype(float)
for cid, g in d.groupby("cell_id", sort=False):
    r = {"block": g.block.iloc[0], "selection": g.sd_type.iloc[0], "sampling": g.sampling.iloc[0], "reps": len(g), "errors": int((g.error.fillna("") != "").sum())}
    avail = [m for m in M if f"{m}_aic" in g and g[f"{m}_aic"].notna().any()]
    cand = [m for m in avail if m != "M6"]
    A = g[[f"{m}_aic" for m in cand]].values; low = A.min(axis=1, keepdims=True)
    win = pd.Series(np.array(cand)[A.argmin(axis=1)]).value_counts(normalize=True)
    for m in avail:
        r[f"{m}_mape"] = g[f"{m}_mape"].mean(); r[f"{m}_late"] = g[f"{m}_late"].mean()
        if f"{m}_cover" in g and g[f"{m}_cover"].notna().any(): r[f"{m}_cover"] = g[f"{m}_cover"].mean(); r[f"{m}_singular"] = B(g[f"{m}_singular"]).mean()
        if m in cand: r[f"{m}_lowest"] = win.get(m, 0.0); r[f"{m}_within2"] = np.mean(A[:, cand.index(m)] - low[:, 0] < 2)
    r["M4_wald_reject"] = (g["M4_wald_p"] < 0.05).mean() if "M4_wald_p" in g and g["M4_wald_p"].notna().any() else np.nan
    for k in ("decomp_mape", "decomp_vs_target", "observed_mape"): r[k] = g[k].mean()
    r["decomp_caution"] = B(g["decomp_caution"]).mean()
    rows.append(r)
S = pd.DataFrame(rows); S.to_csv("../results/vv_summary.csv", index=False)
pd.set_option("display.width", 250); pd.set_option("display.max_columns", 40)
f = lambda x: f"{x:5.1f}" if pd.notna(x) else "   - "
print("MEAN ABSOLUTE % DEVIATION FROM THE TRUE TRAJECTORY (lower is better); lowest-AIC shares among M1-M5; coverage of 95% CIs; M4 ALRxage Wald rejections")
print(f"{'block':22s} {'selection':11s} {'sampling':9s} | {'Obs':>5s} {'Deco':>5s} {'M1':>5s} {'M2':>5s} {'M3':>5s} {'M4':>5s} {'M5':>5s} {'M6':>5s} | lowest AIC (share)          | cov M4 cov M5 | Wald M4 | deco vs surv.")
for _, r in S.iterrows():
    wins = sorted([(r.get(f"{m}_lowest", 0), m) for m in ["M1", "M2", "M3", "M4", "M5"] if pd.notna(r.get(f"{m}_lowest", np.nan))], reverse=True)
    ws = ", ".join(f"{m} {100 * v:.0f}%" for v, m in wins if v >= 0.05)
    print(f"{r.block[:22]:22s} {r.selection:11s} {r.sampling:9s} | {f(r.observed_mape)} {f(r.decomp_mape)} " + " ".join(f(r.get(f'{m}_mape', np.nan)) for m in M)
          + f" | {ws:27s} | {f(100 * r.get('M4_cover', np.nan))}  {f(100 * r.get('M5_cover', np.nan))} | {f(100 * r.M4_wald_reject)}  | {f(r.decomp_vs_target)}")
print("errors:", int(S.errors.sum()), "of", int(S.reps.sum()), "replicates; singular fits (any model):", f"{np.nanmax(S[[c for c in S if c.endswith('_singular')]].values):.3f} max share")
