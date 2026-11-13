"""Grid decomposition (0.9.14 algorithm), transcribed from R/disappearance.R."""
import numpy as np, pandas as pd

def id_age_means(dat):
    return dat.groupby(["id", "age"], as_index=False).trait.mean()

def infer_step(age, ids):
    a = np.asarray(age, float); g = np.asarray(ids)
    o = np.lexsort((a, g)); a2, g2 = a[o], g[o]
    d = np.diff(a2)[g2[1:] == g2[:-1]]; d = d[d > 0]
    if not len(d): return np.nan
    d = np.array([float(f"{x:.6g}") for x in d])
    vals, cnt = np.unique(d, return_counts=True); share = cnt / len(d)
    common = vals[share >= 0.05]
    if not len(common): return float(np.median(d))
    step = common.min(); dom = vals[np.argmax(share)]
    if share.max() >= 0.6 and step / dom >= 0.75 and abs(dom / step - round(dom / step)) > 0.01:
        step = dom
    return float(step)

def decomposition(dat, step=None):
    """Port of the 0.9.14 decomposition_trajectory()."""
    ia = id_age_means(dat)
    if len(ia) < 2: return pd.DataFrame(), ""
    if step is None: step = infer_step(ia.age.values, ia.id.values)
    if not np.isfinite(step) or step <= 0: return pd.DataFrame(), ""
    ia = ia.sort_values("age")
    ages = np.sort(ia.age.unique())
    occ_id = np.cumsum(np.r_[True, np.diff(ages) > 0.25 * step])
    occ_age = pd.Series(ages).groupby(occ_id).mean().values
    merged = bool((pd.Series(occ_id).value_counts() > 1).any())
    ia = ia.assign(occ=pd.Series(occ_id, index=ages).reindex(ia.age).values)
    ia = ia.groupby(["id", "occ"], as_index=False).trait.mean()
    k = len(occ_age)
    if k < 2: return pd.DataFrame(), ""
    inc = np.full(k - 1, np.nan); npair = np.zeros(k - 1, int); anchor = np.full(k - 1, np.nan)
    for j in range(k - 1):
        gap = occ_age[j + 1] - occ_age[j]
        if not (0.5 * step <= gap < 1.5 * step): continue
        a = ia[ia.occ == j + 1].set_index("id").trait
        b_ = ia[ia.occ == j + 2].set_index("id").trait
        both = a.index.intersection(b_.index)
        if not len(both): continue
        ta, tb = a[both].values, b_[both].values
        ok = np.isfinite(ta) & np.isfinite(tb)
        if not ok.any(): continue
        inc[j] = float(np.mean(tb[ok] - ta[ok])); npair[j] = int(ok.sum()); anchor[j] = float(np.mean(ta[ok]))
    if not np.isfinite(inc).any(): return pd.DataFrame(), ""
    rows = []; seg = 0; j = 0
    while j <= k - 2:
        if not np.isfinite(inc[j]): j += 1; continue
        seg += 1; cur = anchor[j]
        rows.append((occ_age[j], cur, npair[j], seg))
        while j <= k - 2 and np.isfinite(inc[j]):
            cur += inc[j]
            rows.append((occ_age[j + 1], cur, npair[j], seg))
            j += 1
    out = pd.DataFrame(rows, columns=["age", "fitted", "n_pairs", "segment"])
    cau = []
    if merged: cau.append("ages merged into occasions")
    if seg > 1: cau.append(f"{seg} segments")
    return out, "; ".join(cau)
