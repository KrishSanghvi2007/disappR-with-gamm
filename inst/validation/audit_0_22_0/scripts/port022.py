"""Faithful Python ports of the disappR 0.22.0 engine code touched or added in 0.22.0, written line by line from the R
source so that they can be run on real and simulated data without R. Thresholds are read from the R source where the
code states them, so a change in R that is not mirrored here makes the checks fail rather than pass silently."""
import os, re, numpy as np, pandas as pd

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..", ".."))
SRC = {f: open(os.path.join(ROOT, "R", f), encoding="utf-8").read() for f in os.listdir(os.path.join(ROOT, "R")) if f.endswith(".R")}

def r_body(name):
    """Source text of an R function in the engine (for assertions that the port matches the code)."""
    for f, s in SRC.items():
        m = re.search(r"\n%s <- function\(" % re.escape(name), s)
        if m:
            i = s.index("{", m.end()); d = 0
            for j in range(i, len(s)):
                d += (s[j] == "{") - (s[j] == "}")
                if d == 0: return s[m.start():j + 1]
    raise KeyError(name)

# thresholds asserted against the R source
_INF = r_body("infer_age_step")
assert "share >= 0.05" in _INF and "max(share) >= 0.6" in _INF and "step / dom >= 0.75" in _INF
_DEC = r_body("decomposition_trajectory")
assert "abs(kk - round(kk)) <= 0.25" in _DEC and "diff(ages) > 0.25 * step" in _DEC and "gap >= 0.5 * step && gap < 1.5 * step" in _DEC
assert "abs(step - inferred) <= 1e-8 * max(1, step)" in _DEC and "* 20) / 20) %% 1" in _DEC and "ia$age_real" in _DEC
_CIF = r_body("compare_individual_functions")
assert "1 / n_tied" in _CIF
_TP = r_body("curve_turning_points")
assert "1e-10 * max(1, diff(range(y)))" in _TP

def signif(x, d=6):
    return np.array([float("%.*g" % (d, v)) for v in np.atleast_1d(x)])

def valid_age_step(step):
    return step is not None and np.isscalar(step) and np.isfinite(step) and step > 0

def infer_age_step(age, ids=None):
    age = np.asarray(age, float)
    ok = np.isfinite(age)
    if ids is not None:
        ids = np.asarray(ids).astype(str); ok &= ids != "nan"
    if ok.sum() < 2: return np.nan
    a = age[ok]
    tol = 1e-6 * max(1.0, np.ptp(a))
    d = np.array([])
    if ids is not None:
        g = ids[ok]; o = np.lexsort((a, g)); a2 = a[o]; g2 = g[o]
        same = g2[1:] == g2[:-1]
        d = np.diff(a2)[same]
    d = d[np.isfinite(d) & (d > tol)]
    if not len(d):
        u = np.unique(a); d = np.diff(u); d = d[d > tol]
        if not len(d): return np.nan
    d = signif(d, 6)
    vals, cnt = np.unique(d, return_counts=True)
    share = cnt / len(d)
    common = vals[share >= 0.05]
    if not len(common): return float(np.median(d))
    step = common.min(); dom = vals[np.argmax(share)]
    if share.max() >= 0.6 and step / dom >= 0.75 and abs(dom / step - np.round(dom / step)) > 0.01: step = dom
    return float(step)

def resolve_age_step(age, ids=None, step=None):
    return float(step) if valid_age_step(step) else infer_age_step(age, ids)

def id_age_means(d):
    z = d[np.isfinite(d["trait"]) & np.isfinite(d["age"])][["id", "age", "trait"]]
    if not len(z): return z
    return z.groupby(["id", "age"], as_index=False)["trait"].mean().sort_values(["id", "age"])

def decomposition_trajectory(d, step=None):
    ia = id_age_means(d)
    empty = dict(rows=pd.DataFrame(columns=["age", "fitted", "n_pairs", "segment"]), segments=0, n_off=0, merged=False, step=np.nan)
    if len(ia) < 2: return empty
    inferred = infer_age_step(ia["age"].values, ia["id"].values)
    manual = valid_age_step(step) and not (np.isfinite(inferred) and abs(step - inferred) <= 1e-8 * max(1, step))
    step = resolve_age_step(ia["age"].values, ia["id"].values, step)
    if not np.isfinite(step) or step <= 0: return empty
    n_off = 0
    if manual:
        ph = (np.round(((ia["age"].values / step) % 1) * 20) / 20) % 1
        vals, cnt = np.unique(ph, return_counts=True); a0 = vals[np.argmax(cnt)] * step
        kk = (ia["age"].values - a0) / step
        on = np.abs(kk - np.round(kk)) <= 0.25
        n_off = int((~on).sum())
        ia = ia[on].copy(); ia["age_real"] = ia["age"]; ia["age"] = a0 + np.round(kk[on]) * step
        if len(ia) < 2: return dict(empty, n_off=n_off)
    ia = ia.sort_values("age", kind="stable")
    ages = np.unique(ia["age"].values)
    occ_id = np.cumsum(np.r_[True, np.diff(ages) > 0.25 * step])
    occ_age = np.array([ages[occ_id == o].mean() for o in np.unique(occ_id)])
    merged = bool(np.any(np.bincount(occ_id)[1:] > 1))
    ia["occ"] = occ_id[np.searchsorted(ages, ia["age"].values)]
    if manual: occ_age = ia.groupby("occ")["age_real"].mean().reindex(range(1, len(occ_age) + 1)).values
    ia = ia.groupby(["id", "occ"], as_index=False)["trait"].mean()
    k = len(occ_age)
    if k < 2: return dict(empty, n_off=n_off)
    by = {o: dict(zip(g["id"], g["trait"])) for o, g in ia.groupby("occ")}
    inc = np.full(k - 1, np.nan); anchor = np.full(k - 1, np.nan); npair = np.zeros(k - 1, int)
    for j in range(k - 1):
        gap = occ_age[j + 1] - occ_age[j]
        if not (gap >= 0.5 * step and gap < 1.5 * step): continue
        a = by.get(j + 1); b = by.get(j + 2)
        if a is None or b is None: continue
        both = [i for i in a if i in b]
        if not both: continue
        ta = np.array([a[i] for i in both]); tb = np.array([b[i] for i in both])
        ok = np.isfinite(ta) & np.isfinite(tb)
        if not ok.any(): continue
        inc[j] = np.mean(tb[ok] - ta[ok]); npair[j] = ok.sum(); anchor[j] = np.mean(ta[ok])
    if not np.isfinite(inc).any(): return dict(empty, n_off=n_off, step=step)
    out = []; seg = 0; j = 0
    while j <= k - 2:
        if not np.isfinite(inc[j]): j += 1; continue
        seg += 1; cur = anchor[j]; out.append((occ_age[j], cur, npair[j], seg))
        while j <= k - 2 and np.isfinite(inc[j]):
            cur += inc[j]; out.append((occ_age[j + 1], cur, npair[j], seg)); j += 1
    return dict(rows=pd.DataFrame(out, columns=["age", "fitted", "n_pairs", "segment"]), segments=seg, n_off=n_off, merged=merged, step=step)

def missing_grid(d, step=None, max_cells=300000):
    """Core of build_missing_grid() with margin 0 and the AFR start: returns the step used and the share of expected
    occasions without a trait value."""
    d = d[np.isfinite(d["age"])]
    has = d.groupby("id")["trait"].apply(lambda v: np.isfinite(v).any())
    d = d[d["id"].isin(has.index[has])]
    if not len(d): return dict(step=np.nan, missing=np.nan, cells=0)
    step0 = resolve_age_step(d["age"].values, d["id"].values, step)
    if not np.isfinite(step0) or step0 <= 0: step0 = 1.0
    ids = pd.unique(d["id"]); pos = {v: i for i, v in enumerate(ids)}; fi = d["id"].map(pos).values
    first_age = d.groupby("id")["age"].min().reindex(ids).values
    ft = d[np.isfinite(d["trait"])].groupby("id")["age"].min().reindex(ids).values
    amax = d["age"].max()
    def build(st):
        n_before = np.maximum(0, np.floor((first_age - ft) / st + 1e-9))
        anchor = first_age - n_before * st
        k_rec = np.round((d["age"].values - anchor[fi]) / st)
        last_k = pd.Series(k_rec).groupby(fi).max().reindex(range(len(ids))).values
        end_k = np.minimum(last_k, np.floor((amax - anchor) / st + 1e-9)); end_k = np.maximum(end_k, last_k)
        first_k = np.round((ft - anchor) / st)
        start_k = np.maximum.reduce([np.zeros(len(ids)), first_k, end_k - 4999])
        return anchor, k_rec, start_k, end_k, np.maximum(1, end_k - start_k + 1).astype(int)
    st = step0; anchor, k_rec, start_k, end_k, n_cells = build(st); coarsen = 1
    while n_cells.sum() > max_cells and coarsen < 1e6:
        coarsen *= max(2, int(np.ceil(n_cells.sum() / max_cells))); st = step0 * coarsen
        anchor, k_rec, start_k, end_k, n_cells = build(st)
    okt = np.isfinite(d["trait"].values)
    filled = set(zip(fi[okt], k_rec[okt]))
    total = int(n_cells.sum())
    have = sum(1 for (i, k) in filled if start_k[i] <= k <= end_k[i])
    return dict(step=st, step_requested=step0, missing=1 - have / total, cells=total)

def curve_turning_points(age, y):
    age = np.asarray(age, float); y = np.asarray(y, float)
    ok = np.isfinite(age) & np.isfinite(y); age = age[ok]; y = y[ok]
    o = np.argsort(age, kind="stable"); age = age[o]; y = y[o]; n = len(y)
    if n < 3: return dict(peak=np.nan, peak_interior=False, onset=np.nan, onset_type="none", trough=np.nan, trough_interior=False)
    tol = 1e-10 * max(1.0, np.ptp(y))
    i_max = int(np.argmax(y)); i_min = int(np.argmin(y)); dy = np.diff(y)
    if dy[n - 2] < -tol:
        k = n - 1
        while k > 1 and dy[k - 2] < -tol: k -= 1
        onset = age[k - 1]; onset_type = "from_start" if k == 1 else "interior"
    else:
        onset = np.nan; onset_type = "no_final_decline"
    return dict(peak=age[i_max], peak_interior=0 < i_max < n - 1, onset=onset, onset_type=onset_type,
                trough=age[i_min], trough_interior=0 < i_min < n - 1)

def stored_set_labels(store):
    grp = ["|".join([str(e["data_sig"]), str(e["rows"]), e["family_class"]]) for e in store]
    uniq = list(dict.fromkeys(grp)); k = [uniq.index(g) + 1 for g in grp]
    return [chr(64 + i) if i <= 26 else "S%d" % i for i in k]

def stored_comparison_table(store):
    if not store: return pd.DataFrame()
    st = stored_set_labels(store); aic = np.array([e["AIC"] for e in store], float); d = np.full(len(aic), np.nan)
    for g in dict.fromkeys(st):
        k = [i for i, s in enumerate(st) if s == g and np.isfinite(aic[i])]
        if k: d[k] = aic[k] - aic[k].min()
    out = pd.DataFrame(dict(ID=[e["id"] for e in store], Set=st, AIC=np.round(aic, 1), dAIC=np.round(d, 1)))
    return out.sort_values(["Set", "dAIC", "AIC"], na_position="last").reset_index(drop=True)

def share_best(per, funs):
    """per: DataFrame id, Function, AICc. Mirrors the common-set and share-of-wins code of compare_individual_functions()."""
    share_fin = {f: np.mean(np.isfinite(per.loc[per.Function == f, "AICc"])) for f in funs}
    comparable = [f for f in funs if share_fin[f] >= 0.5]
    if len(comparable) < 2: comparable = list(funs)
    fin = per[np.isfinite(per.AICc) & per.Function.isin(comparable)]
    nf = fin.groupby("id")["Function"].nunique()
    common = list(nf.index[nf == len(comparable)])
    cc = per.id.isin(common) & per.Function.isin(comparable)
    per = per.copy(); per["Delta"] = np.nan
    per.loc[cc, "Delta"] = per[cc].groupby("id")["AICc"].transform(lambda v: v - v.min())
    sub = per[cc].copy(); sub["best"] = (sub["Delta"] <= 1e-9).astype(float)
    sub["credit"] = sub["best"] / sub.groupby("id")["best"].transform("sum")
    wins = sub.groupby("Function")["credit"].sum()
    n_common = len(common); out = {}
    for f in funs:
        if n_common > 0 and f in wins.index: out[f] = round(100 * wins[f] / n_common, 1)
        elif n_common > 0 and f in comparable: out[f] = 0.0
        else: out[f] = np.nan
    return out, comparable, n_common
