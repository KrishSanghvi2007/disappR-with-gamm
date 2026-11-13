"""Port of the pure helpers added in disappR 0.20.9-0.20.19, tested against expected behaviour.
Constants and regexes are pulled out of the R source so the port cannot drift from the code."""
import re, math, sys
import numpy as np
SRC = {}
for f in ["R/utils.R", "R/evidence.R", "R/model-comparison.R", "R/data-preparation.R", "R/formulas.R", "R/disappearance.R"]:
    SRC[f] = open("../../../" + f, encoding="utf-8").read()
fails, checks = [], 0
def ck(name, cond, extra=""):
    global checks
    checks += 1
    if not cond: fails.append(f"{name} {extra}")

# ---- 1. squish_to (0.20.13) : constants read from the R source
pad_default = float(re.search(r"squish_to <- function\(x, lim, pad = ([0-9.]+)\)", SRC["R/utils.R"]).group(1))
def squish_to(x, lim, pad=pad_default):
    x = np.asarray(x, dtype=float)
    if len(lim) != 2 or not all(np.isfinite(lim)): return x
    w = lim[1] - lim[0]
    if not np.isfinite(w) or w <= 0: w = max(abs(lim[0]), abs(lim[1]), 1)
    return np.minimum(np.maximum(x, lim[0] - pad * w), lim[1] + pad * w)
ck("squish pad default is 0.5", pad_default == 0.5)
ck("squish clamps both tails", list(squish_to([-1e300, 0, 5, 1e304, np.nan], [0, 10])[:4]) == [-5, 0, 5, 15])
ck("squish keeps NA", math.isnan(squish_to([np.nan], [0, 10])[0]))
ck("squish no-op without a window", list(squish_to([1, 2, 3], [np.nan, 1])) == [1, 2, 3])
ck("squish handles a zero-width window", np.isfinite(squish_to([1e300], [5, 5])).all() and squish_to([1e300], [5, 5])[0] == 7.5)
ck("squish bounds the worst case", abs(squish_to([1e304], [-2.3, 2.4])[0]) < 10)

# ---- 2. visual_gap_stats (0.20.7): weighted least squares of the gap on centred age
alpha = 0.05
def visual_gap_stats(age, diff, w=None):
    age, diff = np.asarray(age, float), np.asarray(diff, float)
    if len(diff) < 3 or len(set(age.tolist())) < 3: return None
    w = np.ones_like(age) if w is None or not np.all(np.isfinite(w)) or np.any(np.asarray(w) <= 0) else np.asarray(w, float)
    a = age - np.sum(w * age) / np.sum(w)
    X = np.column_stack([np.ones_like(a), a])
    W = np.diag(w)
    beta = np.linalg.solve(X.T @ W @ X, X.T @ W @ diff)
    resid = diff - X @ beta
    dof = len(diff) - 2
    s2 = float(resid @ W @ resid) / dof
    cov = s2 * np.linalg.inv(X.T @ W @ X)
    se = np.sqrt(np.diag(cov))
    from scipy import stats as st
    p = 2 * st.t.sf(np.abs(beta / se), dof)
    kind = "age_dependent" if p[1] < alpha else ("age_independent" if p[0] < alpha else "none")
    return dict(kind=kind, level=beta[0], trend=beta[1], p_level=p[0], p_trend=p[1])
g = visual_gap_stats(list(range(1, 9)) * 2, [0.1 * a for a in range(1, 9)] + [0.1 * a + 0.01 for a in range(1, 9)])
ck("gap: rising gap is age-dependent", g["kind"] == "age_dependent" and g["trend"] > 0)
flat = visual_gap_stats([1, 2, 3, 4] * 2, [0.5, 0.52, 0.48, 0.51, 0.49, 0.5, 0.53, 0.47])
ck("gap: constant non-zero gap is age-independent", flat["kind"] == "age_independent", str(flat))
noise = visual_gap_stats([1, 2, 3, 4, 5, 6], [0.4, -0.5, 0.3, -0.35, 0.45, -0.4])
ck("gap: noise around zero is none", noise["kind"] == "none", str(noise))
ck("gap: too few ages returns nothing", visual_gap_stats([1, 1, 2], [1, 2, 3]) is None)
wtd = visual_gap_stats([1, 2, 3, 4, 5], [0, 0, 0, 0, 10], w=[1, 1, 1, 1, 1e-6])
ck("gap: a tiny weight barely moves the trend", abs(wtd["trend"]) < 0.01, str(wtd))

# ---- 3. negligible_random_terms (0.20.10)
share_min, sd_min = [float(x) for x in re.search(r"negligible_random_terms <- function\(vc, share_min = ([0-9.]+), sd_min = ([0-9.]+)\)", SRC["R/model-comparison.R"]).groups()]
def negligible(groups, types, variances):
    n = len(groups)
    flag, share, sd = [False] * n, [None] * n, [None] * n
    sd_rows = [i for i in range(n) if types[i] == "SD" and groups[i] != "Residual" and variances[i] is not None and np.isfinite(variances[i])]
    if not sd_rows: return flag, share
    for i in sd_rows: sd[i] = math.sqrt(max(variances[i], 0))
    res = [i for i in range(n) if types[i] == "SD" and groups[i] == "Residual" and variances[i] is not None]
    if res:
        tot = sum(variances[i] for i in sd_rows + res)
        if tot > 0:
            for i in sd_rows:
                share[i] = variances[i] / tot
                flag[i] = share[i] < share_min
    else:
        for i in sd_rows: flag[i] = sd[i] < sd_min
    return flag, share
ck("negligible thresholds are 1% and 0.05", (share_min, sd_min) == (0.01, 0.05))
f, s = negligible(["id", "id", "id", "year", "Residual"], ["SD", "SD", "Correlation", "SD", "SD"], [2, 0, None, 0.01, 3])
ck("negligible: zero and 0.2% flagged, residual never", f == [False, True, False, True, False], str(f))
f2, _ = negligible(["id", "year"], ["SD", "SD"], [0.3, 0.001])
ck("negligible: link scale uses the SD rule", f2 == [False, True])
f3, _ = negligible(["id"], ["SD"], [0.0])
ck("negligible: a single zero-variance term with no residual is flagged", f3 == [True])

# ---- 4. key_age_proxy_terms (0.20.10): the regex itself comes from the source
rx = re.search(r'part_ok <- function\(p\) grepl\("(.+?)", p\)', SRC["R/model-comparison.R"]).group(1)
rx_py = re.compile(rx.replace("\\\\", "\\"))
def key_terms(raw): return [all(rx_py.match(p) for p in t.split(":")) for t in raw]
exp = [False, True, True, True, True, True, True, True, True, True, True, False, False, True]
got = key_terms(["(Intercept)", "f1", "f2", "ALR", "f1:ALR", "ALR:f2", "mean_f1", "delta_f1", "mean_f1:delta_f1", "AFR", "LS:f1", "cv_sex", "cv_sex:f1", "ALR2"])
ck("bold terms: age and proxy terms only", got == exp, str(got))

# ---- 5. duplicate_report (0.20.18)
def duplicate_report(rows, cols):
    key = [f"{r['id']}\r{r['age']}" for r in rows]
    dup_keys = sorted({k for k in key if key.count(k) > 1}, key=key.index)
    ident, differ, diff_cols = [], [], []
    for k in dup_keys:
        grp = [rows[i] for i in range(len(rows)) if key[i] == k]
        same = {c: len({str(r[c]) for r in grp}) == 1 for c in cols}
        (ident if all(same.values()) else differ).append(k)
        if not all(same.values()): diff_cols += [c for c in cols if not same[c]]
    return ident, differ, sorted(set(diff_cols))
rows = [dict(id="A", age=1, trait=5, cv="x"), dict(id="A", age=1, trait=5, cv="x"),
        dict(id="B", age=2, trait=7, cv="x"), dict(id="B", age=2, trait=9, cv="x"),
        dict(id="C", age=3, trait=4, cv="y"), dict(id="D", age=1, trait=2, cv="x"), dict(id="D", age=1, trait=2, cv="z")]
ident, differ, dcols = duplicate_report(rows, ["id", "age", "trait", "cv"])
ck("duplicates: exact pair collapses", ident == ["A\r1"], str(ident))
ck("duplicates: different trait kept", "B\r2" in differ)
ck("duplicates: different covariate kept", "D\r1" in differ)
ck("duplicates: differing columns named", dcols == ["cv", "trait"], str(dcols))

# ---- 6. multi_group_report (0.20.19)
def multi_group(idv, grp):
    g = ["(no group)" if x is None or x == "" else x for x in grp]
    ids = sorted({i for i in set(idv) if len({g[k] for k in range(len(idv)) if idv[k] == i}) > 1})
    ex = [f"{i} ({', '.join(sorted({g[k] for k in range(len(idv)) if idv[k] == i}))})" for i in ids[:6]]
    n_gap = sum(1 for i in ids if any(g[k] == "(no group)" for k in range(len(idv)) if idv[k] == i))
    return ids, ex, n_gap
ids, ex, gap = multi_group(["A", "A", "B", "B", "C", "C"], ["G1", "G2", "G1", "G1", "G1", None])
ck("multi-group: ids named", ids == ["A", "C"] and gap == 1, str((ids, gap)))
ck("multi-group: groups listed", ex == ["A (G1, G2)", "C ((no group), G1)"], str(ex))

# ---- 7. model_set_reading (0.20.17) and model_process_kind (0.20.18)
def model_set_reading(best, supported, dfs=None, next_model=None, gap_next=None):
    sup = supported or [best]
    if len(sup) <= 1:
        return True, f"{best} best; " + (f"the next model, {next_model}, is {gap_next:.1f} AIC behind" if next_model and gap_next is not None else "no other model is within 2 AIC")
    others = [m for m in sup if m != best]
    simplest = None
    if dfs and all(m in dfs for m in sup):
        lo = min(dfs[m] for m in sup)
        simplest = " or ".join([m for m in sup if dfs[m] == lo])
    return False, f"{best} best, but {', '.join(others)} {'are' if len(others) > 1 else 'is'} within 2 AIC of it, so the supported set is {', '.join(sup)}. Parsimony suggests interpreting the simplest{', ' + simplest if simplest else ' of them'}."
clear, note = model_set_reading("Model 4", ["Model 4"], next_model="Model 5", gap_next=3.14)
ck("winner: clear case ticks", clear and "3.1 AIC behind" in note, note)
clear2, note2 = model_set_reading("Model 4", ["Model 4", "Model 5", "Model 2"], {"Model 4": 7, "Model 5": 7, "Model 2": 6})
ck("winner: ambiguous case names the set", (not clear2) and "supported set is Model 4, Model 5, Model 2" in note2, note2)
ck("winner: simplest named", "simplest, Model 2." in note2, note2)
clear3, note3 = model_set_reading("Model 4", ["Model 4", "Model 5"], {"Model 4": 7, "Model 5": 7})
ck("winner: ties name both", "Model 4 or Model 5" in note3, note3)
meaning = {}
for m in re.finditer(r'(M\d+) = list\(name = "[^"]*", proxy = "[^"]*", dis = "([^"]*)", app = "([^"]*)"', SRC["R/formulas.R"]):
    meaning[m.group(1)] = (m.group(2), m.group(3))
def kind_of(v): return "age_dependent" if v.startswith("age-dependent") else ("age_independent" if v.startswith("age-independent") else "none")
ck("all ten models carry a meaning", len(meaning) == 10, str(sorted(meaning)))
want = {"M1": ("none", "none"), "M2": ("age_independent", "none"), "M3": ("age_independent", "none"),
        "M4": ("age_dependent", "none"), "M5": ("age_dependent", "none"), "M6": ("age_dependent", "none"),
        "M7": ("age_independent", "age_independent"), "M8": ("age_dependent", "age_dependent"),
        "M9": ("age_dependent", "age_independent"), "M10": ("age_independent", "age_dependent")}
got = {k: (kind_of(v[0]), kind_of(v[1])) for k, v in meaning.items()}
ck("each model's structural reading matches its formula", got == want, str({k: v for k, v in got.items() if want.get(k) != v}))

# ---- 8. the LRT pair each model's terms need (0.20.18), checked against the pair table in the source
pairs = set(re.findall(r'c\("(M\d+)", "(M\d+)", "', SRC["R/model-comparison.R"]))
chosen = {("M4", "ALR x age"): ("M2", "M4"), ("M8", "ALR x age"): ("M10", "M8"), ("M9", "ALR x age"): ("M7", "M9"),
          ("M8", "AFR x age"): ("M9", "M8"), ("M10", "AFR x age"): ("M7", "M10")}
for (m, term), pr in chosen.items():
    ck(f"pair for {m} {term} exists in the table", pr in pairs, str(sorted(pairs)))
    ck(f"pair for {m} {term} ends at {m}", pr[1] == m)
print(f"{checks - len(fails)}/{checks} logic checks passed")
for f in fails: print("  FAIL:", f)
sys.exit(1 if fails else 0)
