"""Part L of the audit (0.24.4): conflicts between the trait-specific box, mapped AFR/ALR columns and the AFE start of
the missingness window, and propagation of the chosen AFR/ALR to the models.
L1 precedence and wording: mapped columns win over the box; the box's note says so; the automatic choices name the
   box; AFE opens the window at the earlier of AFE and AFR; help says so
L2 propagation: the models' ALR/AFR proxies, Model 6's automatic lifespan, the fit signature (a changed box forces a
   refit), the exported script, the methods draft, the proxy panels, the data checks and the grid all read the same
   prepared AFR/ALR
L3 a line-by-line port of the R grid code (build_missing_grid, 0.24.4) against the specification, on every
   combination: AFR automatic or mapped x ALR automatic or mapped x box ticked or not x AFR mode or AFE (earlier or
   later than AFR), over random datasets with records lacking the trait before, between and after trait values
Output l_conflicts.txt."""
import os, re, sys, itertools, numpy as np, pandas as pd
sys.path.insert(0, os.path.dirname(__file__))
import port022 as P
ROOT = P.ROOT
fails = []
def check(label, ok, detail=""):
    print(("PASS " if ok else "FAIL ") + label + (": " + detail if detail else ""))
    if not ok: fails.append(label)
src = lambda f: open(os.path.join(ROOT, f), encoding="utf-8").read()
server, helpers, miss, prep, exp_, val = src("inst/app/server.R"), src("R/ui-helpers.R"), src("R/missingness.R"), src("R/data-preparation.R"), src("R/export.R"), src("R/validation.R")

print("== L1 precedence and wording")
check("a mapped ALR or AFR column wins over the box (engine)", "out$alr <- if (alr_mapped) round_age(safe_numeric(df[[map$alr]])[rows]) else last_rec" in prep
      and "out$entry <- if (entry_mapped) round_age(safe_numeric(df[[map$entry]])[rows]) else first_rec" in prep)
check("the note under the box says which of ALR/AFR it cannot change when columns are mapped",
      "ALR comes from a column, so this box changes only AFR." in server and "AFR comes from a column, so this box changes only ALR." in server
      and "ALR and AFR both come from columns, so this box changes neither" in server)
check("the automatic ALR, AFR and lifespan choices refer to the box",
      "Automatic: last age (see 'Trait-specific ALR/AFR')" in server and "Automatic: first age (default; see 'Trait-specific ALR/AFR')" in server
      and "Automatic: the automatic ALR (proxy only)" in server)
check("AFE opens the window at the earlier of AFE and the AFR (not the first record of any kind)",
      "start_ref <- if (common_start) pmin(start_age, afr_ref) else afr_ref" in miss and "first_k <- round((start_ref - anchor) / step)" in miss)
check("the AFE help says so, and that AFE changes only the missingness figures",
      "The window then opens at the earlier of the AFE and the individual's AFR." in helpers and "This setting changes the missingness figures only." in helpers)
check("the help panel of the box says mapped columns are used as supplied", "ALR or AFR columns chosen in the mapping are used as supplied, whichever option is chosen." in helpers)

print("\n== L2 propagation of AFR and ALR")
check("models: the ALR and AFR proxies come from the prepared data", "raw <- list(ALR = im$alr[mp], LS = im$lifespan[mp], AFR = afr_i[mp])" in prep
      and "afr_i <- ifelse(is.finite(im$entry), im$entry, im$first_recorded)" in prep)
check("models: an automatic lifespan follows the box", "out$life <- if (life_auto) last_rec" in prep)
check("fits: the data fingerprint includes the box, and results are shown only for the current fingerprint",
      "isTRUE(m$has_group2), !isFALSE(m$trait_specific_ages)," in server and "res$signature <- paste(data_sig(), settings_sig(s))" in server
      and "shiny::need(identical(r$signature, paste(data_sig(), settings_sig(s)))" in server)
check("fits: the fingerprint also sums the prepared ALR and AFR", "signif(sum(d$alr, na.rm = TRUE), 12)" in server and "signif(sum(d$entry, na.rm = TRUE), 12)" in server)
check("exported script and methods draft follow the box", "trait-specific ALR and AFR: only ages with a trait value" in exp_ and "oldest age with a trait value" in exp_)
check("proxy panels and data checks read the prepared ALR/AFR", "proxy_pair_plot(im$mean_age, im$alr" in server and "last_ref <- if (trait_specific" in val)
check("evidence and effect summaries read the prepared ALR", "age_coefficients(by_age, d$alr, d$trait" in src("R/evidence.R") and "im$alr[is.finite(im$alr)]" in src("R/effects.R"))
check("the R snapshot tier counts missingness from AFR, as the app does by default", 'build_missing_grid(b$data, 0, "afr", NA_real_)' in src("tests/scripts/trait_ages_snapshot.R"))
check("the R unit tests cover every combination", all(x in src("tests/testthat/test-0-24-4.R") for x in ("for (afr_col in c(FALSE, TRUE)) for (alr_col in c(FALSE, TRUE)) for (ticked in c(FALSE, TRUE))",
      "for (afe in c(1, 3))", "pm$ALR_raw", "pm$AFR_raw")))

# ------------------------------------------------------------------ L3 port of the R grid code and the specification
def prepare(raw, afr_col, alr_col, ticked):
    d = raw.copy()
    use = d.age.where(np.isfinite(d.y)) if ticked else d.age
    g = use.groupby(d.id)
    d["alr"] = d.ALRc if alr_col else d.id.map(g.max())
    d["entry"] = d.AFRc if afr_col else d.id.map(g.min())
    d["trait"] = d.y
    return d
def r_grid(d, start_mode, start_age, step=1.0):
    """Port of build_missing_grid() 0.24.4 (margin 0, fixed step): returns {id: (window start, window end)}."""
    has = d.groupby("id").trait.apply(lambda v: np.isfinite(v).any()); d = d[d.id.isin(has.index[has])]
    ids = list(pd.unique(d.id)); fi = d.id.map({v: i for i, v in enumerate(ids)}).values
    by = lambda col: d.groupby("id")[col]
    first_age = by("age").min().reindex(ids).values
    entry = d.groupby("id").entry.apply(lambda v: v[np.isfinite(v)].min() if np.isfinite(v).any() else np.nan).reindex(ids).values
    alr_i = by("alr").mean().reindex(ids).values
    amax = max(d.age.max(), np.nanmax(alr_i))
    common = start_mode == "same" and np.isfinite(start_age)
    fta = d[np.isfinite(d.trait)].groupby("id").age.min().reindex(ids).values
    afr_ref = np.where(np.isfinite(entry), np.minimum(entry, fta), fta)
    start_ref = np.minimum(start_age, afr_ref) if common else afr_ref
    n_before = np.maximum(0, np.floor((first_age - start_ref) / step + 1e-9)); anchor = first_age - n_before * step
    k = np.round((d.age.values - anchor[fi]) / step)
    last_k = pd.Series(k).groupby(fi).max().reindex(range(len(ids))).values
    last_tk = pd.Series(np.where(np.isfinite(d.trait.values), k, np.nan)).groupby(fi).max().reindex(range(len(ids))).values
    alr_k = np.where(np.isfinite(alr_i), np.round((alr_i - anchor) / step), np.nan)
    end_k = np.fmax(alr_k, last_tk); end_k = np.minimum(end_k, np.floor((amax - anchor) / step + 1e-9)); end_k = np.fmax(end_k, last_tk)
    end_k = np.where(np.isfinite(end_k), end_k, last_k)
    first_k = np.round((start_ref - anchor) / step)
    start_k = np.maximum.reduce([np.zeros(len(ids)), first_k, end_k - 4999])
    return {ids[i]: (anchor[i] + start_k[i] * step, anchor[i] + end_k[i] * step) for i in range(len(ids))}, afr_ref
def spec(d, afe):
    """The specification: AFR = column, else first age with a trait value (ticked) or of any record (unticked); the
    window opens at the earlier of AFR, the first trait value and (if set) the AFE, and closes at the later of ALR and
    the last trait value (capped at the oldest age or ALR in the data)."""
    out = {}
    ok = d[np.isfinite(d.trait)]
    amax = max(d.age.max(), d.alr.max())
    for i, x in ok.groupby("id"):
        afr = d[d.id == i].entry.iloc[0]; alr = d[d.id == i].alr.iloc[0]
        s = min(afr, x.age.min()) if np.isfinite(afr) else x.age.min()
        if afe is not None: s = min(s, afe)
        e = min(max(alr, x.age.max()), amax)
        out[i] = (s, max(e, x.age.max()))
    return out
print("\n== L3 the R grid code (ported) against the specification, every combination")
rng = np.random.default_rng(11)
n_cases = n_bad = 0; bad = []
for rep in range(60):
    rows = []
    for i in range(25):
        ls = int(rng.integers(2, 12)); start = int(rng.integers(1, 4))
        ages = [a for a in range(start, ls + 1) if rng.random() < 0.85] or [start]
        afr_c = float(start + rng.integers(-1, 2)); alr_c = float(ls - rng.integers(0, 2))   # one value per individual
        for a in ages:
            rows.append((f"i{i}", float(a), rng.normal() if rng.random() < 0.6 else np.nan, afr_c, alr_c))
    raw = pd.DataFrame(rows, columns=["id", "age", "y", "AFRc", "ALRc"])
    for afr_col, alr_col, ticked in itertools.product([False, True], repeat=3):
        d = prepare(raw, afr_col, alr_col, ticked)
        for afe in (None, 0.0, 3.0):
            got, _ = r_grid(d, "same" if afe is not None else "afr", afe if afe is not None else np.nan)
            want = spec(d, afe)
            n_cases += 1
            diff = [(i, got.get(i), want[i]) for i in want if got.get(i) is None or abs(got[i][0] - want[i][0]) > 1e-9 or abs(got[i][1] - want[i][1]) > 1e-9]
            if diff:
                n_bad += 1
                if len(bad) < 3: bad.append((afr_col, alr_col, ticked, afe, diff[:2]))
check("the grid code matches the specification in every combination (%d datasets x combinations)" % n_cases, n_bad == 0,
      "%d mismatching; e.g. %s" % (n_bad, bad[:1]) if n_bad else "")
# the conflict singled out by the review: AFR column + ticked + AFE
raw = pd.DataFrame(dict(id=["P"] * 6, age=np.arange(1.0, 7.0), y=[np.nan, 5, 6, 7, np.nan, np.nan], AFRc=[1.0] * 6, ALRc=[5.0] * 6))
w1, _ = r_grid(prepare(raw, True, False, True), "same", 3.0)
w2, _ = r_grid(prepare(raw, False, False, True), "same", 3.0)
w3, _ = r_grid(prepare(raw, False, False, True), "same", 1.0)
check("AFR column (1) + ticked + AFE 3: window opens at the column AFR (1) and closes at the trait-specific ALR (4)", w1["P"] == (1.0, 4.0))
check("automatic AFR + ticked + AFE 3: window opens at the trait-specific AFR (2), not the first record (1)", w2["P"] == (2.0, 4.0))
check("automatic AFR + ticked + AFE 1: window opens at the AFE (1)", w3["P"] == (1.0, 4.0))
print("\n%d failure(s)%s" % (len(fails), (": " + "; ".join(fails)) if fails else ""))
