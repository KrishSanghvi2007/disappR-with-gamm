"""Extended independent validation for disappR: every scenario over many seeds.

Each scenario simulates data with a known answer and fits the app's models with a second implementation
(harness.py: prepare/formulas transcribed from R/data-preparation.R and R/formulas.R; maximum-likelihood
LMM; Gauss-Hermite GLMMs for Poisson and binomial). Nothing here executes the R package.

A scenario is judged on the whole set of seeds, not on one run: the share of seeds whose lowest-AIC model
is in the expected set, and the median dAIC of the best expected-set model behind the overall winner.
Models within 2 AIC are not distinguished by the data, so a seed whose winner lies outside the expected set
by less than 2 AIC is a tie, not a failure.

    python3 run_extended.py             # 20 seeds per scenario, writes results_extended.csv
    python3 run_extended.py --seeds 10
    python3 run_extended.py --only binomial,decomposition   # rerun some areas, keep the rest
"""
import argparse, os, time, warnings
warnings.filterwarnings("ignore")
import numpy as np, pandas as pd
from harness import simulate, suite, standardise
from grid_decomposition import decomposition

ADD = {"M1", "M2", "M3", "M7"}                 # age-independent corrections (ALR or mean age) and the null
INT = {"M4", "M5", "M6", "M8", "M9", "M10"}    # age-dependent (interaction) models
NONE = {"M1"}


def judge(winners, gaps, expect, documented=False):
    share = float(np.mean([w in expect for w in winners]))
    med = float(np.nanmedian(gaps))
    if documented:
        # an intentional demonstration of a false-positive mechanism: report the rate, do not grade it
        verdict = f"DOCUMENTED: {100 * (1 - share):.0f}% of seeds favour a selection model"
    else:
        verdict = "PASS" if share >= 0.7 else ("PASS (ties)" if med < 2.0 else "FAIL")
    return share, med, verdict


def run_scenario(area, scen, expect, make, models, seeds, note="", documented=False, **fitkw):
    winners, gaps = [], []
    for s in range(seeds):
        tab, _, _ = suite(make(s), models=models, **fitkw)
        winners.append(tab.index[0])
        inset = [m for m in tab.index if m in expect]
        gaps.append(float(tab[inset[0]]) if inset else np.nan)
    share, med, verdict = judge(winners, gaps, expect, documented)
    counts = pd.Series(winners).value_counts()
    return dict(Area=area, Scenario=scen, Expected="/".join(sorted(expect)), Seeds=seeds,
                Expected_win_share=round(share, 2), Median_dAIC_expected_behind_winner=round(med, 2),
                Winners=", ".join(f"{k}:{v}" for k, v in counts.items()), Verdict=verdict, Note=note)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--seeds", type=int, default=20)
    ap.add_argument("--only", type=str, default="")
    ap.add_argument("--out", type=str, default="results_extended.csv")
    a = ap.parse_args()
    S = a.seeds
    only = set(x.strip() for x in a.only.split(",") if x.strip())
    want = lambda area: (not only) or (area in only)
    rows = []
    t0 = time.time()

    if want("count"):
        for sd, exp in [("none", NONE), ("level", ADD), ("rate", INT)]:
            rows.append(run_scenario("count", f"Poisson, SD={sd}", exp,
                                     lambda s, sd=sd: simulate(sd=sd, fun="linear", family="poisson", seed=100 + s),
                                     ("M1", "M2", "M4"), S, family="poisson", fun="Linear"))
    if want("binomial"):
        for tr, lab in [(1, "binary 0/1"), (8, "proportion, 8 trials")]:
            for sd, exp in [("none", NONE), ("level", ADD), ("rate", INT)]:
                rows.append(run_scenario("binomial", f"{lab}, SD={sd}", exp,
                                         lambda s, sd=sd, tr=tr: simulate(sd=sd, fun="linear", family="binomial", trials=tr, seed=200 + s),
                                         ("M1", "M2", "M4"), S, family="binomial", fun="Linear", trials="ntrials"))
    if want("appearance"):
        rows.append(run_scenario("appearance", "selective appearance only", {"M7", "M8", "M9", "M10"},
                                 lambda s: simulate(sd="appearance", fun="linear", seed=300 + s),
                                 ("M1", "M2", "M4", "M7", "M8", "M9", "M10"), S, fun="Linear"))
        rows.append(run_scenario("appearance", "variable AFR, nothing linked to the trait", ADD,
                                 lambda s: simulate(sd="none", fun="linear", afr_var=3, seed=320 + s),
                                 ("M1", "M2", "M4", "M7", "M8", "M9", "M10"), S, fun="Linear",
                                 note="no selection of any kind: only ties are acceptable outside the expected set"))
    if want("random_effects"):
        rows.append(run_scenario("random effects", "nested: individual within group, rate-linked SD", INT,
                                 lambda s: simulate(sd="rate", fun="linear", groups=6, seed=400 + s),
                                 ("M1", "M2", "M4"), S, fun="Linear", group="grp", nested=True))
    if want("covariates"):
        rows.append(run_scenario("covariates", "factor covariate with an age interaction, rate-linked SD", INT,
                                 lambda s: simulate(sd="rate", fun="linear", cov=True, seed=500 + s),
                                 ("M1", "M2", "M4"), S, fun="Linear", covars=("diet",), factors=("diet",)))
    if want("random_slopes"):
        rows.append(run_scenario("random slopes", "slope variation (SD 0.12), no selection, slopes FITTED", ADD,
                                 lambda s: simulate(sd="none", fun="linear", seed=600 + s, slope_sd=0.12),
                                 ("M1", "M2", "M4"), S, fun="Linear", slope_type="uncorrelated"))
        rows.append(run_scenario("random slopes", "slope variation (SD 0.12), no selection, slopes NOT fitted", ADD,
                                 lambda s: simulate(sd="none", fun="linear", seed=600 + s, slope_sd=0.12),
                                 ("M1", "M2", "M4"), S, fun="Linear", slope_type="none", documented=True,
                                 note="documented false-positive mechanism: among-individual variation in ageing rate can be absorbed by the ALR x age interaction when random slopes are omitted"))
        rows.append(run_scenario("random slopes", "rate-linked SD, random slopes fitted", INT,
                                 lambda s: simulate(sd="rate", fun="linear", seed=620 + s, slope_sd=0.05),
                                 ("M1", "M2", "M4"), S, fun="Linear", slope_type="uncorrelated"))
    if want("misspecification"):
        rows.append(run_scenario("misspecification", "quadratic data fitted with a LINEAR ageing function, no selection", NONE,
                                 lambda s: simulate(sd="none", fun="quadratic", seed=700 + s),
                                 ("M1", "M2", "M4", "M5"), S, fun="Linear", documented=True,
                                 note="documented false-positive mechanism: unmodelled curvature is absorbed by the proxy x age terms"))
        rows.append(run_scenario("misspecification", "quadratic data fitted with a QUADRATIC function, no selection", NONE,
                                 lambda s: simulate(sd="none", fun="quadratic", seed=700 + s),
                                 ("M1", "M2", "M4", "M5"), S, fun="Quadratic"))
    if want("decomposition"):
        for sd, exp, lab in [("none", "unbiased", "no SD"), ("level", "unbiased", "level-linked SD"), ("rate", "biased", "rate-linked SD")]:
            slopes = []
            for s in range(S):
                d = simulate(n=400, mean_ls=16, sd=sd, fun="linear", seed=800 + s)
                dat, _ = standardise(d, dict(id="id", age="age", trait="trait", alr="__AUTO_LAST__", life="LS", entry="__AUTO_FIRST__"))
                out, _ = decomposition(dat)
                slopes.append(np.polyfit(out.age, out.fitted, 1)[0])
            mu, sdv = float(np.mean(slopes)), float(np.std(slopes))
            got = "unbiased" if abs(mu + 0.25) < 0.04 else "biased"
            rows.append(dict(Area="decomposition", Scenario=lab, Expected=exp, Seeds=S, Expected_win_share=np.nan,
                             Median_dAIC_expected_behind_winner=np.nan, Winners=f"slope {mu:+.3f} +/- {sdv:.3f} (truth -0.250)",
                             Verdict="PASS" if got == exp else "FAIL", Note="chained mean within-individual change on the population grid"))
    df = pd.DataFrame(rows)
    if only and os.path.exists(a.out):
        old = pd.read_csv(a.out)
        df = pd.concat([old[~old.Area.isin(df.Area.unique())], df], ignore_index=True)
    df.to_csv(a.out, index=False)
    pd.set_option("display.width", 220)
    print(df[["Area", "Scenario", "Expected", "Seeds", "Expected_win_share", "Median_dAIC_expected_behind_winner", "Verdict"]].to_string(index=False))
    graded = df[~df.Verdict.str.startswith("DOCUMENTED")]
    print(f"\n{(graded.Verdict != 'FAIL').sum()}/{len(graded)} graded scenarios pass over {S} seeds each "
          f"({(df.Verdict.str.startswith('DOCUMENTED')).sum()} documented false-positive demonstrations reported separately); {time.time() - t0:.0f} s")


if __name__ == "__main__":
    main()
