"""Part F of the audit (0.23.0): the GAMM layer.
F1 independence: the layer adds code but changes nothing that steps 1-6 use
F2 wiring: every GAMM input is read, every output is placed, every help key exists
F3 the engine only defines gamm_/GAMM_ names, so nothing existing is redefined
F4 the derivative method (finite differences of the model matrix, Bayesian covariance, simultaneous bands) is
   checked against the truth on penalised-spline fits in Python: coverage of the true slope over 300 datasets
F5 the other audit parts still pass (run separately; see README)"""
import os, re, sys, subprocess, numpy as np
from scipy.interpolate import BSpline
sys.path.insert(0, os.path.dirname(__file__))
import port022 as P
ROOT = P.ROOT; APP = os.path.join(ROOT, "inst", "app")
OLD = os.environ.get("OLD", "")
fails = []
def check(label, ok, detail=""):
    print(("PASS " if ok else "FAIL ") + label + (": " + detail if detail else ""))
    if not ok: fails.append(label)
server = open(os.path.join(APP, "server.R"), encoding="utf-8").read()
ui = open(os.path.join(APP, "ui.R"), encoding="utf-8").read()
eng = open(os.path.join(ROOT, "R", "gamm.R"), encoding="utf-8").read()

print("== F1 independence")
i0 = server.index("# 0.23.0: GAMM layer.")
block = server[i0:]
assigned = set(re.findall(r"output\$([A-Za-z0-9_]+)\s*<-", block))
check("every output the layer writes is its own (gamm_*)", all(a.startswith("gamm_") for a in assigned), ", ".join(sorted(a for a in assigned if not a.startswith("gamm_"))))
rv = set(re.findall(r"\b([A-Za-z0-9_]+)\s*<-\s*reactiveVal\(", block)) | set(re.findall(r"\b([A-Za-z0-9_]+)\s*<-\s*reactive\(", block))
check("every reactive the layer defines is its own (gamm_*)", all(a.startswith("gamm_") for a in rv), ", ".join(sorted(rv)))
setters = set(re.findall(r"\b([a-z0-9_]+)\((?:NULL|res|r)\)", block)) - {"gamm_store", "gamm_groups_store", "gamm_proxy_store", "gamm_poly_store", "is.null", "isTRUE", "safe_get", "gamm_code", "gamm_vcomp", "gamm_individual_curves", "gamm_residuals", "gamm_proxy_terms", "gamm_desc", "gamm_level_of", "gamm_tables", "gamm_kcheck", "gamm_concurvity", "gamm_curve", "gamm_derivative", "gamm_peak_onset", "gamm_difference", "list", "nrow", "length", "identical", "unique", "names", "c", "print", "invisible"}
check("the layer sets no reactive value outside itself", not any(s in server[:i0] and re.search(r"\b%s\s*<-\s*reactiveVal" % s, server[:i0]) for s in setters), ", ".join(sorted(setters)))
shared = set(re.findall(r"\b(dat|meta|data_sig|fitted_models|model_settings|ms|input\$(?!gamm_|save_gamm_)[a-z_0-9]+)\b", block))
check("the only shared things the layer reads are dat(), meta() and data_sig()", shared <= {"dat", "meta", "data_sig"}, ", ".join(sorted(shared)))
if OLD and os.path.exists(os.path.join(OLD, "inst", "app", "server.R")):
    old = open(os.path.join(OLD, "inst", "app", "server.R"), encoding="utf-8").read()
    old_lines = set(old.splitlines()); new_pre = server[:i0].splitlines()
    added = [l for l in new_pre if l not in old_lines and l.strip()]
    allowed = re.compile(r'"GAMM |gamm_store\(\)|ctx\$gamm|is\.list\(g\) && isTRUE\(g\$ok\)|renderTable <- function|shiny::renderTable\(bquote|if \(!quoted\) expr|Every table passes through|^\s*}\s*$|^\s*$')
    extra = [l for l in added if not allowed.search(l)]
    print("   lines added before the layer relative to %s: %d; outside the save order and methods context: %d" % (OLD, len(added), len(extra)))
    for l in extra[:10]: print("     " + l.strip()[:120])
m0 = ui.index('tabItem(tabName = "gamm"'); m1 = ui.index('tabItem(tabName = "summary"')
ui_gamm = ui[m0:m1]
ids = re.findall(r'(?:Input|Button|Output|radioButtons|checkboxInput|selectInput|textInput|numericInput|actionButton|save_button|plotOutput|tableOutput|uiOutput|verbatimTextOutput)\("([A-Za-z0-9_]+)"', ui_gamm)
check("every id in the GAMM tab is its own (gamm_* or save_gamm_*)", all(i.startswith(("gamm_", "save_gamm_")) for i in ids), ", ".join(sorted(i for i in ids if not i.startswith(("gamm_", "save_gamm_")))))

print("\n== F2 wiring")
ui_ins = set(re.findall(r'(?:selectInput|numericInput|textInput|checkboxInput|radioButtons|actionButton|save_button)\("(gamm_[a-z_]+|save_gamm_[a-z_]+)"', ui_gamm + block))
read = set(re.findall(r"input\$(gamm_[a-z_]+|save_gamm_[a-z_]+)", block))
check("every GAMM input is read by the server", ui_ins <= read, ", ".join(sorted(ui_ins - read)))
ui_outs = set(re.findall(r'(?:plotOutput|tableOutput|uiOutput|verbatimTextOutput)\("(gamm_[a-z_]+)"', ui_gamm + block))
check("every GAMM output placed in the UI is defined, and vice versa", ui_outs == assigned, "UI only: %s; server only: %s" % (sorted(ui_outs - assigned), sorted(assigned - ui_outs)))
keys = set(re.findall(r'info_title\("[^"]+", "([a-z_0-9]+)"\)', ui_gamm))
helpers = open(os.path.join(ROOT, "R", "ui-helpers.R"), encoding="utf-8").read()
check("every help key of the tab exists", all(re.search(r"\n  %s = info_entry\(" % k, helpers) for k in keys), ", ".join(sorted(keys)))

print("\n== F3 engine names")
defs = re.findall(r"^([A-Za-z_.][A-Za-z0-9_.]*)\s*<-", eng, re.M)
check("the engine defines only gamm_/GAMM_ names (%d)" % len(defs), all(d.startswith(("gamm_", "GAMM_")) for d in defs), ", ".join(d for d in defs if not d.startswith(("gamm_", "GAMM_"))))
others = "".join(open(os.path.join(ROOT, "R", f), encoding="utf-8").read() for f in os.listdir(os.path.join(ROOT, "R")) if f.endswith(".R") and f != "gamm.R")
check("no other engine file defines a gamm_ name", not re.search(r"^gamm_[a-z_]+\s*<-", others, re.M))

print("\n== F4 the derivative method against the truth (penalised B-splines, 300 datasets)")
def design(x, knots, deg=3):
    t = np.r_[[knots[0]] * deg, knots, [knots[-1]] * deg]
    n = len(t) - deg - 1
    return np.column_stack([BSpline(t, np.eye(n)[j], deg, extrapolate=False)(x) for j in range(n)]), t, n
def pfit(x, y, knots, lams=np.exp(np.linspace(-6, 8, 30))):
    X, t, n = design(x, knots)
    X = np.nan_to_num(X)
    Dm = np.diff(np.eye(n), 2, axis=0); S = Dm.T @ Dm
    best = None
    for lam in lams:
        A = X.T @ X + lam * S; Ai = np.linalg.inv(A); H = X @ Ai @ X.T
        b = Ai @ X.T @ y; r = y - X @ b; edf = np.trace(H)
        gcv = len(y) * (r @ r) / (len(y) - edf) ** 2
        if best is None or gcv < best[0]: best = (gcv, b, Ai, r @ r / (len(y) - edf))
    _, b, Ai, s2 = best
    return b, s2 * Ai, t, n
rng = np.random.default_rng(23)
truth_f = lambda a: 1.2 * a - 0.09 * a ** 2
truth_d = lambda a: 1.2 - 0.18 * a
knots = np.linspace(0, 12, 9); grid = np.linspace(0.5, 11.5, 60); eps = 1e-4
cov_pw = []; cov_sim = []; wrong_sig = 0
for rep in range(300):
    x = rng.uniform(0, 12, 400); y = truth_f(x) + rng.normal(0, 1.0, 400)
    b, V, t, n = pfit(x, y, knots)
    X0 = np.nan_to_num(design(grid - eps, knots)[0]); X1 = np.nan_to_num(design(grid + eps, knots)[0])
    D = (X1 - X0) / (2 * eps)
    d = D @ b; se = np.sqrt(np.einsum("ij,jk,ik->i", D, V, D))
    B = rng.multivariate_normal(np.zeros(len(b)), V, 1000)
    crit = np.quantile(np.max(np.abs(D @ B.T) / se[:, None], axis=0), 0.95)
    td = truth_d(grid)
    cov_pw.append(np.mean(np.abs(d - td) <= 1.96 * se))
    cov_sim.append(np.all(np.abs(d - td) <= crit * se))
    sig_dec = (d + crit * se) < 0
    wrong_sig += np.any(sig_dec & (td > 0))
print("   pointwise coverage of the true slope %.3f (average over ages); simultaneous coverage of the whole slope curve %.3f" % (np.mean(cov_pw), np.mean(cov_sim)))
print("   datasets where a simultaneous 'significant decline' was called at an age where the true slope is positive: %d of 300" % wrong_sig)
check("F4 pointwise intervals cover the true slope about 95% of the time (0.90-0.99)", 0.90 <= np.mean(cov_pw) <= 0.99)
check("F4 simultaneous bands cover the whole true slope curve at least 90% of the time", np.mean(cov_sim) >= 0.90)
check("F4 declines are almost never called where the trait truly rises (<= 2%)", wrong_sig <= 6)
print("\n%d failure(s)%s" % (len(fails), (": " + "; ".join(fails)) if fails else ""))
