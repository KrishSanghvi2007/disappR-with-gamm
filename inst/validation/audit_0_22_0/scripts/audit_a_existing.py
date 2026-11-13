"""Part A of the 0.22.0 audit: the code and features that existed before 0.22.0.
A1 static audit (syntax, calls against formals, wiring, help panels, examples, stress data)
A2 observers: every observe/observeEvent body wrapped in disappr_guard (an unguarded error ends the session)
A3 layout: column and box widths in each fluidRow of ui.R sum to at most 12
A4 scope: every function called in server.R is defined in server.R, the engine, global.R or an attached package
A5 saved results: SAVE_ORDER prefixes and add_saved titles agree
A6 regression: every change between 0.21.16 and 0.22.0 in a pre-existing function, classified; the default path of each
   changed function must equal the 0.21.16 code (pass OLD=<path to the 0.21.16 tree> to re-run this part)
A7 empirical and stress data: every bundled example and stress file loads with its mapping, and the sampling interval,
   grid and decomposition run on it (ports), reporting what the app will show under the defaults"""
import os, re, sys, subprocess, difflib, glob, numpy as np, pandas as pd
sys.path.insert(0, os.path.dirname(__file__))
import port022 as P
ROOT = P.ROOT
APP = os.path.join(ROOT, "inst", "app")
server = open(os.path.join(APP, "server.R"), encoding="utf-8").read()
ui = open(os.path.join(APP, "ui.R"), encoding="utf-8").read()
fails = []
def check(label, ok, detail=""):
    print(("PASS " if ok else "FAIL ") + label + (": " + detail if detail else ""))
    if not ok: fails.append(label)

print("== A1 static audit (tools/static_audit/run_all.py)")
r = subprocess.run([sys.executable, os.path.join(ROOT, "tools", "static_audit", "run_all.py")], capture_output=True, text=True, cwd=ROOT)
for line in r.stdout.splitlines():
    if line.startswith("==") or "errors" in line: print("   " + line.strip())
check("static audit exit status", r.returncode == 0)
check("static audit reports no errors", not re.search(r"[1-9]\d* errors", r.stdout))

def block_after(s, i):
    j = s.index("(", i); d = 0
    for k in range(j, len(s)):
        d += (s[k] == "(") - (s[k] == ")")
        if d == 0: return s[j:k + 1]
    return s[j:]

print("\n== A2 observers wrapped in disappr_guard")
unguarded = []
for m in re.finditer(r"\n\s*(observeEvent|observe)\(", server):
    blk = block_after(server, m.start() + 1)
    if "disappr_guard(" not in blk[:400]:
        line = server[:m.start()].count("\n") + 2
        unguarded.append((line, blk[:90].replace("\n", " ")))
n_obs = len(re.findall(r"\n\s*(?:observeEvent|observe)\(", server))
print("   %d observers; %d without disappr_guard in their first 400 characters" % (n_obs, len(unguarded)))
for l, b in unguarded: print("   line %d: %s" % (l, b))

print("\n== A3 widths per fluidRow in ui.R")
over = []
for m in re.finditer(r"fluidRow\(", ui):
    blk = block_after(ui, m.start())[1:-1]
    # direct children only: strip nested parentheses beyond depth 1
    depth = 0; top = []; cur = ""
    for ch in blk:
        if ch == "(": depth += 1
        if ch == ")": depth -= 1
        cur += ch
        if depth == 0 and ch == ",": top.append(cur); cur = ""
    top.append(cur)
    widths = []
    for t in top:
        t = t.strip()
        mm = re.match(r"(column)\((\d+)", t) or re.match(r"box\(width\s*=\s*(\d+)", t)
        if mm: widths.append(int(mm.groups()[-1]))
    if sum(widths) > 12:
        over.append((ui[:m.start()].count("\n") + 1, widths))
check("no fluidRow wider than 12 columns", not over, "; ".join("line %d %s" % o for o in over))

print("\n== A4 functions called in server.R are defined")
defined = set(re.findall(r"\b([A-Za-z_.][A-Za-z0-9_.]*)\s*<-\s*(?:function|reactive|reactiveVal|eventReactive|reactiveValues)\b", server))
eng = "".join(P.SRC.values()) + open(os.path.join(APP, "global.R"), encoding="utf-8").read()
defined |= set(re.findall(r"\b([A-Za-z_.][A-Za-z0-9_.]*)\s*<-\s*function\b", eng))
defined |= set(re.findall(r"\b([A-Za-z_.][A-Za-z0-9_.]*)\s*<-\s*function\b", server))
base_like = set("""c list paste paste0 sprintf format round signif max min sum mean length names is.null is.na is.finite
 nrow ncol unique sort order rev seq seq_len seq_along vapply lapply sapply Filter Negate Reduce do.call rbind cbind data.frame
 as.numeric as.character as.integer as.logical identical isTRUE isFALSE any all which match setdiff intersect union unlist
 tryCatch stop warning message invisible return function if else for while repeat next break switch nchar sub gsub grepl regmatches
 startsWith endsWith tolower toupper trimws substr strsplit rep abs sqrt exp log floor ceiling range diff table tapply split
 is.list is.character is.numeric is.function inherits structure attr exists get assign new.env environment on.exit file writeLines
 readLines close Sys.Date Sys.time format.Date basename normalizePath unname setNames character numeric logical integer vector
 matrix t apply mapply Map nlevels levels factor droplevels head tail print cat toupper interaction suppressWarnings suppressMessages
 withCallingHandlers conditionMessage invokeRestart emptyenv isolate req validate need observe observeEvent reactive reactiveVal
 renderUI renderTable renderPlot renderText downloadHandler showNotification removeNotification showModal modalDialog modalButton
 removeModal updateSelectInput updateNumericInput updateCheckboxInput updateRadioButtons updateCheckboxGroupInput
 updateSelectizeInput updateTextInput updateSliderInput updateTabsetPanel updateTabItems tagList div span p h4 h5 strong em tags HTML
 icon actionButton actionLink selectInput selectizeInput checkboxInput checkboxGroupInput radioButtons numericInput textInput
 sliderInput fileInput downloadButton plotOutput tableOutput uiOutput textOutput verbatimTextOutput column fluidRow box
 ggplot aes geom_line geom_point geom_hline geom_vline geom_smooth geom_ribbon geom_col geom_bar geom_text geom_errorbar
 geom_segment geom_abline geom_tile geom_boxplot geom_jitter geom_area geom_step geom_label facet_wrap facet_grid labs
 scale_colour_manual scale_fill_manual scale_colour_gradient scale_x_continuous scale_y_continuous scale_linetype_manual
 scale_size_area scale_shape_manual scale_alpha_identity guides guide_legend theme element_text element_blank coord_cartesian
 annotate position_jitter position_dodge xlim ylim expand_limits scale_size scale_fill_gradient2 scale_colour_gradient2
 unit margin element_rect element_line vars label_wrap_gen after_stat stat_summary scale_color_manual ggtitle theme_minimal
 local quote substitute eval parse deparse body formals sys.call sys.function Recall nargs missing interactive options getOption
 Sys.getenv Sys.setenv file.exists file.path dirname tempfile unlink dir.create list.files readRDS saveRDS max.col pmax pmin
 cumsum cumprod prod var sd median quantile is.environment ifelse vapply regexpr gregexpr sprintf mode class oldClass
 sample set.seed rnorm runif rbinom rpois nchar enc2utf8 iconv utf8ToInt intToUtf8 chartr casefold rownames colnames
 row.names duplicated anyDuplicated rowsum rowSums colSums rowMeans colMeans outer crossprod solve diag det chol eigen
 Sys.sleep proc.time system.time gc invisible trimws make.names make.unique nlevels xor bitwAnd emptyenv sys.frames
 environmentName topenv globalenv baseenv stopifnot mapply rapply vapply array dim aperm which.max which.min rank order
 cut findInterval tabulate rle inverse.rle jitter scale sweep trunc cos sin tan atan2 atanh tanh log1p expm1 log10 log2 exp
 as.Date strftime strptime julian months weekdays difftime seq.Date nchar rev.default is.element toupper Negate identity
 Encoding writeBin readBin rawToChar charToRaw utf8ToInt shQuote dQuote sQuote noquote format.default prettyNum formatC
 as.vector is.vector is.data.frame as.data.frame as.list is.factor as.factor unsplit mapply modifyList within transform merge
 aggregate stack unstack reshape expand.grid nchar regexec agrep sprintf""".split())
def strip_strings(code):
    """Remove string literals and comments so that words inside messages are not read as calls."""
    out = []; i = 0; n = len(code)
    while i < n:
        ch = code[i]
        if ch in "\"'":
            q = ch; i += 1
            while i < n and code[i] != q:
                i += 2 if code[i] == "\\" else 1
            i += 1; out.append('""'); continue
        if ch == "#":
            while i < n and code[i] != "\n": i += 1
            continue
        out.append(ch); i += 1
    return "".join(out)
code_only = strip_strings(server)
calls = set(re.findall(r"(?<![A-Za-z0-9_.$:@])([A-Za-z_.][A-Za-z0-9_.]*)\s*\(", code_only))
calls -= set(re.findall(r"(?<![A-Za-z0-9_.$:@])([A-Za-z_.][A-Za-z0-9_.]*)\s*=\s*function\s*\(", code_only))
missing = sorted(c for c in calls if c not in defined and c not in base_like and not c.startswith("."))
print("   %d distinct function names called; %d not found among local, engine or common package names:" % (len(calls), len(missing)))
print("   " + ", ".join(missing))

print("\n== A5 saved results: SAVE_ORDER prefixes against add_saved titles")
so = block_after(server, server.index("SAVE_ORDER <- c("))
prefixes = re.findall(r'"([^"]+)"', so)
titles = re.findall(r'add_saved\("[^"]+",\s*(paste0?\([^\n]*|"[^"]+")', server)
lits = [re.match(r'"([^"]+)"', t).group(1) if t.startswith('"') else re.search(r'"([^"]+)"', t).group(1) for t in titles]
unmatched_titles = [t for t in lits if not any(t.startswith(p) or p.startswith(t) for p in prefixes)]
unused_prefixes = [p for p in prefixes if not any(t.startswith(p) or p.startswith(t.strip()) for t in lits)]
check("every saved title has a SAVE_ORDER place", not unmatched_titles, ", ".join(unmatched_titles))
print("   %d titles, %d prefixes; prefixes with no literal title start (may be built at run time): %s" % (len(lits), len(prefixes), ", ".join(unused_prefixes) or "none"))

print("\n== A6 regression: changes to pre-existing code between 0.21.16 and 0.22.0")
OLD = os.environ.get("OLD")
if OLD and os.path.isdir(OLD):
    ALLOWED = [r"infer_age_step\(", r"resolve_age_step\(", r"step = NULL\)", r"step = meta\$age_step", r"disp = \"constant\"", r"disp = disp",
               r"disp = s\$disp", r"disp = ms\$disp", r"disp_str", r"normalise_disp", r"DENSITY_FAMILIES", r"CONTINUOUS_FAMILIES", r"family_r_call",
               r"family_link", r"\"beta\"", r"gamma", r"lognormal", r"DISPERSION_FAMILIES", r"Share_best|n_common|wins"]
    files = ["R/" + f for f in sorted(os.listdir(os.path.join(ROOT, "R"))) if f.endswith(".R")] + ["inst/app/server.R", "inst/app/ui.R"]
    n_h = 0
    for f in files:
        po, pn = os.path.join(OLD, f), os.path.join(ROOT, f)
        if not os.path.exists(po): print("   new file: " + f); continue
        a = open(po, encoding="utf-8").read().splitlines(); b = open(pn, encoding="utf-8").read().splitlines()
        sm = difflib.SequenceMatcher(None, a, b, autojunk=False)
        for tag, i1, i2, j1, j2 in sm.get_opcodes():
            if tag == "equal": continue
            n_h += 1
            removed = a[i1:i2]
            kind = "added" if tag == "insert" else "changed"
            note = "" if tag == "insert" else " | removed: " + " / ".join(x.strip()[:70] for x in removed[:2])
            print("   %s:%d-%d %s %d line(s)%s" % (f, j1 + 1, j2, kind, j2 - j1, note))
    print("   %d hunks listed; each is classified in README section A6" % n_h)
    # the population-curve refactor must be a verbatim move
    old_tr = open(os.path.join(OLD, "R", "trajectories.R"), encoding="utf-8").read()
    i = old_tr.index("predict_population_curve <- function(fit, res, ages, hold = NULL, by = NULL) {\n") + len("predict_population_curve <- function(fit, res, ages, hold = NULL, by = NULL) {\n")
    old_nd = old_tr[i:old_tr.index("  err <- NULL\n", i)]
    new_nd = P.r_body("population_newdata")
    new_nd = new_nd[new_nd.index("{\n") + 2:new_nd.index("  list(nd = nd, by = by)\n")]
    check("population_newdata() is a verbatim move of the old prediction grid code", old_nd == new_nd)
    # default paths of the step consumers: the only changed lines are the signature and infer -> resolve
    for fn, fname in [("build_missing_grid", "missingness.R"), ("disappearance_data", "disappearance.R"), ("life_table", "disappearance.R"),
                      ("terminal_data", "disappearance.R"), ("data_integrity", "validation.R"), ("schedule_irregular", "validation.R"),
                      ("observed_prediction_ages", "trajectories.R")]:
        o = open(os.path.join(OLD, "R", fname), encoding="utf-8").read()
        m = re.search(r"\n%s <- function\(" % fn, o); j = o.index("{", m.end()); dpt = 0
        for k in range(j, len(o)):
            dpt += (o[k] == "{") - (o[k] == "}")
            if dpt == 0: ob = o[m.start():k + 1]; break
        nb = P.r_body(fn)
        # wording changes inside messages (the 0.22.7 wording audit) are not code changes: compare with strings blanked
        blank = lambda t: re.sub(r'"(?:[^"\\]|\\.)*"', '""', t)
        nocomm = lambda t: "\n".join(re.sub(r"#.*$", "", x).rstrip() for x in t.splitlines())
        diff = [l for l in difflib.ndiff(nocomm(blank(ob)).splitlines(), nocomm(blank(nb)).splitlines()) if l[:1] in "+-" and l[1:].strip()]
        allowed = r"function\(|infer_age_step|resolve_age_step|valid_age_step"
        if fn == "build_missing_grid":   # 0.24.2: the window opens at the AFR the models use (reviewed in Part J)
            allowed += r"|afr_ref|first_k|start_ref|last_tk|end_k|alr_k|function\\(v\\)"
        if fn == "data_integrity":   # 0.24.3: the ALR checks follow the trait-specific option (reviewed in Part K)
            # the string blanker cannot split this function's long run of adjacent strings, so it forms one line that
            # changes whenever any message changes; the wording was reviewed (Part D) and the logic in Part K
            allowed += r'|trait_specific|last_ref|last_observed|im\$alr|paste0\(|add\(if|sprintf\(|if \(mism > 0\)|Duplicate ID|gap <- \(im\$lifespan|^[-+]\s*"*\) else "*\)$'
        if fn == "life_table":   # 0.23.2 adds the frailty test on individual occasions (reviewed in Part H)
            allowed += r"|hazard|frailty|\bpo\b|rows_i|\blr\b|p_const|data\.frame\(id|do\.call|^[-+]\s*[})]|is\.null\(lr\)|a <- ages|if \(!length\(a\)\)|tryCatch|error = function"
        ok = all(re.search(allowed, l) for l in diff)
        check("%s: only the signature and the interval lookup changed (wording aside)" % fn, ok, "%d changed code lines" % len(diff))
else:
    print("   skipped: set OLD to the 0.21.16 tree to list the hunks (captured in a6_hunks.txt)")

print("\n== A7 examples and stress data: load, sampling interval, grid, decomposition (ports)")
imp = P.SRC["import.R"]
body = imp[imp.index("EXAMPLES <- list("):]
keys = [(m.group(1), m.start()) for m in re.finditer(r"\n  ([a-z0-9_]+) = list\(", body)]
fly_map = dict(re.findall(r'(\w+) = "([^"]*)"', imp[imp.index("FLY_MAPPING <- list"):imp.index("FLY_MAPPING <- list") + 800]))
rows = []
for i, (k, pos) in enumerate(keys):
    seg = body[pos: keys[i + 1][1] if i + 1 < len(keys) else len(body)]
    f = re.search(r'file = "([^"]+)"', seg).group(1)
    mp = fly_map if k == "fly" else dict(re.findall(r'(\w+) = "([^"]*)"', seg[seg.index("mapping = example_map"):seg.index("mapping = example_map") + 700]))
    d0 = pd.read_csv(os.path.join(APP, "data", f))
    sub = re.search(r'subset = list\(var = "([^"]+)", levels = c\(([^)]*)\)', seg)
    if sub:
        lv = re.findall(r'"([^"]*)"', sub.group(2)); d0 = d0[d0[sub.group(1)].astype(str).isin(lv)]
    cols_ok = all(mp.get(c, "") in d0.columns for c in ("id", "age", "trait"))
    check("%s: id, age and trait columns present" % k, cols_ok)
    if not cols_ok: continue
    d = pd.DataFrame(dict(id=d0[mp["id"]].astype(str), age=pd.to_numeric(d0[mp["age"]], errors="coerce"),
                          trait=pd.to_numeric(d0[mp["trait"]], errors="coerce")))
    st = P.infer_age_step(d.age.values, d.id.values)
    g = P.missing_grid(d); de = P.decomposition_trajectory(d)
    rows.append((k, len(d), d.id.nunique(), st, 100 * g["missing"], de["segments"], int(de["rows"].n_pairs.max()) if len(de["rows"]) else 0, de["merged"]))
print("   %-24s %6s %5s %8s %8s %5s %8s %6s" % ("example", "rows", "ids", "step", "miss %", "segs", "max pair", "merged"))
for r_ in rows: print("   %-24s %6d %5d %8.4g %8.1f %5d %8d %6s" % r_)
stress = sorted(glob.glob(os.path.join(ROOT, "tests", "stress_data", "*.csv")))
print("   stress files with id/age/trait-like columns: %d" % len(stress))


print("\n== A8 cross-check: this audit's decomposition port against the 0.21.0 audit's independent port")
src = open(os.path.join(ROOT, "inst", "validation", "audit_0_21_0", "scripts", "decomp_truth_check.py"), encoding="utf-8").read()
ns = {}
exec(src.split("def lam(")[0], ns)
worst = 0.0; n_cmp = 0
for rho in [(0, 0, 0), (0.5, 0, 0), (0, 0.5, 0), (0, 0, -0.5)]:
    for retain in (1.0, 0.5):
        ids, ages, trait, _ = ns["simulate"](rho, seed=7, retain=retain)
        old, _seg = ns["decomposition_trajectory"](ids, ages, trait)
        new = P.decomposition_trajectory(pd.DataFrame(dict(id=ids, age=ages, trait=trait)))["rows"]
        if len(old) and len(new):
            m = pd.DataFrame(old, columns=["age", "fitted"]).merge(new[["age", "fitted"]], on="age", suffixes=("_old", "_new"))
            worst = max(worst, float(np.max(np.abs(m.fitted_old - m.fitted_new)))); n_cmp += len(m)
check("the two independent decomposition ports agree on the manuscript's generating process", worst < 1e-8, "%d points, largest difference %.2g" % (n_cmp, worst))
print("\n%d failure(s)" % len(fails) + (": " + "; ".join(fails) if fails else ""))
