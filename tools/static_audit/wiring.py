"""Shiny wiring and data audit: inputs, outputs, info keys, example mappings, Collate and exports."""
import re, sys, glob, os, csv, io
from rparse import parse
from xref import walk

root = sys.argv[1]
rd = lambda p: open(os.path.join(root, p), encoding="utf-8", errors="replace").read()
ui, server, helpers = rd("inst/app/ui.R"), rd("inst/app/server.R"), rd("R/ui-helpers.R")
app_all = ui + "\n" + server + "\n" + helpers + "\n" + "\n".join(open(f, encoding="utf-8", errors="replace").read() for f in glob.glob(os.path.join(root, "R/*.R")))
issues = []
INPUT_FUNS = r"(?:save_button|selectInput|selectizeInput|numericInput|textInput|textAreaInput|checkboxInput|checkboxGroupInput|radioButtons|sliderInput|actionButton|actionLink|fileInput|dateInput|dateRangeInput|passwordInput|varSelectInput)"
defined_inputs = set(re.findall(INPUT_FUNS + r"\(\s*(?:inputId\s*=\s*)?[\"']([^\"']+)[\"']", app_all))
defined_inputs |= set(re.findall(r"\b(?:sidebarMenu|tabsetPanel|tabBox|navbarPage|navlistPanel)\([^)]*?\bid\s*=\s*[\"']([^\"']+)[\"']", app_all))
defined_inputs |= set(re.findall(r"setInputValue\(\s*[\"']([^\"']+)[\"']", app_all + "".join(open(f, errors="replace").read() for f in glob.glob(os.path.join(root, "inst/app/www/*.js")))))
defined_inputs |= set(re.findall(r"\b(?:click|dblclick|hover|brush)\s*=\s*[\"']([^\"']+)[\"']", app_all))
# ids built with paste0("prefix", ...) are dynamic: accept the prefix
dyn_prefixes = set(re.findall(INPUT_FUNS + r"\(\s*paste0\(\s*[\"']([^\"']+)[\"']", app_all))
used_inputs = set(re.findall(r"input\$([A-Za-z0-9_.]+)", server)) | set(re.findall(r"input\[\[\s*[\"']([^\"']+)[\"']\s*\]\]", server))
dyn_reads = set(re.findall(r"input\[\[\s*paste0\(\s*[\"']([^\"']+)[\"']", server + helpers))
for i in sorted(used_inputs - defined_inputs):
    if not any(i.startswith(p) for p in dyn_prefixes):
        issues.append(("ERROR", f"input${i} is read by the server but no UI element creates it"))
for i in sorted(defined_inputs - used_inputs):
    if any(i.startswith(p) for p in dyn_reads): continue
    if not re.search(r"[\"']" + re.escape(i) + r"[\"']", server.replace(f'"{i}"', "", 0)) and f"input${i}" not in app_all:
        issues.append(("NOTE", f"input '{i}' is created but never read"))
OUT_FUNS = r"(?:uiOutput|plotOutput|tableOutput|textOutput|verbatimTextOutput|htmlOutput|imageOutput|valueBoxOutput|infoBoxOutput|downloadButton|downloadLink|dataTableOutput|DTOutput)"
placed = set(re.findall(OUT_FUNS + r"\(\s*(?:outputId\s*=\s*)?[\"']([^\"']+)[\"']", app_all))
defined_out = set(re.findall(r"output\$([A-Za-z0-9_.]+)\s*<-", server)) | set(re.findall(r"output\[\[\s*[\"']([^\"']+)[\"']\s*\]\]\s*<-", server))
dyn_out = set(re.findall(r"output\[\[\s*paste0\(\s*[\"']([^\"']+)[\"']", server))
for o in sorted(placed - defined_out):
    if not any(o.startswith(p) for p in dyn_out):
        issues.append(("ERROR", f"output '{o}' is placed in the UI but the server never defines it (renders blank)"))
for o in sorted(defined_out - placed):
    issues.append(("NOTE", f"output${o} is defined but never placed in the UI"))
# outputs that the app tests read must exist in the server
for tf in ("tests/scripts/app_test.R", "tests/scripts/stress_test.R"):
    tp = os.path.join(root, tf)
    if not os.path.exists(tp): continue
    ts = open(tp, encoding="utf-8", errors="replace").read()
    for m in re.finditer(r"touch\(\s*output[^,]*,\s*c\(", ts):
        i = m.end(); depth = 1; j = i
        while depth and j < len(ts):
            depth += {"(": 1, ")": -1}.get(ts[j], 0); j += 1
        for nm in set(re.findall(r'"([A-Za-z0-9_.]+)"', ts[i:j])) - defined_out:
            if not any(nm.startswith(p) for p in dyn_out):
                issues.append(("ERROR", f"{tf} reads output '{nm}', which the server does not define"))
# info panels
info_keys = set(re.findall(r"^\s{2}([A-Za-z0-9_]+)\s*=\s*(?:c\()?info_entry\(", helpers, re.M)) | set(re.findall(r"^INFO\$([A-Za-z0-9_]+)\s*<-\s*info_entry\(", helpers, re.M))
refs = set(re.findall(r"(?:info_title|plain_title)\([^()]*?(?:\([^()]*\)[^()]*?)*,\s*[\"']([A-Za-z0-9_]+)[\"']\s*\)", ui + server))
refs |= set(re.findall(r"info_button\(\s*[\"']([A-Za-z0-9_]+)[\"']", ui + server + helpers))
for k in sorted(refs - info_keys):
    issues.append(("ERROR", f"info panel '{k}' is referenced but INFO has no such entry"))
# examples: every mapped column exists in its CSV
imp = rd("R/import.R")
tree, _ = parse(imp, "R/import.R")
examples = []
def find_examples(n):
    if n[0] == "assign" and n[2][0] == "sym" and n[2][1] == "EXAMPLES":
        for key, val, _ in n[3][2]:
            examples.append((key, val))
walk(tree, find_examples)
def strs(node):
    out = []
    walk(node, lambda m: out.append(m[1]) if m[0] == "const" and isinstance(m[1], str) and not m[1][:1].isdigit() and m[1] not in ("TRUE", "FALSE", "NULL", "NA") else None)
    return out
MAPCOLS = {"id", "age", "trait", "alr", "life", "entry", "condition", "covars", "cov_factor", "cov_int", "cov_age", "group", "group2", "random", "censor", "trials"}
for key, val in examples:
    args = {nm: v for nm, v, _ in val[2]} if val[0] == "call" else {}
    fnode = args.get("file")
    fname = fnode[1] if fnode and fnode[0] == "const" else None
    path = os.path.join(root, "inst/app/data", fname or "")
    if not fname or not os.path.exists(path):
        issues.append(("ERROR", f"example {key}: data file {fname} not found")); continue
    with open(path, encoding="utf-8", errors="replace") as fh:
        cols = next(csv.reader(fh))
    mp = args.get("mapping")
    if mp and mp[0] == "call":
        for nm, v, _ in mp[2]:
            if nm in MAPCOLS:
                for s in strs(v):
                    for c in s.split("|||"):
                        if c and not c.startswith("__") and c not in cols:
                            issues.append(("ERROR", f"example {key}: mapped column '{c}' ({nm}) is not in {fname}"))
# Collate and exports
desc = rd("DESCRIPTION")
coll = re.findall(r"'([^']+\.R)'", desc.split("Collate:")[1]) if "Collate:" in desc else []
for f in sorted(os.path.basename(p) for p in glob.glob(os.path.join(root, "R/*.R"))):
    if coll and f not in coll:
        issues.append(("ERROR", f"R/{f} is not in DESCRIPTION Collate, so it is not loaded"))
ns = rd("NAMESPACE")
for e in re.findall(r"export\(([^)]+)\)", ns):
    if not re.search(r"^" + re.escape(e) + r"\s*<-\s*function", app_all, re.M):
        issues.append(("ERROR", f"NAMESPACE exports {e} but no such function is defined"))
for p in ("grDevices", "graphics", "methods"):
    if re.search(p + r"::", "\n".join(open(f, errors="replace").read() for f in glob.glob(os.path.join(root, "R/*.R")))) and not re.search(r"\b" + p + r"\b", desc.split("Suggests:")[0]):
        issues.append(("WARN", f"R/ uses {p}:: but DESCRIPTION Imports does not declare {p} (R CMD check warning)"))
for s, m in issues:
    print(f"{s:5s} {m}")
n_err = sum(s == 'ERROR' for s, _ in issues)
print(f"-- {sum(s == 'ERROR' for s, _ in issues)} errors, {sum(s == 'WARN' for s, _ in issues)} warnings, {sum(s == 'NOTE' for s, _ in issues)} notes; "
      f"{len(used_inputs)} inputs read, {len(defined_out)} outputs, {len(info_keys)} info entries, {len(examples)} examples checked")
sys.exit(1 if n_err else 0)
