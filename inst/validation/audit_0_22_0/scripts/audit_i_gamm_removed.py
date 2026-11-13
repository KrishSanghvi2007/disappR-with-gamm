"""Part I of the audit (0.24.0): the GAMM layer (0.23.0-0.23.2) is held back from this release. Checks that it was
removed completely and that nothing else changed.
I1 no residue: no GAMM function, input, output, help key, package or file left in the package, app, tests or workflows
I2 removal only: compared with 0.23.2, every changed file lost only GAMM code; every removed block contains GAMM code;
   the only added lines are the re-punctuated last menu item, the reflowed workflow comment and the version number
I3 the engine is otherwise untouched: every R/ file other than gamm.R, export.R and ui-helpers.R is byte-identical
Set OLD to the 0.23.2 source tree (default /home/claude/work/disappR)."""
import os, re, sys, difflib
sys.path.insert(0, os.path.dirname(__file__))
import port022 as P
ROOT = P.ROOT
OLD = os.environ.get("OLD", "/home/claude/work/disappR")
fails = []
def check(label, ok, detail=""):
    print(("PASS " if ok else "FAIL ") + label + (": " + detail if detail else ""))
    if not ok: fails.append(label)
GAMM = re.compile(r"\bgamm|GAMM|\bmgcv\b|gamm4|pedersen2019|wood2017", re.I)
def is_gamm(line): return bool(GAMM.search(line.replace("gamma", "").replace("Gamma", "")))
SCOPE = ["R", "inst/app", "tests", ".github", "DESCRIPTION", "NAMESPACE", "man", "vignettes", "README.md", "VALIDATION.md", "REPLICATION.md"]
def files(root):
    out = []
    for s in SCOPE:
        p = os.path.join(root, s)
        if os.path.isfile(p): out.append(s)
        elif os.path.isdir(p):
            for d, _, fs in os.walk(p):
                for f in fs:
                    if f.endswith((".R", ".r", ".Rmd", ".md", ".yaml", ".yml", ".Rd", ".css", ".js")) or f in ("DESCRIPTION", "NAMESPACE"):
                        out.append(os.path.relpath(os.path.join(d, f), root))
    return sorted(set(out))

print("== I1 no GAMM residue")
new_files = files(ROOT); old_files = files(OLD)
resid = [(f, i + 1, l.strip()[:80]) for f in new_files for i, l in enumerate(open(os.path.join(ROOT, f), encoding="utf-8", errors="replace"))
         if is_gamm(l)]
check("no GAMM code, input, output, help key or package in the package, app, tests or workflows", not resid, "%d lines" % len(resid) + ("; e.g. %s:%d %s" % resid[0] if resid else ""))
gone = sorted(set(old_files) - set(new_files))
check("the GAMM files are gone", gone == ["R/gamm.R", "tests/scripts/gamm_vs_glmm.R", "tests/testthat/test-gamm.R"], ", ".join(gone))
check("no file was added", not (set(new_files) - set(old_files)), ", ".join(sorted(set(new_files) - set(old_files))))

print("\n== I2 removal only")
ALLOWED_ADDED = [r'menuItem\("6 \\u00b7 Summary and report", tabName = "summary", icon = icon\("square-check"\)\)$',
                 r"^# The script test tiers \(smoke, robustness, app server, stress, empirical, sweep, complex scenarios\) through$",
                 r"^# tests/scripts/run_all.R: weekly, and on demand from the Actions tab \(0.23.2; code review\).$",
                 r"^Version: 0\.24\.0$", r"^\}$"]
bad_add, bad_hunk, changed = [], [], []
for f in sorted(set(old_files) & set(new_files)):
    a = open(os.path.join(OLD, f), encoding="utf-8", errors="replace").read().splitlines()
    b = open(os.path.join(ROOT, f), encoding="utf-8", errors="replace").read().splitlines()
    if a == b: continue
    changed.append(f)
    sm = difflib.SequenceMatcher(None, a, b, autojunk=False)
    for tag, i1, i2, j1, j2 in sm.get_opcodes():
        if tag == "equal": continue
        removed, added = a[i1:i2], b[j1:j2]
        def shortened(l):   # the added line is an old line with items deleted (e.g. "gamm.R" dropped from a list)
            tok = lambda t: re.findall(r'"[^"]*"|[A-Za-z_][A-Za-z0-9_.]*|\S', t)
            w = tok(l)
            for r in removed + [" ".join(removed)]:
                it = iter(tok(r))
                if is_gamm(r) and all(any(x == y for y in it) for x in w): return True
            return False
        for l in added:
            if not any(re.search(p, l.strip()) for p in ALLOWED_ADDED) and not shortened(l): bad_add.append((f, l.strip()[:90]))
        if removed and not any(is_gamm(l) for l in removed) and not (f == "DESCRIPTION" and all(l.startswith("Version") for l in removed)) \
           and not (f.endswith("script-tiers.yaml")):
            bad_hunk.append((f, i1 + 1, removed[0].strip()[:80]))
print("   files changed: " + ", ".join(changed))
check("every removed block contains GAMM code", not bad_hunk, "; ".join("%s:%d %s" % x for x in bad_hunk[:3]))
check("nothing was added except shortened lists, the re-punctuated menu item, the reflowed comment and the version", not bad_add, "; ".join("%s: %s" % x for x in bad_add[:3]))

print("\n== I3 engine otherwise untouched")
same = [f for f in new_files if f.startswith("R/") and f not in ("R/export.R", "R/ui-helpers.R")]
diff = [f for f in same if open(os.path.join(ROOT, f), "rb").read() != open(os.path.join(OLD, f), "rb").read()]
check("every other R/ file is byte-identical to 0.23.2", not diff, ", ".join(diff))
old_fns = set(re.findall(r"^([A-Za-z_.][A-Za-z0-9_.]*) <- function", "".join(open(os.path.join(OLD, f), encoding="utf-8").read() for f in old_files if f.startswith("R/") and f != "R/gamm.R"), re.M))
new_fns = set(re.findall(r"^([A-Za-z_.][A-Za-z0-9_.]*) <- function", "".join(open(os.path.join(ROOT, f), encoding="utf-8").read() for f in new_files if f.startswith("R/")), re.M))
check("every engine function outside gamm.R is still defined", old_fns == new_fns, "missing: " + ", ".join(sorted(old_fns - new_fns)) if old_fns - new_fns else "%d functions" % len(new_fns))
print("\n%d failure(s)%s" % (len(fails), (": " + "; ".join(fails)) if fails else ""))
