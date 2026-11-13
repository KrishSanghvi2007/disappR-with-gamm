"""For every bundled example: build individual IDs as standardise_data() does (group/ID when nested) and list
individuals whose mapped ALR, lifespan or AFR column holds more than one value."""
import sys, os, csv
import pandas as pd, numpy as np
from rparse import parse
from xref import walk
root = sys.argv[1]
tree, _ = parse(open(os.path.join(root, "R/import.R")).read(), "import.R")
examples = []
walk(tree, lambda n: examples.extend((k, v) for k, v, _ in n[3][2]) if n[0] == "assign" and n[2][0] == "sym" and n[2][1] == "EXAMPLES" else None)
def val(node):
    if node is None: return None
    if node[0] == "const": return node[1]
    if node[0] == "call" and node[1][1] == "c": return [val(v) for _, v, _ in node[2]]
    return None
unhandled = 0
for key, ex in examples:
    a = {nm: v for nm, v, _ in ex[2]}
    mp = a.get("mapping")
    if not mp or mp[0] != "call" or mp[1][0] != "sym" or mp[1][1] != "example_map":
        print(f"{key:24s} mapping is not a literal example_map() call: {mp[0] if mp else None} {mp[1] if mp else ''}")
        continue
    m = {nm: val(v) for nm, v, _ in mp[2]}
    df = pd.read_csv(os.path.join(root, "inst/app/data", val(a["file"])), dtype=str, keep_default_na=False)
    ids = df[m["id"]].astype(str)
    g = m.get("group") or ""
    if g and m.get("nested", "TRUE") != "FALSE" and g in df:
        ids = np.where(df[g].isin(["", "NA"]), ids, df[g].astype(str) + "/" + ids)
    out = []
    for role in ("alr", "life", "entry"):
        col = m.get(role) or ""
        if not col or col.startswith("__") or col not in df: continue
        x = pd.to_numeric(df[col], errors="coerce")
        rng = x.groupby(ids).agg(lambda s: s.max() - s.min() if s.notna().any() else 0)
        bad = sorted(rng[rng > 1e-8 * max(1, np.nanmax(np.abs(x)))].index)
        if bad: out.append(f"{role}={col}: {len(bad)} [{', '.join(bad[:4])}{', ...' if len(bad) > 4 else ''}]")
    sub = " (example has a subset)" if "subset" in a else ""
    if out and m.get("inconsistent") != "exclude": unhandled += 1
    print(f"{key:24s} {'; '.join(out) if out else 'consistent'}{sub}  inconsistent={m.get('inconsistent') or 'error'}")
print(f"-- {unhandled} example(s) with conflicts that would stop loading")
sys.exit(1 if unhandled else 0)
