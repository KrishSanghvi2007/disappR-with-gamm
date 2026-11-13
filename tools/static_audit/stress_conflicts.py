import sys, os
import pandas as pd, numpy as np
from rparse import parse
from xref import walk
root = sys.argv[1]
src = open(os.path.join(root, "tests/scripts/stress_test.R"), encoding="utf-8", errors="replace").read()
tree, _ = parse(src, "stress_test.R")
entries = []
def find(n):
    if n[0] == "call" and n[1][0] == "sym" and n[1][1] == "list":
        names = {nm for nm, _, _ in n[2]}
        if "file" in names and "map" in names:
            entries.append(n)
walk(tree, find)
def val(node):
    if node is None: return None
    if node[0] == "const": return node[1]
    if node[0] == "call" and node[1][0] == "sym" and node[1][1] == "c": return [val(v) for _, v, _ in node[2]]
    return None
for e in entries:
    a = {nm: v for nm, v, _ in e[2]}
    f = val(a["file"]); mp = a["map"]
    m = {nm: val(v) for nm, v, _ in mp[2]} if mp[0] == "call" else {}
    path = os.path.join(root, "tests/stress_data", f)
    try:
        df = None
        for enc, sep in (("utf-8", ","), ("latin-1", ";"), ("utf-8", ";")):
            try:
                d = pd.read_csv(path, dtype=str, keep_default_na=False, encoding=enc, sep=sep)
                if m.get("id") in d.columns: df = d; break
            except Exception: pass
        if df is None: print(f"{f:40s} (could not read with the mapped columns)"); continue
    except FileNotFoundError:
        print(f"{f:40s} missing"); continue
    ids = df[m["id"]].astype(str)
    g = m.get("group") or ""
    if g and g in df and m.get("nested", "TRUE") != "FALSE":
        ids = np.where(df[g].isin(["", "NA"]), ids, df[g].astype(str) + "/" + ids)
    out = []
    for role in ("alr", "life", "entry"):
        col = m.get(role) or ""
        if not isinstance(col, str) or not col or col.startswith("__") or col not in df: continue
        x = pd.to_numeric(df[col].str.replace(",", "."), errors="coerce")
        rng = x.groupby(ids).agg(lambda s: s.max() - s.min() if s.notna().any() else 0)
        bad = sorted(rng[rng > 1e-8 * max(1, np.nanmax(np.abs(x)) if x.notna().any() else 1)].index)
        if bad: out.append(f"{role}={col}: {len(bad)} ({', '.join(map(str, bad[:3]))}...)")
    print(f"{f:40s} {'; '.join(out) if out else 'consistent'}")
