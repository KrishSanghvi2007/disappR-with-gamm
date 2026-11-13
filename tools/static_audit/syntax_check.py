import sys, glob, os
from rparse import parse, RSyntaxError
def check_tree(root):
    files = sorted(glob.glob(f"{root}/**/*.R", recursive=True) + glob.glob(f"{root}/**/*.Rmd", recursive=True))
    errors, lints, n = [], [], 0
    for f in files:
        src = open(f, encoding="utf-8", errors="replace").read()
        if f.endswith(".Rmd"):
            # R chunks only
            chunks, keep, out = [], False, []
            for ln in src.split("\n"):
                s = ln.strip()
                if s.startswith("```{r"): keep = True; out.append(""); continue
                if keep and s.startswith("```"): keep = False; out.append(""); continue
                out.append(ln if keep else "")
            src = "\n".join(out)
        n += 1
        try:
            _, lt = parse(src, os.path.relpath(f, root))
            lints += [(os.path.relpath(f, root), l, m) for l, m in lt]
        except RSyntaxError as e:
            errors.append(str(e))
    return n, errors, lints
if __name__ == "__main__":
    for root in sys.argv[1:]:
        n, errors, lints = check_tree(root)
        print(f"== {root}: {n} files, {len(errors)} syntax errors, {len(lints)} lints")
        for e in errors: print("  ERROR", e)
        for f, l, m in lints: print(f"  lint {f}:{l}: {m}")
        if errors: sys.exit(1)
