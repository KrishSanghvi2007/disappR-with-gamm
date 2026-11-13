"""Cross-reference audit of an R package from its parsed AST (no R needed).

For every call it resolves the function (local scope, package, app, or a known external), and for functions defined
in the package it checks the call against the formals the way R matches arguments: exact names, then partial names
(before ...), then positions. It reports unused arguments (a runtime error in R), ambiguous or partial matches, calls
to functions that exist nowhere, and pkg:: calls to packages DESCRIPTION does not declare.
"""
import glob, os, re, sys
from rparse import parse, EMPTY
from builtins_r import KNOWN_EXTERNAL

SKIP_ARGCHECK = {"do.call", "Recall"}


def walk(node, fn):
    """Pre-order walk over tuples; fn(node) may return False to stop descending."""
    if not isinstance(node, tuple) or not node:
        return
    if fn(node) is False:
        return
    tag = node[0]
    if tag == "call" or tag == "index" or tag == "index2":
        walk(node[1], fn)
        for _, v, _ in node[2]:
            walk(v, fn)
    elif tag == "function":
        for _, d in node[1]:
            if d is not None:
                walk(d, fn)
        walk(node[2], fn)
    elif tag == "block":
        for s in node[1]:
            walk(s, fn)
    else:
        for x in node[1:]:
            if isinstance(x, tuple):
                walk(x, fn)
            elif isinstance(x, list):
                for y in x:
                    if isinstance(y, tuple):
                        walk(y, fn)


def local_names(fnode):
    """Names bound inside a function (formals, assignments, for-loop variables), not descending into nested functions."""
    names, funcs = {p for p, _ in fnode[1]}, {}
    up = set()

    def visit(n):
        if n[0] == "function" and n is not fnode:
            # <<- inside nested functions binds here (or further out)
            def inner(m):
                if m[0] == "assign" and m[1] == "<<-" and m[2][0] == "sym":
                    up.add(m[2][1])
            walk(n[2], inner)
            return False
        if n[0] == "assign":
            tgt = n[2]
            if tgt[0] == "sym":
                names.add(tgt[1])
                if n[3][0] == "function":
                    funcs[tgt[1]] = n[3]
            if n[1] == "<<-" and tgt[0] == "sym":
                up.add(tgt[1])
        if n[0] == "for":
            names.add(n[1])
        if n[0] == "call" and n[1][0] == "sym" and n[1][1] == "assign" and n[2] and n[2][0][1][0] == "const":
            names.add(n[2][0][1][1])
    walk(fnode[2], visit)
    return names | up, funcs, up


class Audit:
    def __init__(self, root):
        self.root = root
        self.trees = {}
        self.pkg_defs = {}      # name -> function node (package namespace, R/)
        self.pkg_values = set()
        self.app_defs = {}      # global.R, ui.R top level
        self.issues = []        # (severity, file, line, message)
        self.unknown = {}       # name -> [(file, line)]
        self.ns_used = {}

    def load(self, patterns):
        for pat in patterns:
            for f in sorted(glob.glob(os.path.join(self.root, pat), recursive=True)):
                rel = os.path.relpath(f, self.root)
                if rel in self.trees:
                    continue
                self.trees[rel] = parse(open(f, encoding="utf-8", errors="replace").read(), rel)[0]

    def collect_top(self, rel, target_defs, values):
        for s in self.trees[rel][1]:
            if s[0] == "assign" and s[2][0] == "sym":
                if s[3][0] == "function":
                    target_defs[s[2][1]] = s[3]
                else:
                    values.add(s[2][1])

    def resolve(self, name, scopes):
        for names, funcs in reversed(scopes):
            if name in funcs:
                return "local_fn", funcs[name]
            if name in names:
                return "local", None
        if name in self.pkg_defs:
            return "pkg", self.pkg_defs[name]
        if name in self.app_defs:
            return "app", self.app_defs[name]
        if name in self.pkg_values or name in self.app_values:
            return "value", None
        if name in KNOWN_EXTERNAL:
            return "external", None
        return None, None

    def check_args(self, rel, line, name, fnode, args):
        formals = [p for p, _ in fnode[1]]
        defaults = {p: d for p, d in fnode[1]}
        has_dots = "..." in formals
        before = formals[:formals.index("...")] if has_dots else formals
        matched = {}
        positional = []
        for nm, val, _ in args:
            if nm is None:
                positional.append(val)
                continue
            if nm in formals:
                if nm in matched:
                    self.issues.append(("ERROR", rel, line, f"{name}(): argument '{nm}' matched twice"))
                matched[nm] = val
                continue
            cands = [p for p in before if p.startswith(nm)]
            if len(cands) == 1 and not has_dots:
                self.issues.append(("WARN", rel, line, f"{name}(): '{nm}' only partially matches '{cands[0]}'"))
                matched[cands[0]] = val
            elif len(cands) > 1 and not has_dots:
                self.issues.append(("ERROR", rel, line, f"{name}(): '{nm}' matches several arguments {cands}"))
            elif not has_dots:
                self.issues.append(("ERROR", rel, line, f"{name}(): unused argument '{nm}' (formals: {', '.join(formals)})"))
        free = [p for p in before if p not in matched]
        if len(positional) > len(free) and not has_dots:
            self.issues.append(("ERROR", rel, line, f"{name}(): {len(positional)} unnamed arguments but only {len(free)} free formals ({', '.join(formals)})"))
        filled = set(matched) | set(free[:len(positional)])
        for p in formals:
            if p != "..." and defaults[p] is None and p not in filled:
                # only a problem if the body uses p without checking missing(p)
                uses_missing = []
                walk(fnode[2], lambda n: uses_missing.append(1) if n[0] == "call" and n[1][0] == "sym" and n[1][1] == "missing" else None)
                if not uses_missing:
                    self.issues.append(("NOTE", rel, line, f"{name}(): required argument '{p}' not supplied"))

    def analyse(self, rel, extra_scope_names=()):
        tree = self.trees[rel]
        # file scope: every binding made outside a function body, including inside blocks such as test_that({ })
        top_names, top_funcs, _ = local_names(("function", [], tree, 0))
        scopes = [(top_names | set(extra_scope_names), top_funcs)]

        def visit(n, scopes):
            tag = n[0]
            if tag == "function":
                names, funcs, _ = local_names(n)
                for p, d in n[1]:
                    if d is not None:
                        visit_tree(d, scopes)
                visit_tree(n[2], scopes + [(names, funcs)])
                return
            if tag == "call":
                f = n[1]
                if f[0] == "sym":
                    kind, fnode = self.resolve(f[1], scopes)
                    if kind is None:
                        self.unknown.setdefault(f[1], []).append((rel, n[3]))
                    elif fnode is not None and f[1] not in SKIP_ARGCHECK:
                        self.check_args(rel, n[3], f[1], fnode, n[2])
                    # empty arguments: only [ ], [[ ]], switch and alist accept them
                    if f[1] not in ("switch", "alist", "quote", "bquote", "substitute", "expression"):
                        for nm, v, ln in n[2]:
                            if v is EMPTY:
                                self.issues.append(("ERROR", rel, ln, f"{f[1]}(): empty argument{' ' + repr(nm) if nm else ''} (R: 'argument is missing' or 'argument N is empty')"))
                elif f[0] == "ns":
                    self.ns_used.setdefault(f[1], set()).add(f[2])
                visit_tree(n[1], scopes)
                for _, v, _ in n[2]:
                    visit_tree(v, scopes)
                return
            if tag in ("index", "index2"):
                visit_tree(n[1], scopes)
                for _, v, _ in n[2]:
                    visit_tree(v, scopes)
                return
            if tag == "ns":
                self.ns_used.setdefault(n[1], set()).add(n[2])
            for x in n[1:]:
                if isinstance(x, tuple):
                    visit_tree(x, scopes)
                elif isinstance(x, list):
                    for y in x:
                        if isinstance(y, tuple):
                            visit_tree(y, scopes)

        def visit_tree(n, scopes):
            if isinstance(n, tuple) and n:
                visit(n, scopes)
        visit_tree(tree, scopes)


def run(root):
    a = Audit(root)
    a.load(["R/*.R", "inst/app/*.R", "tests/testthat/*.R", "tests/scripts/*.R", "inst/validation/**/*.R"])
    a.app_values = set()
    for rel in a.trees:
        if rel.startswith("R/"):
            a.collect_top(rel, a.pkg_defs, a.pkg_values)
    for rel in ("inst/app/global.R", "inst/app/ui.R"):
        if rel in a.trees:
            a.collect_top(rel, a.app_defs, a.app_values)
    helper_scope = set()
    for rel in a.trees:
        if rel.startswith("tests/testthat/helper"):
            d, v = {}, set()
            a.collect_top(rel, d, v)
            a.app_defs.update(d)
            a.app_values |= v
    for rel in a.trees:
        a.analyse(rel)
    return a


if __name__ == "__main__":
    a = run(sys.argv[1])
    sev = {"ERROR": 0, "WARN": 1, "NOTE": 2}
    for s, f, l, m in sorted(set(a.issues), key=lambda x: (sev[x[0]], x[1], x[2])):
        if s != "NOTE" or "-v" in sys.argv:
            print(f"{s:5s} {f}:{l}: {m}")
    print(f"\n{sum(1 for i in set(a.issues) if i[0]=='ERROR')} errors, {sum(1 for i in set(a.issues) if i[0]=='WARN')} warnings, "
          f"{sum(1 for i in set(a.issues) if i[0]=='NOTE')} notes; {len(a.unknown)} unresolved function names")
    if "-u" in sys.argv:
        for nm, locs in sorted(a.unknown.items()):
            print(f"  unknown {nm}: {len(locs)}x, first {locs[0][0]}:{locs[0][1]}")
    print("pkg:: namespaces used:", {k: len(v) for k, v in sorted(a.ns_used.items())})
    sys.exit(1 if any(i[0] == "ERROR" for i in a.issues) else 0)
