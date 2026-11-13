"""Static audit of the disappR package: checks that need no R runtime.

    python3 tools/static_audit/run_all.py [package_root]

Exits non-zero if any step finds an error. It catches whole classes of errors (syntax, unused or misnamed
arguments, missing helpers, broken Shiny wiring, wrong example columns, individual-level conflicts in bundled data,
undeclared imports) but it cannot show that the code computes the right answer: run the R test tiers for that.
"""
import os, subprocess, sys
HERE = os.path.dirname(os.path.abspath(__file__))
root = os.path.abspath(sys.argv[1]) if len(sys.argv) > 1 else os.path.abspath(os.path.join(HERE, "..", ".."))
steps = [("parser self-test", ["parser_selftest.py"]), ("R syntax", ["syntax_check.py", root]),
         ("calls against formals", ["xref.py", root]), ("app wiring, help panels, example columns, DESCRIPTION", ["wiring.py", root]),
         ("individual-level conflicts in the examples", ["example_conflicts.py", root]),
         ("individual-level conflicts in the stress data", ["stress_conflicts.py", root])]
failed = []
for name, cmd in steps:
    r = subprocess.run([sys.executable, os.path.join(HERE, cmd[0])] + cmd[1:], capture_output=True, text=True, cwd=HERE)
    print(f"== {name}: {'ok' if r.returncode == 0 else 'FAILED'}")
    print("\n".join("   " + l for l in (r.stdout + r.stderr).strip().splitlines()[-12:]))
    if r.returncode != 0:
        failed.append(name)
print("\nstatic audit:", "all steps passed" if not failed else "FAILED: " + ", ".join(failed))
sys.exit(1 if failed else 0)
