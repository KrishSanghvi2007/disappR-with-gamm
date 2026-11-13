# GAMM layer archive (from disappR 0.23.2)

Held back from 0.24.0. To reintroduce it: copy R/ and tests/ back into the package and validation/ into
inst/validation/audit_0_22_0/, then apply each patch in snippets/ (unified diffs from 0.24.0 to 0.23.2) from the
package root, e.g. `git apply snippets/*.diff` or `patch -p1 < snippets/<file>.diff`. They add the menu item, the tab,
the server block, the saved-result types, the methods paragraph, the help panels, the engine file, the Suggests
entries, the app-test outputs, run_all tier 9 and the workflow packages. Then rerun the static audit and audit Parts
F, G and I (Part I will then report the GAMM code as added, as expected).

Note: DESCRIPTION.diff also changes the version back to 0.23.2; set the version by hand instead of applying that hunk.
