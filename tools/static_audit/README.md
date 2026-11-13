# Static audit (no R needed)

`python3 tools/static_audit/run_all.py` checks a disappR source tree without needing to load it. It complements the R test suite, and was written for
0.20.2, which was prepared in an environment without R. It needs Python 3 with numpy and pandas.

| Step | What it checks |
|---|---|
| `parser_selftest.py` | The R parser accepts 49 valid snippets and rejects 15 invalid ones. |
| `syntax_check.py` | Parses every `.R` file and the R chunks of every `.Rmd` with a lexer and Pratt parser that follow R's grammar (`rparse.py`). It reports syntax errors and lines that start with `+`, a broken continuation. |
| `xref.py` | Resolves every function call through local scopes, the package and the app. It matches the arguments against the called function's formals as R does (exact names, then partial names, then positions) and reports unused, surplus, ambiguous and empty arguments, and calls to functions that exist nowhere. `builtins_r.py` lists known external functions. |
| `wiring.py` | Shiny inputs read but never created, and outputs placed but never defined; help panels that do not exist; example mappings that name columns missing from their CSV; files missing from Collate; exports without a function; `pkg::` uses not declared in Imports. |
| `example_conflicts.py`, `stress_conflicts.py` | Individuals whose mapped ALR, lifespan or AFR differs between records (with IDs built as the app builds them), which would stop loading. |

Parsing is checked against 0.19.6, which loads in R: all 66 files are accepted. A clean audit means none of these
classes of error were found. It does not mean the code runs or computes the right answer: run the R test tiers
(`tests/scripts/run_all.R`) for that.
