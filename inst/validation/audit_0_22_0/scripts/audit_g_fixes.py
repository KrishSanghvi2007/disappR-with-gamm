"""Part G of the audit (0.23.1), kept in 0.24.0 without its GAMM comparisons (which left with the GAMM layer).
G1  the review's ten correctness findings and the easy recommendations: each fix checked in the source."""
import os, sys
sys.path.insert(0, os.path.dirname(__file__))
import port022 as P
ROOT = P.ROOT; APP = os.path.join(ROOT, "inst", "app")
fails = []
def check(label, ok, detail=""):
    print(("PASS " if ok else "FAIL ") + label + (": " + detail if detail else ""))
    if not ok: fails.append(label)

if True:
    print("== G1 the review's findings, fixed in 0.23.1")
    server = open(os.path.join(APP, "server.R"), encoding="utf-8").read()
    glob = open(os.path.join(APP, "global.R"), encoding="utf-8").read()
    runapp = open(os.path.join(ROOT, "R", "run_app.R"), encoding="utf-8").read()
    diag = open(os.path.join(ROOT, "R", "diagnostics.R"), encoding="utf-8").read()
    export = open(os.path.join(ROOT, "R", "export.R"), encoding="utf-8").read()
    modelfit = open(os.path.join(ROOT, "R", "model-fit.R"), encoding="utf-8").read()
    api = open(os.path.join(ROOT, "R", "api.R"), encoding="utf-8").read()
    formulas = open(os.path.join(ROOT, "R", "formulas.R"), encoding="utf-8").read()
    uitest = open(os.path.join(ROOT, "tests", "testthat", "test-ui-builds.R"), encoding="utf-8").read()
    check("1 the random-slope warning reads the fitted structure", "normalise_slope(r$random_structure" in server and "slope_fitted <- !identical(normalise_slope(input$random_structure" not in server)
    check("2 the suggested analysis offers Model 6 only with a known lifespan (the whole suggestion was removed in 0.24.7)",
          "suggested_settings" not in server or ('has_life = isTRUE(m$has_life) && !isTRUE(m$life_auto)' in server))
    check("3 the bootstrap caution prints the number that refitted", "1 / ((z$n_ok %||% 0L) + 1)" in server and "z$n_perm, 1 / (z$n_perm + 1)" not in server)
    check("4 the checksum is of the file the app loaded", 'f_loaded <- file.path("data", current_example()$file)' in server)
    check("5 one upload limit, set in one place", "50 * 1024^2" in glob and "maxRequestSize" not in runapp)
    check("6 the random-slope support check covers the _all structures", '"correlated_all", "uncorrelated_all"' in diag)
    check("7 no message says duplicates were averaged", "averaged duplicate" not in export and "Averaging duplicate" not in modelfit and "nothing is averaged" in api)
    check("8 the mean-age note is for Models 3 and 5 only", 'c("M3", "M5"), function(m) m %in% names(r$formulas)' in server)
    check("9 the model script is written as UTF-8", 'file(file, open = "w", encoding = "UTF-8")' in server)
    check("10 the layout test skips without the app sources", 'file.exists(file.path(pkg_root, "inst", "app", "global.R"))' in uitest)
    check("unused server code removed (summary_parts, overview_text, d45)", all(x not in server for x in ("summary_parts", "overview_text", "d45 <-")))
    check("automatic checks cached by signature", "auto_check_cache[[key]]" in server and server.count("assign(key, res, envir = auto_check_cache)") == 2)
    check("exported scripts use the public reader", "disappR:::read_user_csv" not in export and "disappR::disappr_read(" in export)
    check("model names from one source", "MODEL_MEANING[[m]]$name" in formulas)
    check("stale 'moved verbatim' headers gone", not any("Moved verbatim" in open(os.path.join(ROOT, "R", f), encoding="utf-8").read() for f in os.listdir(os.path.join(ROOT, "R")) if f.endswith(".R")))
    check(".Rhistory removed and ignored", not os.path.exists(os.path.join(ROOT, ".Rhistory")) and ".Rhistory" in open(os.path.join(ROOT, ".gitignore")).read())

print("\n%d failure(s)%s" % (len(fails), (": " + "; ".join(fails)) if fails else ""))
