"""Part D of the audit (0.22.7): wording. Could a user who only reads the boxes, labels and help panels know what to
do, what to run and how to read each result? Automated checks over every user-facing string, followed by a manual
read-through recorded in README section D.
D1 every box has a help panel (or needs none)
D2 every help panel says what it is, how to use or read it, and what to watch for
D3 every button, tab, box or setting named in a text exists with that name in the interface
D4 every abbreviation shown to users is defined in the glossary or where it is used
D5 no long sentence repeated across help panels
D6 every 'not available' message tells the user what to do
D7 sentence length: long sentences listed for review
D8 every help panel's 'how' says how to read the result, not only how to run it"""
import os, re, sys
sys.path.insert(0, os.path.dirname(__file__))
import port022 as P
ROOT = P.ROOT; APP = os.path.join(ROOT, "inst", "app")
ui = open(os.path.join(APP, "ui.R"), encoding="utf-8").read()
server = open(os.path.join(APP, "server.R"), encoding="utf-8").read()
helpers = P.SRC["ui-helpers.R"]
ENG = "".join(P.SRC.values())
fails = []
def check(label, ok, detail=""):
    print(("PASS " if ok else "FAIL ") + label + (": " + detail if detail else ""))
    if not ok: fails.append(label)
def unesc(s): return re.sub(r"\\u([0-9a-fA-F]{4})", lambda m: chr(int(m.group(1), 16)), s)
def strings(code):
    return [unesc(m.group(1)) for m in re.finditer(r'"((?:[^"\\]|\\.)*)"', code)]

# ---- the help panels ----
INFO = {}
blk = helpers[helpers.index("INFO <- list("):]
for m in re.finditer(r"\n  ([a-z_0-9]+) = info_entry\(", blk):
    start = m.end(); depth = 1; i = start
    while depth and i < len(blk):
        depth += (blk[i] == "(") - (blk[i] == ")"); i += 1
    body = blk[start:i - 1]
    parts = strings(body)
    more = re.search(r"more = (?:paste\()?\"", body) is not None
    INFO[m.group(1)] = dict(title=parts[0] if parts else "", texts=parts[1:], more=more, body=body)
print("== D1 help coverage: %d help panels" % len(INFO))
boxes = re.findall(r'box\((?:width = \d+, )?title = (?:info_title\("([^"]+)", "([^"]+)"\)|"([^"]+)")', ui)
no_help = [b[2] for b in boxes if b[2]]
used_keys = set(re.findall(r'info_title\("[^"]*", "([a-z_0-9]+)"\)', ui + server))
used_keys |= set(re.findall(r'actionLink\("info_([a-z_0-9]+)"', ui + server))   # direct help links (0.24.2)
print("   %d boxes, %d without a help panel: %s" % (len(boxes), len(no_help), "; ".join(no_help)))
unused = sorted(set(INFO) - used_keys - {"overview_auto"})
print("   help panels never linked from the interface: %s" % (", ".join(unused) or "none"))

print("\n== D2 each panel has what / how / caution")
thin = [k for k, v in INFO.items() if len(v["texts"]) < 3 or any(len(t) < 25 for t in v["texts"][:3])]
check("every help panel has three substantive parts", not thin, ", ".join(thin))

print("\n== D3 interface names quoted in texts exist")
labels = set(re.findall(r'actionButton\("[^"]+", "([^"]+)"', ui + server)) | {"Save to summary"}
labels |= set(re.findall(r'tabPanel\("([^"]+)"', ui)) | set(unesc(x).split(" \u00b7 ")[-1] for x in re.findall(r'menuItem\("([^"]+)"', ui))
labels |= set(re.findall(r'(?:checkboxInput|radioButtons|selectInput|selectizeInput|numericInput|sliderInput|downloadButton)\("[^"]+", (?:info_title\()?"([^"]+)"', ui + server))
labels |= set(re.findall(r'strong\("([^"]+)"\)', ui)) | set(re.findall(r'paste0\("([^"]+) \(", fn', server))
labels |= {"Exponential (a \u00b7 exp(b \u00b7 age))", "Poor", "Note", "Warning", "Caution", "Valid", "Failed", "P type", "Random slope", "Asymptotic exponential", "Unicode text",
           "IDs linked to >1 higher-level group"}   # table values, column names and function names quoted in texts
labels |= set(b[0] or b[2] for b in boxes)
labels |= set(re.findall(r'page_title\("([^"]+)"', ui)) | set(re.findall(r'h5\(strong\(info_title\("([^"]+)"', ui))
labels |= set(unesc(x) for x in re.findall(r'"([^"]+)" = "[a-z_]+"', ENG + ui))   # choice labels
all_text = [(k, t) for k, v in INFO.items() for t in v["texts"]] + [("server", t) for t in strings(server)] + [("engine", t) for t in strings(ENG)]
quoted = []
for src, t in all_text:
    for q in re.findall(r"'([A-Z][^']{3,60})'", t):
        quoted.append((src, q))
norm = lambda x: re.sub(r"\s*\([^)]*\)", "", unesc(x)).strip().lower()
bad = sorted({(s, q) for s, q in quoted if not any(norm(q) and (norm(q) in norm(l) or norm(l).startswith(norm(q)) or norm(q).startswith(norm(l)) and len(norm(l)) > 8) for l in labels)})
for s, q in bad: print("   not an interface name: '%s' (in %s)" % (q, s))
check("every quoted interface name exists", not bad, "%d of %d quoted names" % (len(bad), len(quoted)))
tab_refs = set(re.findall(r"\b(?:the|on the|in the) ([A-Z][a-z]+) tab\b", " ".join(t for _, t in all_text)))
bad_tabs = sorted(x for x in tab_refs if x not in labels and x.split()[0] not in labels)
check("every 'X tab' named exists", not bad_tabs, ", ".join(bad_tabs))

print("\n== D4 abbreviations")
gl = ui[ui.index('"Glossary of terms"'):ui.index('"Preview (first 10 rows)"')]
glossary = set(x.strip() for g in re.findall(r'strong\("([^"]+)"\)', gl) for x in unesc(g).split(","))
visible = " ".join(t for _, t in all_text if _ != "engine") + " " + " ".join(strings(ui))
abbr = sorted(set(re.findall(r"\b(A[A-Z]{1,4}c?|[A-Z]{2,5})\b", visible)) - {"ID", "CSV", "HTML", "PDF", "PNG", "SVG", "OK", "UI", "R", "NA", "II", "III", "IV", "URL"})
def defined(a):
    if a in glossary: return True
    pat = re.compile(r"\(%s\)|%s \(|%s:|%s means|%s is the|%s =|, %s," % ((re.escape(a),) * 7))
    return bool(pat.search(visible))
undef = [a for a in abbr if not defined(a)]
for a in undef:
    n = len(re.findall(r"\b%s\b" % re.escape(a), visible))
    print("   %-8s used %3d times, not defined" % (a, n))
print("   glossary terms: %s" % ", ".join(sorted(glossary)))

print("\n== D5 long sentences repeated across help panels")
sent = {}
for k, v in INFO.items():
    for t in v["texts"]:
        for s in re.split(r"(?<=[.!?])\s+", t):
            if len(s) > 60: sent.setdefault(s, set()).add(k)
rep = {s: ks for s, ks in sent.items() if len(ks) > 1}
for s, ks in rep.items(): print("   '%s...' in %s" % (s[:80], ", ".join(sorted(ks))))
check("no long sentence repeated across help panels", not rep, "%d repeated" % len(rep))

print("\n== D6 'not available' messages say what to do")
needs = [unesc(m) for m in re.findall(r'need\([^,]+,\s*"((?:[^"\\]|\\.)+)"\)', server)]
act = re.compile(r"\b(needs?|change|press|fit|choose|select|map|run|save|tick|untick|enter|upload|install|refit|add|remove|set|use|switch|pick|open|go|load|simulate|wait|try|check)\b", re.I)
passive = [m for m in needs if not act.search(m)]
for m in passive: print("   no action: " + m[:110])
check("every 'not available' message names an action (%d messages)" % len(needs), len(passive) <= 3, "%d without" % len(passive))

print("\n== D7 long sentences in help panels (over 45 words)")
longs = [(k, s) for k, v in INFO.items() for t in v["texts"] for s in re.split(r"(?<=[.!?])\s+", t) if len(s.split()) > 45]
for k, s in longs: print("   %-18s %d words: %s..." % (k, len(s.split()), s[:90]))
check("no help sentence over 45 words", not longs, "%d found" % len(longs))

print("\n== D8 each 'how' says how to read the result")
cue = re.compile(r"\b(means?|suggests?|indicates?|if |when |read|shows?|higher|lower|small|large|supported|compare|choose|look)\b", re.I)
SETUP = {"data_source", "subset", "integrity", "rcode", "saved", "model_store", "methods_text", "duplicates", "definitions", "quickstart"}   # settings and tools, not results
no_read = [k for k, v in INFO.items() if k not in SETUP and len(v["texts"]) >= 2 and not cue.search(v["texts"][1])]
for k in no_read: print("   %-18s how: %s" % (k, INFO[k]["texts"][1][:100]))
check("every 'how' gives a reading cue", not no_read, ", ".join(no_read))
print("\n%d failure(s)%s" % (len(fails), (": " + "; ".join(fails)) if fails else ""))
