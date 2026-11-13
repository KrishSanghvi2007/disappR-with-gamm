"""A lexer and Pratt parser for R source, following the precedence table of R's gram.y.

Used to syntax-check R files without an R runtime and to build an AST for cross-reference checks.
It accepts the R language as used in packages (R >= 4.1: raw strings, \\(x) lambdas, |> pipes). Differences from
R's own parser are conservative: anything R rejects that matters here (unbalanced brackets, `else` on a new line
at top level, `=` inside if/while conditions or unnamed arguments, chained comparisons, a symbol directly after an
expression, ...) is reported as a syntax error.
"""
import re

CONSTS = {"TRUE", "FALSE", "NULL", "NA", "NA_integer_", "NA_real_", "NA_character_", "NA_complex_", "Inf", "NaN",
          "T", "F"}
RESERVED = {"if", "else", "for", "in", "while", "repeat", "function", "break", "next"}
OPS3 = ["<<-", "->>", ":::"]
OPS2 = ["|>", "::", "<-", "<=", ">=", "==", "!=", "&&", "||", "->", ":=", "**"]
OPS1 = set("+-*/^<>!&|~?:$@=\\")
NUM_RE = re.compile(r"0[xX][0-9a-fA-F]+[Li]?|(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?[Li]?")
ID_RE = re.compile(r"(?:[^\W\d_]|\.(?!\d))[\w.]*")


class RSyntaxError(Exception):
    pass


class Tok:
    __slots__ = ("t", "v", "line", "col")

    def __init__(self, t, v, line, col):
        self.t, self.v, self.line, self.col = t, v, line, col

    def __repr__(self):
        return f"{self.t}:{self.v!r}@{self.line}"


def lex(src, fname="<src>"):
    toks, i, n, line, col0 = [], 0, len(src), 1, 0

    def add(t, v, start):
        toks.append(Tok(t, v, line, start - col0 + 1))

    while i < n:
        c = src[i]
        if c == "\n":
            if not toks or toks[-1].t != "NL":
                add("NL", "\n", i)
            i += 1
            line += 1
            col0 = i
            continue
        if c in " \t\r\f\u00a0":
            i += 1
            continue
        if c == "#":
            while i < n and src[i] != "\n":
                i += 1
            continue
        # raw strings r"(...)" R'[...]' r"--{...}--"
        if c in "rR" and i + 1 < n and src[i + 1] in "\"'":
            q = src[i + 1]
            j = i + 2
            dashes = ""
            while j < n and src[j] == "-":
                dashes += "-"
                j += 1
            if j < n and src[j] in "([{":
                close = {"(": ")", "[": "]", "{": "}"}[src[j]]
                end = src.find(close + dashes + q, j + 1)
                if end < 0:
                    raise RSyntaxError(f"{fname}:{line}: unterminated raw string")
                body = src[j + 1:end]
                add("STR", body, i)
                line += body.count("\n")
                i = end + len(close + dashes + q)
                continue
        if c in "\"'":
            start, sline = i, line
            i += 1
            buf = []
            while i < n and src[i] != c:
                if src[i] == "\\":
                    buf.append(src[i:i + 2])
                    i += 2
                    continue
                if src[i] == "\n":
                    line += 1
                buf.append(src[i])
                i += 1
            if i >= n:
                raise RSyntaxError(f"{fname}:{sline}: unterminated string")
            i += 1
            toks.append(Tok("STR", "".join(buf), sline, start - col0 + 1))
            continue
        if c == "`":
            start = i
            j = src.find("`", i + 1)
            if j < 0:
                raise RSyntaxError(f"{fname}:{line}: unterminated backtick name")
            add("SYM", src[i + 1:j], start)
            i = j + 1
            continue
        if c.isdigit() or (c == "." and i + 1 < n and src[i + 1].isdigit()):
            m = NUM_RE.match(src, i)
            add("NUM", m.group(0), i)
            i = m.end()
            continue
        m = ID_RE.match(src, i)
        if m:
            w = m.group(0)
            if w in RESERVED:
                add("KW", w, i)
            elif w in CONSTS:
                add("CONST", w, i)
            else:
                add("SYM", w, i)
            i = m.end()
            continue
        if c == "%":
            j = src.find("%", i + 1)
            if j < 0 or "\n" in src[i:j]:
                raise RSyntaxError(f"{fname}:{line}: unterminated %operator%")
            add("OP", src[i:j + 1], i)
            i = j + 1
            continue
        if c == "[":
            if i + 1 < n and src[i + 1] == "[":
                add("LBB", "[[", i)
                i += 2
            else:
                add("[", "[", i)
                i += 1
            continue
        if c in "](){},;":
            add(c, c, i)
            i += 1
            continue
        three, two = src[i:i + 3], src[i:i + 2]
        if three in OPS3:
            add("OP", three, i)
            i += 3
            continue
        if two in OPS2:
            add("OP", "^" if two == "**" else two, i)
            i += 2
            continue
        if c in OPS1:
            add("OP", c, i)
            i += 1
            continue
        raise RSyntaxError(f"{fname}:{line}: unexpected character {c!r}")
    toks.append(Tok("EOF", "", line, 0))
    return toks


# binding powers: (left binding power, right-associative?)
BIN = {"?": (5, False), "=": (10, True), "<-": (20, True), "<<-": (20, True), ":=": (20, True),
       "->": (30, False), "->>": (30, False), "~": (40, False), "|": (50, False), "||": (50, False),
       "&": (60, False), "&&": (60, False), "==": (80, None), "!=": (80, None), "<": (80, None), ">": (80, None),
       "<=": (80, None), ">=": (80, None), "+": (90, False), "-": (90, False), "*": (100, False), "/": (100, False),
       "|>": (110, False), ":": (120, False), "^": (140, True), "$": (150, False), "@": (150, False)}
CMP = {"==", "!=", "<", ">", "<=", ">="}
POSTFIX_BP = 170
EMPTY = ("empty",)


class Parser:
    def __init__(self, toks, fname="<src>"):
        self.toks, self.i, self.fname = toks, 0, fname
        self.ctx = [False]          # True: newlines are insignificant (inside ( [ [[ )
        self.depth = 0              # brace depth, for the `else` rule
        self.lints = []

    # ---- token access ----
    def peek(self):
        if self.ctx[-1]:
            while self.toks[self.i].t == "NL":
                self.i += 1
        return self.toks[self.i]

    def advance(self):
        t = self.peek()
        self.i += 1
        return t

    def skip_nl(self):
        while self.toks[self.i].t == "NL":
            self.i += 1

    def err(self, tok, msg):
        what = {"NL": "end of line", "EOF": "end of input"}.get(tok.t, repr(tok.v))
        raise RSyntaxError(f"{self.fname}:{tok.line}:{tok.col}: {msg} (unexpected {what})")

    def expect(self, t, v=None):
        tok = self.advance()
        if tok.t != t or (v is not None and tok.v != v):
            self.err(tok, f"expected {v or t}")
        return tok

    def lint(self, tok, msg):
        self.lints.append((tok.line, msg))

    # ---- program structure ----
    def program(self):
        stmts = []
        while True:
            while self.toks[self.i].t in ("NL", ";"):
                self.i += 1
            if self.toks[self.i].t == "EOF":
                return ("block", stmts)
            stmts.append(self.statement())
            t = self.toks[self.i]
            if t.t not in ("NL", ";", "EOF"):
                self.err(t, "statement not terminated")

    def statement(self):
        t = self.toks[self.i]
        if t.t == "OP" and t.v == "+":
            self.lint(t, "statement starts with '+': a line break before '+' ends the expression above (broken chain?)")
        return self.expr(0, True)

    def block(self):
        self.ctx.append(False)
        self.depth += 1
        stmts = []
        while True:
            while self.toks[self.i].t in ("NL", ";"):
                self.i += 1
            t = self.toks[self.i]
            if t.t == "}":
                self.i += 1
                break
            if t.t == "EOF":
                self.err(t, "unclosed '{'")
            stmts.append(self.statement())
            t = self.toks[self.i]
            if t.t not in ("NL", ";", "}"):
                self.err(t, "statement not terminated")
        self.depth -= 1
        self.ctx.pop()
        return ("block", stmts)

    def body(self):
        self.skip_nl()
        return self.expr(5, True)

    # ---- expressions ----
    def expr(self, rbp, allow_eq):
        self.skip_nl()
        left = self.nud(self.advance(), allow_eq)
        while True:
            t = self.peek()
            if t.t in ("NL", "EOF"):
                break
            lbp, right_assoc = self.lbp(t, allow_eq)
            if lbp <= rbp:
                break
            self.i += 1
            left = self.led(t, left, lbp, right_assoc, allow_eq)
        return left

    def lbp(self, t, allow_eq):
        if t.t in ("(", "[", "LBB"):
            return POSTFIX_BP, False
        if t.t == "OP":
            if t.v.startswith("%"):
                return 110, False
            if t.v == "=" and not allow_eq:
                return 0, False
            if t.v in BIN:
                bp, ra = BIN[t.v]
                return bp, bool(ra)
        return 0, False

    def nud(self, t, allow_eq):
        if t.t in ("NUM", "STR", "CONST"):
            return ("const", t.v)
        if t.t == "SYM":
            nx = self.toks[self.i]
            if nx.t == "OP" and nx.v in ("::", ":::"):
                self.i += 1
                name = self.toks[self.i]
                if name.t not in ("SYM", "STR"):
                    self.err(name, "expected a name after ::")
                self.i += 1
                return ("ns", t.v, name.v, t.line)
            return ("sym", t.v, t.line)
        if t.t == "(":
            self.ctx.append(True)
            e = self.expr(0, True)
            self.expect(")")
            self.ctx.pop()
            return ("paren", e)
        if t.t == "{":
            return self.block()
        if t.t == "OP":
            if t.v in ("-", "+"):
                return ("unop", t.v, self.expr(130, allow_eq))
            if t.v == "!":
                return ("unop", "!", self.expr(70, allow_eq))
            if t.v == "~":
                return ("formula", self.expr(40, allow_eq))
            if t.v == "?":
                return ("help", self.expr(5, allow_eq))
            if t.v == "\\":
                return self.function(t)
            self.err(t, "operator without a left operand")
        if t.t == "KW":
            if t.v == "function":
                return self.function(t)
            if t.v == "if":
                self.expect("(")
                self.ctx.append(True)
                cond = self.expr(0, False)
                self.expect(")")
                self.ctx.pop()
                yes = self.body()
                no = None
                if self.ctx[-1]:
                    if self.peek().t == "KW" and self.peek().v == "else":
                        self.i += 1
                        no = self.body()
                else:
                    j = self.i
                    while self.toks[j].t == "NL":
                        j += 1
                    if self.toks[j].t == "KW" and self.toks[j].v == "else" and (j == self.i or self.depth > 0):
                        self.i = j + 1
                        no = self.body()
                return ("if", cond, yes, no, t.line)
            if t.v == "for":
                self.expect("(")
                self.ctx.append(True)
                var = self.advance()
                if var.t != "SYM":
                    self.err(var, "expected a loop variable")
                self.expect("KW", "in")
                seq = self.expr(0, False)
                self.expect(")")
                self.ctx.pop()
                return ("for", var.v, seq, self.body())
            if t.v == "while":
                self.expect("(")
                self.ctx.append(True)
                cond = self.expr(0, False)
                self.expect(")")
                self.ctx.pop()
                return ("while", cond, self.body())
            if t.v == "repeat":
                return ("repeat", self.body())
            if t.v in ("break", "next"):
                return ("const", t.v)
        self.err(t, "expression expected")

    def function(self, t):
        self.expect("(")
        self.ctx.append(True)
        formals = []
        if self.peek().t != ")":
            while True:
                p = self.advance()
                if p.t != "SYM":
                    self.err(p, "expected an argument name")
                default = None
                if self.peek().t == "OP" and self.peek().v == "=":
                    self.i += 1
                    default = self.expr(0, False)
                formals.append((p.v, default))
                if self.peek().t == ",":
                    self.i += 1
                    continue
                break
        self.expect(")")
        self.ctx.pop()
        return ("function", formals, self.body(), t.line)

    def led(self, t, left, bp, right_assoc, allow_eq):
        if t.t == "(":
            return ("call", left, self.sublist(")"), t.line)
        if t.t == "[":
            return ("index", left, self.sublist("]"), t.line)
        if t.t == "LBB":
            args = self.sublist("]")
            self.expect("]")
            return ("index2", left, args, t.line)
        op = t.v
        if op in ("$", "@"):
            nm = self.advance()
            if nm.t not in ("SYM", "STR", "CONST", "KW") and not (nm.t == "("):
                self.err(nm, f"expected a name after {op}")
            if nm.t == "(":
                self.ctx.append(True)
                e = self.expr(0, True)
                self.expect(")")
                self.ctx.pop()
                return ("dollar", left, "(expr)", t.line)
            return ("dollar", left, nm.v, t.line)
        rhs = self.expr(bp - 1 if right_assoc else bp, allow_eq if op == "=" else (allow_eq and op in ("<-", "<<-", ":=")))
        if op in CMP:
            nx = self.peek()
            if nx.t == "OP" and nx.v in CMP:
                self.err(nx, "comparisons cannot be chained")
        if op in ("<-", "<<-", "=", ":="):
            return ("assign", op, left, rhs, t.line)
        if op in ("->", "->>"):
            return ("assign", "<-" if op == "->" else "<<-", rhs, left, t.line)
        if op.startswith("%") or op == "|>":
            return ("binop", op, left, rhs, t.line)
        if op == "~":
            return ("formula2", left, rhs)
        return ("binop", op, left, rhs, t.line)

    def sublist(self, close):
        self.ctx.append(True)
        items = []
        if self.peek().t == close:
            self.i += 1
            self.ctx.pop()
            return items
        while True:
            t = self.peek()
            if t.t == ",":
                items.append((None, EMPTY, t.line))
                self.i += 1
                if self.peek().t == close:
                    items.append((None, EMPTY, t.line))
                    break
                continue
            name = None
            nx = self.toks[self.i + 1] if self.i + 1 < len(self.toks) else None
            j = self.i + 1
            while self.toks[j].t == "NL":
                j += 1
            nx = self.toks[j]
            if t.t in ("SYM", "STR") or (t.t == "CONST" and t.v == "NULL"):
                if nx.t == "OP" and nx.v == "=":
                    name = t.v
                    self.i = j + 1
            if name is not None and self.peek().t in (",", close):
                items.append((name, EMPTY, t.line))
            else:
                items.append((name, self.expr(0, False), t.line))
            t = self.peek()
            if t.t == ",":
                self.i += 1
                if self.peek().t == close:
                    items.append((None, EMPTY, t.line))
                    break
                continue
            if t.t == close:
                break
            self.err(t, f"expected ',' or '{close}'")
        self.expect(close)
        self.ctx.pop()
        return items


def parse(src, fname="<src>"):
    p = Parser(lex(src, fname), fname)
    tree = p.program()
    return tree, p.lints
