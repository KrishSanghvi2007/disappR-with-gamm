# The parser must accept valid R and reject what R rejects.
from rparse import parse, RSyntaxError
good = [
 "x <- 1; y = 2", "f <- function(a, b = 2, ...) { a + b }", "if (a) b else c", "{\n if (a) b\n else c\n}",
 "x[1, ]", "x[, 1]", "x[[1]][[2]]", "a[b[1]]", "a[[b[1]]]", "m[a[[1]], ]", "switch(x, a = , b = 2)",
 "f(a = 1, 'b' = 2, `c d` = 3)", "-2^2", "!x %in% y", "y ~ x + z", "~ x", "x |> f()", "\\(x) x + 1",
 "r\"(raw \\ string)\"", "pkg::f(1)", "x$a$b[[1]]", "x@slot", "for (i in 1:3) print(i)", "while (TRUE) break",
 "repeat { break }", "if (TRUE)\n  print(1)", "function(x)\n{\n x\n}", "a -> b", "x <- if (a) 1 else 2 + 3",
 "f(function(i) i, x)", "c(`a` = 1)", "x$`weird name`", "x[['a']] <- 1", "1e-3L", "0x1FL", ".5", "...", "..1",
 "x <- c(1,\n 2)", "x <-\n 5", "a %||% b", "(x = 5)", "f(x == 1)", "tryCatch(f(), error = function(e) NULL)",
 "if (a) {\n} else if (b) {\n} else {\n}", "x <- function() NULL", "lapply(x, `[[`, 1)", "a[-1]", "T & F",
]
bad = [
 "x <- (1", "f(a b)", "if (x = 1) y", "if (a) b\nelse c", "1 < 2 < 3", "x <- c(1,, 2", "function(1) x",
 "x <- 1 y <- 2", "f(a + b = 1)", "x[[1]", "{ x", "'unterminated", "x <- }", "a $ ", "for (1 in x) y",
]
fails = 0
for s in good:
    try: parse(s)
    except RSyntaxError as e: print("REJECTED VALID:", repr(s), e); fails += 1
for s in bad:
    try: parse(s); print("ACCEPTED INVALID:", repr(s)); fails += 1
    except RSyntaxError: pass
_, lints = parse("p <- ggplot(d)\n+ geom_point()")
assert lints, "missed the broken-chain lint"
print("parser self-test:", "PASS" if not fails else f"{fails} FAILURES", f"({len(good)} valid, {len(bad)} invalid cases)")
