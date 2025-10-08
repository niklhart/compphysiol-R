test_that(".replace_symbols correctly replaces symbols in simple expressions", {
    expr <- quote(a * b / c + a)
    result <- .replace_symbols(expr, c("a", "b"), c("A", "B"))
    expect_equal(result, quote(A * B / c + A))
})

test_that(".replace_symbols leaves untouched symbols not in 'old'", {
    expr <- quote(x + y)
    result <- .replace_symbols(expr, "a", "A")
    expect_equal(result, expr)
})

test_that(".replace_symbols works with nested calls", {
    expr <- quote(f(a + g(b, c)))
    result <- .replace_symbols(expr, c("a", "b"), c("A", "B"))
    expect_equal(result, quote(f(A + g(B, c))))
})

test_that(".replace_symbols errors if 'old' and 'new' lengths differ", {
    expr <- quote(a + b)
    expect_error(.replace_symbols(expr, c("a", "b"), "A"))
})



test_that(".suffix_symbols appends suffix to variable symbols", {
    expr <- quote(a * b + f(c))
    result <- .suffix_symbols(expr, "_new")
    expect_equal(result, quote(a_new * b_new + f(c_new)))
})

test_that(".suffix_symbols preserves function names", {
    expr <- quote(f(a, g(b)))
    result <- .suffix_symbols(expr, "_x")
    expect_equal(result, quote(f(a_x, g(b_x))))
})

test_that(".suffix_symbols skips specified symbols", {
    expr <- quote(a + b + c)
    result <- .suffix_symbols(expr, "_suf", skip = "b")
    expect_equal(result, quote(a_suf + b + c_suf))
})

test_that(".suffix_symbols leaves constants and operators unchanged", {
    expr <- quote(a + 1L + TRUE)
    result <- .suffix_symbols(expr, "_t")
    expect_equal(result, quote(a_t + 1L + TRUE))
})

test_that(".suffix_symbols works with nested calls and skips", {
    expr <- quote(f(a, g(b, h(c))))
    result <- .suffix_symbols(expr, "_z", skip = c("b", "h"))
    expect_equal(result, quote(f(a_z, g(b, h(c_z)))))
})
