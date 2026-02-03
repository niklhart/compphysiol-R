test_that("Ryacas simplification removes redundant minus signs and cancels terms", {

  skip_if_not_installed("Ryacas")

  expr <- quote(-(kAB * A)/-(kAB))
  simp <- .simplify_ryacas(expr)

  expect_equal(simp, quote(A))
})
