test_that("distribute works", {
  p <- 2
  nn <- 5:10
  out <- vapply(X = nn, FUN = function(x) {distribute(n = x, p = p)}, FUN.VALUE = numeric(p))
  expect_equal(colSums(out), nn)

  p <- 3
  nn <- 5:10
  out <- vapply(X = nn, FUN = function(x) {distribute(n = x, p = p)}, FUN.VALUE = numeric(p))
  expect_equal(colSums(out), nn)
})


test_that("approx equal is working", {
  expect_true(all(approx_equal(1:5, 1:5)))
  expect_true(all(approx_equal(as.numeric(1:5), as.numeric(1:5))))
  expect_false(approx_equal(5, 5 + 1e-7))
})

test_that("sample stochastic vector", {

  cell <- 4
  bins <- 4
  pmat <- matrix(rexp(cell*bins), nrow = cell, ncol = bins)
  pmat <- pmat / rowSums(pmat)

  x <- rpois(n = cell, lambda = 1e5)

  expect <- as.vector(x %*% pmat)

  draw <- sample_stochastic_vector(x = x, prob = pmat)
  expect_true(all(abs(expect - draw) / expect < 0.025))
  expect_equal(sum(x), sum(draw))

  cell <- 4
  bins <- 5
  pmat <- matrix(rexp(cell*bins), nrow = cell, ncol = bins)
  pmat <- pmat / rowSums(pmat)

  x <- rpois(n = cell, lambda = 1e5)

  expect <- as.vector(x %*% pmat)

  draw <- sample_stochastic_vector(x = x, prob = pmat)
  expect_true(all(abs(expect - draw) / expect < 0.025))
  expect_equal(sum(x), sum(draw))

})

test_that("sample stochastic matrix", {
  ZZ <- matrix(data = rpois(n = 2*3, lambda = (1:6) * 1e7),nrow = 2,ncol = 3)
  psi <- matrix(c(
    0.9, 0.025, 0.075,
    0.1, 0.7, 0.2,
    0.01, 0.09, 0.9
  ),nrow=3,ncol=3,byrow=T)

  expectation <- ZZ %*% psi

  draw <- sample_stochastic_matrix(x = ZZ, prob = psi)

  expect_true(all(abs(expectation - draw) / expectation < 0.025))
  expect_equal(sum(draw), sum(ZZ))
  expect_equal(rowSums(draw), rowSums(ZZ))
})

