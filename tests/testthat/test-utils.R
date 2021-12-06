test_that("sample stochastic matrix", {

  cell <- 4
  bins <- 4
  pmat <- matrix(rexp(cell*bins), nrow = cell, ncol = bins)
  pmat <- pmat / rowSums(pmat)

  x <- rpois(n = cell, lambda = 1e4)

  expect <- as.vector(x %*% pmat)

  draw <- sample_stochastic_matrix(x = x, prob = pmat)
  expect_true(ks.test(x = expect, y = draw)$p.value > 0.95)


  cell <- 4
  bins <- 5
  pmat <- matrix(rexp(cell*bins), nrow = cell, ncol = bins)
  pmat <- pmat / rowSums(pmat)

  x <- rpois(n = cell, lambda = 1e4)

  expect <- as.vector(x %*% pmat)

  draw <- sample_stochastic_matrix(x = x, prob = pmat)
  expect_true(ks.test(x = expect, y = draw)$p.value > 0.95)

})
