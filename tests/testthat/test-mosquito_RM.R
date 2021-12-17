test_that("RM model setup is working", {
  tmax <- 20

  a <- 0.3
  psi <- matrix(
    c(0.9, 0.025, 0.075,
      0.15, 0.6, 0.25,
      0.01, 0.04, 0.95), nrow = 3, ncol = 3, byrow = TRUE
  )
  M <- c(100, 150, 120)
  Y <- c(10, 15, 8)
  Z <- c(8, 10, 4)

  mod <- make_microWNV(tmax = tmax, p = 3)

  # basic errors with scalar values for p, eip
  expect_error(setup_mosquito_RM(mod, stochastic = FALSE, a = a, eip = 5, p = -2, psi = psi, M = M, Y = Y, Z = Z))
  expect_error(setup_mosquito_RM(mod, stochastic = FALSE, a = a, eip = 5, p = 2, psi = psi, M = M, Y = Y, Z = Z))
  expect_error(setup_mosquito_RM(mod, stochastic = FALSE, a = a, eip = 5, p = numeric(0), psi = psi, M = M, Y = Y, Z = Z))
  expect_error(setup_mosquito_RM(mod, stochastic = FALSE, a = a, eip = -5, p = 0.9, psi = psi, M = M, Y = Y, Z = Z))
  expect_error(setup_mosquito_RM(mod, stochastic = FALSE, a = a, eip = numeric(0), p = 0.9, psi = psi, M = M, Y = Y, Z = Z))

  # eip tests
  setup_mosquito_RM(mod, stochastic = FALSE, a = a, eip = 5, p = 0.9, psi = psi, M = M, Y = Y, Z = Z)
  expect_equal(mod$mosquito$eip, rep(5, tmax))
  expect_equal(mod$mosquito$p, rep(0.9, tmax))

  mod <- make_microWNV(tmax = tmax, p = 3)
  eip <- 1:365
  setup_mosquito_RM(mod, stochastic = FALSE, a = a, eip = eip, p = 0.9, psi = psi, M = M, Y = Y, Z = Z)
  expect_equal(mod$mosquito$eip, 1:tmax)
  expect_equal(mod$mosquito$p, rep(0.9, tmax))

  mod <- make_microWNV(tmax = tmax, p = 3)
  eip <- 1:tmax
  setup_mosquito_RM(mod, stochastic = FALSE, a = a, eip = eip, p = 0.9, psi = psi, M = M, Y = Y, Z = Z)
  expect_equal(mod$mosquito$eip, 1:tmax)
  expect_equal(mod$mosquito$p, rep(0.9, tmax))

  # p tests
  mod <- make_microWNV(tmax = tmax, p = 3)
  p <- seq(from = 0.01, to = 0.99, length.out = 365)
  setup_mosquito_RM(mod, stochastic = FALSE, a = a, eip = 5, p = p, psi = psi, M = M, Y = Y, Z = Z)
  expect_equal(mod$mosquito$p, p[1:tmax])

  mod <- make_microWNV(tmax = tmax, p = 3)
  p <- seq(from = 0.01, to = 0.99, length.out = tmax)
  setup_mosquito_RM(mod, stochastic = FALSE, a = a, eip = 5, p = p, psi = psi, M = M, Y = Y, Z = Z)
  expect_equal(mod$mosquito$p, p)

  # other objs
  expect_equal(length(mod$mosquito$kappa), 3)
  expect_equal(length(mod$mosquito$M), 3)
  expect_equal(length(mod$mosquito$Y), 3)
  expect_equal(length(mod$mosquito$Z), 3)
  expect_equal(dim(mod$mosquito$ZZ), c(5, 3))
})
