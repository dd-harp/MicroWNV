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


# test_that("RM step is working", {
#   tmax <- 20
#
#   a <- 0.3
#   psi <- diag(3)
#   M <- c(100, 150, 120)
#   Y <- c(0, 0, 0)
#   Z <- c(0, 0, 0)
#
#   mod <- make_microWNV(tmax = tmax, p = 3)
#   setup_mosquito_RM(mod, stochastic = FALSE, a = a, eip = 3, p = 0.9, psi = psi, M = M, Y = Y, Z = Z)
#   setup_aqua_trace(model = mod, lambda = c(1e1, 1e2, 1e3), stochastic = FALSE)
#
#   expect_equal(mod$mosquito$Y, Y)
#   expect_equal(mod$mosquito$Z, Z)
#
#   # time = 1
#   mod$mosquito$kappa <- rep(1, 3)
#   step_mosquitoes(model = mod)
#
#   expect_true(all(mod$mosquito$Y > 0))
#   expect_true(all(mod$mosquito$Z == 0))
#   expect_equal(mod$mosquito$ZZ[3, ], mod$mosquito$Y)
#
#   # time = 2
#   mod$mosquito$kappa <- rep(0, 3)
#   mod$global$tnow <- 2
#   step_mosquitoes(model = mod)
#
#   expect_true(all(mod$mosquito$Y > 0))
#   expect_true(all(mod$mosquito$Z == 0))
#   expect_equal(mod$mosquito$ZZ[2, ], mod$mosquito$Y)
#
#   # time = 3
#   mod$global$tnow <- 3
#   step_mosquitoes(model = mod)
#
#   expect_true(all(mod$mosquito$Y > 0))
#   expect_true(all(mod$mosquito$Z == 0))
#   expect_equal(mod$mosquito$ZZ[1, ], mod$mosquito$Y)
#
#   # time = 4 (expect Z mosquitoes)
#   mod$global$tnow <- 4
#   step_mosquitoes(model = mod)
#
#
#
#   ZZ_shift <- matrix(0, 3, 3)
#   ZZ_shift[1:(3-1), 2:3] <- diag(2)
#
#
# })
