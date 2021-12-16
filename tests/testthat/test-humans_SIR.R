test_that("human object setup is working", {

  n <- 3
  p <- 3
  tmax <- 10

  mod <- make_microWNV(tmax = tmax, p = p)

  b <- 0.55
  c <- 0.15
  gamma <- 1/5

  theta <- matrix(rexp(9), n, p)
  theta <- theta / rowSums(theta)
  wf <- rep(1, n)
  H <- c(100, 80, 50)
  SIR <- matrix(
    c(85, 5, 10,
      70, 5, 5,
      25, 10, 15),
    nrow = n, ncol = 3, byrow = TRUE
  )

  setup_humans_SIR(model = mod, stochastic = FALSE, theta = theta, wf = wf, H = H, SIR = SIR, b = b, c = c, gamma = gamma)
  expect_equal(compute_W(mod), t(theta) %*% (wf * H))
  expect_equal(compute_x(mod), (SIR[, 2] / H) * c)
})


test_that("deterministic updates of human SIR model work", {

  tmax <- 1e3
  n <- 3
  p <- 3

  mod <- make_microWNV(tmax = tmax, p = p)

  b <- 0.55
  c <- 0.15
  gamma <- 1/5

  theta <- matrix(rexp(9), n, p)
  theta <- theta / rowSums(theta)
  wf <- rep(1, 3)
  H <- c(100, 80, 50)
  SIR <- matrix(
    c(85, 5, 10,
      70, 5, 5,
      25, 10, 15),
    nrow = n, ncol = 3, byrow = TRUE
  )

  setup_humans_SIR(model = mod, stochastic = FALSE, theta = theta, wf = wf, H = H, SIR = SIR, b = b, c = c, gamma = gamma)

  for (i in 1:tmax) {
    step_humans(model = mod)
    mod$global$tnow <- mod$global$tnow + 1L
  }

  expect_equal(mod$human$SIR[, 3], rowSums(SIR[, 2:3]))
  expect_equal(mod$human$SIR[, 1], SIR[, 1])
  expect_true(all(mod$human$SIR[, 2] >= 0))
})


test_that("stochastic updates of human SIR model work", {

  tmax <- 1e3
  n <- 3
  p <- 3

  mod <- make_microWNV(tmax = tmax, p = p)

  b <- 0.55
  c <- 0.15
  gamma <- 1/5

  theta <- matrix(rexp(9), n, p)
  theta <- theta / rowSums(theta)
  wf <- rep(1, 3)
  H <- c(100, 80, 50)
  SIR <- matrix(
    c(85, 5, 10,
      70, 5, 5,
      25, 10, 15),
    nrow = n, ncol = 3, byrow = TRUE
  )

  setup_humans_SIR(model = mod, stochastic = TRUE, theta = theta, wf = wf, H = H, SIR = SIR, b = b, c = c, gamma = gamma)

  for (i in 1:tmax) {
    step_humans(model = mod)
    mod$global$tnow <- mod$global$tnow + 1L
  }

  expect_equal(mod$human$SIR[, 3], rowSums(SIR[, 2:3]))
  expect_equal(mod$human$SIR[, 1], SIR[, 1])
  expect_true(all(mod$human$SIR[, 2] >= 0))
})

