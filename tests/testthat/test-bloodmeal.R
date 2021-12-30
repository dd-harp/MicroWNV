test_that("test bloodmeal with simple RM setup (SIR humans and SIRS birds)", {

  patches <- 1
  n <- 1
  tmax <- 5

  # human parameters
  theta <- diag(n)
  SIR <- matrix(data = c(200, 80, 0), nrow = 1, ncol = 3)
  wf <- 1.1
  b <- 0.55
  c <- 0.15
  gamma <- 1/5

  # bird parameters
  theta_B <- diag(n)
  SIR_B <- matrix(data = c(100, 20, 0), nrow = 1, ncol = 3)
  wfB <- 0.9
  bB <- 0.65
  cB <- 0.25
  gammaB <- 1/7
  rB <- 1/150
  muB <- 1/365

  # mosquito parameters
  f <- 0.3
  q <- 0.8
  eip <- 10
  p <- 0.9
  M <- 500
  Y <- 100
  Z <- 80
  psi <- diag(patches)

  mod <- make_microWNV(tmax = tmax, p = patches)

  # humans
  setup_humans_SIR(mod, stochastic = FALSE, theta = theta, wf = wf, H = sum(SIR), SIR = SIR, b = b, c = c, gamma = gamma)

  # birds
  setup_birds_SIRS(mod, stochastic = FALSE, fledge_disperse = theta_B, theta = theta_B, SIR = SIR_B, mu = muB, wf = wfB, b = bB, c = cB, gamma = gammaB, r = rB)
  setup_fledge_trace(mod, stochastic = FALSE, trace = 0)

  # mosquitoes
  setup_aqua_trace(mod, stochastic = FALSE, lambda = 0)
  setup_mosquito_RM(mod, stochastic = FALSE, f = f, q = q, eip = eip, p = p, psi = psi, M = M, Y = Y, Z = Z)

  # compute human terms
  W <- compute_W(mod)
  H <- compute_H(mod)
  x <- compute_x(mod)

  expect_equal(sum(SIR) * wf, W)
  expect_equal(sum(SIR), H)
  expect_equal((SIR[, 2] / sum(SIR)) * c, x)
  expect_equal(compute_wf(mod), wf)
  expect_equal(compute_Psi(mod), theta)

  # compute bird terms
  WB <- compute_WB(mod)
  B_pop <- compute_B_pop(mod)
  xB <- compute_xB(mod)

  expect_equal(sum(SIR_B) * wfB, WB)
  expect_equal(sum(SIR_B), B_pop)
  expect_equal((SIR_B[, 2] / sum(SIR_B)) * cB, xB)
  expect_equal(compute_wfB(mod), wfB)
  expect_equal(compute_PsiB(mod), theta_B)

  # compute betas
  beta_H <- diag(wf, nrow = 1, ncol = 1) %*% theta %*% diag(1/W, nrow = 1, ncol = 1)
  beta_B <- diag(wfB, nrow = 1, ncol = 1) %*% theta_B %*% diag(1/WB, nrow = 1, ncol = 1)

  # compute terms
  EIR_H <- beta_H %*% (f*q*Z)
  EIR_B <- beta_B %*% (f*(1-q)*Z)
  kappa <- (q * (t(beta_H) %*% (x*H))) + ((1 - q) * (t(beta_B) %*% (xB*B_pop)))

  # compute in MicroWNV
  compute_bloodmeal(mod)

  expect_equal(mod$human$EIR, as.vector(EIR_H))
  expect_equal(mod$bird$EIR, as.vector(EIR_B))
  expect_equal(mod$mosquito$kappa, as.vector(kappa))
})
