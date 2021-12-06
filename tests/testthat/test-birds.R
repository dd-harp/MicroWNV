test_that("test deterministic SIRS birds", {
  mod <- make_microWNV(tmax = 10)
  p <- 5
  mod$global$p <- p

  fledge_disperse <- matrix(rexp(p^2), nrow = p, ncol = p)
  fledge_disperse <- fledge_disperse / rowSums(fledge_disperse)

  theta <- fledge_disperse + rexp(p^2)
  theta <- theta / rowSums(theta)

  SIR <- matrix(data = rpois(n = p * 3, lambda = 1e4), nrow = p, ncol = 3, dimnames = list(NULL, c("S", "I", "R")))
  mu <- rnorm(n = p, mean = 1/70, sd = 0.001)
  mu <- replicate(10, mu)

  gamma <- 1/7
  r <- 1/20

  fledge_trace <- matrix(1, nrow = p, ncol = 10)

  setup_birds_SIRS(
    model = mod, stochastic = FALSE,
    fledge_disperse = fledge_disperse, theta = theta,
    SIR = SIR, mu = mu, gamma = gamma, r = r
  )

  setup_clutch_null(model = mod)
  setup_fledge_trace(model = mod, trace = fledge_trace, stochastic = FALSE)

  h <- sample(x = c(0.01, 0.025), size = p, replace = TRUE)
  mod$bird$h <- h

  # compute update in model
  step_birds(model = mod)

  # compute update by hand
  fledge <- fledge_trace[, 1] %*% fledge_disperse
  expect_true(all.equal(sum(fledge), sum(fledge_trace[, 1])))

  eggs <- NULL

  # compute differences: S
  S_haz <- h + mu[, 1]
  S_leave <- SIR[, "S"] * pexp(q =  S_haz)
  S_toI <- S_leave * (h / S_haz)

  # compute differences: I
  I_haz <- gamma + mu[, 1]
  I_leave <- SIR[, "I"] * pexp(q =  I_haz)
  I_toR <- I_leave * (gamma / I_haz)

  # compute differences: R
  R_haz <- r + mu[, 1]
  R_leave <- SIR[, "R"] * pexp(q =  R_haz)
  R_toS <- R_leave * (r / R_haz)

  newSIR <- SIR
  newSIR[, "S"] <- newSIR[, "S"] + fledge - S_leave + R_toS
  newSIR[, "I"] <- newSIR[, "I"] - I_leave + S_toI
  newSIR[, "R"] <- newSIR[, "R"] - R_leave + I_toR

  expect_equal(mod$bird$SIR, newSIR)

})




