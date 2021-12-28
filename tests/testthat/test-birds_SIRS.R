test_that("SIRS birds model setup is working", {
  tmax <- 20
  p <- 3

  gamma <- 1/7
  r <- 1/60

  fledge_disperse <- matrix(
    c(
      0.9, 0.05, 0.05,
      0.05, 0.9, 0.05,
      0.05, 0.05, 0.9
    ), nrow = 3, ncol = 3,
    byrow = TRUE
  )

  theta <- fledge_disperse

  SIR <- matrix(0, p, 3)
  SIR[, 1] <- 10

  mod <- make_microWNV(tmax = tmax, p = p)

  # basic errors with scalar values for mu
  expect_error(setup_birds_SIRS(mod, stochastic = FALSE, fledge_disperse = fledge_disperse, theta = theta, SIR = SIR, mu = -1, gamma = gamma, r = r))
  expect_error(setup_birds_SIRS(mod, stochastic = FALSE, fledge_disperse = fledge_disperse, theta = theta, SIR = SIR, mu = numeric(0), gamma = gamma, r = r))
  expect_error(setup_birds_SIRS(mod, stochastic = FALSE, fledge_disperse = fledge_disperse, theta = theta, SIR = SIR, mu = Inf, gamma = gamma, r = r))
  expect_error(setup_birds_SIRS(mod, stochastic = FALSE, fledge_disperse = fledge_disperse, theta = theta, SIR = SIR, mu = NULL, gamma = gamma, r = r))
  expect_error(setup_birds_SIRS(mod, stochastic = FALSE, fledge_disperse = fledge_disperse, theta = theta, SIR = SIR, mu = matrix(0, 10, 10), gamma = gamma, r = r))
  expect_error(setup_birds_SIRS(mod, stochastic = FALSE, fledge_disperse = fledge_disperse, theta = theta, SIR = SIR, mu = rep(0.9, 100), gamma = gamma, r = r))
  expect_error(setup_birds_SIRS(mod, stochastic = FALSE, fledge_disperse = fledge_disperse, theta = theta, SIR = matrix(0), mu = 1/60, gamma = gamma, r = r))

  # mu tests
  setup_birds_SIRS(mod, stochastic = FALSE, fledge_disperse = fledge_disperse, theta = theta, SIR = SIR, mu = 1/365, gamma = gamma, r = r)
  expect_equal(mod$bird$mu, rep(1/365, tmax))
  expect_equal(unname(mod$bird$SIR), SIR)

  mod <- make_microWNV(tmax = tmax, p = 3)
  mu <- 1:365
  setup_birds_SIRS(mod, stochastic = FALSE, fledge_disperse = fledge_disperse, theta = theta, SIR = SIR, mu = mu, gamma = gamma, r = r)
  expect_equal(mod$bird$mu, 1:tmax)

  mod <- make_microWNV(tmax = tmax, p = 3)
  mu <- 1:tmax
  setup_birds_SIRS(mod, stochastic = FALSE, fledge_disperse = fledge_disperse, theta = theta, SIR = SIR, mu = mu, gamma = gamma, r = r)
  expect_equal(mod$bird$mu, 1:tmax)

  # other objs
  expect_equal(length(mod$bird$h), p)
  expect_equal(
    names(mod$bird),
    c("fledge_disperse", "theta", "wf", "SIR", "h", "mu", "b", "c", "gamma", "r")
  )
  expect_equal(compute_WB(mod), as.vector(theta %*% rowSums(SIR)))
  expect_equal(compute_wfB(mod), rep(1, p))
  expect_equal(compute_xB(mod), rep(0, p))
  expect_equal(compute_B_pop(mod), rowSums(SIR))
  expect_equal(compute_PsiB(mod), theta)

})


test_that("SIRS birds interface is working", {
  tmax <- 20
  p <- 3

  gamma <- 1/7
  r <- 1/60

  fledge_disperse <- matrix(
    c(
      0.9, 0.05, 0.05,
      0.05, 0.9, 0.05,
      0.05, 0.05, 0.9
    ), nrow = 3, ncol = 3,
    byrow = TRUE
  )

  theta <- matrix(
    c(
      0.9, 0.025, 0.075,
      0.01, 0.9, 0.09,
      0.04, 0.06, 0.9
    ), nrow = 3, ncol = 3,
    byrow = TRUE
  )

  SIR <- matrix(0, p, 3)
  SIR[, 1] <- c(80, 90, 95)
  SIR[, 2] <- c(10, 5, 1)
  SIR[, 3] <- c(10, 5, 4)

  mod <- make_microWNV(tmax = tmax, p = p)
  setup_birds_SIRS(mod, stochastic = FALSE, fledge_disperse = fledge_disperse, theta = theta, SIR = SIR, mu = 1/365, gamma = gamma, r = r)

  WB_manual <- (rep(1, 3) * rowSums(SIR)) %*% theta
  xB_manual <- SIR[, 2] / rowSums(SIR) * mod$bird$c

  expect_equal(compute_xB(mod), xB_manual)
  expect_equal(sum(sum(WB_manual)), sum(SIR))
  expect_equal(compute_WB(mod), as.vector(WB_manual))
  expect_equal(compute_B_pop(mod), rowSums(SIR))

})


test_that("test deterministic SIRS birds step equal hand-calculation", {
  p <- 5
  tmax <- 10
  mod <- make_microWNV(tmax = tmax, p = p)

  fledge_disperse <- matrix(rexp(p^2), nrow = p, ncol = p)
  fledge_disperse <- fledge_disperse / rowSums(fledge_disperse)

  theta <- fledge_disperse + rexp(p^2)
  theta <- theta / rowSums(theta)

  SIR <- matrix(data = rpois(n = p * 3, lambda = 1e4), nrow = p, ncol = 3, dimnames = list(NULL, c("S", "I", "R")))
  mu <- rnorm(n = tmax, mean = 1/70, sd = 0.001)

  gamma <- 1/7
  r <- 1/20

  fledge_trace <- matrix(1, nrow = p, ncol = tmax)

  setup_birds_SIRS(
    model = mod, stochastic = FALSE,
    fledge_disperse = fledge_disperse, theta = theta,
    SIR = SIR, mu = mu, gamma = gamma, r = r
  )
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
  S_haz <- h + mu[1]
  S_leave <- SIR[, "S"] * pexp(q =  S_haz)
  S_toI <- S_leave * (h / S_haz)

  # compute differences: I
  I_haz <- gamma + mu[1]
  I_leave <- SIR[, "I"] * pexp(q =  I_haz)
  I_toR <- I_leave * (gamma / I_haz)

  # compute differences: R
  R_haz <- r + mu[1]
  R_leave <- SIR[, "R"] * pexp(q =  R_haz)
  R_toS <- R_leave * (r / R_haz)

  newSIR <- SIR
  newSIR[, "S"] <- newSIR[, "S"] + fledge - S_leave + R_toS
  newSIR[, "I"] <- newSIR[, "I"] - I_leave + S_toI
  newSIR[, "R"] <- newSIR[, "R"] - R_leave + I_toR

  expect_equal(mod$bird$SIR, newSIR)

})



test_that("test stochastic SIRS birds step equal hand-calculation", {
  p <- 5
  tmax <- 10

  fledge_disperse <- matrix(rexp(p^2), nrow = p, ncol = p)
  fledge_disperse <- fledge_disperse / rowSums(fledge_disperse)

  theta <- fledge_disperse + rexp(p^2)
  theta <- theta / rowSums(theta)

  SIR <- matrix(data = rpois(n = p * 3, lambda = 1e4), nrow = p, ncol = 3, dimnames = list(NULL, c("S", "I", "R")))
  mu <- rnorm(n = tmax, mean = 1/70, sd = 0.001)

  gamma <- 1/7
  r <- 1/20

  fledge_trace <- matrix(1, nrow = p, ncol = tmax)

  h <- sample(x = c(0.01, 0.025), size = p, replace = TRUE)

  # first calculate expectation with deterministic model
  mod <- make_microWNV(tmax = tmax, p = p)

  setup_birds_SIRS(
    model = mod, stochastic = FALSE,
    fledge_disperse = fledge_disperse, theta = theta,
    SIR = SIR, mu = mu, gamma = gamma, r = r
  )
  setup_fledge_trace(model = mod, trace = fledge_trace, stochastic = FALSE)

  mod$bird$h <- h

  step_birds(model = mod)
  SIR_det <- mod$bird$SIR


  # sample update in stochastic model
  mod <- make_microWNV(tmax = tmax, p = p)

  setup_birds_SIRS(
    model = mod, stochastic = TRUE,
    fledge_disperse = fledge_disperse, theta = theta,
    SIR = SIR, mu = mu, gamma = gamma, r = r
  )
  setup_fledge_trace(model = mod, trace = fledge_trace, stochastic = TRUE)

  mod$bird$h <- h

  step_birds(model = mod)
  SIR_stoch <- mod$bird$SIR

  # difference in expectation and sampled values not greater than some proportion
  # of expectation
  expect_true(all(abs(SIR_det - SIR_stoch) / SIR_det < 0.05))

})


test_that("test SIRS birds die out with no fledglings", {
  tmax <- 200
  p <- 5

  fledge_disperse <- diag(p)

  theta <- diag(p)

  SIR <- matrix(data = rpois(n = p * 3, lambda = 20), nrow = p, ncol = 3, dimnames = list(NULL, c("S", "I", "R")))

  mu <- 1/10

  gamma <- 1/7
  r <- 1/20

  fledge_trace <- matrix(0, nrow = p, ncol = tmax)

  # sample update in stochastic model
  mod <- make_microWNV(tmax = tmax, p = p)

  setup_birds_SIRS(
    model = mod, stochastic = TRUE,
    fledge_disperse = fledge_disperse, theta = theta,
    SIR = SIR, mu = mu, gamma = gamma, r = r
  )
  setup_fledge_trace(model = mod, trace = fledge_trace, stochastic = TRUE)

  for (i in 1:tmax) {
    step_birds(model = mod)
    mod$global$tnow <- mod$global$tnow + 1L
  }

  expect_true(all(mod$bird$SIR == 0))

})


test_that("deterministic SIRS bird step is working with pulse of infection", {
  tmax <- 20
  p <- 3

  gamma <- 1/7
  r <- 1/60

  fledge_disperse <- matrix(
    c(
      0.9, 0.05, 0.05,
      0.05, 0.9, 0.05,
      0.05, 0.05, 0.9
    ), nrow = 3, ncol = 3,
    byrow = TRUE
  )

  theta <- fledge_disperse

  SIR <- matrix(0, p, 3)
  SIR[, 1] <- 10

  mod <- make_microWNV(tmax = tmax, p = p)
  setup_birds_SIRS(mod, stochastic = FALSE, fledge_disperse = fledge_disperse, theta = theta, SIR = SIR, mu = 1/365, gamma = gamma, r = r)
  setup_fledge_trace(mod, stochastic = FALSE, trace = c(0.1, 0.1, 0.1))

  expect_equal(unname(mod$bird$SIR), SIR)
  expect_true(all(mod$bird$SIR[, 2] == 0))

  # time = 1
  mod$bird$h <- rep(qexp(p = 0.25), 3)
  step_birds(model = mod)

  expect_S <- SIR[, 1] * (1 - pexp(q = qexp(p = 0.25) + 1/365))
  expect_S <- expect_S + rep(0.1, 3)

  expect_true(all(mod$bird$SIR[, 1] < SIR[, 1]))
  expect_equal(mod$bird$SIR[, 1], expect_S)
  expect_true(all(mod$bird$SIR[, 2] > 0))
  expect_true(all(mod$bird$SIR[, 3] == 0))

  t1_I <- mod$bird$SIR[, 2]

  # time = 2
  mod$bird$h <- rep(0, 3)
  mod$global$tnow <- 2
  step_birds(model = mod)

  expect_true(all(mod$bird$SIR[, 1] < SIR[, 1]))
  expect_true(all(mod$bird$SIR[, 2] > 0))
  expect_true(all(mod$bird$SIR[, 3] > 0))
  expect_true(all(t1_I > mod$bird$SIR[, 2]))

})


test_that("stochastic SIRS bird step is working with pulse of infection", {
  tmax <- 20
  p <- 3

  gamma <- 1/7
  r <- 1/60

  fledge_disperse <- matrix(
    c(
      0.9, 0.05, 0.05,
      0.05, 0.9, 0.05,
      0.05, 0.05, 0.9
    ), nrow = 3, ncol = 3,
    byrow = TRUE
  )

  theta <- fledge_disperse

  SIR <- matrix(0, p, 3)
  SIR[, 1] <- 1e3

  mod <- make_microWNV(tmax = tmax, p = p)
  setup_birds_SIRS(mod, stochastic = TRUE, fledge_disperse = fledge_disperse, theta = theta, SIR = SIR, mu = 1/365, gamma = gamma, r = r)
  setup_fledge_trace(mod, stochastic = FALSE, trace = rep(1, 3))

  expect_equal(unname(mod$bird$SIR), SIR)
  expect_true(all(mod$bird$SIR[, 2] == 0))

  # time = 1
  mod$bird$h <- rep(qexp(p = 0.25), 3)
  step_birds(model = mod)

  expect_true(all(mod$bird$SIR[, 1] < SIR[, 1]))
  expect_true(all(mod$bird$SIR[, 2] > 0))
  expect_true(all(mod$bird$SIR[, 3] == 0))

  t1_I <- mod$bird$SIR[, 2]

  # time = 2
  mod$bird$h <- rep(0, 3)
  mod$global$tnow <- 2
  step_birds(model = mod)

  expect_true(all(mod$bird$SIR[, 1] < SIR[, 1]))
  expect_true(all(mod$bird$SIR[, 2] > 0))
  expect_true(all(mod$bird$SIR[, 3] > 0))
  expect_true(all(t1_I > mod$bird$SIR[, 2]))

})

