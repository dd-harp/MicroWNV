test_that("trace fledgling model errors with incorrect trace", {
  p <- 2
  tmax <- 10
  mod <- MicroMoB::make_MicroMoB(tmax = tmax, p = p)

  trace <- c(10, 100, 1000)
  expect_error(setup_fledge_trace(model = mod, trace = trace, stochastic = FALSE))
  expect_error(setup_fledge_trace(model = mod, trace = trace, stochastic = TRUE))

  trace <- matrix()
  expect_error(setup_fledge_trace(model = mod, trace = trace, stochastic = FALSE))
  expect_error(setup_fledge_trace(model = mod, trace = trace, stochastic = TRUE))

  trace <- matrix(rexp(100), 10, 10)
  expect_error(setup_fledge_trace(model = mod, trace = trace, stochastic = FALSE))
  expect_error(setup_fledge_trace(model = mod, trace = trace, stochastic = TRUE))
})


test_that("test trace fledgling model with vector trace", {
  tmax <- 10
  p <- 2
  trace <- c(10, 100)

  # deterministic
  mod <- MicroMoB::make_MicroMoB(tmax = tmax, p = p)
  setup_fledge_trace(model = mod, trace = trace, stochastic = FALSE)
  expect_equal(compute_fledge(model = mod), trace)

  aq_before <- mod$fledge
  compute_clutch(model = mod)
  expect_equal(aq_before, mod$fledge)

  aq_before <- mod$fledge
  add_clutch(model = mod, eggs = c(10, 20))
  expect_equal(aq_before, mod$fledge)

  # stochastic
  mod <- MicroMoB::make_MicroMoB(tmax = tmax, p = p)
  setup_fledge_trace(model = mod, trace = trace, stochastic = TRUE)
  trace <- compute_fledge(model = mod)
  expect_true(trace[1] < trace[2])

  aq_before <- mod$fledge
  compute_clutch(model = mod)
  expect_equal(aq_before, mod$fledge)

  aq_before <- mod$fledge
  add_clutch(model = mod, eggs = c(10, 20))
  expect_equal(aq_before, mod$fledge)

})


test_that("test trace fledgling model with 365 matrix trace, tmax < 365", {
  tmax <- 50
  p <- 2
  trace <- matrix(rnorm(n = 365 * p, mean = rep(c(10, 100), each = 365), sd = rep(c(2.5, 20), each = 365)), nrow = p, ncol = 365, byrow = TRUE)
  trace <- pmax(trace, 0)

  # deterministic
  mod <- MicroMoB::make_MicroMoB(tmax = tmax, p = p)
  setup_fledge_trace(model = mod, trace = trace, stochastic = FALSE)

  expect_equal(mod$fledge$trace, trace[, 1:tmax])
  expect_equal(compute_fledge(model = mod), trace[, 1])

  aq_before <- mod$fledge
  compute_clutch(model = mod)
  expect_equal(aq_before, mod$fledge)

  aq_before <- mod$fledge
  add_clutch(model = mod, eggs = c(10, 20))
  expect_equal(aq_before, mod$fledge)

  # stochastic
  mod <- MicroMoB::make_MicroMoB(tmax = tmax, p = p)
  setup_fledge_trace(model = mod, trace = trace, stochastic = TRUE)

  expect_equal(mod$fledge$trace, trace[, 1:tmax])
  trace <- compute_fledge(model = mod)
  expect_true(trace[1] < trace[2])

  aq_before <- mod$fledge
  compute_clutch(model = mod)
  expect_equal(aq_before, mod$fledge)

  aq_before <- mod$fledge
  add_clutch(model = mod, eggs = c(10, 20))
  expect_equal(aq_before, mod$fledge)

})


test_that("test trace fledgling model with 365 matrix trace, tmax > 365", {
  tmax <- 730
  p <- 2
  trace <- matrix(rnorm(n = 365 * p, mean = rep(c(10, 100), each = 365), sd = rep(c(2.5, 20), each = 365)), nrow = p, ncol = 365, byrow = TRUE)
  trace <- pmax(trace, 0)

  # deterministic
  mod <- MicroMoB::make_MicroMoB(tmax = tmax, p = p)
  setup_fledge_trace(model = mod, trace = trace, stochastic = FALSE)

  expect_equal(mod$fledge$trace, cbind(trace, trace))
  expect_equal(compute_fledge(model = mod), trace[, 1])

  mod$global$tnow <- 366
  expect_equal(compute_fledge(model = mod), trace[, 1])

  mod$global$tnow <- tmax
  expect_equal(compute_fledge(model = mod), trace[, 365])

  # stochastic
  mod <- MicroMoB::make_MicroMoB(tmax = tmax, p = p)
  setup_fledge_trace(model = mod, trace = trace, stochastic = TRUE)

  expect_equal(mod$fledge$trace, cbind(trace, trace))
  trace <- compute_fledge(model = mod)
  expect_true(trace[1] < trace[2])

})


test_that("test trace fledgling model with tmax matrix trace", {
  tmax <- 20
  p <- 2
  trace <- matrix(rnorm(n = 20 * p, mean = rep(c(10, 100), each = 20), sd = rep(c(2.5, 20), each = 20)), nrow = p, ncol = 20, byrow = TRUE)
  trace <- pmax(trace, 0)

  # deterministic
  mod <- MicroMoB::make_MicroMoB(tmax = tmax, p = p)
  setup_fledge_trace(model = mod, trace = trace, stochastic = FALSE)

  expect_equal(mod$fledge$trace, trace)
  expect_equal(compute_fledge(model = mod), trace[, 1])

  mod$global$tnow <- 19
  expect_equal(compute_fledge(model = mod), trace[, 19])

  # stochastic
  mod <- MicroMoB::make_MicroMoB(tmax = tmax, p = p)
  setup_fledge_trace(model = mod, trace = trace, stochastic = TRUE)

  expect_equal(mod$fledge$trace, trace)
  trace <- compute_fledge(model = mod)
  expect_true(trace[1] < trace[2])

})


