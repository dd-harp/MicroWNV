test_that("trace fledgling model compute_fledge works (vector)", {
  mod <- make_microWNV(tmax = 10)
  p <- 5
  mod$global$p <- p
  trace <- 10^(5:(p+4))
  setup_fledge_trace(model = mod, trace = trace, stochastic = TRUE)
  fledge <- compute_fledge(model = mod)

  expect_true(all(abs(log10(fledge) - log10(trace)) < 0.05))

  mod <- make_microWNV(tmax = 10)
  p <- 5
  mod$global$p <- p
  setup_fledge_trace(model = mod, trace = trace, stochastic = FALSE)
  fledge <- compute_fledge(model = mod)

  expect_equal(fledge, trace)
})


test_that("trace fledgling model compute_fledge works (matrix)", {
  tmax <- 5
  mod <- make_microWNV(tmax = tmax)
  p <- 5
  mod$global$p <- p
  trace <- replicate(n = tmax, expr = 10^(5:(p+4)))
  trace <- trace * matrix(rep(1:tmax, each = tmax), nrow = p, ncol = tmax)

  setup_fledge_trace(model = mod, trace = trace, stochastic = TRUE)
  mod$global$tnow <- 3
  fledge <- compute_fledge(model = mod)

  expect_true(all(abs(log10(fledge) - log10(trace[, 3])) < 0.01))

  mod <- make_microWNV(tmax = tmax)
  p <- 5
  mod$global$p <- p
  setup_fledge_trace(model = mod, trace = trace, stochastic = FALSE)
  mod$global$tnow <- 3
  fledge <- compute_fledge(model = mod)

  expect_equal(fledge, trace[, 3])
})


test_that("trace fledgling model add_eggs doesn't do anything", {
  # matrix
  tmax <- 5
  mod <- make_microWNV(tmax = tmax)
  p <- 5
  mod$global$p <- p
  trace <- replicate(n = tmax, expr = 1:p)

  setup_fledge_trace(model = mod, trace = trace, stochastic = TRUE)

  fledge_before <- mod$fledge
  add_eggs(model = mod, eggs = rep(1, p))
  expect_equal(fledge_before, mod$fledge)

  # vector
  trace <- 1:p

  setup_fledge_trace(model = mod, trace = trace, stochastic = TRUE)

  fledge_before <- mod$fledge
  add_eggs(model = mod, eggs = rep(1, p))
  expect_equal(fledge_before, mod$fledge)

})
