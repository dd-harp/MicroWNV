test_that("the model object constructs", {
  obj <- make_microWNV(tmax = 10)
  expect_true(inherits(obj, "microWNV"))
  expect_true(inherits(obj$global, "list"))
  expect_true(storage.mode(obj) == "environment")
  expect_equal(obj$global$tmax, 10)
  expect_equal(obj$global$tnow, 1)

  expect_error(make_microWNV(tmax = Inf))
  expect_error(make_microWNV(tmax = -5))
  expect_error(make_microWNV(tmax = 0))
})
