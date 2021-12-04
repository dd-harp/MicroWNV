library(testthat)

test_that("the model object constructs", {
  obj <- make_microWNV()
  expect_true(inherits(obj, "microWNV"))
  expect_true(inherits(obj$global, "list"))
  expect_true(storage.mode(obj) == "environment")
})
