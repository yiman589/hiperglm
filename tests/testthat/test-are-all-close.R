test_that("are_all_close correctly returns TRUE", {
  v = 1
  w = 1
  expect_true(are_all_close(v, w))
})

test_that("are_all_close returns correctly returns FALSE because the relative error is above rel_tol", {
  v = 1
  w = 1+rnorm(1)
  expect_false(are_all_close(v, w, abs_tol = 1, rel_tol = 1e-16))
})

test_that("are_all_close returns correctly returns FALSE because the absolute error is above abs_tol", {
  v = 1
  w = 1+rnorm(1)
  expect_false(are_all_close(v, w, abs_tol = 1e-16, rel_tol = 1))
})
