test_that("Allow delta to have arguments if min_delta and max_delta are specified", {
  expect_error(
    scqe(
      # Counts by period and treatment status
      untr_pre = 1e6,
      untr_post = 9e5,
      tr_post = 1e5,
      tr_pre = 0,
      # Counts with Y=1 by period and treatment status
      Y_tr_post = 43e3,
      Y_untr_post = 315e3,
      Y_tr_pre = 0,
      Y_untr_pre = 4e5,
      min_delta = -0.05,
      max_delta = 0.05,
      alpha = 0.05
    ),
    NA
  )
})


test_that("Warning if delta, min_delta, and max_delta are all specified", {
  expect_warning(
    scqe(
      # Counts by period and treatment status
      untr_pre = 1e6,
      untr_post = 9e5,
      tr_post = 1e5,
      tr_pre = 0,
      # Counts with Y=1 by period and treatment status
      Y_tr_post = 43e3,
      Y_untr_post = 315e3,
      Y_tr_pre = 0,
      Y_untr_pre = 4e5,
      min_delta = -0.05,
      max_delta = 0.05,
      alpha = 0.95,
      delta = 0.1
    ),
    "delta, min_delta, and max_delta all provided. Ignoring min_delta and max_delta."
  )
})

test_that("Return error if argument class is not numeric or integer", {
  expect_error(
    scqe(
      # Counts by period and treatment status
      untr_pre = "test",
      untr_post = 9e5,
      tr_post = 1e5,
      tr_pre = 0,
      # Counts with Y=1 by period and treatment status
      Y_tr_post = 43e3,
      Y_untr_post = 315e3,
      Y_tr_pre = 0,
      Y_untr_pre = 4e5,
      min_delta = -0.05,
      max_delta = 0.05,
      alpha = 0.05
    ),
    "One or more function arguments are of an invalid class. All arguments must be numeric."
  )
})
