test_that("Allow delta to have now arguments if min_delta and max_delta are specified", {
  expect_error(
    scqe.2csumm(
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
      min_delta = -0.1,
      max_delta = 0.1,
      alpha = 0.95
    ),
    NA
  )
})
