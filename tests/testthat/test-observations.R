test_that("observations() returns the observations", {
  skip_if_offline()
  x <- example_dataset()
  expect_identical(
    observations(x),
    x$data$observations
  )
})
