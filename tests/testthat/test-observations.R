test_that("observations() returns the observations", {
  x <- example_dataset()
  expect_identical(
    observations(x),
    x$data$observations
  )
})
