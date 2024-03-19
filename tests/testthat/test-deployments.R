test_that("deployments() returns the deployments", {
  skip_if_offline()
  x <- example_dataset()
  expect_identical(
    deployments(x),
    x$data$deployments
  )
})
