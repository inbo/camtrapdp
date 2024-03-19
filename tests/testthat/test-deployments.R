test_that("deployments() returns the deployments", {
  x <- example_dataset()
  expect_identical(
    deployments(x),
    x$data$deployments
  )
})
