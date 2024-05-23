test_that("deployments() returns a tibble", {
  skip_if_offline()
  x <- example_dataset()
  expect_s3_class(deployments(x), "tbl")
})

test_that("deployments() returns the deployments", {
  skip_if_offline()
  x <- example_dataset()
  expect_identical(
    deployments(x),
    x$data$deployments
  )
})

test_that("deployments() <- allows assignment of data.frames to deployments", {
  skip_if_offline()
  y <- example_dataset()
  test_df <- data.frame(a = 1:3)
  # overwrite the deployments of example dataset with a data.frame
  deployments(y) <- test_df

  expect_identical(
    y$data$deployments,
    test_df
  )

  expect_identical(
    deployments(y),
    test_df
  )
})

test_that("deployments() <- returns the object after assignment", {
  skip_if_offline()
  y <- example_dataset()
  test_df <- data.frame(a = 1:3)

  expect_identical(
    deployments(y) <- test_df,
    test_df
  )
})

test_that("deployments() <- errors when you try to assign a non data.frame", {
  skip_if_offline()
  y <- example_dataset()

  expect_error(
    deployments(y) <- "not a data.frame object",
    class = "camtrapdp_error_assignment_wrong_class"
  )

  expect_error(
    deployments(y) <- matrix(1:6,2,3),
    regexp = "`value` is a an integer matrix but needs to be a `data.frame`",
    fixed = TRUE
  )
})
