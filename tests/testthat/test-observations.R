test_that("observations() returns a tibble", {
  skip_if_offline()
  x <- example_dataset()
  expect_s3_class(observations(x), "tbl")
})

test_that("observations() returns the observations", {
  skip_if_offline()
  x <- example_dataset()
  expect_identical(
    observations(x),
    x$data$observations
  )
})

test_that("observations() <- allows assignment of data.frames to observations", {
  skip_if_offline()
  y <- example_dataset()
  test_df <- data.frame(a = 1:3)
  # overwrite the observations of example dataset with a data.frame
  observations(y) <- test_df

  expect_identical(
    y$data$observations,
    test_df
  )

  expect_identical(
    observations(y),
    test_df
  )
})

test_that("observations() <- returns the object after assignment", {
  skip_if_offline()
  y <- example_dataset()
  test_df <- data.frame(a = 1:3)

  expect_identical(
    observations(y) <- test_df,
    test_df
  )
})

test_that("observations() <- errors when you try to assign a non data.frame", {
  skip_if_offline()
  y <- example_dataset()

  expect_error(
    observations(y) <- "not a data.frame object",
    class = "camtrapdp_error_assignment_wrong_class"
  )

  expect_error(
    observations(y) <- matrix(1:6,2,3),
    regexp = "`value` is a an integer matrix but needs to be a `data.frame`",
    fixed = TRUE
  )
})
