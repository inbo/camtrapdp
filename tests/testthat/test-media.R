test_that("media() returns a tibble", {
  skip_if_offline()
  x <- example_dataset()
  expect_s3_class(media(x), "tbl")
})

test_that("media() returns the media", {
  skip_if_offline()
  x <- example_dataset()
  expect_identical(
    media(x),
    x$data$media
  )
})

test_that("media() <- allows assignment of data.frames to media", {
  skip_if_offline()
  y <- example_dataset()
  test_df <- data.frame(a = 1:3)
  # overwrite the media of example dataset with a data.frame
  media(y) <- test_df

  expect_identical(
    y$data$media,
    test_df
  )

  expect_identical(
    media(y),
    test_df
  )
})

test_that("media() <- returns the object after assignment", {
  skip_if_offline()
  y <- example_dataset()
  test_df <- data.frame(a = 1:3)

  expect_identical(
    media(y) <- test_df,
    test_df
  )
})

test_that("media() <- errors when you try to assign a non data.frame", {
  skip_if_offline()
  y <- example_dataset()

  expect_error(
    media(y) <- "not a data.frame object",
    class = "camtrapdp_error_assignment_wrong_class"
  )

  expect_error(
    media(y) <- matrix(1:6,2,3),
    regexp = "`value` is a an integer matrix but needs to be a `data.frame`",
    fixed = TRUE
  )
})
