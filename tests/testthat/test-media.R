test_that("media() returns a tibble", {
  skip_if_offline()
  x <- example_dataset()
  expect_s3_class(media(x), "tbl")
})

test_that("media() returns the media", {
  skip_if_offline()
  x <- example_dataset()
  expect_identical(media(x), x$data$media)
})

test_that("media()<- assigns a data frame (as tibble) as media", {
  skip_if_offline()
  x <- example_dataset()
  df <- data.frame(a = 1:3)
  media(x) <- df
  expect_identical(media(x), dplyr::as_tibble(df))
  expect_s3_class(media(x), "tbl")
})

test_that("media()<- returns error when value is not a data frame", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(
    media(x) <- "not_a_data_frame",
    class = "camtrapdp_error_assignment_wrong_class"
  )
})
