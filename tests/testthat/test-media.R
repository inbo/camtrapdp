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
