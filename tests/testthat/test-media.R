test_that("media() returns the media", {
  x <- example_dataset()
  expect_identical(
    media(x),
    x$data$media
  )
})
