test_that("observations() returns a tibble", {
  skip_if_offline()
  x <- example_dataset()
  expect_s3_class(observations(x), "tbl")
})

test_that("observations() returns the observations", {
  skip_if_offline()
  x <- example_dataset()
  expect_identical(observations(x), x$data$observations)
})

test_that("observations<-() assigns a data frame (as tibble) as observations", {
  skip_if_offline()
  x <- example_dataset()
  df <- data.frame(a = 1:3)
  observations(x) <- df
  expect_identical(observations(x), dplyr::as_tibble(df))
  expect_s3_class(observations(x), "tbl")
})

test_that("observations<-() returns error when value is not a data frame", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(
    observations(x) <- "not_a_data_frame",
    class = "camtrapdp_error_assignment_wrong_class"
  )
})
