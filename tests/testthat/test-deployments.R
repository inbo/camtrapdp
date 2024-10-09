test_that("deployments<-() returns error when value is not a data frame", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(
    deployments(x) <- "not_a_data_frame",
    class = "camtrapdp_error_assignment_wrong_class"
  )
})

test_that("deployments<-() returns error when expected columns are missing", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(
    deployments(x) <- data.frame(
      deploymentStart = 1:3,
      deploymentEnd = 1:3,
      latitude = 1:3),
    class = "camtrapdp_error_deployments_columns_missing"
  )
})

test_that("deployments() returns a tibble", {
  skip_if_offline()
  x <- example_dataset()
  expect_s3_class(deployments(x), "tbl")
})

test_that("deployments() returns the deployments", {
  skip_if_offline()
  x <- example_dataset()
  expect_identical(deployments(x), x$data$deployments)
})

test_that("deployments<-() assigns a data frame (as tibble) as deployments", {
  skip_if_offline()
  x <- example_dataset()
  df <- data.frame(
    deploymentStart = 1:3,
    deploymentEnd = 1:3,
    latitude = 1:3,
    longitude = 1:3)
  deployments(x) <- df
  expect_identical(deployments(x), dplyr::as_tibble(df))
  expect_s3_class(deployments(x), "tbl")
})
