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

test_that("deployments()<- assigns a data frame (as tibble) as deployments", {
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

test_that("deployments()<- returns error when value is not a data frame", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(
    deployments(x) <- "not_a_data_frame",
    class = "camtrapdp_error_assignment_wrong_class"
  )
})

test_that("deployments()<- updates spatial and temporal scope in metadata", {
  skip_if_offline()
  x <- example_dataset()
  deployments(x) <- data.frame(
    deploymentID = "00a2c20d",
    latitude = 51.496,
    longitude = 4.774,
    deploymentStart = as.POSIXct("2020-05-30 02:57:37 UTC", tz = "UTC"),
    deploymentEnd = as.POSIXct("2020-07-01 09:41:41 UTC", tz = "UTC")
  )

  expected_spatial <- list(
    type = "Polygon",
    coordinates = array(
      c(
        4.774, 4.774, 4.774, 4.774, 4.774,
        51.496, 51.496, 51.496, 51.496, 51.496
      ),
      dim = c(1, 5, 2)
    )
  )
  expect_identical(x$spatial, expected_spatial)

  expect_identical(x$temporal$start, "2020-05-30")
  expect_identical(x$temporal$end, "2020-07-01")
})
