test_that("round_coordinates() returns error on invalid digits", {
  x <- example_dataset()
  expect_error(round_coordinates(x, digits = 0), class = "camtrapdp_error_digits")
  expect_error(round_coordinates(x, digits = 4), class = "camtrapdp_error_digits")
  expect_error(round_coordinates(x, digits = 1.5), class = "camtrapdp_error_digits")
})

test_that("round_coordinates() sets lat, long, uncertainty and precision", {
  # Set coordinates and uncertainty of deployments along latitude gradients
  x <- example_dataset()
  x$data$deployments$longitude[[1]] <- 5.65555
  x$data$deployments$latitude[[1]] <- 15.18155 # 0 latitude
  x$data$deployments$coordinateUncertainty[[1]] <- 10
  x$data$deployments$longitude[[2]] <- 5.65
  x$data$deployments$latitude[[2]] <- 51.18 # 30 latitude
  x$data$deployments$coordinateUncertainty[[2]] <- NA_integer_
  x$data$deployments$longitude[[3]] <- 5.65
  x$data$deployments$latitude[[3]] <- -61.18 # 60 latitude
  x$data$deployments$coordinateUncertainty[[3]] <- NA_integer_
  x$data$deployments$longitude[[4]] <- 5.65
  x$data$deployments$latitude[[4]] <- -85.18 # 85 latitude
  x$data$deployments$coordinateUncertainty[[4]] <- NA_integer_

  x1 <- round_coordinates(x, 1)
  expect_equal(x1$coordinatePrecision, 0.1)
  expect_equal(x1$data$deployments$longitude[[1]], 5.7)
  expect_equal(x1$data$deployments$latitude[[1]], 15.2)
  expect_equal(x1$data$deployments$coordinateUncertainty[[1]], 10 - 157 + 15691)
  expect_equal(x1$data$deployments$longitude[[2]], 5.7)
  expect_equal(x1$data$deployments$latitude[[2]], 51.2)
  expect_equal(x1$data$deployments$coordinateUncertainty[[2]], 30 + 15691)
  expect_equal(x1$data$deployments$longitude[[3]], 5.7)
  expect_equal(x1$data$deployments$latitude[[3]], -61.2)
  expect_equal(x1$data$deployments$coordinateUncertainty[[3]], 30 + 15691)
  expect_equal(x1$data$deployments$longitude[[4]], 5.7)
  expect_equal(x1$data$deployments$latitude[[4]], -85.2)
  expect_equal(x1$data$deployments$coordinateUncertainty[[4]], 30 + 15691)

  x2 <- round_coordinates(x, 2)
  expect_equal(x2$coordinatePrecision, 0.01)
  expect_equal(x2$data$deployments$longitude[[1]], 5.66)
  expect_equal(x2$data$deployments$latitude[[1]], 15.18)
  expect_equal(x2$data$deployments$coordinateUncertainty[[1]], 10 - 157 + 1570)
  expect_equal(x2$data$deployments$longitude[[2]], 5.65)
  expect_equal(x2$data$deployments$latitude[[2]], 51.18)
  expect_equal(x2$data$deployments$coordinateUncertainty[[2]], 30 + 1570)
  expect_equal(x2$data$deployments$longitude[[3]], 5.65)
  expect_equal(x2$data$deployments$latitude[[3]], -61.18)
  expect_equal(x2$data$deployments$coordinateUncertainty[[3]], 30 + 1570)
  expect_equal(x2$data$deployments$longitude[[4]], 5.65)
  expect_equal(x2$data$deployments$latitude[[4]], -85.18)
  expect_equal(x2$data$deployments$coordinateUncertainty[[4]], 30 + 1570)

  x3 <- round_coordinates(x, 3)
  expect_equal(x3$coordinatePrecision, 0.001)
  expect_equal(x3$data$deployments$longitude[[1]], 5.656)
  expect_equal(x3$data$deployments$latitude[[1]], 15.182)
  expect_equal(x3$data$deployments$coordinateUncertainty[[1]], 10 - 157 + 157)
  expect_equal(x3$data$deployments$longitude[[2]], 5.65) # Unchanged
  expect_equal(x3$data$deployments$latitude[[2]], 51.18) # Unchanged
  expect_equal(x3$data$deployments$coordinateUncertainty[[2]], 30 + 157)
  expect_equal(x3$data$deployments$longitude[[3]], 5.65) # Unchanged
  expect_equal(x3$data$deployments$latitude[[3]], -61.18) # Unchanged
  expect_equal(x3$data$deployments$coordinateUncertainty[[3]], 30 + 157)
  expect_equal(x3$data$deployments$longitude[[4]], 5.65) # Unchanged
  expect_equal(x3$data$deployments$latitude[[4]], -85.18) # Unchanged
  expect_equal(x3$data$deployments$coordinateUncertainty[[4]], 30 + 157)
})

test_that("round_coordinates() does not allow to round to higher precision", {
  x <- example_dataset()
  x2 <- round_coordinates(x, 2)

  # Based on package$coordinatePrecision
  expect_error(round_coordinates(x2, 3), class = "camtrapdp_error_precision")

  # Based on data
  x2$coordinatePrecision <- NULL
  expect_error(round_coordinates(x2, 3), class = "camtrapdp_error_precision_max")
})

test_that("round_coordinates() doesn't overestimate uncertainty on multiple runs", {
  # Set uncertainty of first 2 deployments
  x <- example_dataset()
  x$data$deployments$coordinateUncertainty[[1]] <- 10
  x$data$deployments$coordinateUncertainty[[2]] <- NA_integer_

  x1 <- round_coordinates(x, 1)
  x1_via_2 <- round_coordinates(round_coordinates(x, 2), 1)
  expect_equal(
    x1_via_2$coordinatePrecision,
    x1$coordinatePrecision
  )
  expect_equal(
    x1_via_2$data$deployments$coordinateUncertainty[[1]],
    x1$data$deployments$coordinateUncertainty[[1]]
  )
  expect_equal(
    x1_via_2$data$deployments$coordinateUncertainty[[2]],
    x1$data$deployments$coordinateUncertainty[[2]]
  )
})
