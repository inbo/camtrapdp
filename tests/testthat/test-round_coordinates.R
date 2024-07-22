test_that("round_coordinates() returns a valid camtrapdp object", {
  skip_if_offline()
  x <- example_dataset()
  expect_no_error(
    check_camtrapdp(round_coordinates(x, 2))
  )
})

test_that("round_coordinates() returns error on empty or invalid digits", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(
    round_coordinates(x, digits = NULL),
    class = "camtrapdp_error_digits_invalid"
  )
  expect_error(
    round_coordinates(x, digits = 0),
    class = "camtrapdp_error_digits_invalid"
  )
  expect_error(
    round_coordinates(x, digits = 4),
    class = "camtrapdp_error_digits_invalid"
  )
  expect_error(
    round_coordinates(x, digits = 1.5),
    class = "camtrapdp_error_digits_invalid"
  )
})

test_that("round_coordinates() does not change the variable names of
           deployments", {
  skip_if_offline()
  x <- example_dataset()
  x_rounded <- round_coordinates(x, 2)
  expect_equal(names(deployments(x)), names(deployments(x_rounded)))
})

test_that("round_coordinates() sets lat, lon, uncertainty and precision", {
  skip_if_offline()
  x <- example_dataset()

  # Set coordinates and uncertainty of deployments along latitude gradients
  deployments(x)$longitude[[1]] <- 5.65555
  deployments(x)$latitude[[1]] <- 15.18155 # 0 latitude
  deployments(x)$coordinateUncertainty[[1]] <- 10
  deployments(x)$longitude[[2]] <- 5.65
  deployments(x)$latitude[[2]] <- 51.18 # 30 latitude
  deployments(x)$coordinateUncertainty[[2]] <- NA_integer_
  deployments(x)$longitude[[3]] <- 5.65
  deployments(x)$latitude[[3]] <- -61.18 # 60 latitude
  deployments(x)$coordinateUncertainty[[3]] <- NA_integer_
  deployments(x)$longitude[[4]] <- 5.65
  deployments(x)$latitude[[4]] <- -85.18 # 85 latitude
  deployments(x)$coordinateUncertainty[[4]] <- NA_integer_

  x1 <- round_coordinates(x, 1)
  expect_equal(x1$coordinatePrecision, 0.1)
  expect_equal(deployments(x1)$longitude[[1]], 5.7)
  expect_equal(deployments(x1)$latitude[[1]], 15.2)
  expect_equal(deployments(x1)$coordinateUncertainty[[1]], 10 - 157 + 15691)
  expect_equal(deployments(x1)$longitude[[2]], 5.7)
  expect_equal(deployments(x1)$latitude[[2]], 51.2)
  expect_equal(deployments(x1)$coordinateUncertainty[[2]], 30 + 14697)
  expect_equal(deployments(x1)$longitude[[3]], 5.7)
  expect_equal(deployments(x1)$latitude[[3]], -61.2)
  expect_equal(deployments(x1)$coordinateUncertainty[[3]], 30 + 12461)
  expect_equal(deployments(x1)$longitude[[4]], 5.7)
  expect_equal(deployments(x1)$latitude[[4]], -85.2)
  expect_equal(deployments(x1)$coordinateUncertainty[[4]], 30 + 11211)

  x2 <- round_coordinates(x, 2)
  expect_equal(x2$coordinatePrecision, 0.01)
  expect_equal(deployments(x2)$longitude[[1]], 5.66)
  expect_equal(deployments(x2)$latitude[[1]], 15.18)
  expect_equal(deployments(x2)$coordinateUncertainty[[1]], 10 - 157 + 1570)
  expect_equal(deployments(x2)$longitude[[2]], 5.65)
  expect_equal(deployments(x2)$latitude[[2]], 51.18)
  expect_equal(deployments(x2)$coordinateUncertainty[[2]], 30 + 1470)
  expect_equal(deployments(x2)$longitude[[3]], 5.65)
  expect_equal(deployments(x2)$latitude[[3]], -61.18)
  expect_equal(deployments(x2)$coordinateUncertainty[[3]], 30 + 1246)
  expect_equal(deployments(x2)$longitude[[4]], 5.65)
  expect_equal(deployments(x2)$latitude[[4]], -85.18)
  expect_equal(deployments(x2)$coordinateUncertainty[[4]], 30 + 1121)
})

test_that("round_coordinates() forbids rounding to equal or higher precision", {
  skip_if_offline()
  x1 <- example_dataset()
  deployments(x1)$latitude[[1]] <- 4.1
  deployments(x1)$latitude[[2]] <- 4.0
  deployments(x1)$latitude[[3]] <- 4.1
  deployments(x1)$latitude[[4]] <- 4.3
  x1$coordinatePrecision <- NULL
  x2 <- round_coordinates(example_dataset(), 2)

  # Try setting from 1 to 3, blocked by (max 1) decimals found in deployments
  expect_error(
    round_coordinates(x1, 3),
    class = "camtrapdp_error_precision_data"
  )

  # Try setting from 1 to 1, blocked by (max 1) decimals found in deployments
  expect_error(
    round_coordinates(x1, 1),
    class = "camtrapdp_error_precision_data"
  )

  # Try setting from 2 to 3, blocked by x$coordinatePrecision = 0.01
  expect_error(
    round_coordinates(x2, 3),
    class = "camtrapdp_error_precision_metadata"
  )

  # Try setting from 2 to 2, blocked by x$coordinatePrecision = 0.01
  expect_error(
    round_coordinates(x2, 2),
    class = "camtrapdp_error_precision_metadata"
  )
})

test_that("round_coordinates() doesn't overestimate uncertainty on multiple
           runs", {
  skip_if_offline()
  # Set uncertainty of first 2 deployments
  x <- example_dataset()
  deployments(x)$coordinateUncertainty[[1]] <- 10
  deployments(x)$coordinateUncertainty[[2]] <- NA_integer_

  x1 <- round_coordinates(x, 1)
  x1_via_2 <- round_coordinates(round_coordinates(x, 2), 1)
  expect_equal(
    x1_via_2$coordinatePrecision,
    x1$coordinatePrecision
  )
  expect_equal(
    deployments(x1_via_2)$coordinateUncertainty[[1]],
    deployments(x1)$coordinateUncertainty[[1]]
  )
  expect_equal(
    deployments(x1_via_2)$coordinateUncertainty[[2]],
    deployments(x1)$coordinateUncertainty[[2]]
  )
})

test_that("round_coordinates() updates spatial scope in metadata", {
  skip_if_offline()
  x <- example_dataset()
  x2 <- round_coordinates(x, 2)
  x1 <- round_coordinates(x, 1)

  # The number of digits in spatial scope is updated
  expect_equal(max(nchar(gsub("^\\d*.", "", x2$spatial$coordinates))), 2)
  expect_equal(max(nchar(gsub("^\\d*.", "", x1$spatial$coordinates))), 1)
})
