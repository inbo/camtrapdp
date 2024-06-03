test_that("round_coordinates() returns error on invalid digits", {
  error_msg <- "`digits` must be 1, 2 or 3."
  expect_error(round_coordinates(mica, digits = 0), error_msg)
  expect_error(round_coordinates(mica, digits = 4), error_msg)
  expect_error(round_coordinates(mica, digits = 1.5), error_msg)
})

test_that("round_coordinates() sets lat, long, uncertainty and precision", {
  # Set coordinates and uncertainty of deployments along latitude gradients
  mica$data$deployments$longitude[[1]] <- 5.65555
  mica$data$deployments$latitude[[1]] <- 15.18155 # 0 latitude
  mica$data$deployments$coordinateUncertainty[[1]] <- 10
  mica$data$deployments$longitude[[2]] <- 5.65
  mica$data$deployments$latitude[[2]] <- 51.18 # 30 latitude
  mica$data$deployments$coordinateUncertainty[[2]] <- NA_integer_
  mica$data$deployments$longitude[[3]] <- 5.65
  mica$data$deployments$latitude[[3]] <- -61.18 # 60 latitude
  mica$data$deployments$coordinateUncertainty[[3]] <- NA_integer_
  mica$data$deployments$longitude[[4]] <- 5.65
  mica$data$deployments$latitude[[4]] <- -85.18 # 85 latitude
  mica$data$deployments$coordinateUncertainty[[4]] <- NA_integer_

  mica1 <- round_coordinates(mica, 1)
  expect_equal(mica1$coordinatePrecision, 0.1)
  expect_equal(mica1$data$deployments$longitude[[1]], 5.7)
  expect_equal(mica1$data$deployments$latitude[[1]], 15.2)
  expect_equal(mica1$data$deployments$coordinateUncertainty[[1]], 10 + 15691)
  expect_equal(mica1$data$deployments$longitude[[2]], 5.7)
  expect_equal(mica1$data$deployments$latitude[[2]], 51.2)
  expect_equal(mica1$data$deployments$coordinateUncertainty[[2]], 30 + 15691)
  expect_equal(mica1$data$deployments$longitude[[3]], 5.7)
  expect_equal(mica1$data$deployments$latitude[[3]], -61.2)
  expect_equal(mica1$data$deployments$coordinateUncertainty[[3]], 30 + 15691)
  expect_equal(mica1$data$deployments$longitude[[4]], 5.7)
  expect_equal(mica1$data$deployments$latitude[[4]], -85.2)
  expect_equal(mica1$data$deployments$coordinateUncertainty[[4]], 30 + 15691)

  mica2 <- round_coordinates(mica, 2)
  expect_equal(mica2$coordinatePrecision, 0.01)
  expect_equal(mica2$data$deployments$longitude[[1]], 5.66)
  expect_equal(mica2$data$deployments$latitude[[1]], 15.18)
  expect_equal(mica2$data$deployments$coordinateUncertainty[[1]], 10 + 1570)
  expect_equal(mica2$data$deployments$longitude[[2]], 5.65)
  expect_equal(mica2$data$deployments$latitude[[2]], 51.18)
  expect_equal(mica2$data$deployments$coordinateUncertainty[[2]], 30 + 1570)
  expect_equal(mica2$data$deployments$longitude[[3]], 5.65)
  expect_equal(mica2$data$deployments$latitude[[3]], -61.18)
  expect_equal(mica2$data$deployments$coordinateUncertainty[[3]], 30 + 1570)
  expect_equal(mica2$data$deployments$longitude[[4]], 5.65)
  expect_equal(mica2$data$deployments$latitude[[4]], -85.18)
  expect_equal(mica2$data$deployments$coordinateUncertainty[[4]], 30 + 1570)

  mica3 <- round_coordinates(mica, 3)
  expect_equal(mica3$coordinatePrecision, 0.001)
  expect_equal(mica3$data$deployments$longitude[[1]], 5.656)
  expect_equal(mica3$data$deployments$latitude[[1]], 15.182)
  expect_equal(mica3$data$deployments$coordinateUncertainty[[1]], 10 + 157)
  expect_equal(mica3$data$deployments$longitude[[2]], 5.65) # Unchanged
  expect_equal(mica3$data$deployments$latitude[[2]], 51.18) # Unchanged
  expect_equal(mica3$data$deployments$coordinateUncertainty[[2]], 30 + 157)
  expect_equal(mica3$data$deployments$longitude[[3]], 5.65) # Unchanged
  expect_equal(mica3$data$deployments$latitude[[3]], -61.18) # Unchanged
  expect_equal(mica3$data$deployments$coordinateUncertainty[[3]], 30 + 157)
  expect_equal(mica3$data$deployments$longitude[[4]], 5.65) # Unchanged
  expect_equal(mica3$data$deployments$latitude[[4]], -85.18) # Unchanged
  expect_equal(mica3$data$deployments$coordinateUncertainty[[4]], 30 + 157)
})

test_that("round_coordinates() does not allow to round to higher precision", {
  mica2 <- round_coordinates(mica, 2)

  # Based on package$coordinatePrecision
  expect_error(
    round_coordinates(mica2, 3),
    paste(
      "Can't round from 2 to 3 digits.",
      "`2` is derived from the `package$coordinatePrecision=0.01`."
    ),
    fixed = TRUE
  )

  # Based on data
  mica2$coordinatePrecision <- NULL
  expect_error(
    round_coordinates(mica2, 3),
    paste(
      "Can't round from 2 to 3 digits.",
      "`2` is the maximum number of decimals for latitude in the data."
    ),
    fixed = TRUE
  )
})

test_that("round_coordinates() doesn't overestimate uncertainty on multiple runs", {
  # Set uncertainty of first 2 deployments
  mica$data$deployments$coordinateUncertainty[[1]] <- 10
  mica$data$deployments$coordinateUncertainty[[2]] <- NA_integer_

  mica1 <- round_coordinates(mica, 1)
  mica1_via_2 <- round_coordinates(round_coordinates(mica, 2), 1)
  expect_equal(
    mica1_via_2$coordinatePrecision,
    mica1$coordinatePrecision
  )
  expect_equal(
    mica1_via_2$data$deployments$coordinateUncertainty[[1]],
    mica1$data$deployments$coordinateUncertainty[[1]]
  )
  expect_equal(
    mica1_via_2$data$deployments$coordinateUncertainty[[2]],
    mica1$data$deployments$coordinateUncertainty[[2]]
  )
})
