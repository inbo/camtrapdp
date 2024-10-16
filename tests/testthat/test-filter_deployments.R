test_that("filter_deployments() returns a valid camtrapdp object", {
  skip_if_offline()
  x <- example_dataset()
  expect_no_error(
    check_camtrapdp(filter_deployments(x, deploymentID == "62c200a9"))
  )
})

test_that("filter_deployments() returns error on wrong condition", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(
    filter_deployments(x, non_existing_col == "62c200a9"),
    regex = "object 'non_existing_col' not found",
    fixed = TRUE
  )
})

test_that("filter_deployments() supports combinations of conditions", {
  skip_if_offline()
  x <- example_dataset()

  x_single <- filter_deployments(x, deploymentID == "62c200a9")
  expect_equal(nrow(deployments(x_single)), 1) # 1 deployments
  x_multiple <- filter_deployments(x, cameraHeight >= 1.0)
  expect_equal(nrow(deployments(x_multiple)), 2) # 2 heights: 1.0, 1.3
  x_or <- filter_deployments(x, latitude > 51.0 | longitude > 5.0)
  expect_equal(nrow(deployments(x_or)), 3) # 3 latitudes: 51.5, 51.2, 51.2
  x_and <- filter_deployments(x, latitude > 51.0, longitude > 5.0)
  expect_equal(nrow(deployments(x_and)), 2) # 2 longitudes: 5.66, 5.66
  x_empty <- filter_deployments(x, latitude < 0, longitude < 0)
  expect_equal(nrow(deployments(x_empty)), 0) # 0 matching records
  x_between <- filter_deployments(
    x,
    deploymentStart >= lubridate::as_date("2020-06-19"),
    deploymentEnd <= lubridate::as_date("2020-08-09"),
  )
  expect_equal(nrow(deployments(x_between)), 2) # 2020-06-19 21:00:00 till 2020-08-08 04:20:40
  x_combo <- filter_deployments(x, setupBy %in% c("Danny Van der beeck") | is.na(setupBy))
  expect_equal(nrow(deployments(x_combo)), 3) # 2 Danny van der beeck & 1 NA
})

test_that("filter_deployments() filters deployments, media and observations", {
  skip_if_offline()
  x <- example_dataset()

  x_filtered <- filter_deployments(x, deploymentID == "62c200a9")
  expect_lt(nrow(deployments(x_filtered)), nrow(deployments(x)))
  expect_lt(nrow(media(x_filtered)), nrow(media(x)))
  expect_lt(nrow(observations(x_filtered)), nrow(observations(x)))

  x_empty <- filter_deployments(x, latitude < 0, longitude < 0)
})

test_that("filter_deployments() updates temporal and spatial scope in metadata", {
  skip_if_offline()
  x <- example_dataset()

  # One deployment
  x_filtered <- filter_deployments(x, deploymentID == "62c200a9")
  expect_identical(x_filtered$temporal$start, "2021-03-27")
  expect_identical(x_filtered$temporal$end, "2021-04-18")
  coordinates_array <- array(
    c(
      4.013, 4.013, 4.013, 4.013, 4.013,
      50.699, 50.699, 50.699, 50.699, 50.699
    ),
    dim = c(1, 5, 2)
  )
  expect_identical(x_filtered$spatial$coordinates, coordinates_array)

  # Multiple deployments
  x_multiple <- filter_deployments(x, cameraHeight >= 1.0)
  expect_identical(x_multiple$temporal$start, "2020-05-30")
  expect_identical(x_multiple$temporal$end, "2021-04-18")
  coordinates_array <- array(
    c(
      4.013, 4.774, 4.774, 4.013, 4.013,
      50.699, 50.699, 51.496, 51.496, 50.699
    ),
    dim = c(1, 5, 2)
  )
  expect_identical(x_multiple$spatial$coordinates, coordinates_array)

  # Zero deployments
  x_empty <- filter_deployments(x, latitude < 0, longitude < 0)
  expect_null(x_empty$temporal)
  expect_null(x_empty$spatial)
})
