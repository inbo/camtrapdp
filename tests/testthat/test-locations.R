test_that("locations() returns a tibble", {
  skip_if_offline()
  x <- example_dataset()
  expect_s3_class(locations(x), "tbl")
})

test_that("locations() returns the expected columns", {
  skip_if_offline()
  x <- example_dataset()
  expected_cols <- c(
    "locationID",
    "locationName",
    "latitude",
    "longitude",
    "coordinateUncertainty"
  )
  expect_named(locations(x), expected_cols)
})

test_that("locations() returns the expected rows (unique locations)", {
  skip_if_offline()
  x <- example_dataset()
  expect_equal(nrow(locations(x)), 4)
  # Less or equal than deployments
  expect_lte(
    nrow(locations(x)),
    nrow(deployments(x))
  )
  # TODO: reduce cardinality of location information in deployments and expect fewer rows
})
