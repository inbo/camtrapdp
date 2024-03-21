test_that("locations() returns a tibble", {
  skip_if_offline()
  x <- example_dataset()
  expect_s3_class(locations(x), "tbl")
})

test_that("locations() returns expected columns", {
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

test_that("locations() returns unique location information", {
  skip_if_offline()
  x <- example_dataset()
  expect_identical(nrow(locations(x)), 4)
  # TODO: reduce cardinality of location information in deployments and expect fewer rows
})
