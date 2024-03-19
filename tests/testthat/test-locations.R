library(tibble)

test_that("locations returns a tibble data.frame", {
  skip_if_offline()
  dataset <- example_dataset()
  locs <- locations(dataset)
  expect_true(is.data.frame(locs))
  expect_true(is_tibble(locs))
})

test_that("locations returns the expected columns", {
  skip_if_offline()
  dataset <- example_dataset()
  locs <- locations(dataset)
  cols_locations <- c("locationID",
                 "locationName",
                 "latitude",
                 "longitude",
                 "coordinateUncertainty"
  )
  expect_equal(names(locs), cols_locations)
})

test_that("locations returns the expected amount of rows", {
  skip_if_offline()
  dataset <- example_dataset()
  locs <- locations(dataset)
  expect_equal(nrow(locs), 4)
})

test_that("locations never returns more rows than deployments", {
  skip_if_offline()
  dataset <- example_dataset()
  deploys <- deployments(dataset)
  locs <- locations(dataset)
  expect_true(nrow(locs) <= nrow(deploys))
})


test_that(
  "one row returned if all deployments have same location information", {
    skip_if_offline()
    dataset <- example_dataset()
    dataset$data$deployments$locationID <- NA
    dataset$data$deployments$locationName <- "A"
    dataset$data$deployments$latitude <- 50
    dataset$data$deployments$longitude <- 5
    dataset$data$deployments$coordinateUncertainty <- 100
    locs <- locations(dataset)
    expect_equal(nrow(locs), 1)
  })
