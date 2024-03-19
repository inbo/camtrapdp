test_that("locations returns a tibble data.frame", {
  locs <- locations(dataset)
  expect_true(is.data.frame(locs))
  expect_true(is_tibble(locs))
})


test_that("locations returns the expected columns", {
  cols_locations <- c("locationID",
                 "locationName",
                 "latitude",
                 "longitude",
                 "coordinateUncertainty"
  )
  expect_equal(names(locations(dataset)), cols_locations)
})
