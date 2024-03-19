test_that("locations returns a tibble data.frame", {
  # Read camtrap DP (to be changed when example is created)
  dataset <- read_camtrapdp("https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json")
  locs <- locations(dataset)
  expect_true(is.data.frame(locs))
  expect_true(is_tibble(locs))
})


test_that("locations returns the expected columns", {
  # Read camtrap DP (to be changed when example is created)
  dataset <- read_camtrapdp("https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json")
  cols_locations <- c("locationID",
                 "locationName",
                 "latitude",
                 "longitude",
                 "coordinateUncertainty"
  )
  expect_equal(names(locations(dataset)), cols_locations)
})
