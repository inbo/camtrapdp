test_that("filter_observations() returns a Camtrap DP object, same version", {
  skip_if_offline()
  x <- example_dataset()
  filtered_dp <- filter_observations(x, observationType == "animal")
  expect_s3_class(filtered_dp, "camtrapdp")
  expect_s3_class(filtered_dp, "datapackage")
  expect_s3_class(filtered_dp, "list")
  expect_identical(version(filtered_dp), version(x))
})

test_that("filter_observations() returns error if condition is wrong", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(filter_observations(x, non_existing_col > 51),
               regex = "object 'non_existing_col' not found")
})

test_that("filter_observations() filters correctly on obs, deploys and media", {
  skip_if_offline()
  x <- example_dataset()
  x_single <- filter_observations(x, observationID == "1fcdba64")
  expect_equal(nrow(observations(x_single)), 1) # 1 observation
  x_multiple <- filter_observations(x, scientificName == "Vulpes vulpes")
  expect_equal(nrow(observations(x_multiple)), 7) # 1 event, 6 media
  x_or <- filter_observations(x, count == 4 | behavior == "foraging")
  expect_equal(nrow(observations(x_or)), 27) # 15 count 4, 11 foraging, 1 both
  x_and <- filter_observations(x, count == 4, behavior == "foraging")
  expect_equal(nrow(observations(x_and)), 1) # 1 with both
  x_empty <- filter_observations(x, count == 5, behavior == "not_a_behavior")
  expect_equal(nrow(observations(x_empty)), 0) # 0 matching records
  x_between <- filter_observations(
    x,
    eventStart >= lubridate::as_datetime("2020-06-19 22:00:00"),
    eventEnd <= lubridate::as_datetime("2020-06-19 22:10:00")
  )
})
