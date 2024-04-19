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
  filtered_dp <- filter_observations(x, deploymentID == "29b7d356")
  expect_identical(
    observations(filtered_dp),
    dplyr::filter(observations(x), deploymentID == "29b7d356")
  )
  # No filtering applied to deployments: it allows to detect absences
  expect_identical(
    deployments(filtered_dp),
    deployments(x)
  )
  # No filtering applied to media: media can be linked by multiple observations
  expect_identical(
    media(filtered_dp),
    media(x)
  )
})
