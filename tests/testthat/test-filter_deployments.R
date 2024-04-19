test_that("filter_deployments() returns a Camtrap DP object, same version", {
  skip_if_offline()
  x <- example_dataset()
  filtered_dp <- filter_deployments(x, latitude > 51)
  expect_s3_class(filtered_dp, "camtrapdp")
  expect_s3_class(filtered_dp, "datapackage")
  expect_s3_class(filtered_dp, "list")
  expect_identical(version(filtered_dp), version(x))
})

test_that("filter_deployments() returns error if condition is wrong", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(filter_deployments(x, non_existing_col > 51),
               regex = "object 'non_existing_col' not found")
})

test_that("filter_deployments() filters correctly on deploys, media and obs", {
  skip_if_offline()
  x <- example_dataset()
  filtered_dp <- filter_deployments(x, latitude > 51.4)
  expect_identical(
    deployments(filtered_dp),
    dplyr::filter(deployments(x), latitude > 51.4)
  )
  # Only observations from the left deployment are present
  deploys_id <- unique(purrr::pluck(deployments(filtered_dp), "deploymentID"))
  expect_identical(
    observations(filtered_dp),
    dplyr::filter(observations(x), deploymentID %in% deploys_id)
  )
  # Only media taken by the left deployment are present
  expect_identical(
    media(filtered_dp),
    dplyr::filter(media(x), deploymentID %in% deploys_id)
  )
})
