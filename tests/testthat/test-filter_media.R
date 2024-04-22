test_that("filter_media() returns a Camtrap DP object, same version", {
  skip_if_offline()
  x <- example_dataset()
  filtered_dp <- filter_media(x, !is.na(favorite))
  expect_s3_class(filtered_dp, "camtrapdp")
  expect_s3_class(filtered_dp, "datapackage")
  expect_s3_class(filtered_dp, "list")
  expect_identical(version(filtered_dp), version(x))
})

test_that("filter_media() returns error if condition is wrong", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(filter_media(x, non_existing_col > 51),
               regex = "object 'non_existing_col' not found")
})

test_that("filter_media() filters correctly on media, deploys and obs", {
  skip_if_offline()
  x <- example_dataset()
  time_threshold <- as.POSIXct("2021-03-31 22:00:00")
  filtered_dp <- filter_media(x, timestamp > time_threshold)
  expect_identical(
    media(filtered_dp),
    dplyr::filter(media(x), timestamp > time_threshold)
  )
  # No filtering applied to deployments: it allows to detect absences
  expect_identical(
    deployments(filtered_dp),
    deployments(x)
  )
  # Only observations:
  # - with same `mediaID` as in media left
  media_id <- unique(purrr::pluck(media(filtered_dp), "mediaID"))
  expect_true(
    all(
      purrr::pluck(observations(filtered_dp), "mediaID") %in% media_id
    )
  )
  # Only observations:
  # - from the deployments mentioned in media left
  filtered_dp <- filter_media(x, timestamp > time_threshold)
  deploys_id <- unique(purrr::pluck(media(filtered_dp), "deploymentID"))
  expect_true(
    all(
      purrr::pluck(observations(filtered_dp), "deploymentID") %in% deploys_id
    )
  )
  # Only observations:
  # - where eventStart <= one or more timestamp of the media left <= eventEnd
  expect_true(
    all(
      any(
        observations(filtered_dp)$eventStart <= media(filtered_dp)$timestamp &
          media(filtered_dp)$timestamp <= observations(filtered_dp)$eventEnd
      )
    )
  )
})
