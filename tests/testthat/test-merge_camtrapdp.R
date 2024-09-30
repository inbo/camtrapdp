test_that("merge_camtrapdp() returns a valid camtrapdp object", {
  skip_if_offline()
  x1 <- example_dataset() %>%
    filter_deployments(deploymentID %in% c("00a2c20d", "29b7d356"))
  x2 <- example_dataset() %>%
    filter_deployments(deploymentID %in% c("577b543a", "62c200a9"))
  expect_no_error(
    suppressMessages(
      check_camtrapdp(
        merge_camtrapdp(x1, x2,)
      )
    )
  )
})

test_that("merge_camtrapdp() returns error on duplicate Data Package id", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(
    merge_camtrapdp(x, x),
    class = "camtrapdp_error_camtrapdpid_duplicated"
  )
})

test_that("merge_camtrapdp() returns error on invalid prefix", {
  skip_if_offline()
  x1 <- example_dataset()
  x2 <- example_dataset()
  x1$id <- 1
  x2$id <- 2

  expect_error(
    merge_camtrapdp(x1, x2, prefix = c(1, 2)),
    class = "camtrapdp_error_prefix_invalid"
  )
  expect_error(
    merge_camtrapdp(x1, x2, prefix = c("one", "two", "three")),
    class = "camtrapdp_error_prefix_invalid"
  )
  expect_error(
    merge_camtrapdp(x1, x2, prefix = c("one-", NA)),
    class = "camtrapdp_error_prefix_NA"
  )
  expect_no_error(merge_camtrapdp(x1, x2, prefix = c("this_", "works_")))
  prefix_ids <- c(paste0(x1$id, "-"), paste0(x2$id, "-"))
  expect_no_error(merge_camtrapdp(x1, x2, prefix = prefix_ids))
})

test_that("merge_camtrapdp() returns unique deploymentIDs, mediaIDs and
          observationIDs", {
  skip_if_offline()
  x1 <- example_dataset()
  x2 <- example_dataset()
  x1$id <- 1
  x2$id <- 2
  x_merged <- merge_camtrapdp(x1, x2)

  deploymentIDs <- purrr::pluck(deployments(x1), "deploymentID")
  mediaIDs <- purrr::pluck(media(x1), "mediaID")
  observationIDs <- purrr::pluck(observations(x1), "observationID")

  expect_false(any(duplicated(deploymentIDs)))
  expect_false(any(duplicated(mediaIDs)))
  expect_false(any(duplicated(observationIDs)))
})

test_that("merge_camtrapdp() adds prefixes to all values of identifiers
          (deploymentID, mediaID, observationID and eventID) with duplicates
          between packages, but not for mediaID = NA", {
  skip_if_offline()
  x1 <- example_dataset()
  x2 <- example_dataset()
  x1$id <- 1
  x2$id <- 2
  x_merged <- merge_camtrapdp(x1, x2, prefix = c("project1-", "project2-"))

  expect_true("project1-00a2c20d" %in% deployments(x_merged)$deploymentID)
  expect_true("project2-00a2c20d" %in% deployments(x_merged)$deploymentID)
  expect_true("project1-00a2c20d" %in% media(x_merged)$deploymentID)
  expect_true("project1-00a2c20d" %in% observations(x_merged)$deploymentID)

  expect_true("project1-07840dcc" %in% media(x_merged)$mediaID)
  expect_true("project1-07840dcc" %in% observations(x_merged)$mediaID)
  expect_false("project1-NA" %in% observations(x_merged)$mediaID)
  expect_true(NA %in% observations(x_merged)$mediaID)

  expect_true("project1-705e6036" %in% observations(x_merged)$observationID)

  expect_true("project1-4bb69c45" %in% media(x_merged)$eventID)
  expect_true("project1-4bb69c45" %in% observations(x_merged)$eventID)
})
