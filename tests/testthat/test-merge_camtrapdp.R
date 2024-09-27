test_that("merge_camtrapdp() returns a valid camtrapdp object", {
  skip_if_offline()
  x1 <- example_dataset() %>%
    filter_deployments(deploymentID %in% c("00a2c20d", "29b7d356"))
  x2 <- example_dataset() %>%
    filter_deployments(deploymentID %in% c("577b543a", "62c200a9"))
  expect_no_error(
    suppressMessages(
      check_camtrapdp(
        merge_camtrapdp(x1, x2)
        )
    )
  )
})

test_that("merge_camtrapdp() returns no duplicated deploymentIDs, mediaIDs
          and observationIDs", {
  skip_if_offline()
  x1 <- example_dataset()

  # Merge
  x_merged <- merge_camtrapdp(x1, x1)

  # Check for duplicates
  deploymentIDs <- purrr::pluck(deployments(x1), "deploymentID")
  mediaIDs <- purrr::pluck(media(x1), "mediaID")
  observationIDs <- purrr::pluck(observations(x1), "observationID")

  # Tests
  expect_false(any(duplicated(deploymentIDs)))
  expect_false(any(duplicated(mediaIDs)))
  expect_false(any(duplicated(observationIDs)))
})

test_that("merge_camtrapdp() adds suffix to duplicated IDs but not if mediaID = NA", {

})


