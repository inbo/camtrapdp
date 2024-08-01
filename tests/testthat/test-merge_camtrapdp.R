test_that("merge_camtrapdp() returns error on invalid name", {
  skip_if_offline()
  x1 <- example_dataset() %>%
    filter_deployments(deploymentID %in% c("00a2c20d", "29b7d356"))
  x2 <- example_dataset() %>%
    filter_deployments(deploymentID %in% c("577b543a", "62c200a9"))

  expect_error(merge_camtrapdp(x1, x2, "Name", "new title"))
  expect_error(merge_camtrapdp(x1, x2, "package name", "new title"))
  expect_error(merge_camtrapdp(x1, x2, "name?", "new title"))
  expect_error(merge_camtrapdp(x1, x2, "new/name", "new title"))
})

test_that("merge_camtrapdp() returns error on invalid title", {
  skip_if_offline()
  x1 <- example_dataset() %>%
    filter_deployments(deploymentID %in% c("00a2c20d", "29b7d356"))
  x2 <- example_dataset() %>%
    filter_deployments(deploymentID %in% c("577b543a", "62c200a9"))

  #invalid
  expect_error(merge_camtrapdp(x1, x2, "new_name", "Period.in the middle"))
  expect_error(merge_camtrapdp(x1, x2, "new_name", "start with lowercase"))
  expect_error(merge_camtrapdp(x1, x2, "new_name", "Invalid_character"))
  expect_error(
    merge_camtrapdp(x1, x2, "new_name", "Hello! Second sentence.")
    )

  # valid
  expect_no_error(merge_camtrapdp(x1, x2, "new_name", "This is a title"))
  expect_no_error(merge_camtrapdp(x1, x2, "new_name", "Title with punctuation."))
  expect_no_error(merge_camtrapdp(x1, x2, "new_name", "Title: with a colon"))
  expect_no_error(merge_camtrapdp(x1, x2, "new_name", "Title - with a hyphen."))
  expect_no_error(
    merge_camtrapdp(x1, x2, "new_name", "A bit of a longer sentence is ok!")
    )
})

test_that("merge_camtrapdp() returns a valid camtrapdp object", {
  skip_if_offline()
  x1 <- example_dataset() %>%
    filter_deployments(deploymentID %in% c("00a2c20d", "29b7d356"))
  x2 <- example_dataset() %>%
    filter_deployments(deploymentID %in% c("577b543a", "62c200a9"))
  expect_no_error(
    suppressMessages(
      check_camtrapdp(
        merge_camtrapdp(x1, x2, "new_package_name", "New title")
        )
    )
  )
})

test_that("merge_camtrapdp() returns unique deplpymentIDs, mediaIDs and
          observationIDs", {
  skip_if_offline()
  duplicated_deploymentID <- "00a2c20d"
  duplicated_mediaID <- "ca3ff293"
  x1 <- example_dataset() %>%
    filter_deployments(deploymentID %in% c(duplicated_deploymentID, "29b7d356"))
  x2 <- example_dataset() %>%
    filter_deployments(
      deploymentID %in% c(duplicated_deploymentID, "62c200a9")
      ) %>%
    filter_media(mediaID %in% c(duplicated_mediaID, "bf610120"))

  # get original IDs
  original_deploymentIDs <- c(
    purrr::pluck(deployments(x1), "deploymentID"),
    purrr::pluck(deployments(x2), "deploymentID")
  )
  original_mediaIDs <- c(
    purrr::pluck(media(x1), "mediaID"),
    purrr::pluck(media(x2), "mediaID")
  )
  original_observationIDs <- c(
    purrr::pluck(observations(x1), "observationID"),
    purrr::pluck(observations(x2), "observationID")
  )
  duplicated_observationID <-
    original_observationIDs[duplicated(original_observationIDs)]

  # merge
  x_merged <- suppressMessages(
    merge_camtrapdp(x1, x2, "new_package_name", "New title")
  )

  # get new IDs
  new_deploymentIDs <- purrr::pluck(deployments(x_merged), "deploymentID")
  new_mediaIDs <- purrr::pluck(media(x_merged), "mediaID")
  new_observationIDs <- purrr::pluck(observations(x_merged), "observationID")

  # tests
  expect_true(any(duplicated(original_deploymentIDs)))
  expect_false(any(duplicated(new_deploymentIDs)))
  expect_true(
    vdigest_crc32(duplicated_deploymentID) %in% new_deploymentIDs
    )
  expect_identical(
    c(duplicated_deploymentID, "29b7d356", "77b0e58b", "62c200a9"),
    new_deploymentIDs
  )

  expect_true(any(duplicated(original_mediaIDs)))
  expect_false(any(duplicated(new_mediaIDs)))
  expect_true(vdigest_crc32(duplicated_mediaID) %in% new_mediaIDs)

  expect_true(any(duplicated(original_observationIDs)))
  expect_false(any(duplicated(new_observationIDs)))
  expect_true(
    all(vdigest_crc32(duplicated_observationID) %in% new_observationIDs)
    )
})

test_that("merge_camtrapdp() returns message when IDs are replaced", {
  skip_if_offline()
  x1 <- example_dataset()
  x2 <- example_dataset() %>%
    filter_deployments(deploymentID %in% c("62c200a9")) %>%
    filter_media(mediaID %in% c("fb58a2b9", "0bb2566e", "a6a7a04c"))

  expect_message(
    merge_camtrapdp(x1, x2, "new_package_name", "New title") #,
    # regexp =  paste(
    #   "! `x1` and `x2` must have unique deploymentIDs.",
    #   "`x1` and `x2` have duplicated deploymentIDs: \"62c200a9\".",
    #   "Duplicated deploymentIDs of `x2` are now replaced by \"07ce6950\" respectively.",
    #   "! `x1` and `x2` must have unique mediaIDs.",
    #   "`x1` and `x2` have duplicated mediaIDs: \"fb58a2b9\", \"0bb2566e\", and \"a6a7a04c\".",
    #   "Duplicated mediaIDs of `x2` are now replaced by \"ba426f00\", \"8d5c0009\", and \"1689e0db\" respectively.",
    #   "! `x1` and `x2` must have unique observationIDs.",
    #   "`x1` and `x2` have duplicated observationIDs: \"a0431321\", \"fb58a2b9_1\", \"0bb2566e_1\", and \"a6a7a04c_1\".",
    #   "Duplicated observationIDs of `x2` are now replaced by \"c6eeccc0\", \"a8452c14\", \"a48adc8a\", and \"b78e02ba\" respectively."
    #   )
  )
})
