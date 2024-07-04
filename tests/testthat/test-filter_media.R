test_that("filter_media() returns a valid camtrapdp object", {
  skip_if_offline()
  x <- example_dataset()
  expect_no_error(
    check_camtrapdp(filter_media(x, captureMethod == "timeLapse"))
  )
})

test_that("filter_media() returns error on wrong condition", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(
    filter_media(x, non_existing_col == "timeLapse"),
    regex = "object 'non_existing_col' not found",
    fixed = TRUE
  )
})

test_that("filter_media() supports combinations of conditions", {
  skip_if_offline()
  x <- example_dataset()

  x_single <- filter_media(x, mediaID == "e8b8e44c")
  expect_equal(nrow(media(x_single)), 1) # 1 media
  x_multiple <- filter_media(x, captureMethod == "timeLapse")
  expect_equal(nrow(media(x_multiple)), 3) # 3 with timeLapse
  x_or <- filter_media(x, deploymentID == "62c200a9" | filePublic == FALSE)
  expect_equal(nrow(media(x_or)), 100) # All media from 62c200a9
  x_and <- filter_media(x, deploymentID == "62c200a9", filePublic == FALSE)
  expect_equal(nrow(media(x_and)), 20) # Hidden media from 62c200a9
  x_empty <- filter_media(x, favorite == TRUE, filePublic == FALSE)
  expect_equal(nrow(media(x_empty)), 0) # 0 matching records
  x_between <- filter_media(
    x,
    timestamp >= lubridate::as_datetime("2020-08-02 05:01:00"),
    timestamp <= lubridate::as_datetime("2020-08-02 05:02:00")
  )
  expect_equal(nrow(media(x_between)), 10) # 2020-08-02 05:01:00 till 2020-08-02 05:01:05
  x_combo <- filter_media(x, eventID %in% c("5fbf69a4") | is.na(eventID))
  expect_equal(nrow(media(x_combo)), 13) # 10 & 3
})

test_that("filter_media() filters media and observations, but not deployments", {
  skip_if_offline()
  x <- example_dataset()
  x_filtered <- filter_media(x, favorite == TRUE)

  expect_equal(deployments(x_filtered), deployments(x))
  expect_lt(nrow(media(x_filtered)), nrow(media(x)))
  expect_lt(nrow(observations(x_filtered)), nrow(observations(x)))
  expect_equal(nrow(observations(x_filtered)), 4) # 1 event, 3 media observations
})


test_that("filter_media() updates the taxonomic property", {
  skip_if_offline()
  x <- example_dataset()
  x_favorite <- filter_media(x, favorite == TRUE)
  remaining_taxa_obs <- unique(observations(x_favorite)$scientificName)
  remaining_taxa_tax <-
    purrr::map_chr(x_favorite$taxonomic, ~ purrr::pluck(.x, "scientificName"))
  expect_equal(remaining_taxa_obs, remaining_taxa_tax)

  x_filtered <-
    filter_media(x, captureMethod == "activityDetection", filePublic == FALSE)
  remaining_taxa_obs <-
    unique(observations(x_filtered)$scientificName) %>%
    sort()
  remaining_taxa_tax <-
    purrr::map_chr(
      x_filtered$taxonomic, ~ purrr::pluck(.x, "scientificName")
    ) %>%
    sort()
  expect_equal(remaining_taxa_obs, remaining_taxa_tax)
})
