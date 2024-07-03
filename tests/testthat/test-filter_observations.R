test_that("filter_observations() returns a valid camtrapdp object", {
  skip_if_offline()
  x <- example_dataset()
  expect_no_error(
    check_camtrapdp(filter_observations(x, observationType == "animal"))
  )
})

test_that("filter_observations() returns error on wrong condition", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(
    filter_observations(x, non_existing_col == "animal"),
    regex = "object 'non_existing_col' not found",
    fixed = TRUE
  )
})

test_that("filter_observations() supports combinations of conditions", {
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
  expect_equal(nrow(observations(x_between)), 12) # 2020-06-19 22:00:00 till 2020-06-19 T22:06:00
  x_combo <- filter_observations(x, eventID %in% c("a80896b5", "5fbf69a4"))
  expect_equal(nrow(observations(x_combo)), 12) # Same as x_between: 1 timestamp, 1 event, 10 media
})

test_that("filter_observations() filters observations and media, but not deployments", {
  skip_if_offline()
  x <- example_dataset()
  x_filtered <- filter_observations(
    x,
    scientificName == "Vulpes vulpes",
    observationLevel == "media"
  )
  expect_equal(deployments(x_filtered), deployments(x))
  expect_lt(nrow(media(x_filtered)), nrow(media(x)))
  expect_equal(nrow(media(x_filtered)), 6) # 6 directly linked media files
  expect_lt(nrow(observations(x_filtered)), nrow(observations(x)))

  # When event-based observations are included, include all media files from event
  x_filtered_event <- filter_observations(
    x,
    scientificName == "Vulpes vulpes",
    observationLevel == "event"
  )
  expect_equal(nrow(media(x_filtered_event)), 10) # All media files in the event
})

test_that("filter_observations() updates the taxonomic property as default", {
  skip_if_offline()
  x <- example_dataset()
  x_vulpes_media <- filter_observations(
    x,
    scientificName == "Vulpes vulpes",
    observationLevel == "media"
  )
  remaining_taxa_obs <- unique(observations(x_vulpes_media)$scientificName)
  remaining_taxa_tax <-
    purrr::map_chr(x_vulpes_media$taxonomic, ~ purrr::pluck(.x, "scientificName"))
  expect_equal(remaining_taxa_obs, remaining_taxa_tax)

  x_animal <- filter_observations(x, observationType == "animal")
  remaining_taxa_obs <-
    unique(observations(x_animal)$scientificName) %>%
    sort()
  remaining_taxa_tax <-
    purrr::map_chr(x_animal$taxonomic, ~ purrr::pluck(.x, "scientificName")) %>%
    sort()
  expect_equal(remaining_taxa_obs, remaining_taxa_tax)
})

test_that("filter_observations() does not update the taxonomic property when update_metadata == FALSE", {
  skip_if_offline()
  x <- example_dataset()
  original_taxa <-
    purrr::map_chr(x$taxonomic, ~ purrr::pluck(.x, "scientificName")) %>%
    sort()
  x_vulpes_media <- filter_observations(
    x,
    scientificName == "Vulpes vulpes",
    observationLevel == "media",
    update_metadata = FALSE
  )
  remaining_taxa_tax <-
    purrr::map_chr(x_vulpes_media$taxonomic, ~ purrr::pluck(.x, "scientificName"))
  expect_equal(original_taxa, remaining_taxa_tax)
})
