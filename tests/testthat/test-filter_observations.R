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

test_that("filter_observations() updates taxonomic scope in metadata", {
  skip_if_offline()
  x <- example_dataset()
  x_1_taxon <- filter_observations(
    x,
    scientificName == "Vulpes vulpes",
    observationLevel == "media"
  )
  expect_identical(
    purrr::map_chr(x_1_taxon$taxonomic, ~ purrr::pluck(.x, "scientificName")),
    "Vulpes vulpes"
  )
  expect_identical(
    purrr::map_chr(x_1_taxon$taxonomic, ~ purrr::pluck(.x, "vernacularNames", "eng")),
    "red fox" # Original data is still present
  )

  x_empty <- filter_observations(x, count == 5, behavior == "not_a_behavior")
  expect_null(x_empty$taxonomic)

  # Taxonomic scope is created when not present, names are alphabetical
  x$taxonomic <- NULL
  x_2_taxa <- filter_observations(
    x,
    scientificName %in% c("Mustela putorius", "Martes foina")
  )
  expected_taxononic <-  list(
    list(scientificName = "Martes foina"),
    list(scientificName = "Mustela putorius")
  )
  expect_identical(x_2_taxa$taxonomic, expected_taxononic)
})
