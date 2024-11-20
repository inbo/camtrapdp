test_that("merge_camtrapdp() returns a valid camtrapdp object", {
  skip_if_offline()
  x <- example_dataset()
  x$name <- "x"
  y <- x
  y$name <- "y"

  expect_no_error(check_camtrapdp(merge_camtrapdp(x, y)))
})

test_that("merge_camtrapdp() returns error on missing/invalid/duplicate dataset
           name(s)", {
  skip_if_offline()
  x <- example_dataset()

  # Duplicate identifiers
  expect_error(
    merge_camtrapdp(x, x),
    class = "camtrapdp_error_name_duplicated"
  )

  # Invalid identifier
  y <- x
  x$name <- NULL
  expect_error(
    merge_camtrapdp(x, y),
    class = "camtrapdp_error_name_invalid"
  )
  x$name <- NA_character_
  expect_error(
    merge_camtrapdp(x, y),
    class = "camtrapdp_error_name_invalid"
  )
  x$name <- 1
  expect_error(
    merge_camtrapdp(x, y),
    class = "camtrapdp_error_name_invalid"
  )
  x$name <- "x"
  y$name <- 1
  expect_error(
    merge_camtrapdp(x, y),
    class = "camtrapdp_error_name_invalid"
  )
  y$name <- "y"
  expect_no_error(merge_camtrapdp(x, y))
})

test_that("merge_camtrapdp() adds prefixes to additional resource names to keep
           them unique", {
  skip_if_offline()

  # Merge datasets with overlapping resources: individuals and individuals, iris
  x <- example_dataset()
  x$name <- "x"
  y <- x
  y <- frictionless::add_resource(y, "iris", iris)
  y$name <- "y"
  xy <- merge_camtrapdp(x, y)

  expect_identical(
    frictionless::resources(xy),
    c(
      "deployments", "media", "observations", "x_individuals", "y_individuals",
      "iris"
    )
  )
})

test_that("merge_camtrapdp() adds prefixes to identifiers in the data to keep
           them unique", {
  skip_if_offline()

  # Merge datasets with overlapping deployments: a, b and b, c
  x <- example_dataset() %>%
    filter_deployments(deploymentID %in% c("00a2c20d", "29b7d356")) # a, b
  x$name <- "x"
  y <- example_dataset() %>%
    filter_deployments(deploymentID %in% c("29b7d356", "577b543a")) # b, c
  y$name <- "y"
  xy <- merge_camtrapdp(x, y)

  # No duplicate primary keys
  deployment_ids <- purrr::pluck(deployments(xy), "deploymentID")
  media_ids <- purrr::pluck(media(xy), "mediaID")
  observation_ids <- purrr::pluck(observations(xy), "observationID")
  expect_false(any(duplicated(deployment_ids)))
  expect_false(any(duplicated(media_ids)))
  expect_false(any(duplicated(observation_ids)))

  # deploymentID
  merged_deployment_ids <- c("00a2c20d", "x_29b7d356", "y_29b7d356", "577b543a")
  expect_in(merged_deployment_ids, deployments(xy)$deploymentID)
  expect_in(merged_deployment_ids, media(xy)$deploymentID)
  expect_in(merged_deployment_ids, observations(xy)$deploymentID)

  # eventID
  merged_event_ids <- c("4bb69c45", "x_8f5ffbf2", "y_8f5ffbf2", "5fbf69a4")
  expect_in(merged_event_ids, media(xy)$eventID)
  expect_in(merged_event_ids, observations(xy)$eventID)

  # mediaID
  merged_media_ids <- c("07840dcc", "x_3e65dfaa", "y_3e65dfaa", "44201e9e")
  expect_in(merged_media_ids, media(xy)$mediaID)
  expect_in(merged_media_ids, observations(xy)$mediaID)

  # observationID
  merged_observation_ids <- c("705e6036", "x_ef2f7140", "y_ef2f7140", "d350d2bc")
  expect_in(merged_observation_ids, observations(xy)$observationID)
})

test_that("merge_camtrapdp() returns the expected datapackage.json when merging
           identical datasets", {
  skip_if_offline()
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Merge datasets that are identical except for name
  x <- example_dataset()
  x$name <- "x"
  y <- x
  y$name <- "y"
  xy <- merge_camtrapdp(x, y)
  xy$created <- NULL

  # Write to file
  write_camtrapdp(xy, temp_dir)
  file.rename(
    file.path(temp_dir, "datapackage.json"),
    file.path(temp_dir, "datapackage_identical_xy.json")
  )

  expect_snapshot_file(file.path(temp_dir, "datapackage_identical_xy.json"))
  expect_no_error(
    read_camtrapdp(file.path(temp_dir, "datapackage_identical_xy.json"))
  )
})

test_that("merge_camtrapdp() returns the expected datapackage.json when merging
           different datasets", {
   skip_if_offline()
   temp_dir <- tempdir()
   on.exit(unlink(temp_dir, recursive = TRUE))

   # Merge datasets that are different: example_dataset + awd_pilot2
   x <- example_dataset()
   x$name <- "x"
   y_url <- "https://ipt.nlbif.nl/archive.do?r=awd_pilot2"
   zip_file <- file.path(temp_dir, "dataset.zip")
   datapackage_file <- file.path(temp_dir, "datapackage.json")
   download.file(y_url, zip_file, mode = "wb", quiet = TRUE)
   unzip(zip_file, exdir = temp_dir)
   y <- read_camtrapdp(datapackage_file)
   xy <- merge_camtrapdp(x, y)
   xy$created <- NULL

   # Write to file
   temp_dir_merged <- file.path(temp_dir, "merged")
   write_camtrapdp(xy, temp_dir_merged)
   file.rename(
     file.path(temp_dir_merged, "datapackage.json"),
     file.path(temp_dir_merged, "datapackage_different_xy.json")
   )

   expect_snapshot_file(
     file.path(temp_dir_merged, "datapackage_different_xy.json")
   )
   expect_no_error(
     read_camtrapdp(file.path(temp_dir_merged, "datapackage_different_xy.json"))
   )
})

test_that("merge_camtrapdp() adds the DOI of the original datasets as related
           identifiers", {
  skip_if_offline()

  # Merge datasets that are identical except for name and DOI
  x <- example_dataset()
  x$name <- "x"
  x$id <- "https://doi.org/x"
  y <- x
  y$name <- "y"
  y$id <- "http://doi.org/y"
  xy <- merge_camtrapdp(x, y)

  expect_identical(
    xy$relatedIdentifiers[[3]], # 1 and 2 were present in x and y (identical)
    list(
      relationType = "isDerivedFrom",
      relatedIdentifier = "https://doi.org/x",
      resourceTypeGeneral = "Dataset",
      relatedIdentifierType = "DOI"
    )
  )
  expect_identical(
    xy$relatedIdentifiers[[4]],
    list(
      relationType = "isDerivedFrom",
      relatedIdentifier = "http://doi.org/y",
      resourceTypeGeneral = "Dataset",
      relatedIdentifierType = "DOI"
    )
  )
})
