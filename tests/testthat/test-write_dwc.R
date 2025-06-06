test_that("write_dwc() writes CSV and meta.xml files to a directory and
           a list of data frames invisibly", {
  skip_if_offline()
  x <- example_dataset()
  temp_dir <- file.path(tempdir(), "dwc")
  on.exit(unlink(temp_dir, recursive = TRUE))
  result <- suppressMessages(write_dwc(x, temp_dir))

  expect_identical(
    list.files(temp_dir),
    c("meta.xml", "multimedia.csv", "occurrence.csv")
  )
  expect_identical(names(result), c("occurrence", "multimedia"))
  expect_s3_class(result$occurrence, "tbl")
  expect_s3_class(result$multimedia, "tbl")
  expect_invisible(suppressMessages(write_dwc(x, temp_dir)))
})

test_that("write_dwc() returns the expected Darwin Core terms as columns", {
  skip_if_offline()
  x <- example_dataset()
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  result <- suppressMessages(write_dwc(x, temp_dir))

  expect_identical(
    colnames(result$occurrence),
    c(
      "type",
      "license",
      "rightsHolder",
      "datasetID",
      "collectionCode",
      "datasetName",
      "basisOfRecord",
      "dataGeneralizations",
      "occurrenceID",
      "individualCount",
      "sex",
      "lifeStage",
      "behavior",
      "occurrenceStatus",
      "occurrenceRemarks",
      "organismID",
      "eventID",
      "parentEventID",
      "eventDate",
      "habitat",
      "samplingProtocol",
      "samplingEffort",
      "eventRemarks",
      "locationID",
      "locality",
      "minimumDepthInMeters",
      "maximumDepthInMeters",
      "minimumDistanceAboveSurfaceInMeters",
      "maximumDistanceAboveSurfaceInMeters",
      "decimalLatitude",
      "decimalLongitude",
      "geodeticDatum",
      "coordinateUncertaintyInMeters",
      "coordinatePrecision",
      "identifiedBy",
      "dateIdentified",
      "identificationVerificationStatus",
      "identificationRemarks",
      "taxonID",
      "scientificName",
      "kingdom"
    )
  )
  expect_identical(
    colnames(result$multimedia),
    c(
      "occurrenceID",
      "identifier",
      "dc:type",
      "comments",
      "dcterms:rights",
      "CreateDate",
      "captureDevice",
      "resourceCreationTechnique",
      "accessURI",
      "dc:format",
      "serviceExpectation"
    )
  )
})

test_that("write_dwc() returns the expected Darwin Core mapping for the example
           dataset", {
  skip_if_offline()
  x <- example_dataset()
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  suppressMessages(write_dwc(x, temp_dir))

  expect_snapshot_file(file.path(temp_dir, "occurrence.csv"))
  expect_snapshot_file(file.path(temp_dir, "multimedia.csv"))
  expect_snapshot_file(file.path(temp_dir, "meta.xml"))
})

test_that("write_dwc() can write media-based occurrences", {
  skip_if_offline()
  x <- example_dataset()
  x$gbifIngestion$observationLevel <- "media"
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  result <- suppressMessages(write_dwc(x, temp_dir))
  file.rename(
    file.path(temp_dir, "occurrence.csv"),
    file.path(temp_dir, "occurrence_media_based.csv")
  )
  file.rename(
    file.path(temp_dir, "multimedia.csv"),
    file.path(temp_dir, "multimedia_media_based.csv")
  )

  expect_snapshot_file(file.path(temp_dir, "occurrence_media_based.csv"))
  expect_snapshot_file(file.path(temp_dir, "multimedia_media_based.csv"))
  expect_true("07840dcc_1" %in% result$occurrence$occurrenceID)
  expect_false("705e6036" %in% result$occurrence$occurrenceID)
  expect_true(nrow(result$occurrence) == nrow(result$multimedia))
  expect_true("07840dcc_1" %in% result$occurrence$occurrenceID)
})

test_that("write_dwc() returns files that comply with the info in meta.xml", {
  skip_if_offline()
  x <- example_dataset()
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  suppressMessages(write_dwc(x, temp_dir))

  # Use helper function to compare
  expect_meta_match(file.path(temp_dir, "occurrence.csv"))
  expect_meta_match(file.path(temp_dir, "multimedia.csv"))
})

test_that("write_dwc() returns output when taxonID is missing", {
  skip_if_offline()
  x <- example_dataset()
  optional_cols <- c("taxon.taxonID")
  observations(x) <-
    dplyr::select(observations(x), -dplyr::all_of(optional_cols))
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))

  expect_no_error(suppressMessages(write_dwc(x, temp_dir)))
})
