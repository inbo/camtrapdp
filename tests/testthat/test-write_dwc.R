test_that("write_dwc() writes CSV and meta.xml files to a directory and
           a list of data frames invisibly", {
  skip_if_offline()
  x <- example_dataset()
  temp_dir <- file.path(tempdir(), "dwc")
  on.exit(unlink(temp_dir, recursive = TRUE))
  result <- suppressMessages(write_dwc(x, temp_dir))

  expect_identical(
    list.files(temp_dir),
    c("dwc_audiovisual.csv", "dwc_occurrence.csv", "meta.xml")
  )
  expect_identical(names(result), c("dwc_occurrence", "dwc_audiovisual"))
  expect_s3_class(result$dwc_occurrence, "tbl")
  expect_s3_class(result$dwc_audiovisual, "tbl")
})

test_that("write_dwc() returns the expected Darwin Core terms as columns", {
  skip_if_offline()
  x <- example_dataset()
  temp_dir <- file.path(tempdir(), "dwc")
  on.exit(unlink(temp_dir, recursive = TRUE))
  result <- suppressMessages(write_dwc(x, temp_dir))

  expect_identical(
    colnames(result$dwc_occurrence),
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
      "identificationRemarks",
      "taxonID",
      "scientificName",
      "kingdom"
    )
  )
  expect_identical(
    colnames(result$dwc_audiovisual),
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
  temp_dir <- file.path(tempdir(), "dwc")
  on.exit(unlink(temp_dir, recursive = TRUE))
  suppressMessages(write_dwc(x, temp_dir))

  expect_snapshot_file(file.path(temp_dir, "dwc_occurrence.csv"))
  expect_snapshot_file(file.path(temp_dir, "dwc_audiovisual.csv"))
  expect_snapshot_file(file.path(temp_dir, "meta.xml"))
})

test_that("write_dwc() returns files that comply with the info in meta.xml", {
  skip_if_offline()
  x <- example_dataset()
  temp_dir <- file.path(tempdir(), "dwc")
  on.exit(unlink(temp_dir, recursive = TRUE))
  suppressMessages(write_dwc(x, temp_dir))

  # Use helper function to compare
  expect_comform_meta(file.path(temp_dir, "dwc_occurrence.csv"))
  expect_comform_meta(file.path(temp_dir, "dwc_audiovisual.csv"))
})
