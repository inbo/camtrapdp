test_that("write_dwc() writes csv files to a path", {
  skip_if_offline()
  x <- example_dataset()
  out_dir <- file.path(tempdir(), "dwc")
  on.exit(unlink(out_dir, recursive = TRUE))
  suppressMessages(write_dwc(x, directory = out_dir))
  expect_identical(
    list.files(out_dir, pattern = "*.csv"),
    c("dwc_audubon.csv", "dwc_occurrence.csv")
  )
})

test_that("write_dwc() can return data as list of tibbles rather than files", {
  skip_if_offline()
  x <- example_dataset()
  result <- suppressMessages(write_dwc(x, directory = NULL))

  expect_identical(names(result), c("dwc_occurrence", "dwc_audubon"))
  expect_s3_class(result$dwc_occurrence, "tbl")
  expect_s3_class(result$dwc_audubon, "tbl")
  # meta.xml is not included
})

test_that("write_dwc() writes the expected meta.xml", {
  skip_if_offline()
  x <- example_dataset()
  out_dir <- file.path(tempdir(), "dwc_meta")
  on.exit(unlink(out_dir, recursive = TRUE))
  suppressMessages(write_dwc(x, directory = out_dir))

  expect_true("meta.xml" %in% list.files(out_dir))
  expect_snapshot_file(file.path(out_dir, "meta.xml"))
})

test_that("write_dwc() returns the expected Darwin Core terms as columns", {
  skip_if_offline()
  x <- example_dataset()
  result <- suppressMessages(write_dwc(x, directory = NULL))

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
    colnames(result$dwc_audubon),
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
      "dc:format"
    )
  )
})

test_that("write_dwc() returns the expected Darwin Core mapping for a known dataset", {
  skip_if_offline()
  x <- example_dataset()
  out_dir <- file.path(tempdir(), "dwc_mapping")
  on.exit(unlink(out_dir, recursive = TRUE))

  # Use helper function that outputs path write_dwc() wrote to.
  expect_snapshot_file(write_dwc_snapshot(x, out_dir, "occurrence"))
  expect_snapshot_file(write_dwc_snapshot(x, out_dir, "audubon"))
})

test_that("write_dwc() returns files that comply with the info in meta.xml", {
  skip_if_offline()
  x <- example_dataset()
  out_dir <- file.path(tempdir(), "dwc")
  on.exit(unlink(out_dir, recursive = TRUE))
  suppressMessages(write_dwc(x, out_dir))

  # Test if all fields are present, in the right order
  expect_fields(file.path(out_dir,"dwc_occurrence.csv"))
  expect_fields(file.path(out_dir,"dwc_audubon.csv"))
  # Test if the file locations (filenames) are the same as in meta.xml
  expect_location(file.path(out_dir,"dwc_occurrence.csv"))
  expect_location(file.path(out_dir,"dwc_audubon.csv"))
})
