test_that("merge_camtrapdp() returns a valid camtrapdp object", {
  skip_if_offline()
  x1 <- example_dataset()
  x2 <- example_dataset()
  x1$id <- "1"
  x2$id <- "2"

  expect_no_error(check_camtrapdp(merge_camtrapdp(x1, x2)))
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
  x1$id <- "1"
  x2$id <- "2"

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
  x1$id <- "1"
  x2$id <- "2"
  x_merged <- merge_camtrapdp(x1, x2)

  deploymentIDs <- purrr::pluck(deployments(x_merged), "deploymentID")
  mediaIDs <- purrr::pluck(media(x_merged), "mediaID")
  observationIDs <- purrr::pluck(observations(x_merged), "observationID")

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
  x1$id <- "1"
  x2$id <- "2"
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

test_that("merge_camtrapdp() returns the expected result", {
  skip_if_offline()
  x1 <- example_dataset()
  x2 <- example_dataset()
  x1$id <- "1"
  x2$id <- "2"
  x_merged <- merge_camtrapdp(x1, x2)

  # Check metadata
  expect_identical(x_merged$resources, x1$resources)
  expect_identical(x_merged$profile, "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0.1/camtrap-dp-profile.json")
  expect_identical(x_merged$name, NA)
  expect_identical(x_merged$id, NA)
  expect_identical(x_merged$title, NA)
  expect_identical(x_merged$contributors, x1$contributors)
  # no test for description
  expect_identical(x_merged$version, "1.0")
  expect_identical(x_merged$keywords, x1$keywords)
  expect_identical(x_merged$image, NULL)
  expect_identical(x_merged$homepage, NULL)
  expect_identical(x_merged$sources, x1$sources)
  expect_equal(x_merged$licenses, x1$licenses) # fails because remove_duplicates switches order of subelements
  expect_identical(x_merged$bibliographicCitation, NULL)
  expect_identical(x_merged$projects, list(x1$project, x2$project))
  expect_identical(x_merged$coordinatePrecision, x1$coordinatePrecision)
  expect_identical(x_merged$spatial, x1$spatial)
  expect_identical(x_merged$temporal, x1$temporal)
  expect_identical(x_merged$taxonomic, x1$taxonomic)
  expect_identical(x_merged$references, x1$references)
  expect_identical(x_merged$directory, x1$directory)

  relatedIdentifiers_merged <- list(
    list(
      relationType = "IsDerivedFrom",
      relatedIdentifier = "https://doi.org/10.15468/5tb6ze",
      resourceTypeGeneral = "Dataset",
      relatedIdentifierType = "DOI"
    ),
    list(
      relationType = "IsSupplementTo",
      relatedIdentifier = "https://inbo.github.io/camtraptor/",
      resourceTypeGeneral = "Software",
      relatedIdentifierType = "URL"
    ),
    list(
      relationType = "IsDerivedFrom",
      relatedIdentifier = "1",
      resourceTypeGeneral = "Data package",
      relatedIdentifierType = "id"
    ),
    list(
      relationType = "IsDerivedFrom",
      relatedIdentifier = "2",
      resourceTypeGeneral = "Data package",
      relatedIdentifierType = "id"
    )
  )

  expect_identical(x_merged$relatedIdentifiers, relatedIdentifiers_merged)

  # Check data
})
