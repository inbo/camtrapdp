test_that("merge_camtrapdp() returns a valid camtrapdp object", {
  skip_if_offline()
  x <- example_dataset()
  y <- example_dataset()
  x$id <- "1"
  y$id <- "2"

  expect_no_error(check_camtrapdp(merge_camtrapdp(x, y)))
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
  x <- example_dataset()
  y <- example_dataset()
  x$id <- "1"
  y$id <- "2"
  expect_error(
    merge_camtrapdp(x, y, prefix = c(1, 2)),
    class = "camtrapdp_error_prefix_invalid"
  )
  expect_error(
    merge_camtrapdp(x, y, prefix = c("one", "two", "three")),
    class = "camtrapdp_error_prefix_invalid"
  )
  expect_error(
    merge_camtrapdp(x, y, prefix = c("one", NA)),
    class = "camtrapdp_error_prefix_NA"
  )

  expect_no_error(merge_camtrapdp(x, y))
  expect_no_error(merge_camtrapdp(x, y, prefix = c("this", "works")))

  x$id <- NULL
  y$id <- NULL
  expect_error(
    merge_camtrapdp(x, y),
    class = "camtrapdp_error_prefix_invalid"
  )
})

test_that("merge_camtrapdp() returns unique deploymentIDs, mediaIDs and
          observationIDs", {
  skip_if_offline()
  x <- example_dataset()
  y <- example_dataset()
  x$id <- "1"
  y$id <- "2"
  xy_merged <- merge_camtrapdp(x, y)

  deploymentIDs <- purrr::pluck(deployments(xy_merged), "deploymentID")
  mediaIDs <- purrr::pluck(media(xy_merged), "mediaID")
  observationIDs <- purrr::pluck(observations(xy_merged), "observationID")

  expect_false(any(duplicated(deploymentIDs)))
  expect_false(any(duplicated(mediaIDs)))
  expect_false(any(duplicated(observationIDs)))
})

test_that("merge_camtrapdp() adds default prefixes to all values of identifiers
          (deploymentID, mediaID, observationID and eventID) with duplicates
          between packages, but not for mediaID = NA", {
  skip_if_offline()
  x <- example_dataset()
  y <- example_dataset()
  x$id <- "1"
  y$id <- "2"
  xy_merged <- merge_camtrapdp(x, y)

  # deploymentID
  expect_true("1_00a2c20d" %in% deployments(xy_merged)$deploymentID)
  expect_true("2_00a2c20d" %in% deployments(xy_merged)$deploymentID)
  expect_true("1_00a2c20d" %in% media(xy_merged)$deploymentID)
  expect_true("2_00a2c20d" %in% media(xy_merged)$deploymentID)
  expect_true("1_00a2c20d" %in% observations(xy_merged)$deploymentID)
  expect_true("2_00a2c20d" %in% observations(xy_merged)$deploymentID)

  # mediaID
  expect_true("1_07840dcc" %in% media(xy_merged)$mediaID)
  expect_true("2_07840dcc" %in% media(xy_merged)$mediaID)
  expect_true("1_07840dcc" %in% observations(xy_merged)$mediaID)
  expect_true("2_07840dcc" %in% observations(xy_merged)$mediaID)
  expect_false("1_NA" %in% observations(xy_merged)$mediaID)
  expect_true(NA %in% observations(xy_merged)$mediaID)

  # observationID
  expect_true("1_705e6036" %in% observations(xy_merged)$observationID)
  expect_true("2_705e6036" %in% observations(xy_merged)$observationID)

  # eventID
  expect_true("1_4bb69c45" %in% media(xy_merged)$eventID)
  expect_true("2_4bb69c45" %in% media(xy_merged)$eventID)
  expect_true("1_4bb69c45" %in% observations(xy_merged)$eventID)
  expect_true("2_4bb69c45" %in% observations(xy_merged)$eventID)
})

test_that("merge_camtrapdp() adds custom prefixes to all values of identifiers
          (deploymentID, mediaID, observationID and eventID) with duplicates
          between packages, but not for mediaID = NA", {
  skip_if_offline()
  x <- example_dataset()
  y <- example_dataset()
  x$id <- NULL
  y$id <- NULL
  xy_merged <- merge_camtrapdp(x, y, prefix = c("x", "y"))

  # deploymentID
  expect_true("x_00a2c20d" %in% deployments(xy_merged)$deploymentID)
  expect_true("y_00a2c20d" %in% deployments(xy_merged)$deploymentID)
  expect_true("x_00a2c20d" %in% media(xy_merged)$deploymentID)
  expect_true("y_00a2c20d" %in% media(xy_merged)$deploymentID)
  expect_true("x_00a2c20d" %in% observations(xy_merged)$deploymentID)
  expect_true("y_00a2c20d" %in% observations(xy_merged)$deploymentID)

  # mediaID
  expect_true("x_07840dcc" %in% media(xy_merged)$mediaID)
  expect_true("y_07840dcc" %in% media(xy_merged)$mediaID)
  expect_true("x_07840dcc" %in% observations(xy_merged)$mediaID)
  expect_true("y_07840dcc" %in% observations(xy_merged)$mediaID)
  expect_false("x_NA" %in% observations(xy_merged)$mediaID)
  expect_true(NA %in% observations(xy_merged)$mediaID)

  # observationID
  expect_true("x_705e6036" %in% observations(xy_merged)$observationID)
  expect_true("y_705e6036" %in% observations(xy_merged)$observationID)

  # eventID
  expect_true("x_4bb69c45" %in% media(xy_merged)$eventID)
  expect_true("y_4bb69c45" %in% media(xy_merged)$eventID)
  expect_true("x_4bb69c45" %in% observations(xy_merged)$eventID)
  expect_true("y_4bb69c45" %in% observations(xy_merged)$eventID)
})

test_that("merge_camtrapdp() returns the expected metadata ", {
  skip_if_offline()
  x <- example_dataset()
  y <- example_dataset()
  x$id <- "1"
  y$id <- "2"
  xy_merged <- merge_camtrapdp(x, y)

  # Check metadata
  expect_identical(xy_merged$resources, x$resources)
  expect_identical(xy_merged$profile, "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0.1/camtrap-dp-profile.json")
  expect_identical(xy_merged$name, NA)
  expect_identical(xy_merged$id, NA)
  expect_identical(xy_merged$title, NA)
  expect_identical(xy_merged$contributors, x$contributors)
  # no test for description
  expect_identical(xy_merged$version, "1.0")
  expect_identical(xy_merged$keywords, x$keywords)
  expect_identical(xy_merged$image, NULL)
  expect_identical(xy_merged$homepage, NULL)
  expect_identical(xy_merged$sources, x$sources)
  expect_equal(xy_merged$licenses, x$licenses) # fails because remove_duplicates switches order of subelements
  expect_identical(xy_merged$bibliographicCitation, NULL)
  expect_identical(xy_merged$projects, list(x$project, y$project))
  expect_identical(xy_merged$coordinatePrecision, x$coordinatePrecision)
  expect_identical(xy_merged$spatial, x$spatial)
  expect_identical(xy_merged$temporal, x$temporal)
  expect_identical(xy_merged$taxonomic, x$taxonomic)
  expect_identical(xy_merged$references, x$references)
  expect_identical(xy_merged$directory, x$directory)

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

  expect_identical(xy_merged$relatedIdentifiers, relatedIdentifiers_merged)

  # Check data
})

test_that("merge_camtrapdp() returns the expected metadata when merging two
          different Data Packages", {
  skip_if_offline()
  x <- example_dataset()

  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  zip_file <- file.path(temp_dir, "dataset.zip")
  datapackage_file <- file.path(temp_dir, "datapackage.json")
  url <- "https://ipt.nlbif.nl/archive.do?r=awd_pilot2"

  download.file(url, zip_file, mode = 'wb')
  unzip(zip_file, exdir = temp_dir)

  y <- read_camtrapdp(datapackage_file)

  xy_merged <- merge_camtrapdp(x, y)

  # Check metadata
  profile <- "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0.1/camtrap-dp-profile.json"
  contributors <- list(
    list(
      title = "Axel Neukermans",
      email = "axel.neukermans@inbo.be",
      path = "https://orcid.org/0000-0003-0272-9180",
      role = "contributor",
      organization = "Research Institute for Nature and Forest (INBO)"
    ),
    list(
      title = "Danny Van der beeck",
      email = "daniel.vanderbeeck@gmail.com"
    ),
    list(
      title = "Emma Cartuyvels",
      email = "emma.cartuyvels@inbo.be",
      role = "principalInvestigator",
      organization = "Research Institute for Nature and Forest (INBO)"
    ),
    list(
      title = "Peter Desmet",
      email = "peter.desmet@inbo.be",
      path = "https://orcid.org/0000-0002-8442-8025",
      role = "contact",
      organization = "Research Institute for Nature and Forest (INBO)"
    ),
    list(
      title = "Research Institute for Nature and Forest (INBO)",
      path = "https://inbo.be",
      role = "rightsHolder"
    ),
    list(
      title = "Research Institute for Nature and Forest (INBO)",
      path = "https://inbo.be",
      role = "publisher"
    ),
    list(
      title = "Julian Evans",
      email = "jevansbio@gmail.com",
      role = "principalInvestigator",
      organization = "University of Amsterdam",
      firstName = "Julian",
      lastName = "Evans"
    ),
    list(
      title = "Rotem Zilber",
      email = "r.kadanzilber@uva.nl",
      role = "principalInvestigator",
      organization = "University of Amsterdam",
      firstName = "Rotem",
      lastName = "Zilber"
    ),
    list(
      title = "W. Daniel  Kissling",
      email = "wdkissling@gmail.com",
      path = "https://www.danielkissling.de/",
      role = "principalInvestigator",
      organization = "University of Amsterdam",
      firstName = "W. Daniel ",
      lastName = "Kissling"
    )
  )

  sources <- list(
    list(
      title = "Agouti",
      path = "https://www.agouti.eu",
      email = "agouti@wur.nl",
      version = "v3.21"
    ),
    list(
      title = "Agouti",
      path = "https://www.agouti.eu",
      email = "agouti@wur.nl",
      version = "v4"
    )
  )

  licenses <- list(
    list(name = "CC0-1.0", scope = "data"),
    list(scope = "media", path = "http://creativecommons.org/licenses/by/4.0/"),
    list(name = "CC-BY-4.0", scope = "data")
  )

  coordinatePrecision <- 0.001

  spatial <- list(
    type = "Polygon",
    coordinates = structure(
      c(
        4.013, 5.659, 5.659, 4.013, 4.013,
        50.699, 50.699, 52.35604, 52.35604, 50.699
      ),
      dim = c(1L, 5L, 2L)
    )
  )

  temporal <- list(start = "2020-05-30", end = "2022-03-18")

  references <- list("Evans, J.C., Zilber, R., & Kissling, W.D. (2024). Data from three camera trapping pilots in the Amsterdam Water Supply Dunes of the Netherlands. Data in Brief, 54, 110544. https://doi.org/10.1016/j.dib.2024.110544")

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
      relationType = "IsPublishedIn",
      relatedIdentifier = "https://doi.org/10.1016/j.dib.2024.110544",
      resourceTypeGeneral = "DataPaper",
      relatedIdentifierType = "DOI"
    ),
    list(
      relationType = "IsDerivedFrom",
      relatedIdentifier = "7cca70f5-ef8c-4f86-85fb-8f070937d7ab",
      resourceTypeGeneral = "Data package",
      relatedIdentifierType = "id"
    )
  )

  expect_identical(xy_merged$resources, x$resources)
  expect_identical(xy_merged$profile, profile)
  expect_identical(xy_merged$name, NA)
  expect_identical(xy_merged$id, NA)
  expect_identical(xy_merged$title, NA)
  expect_identical(xy_merged$contributors, contributors)
  # no test for description
  expect_identical(xy_merged$version, "1.0")
  expect_identical(xy_merged$keywords, c(x$keywords, y$keywords))
  expect_identical(xy_merged$image, NULL)
  expect_identical(xy_merged$homepage, NULL)
  expect_identical(xy_merged$sources, sources)
  expect_identical(xy_merged$licenses, licenses)
  expect_identical(xy_merged$bibliographicCitation, NULL)
  expect_identical(xy_merged$projects, list(x$project, y$project))
  expect_identical(xy_merged$coordinatePrecision, coordinatePrecision)
  expect_identical(xy_merged$spatial, spatial)
  expect_identical(xy_merged$temporal, temporal)
  expect_identical(xy_merged$taxonomic, taxonomic)
  expect_identical(xy_merged$references, references)
  # expect_identical(xy_merged$directory, directory)
  expect_identical(xy_merged$relatedIdentifiers, relatedIdentifiers_merged)
})
