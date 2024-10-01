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

  # Default prefixes
  x_merged_default <- merge_camtrapdp(x1, x2)
  expect_true("100a2c20d" %in% deployments(x_merged)$deploymentID)
  expect_true("200a2c20d" %in% deployments(x_merged)$deploymentID)

  # Custom prefixes
  x_merged <- merge_camtrapdp(x1, x2, prefix = c("project1-", "project2-"))

  # deploymentID
  expect_true("project1-00a2c20d" %in% deployments(x_merged)$deploymentID)
  expect_true("project2-00a2c20d" %in% deployments(x_merged)$deploymentID)
  expect_true("project1-00a2c20d" %in% media(x_merged)$deploymentID)
  expect_true("project1-00a2c20d" %in% observations(x_merged)$deploymentID)

  # mediaID
  expect_true("project1-07840dcc" %in% media(x_merged)$mediaID)
  expect_true("project1-07840dcc" %in% observations(x_merged)$mediaID)
  expect_false("project1-NA" %in% observations(x_merged)$mediaID)
  expect_true(NA %in% observations(x_merged)$mediaID)

  # observationID
  expect_true("project1-705e6036" %in% observations(x_merged)$observationID)

  # eventID
  expect_true("project1-4bb69c45" %in% media(x_merged)$eventID)
  expect_true("project1-4bb69c45" %in% observations(x_merged)$eventID)
})

test_that("merge_camtrapdp() returns the expected metadata", {
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

test_that("merge_camtrapdp() returns the expected metadata when merging two
          different Data Packages", {
  skip_if_offline()
  x1 <- example_dataset()

  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  zip_file <- file.path(temp_dir, "dataset.zip")
  datapackage_file <- file.path(temp_dir, "datapackage.json")
  url <- "https://ipt.nlbif.nl/archive.do?r=awd_pilot2"

  download.file(url, zip_file, mode = 'wb')
  unzip(zip_file, exdir = temp_dir)

  x2 <- read_camtrapdp(datapackage_file)

  x_merged <- merge_camtrapdp(x1, x2)

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

  expect_identical(x_merged$resources, x1$resources)
  expect_identical(x_merged$profile, profile)
  expect_identical(x_merged$name, NA)
  expect_identical(x_merged$id, NA)
  expect_identical(x_merged$title, NA)
  expect_identical(x_merged$contributors, contributors)
  # no test for description
  expect_identical(x_merged$version, "1.0")
  expect_identical(x_merged$keywords, c(x1$keywords, x2$keywords))
  expect_identical(x_merged$image, NULL)
  expect_identical(x_merged$homepage, NULL)
  expect_identical(x_merged$sources, sources)
  expect_equal(x_merged$licenses, licenses)
  expect_identical(x_merged$bibliographicCitation, NULL)
  expect_identical(x_merged$projects, list(x1$project, x2$project))
  expect_identical(x_merged$coordinatePrecision, coordinatePrecision)
  expect_identical(x_merged$spatial, spatial)
  expect_identical(x_merged$temporal, temporal)
  expect_identical(x_merged$taxonomic, taxonomic)
  expect_identical(x_merged$references, references)
  # expect_identical(x_merged$directory, directory)
  expect_identical(x_merged$relatedIdentifiers, relatedIdentifiers_merged)
})
