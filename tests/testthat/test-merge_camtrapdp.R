test_that("merge_camtrapdp() returns a valid camtrapdp object", {
  skip_if_offline()
  x <- example_dataset()
  y <- x
  x$name <- "x"
  y$name <- "y"

  expect_no_error(check_camtrapdp(merge_camtrapdp(x, y)))
})

test_that("merge_camtrapdp() returns error on duplicate or missing package name", {
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

test_that("merge_camtrapdp() returns unique deploymentIDs, mediaIDs and
          observationIDs", {
  skip_if_offline()
  x <- example_dataset() %>%
    filter_deployments(deploymentID %in% c("00a2c20d", "29b7d356"))
  y <- example_dataset() %>%
    filter_deployments(deploymentID %in% c("577b543a", "62c200a9"))
  x$name <- "x"
  y$name <- "y"
  xy_merged <- merge_camtrapdp(x, y)

  deploymentIDs <- purrr::pluck(deployments(xy_merged), "deploymentID")
  mediaIDs <- purrr::pluck(media(xy_merged), "mediaID")
  observationIDs <- purrr::pluck(observations(xy_merged), "observationID")

  expect_false(any(duplicated(deploymentIDs)))
  expect_false(any(duplicated(mediaIDs)))
  expect_false(any(duplicated(observationIDs)))

  expect_true("00a2c20d" %in% deployments(xy_merged)$deploymentID)
  expect_true("577b543a" %in% deployments(xy_merged)$deploymentID)
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

test_that("merge_camtrapdp() adds default prefixes to the names of
          additional resources that are not unique and not required by Camera
          Trap Data Package standard", {
  skip_if_offline()
  x <- example_dataset()
  y <- example_dataset()
  x$id <- "1"
  y$id <- "2"
  x$resources <- append(
    y$resources,
    list(list(
      name = "annotations",
      data = list(id = 1L, comment = "albino fox"))
      ))
  y$resources <- append(
    y$resources,
    list(list(name = "foo", description = "blabla")))
  xy_merged <- merge_camtrapdp(x, y)

  resource_names <- purrr::map(xy_merged$resources, ~ .[["name"]]) %>% unlist()
  expected_names <- c(
    "deployments", "media", "observations", "1_individuals", "annotations",
    "2_individuals", "foo")

  expect_identical(resource_names, expected_names)
})

test_that("merge_camtrapdp() returns the expected metadata ", {
  skip_if_offline()
  x <- example_dataset()
  y <- example_dataset()
  x$id <- "1"
  y$id <- "2"
  xy_merged <- merge_camtrapdp(x, y)

  # Can't compare with x$licenses because remove_duplicates switches order of
  # subelements
  licenses <- list(
    list(name = "CC0-1.0", scope = "data"),
    list(scope = "media", path = "http://creativecommons.org/licenses/by/4.0/"))

  # Check metadata
  expect_equal(length(xy_merged$resources), 5)
  expect_identical(xy_merged$profile, "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0.1/camtrap-dp-profile.json")
  expect_identical(xy_merged$name, NA)
  expect_identical(xy_merged$id, NULL)
  expect_identical(xy_merged$title, NA)
  expect_identical(xy_merged$contributors, x$contributors)
  expect_identical(
    xy_merged$description,
    paste(x$description, y$description, sep = "/n")
  )
  expect_identical(xy_merged$version, "1.0")
  expect_identical(xy_merged$keywords, x$keywords)
  expect_identical(xy_merged$image, NULL)
  expect_identical(xy_merged$homepage, NULL)
  expect_identical(xy_merged$sources, x$sources)
  expect_identical(xy_merged$licenses, licenses)
  expect_identical(xy_merged$bibliographicCitation, NULL)
  expect_identical(xy_merged$project, list(x$project, y$project))
  expect_identical(xy_merged$coordinatePrecision, x$coordinatePrecision)
  expect_identical(xy_merged$spatial, x$spatial)
  expect_identical(xy_merged$temporal, x$temporal)
  expect_identical(xy_merged$taxonomic, x$taxonomic)
  expect_identical(xy_merged$references, x$references)
  expect_identical(xy_merged$directory, ".")

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

  # Download second Camera Trap Data package
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  zip_file <- file.path(temp_dir, "dataset.zip")
  datapackage_file <- file.path(temp_dir, "datapackage.json")
  url <- "https://ipt.nlbif.nl/archive.do?r=awd_pilot2"
  download.file(url, zip_file, mode = 'wb', quiet = TRUE)
  unzip(zip_file, exdir = temp_dir)
  y <- read_camtrapdp(datapackage_file)
  y$id <- "y"

  # Merge
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

  description <- "MICA - Muskrat and coypu camera trap observations in Belgium, the Netherlands and Germany is an occurrence dataset published by the Research Institute of Nature and Forest (INBO). It is part of the LIFE project MICA, in which innovative techniques are tested for a more efficient control of muskrat and coypu populations, both invasive species. This dataset is a sample of the original dataset and serves as an example of a Camera Trap Data Package (Camtrap DP)./nCamera trap pilot 2 was a test of the difference in species detection and data accumulation between a Snyper Commander camera with a regular lens (52°) and one with a wide lens (100°). The cameras were deployed at 30 cm above the ground within the herbivore exclosure Zeeveld Noord in the Amsterdam Water Supply Dunes from 14th of August 2021 to 24th of September 2021. During this pilot, a solar panel failure caused the cameras to stop recording data from the 24th of August 2021 to the 6th of September (14 days). During annotation, only days in which both cameras were operational were annotated. This led to a total of 1,113 images over 28 days from the two cameras. A detailed description of the dataset can be found in a data paper published in the journal Data in Brief (Evans et al. 2024, https://doi.org/10.1016/j.dib.2024.110544)."

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

  taxonomic <- list(
    list(
      scientificName = "Anas platyrhynchos",
      taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/DGP6",
      taxonRank = "species",
      family = NA_character_,
      order. = NA_character_,
      vernacularNames = list(eng = "mallard", nld = "wilde eend")
    ),
    list(
      scientificName = "Anas strepera",
      taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/DGPL",
      taxonRank = "species",
      family = NA_character_,
      order. = NA_character_,
      vernacularNames = list(eng = "gadwall", nld = "krakeend")
    ),
    list(
      scientificName = "Apodemus sylvaticus",
      taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/FRJJ",
      taxonRank = "species",
      family = "Muridae",
      order. = "Rodentia",
      vernacularNames = list(eng = "wood mouse", nld = "bosmuis")
    ),
    list(
      scientificName = "Ardea",
      taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/32FH",
      taxonRank = "genus",
      family = NA_character_,
      order. = NA_character_,
      vernacularNames = list(eng = "great herons", nld = "reigers")
    ),
    list(
      scientificName = "Ardea cinerea",
      taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/GCHS",
      taxonRank = "species",
      family = NA_character_,
      order. = NA_character_,
      vernacularNames = list(eng = "grey heron", nld = "blauwe reiger")
    ),
    list(
      scientificName = "Aves",
      taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/V2",
      taxonRank = "class",
      family = NA_character_,
      order. = NA_character_,
      vernacularNames = list(eng = "bird sp.", nld = "vogel")
    ),
    list(
      scientificName = "Corvus corone",
      taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/YNHJ",
      taxonRank = "species",
      family = "Corvidae",
      order. = "Passeriformes",
      vernacularNames = list(eng = "carrion crow", nld = "zwarte kraai")
    ),
    list(
      scientificName = "Homo sapiens",
      taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/6MB3T",
      taxonRank = "species",
      family = "Hominidae",
      order. = "Primates",
      vernacularNames = list(eng = "human", nld = "mens")
    ),
    list(
      scientificName = "Martes foina",
      taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/3Y9VW",
      taxonRank = "species",
      family = NA_character_,
      order. = NA_character_,
      vernacularNames = list(eng = "beech marten", nld = "steenmarter")
    ),
    list(
      scientificName = "Mustela putorius",
      taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/44QYC",
      taxonRank = "species",
      family = NA_character_,
      order. = NA_character_,
      vernacularNames = list(eng = "European polecat", nld = "bunzing")
    ),
    list(
      scientificName = "Oryctolagus cuniculus",
      taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/74ZBP",
      taxonRank = "species",
      family = "Leporidae",
      order. = "Lagomorpha",
      vernacularNames = list(eng = "European rabbit", nld = "Europees konijn")
    ),
    list(
      scientificName = "Rattus norvegicus",
      taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/4RM67",
      taxonRank = "species",
      family = NA_character_,
      order. = NA_character_,
      vernacularNames = list(eng = "brown rat", nld = "bruine rat")
    ),
    list(
      scientificName = "Vulpes vulpes",
      taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/5BSG3",
      taxonRank = "species",
      family = "Canidae",
      order. = "Carnivora",
      vernacularNames = list(eng = "red fox", nld = "vos")
    )
  )

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
    ),
    list(
      relationType = "IsDerivedFrom",
      relatedIdentifier = "y",
      resourceTypeGeneral = "Data package",
      relatedIdentifierType = "id"
    )
  )

  expect_identical(xy_merged$resources, x$resources)
  expect_identical(xy_merged$profile, profile)
  expect_identical(xy_merged$name, NA)
  expect_identical(xy_merged$id, NULL)
  expect_identical(xy_merged$title, NA)
  expect_identical(xy_merged$contributors, contributors)
  expect_identical(
    xy_merged$description,
    paste(x$description, y$description, sep = "/n")
    )
  expect_identical(xy_merged$version, "1.0")
  expect_identical(xy_merged$keywords, c(x$keywords, y$keywords))
  expect_identical(xy_merged$image, NULL)
  expect_identical(xy_merged$homepage, NULL)
  expect_identical(xy_merged$sources, sources)
  expect_identical(xy_merged$licenses, licenses)
  expect_identical(xy_merged$bibliographicCitation, NULL)
  expect_identical(xy_merged$project, list(x$project, y$project))
  expect_identical(xy_merged$coordinatePrecision, coordinatePrecision)
  expect_identical(xy_merged$spatial, spatial)
  expect_identical(xy_merged$temporal, temporal)
  expect_identical(xy_merged$taxonomic, taxonomic)
  expect_identical(xy_merged$references, references)
  expect_identical(xy_merged$directory, ".")
  expect_identical(xy_merged$relatedIdentifiers, relatedIdentifiers_merged)
})
