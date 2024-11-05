test_that("merge_camtrapdp() returns a valid camtrapdp object", {
  skip_if_offline()
  x <- example_dataset()
  y <- x
  x$name <- "x"
  y$name <- "y"

  expect_no_error(check_camtrapdp(merge_camtrapdp(x, y)))
})

test_that("merge_camtrapdp() returns error on duplicate or missing dataset
           name", {
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

test_that("merge_camtrapdp() adds prefixes to identifiers to keep them unique", {
  skip_if_offline()
  x <- example_dataset() %>%
    filter_deployments(deploymentID %in% c("00a2c20d", "29b7d356"))
  y <- example_dataset() %>%
    filter_deployments(deploymentID %in% c("29b7d356", "577b543a"))
  x$name <- "x"
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

test_that("merge_camtrapdp() adds prefixes to additional resources to keep them
           unique", {
  skip_if_offline()
  x <- example_dataset()
  y <- x
  y <- frictionless::add_resource(y, "iris", iris)
  x$name <- "x"
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

test_that("merge_camtrapdp() returns the expected metadata ", {
  skip_if_offline()
  x <- example_dataset()
  y <- example_dataset()
  x$name <- "x"
  y$name <- "y"
  xy <- merge_camtrapdp(x, y)

  # Can't compare with x$licenses because remove_duplicates switches order of
  # subelements
  licenses <- list(
    list(name = "CC0-1.0", scope = "data"),
    list(scope = "media", path = "http://creativecommons.org/licenses/by/4.0/"))

  # Check metadata
  expect_equal(length(xy$resources), 5)
  expect_identical(xy$profile, "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0.1/camtrap-dp-profile.json")
  expect_identical(xy$name, NA)
  expect_identical(xy$id, NULL)
  expect_identical(xy$title, NA)
  expect_identical(xy$contributors, x$contributors)
  expect_identical(
    xy$description,
    paste(x$description, y$description, sep = "/n")
  )
  expect_identical(xy$version, "1.0")
  expect_identical(xy$keywords, x$keywords)
  expect_identical(xy$image, NULL)
  expect_identical(xy$homepage, NULL)
  expect_identical(xy$sources, x$sources)
  expect_identical(xy$licenses, licenses)
  expect_identical(xy$bibliographicCitation, NULL)
  expect_identical(xy$project, list(x$project, y$project))
  expect_identical(xy$coordinatePrecision, x$coordinatePrecision)
  expect_identical(xy$spatial, x$spatial)
  expect_identical(xy$temporal, x$temporal)
  expect_identical(xy$taxonomic, x$taxonomic)
  expect_identical(xy$references, x$references)
  expect_identical(xy$directory, ".")

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

  expect_identical(xy$relatedIdentifiers, relatedIdentifiers_merged)

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
  y$name <- "y"

  # Merge
  xy <- merge_camtrapdp(x, y)

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

  expect_identical(xy$resources, x$resources)
  expect_identical(xy$profile, profile)
  expect_identical(xy$name, NA)
  expect_identical(xy$id, NULL)
  expect_identical(xy$title, NA)
  expect_identical(xy$contributors, contributors)
  expect_identical(
    xy$description,
    paste(x$description, y$description, sep = "/n")
    )
  expect_identical(xy$version, "1.0")
  expect_identical(xy$keywords, c(x$keywords, y$keywords))
  expect_identical(xy$image, NULL)
  expect_identical(xy$homepage, NULL)
  expect_identical(xy$sources, sources)
  expect_identical(xy$licenses, licenses)
  expect_identical(xy$bibliographicCitation, NULL)
  expect_identical(xy$project, list(x$project, y$project))
  expect_identical(xy$coordinatePrecision, coordinatePrecision)
  expect_identical(xy$spatial, spatial)
  expect_identical(xy$temporal, temporal)
  expect_identical(xy$taxonomic, taxonomic)
  expect_identical(xy$references, references)
  expect_identical(xy$directory, ".")
  expect_identical(xy$relatedIdentifiers, relatedIdentifiers_merged)
})
