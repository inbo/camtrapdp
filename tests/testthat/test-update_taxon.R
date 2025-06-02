test_that("update_taxon() returns a valid camtrapdp object", {
  skip_if_offline()
  x <- example_dataset()
  expect_no_error(
    check_camtrapdp(
      suppressMessages(
        update_taxon(
          x,
          from = "Anas platyrhynchos",
          to = list(
            scientificName = "Anas",
            taxonID = "https://www.checklistbank.org/dataset/9910/taxon/V8R",
            taxonRank = "genus",
            vernacularNames.eng = "dabbling ducks"
          )
        )
      )
    )
  )
})

test_that("update_taxon() warns if provided 'from' cannot be found as a value in
          scientificName", {
  skip_if_offline()
  x <- example_dataset()
  to <- list(scientificName = "Anemone")

  expect_warning(
    update_taxon(x, "not a species name", to),
    class = "camtrapdp_warning_taxon_not_found"
    )
  expect_warning(
    update_taxon(x, "Anemone nemorosa", to),
    class = "camtrapdp_warning_taxon_not_found"
    )
})

test_that("update_taxon() returns error if 'to' is not a named list or missing
           a scientificName", {
  skip_if_offline()
  x <- example_dataset()
  from <- "Anas platyrhynchos"

  # Not a list
  expect_error(
    update_taxon(x, from, "not_a_list"),
    class = "camtrapdp_error_to_invalid"
  )
  expect_error(
    update_taxon(x, from, NULL),
    class = "camtrapdp_error_to_invalid"
  )

  # Unnamed properties
  expect_error(
    update_taxon(x, from, list()),
    class = "camtrapdp_error_to_properties_invalid"
  )
  expect_error(
    update_taxon(x, from, list(scientificName = "Anas", "unnamed_property")),
    class = "camtrapdp_error_to_properties_invalid"
  )

  # No or invalid scientificName
  expect_error(
    update_taxon(x, from, list(taxonRank = "species")),
    class = "camtrapdp_error_to_scientificname_invalid"
  )
  expect_error(
    update_taxon(x, from, list(scientificName = 2)),
    class = "camtrapdp_error_to_scientificname_invalid"
  )
})

test_that("update_taxon() returns message", {
  skip_if_offline()
  x <- example_dataset()
  from <- "Anas platyrhynchos"
  to <- list(
    scientificName = "Anas",
    taxonRank = "genus",
    vernacularNames.fr = "canard colvert"
  )

  suppressMessages(expect_message(
    update_taxon(x, from, to),
    regexp = "Taxon \"Anas platyrhynchos\" is replaced by:",
    fixed = TRUE
  ))
  suppressMessages(expect_message(
    update_taxon(x, from, to),
    regexp = "scientificName: Anas",
    fixed = TRUE
  ))
  suppressMessages(expect_message(
    update_taxon(x, from, to),
    regexp = "taxonID: NA",
    fixed = TRUE
  ))
  # Further cli_dl rows not tested
})

test_that("update_taxon() can update a taxon in data and metadata", {
  skip_if_offline()
  x <- example_dataset()
  x_updated <- suppressMessages(update_taxon(
    x,
    from = "Ardea cinerea",
    to = list(
      scientificName = "Ardea cinerea",
      taxonID = "updated_taxonID",
      vernacularNames.fra = "updated_vernacularNames.fra"
    )
  ))

  # Metadata
  expect_identical(
    purrr::detect(x_updated$taxonomic, ~ .x$scientificName == "Ardea cinerea"),
    list(
      scientificName = "Ardea cinerea",
      taxonID = "updated_taxonID",
      taxonRank = NA_character_,
      vernacularNames = list(
        eng = NA_character_,
        nld = NA_character_,
        fra = "updated_vernacularNames.fra"
      )
    )
  )

  # Data
  expect_identical(
    dplyr::filter(taxa(x_updated), scientificName == "Ardea cinerea"),
    tibble::tibble_row(
      scientificName = "Ardea cinerea",
      taxonID = "updated_taxonID",
      taxonRank = NA_character_,
      vernacularNames.eng = NA_character_,
      vernacularNames.nld = NA_character_,
      vernacularNames.fra = "updated_vernacularNames.fra"
    )
  )
})

test_that("update_taxon() can replace a taxon in data and metadata", {
  skip_if_offline()
  x <- example_dataset()
  x_updated <- suppressMessages(update_taxon(
    x,
    from = "Ardea cinerea",
    to = list(
      scientificName = "replaced",
      taxonID = "replaced_taxonID",
      vernacularNames.fra = "replaced_vernacularNames.fra"
    )
  ))

  # Metadata
  expect_null(
    purrr::detect(x_updated$taxonomic, ~ .x$scientificName == "Ardea cinerea")
  )
  expect_identical(
    purrr::detect(x_updated$taxonomic, ~ .x$scientificName == "replaced"),
    list(
      scientificName = "replaced",
      taxonID = "replaced_taxonID",
      taxonRank = NA_character_,
      vernacularNames = list(
        eng = NA_character_,
        nld = NA_character_,
        fra = "replaced_vernacularNames.fra"
      )
    )
  )

  # Data
  expect_equal(
    nrow(dplyr::filter(taxa(x_updated), scientificName == "Ardea cinerea")),
    0
  )
  expect_identical(
    dplyr::filter(taxa(x_updated), scientificName == "replaced"),
    tibble::tibble_row(
      scientificName = "replaced",
      taxonID = "replaced_taxonID",
      taxonRank = NA_character_,
      vernacularNames.eng = NA_character_,
      vernacularNames.nld = NA_character_,
      vernacularNames.fra = "replaced_vernacularNames.fra"
    )
  )
})

test_that("update_taxon() can lump a taxon in data and metadata", {
  skip_if_offline()
  x <- example_dataset()
  x_updated <- suppressMessages(update_taxon(
    x,
    from = "Ardea cinerea",
    to = list(
      scientificName = "Ardea",
      taxonID = "lumped_taxonID",
      vernacularNames.fra = "lumped_vernacularNames.fra"
    )
  ))

  # Metadata
  expect_null(
    purrr::detect(x_updated$taxonomic, ~ .x$scientificName == "Ardea cinerea")
  )
  expect_identical(
    purrr::detect(x_updated$taxonomic, ~ .x$scientificName == "Ardea"),
    list(
      scientificName = "Ardea",
      taxonID = "lumped_taxonID",
      taxonRank = NA_character_,
      vernacularNames = list(
        eng = NA_character_,
        nld = NA_character_,
        fra = "lumped_vernacularNames.fra"
      )
    )
  )

  # Data
  expect_equal(
    nrow(dplyr::filter(taxa(x_updated), scientificName == "Ardea cinerea")),
    0
  )
  expect_identical(
    dplyr::filter(taxa(x_updated), scientificName == "Ardea"),
    tibble::tibble_row(
      scientificName = "Ardea",
      taxonID = "lumped_taxonID",
      taxonRank = NA_character_,
      vernacularNames.eng = NA_character_,
      vernacularNames.nld = NA_character_,
      vernacularNames.fra = "lumped_vernacularNames.fra"
    )
  )
})
