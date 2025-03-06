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

test_that("update_taxon() returns error if 'to' is not a named list", {
  skip_if_offline()
  x <- example_dataset()
  from <- "Anas platyrhynchos"
  string <- "Anas"
  df <- data.frame(scientificName = "Anas")
  list_not_named <- list(
    scientificName = "Anas",
    "https://www.checklistbank.org/dataset/9910/taxon/V8R"
    )

  expect_error(
    update_taxon(x, from, string),
    class = "camtrapdp_error_class_invalid"
    )
  expect_error(
    update_taxon(x, from, df),
    class = "camtrapdp_error_class_invalid"
  )
  expect_error(
    update_taxon(x, from, list_not_named),
    class = "camtrapdp_error_list_not_named"
  )
})

test_that("update_taxon() returns error if scientificName is missing from 'to'", {
  skip_if_offline()
  x <- example_dataset()
  from <- "Anas platyrhynchos"
  to <- list(
    taxonID = "https://www.checklistbank.org/dataset/9910/taxon/V8R",
    taxonRank = "genus",
    vernacularNames.eng = "dabbling ducks"
  )
  expect_error(
    update_taxon(x, from, to),
    class = "camtrapdp_error_scientificname_missing"
    )
})

test_that("shift_time() returns a valid camtrapdp object", {
  skip_if_offline()
  x <- example_dataset()
  from <- "Anas platyrhynchos"
  to <- list(
    scientificName = "Anas",
    taxonID = "https://www.checklistbank.org/dataset/9910/taxon/V8R",
    taxonRank = "genus",
    vernacularNames.eng = "dabbling ducks"
  )

  expect_no_error(
    check_camtrapdp(
      suppressMessages(update_taxon(x, from, to))
    )
  )
})

test_that("update_taxon() updates scientificName in observations", {
  skip_if_offline()

  x <- example_dataset()

  # Test 1: new taxon
  from <- "Anas platyrhynchos"
  to <- list(
    scientificName = "Anas",
    taxonID = "https://www.checklistbank.org/dataset/9910/taxon/V8R",
    taxonRank = "genus",
    vernacularNames.eng = "dabbling ducks"
  )

  x_updated_new <- suppressMessages(update_taxon(x, from, to))

  expected_scientificNames_new <- c(
    "Anas", NA, "Rattus norvegicus", "Ardea cinerea",
    "Homo sapiens", "Anas strepera", "Aves", "Mustela putorius",
    "Vulpes vulpes", "Martes foina", "Ardea"
  )
  expect_identical(
    unique(observations(x_updated_new)$scientificName),
    expected_scientificNames_new

  )

  # Test 2: present taxon
  from <- "Ardea cinerea"
  to <- list(
    scientificName = "Ardea",
    comment = "taxon updated from Ardea cinerea"
  )

  x_updated_present <- suppressMessages(update_taxon(x, from, to))

  expected_scientificNames_present <- c(
    "Anas platyrhynchos", NA, "Rattus norvegicus", "Ardea",
    "Homo sapiens", "Anas strepera", "Aves", "Mustela putorius",
    "Vulpes vulpes", "Martes foina"
  )
  expect_identical(
    unique(observations(x_updated_present)$scientificName),
    expected_scientificNames_present

  )

})

test_that("update_taxon() updates taxonomy in data", {
  skip_if_offline()
  x <- example_dataset()
  taxa_original <- taxa(x)
  from <- "Anas platyrhynchos"
  to <- list(
    scientificName = "Anas",
    taxonID = "https://www.checklistbank.org/dataset/9910/taxon/V8R",
    taxonRank = "genus",
    vernacularNames.fr = "canards et sarcelles"
  )
  expected_taxa <- taxa(x) %>%
    dplyr::filter(scientificName != "Anas platyrhynchos") %>%
    dplyr::mutate(vernacularNames.fr = NA) %>%
    rbind(
      data.frame(
        scientificName = "Anas",
        taxonID = "https://www.checklistbank.org/dataset/9910/taxon/V8R",
        taxonRank = "genus",
        vernacularNames.eng = NA,
        vernacularNames.nld = NA,
        vernacularNames.fr = "canards et sarcelles"
      )
    ) %>%
    dplyr::arrange(.data$scientificName)

  x_updated <- suppressMessages(update_taxon(x, from, to))
  taxa_new <- taxa(x_updated)

  expect_identical(
    taxa_new,
    expected_taxa
  )
})

test_that("update_taxon() returns message", {
  skip_if_offline()
  x <- example_dataset()
  taxa_original <- taxa(x)
  from <- "Anas platyrhynchos"
  to <- list(
    scientificName = "Anas",
    taxonRank = "genus"
  )

  expect_message(
    update_taxon(x, from, to),
    class = "camtrapdp_message_update_taxon"
  )

  # Test fails
  # expect_message(
  #   update_taxon(x, from, to),
  #   regexp = "Taxon \"Anas platyrhynchos\" is replaced by Anas and genus.",
  #   fixed = TRUE
  # )
})
