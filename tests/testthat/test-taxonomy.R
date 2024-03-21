library(tibble)
library(dplyr)

test_that("taxonomy returns a tibble data.frame", {
  skip_if_offline()
  dataset <- example_dataset()
  taxa <- taxonomy(dataset)
  expect_true(is.data.frame(taxa))
  expect_true(is_tibble(taxa))
})

test_that("taxonomy returns the columns scientificName and with taxon prefix", {
  skip_if_offline()
  dataset <- example_dataset()
  taxa <- taxonomy(dataset)
  expect_true("scientificName" %in% names(taxa))
  expect_true(
    all(
      grepl(pattern = "^taxon\\.",
              x = names(taxa %>% dplyr::select(-scientificName))
            )
    )
  )
})

test_that(
  "taxonomy returns scientificName column only if no taxon info in observations", {
  skip_if_offline()
  dataset <- example_dataset()
  dataset$data$observations <- dataset$data$observations %>%
    select(-starts_with("taxon."))
  expect_identical(
    taxonomy(dataset),
    observations(dataset) %>%
      distinct(scientificName) %>%
      filter(!is.na(scientificName))
  )
})

test_that("taxonomy returns the expected amount of rows", {
  skip_if_offline()
  dataset <- example_dataset()
  taxa <- taxonomy(dataset)
  expect_identical(nrow(taxa),10L)
})

test_that("taxonomy never returns more rows than observations", {
  skip_if_offline()
  dataset <- example_dataset()
  obs <- observations(dataset)
  taxa <- taxonomy(dataset)
  expect_true(nrow(taxa) <= nrow(obs))
})

test_that(
  "one row returned if all observations have same taxonomy information", {
    skip_if_offline()
    dataset <- example_dataset()
    dataset$data$observations$scientificName <- "Vulpes vulpes"
    dataset$data$observations$taxon.taxonRank <- "species"
    dataset$data$observations$taxon.taxonID <- "https://www.checklistbank.org/dataset/COL2023/taxon/5BSG3"
    dataset$data$observations$taxon.vernacularNames.eng <- "red fox"
    dataset$data$observations$taxon.vernacularNames.nld <- "vos"
    taxa <- taxonomy(dataset)
    expect_equal(nrow(taxa), 1)
  })
