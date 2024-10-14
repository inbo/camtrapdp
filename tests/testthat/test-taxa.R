test_that("taxa() returns a tibble", {
  skip_if_offline()
  x <- example_dataset()
  expect_s3_class(taxa(x), "tbl")
})

test_that("taxa() returns the expected columns, without .taxon prefix", {
  skip_if_offline()
  x <- example_dataset()
  expected_cols <- c(
    "scientificName",
    "taxonID",
    "taxonRank",
    "vernacularNames.eng",
    "vernacularNames.nld"
  )
  expect_named(taxa(x), expected_cols)
})

test_that("taxa() returns the expected rows (unique taxa)", {
  skip_if_offline()
  x <- example_dataset()
  expect_equal(nrow(taxa(x)), 10)
  # Less or equal than observations
  expect_lte(
    nrow(taxa(x)),
    nrow(observations(x))
  )
})

test_that("taxa() removes duplicates and keeps the rows with most taxnomical
          information", {
  skip_if_offline()
  x <- example_dataset()
  x_without_taxonID <- x
  obs <- data.frame(
    observationID = c(1, 2, 3, 4, 5),
    scientificName = c(
      "Anas platyrhynchos",
      "Anas platyrhynchos",
      "Anas strepera",
      "Anas strepera",
      "Homo sapiens"
    ),
    taxon.taxonID = c(
      "https://www.checklistbank.org/dataset/COL2023/taxon/DGP6",
      "https://www.checklistbank.org/dataset/COL2023/taxon/DGP6",
      "https://www.checklistbank.org/dataset/COL2023/taxon/DGPL",
      "https://www.checklistbank.org/dataset/COL2023/taxon/DGPL",
      "https://www.checklistbank.org/dataset/COL2023/taxon/6MB3T"
    ),
    taxon.taxonRank = c("species", "species", "species", "species", "species"),
    taxon.vernacularNames.eng = c(NA, "mallard", "gadwall", "gadwall", "human"),
    taxon.vernacularNames.nld = c(
      "wilde eend", NA, "krakeend", "krakeend", "mens"
    ),
    taxon.family = c(NA, NA, NA, "Anatidae", NA)
  )
  observations(x) <- obs
  observations(x_without_taxonID) <- obs %>% dplyr::select(-taxon.taxonID)

  expected_taxa <- dplyr::tibble(
    scientificName = c("Anas platyrhynchos", "Anas strepera", "Homo sapiens"),
    taxonID = c(
    "https://www.checklistbank.org/dataset/COL2023/taxon/DGP6",
    "https://www.checklistbank.org/dataset/COL2023/taxon/DGPL",
    "https://www.checklistbank.org/dataset/COL2023/taxon/6MB3T"
  ),
  taxonRank = c("species", "species", "species"),
  vernacularNames.eng = c("mallard", "gadwall", "human"),
  vernacularNames.nld = c(NA, "krakeend", "mens"),
  family = c(NA, "Anatidae", NA)
  )

  expect_identical(taxa(x), expected_taxa)
  expect_identical(
    taxa(x_without_taxonID),
    dplyr::select(expected_taxa, -taxonID)
  )
})

test_that("taxa() warns on duplicates", {
  skip_if_offline()
  x <- example_dataset()
  observations(x) <- observations(x) %>%
    dplyr::mutate(
      taxon.vernacularNames.nld =
        dplyr::if_else(
        observationID == "07840dcc_1", NA, .data$taxon.vernacularNames.nld
        )
    )

  expect_warning(taxa(x))
})

