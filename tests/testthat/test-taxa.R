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

test_that("taxa() removes duplicates and keeps the rows with most taxonomical
          information", {
  skip_if_offline()
  x <- example_dataset()

  # Duplicate sci names: keep first record with taxonID
  observations(x) <- data.frame(
    observationID = c(
      "delete:no_taxonID",
      "keep",
      "delete:exact_duplicate",
      "delete:not_first",
      "delete:not_first",
      "keep:diff_name",
      "delete:not_first"
    ),
    scientificName = c(
      "Anas platyrhynchos",
      "Anas platyrhynchos",
      "Anas platyrhynchos",
      "Anas platyrhynchos",
      "Anas platyrhynchos",
      "Anas strepera",
      "Anas strepera"
    ),
    taxon.taxonID = c(
      NA_character_,
      "DGP6",
      "DGP6",
      "DGP6_1",
      "DGP6_2",
      "DGPL",
      "DGPL_1"
    )
  )
  expected_taxa <- dplyr::tibble(
    scientificName = c("Anas platyrhynchos", "Anas strepera"),
    taxonID = c("DGP6", "DGPL")
  )
  expect_identical(taxa(x), expected_taxa)

  # Duplicate with different amount of info: keep with most
  observations(x) <- data.frame(
    observationID = c(
      "delete:1_columns",
      "delete:4_columns",
      "keep:5_columns"
    ),
    scientificName = c(
      "Anas platyrhynchos",
      "Anas platyrhynchos",
      "Anas platyrhynchos"
    ),
    taxon.taxonRank = c(
      NA_character_,
      "species",
      "species"
    ),
    taxon.class = c(
      NA_character_,
      "Animalia",
      NA_character_
    ),
    taxon.family = c(
      NA_character_,
      NA_character_,
      "Anatidae"
    ),
    taxon.vernacularNames.eng = c(
      NA_character_,
      "mallard",
      "mallard"
    ),
    taxon.vernacularNames.fra = c(
      NA_character_,
      NA_character_,
      NA_character_
    ),
    taxon.vernacularNames.nld = c(
      NA_character_,
      NA_character_,
      "wilde eend"
    )
  )
  expected_taxa <- dplyr::tibble(
    scientificName = c("Anas platyrhynchos"),
    taxonRank = c("species"),
    # class: removed because duplicate with less columns has it
    family = c("Anatidae"),
    vernacularNames.eng = c("mallard"),
    # vernacularNames.fra removed because no data
    vernacularNames.nld = c("wilde eend")
  )
  expect_identical(taxa(x), expected_taxa)
})

