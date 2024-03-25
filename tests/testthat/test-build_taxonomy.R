test_that("build_taxonomy() returns a data frame", {
  skip_if_offline()
  x <- example_dataset()
  expect_s3_class(build_taxonomy(x), "data.frame")
})

test_that("build_taxonomy() returns NULL when there is no taxonomic information", {
  skip_if_offline()
  x <- example_dataset()
  x$taxonomic <- NULL
  expect_null(build_taxonomy(x))
})

test_that("build_taxonomy() returns one row per species in $data$observations", {
  skip_if_offline()
  x <- example_dataset()
  number_of_species <-
    dplyr::n_distinct(x$data$observations$scientificName, na.rm = TRUE)
  expect_equal(
    nrow(build_taxonomy(x)),
    number_of_species
  )
})

test_that("build_taxonomy() returns the expected columns", {
  skip_if_offline()
  x <- example_dataset()
  expected_cols <- c(
    "scientificName",
    "taxonID",
    "taxonRank",
    "vernacularNames.eng",
    "vernacularNames.nld"
  )
  expect_named(build_taxonomy(x), expected_cols)
})

test_that("build_taxonomy() creates a column per language for vernacularName", {
  skip_if_offline()
  x <- example_dataset()
  taxonomy_many_languages <- list(
    list(
      scientificName = "Anas platyrhynchos",
      taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/DGP6",
      taxonRank = "species",
      vernacularNames = list(
        eng = "mallard",
        nld = "wilde eend",
        est = "sinikael-part",
        glv = "Laagh Voirrey",
        wel = "Hwyaden Wyllt",
        afr = "Groenkopeend"
      )
    )
  )
  x$taxonomic <- taxonomy_many_languages

  # Expect 6 vernacularName columns
  expect_length(
    dplyr::select(build_taxonomy(x), dplyr::starts_with("vernacularNames.")),
    6
  )

  # Expect the right vernacularName columns
  expected_cols <- c(
    "vernacularNames.eng",
    "vernacularNames.nld",
    "vernacularNames.est",
    "vernacularNames.glv",
    "vernacularNames.wel",
    "vernacularNames.afr"
  )
  expect_named(
    dplyr::select(build_taxonomy(x), dplyr::starts_with("vernacularNames.")),
    expected_cols
  )
})

test_that("build_taxonomy() can handle missing vernacular names", {
  skip_if_offline()
  x <- example_dataset()
  # Create a taxonomy where the English vernacularName of Anas strepera is not
  # provided.
  taxonomy_missing_vernacular <- list(
    list(
      scientificName = "Anas platyrhynchos",
      taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/DGP6",
      taxonRank = "species",
      vernacularNames = list(
        eng = "mallard",
        nld = "wilde eend"
      )
    ),
    list(
      scientificName = "Anas strepera",
      taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/DGPL",
      taxonRank = "species",
      vernacularNames = list(
        nld = "krakeend"
      )
    )
  )
  x$taxonomic <- taxonomy_missing_vernacular

  # Check that we still get the `vernacularNames.` prefix
  expected_cols <- c(
    "scientificName",
    "taxonID",
    "taxonRank",
    "vernacularNames.eng",
    "vernacularNames.nld"
  )
  expect_named(build_taxonomy(x), expected_cols)
})

test_that("build_taxonomy() fills missing values with NA when a taxonomic field
           is only present for some of the records", {
  skip_if_offline()
  x <- example_dataset()
  taxonomy_missing_vernaculars <- list(
    list(
      scientificName = "Anas platyrhynchos",
      taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/DGP6",
      taxonRank = "species",
      vernacularNames = list(
        eng = "mallard"
      )
    ),
    list(
      scientificName = "Anas strepera",
      taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/DGPL",
      taxonRank = "species",
      vernacularNames = list(
        nld = "krakeend"
      )
    )
  )
  x$taxonomic <- taxonomy_missing_vernaculars

  # Check that we still get the `vernacularNames.` prefix
  expected_cols <- c(
    "scientificName",
    "taxonID",
    "taxonRank",
    "vernacularNames.eng",
    "vernacularNames.nld"
  )
  expect_named(build_taxonomy(x), expected_cols)

  # Check that the Dutch vernacular name column contains an NA
  expect_contains(
    dplyr::pull(build_taxonomy(x), vernacularNames.nld),
    NA_character_
  )
  # Check that the English vernacular name column contains an NA
  expect_contains(
    dplyr::pull(build_taxonomy(x), vernacularNames.eng),
    NA_character_
  )
})

test_that("build_taxonomy() returns warning when a scientificName occurs more than once in x$taxonomic", {
  skip_if_offline()
  x <- example_dataset()
  x$taxonomic <- append(
    x$taxonomic,
    list(list(
      scientificName = "Vulpes vulpes",
      taxonID = "https://www.wikidata.org/wiki/Q8332",
      taxonRank = "species",
      vernacularNames = list(
        eng = "red fox",
        lbe = "Цулчӏа"
      )
    ))
  )
  expect_warning(
    build_taxonomy(x),
    class = "camtrapdp_warning_duplicate_scientificname"
  )
  expect_warning(
    build_taxonomy(x),
    regexp = paste(
      "Duplicate scientificNames present in `x$taxonmic`,",
      "only the first one will be returned."
    ),
    fixed = TRUE
  )
})

test_that("build_taxonomy() returns only the first species if duplicates are present in `x$taxonomic`", {
  skip_if_offline()
  x <- example_dataset()
  x$taxonomic <- append(
    x$taxonomic,
    list(list(
      scientificName = "Vulpes vulpes",
      taxonID = "https://www.wikidata.org/wiki/Q8332",
      taxonRank = "species",
      vernacularNames = list(
        eng = "red fox",
        lbe = "Цулчӏа"
      )
    ))
  )
  expect_identical(
    build_taxonomy(x)$scientificName,
    unique(build_taxonomy(x)$scientificName)
  )
})

test_that("build_taxonomy() doesn't return empty columns when a duplicate species has a field missing from the first record of that species in `x$taxonomic`", {

})
