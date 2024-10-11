test_that("read_taxonomic() returns a data frame", {
  skip_if_offline()
  x <- example_dataset()
  expect_s3_class(read_taxonomic(x), "data.frame")
})

test_that("read_taxonomic() returns NULL when there is no taxonomic information", {
  skip_if_offline()
  x <- example_dataset()
  x$taxonomic <- NULL
  expect_null(read_taxonomic(x))
})

test_that("read_taxonomic() returns one row per species in $data$observations", {
  skip_if_offline()
  x <- example_dataset()
  number_of_species <-
    dplyr::n_distinct(x$data$observations$scientificName, na.rm = TRUE)
  expect_equal(
    nrow(read_taxonomic(x)),
    number_of_species
  )
})

test_that("read_taxonomic() returns the expected columns", {
  skip_if_offline()
  x <- example_dataset()
  expected_cols <- c(
    "scientificName",
    "taxonID",
    "taxonRank",
    "vernacularNames.eng",
    "vernacularNames.nld"
  )
  expect_named(read_taxonomic(x), expected_cols)
})

test_that("read_taxonomic() creates a column per language for vernacularName", {
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
    dplyr::select(read_taxonomic(x), dplyr::starts_with("vernacularNames.")),
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
    dplyr::select(read_taxonomic(x), dplyr::starts_with("vernacularNames.")),
    expected_cols
  )
})

test_that("read_taxonomic() can handle missing vernacular names", {
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
  expect_named(read_taxonomic(x), expected_cols)
})

test_that("read_taxonomic() fills missing values with NA when a taxonomic field
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
  expect_named(read_taxonomic(x), expected_cols)

  # Check that the Dutch vernacular name column contains an NA
  expect_contains(
    dplyr::pull(read_taxonomic(x), vernacularNames.nld),
    NA_character_
  )
  # Check that the English vernacular name column contains an NA
  expect_contains(
    dplyr::pull(read_taxonomic(x), vernacularNames.eng),
    NA_character_
  )
})

test_that("read_taxonomic() returns warning for duplicate scientificNames", {
  skip_if_offline()
  x <- example_dataset()
  # Add duplicate name
  x$taxonomic <- append(
    x$taxonomic,
    list(list(scientificName = "Vulpes vulpes"))
  )
  expect_warning(
    read_taxonomic(x),
    class = "camtrapdp_warning_duplicate_scientificname"
  )
})

test_that("read_taxonomic() uses first record for duplicate scientificNames", {
  skip_if_offline()
  x <- example_dataset()
  # Add duplicate name
  x$taxonomic <- append(
    x$taxonomic,
    list(list(scientificName = "Vulpes vulpes"))
  )
  expect_identical(
    suppressWarnings(read_taxonomic(x))$scientificName,
    unique(suppressWarnings(read_taxonomic(x))$scientificName)
  )
})

test_that("read_taxonomic() removes columns that are empty", {
  skip_if_offline()
  x <- example_dataset()
  # Overwrite taxonomy with two duplicate taxa
  # Only first one (and its columns only) should be retained
  x$taxonomic <-
    list(
      list(
        scientificName = "Vulpes vulpes",
        taxonID = "https://www.wikidata.org/wiki/Q8332",
        taxonRank = "species",
        vernacularNames = list(
          eng = "red fox", # Retain
          lbe = NA_character_ # Ignore
        )
      ),
      list(
        scientificName = "Vulpes vulpes",
        order = "Carnivora", # Ignore
        vernacularNames = list(
          dsb = "Cerwjena liška", # Ignore
          eng = "red fox",
          est = "Rebane" # Ignore
        )
      )
    )
  expect_named(
    suppressWarnings(read_taxonomic(x)),
    c(
      "scientificName",
      "taxonID",
      "taxonRank",
      "vernacularNames.eng"
    )
  )
})
