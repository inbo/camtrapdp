test_that("build_taxonomy() returns tibble", {
  skip_if_offline()
  expect_s3_class(build_taxonomy(example_dataset()),
                  "data.frame")
})

test_that("build_taxonomy() returns one row per species in $data$observations",{
  skip_if_offline()
  number_of_species <-
    dplyr::n_distinct(
      example_dataset()$data$observations$scientificName,
      na.rm = TRUE)

  expect_identical(
    nrow(build_taxonomy(example_dataset())),
    number_of_species
  )
})

test_that("build_taxonomy() returns the expected columns", {
  skip_if_offline()
  expect_named(
    build_taxonomy(example_dataset()),
    c("scientificName", "taxonID", "taxonRank", "vernacularNames.eng",
      "vernacularNames.nld")
  )
})

test_that("build_taxonomy() can handle missing vernacular names",{
  # Create a list with the taxonomic part of a Camera Trap Data Package object,
  # the English vernacularName of Anas strepera is not provided.
  missing_vernacular_name <-
  list(taxonomic = list(
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
      vernacularNames = list(nld = "krakeend")
    )
  ))

  # Test to check that vernacularNames are still receiving the
  # `vernacularNames.` prefix
  expect_named(
    build_taxonomy(missing_vernacular_name),
    c("scientificName", "taxonID", "taxonRank", "vernacularNames.eng",
      "vernacularNames.nld")
  )
})

test_that("build_taxonomy() fills missing values with NA when a taxonomic field is only present for some of the records", {
  two_missing_vernacular_names <-
    list(taxonomic = list(
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
        vernacularNames = list(nld = "krakeend")
      )
    ))

  # Check that we still get the `vernacularNames.` prefix
  expect_named(
    build_taxonomy(two_missing_vernacular_names),
    c("scientificName", "taxonID", "taxonRank", "vernacularNames.eng",
      "vernacularNames.nld")
  )
  # Check that the Dutch vernacular name column contains an NA
  expect_contains(
    dplyr::pull(build_taxonomy(two_missing_vernacular_names), vernacularNames.nld),
    NA_character_
  )
  # Check that the English vernacular name column contains an NA
  expect_contains(
    dplyr::pull(build_taxonomy(two_missing_vernacular_names), vernacularNames.eng),
    NA_character_
  )
})

test_that("build_taxonomy() creates a column per language for vernacularName", {
  many_languages <-
    list(taxonomic = list(
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
    ))
  # Expect 6 vernacularName columns
  expect_length(
    dplyr::select(
      build_taxonomy(many_languages),
      dplyr::starts_with("vernacularNames.")
    ),
    6
  )
  # Expect the right vernacularName columns
  expect_named(
    dplyr::select(
      build_taxonomy(many_languages),
      dplyr::starts_with("vernacularNames.")
    ),
    c(
      "vernacularNames.eng", "vernacularNames.nld", "vernacularNames.est",
      "vernacularNames.glv", "vernacularNames.wel", "vernacularNames.afr"
    )
  )
})

test_that("build_taxonomy() returns NULL when there is no taxonomic information", {
  skip_if_offline()
  no_taxonomic_information <- example_dataset()
  no_taxonomic_information$taxonomic <- NULL

  expect_null(
    build_taxonomy(no_taxonomic_information)
  )
})
