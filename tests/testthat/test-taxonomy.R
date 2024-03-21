test_that("taxonomy() returns a tibble", {
  skip_if_offline()
  x <- example_dataset()
  expect_s3_class(taxonomy(x), "tbl")
})

test_that("taxonomy() returns the expected columns, incl. scientificName", {
  skip_if_offline()
  x <- example_dataset()
  expected_cols <- c(
    "scientificName",
    "taxon.taxonID",
    "taxon.taxonRank",
    "taxon.vernacularNames.eng",
    "taxon.vernacularNames.nld"
  )
  expect_named(taxonomy(x), expected_cols)
})

test_that("taxonomy() returns the expected rows (unique taxa)", {
  skip_if_offline()
  x <- example_dataset()
  expect_equal(nrow(taxonomy(x)), 10)
  # Less or equal than observations
  expect_lte(
    nrow(taxonomy(x)),
    nrow(observations(x))
  )
})

test_that(
  "one row returned if all observations have same taxonomy information", {
    skip_if_offline()
    dataset_only_fox <- example_dataset()
    dataset_only_fox$data$observations$scientificName <- "Vulpes vulpes"
    dataset_only_fox$data$observations$taxon.taxonRank <- "species"
    dataset_only_fox$data$observations$taxon.taxonID <- "https://www.checklistbank.org/dataset/COL2023/taxon/5BSG3"
    dataset_only_fox$data$observations$taxon.vernacularNames.eng <- "red fox"
    dataset_only_fox$data$observations$taxon.vernacularNames.nld <- "vos"
    taxa <- taxonomy(dataset_only_fox)
    expect_identical(nrow(taxa), 1L)
  })
