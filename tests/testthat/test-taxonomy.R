test_that("taxonomy() returns a tibble", {
  skip_if_offline()
  x <- example_dataset()
  expect_s3_class(taxonomy(x), "tbl")
})

test_that("taxonomy returns the columns scientificName and with taxon prefix", {
  skip_if_offline()
  dataset <- example_dataset()
  taxa <- taxonomy(dataset)
  expect_named(
    taxa,
    c(
      "scientificName",
      "taxon.taxonID",
      "taxon.taxonRank",
      "taxon.vernacularNames.eng",
      "taxon.vernacularNames.nld"
    )
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
  expect_lte(nrow(taxa), nrow(obs))
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
