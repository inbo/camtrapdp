test_that("taxonomy returns a tibble data.frame", {
  skip_if_offline()
  dataset <- example_dataset()
  taxa <- taxonomy(dataset)
  expect_s3_class(taxa, c("tbl_df", "tbl", "data.frame"))
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
    dataset <- example_dataset()
    dataset$data$observations$scientificName <- "Vulpes vulpes"
    dataset$data$observations$taxon.taxonRank <- "species"
    dataset$data$observations$taxon.taxonID <- "https://www.checklistbank.org/dataset/COL2023/taxon/5BSG3"
    dataset$data$observations$taxon.vernacularNames.eng <- "red fox"
    dataset$data$observations$taxon.vernacularNames.nld <- "vos"
    taxa <- taxonomy(dataset)
    expect_identical(nrow(taxa), 1L)
  })
