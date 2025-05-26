test_that("observations() returns a tibble", {
  skip_if_offline()
  x <- example_dataset()
  expect_s3_class(observations(x), "tbl")
})

test_that("observations() returns the observations", {
  skip_if_offline()
  x <- example_dataset()
  expect_identical(observations(x), x$data$observations)
})

test_that("observations()<- assigns a data frame (as tibble) as observations", {
  skip_if_offline()
  x <- example_dataset()
  df <- data.frame(scientificName = 1:3)
  observations(x) <- df
  expect_identical(observations(x), dplyr::as_tibble(df))
  expect_s3_class(observations(x), "tbl")
})

test_that("observations()<- returns error when value is not a data frame", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(
    observations(x) <- "not_a_data_frame",
    class = "camtrapdp_error_assignment_wrong_class"
  )
})

test_that("observations()<- updates taxonomic scope in metadata", {
  skip_if_offline()
  x <- example_dataset()
  observations(x) <- data.frame(
    scientificName = "Anas platyrhynchos",
    taxon.taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/DGP6",
    taxon.taxonRank = "species"
  )

  expected_taxonomic <- list(
    list(
      scientificName = "Anas platyrhynchos",
      taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/DGP6",
      taxonRank = "species"
    )
  )
  expect_identical(x$taxonomic, expected_taxonomic)
})

