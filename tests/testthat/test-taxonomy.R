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
