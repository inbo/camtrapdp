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
