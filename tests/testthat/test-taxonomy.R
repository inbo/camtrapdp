library(tibble)

test_that("taxonomy returns a tibble data.frame", {
  skip_if_offline()
  dataset <- example_dataset()
  taxa <- taxonomy(dataset)
  expect_true(is.data.frame(taxa))
  expect_true(is_tibble(taxa))
})

test_that("taxonomy returns the expected columns", {
  skip_if_offline()
  dataset <- example_dataset()
  taxa <- taxonomy(dataset)
  cols_taxonomy <- c("scientificName")
  expect_equal(names(taxa), cols_taxonomy)
})

test_that("taxonomy returns the expected amount of rows", {
  skip_if_offline()
  dataset <- example_dataset()
  taxa <- taxonomy(dataset)
  expect_equal(nrow(taxa),11)
})

test_that("taxonomy never returns more rows than observations", {
  skip_if_offline()
  dataset <- example_dataset()
  obs <- observations(dataset)
  taxa <- taxonomy(dataset)
  expect_true(nrow(taxa) <= nrow(obs))
})


test_that(
  "one row returned if all observations have same taxonomy information", {
    skip_if_offline()
    dataset <- example_dataset()
    dataset$data$observations$scientificName <- "Vulpes vulpes"
    taxa <- taxonomy(dataset)
    expect_equal(nrow(taxa), 1)
  })
