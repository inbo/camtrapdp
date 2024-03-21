library(purrr)
library(dplyr)
test_that("observations() returns a tibble data.frame", {
  skip_if_offline()
  dataset <- example_dataset()
  obs <- observations(dataset)
  expect_s3_class(obs, c("tbl_df", "tbl", "data.frame"))
})

test_that("observations() returns the observations", {
  skip_if_offline()
  x <- example_dataset()
  expect_identical(
    observations(x),
    x$data$observations
  )
})

test_that("observations() returns taxonomic information if present", {
  skip_if_offline()
  x <- example_dataset()
  col_taxon <- map(x$taxonomic,
                   list_flatten,
                   name_spec = "{outer}.{inner}") %>%
    map(as.data.frame) %>%
    list_rbind() %>%
    names
  taxon_df <- observations(x) %>%
    select(scientificName, starts_with("taxon."))
  names(taxon_df) <- sub('^taxon.', '', names(taxon_df))
  expect_equal(col_taxon, names(taxon_df))
})

