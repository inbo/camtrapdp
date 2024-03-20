test_that("build_taxonomy() returns tibble", {
  expect_s3_class(build_taxonomy(example_dataset()),
                  "data.frame")
})

test_that("build_taxonomy() returns one row per species in $data$observations",{
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
  expect_named(
    build_taxonomy(example_dataset()),
    c("scientificName", "taxonID", "taxonRank", "vernacularNames.eng",
      "vernacularNames.nld")
  )
})

test_that("build_taxonomy() can handle missing vernacular names",{

})

test_that("build_taxonomy() returns NULL when there is no taxonomic information", {

})

test_that("build_taxonomy() fills missing values with NA when a taxonomic field is only present for some of the records", {

})

test_that("build_taxonomy() creates a column per language for vernacularName", {

})
