test_that("build_taxonomy() returns tibble", {

})

test_that("build_taxonomy() returns one row per species in $data$observations",{
  number_of_species <-
    length(unique(example_package()$data$observations$scientificName))
})

test_that("build_taxonomy() can handle missing vernacular names",{

})

test_that("build_taxonomy() returns NULL when there is no taxonomic information", {

})

test_that("build_taxonomy() fills missing values with NA when a taxonomic field is only present for some of the records", {

})
