test_that("update_taxon() returns error if scientificName is missing from 'to'", {
  skip_if_offline()
  x <- example_dataset()
  from <- "Anas platyrhynchos"
  to <- list(
    taxonID = "https://www.checklistbank.org/dataset/9910/taxon/V8R",
    taxonRank = "genus",
    vernacularNames.eng = "dabbling ducks"
  )
  expect_error(
    update_taxon(x, from, to),
    "camtrapdp_error_scientificname_missing"
    )
})

test_that("update_taxon() updates scientificName in observations", {
  skip_if_offline()
  x <- example_dataset()
  from <- "Anas platyrhynchos"
  to <- list(
    scientificName = "Anas",
    taxonID = "https://www.checklistbank.org/dataset/9910/taxon/V8R",
    taxonRank = "genus",
    vernacularNames.eng = "dabbling ducks"
  )

  x <- update_taxon(x, from, to)

  expected_scientificNames <- c(
    "Anas", NA, "Rattus norvegicus", "Ardea cinerea",
    "Homo sapiens", "Anas strepera", "Aves", "Mustela putorius",
    "Vulpes vulpes", "Martes foina", "Ardea"
  )
  expect_identical(
    expected_scientificNames,
    unique(observations(x)$scientificName)
  )
})
