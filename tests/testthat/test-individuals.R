test_that("individuals() returns a tibble", {
  skip_if_offline()
  x <- example_dataset()
  expect_s3_class(individuals(x), "tbl")
})

test_that("individuals() returns the expected columns", {
  skip_if_offline()
  x <- example_dataset()
  expected_cols <- c(
    "individualID",
    "scientificName",
    "lifeStage",
    "sex"
  )
  expect_named(individuals(x), expected_cols)
})

test_that("individuals() returns the expected rows (unique individuals)", {
  skip_if_offline()
  x <- example_dataset()

  # Expect 0 individuals
  expect_equal(nrow(individuals(x)), 0)

  # Add 3 individuals to event 79204343
  # TODO: remove once https://github.com/tdwg/camtrap-dp/issues/396 is released
  x$data$observations[x$data$observations$observationID == "05230014",]$individualID <- "Fezzik"
  x$data$observations[x$data$observations$observationID == "5ead5692",]$individualID <- "Buttercup"
  x$data$observations[x$data$observations$observationID == "29939500",]$individualID <- "Westley"
  expect_equal(nrow(individuals(x)), 3)
})
