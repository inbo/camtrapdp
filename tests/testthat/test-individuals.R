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

  # Expect 3 individuals
  x$data$observations[1,]$individualID <- "anas_1"
  x$data$observations[23,]$individualID <- "rattus_1"
  x$data$observations[75,]$individualID <- "anas_2"
  expect_equal(nrow(individuals(x)), 3)
})
