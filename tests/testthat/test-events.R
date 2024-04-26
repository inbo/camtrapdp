test_that("events() returns a tibble", {
  skip_if_offline()
  x <- example_dataset()
  expect_s3_class(events(x), "tbl")
})

test_that("events() returns the expected columns", {
  skip_if_offline()
  x <- example_dataset()
  expected_cols <- c(
    "deploymentID",
    "eventID",
    "eventStart",
    "eventEnd"
  )
  expect_named(events(x), expected_cols)
})

test_that("events() returns the expected rows (unique locations)", {
  skip_if_offline()
  x <- example_dataset()

  # Expect 31 events
  expect_equal(nrow(events(x)), 31)
})
