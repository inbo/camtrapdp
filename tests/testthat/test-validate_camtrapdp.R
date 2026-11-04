test_that("validate_camtrapdp() returns a list of data frames invisibly", {
  skip_if_offline()
  x <- example_dataset()
  result <- suppressMessages(validate_camtrapdp(x))

  expect_named(result, c("deployments", "observations"))
  expect_s3_class(result$deployments, "tbl")
  expect_s3_class(result$observations, "tbl")
  expect_invisible(suppressMessages(validate_camtrapdp(x)))
})

test_that("validate_camtrapdp() informs about the the number of issues per table", {
  skip_if_offline()
  x <- example_dataset()

  # No issues
  expect_output(
    validate_camtrapdp(x),
    regexp = paste(
      "* deployments: 0 issues",
      "* observations: 0 issues",
      sep = "\n"
    ),
    fixed = TRUE
  )
})
