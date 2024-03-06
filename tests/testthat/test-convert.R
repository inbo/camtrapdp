test_that("convert() returns error on unsupported Camtrap DP version", {
  dataset <- list()
  # Supported versions
  dataset$profile <- "camtrap-dp/1.0"
  expect_no_error(convert(dataset))

  # Unsupported versions
  dataset$profile <- "camtrap-dp/1.0.1"
  expect_error(convert(dataset), class = "camtrapdp_error_unsupported_version")
  dataset$profile <- "camtrap-dp/1"
  expect_error(convert(dataset), class = "camtrapdp_error_unsupported_version")

  # Unsupported profiles
  dataset$profile <- NULL
  expect_error(convert(dataset), class = "camtrapdp_error_unsupported_version")
  dataset$profile <- NA
  expect_error(convert(dataset), class = "camtrapdp_error_unsupported_version")
  dataset$profile <- "tabular-data-package"
  expect_error(convert(dataset), class = "camtrapdp_error_unsupported_version")

  # Error message
  expect_error(
    convert(dataset),
    regexp = "\"tabular-data-package\" is not a supported Camtrap DP version.",
    fixed = TRUE
  )
})
