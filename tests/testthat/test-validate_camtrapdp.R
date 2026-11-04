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

  # With issues
  x$data$deployments$latitude[1] <- NA
  x$data$deployments$deploymentStart[1] <- NA
  x$data$observations$eventStart[1] <- NA

  expect_output(
    validate_camtrapdp(x),
    regexp = paste(
      "* deployments: 2 issues",
      "* observations: 1 issues",
      sep = "\n"
    ),
    fixed = TRUE
  )

})

test_that("validate_camtrapdp() reports missing deployment and observation fields", {
  skip_if_offline()
  x <- example_dataset()

  deployment_id <- x$data$deployments$deploymentID[[1]]
  observation_id <- x$data$observations$observationID[[1]]

  x$data$deployments$latitude[1] <- NA
  x$data$deployments$deploymentStart[1] <- NA
  x$data$observations$eventStart[1] <- NA
  x$data$observations$scientificName[1] <- "Vulpes vulpes"
  x$data$observations$count[1] <- NA_real_

  result <- validate_camtrapdp(x)

  expect_equal(
    result$deployments,
    dplyr::tibble(
      deploymentID = deployment_id,
      field = c("latitude/longitude", "deploymentStart/deploymentEnd"),
      severity = "error",
      type = "required_field_missing",
      message = c(
        "coordinates are required",
        "deploymentStart and deploymentEnd are required"
      )
    )
  )
  expect_equal(
    result$observations,
    dplyr::tibble(
      observationID = observation_id,
      field = c("eventStart/eventEnd", "count"),
      severity = c("error", "issue"),
      type = "required_field_missing",
      message = c(
        "eventStart and eventEnd are required",
        "count is required when scientificName is provided"
      )
    )
  )
})

test_that("validate_camtrapdp() reports zero counts as invalid", {
  skip_if_offline()
  x <- example_dataset()
  observation_id <- x$data$observations$observationID[[1]]
  x$data$observations$count[1] <- 0

  result <- validate_camtrapdp(x)

  expect_equal(
    result$observations,
    dplyr::tibble(
      observationID = observation_id,
      field = "count",
      severity = "error",
      type = "invalid_value",
      message = "count must be greater than 0"
    )
  )
})

test_that("validate_camtrapdp() does not require count without a scientific name", {
  skip_if_offline()
  x <- example_dataset()
  x$data$observations$scientificName[1] <- NA_character_
  x$data$observations$count[1] <- NA_real_

  result <- validate_camtrapdp(x)

  expect_equal(
    result$observations,
    dplyr::tibble(
      observationID = character(),
      field = character(),
      severity = character(),
      type = character(),
      message = character()
    )
  )
})
