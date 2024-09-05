test_that("shift_time() returns a valid camtrapdp object", {
  skip_if_offline()
  x <- example_dataset()
  duration <- lubridate::duration(-4, units = "hours")
  expect_no_error(
    check_camtrapdp(
      suppressMessages(shift_time(x, c("00a2c20d", "29b7d356"), duration))
    )
  )
})

test_that("shift_time() returns error on empty or invalid deployment_id", {
  skip_if_offline()
  x <- example_dataset()
  duration <- lubridate::duration(-4, units = "hours")

  expect_error(
    shift_time(x, 123, duration),
    class = "camtrapdp_error_deployment_id_invalid"
  )
  expect_error(
    shift_time(x, "not_an_id", duration),
    class = "camtrapdp_error_deployment_id_invalid"
  )
  expect_error(
    shift_time(x, c("00a2c20d", "not_an_id"), duration),
    class = "camtrapdp_error_deployment_id_invalid"
  )
  expect_error(
    shift_time(x, c("00a2c20d", 00220), duration),
    class = "camtrapdp_error_deployment_id_invalid"
  )

  # Duplicate IDs are removed (error message only lists them once)
  expect_error(
    shift_time(x, c("not_an_id", "not_an_id"), duration),
    regex = "Can't find deployment with deploymentID \"not_an_id\".",
    fixed = TRUE
  )
})

test_that("shift_time() returns error on invalid duration", {
  skip_if_offline()
  x <- example_dataset()
  deployment_id <- "00a2c20d"

  expect_error(
    shift_time(x, deployment_id, "02:00:00"),
    class = "camtrapdp_error_duration_invalid"
  )
  expect_error(
    shift_time(x, deployment_id, 1),
    class = "camtrapdp_error_duration_invalid"
  )
  expect_error(
    shift_time(x, deployment_id, as.Date('1915-06-16')),
    class = "camtrapdp_error_duration_invalid"
  )
  expect_error(
    shift_time(
      x, deployment_id, as.POSIXct("2024-04-01 00:00:00", tz = "UTC")
      ),
    class = "camtrapdp_error_duration_invalid"
  )
})

test_that("shift_time() shifts datetime for selected deployments, media and
           observations", {
  skip_if_offline()
  x <- example_dataset()
  purrr::pluck(x, "data", "deployments", "timestampIssues", 1) <- TRUE # For test
  deployment_id <- "00a2c20d"
  duration <- lubridate::duration(-4, units = "hours")

  # Current data
  x_selected <- filter_deployments(x, .data$deploymentID == deployment_id)
  x_unselected <- filter_deployments(x, .data$deploymentID != deployment_id)

  # Shift date-time
  x_new <- suppressMessages(shift_time(x, deployment_id, duration))
  x_new_selected <- filter_deployments(x_new, .data$deploymentID == deployment_id)
  x_new_unselected <- filter_deployments(x_new, .data$deploymentID != deployment_id)

  # Check data that should be updated
  expect_identical(
    purrr::pluck(deployments(x_new_selected), "deploymentStart"),
    purrr::pluck(deployments(x_selected), "deploymentStart") + duration
  )
  expect_identical(
    purrr::pluck(deployments(x_new_selected), "deploymentEnd"),
    purrr::pluck(deployments(x_selected), "deploymentEnd") + duration
  )
  expect_identical(
    purrr::pluck(deployments(x_new_selected), "deploymentEnd", 1),
    as.POSIXct("2020-07-01 05:41:41", tz = "UTC") # Orig 2020-07-01 09:41:41 UTC
  )
  expect_identical(
    purrr::pluck(deployments(x_new_selected), "timestampIssues", 1),
    FALSE # Orig TRUE (set to that value above)
  )
  expect_identical(
    purrr::pluck(media(x_new_selected), "timestamp"),
    purrr::pluck(media(x_selected), "timestamp") + duration
  )
  expect_identical(
    purrr::pluck(media(x_new_selected), "timestamp", 1),
    as.POSIXct("2020-05-29 22:57:37", tz = "UTC") # Orig 2020-05-30 02:57:37 UTC
  )
  expect_identical(
    purrr::pluck(observations(x_new_selected), "eventStart"),
    purrr::pluck(observations(x_selected), "eventStart") + duration
  )
  expect_identical(
    purrr::pluck(observations(x_new_selected), "eventEnd"),
    purrr::pluck(observations(x_selected), "eventEnd") + duration
  )
  expect_identical(
    purrr::pluck(observations(x_new_selected), "eventEnd", 1),
    as.POSIXct("2020-05-29 22:57:44", tz = "UTC") # Orig 2020-05-30 02:57:44 UTC
  )

  # Check data that should be NOT be updated
  expect_identical(
    purrr::pluck(deployments(x_new_unselected), "deploymentStart"),
    purrr::pluck(deployments(x_unselected), "deploymentStart")
  )
  expect_identical(
    purrr::pluck(deployments(x_new_unselected), "deploymentEnd"),
    purrr::pluck(deployments(x_unselected), "deploymentEnd")
  )
  expect_identical(
    purrr::pluck(deployments(x_new_unselected), "timestampIssues", 1),
    FALSE
  )
  expect_identical(
    purrr::pluck(media(x_new_unselected), "timestamp"),
    purrr::pluck(media(x_unselected), "timestamp")
  )
  expect_identical(
    purrr::pluck(observations(x_new_unselected), "eventStart"),
    purrr::pluck(observations(x_unselected), "eventStart")
  )
  expect_identical(
    purrr::pluck(observations(x_new_unselected), "eventEnd"),
    purrr::pluck(observations(x_unselected), "eventEnd")
  )
})

test_that("shift_time() supports duration() and difftime()", {
  skip_if_offline()
  x <- example_dataset()
  deployment_id <- "00a2c20d"
  duration <- lubridate::duration(-4, units = "hours")
  difftime <- difftime("2024-04-01 00:00:00", "2024-04-01 04:00:00", tz = "UTC")
  x_duration <- suppressWarnings(shift_time(x, deployment_id, duration))
  x_difftime <- suppressWarnings(shift_time(x, deployment_id, difftime))

  expect_identical(deployments(x_duration), deployments(x_difftime))
  expect_identical(media(x_duration), media(x_difftime))
  expect_identical(observations(x_duration), observations(x_difftime))
})

test_that("shift_time() updates temporal scope in metadata", {
  skip_if_offline()
  x <- example_dataset()
  deployment_id <- c("00a2c20d", "62c200a9") # first and last
  duration <- lubridate::duration(24, units = "hours") # 24h later
  x_new <- suppressMessages(shift_time(x, deployment_id, duration))

  expect_identical(x_new$temporal$start, "2020-05-31") # Orig 2020-05-30
  expect_identical(x_new$temporal$end, "2021-04-19") # Orig 2021-04-18
})

test_that("shift_time() returns message", {
  skip_if_offline()
  x <- example_dataset()
  deployment_id <- "00a2c20d"
  duration <- lubridate::duration(-4, units = "hours")

  expect_message(
    shift_time(x, deployment_id, duration),
    class = "camtrapdp_message_shift_time"
  )
  expect_message(
    shift_time(x, deployment_id, duration),
    regexp = paste(
      "Date-times in selected deployments, media and observations were",
      "shifted by -14400s (~-4 hours). E.g. 2020-05-30 02:57:37 is now",
      "2020-05-29 22:57:37."
    ),
    fixed = TRUE
  )
})
