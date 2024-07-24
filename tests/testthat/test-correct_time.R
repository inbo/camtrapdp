test_that("correct_time() returns a valid camtrapdp object", {
  skip_if_offline()
  x <- example_dataset()
  deploymentID <- c("00a2c20d", "29b7d356")
  wrong <- lubridate::ymd_hms("2024-04-01T00:00:00", tz = "UTC")
  right <- lubridate::ymd_hms("2024-04-01T02:00:00", tz = "UTC")
  duration <- lubridate::as.duration(lubridate::interval(wrong, right))
  expect_no_error(
    check_camtrapdp(
      suppressMessages(correct_time(x, deploymentID, duration))
    )
  )
})

test_that("correct_time() returns error on empty or invalid deploymentID", {
  skip_if_offline()
  x <- example_dataset()
  wrong <- lubridate::ymd_hms("2024-04-01T00:00:00", tz = "UTC")
  right <- lubridate::ymd_hms("2024-04-01T02:00:00", tz = "UTC")
  duration <- lubridate::as.duration(lubridate::interval(wrong, right))
  expect_error(
    correct_time(x, 123, duration),
    class = "camtrapdp_error_deploymentID_invalid"
  )
  expect_error(
    correct_time(x, "not an id", duration),
    class = "camtrapdp_error_deploymentID_invalid"
  )
  expect_error(
    correct_time(x, c("00a2c20d", "not an id"), duration),
    class = "camtrapdp_error_deploymentID_invalid"
  )
  expect_error(
    correct_time(x, c("00a2c20d", 00220), duration),
    class = "camtrapdp_error_deploymentID_invalid"
  )
})

test_that("correct_time() returns error on invalid duration", {
  skip_if_offline()
  x <- example_dataset()
  deploymentID <- c("00a2c20d", "29b7d356")
  wrong <- lubridate::ymd_hms("2024-04-01T00:00:00", tz = "UTC")
  right <- lubridate::ymd_hms("2024-04-01T02:00:00", tz = "UTC")
  duration <- lubridate::as.duration(lubridate::interval(wrong, right))
  expect_error(
    correct_time(x, deploymentID, "02:00:00"),
    class = "camtrapdp_error_duration_invalid"
  )
  expect_error(
    correct_time(x, deploymentID, 1),
    class = "camtrapdp_error_duration_invalid"
  )
  expect_error(
    correct_time(x, deploymentID, as.Date('1915-6-16')),
    class = "camtrapdp_error_duration_invalid"
  )
  expect_error(
    correct_time(
      x, deploymentID, as.POSIXct("2024-04-01 00:00:00", tz = "UTC")
      ),
    class = "camtrapdp_error_duration_invalid"
  )
})

test_that("correct_time() returns warning on duplicated deploymentID's", {
  skip_if_offline()
  x <- example_dataset()
  deploymentID <- c("00a2c20d", "00a2c20d", "29b7d356")
  wrong <- lubridate::ymd_hms("2024-04-01T00:00:00", tz = "UTC")
  right <- lubridate::ymd_hms("2024-04-01T02:00:00", tz = "UTC")
  duration <- lubridate::as.duration(lubridate::interval(wrong, right))
  expect_warning(
    suppressMessages(correct_time(x, deploymentID, duration)),
    class = "camtrapdp_warning_deploymentID_duplicated"
  )
})

test_that("correct_time() returns no error on valid deploymentID", {
  skip_if_offline()
  x <- example_dataset()
  wrong <- lubridate::ymd_hms("2024-04-01T00:00:00", tz = "UTC")
  right <- lubridate::ymd_hms("2024-04-01T02:00:00", tz = "UTC")
  duration <- lubridate::as.duration(lubridate::interval(wrong, right))
  # `deploymentID` has length 1
  expect_no_error(suppressMessages(correct_time(x, "62c200a9", duration)))
  # `deploymentID` has length 2
  expect_no_error(
    suppressMessages(correct_time(x, c("62c200a9", "29b7d356"), duration))
  )
  # `deploymentID` contains all the deploymentIDs
  expect_no_error(
    suppressMessages(
      correct_time(
        x, c("00a2c20d", "29b7d356", "577b543a", "62c200a9"), duration
      )
    )
  )
})

test_that("correct_time() returns no error and corrects datetimes of given
           deploymentID when duration has class Duration", {
  skip_if_offline()
  x <- example_dataset()
  depID <- "00a2c20d"

  # Get wrong datetimes for deploymentID == depID
  x_to_correct <- filter_deployments(x, .data$deploymentID == depID)
  deployment_to_correct <- deployments(x_to_correct)
  deploymentStart_to_correct <-
    dplyr::pull(deployment_to_correct, deploymentStart)
  deploymentEnd_to_correct <- dplyr::pull(deployment_to_correct, deploymentEnd)
  observations_to_correct <- observations(x_to_correct)
  eventStart_to_correct <- dplyr::pull(observations_to_correct, eventStart)[1]
  eventEnd_to_correct <- dplyr::pull(observations_to_correct, eventEnd)[1]
  timestamp_to_correct <- dplyr::pull(media(x_to_correct), timestamp)[1]

  # Set parameters
  wrong_Duration <- lubridate::ymd_hms("2024-04-01T00:00:00", tz = "UTC")
  right_Duration <- lubridate::ymd_hms("2024-04-02T02:17:03", tz = "UTC")
  duration_Duration <-
    lubridate::as.duration(lubridate::interval(wrong_Duration, right_Duration))

  # No error test
  expect_no_error(suppressMessages(correct_time(x, depID, duration_Duration)))

  x_corrected <- suppressMessages(correct_time(x, depID, duration_Duration))

  # Get corrected datetimes
  x_corrected_filtered <- filter_deployments(
    x_corrected, .data$deploymentID == depID
    )
  deployment_corrected <- deployments(x_corrected_filtered)
  deploymentStart_corrected <- dplyr::pull(
    deployment_corrected, deploymentStart
    )
  deploymentEnd_corrected <- dplyr::pull(deployment_corrected, deploymentEnd)
  observations_corrected <- observations(x_corrected_filtered)
  eventStart_corrected <- dplyr::pull(observations_corrected, eventStart)[1]
  eventEnd_corrected <- dplyr::pull(observations_corrected, eventEnd)[1]
  timestamp_corrected <- dplyr::pull(media(x_corrected_filtered), timestamp)[1]

  # Test corrected deployment
  expect_identical(
    deploymentStart_to_correct + duration_Duration,
    deploymentStart_corrected,
    )
  expect_identical(
    deploymentStart_corrected,
    as.POSIXct("2020-05-31 05:14:40", tz = "UTC")
    )
  expect_identical(
    deploymentEnd_to_correct + duration_Duration,
    deploymentEnd_corrected,
  )
  expect_identical(
    deploymentEnd_corrected,
    as.POSIXct("2020-07-02 11:58:44", tz = "UTC")
  )

  # Test corrected observations
  expect_identical(
    eventStart_to_correct + duration_Duration,
    eventStart_corrected
  )
  expect_identical(
    eventStart_corrected,
    as.POSIXct("2020-05-31 05:14:40", tz = "UTC")
  )
  expect_identical(
    eventEnd_to_correct + duration_Duration,
    eventEnd_corrected
  )
  expect_identical(
    eventEnd_corrected,
    as.POSIXct("2020-05-31 05:14:47", tz = "UTC")
  )

  # Test corrected media
  expect_identical(
    timestamp_to_correct + duration_Duration,
    timestamp_corrected
  )
  expect_identical(
    timestamp_corrected,
    as.POSIXct("2020-05-31 05:14:40", tz = "UTC")
  )
})

test_that("correct_time() does not correct datetimes of a deploymentID that is
           not selected, when duration has class Duration", {
  skip_if_offline()
  x <- example_dataset()
  depID <- "00a2c20d"
  depID_unchanged <- "29b7d356"

  # Set parameters
  wrong_Duration <- lubridate::ymd_hms("2024-04-01T00:00:00", tz = "UTC")
  right_Duration <- lubridate::ymd_hms("2024-04-02T02:17:03", tz = "UTC")
  duration_Duration <-
    lubridate::as.duration(lubridate::interval(wrong_Duration, right_Duration))

  # Get datetimes of original x for a deploymentID that does not need correction
  x_filtered <- filter_deployments(x, .data$deploymentID == depID_unchanged)
  deployment_no_correction <- deployments(x_filtered)
  deploymentStart_no_correction <-
    dplyr::pull(deployment_no_correction, deploymentStart)
  deploymentEnd_no_correction <-
    dplyr::pull(deployment_no_correction, deploymentEnd)
  observations_no_correction <- observations(x_filtered)
  eventStart_no_correction <-
    dplyr::pull(observations_no_correction, eventStart)[1]
  eventEnd_no_correction <- dplyr::pull(observations_no_correction, eventEnd)[1]
  timestamp_no_correction <- dplyr::pull(media(x_filtered), timestamp)[1]

  # correct_time()
  x_corrected <- suppressMessages(correct_time(x, depID, duration_Duration))

  # Get unchanged datetimes for a deploymentID that does not need correction
  x_unchanged_filtered <- filter_deployments(
    x_corrected, .data$deploymentID == depID_unchanged
  )
  deployment_unchanged <- deployments(x_unchanged_filtered)
  deploymentStart_unchanged <- dplyr::pull(
    deployment_unchanged, deploymentStart
  )
  deploymentEnd_unchanged <- dplyr::pull(deployment_unchanged, deploymentEnd)
  observations_unchanged <- observations(x_unchanged_filtered)
  eventStart_unchanged <- dplyr::pull(observations_unchanged, eventStart)[1]
  eventEnd_unchanged <- dplyr::pull(observations_unchanged, eventEnd)[1]
  timestamp_unchanged <- dplyr::pull(media(x_unchanged_filtered), timestamp)[1]

  # Test unchanged deployment
  expect_identical(deploymentStart_no_correction, deploymentStart_unchanged)
  expect_identical(deploymentEnd_no_correction, deploymentEnd_unchanged)

  # Test unchanged observations
  expect_identical(eventStart_no_correction, eventStart_unchanged)
  expect_identical(eventEnd_no_correction, eventEnd_unchanged)

  # Test unchanged media
  expect_identical(timestamp_no_correction, timestamp_unchanged)
})

test_that("correct_time() returns no error and corrects datetimes of given
           deploymentID when duration has class difftime", {
  skip_if_offline()
  x <- example_dataset()
  depID <- "00a2c20d"

  # Get wrong datetimes for deploymentID == depID
  x_to_correct <- filter_deployments(x, .data$deploymentID == depID)
  deployment_to_correct <- deployments(x_to_correct)
  deploymentStart_to_correct <-
    dplyr::pull(deployment_to_correct, deploymentStart)
  deploymentEnd_to_correct <- dplyr::pull(deployment_to_correct, deploymentEnd)
  observations_to_correct <- observations(x_to_correct)
  eventStart_to_correct <- dplyr::pull(observations_to_correct, eventStart)[1]
  eventEnd_to_correct <- dplyr::pull(observations_to_correct, eventEnd)[1]
  timestamp_to_correct <- dplyr::pull(media(x_to_correct), timestamp)[1]

  # Option 1 to obtain a difftime object
  wrong_POSIXct <- as.POSIXct("2024-04-01 00:00:00", tz = "UTC")
  right_POSIXct <- as.POSIXct("2024-04-02 02:17:03", tz = "UTC")
  duration_difftime1 <- right_POSIXct - wrong_POSIXct

  # Option 2 to obtain a difftime object
  wrong <- "2024-04-01 00:00:00"
  right <- "2024-04-02 02:17:03"
  duration_difftime2 <- difftime(right, wrong, tz = "UTC")

  # Test if both yield the same result
  expect_identical(duration_difftime1, duration_difftime2)

  # Test further with only duration_difftime1

  # No error test
  expect_no_error(suppressMessages(correct_time(x, depID, duration_difftime1)))

  x_corrected <- suppressMessages(correct_time(x, depID, duration_difftime1))

  # Get corrected datetimes
  x_corrected_filtered <- filter_deployments(
    x_corrected, .data$deploymentID == depID
  )
  deployment_corrected <- deployments(x_corrected_filtered)
  deploymentStart_corrected <- dplyr::pull(
    deployment_corrected, deploymentStart
  )
  deploymentEnd_corrected <- dplyr::pull(deployment_corrected, deploymentEnd)
  observations_corrected <- observations(x_corrected_filtered)
  eventStart_corrected <- dplyr::pull(observations_corrected, eventStart)[1]
  eventEnd_corrected <- dplyr::pull(observations_corrected, eventEnd)[1]
  timestamp_corrected <- dplyr::pull(media(x_corrected_filtered), timestamp)[1]

  # Test corrected deployment
  expect_identical(
    deploymentStart_to_correct + duration_difftime1,
    deploymentStart_corrected,
  )
  expect_identical(
    deploymentStart_corrected,
    as.POSIXct("2020-05-31 05:14:40", tz = "UTC")
  )
  expect_identical(
    deploymentEnd_to_correct + duration_difftime1,
    deploymentEnd_corrected,
  )
  expect_identical(
    deploymentEnd_corrected,
    as.POSIXct("2020-07-02 11:58:44", tz = "UTC")
  )

  # Test corrected observations
  expect_identical(
    eventStart_to_correct + duration_difftime1,
    eventStart_corrected
  )
  expect_identical(
    eventStart_corrected,
    as.POSIXct("2020-05-31 05:14:40", tz = "UTC")
  )
  expect_identical(
    eventEnd_to_correct + duration_difftime1,
    eventEnd_corrected
  )
  expect_identical(
    eventEnd_corrected,
    as.POSIXct("2020-05-31 05:14:47", tz = "UTC")
  )

  # Test corrected media
  expect_identical(
    timestamp_to_correct + duration_difftime1,
    timestamp_corrected
  )
  expect_identical(
    timestamp_corrected,
    as.POSIXct("2020-05-31 05:14:40", tz = "UTC")
  )
})

test_that("correct_time() does not correct datetimes of a deploymentID that is
           not selected, when duration has class difftime", {
  skip_if_offline()
  x <- example_dataset()
  depID <- "00a2c20d"
  depID_unchanged <- "29b7d356"

  # Set parameters
  wrong_POSIXct <- as.POSIXct("2024-04-01 00:00:00", tz = "UTC")
  right_POSIXct <- as.POSIXct("2024-04-02 02:17:03", tz = "UTC")
  duration_difftime <- right_POSIXct - wrong_POSIXct

  # Get datetimes of original x for a deploymentID that does not need correction
  x_filtered <- filter_deployments(x, .data$deploymentID == depID_unchanged)
  deployment_no_correction <- deployments(x_filtered)
  deploymentStart_no_correction <-
    dplyr::pull(deployment_no_correction, deploymentStart)
  deploymentEnd_no_correction <-
    dplyr::pull(deployment_no_correction, deploymentEnd)
  observations_no_correction <- observations(x_filtered)
  eventStart_no_correction <-
    dplyr::pull(observations_no_correction, eventStart)[1]
  eventEnd_no_correction <- dplyr::pull(observations_no_correction, eventEnd)[1]
  timestamp_no_correction <- dplyr::pull(media(x_filtered), timestamp)[1]

  # correct_time()
  x_corrected <- suppressMessages(correct_time(x, depID, duration_difftime))

  # Get unchanged datetimes for a deploymentID that does not need correction
  x_unchanged_filtered <- filter_deployments(
    x_corrected, .data$deploymentID == depID_unchanged
  )
  deployment_unchanged <- deployments(x_unchanged_filtered)
  deploymentStart_unchanged <- dplyr::pull(
    deployment_unchanged, deploymentStart
  )
  deploymentEnd_unchanged <- dplyr::pull(deployment_unchanged, deploymentEnd)
  observations_unchanged <- observations(x_unchanged_filtered)
  eventStart_unchanged <- dplyr::pull(observations_unchanged, eventStart)[1]
  eventEnd_unchanged <- dplyr::pull(observations_unchanged, eventEnd)[1]
  timestamp_unchanged <- dplyr::pull(media(x_unchanged_filtered), timestamp)[1]

  # Test unchanged deployment
  expect_identical(deploymentStart_no_correction, deploymentStart_unchanged)
  expect_identical(deploymentEnd_no_correction, deploymentEnd_unchanged)

  # Test unchanged observations
  expect_identical(eventStart_no_correction, eventStart_unchanged)
  expect_identical(eventEnd_no_correction, eventEnd_unchanged)

  # Test unchanged media
  expect_identical(timestamp_no_correction, timestamp_unchanged)
})

test_that("correct_time() updates temporal scope in metadata", {
  skip_if_offline()
  x <- example_dataset()
  deploymentID <- c("00a2c20d", "62c200a9")
  wrong <- lubridate::ymd_hms("2024-04-01T00:00:00", tz = "UTC")
  right <- lubridate::ymd_hms("2024-04-03T02:00:00", tz = "UTC")
  duration <- lubridate::as.duration(lubridate::interval(wrong, right))
  x_corrected <- suppressMessages(correct_time(x, deploymentID, duration))
  expect_identical(x_corrected$temporal$start, "2020-06-01")
  expect_identical(x_corrected$temporal$end, "2021-04-20")
})

test_that("correct_time() returns message", {
  skip_if_offline()
  x <- example_dataset()
  deploymentID <- c("00a2c20d", "29b7d356")
  wrong <- lubridate::ymd_hms("2024-04-01T00:00:00", tz = "UTC")
  right <- lubridate::ymd_hms("2024-04-01T02:00:00", tz = "UTC")
  duration <- lubridate::as.duration(lubridate::interval(wrong, right))

  expect_message(
    correct_time(x, deploymentID, duration),
    regexp = "v Timestamps in selected deployments, media and observations were shifted by\n7200s (~2 hours) (e.g. 2020-05-30 02:57:37 is now\n2020-05-30 04:57:37).\n",
    fixed = TRUE
  )
})
