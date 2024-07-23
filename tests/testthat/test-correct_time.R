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
  # `deploymentID`contains all the deploymentIDs
  expect_no_error(
    suppressMessages(
      correct_time(
        x, c("00a2c20d", "29b7d356", "577b543a", "62c200a9"), duration
      )
    )
  )
})

test_that("correct_time() returns no error and corrects deploymentStart when
          duration has class Duration", {
  skip_if_offline()
  x <- example_dataset()
  depID <- "00a2c20d"

  # Wrong deploymentStart
  deploymentStart <-
    deployments(x) %>%
    dplyr::filter(.data$deploymentID == depID) %>%
    dplyr::pull(deploymentStart)

  # Set parameters
  wrong_Duration <- lubridate::ymd_hms("2024-04-01T00:00:00", tz = "UTC")
  right_Duration <- lubridate::ymd_hms("2024-04-01T02:00:00", tz = "UTC")
  duration_Duration <-
    lubridate::as.duration(lubridate::interval(wrong_Duration, right_Duration))

  # No error test
  expect_no_error(suppressMessages(correct_time(x, depID, duration_Duration)))

  x_Duration <- suppressMessages(correct_time(x, depID, duration_Duration))

  # New deploymentStart
  deploymentStart_Duration <-
    deployments(x_Duration) %>%
    dplyr::filter(.data$deploymentID == depID) %>%
    dplyr::pull(deploymentStart)

  # Tests
  expect_identical(
    deploymentStart_Duration,
    deploymentStart + duration_Duration
    )
  expect_identical(
    deploymentStart_Duration,
    as.POSIXct("2020-05-30 04:57:37", tz = "UTC")
    )
})

test_that("correct_time() returns no error and corrects deploymentStart when
          deploymentID has class difftime", {
  skip_if_offline()
  x <- example_dataset()
  depID <- "00a2c20d"

  # wrong deploymentStart
  deploymentStart <-
    deployments(x) %>%
    dplyr::filter(.data$deploymentID == depID) %>%
    dplyr::pull(deploymentStart)

  # Option 1 to obtain a difftime object
  wrong_POSIXct <- as.POSIXct("2024-04-01 00:00:00", tz = "UTC")
  right_POSIXct <- as.POSIXct("2024-04-01 02:00:00", tz = "UTC")
  duration_difftime1 <- right_POSIXct - wrong_POSIXct

  # Option 2 to obtain a difftime object
  wrong <- "2024-04-01 00:00:00"
  right <- "2024-04-01 02:00:00"
  duration_difftime2 <- difftime(right, wrong, tz = "UTC")

  # Test if both yield the same result
  expect_identical(duration_difftime1, duration_difftime2)

  # Test further with only duration_difftime1
  # No error test
  expect_no_error(suppressMessages(correct_time(x, depID, duration_difftime1)))

  x_difftime <- suppressMessages(correct_time(x, depID, duration_difftime1))

  # New deploymentStart
  deploymentStart_difftime <-
    deployments(x_difftime) %>%
    dplyr::filter(.data$deploymentID == depID) %>%
    dplyr::pull(deploymentStart)

  # Tests
  expect_identical(
    deploymentStart_difftime,
    deploymentStart + duration_difftime1
  )
  expect_identical(
    deploymentStart_difftime,
    as.POSIXct("2020-05-30 04:57:37", tz = "UTC")
  )
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
  #   regexp = paste(
  #     "Timestamps in selected deployments, media and observations were shifted by 7200s (~2 hours) (e.g. 2020-05-30 02:57:37 is now 2020-05-30 04:57:37).",
  #     "A Camera Trap Data Package with 3 tables:",
  #     "* deployments: 4 rows",
  #     "* media: 423 rows",
  #     "* observations: 549 rows",
  #     "Use `unclass()` to print the Data Package as a list.",
  #     sep = "\n"
  #   ),
  #   fixed = TRUE
  )
})
