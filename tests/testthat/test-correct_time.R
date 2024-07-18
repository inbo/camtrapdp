test_that("correct_time() returns a valid camtrapdp object", {
  x <- example_dataset()
  deploymentID <- c("00a2c20d", "29b7d356")
  wrong <- lubridate::ymd_hms("2024-04-01T00:00:00", tz = "UTC")
  right <- lubridate::ymd_hms("2024-04-01T02:00:00", tz = "UTC")
  duration <- lubridate::as.duration(lubridate::interval(wrong, right))
  expect_no_error(
    check_camtrapdp(correct_time(x, deploymentID, duration))
  )
})

test_that("correct_time() returns error on empty or invalid deploymentID", {
  x <- example_dataset()
  wrong <- lubridate::ymd_hms("2024-04-01T00:00:00", tz = "UTC")
  right <- lubridate::ymd_hms("2024-04-01T02:00:00", tz = "UTC")
  duration <- lubridate::as.duration(lubridate::interval(wrong, right))
  expect_error(
    correct_time(x = x, deploymentID = 123, duration = duration),
    class = "camtrapdp_error_deploymentID_invalid"
  )
  expect_error(
    correct_time(x = x, deploymentID = "not an id", duration = duration),
    class = "camtrapdp_error_deploymentID_invalid"
  )
  expect_error(
    correct_time(x = x, deploymentID = c("not an id"), duration = duration),
    class = "camtrapdp_error_deploymentID_invalid"
  )
  expect_error(
    correct_time(x = x, deploymentID = c("00a2c20d", "not an id"), duration),
    class = "camtrapdp_error_deploymentID_invalid"
  )
})

test_that("correct_time() returns error on invalid duration", {
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
})

test_that("correct_time() returns no error on valid deploymentID", {
  x <- example_dataset()
  wrong <- lubridate::ymd_hms("2024-04-01T00:00:00", tz = "UTC")
  right <- lubridate::ymd_hms("2024-04-01T02:00:00", tz = "UTC")
  duration <- lubridate::as.duration(lubridate::interval(wrong, right))
  expect_no_error(correct_time(x, "62c200a9", duration))
  expect_no_error(correct_time(x, c("62c200a9", "29b7d356"), duration))
  expect_no_error(
    correct_time(x, c("00a2c20d", "29b7d356", "577b543a", "62c200a9"), duration)
  )
})
