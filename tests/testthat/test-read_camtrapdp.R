test_that("read_camtrapdp() reads a Camtrap DP", {
  skip_if_offline()
  file <-
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json"
  expect_no_error(read_camtrapdp(file))
  # TODO: add more tests on returned object
})

test_that("read_camtrapdp() returns error on unsupported Camtrap DP version", {
  skip_if_offline()

  # Unsupported version
  camtrapdp_1.0_rc.1 <-
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0-rc.1/example/datapackage.json"
  expect_error(
    read_camtrapdp(camtrapdp_1.0_rc.1),
    class = "camtrapdp_error_unsupported_version"
  )

  # Not a Camtrap DP
  o_assen <- "https://zenodo.org/records/10053903/files/datapackage.json"
  expect_error(
    read_camtrapdp(o_assen),
    class = "camtrapdp_error_unsupported_version"
  )
})

test_that("read_camtrapdp() does not convert 1.0", {
  skip_if_offline()
  camtrapdp_1.0 <-
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json"
  x <- read_camtrapdp(camtrapdp_1.0)
  expect_identical(version(x), "1.0")
})

test_that("read_camtrapdp() optionally adds eventIDs to media", {
  skip_if_offline()
  file <-
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json"

  # Don't assign eventID
  x_without_eventid <- read_camtrapdp(file, media_eventid = FALSE)
  expect_true(!"eventID" %in% colnames(media(x_without_eventid)))

  # Assign eventID
  x <- read_camtrapdp(file, media_eventid = TRUE)
  expect_true("eventID" %in% colnames(media(x)))
  expect_identical(
    nrow(media(x)),
    nrow(media(x_without_eventid)) # No duplicates are created in example dataset
  )
  expect_equal(
    media(x) %>% dplyr::filter(is.na(eventID)) %>% nrow(),
    3 # Only 3 (timeLapse) media don't have an eventID assigned
  )
})
