test_that("read_camtrapdp() reads a Camtrap DP", {
  skip_if_offline()
  file <-
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json"
  expect_no_error(suppressMessages(read_camtrapdp(file)))
  # TODO: add more tests on returned object
})

test_that("read_camtrapdp() returns error on unsupported Camtrap DP version", {
  skip_if_offline()

  # Unsupported version
  camtrapdp_1.0_rc.1 <-
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0-rc.1/example/datapackage.json"
  expect_error(
    suppressMessages(read_camtrapdp(camtrapdp_1.0_rc.1)),
    class = "camtrapdp_error_unsupported_version"
  )

  # Not a Camtrap DP
  o_assen <- "https://zenodo.org/records/10053903/files/datapackage.json"
  expect_error(
    suppressMessages(read_camtrapdp(o_assen)),
    class = "camtrapdp_error_unsupported_version"
  )
})

test_that("read_camtrapdp() upgrades 1.0", {
  skip_if_offline()
  camtrapdp_1.0 <-
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json"
  x <- suppressMessages(read_camtrapdp(camtrapdp_1.0))
  expect_identical(version(x), "1.0.1")
})

test_that("read_camtrapdp() does not upgrade 1.0.1", {
  skip_if_offline()
  camtrapdp_1.0.1 <-
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0.1/example/datapackage.json"
  x <- suppressMessages(read_camtrapdp(camtrapdp_1.0.1))
  expect_identical(version(x), "1.0.1")
})

test_that("read_camtrapdp() adds eventIDs to media", {
  skip_if_offline()
  file <-
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json"
  x <- suppressMessages(read_camtrapdp(file))
  expect_true("eventID" %in% colnames(media(x)))

  # All media get eventID, except 3 (timeLapse) media that have no event-level obs
  expect_equal(media(x) %>% dplyr::filter(is.na(eventID)) %>% nrow(), 3)

  # At least for example dataset, no duplicate mediaIDs are created
  expect_identical(
    nrow(media(x)),
    nrow(dplyr::distinct(media(x), mediaID)),
  )
})

test_that("read_camtrapdp() returns message", {
  skip_if_offline()
  x <- "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json"
  expect_message(
    read_camtrapdp(x),
    class = "camtrapdp_message_update_metadata"
  )
})
