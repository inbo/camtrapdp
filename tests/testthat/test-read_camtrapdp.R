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

test_that("read_camtrapdp() upgrades 1.0 and 1.0.1", {
  skip_if_offline()
  camtrapdp_1.0 <- read_camtrapdp(
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json"
  )
  camtrapdp_1.0.1 <- read_camtrapdp(
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0.1/example/datapackage.json"
  )

  expect_identical(version(camtrapdp_1.0), "1.0.2")
  expect_identical(version(camtrapdp_1.0.1), "1.0.2")
})

test_that("read_camtrapdp() does not upgrade 1.0.2", {
  skip_if_offline()
  camtrapdp_1.0.2 <- read_camtrapdp(
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0.2/example/datapackage.json"
  )
  expect_identical(version(camtrapdp_1.0.2), "1.0.2")
})

test_that("read_camtrapdp() adds eventIDs to media", {
  skip_if_offline()
  file <-
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json"
  x <- read_camtrapdp(file)
  expect_true("eventID" %in% colnames(media(x)))

  # All media get eventID, except 3 (timeLapse) media that have no event-level obs
  expect_equal(media(x) %>% dplyr::filter(is.na(eventID)) %>% nrow(), 3)

  # At least for example dataset, no duplicate mediaIDs are created
  expect_identical(
    nrow(media(x)),
    nrow(dplyr::distinct(media(x), mediaID)),
  )
})

test_that("read_camtrapdp() creates scopes if missing", {
  skip_if_offline()
  temp_dir <- file.path(tempdir(), "no_scopes")
  on.exit(unlink(temp_dir, recursive = TRUE))
  no_scopes <- frictionless::read_package(
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json"
  )
  no_scopes$spatial <- NULL
  no_scopes$temporal <- NULL
  no_scopes$taxonomic <- NULL
  suppressMessages(frictionless::write_package(no_scopes, temp_dir))

  x <- read_camtrapdp(file.path(temp_dir, "datapackage.json"))
  expect_type(x$spatial, "list")
  expect_type(x$temporal, "list")
  expect_type(x$taxonomic, "list")
})
