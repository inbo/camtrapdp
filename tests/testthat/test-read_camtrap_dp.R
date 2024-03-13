test_that("read_camtrap_dp() reads a Camtrap DP", {
  skip_if_offline()
  file <-
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json"
  expect_no_error(read_camtrap_dp(file))

  # TODO: add more tests on returned object
})

test_that("read_camtrap_dp() returns error on unsupported Camtrap DP version", {
  skip_if_offline()

  # Unsupported version
  camtrap_dp_1.0_rc.1 <-
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0-rc.1/example/datapackage.json"
  expect_error(
    read_camtrap_dp(camtrap_dp_1.0_rc.1),
    class = "camtrapdp_error_unsupported_version"
  )
  expect_error(
    read_camtrap_dp(camtrap_dp_1.0_rc.1),
    regexp = "\"1.0-rc.1\" is not a supported Camtrap DP version.",
    fixed = TRUE
  )

  # Not a Camtrap DP
  o_assen <- "https://zenodo.org/records/10053903/files/datapackage.json"
  expect_error(
    read_camtrap_dp(o_assen),
    class = "camtrapdp_error_unsupported_version"
  )
})

test_that("read_camtrap_dp() does not convert 1.0", {
  skip_if_offline()
  camtrap_dp_1.0 <-
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json"
  x <- read_camtrap_dp(camtrap_dp_1.0)
  expect_identical(version(x), "1.0")
})
