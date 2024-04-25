test_that("check_camtrapdp() returns x invisibly when valid", {
  skip_if_offline()
  x <- example_dataset()
  expect_identical(check_camtrapdp(x), x)
  expect_invisible(check_camtrapdp(x))
})

test_that("check_camtrapdp() returns error on invalid camtrapdp object", {
  skip_if_offline()
  o_assen <- frictionless::read_package(
    "https://zenodo.org/records/10053903/files/datapackage.json"
  )
  expect_error(
    check_camtrapdp(o_assen),
    class = "camtrapdp_error_object_invalid"
  )
})

#  Check that deployments, media and observations data frames are present.
test_that("check_camtrapdp() returns error if deployments, media, observations
           are not dataframes", {
  skip_if_offline()
  x <- example_dataset()
  y <- example_dataset()
  z <- example_dataset()
  x$data$deployments <- "not_a_dataframe"
  y$data$media <- NULL
  z$data$observations <- data.frame() # Valid
  expect_error(check_camtrapdp(x), class = "camtrapdp_error_data_invalid")
  expect_error(check_camtrapdp(y), class = "camtrapdp_error_data_invalid")
  expect_no_error(check_camtrapdp(z))
})
