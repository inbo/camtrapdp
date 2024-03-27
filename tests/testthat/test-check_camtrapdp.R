test_that("check_camtrapdp() returns x invisibly when valid", {
  skip_if_offline()
  x <- example_dataset()
  expect_identical(check_camtrapdp(x), x)
  expect_invisible(check_camtrapdp(x))
})

test_that("read_camtrapdp() returns error on invalid camtrapdp object", {
  skip_if_offline()
  o_assen <- "https://zenodo.org/records/10053903/files/datapackage.json"
  expect_error(
    check_camtrapdp(o_assen),
    class = "camtrapdp_error_object_invalid"
  )
  expect_error(
    check_camtrapdp(o_assen),
    regexp = paste(
      "`x` must be a Camera Trap Data Package object created with",
      "`read_camtrapdp()`."
    ),
    fixed = TRUE
  )
})
