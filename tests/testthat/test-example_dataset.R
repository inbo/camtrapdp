test_that("example_dataset() returns the latest example dataset", {
  skip_if_offline()
  camtrapdp_latest <-
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0.1/example/datapackage.json"
  expect_identical(
    example_dataset(),
    read_camtrapdp(camtrapdp_latest)
  )
})

test_that("example_dataset() returns a valid camtrapdp object", {
  skip_if_offline()
  expect_no_error(check_camtrapdp(example_dataset()))
})
