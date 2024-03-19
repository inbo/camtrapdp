test_that("example_dataset() returns the expected dataset", {
  skip_if_offline()
  camtrapdp_1.0 <-
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json"
  expect_identical(
    example_dataset(),
    read_camtrapdp(camtrapdp_1.0)
  )
})
