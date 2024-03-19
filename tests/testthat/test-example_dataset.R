test_that("example_dataset() returns the expected object", {
  skip_if_offline()

  expect_identical(
    read_camtrap_dp(
      file =
        file.path(
          "https://raw.githubusercontent.com",
          "tdwg",
          "camtrap-dp", "1.0", "example", "datapackage.json"
        )
    ),
    example_dataset()
  )
})
