test_that("get_version() extracts version from dataset$profile on match", {
  dataset <- list()
  dataset$profile <- "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/camtrap-dp-profile.json"
  expect_identical(get_version(dataset), "1.0")
  dataset$profile <- "https://rs.gbif.org/sandbox/data-packages/camtrap-dp/1.0/profile/camtrap-dp-profile.json"
  expect_identical(get_version(dataset), "1.0")
  dataset$profile <- "foo/camtrap-dp/2.30/foo"
  expect_identical(get_version(dataset), "2.30")
  dataset$profile <- "foo/camtrap-dp/2.30.5/foo"
  expect_identical(get_version(dataset), "2.30.5")
})

test_that("get_version() returns dataset$profile when no match is found", {
  dataset <- list()
  dataset$profile <- NULL
  expect_identical(get_version(dataset), NA)
  dataset$profile <- NA
  expect_identical(get_version(dataset), NA)
  dataset$profile <- 1
  expect_identical(get_version(dataset), 1)
  dataset$profile <- "tabular-data-package"
  expect_identical(get_version(dataset), "tabular-data-package")
  dataset$profile <- "foo/2.30/foo" # Must contain camtrap-dp/<version>
  expect_identical(get_version(dataset), "foo/2.30/foo")
  dataset$profile <- "foo/camtrap-dp/2/foo" # Must at least be major/minor
  expect_identical(get_version(dataset), "foo/camtrap-dp/2/foo")
})
