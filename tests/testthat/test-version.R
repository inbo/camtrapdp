test_that("version() extracts version from attribute", {
  x <- list()
  attr(x, "version") <- "1.0"
  expect_identical(version(x), "1.0")
})

test_that("version() extracts version from camtrapdp$profile on match", {
  x <- list()
  x$profile <- "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/camtrap-dp-profile.json"
  expect_identical(version(x), "1.0")
  x$profile <- "https://rs.gbif.org/sandbox/data-packages/camtrap-dp/1.0/profile/camtrap-dp-profile.json"
  expect_identical(version(x), "1.0")
  x$profile <- "foo/camtrap-dp/2/foo"
  expect_identical(version(x), "2")
  x$profile <- "foo/camtrap-dp/2.30/foo"
  expect_identical(version(x), "2.30")
  x$profile <- "foo/camtrap-dp/2.30.5/foo"
  expect_identical(version(x), "2.30.5")
  x$profile <- "foo/camtrap-dp/1.0-rc.1/foo"
  expect_identical(version(x), "1.0-rc.1")
})

test_that("version() returns camtrapdp$profile when no match is found", {
  x <- list()
  x$profile <- NULL
  expect_identical(version(x), NA)
  x$profile <- NA
  expect_identical(version(x), NA)
  x$profile <- 1
  expect_identical(version(x), 1)
  x$profile <- "tabular-data-package"
  expect_identical(version(x), "tabular-data-package")
  x$profile <- "foo/2.30/foo" # Must contain camtrap-dp/<version>
  expect_identical(version(x), "foo/2.30/foo")
})
