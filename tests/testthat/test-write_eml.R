test_that("write_eml() writes an eml to the given directory", {
  skip_if_offline()
  x <- example_dataset()
  # compare against known good result
  eml <-
    suppressMessages(write_eml(x, directory = NULL))
  ## don't compare the packageId because it's a random guid
  purrr::pluck(eml, "packageId") <- NULL
  expect_snapshot(eml)

  # write to temp dir
  suppressMessages(expect_no_error(write_eml(x, directory = tempdir())))
  ## read from file, and remove packageID because it's a random guid
  eml_from_file <- EML::read_eml(file.path(tempdir(), "eml.xml"))
  purrr::pluck(eml_from_file, "packageId") <- NULL
  ## compare to known output
  expect_snapshot(eml_from_file)
  ## remove temp file
  unlink(file.path(tempdir(), "eml.xml"))
})
