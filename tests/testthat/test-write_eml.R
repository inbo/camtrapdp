# Create temporary files
temp_dir <- file.path(tempdir(), "camtrapdp")
dir.create(temp_dir)

test_that("write_eml() can write an eml", {
  skip_if_offline()
  x <- example_dataset()
  # compare against known good result
  eml <-
    suppressMessages(write_eml(x, directory = temp_dir))
  ## don't compare the packageId because it's a random guid
  purrr::pluck(eml, "packageId") <- NULL
  expect_snapshot(eml)

  # write to temp dir
  suppressMessages(expect_no_error(write_eml(x, directory = temp_dir)))
  ## read from file, and remove packageID because it's a random guid
  eml_from_file <- EML::read_eml(file.path(temp_dir, "eml.xml"))
  purrr::pluck(eml_from_file, "packageId") <- NULL
  ## compare to known output
  expect_snapshot(eml_from_file)
})

# Remove temporary files
unlink(temp_dir, recursive = TRUE)
