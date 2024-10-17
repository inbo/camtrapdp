test_that("write_camtrapdp() returns output Camera Trap Data Package
          (invisibly)", {
  x <- example_dataset()

  temp_dir <- file.path(tempdir(), "package")
  on.exit(unlink(temp_dir, recursive = TRUE))
  x_written <- write_camtrapdp(x, temp_dir)
  x_from_file <- read_camtrapdp(file.path(temp_dir, "datapackage.json"))

  expect_invisible(write_camtrapdp(x, temp_dir))

  # $data and $directory will differ: overwrite to make the same
  x_from_file$data <- NULL
  x_from_file$directory <- x_written$directory

  expect_identical(x_written, x_from_file)
})
