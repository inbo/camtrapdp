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

test_that("write_camtrapdp() writes the same json and CSV files as the original
          files", {
  x <- example_dataset()
  temp_dir <- file.path(tempdir(), "package")
  on.exit(unlink(temp_dir, recursive = TRUE))
  write_camtrapdp(x, temp_dir)

  # datapackage.json
  original_datapackage <- jsonlite::fromJSON(
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0.1/example/datapackage.json",
    simplifyDataFrame = FALSE, simplifyVector = TRUE
    )
  written_datapackage <- jsonlite::fromJSON(
    file.path(temp_dir, "datapackage.json"),
    simplifyDataFrame = FALSE, simplifyVector = TRUE
    )
  expect_identical(original_datapackage, written_datapackage)

  # deployments.csv
  original_deployments <- suppressMessages(readr::read_delim(
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/deployments.csv",
    delim = ","
  ))
  written_deployments <- suppressMessages(readr::read_delim(
    file.path(temp_dir, "deployments.csv")
  ))
  expect_identical(original_deployments, written_deployments)

  # media.csv
  original_media <- suppressMessages(readr::read_delim(
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/media.csv",
    delim = ","
  ))
  written_media <- suppressMessages(readr::read_delim(
    file.path(temp_dir, "media.csv")
  ))
  expect_identical(original_media, written_media)

  # observations.csv
  original_observations <- suppressMessages(readr::read_delim(
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/observations.csv",
    delim = ","
  ))
  written_observations <- suppressMessages(readr::read_delim(
    file.path(temp_dir, "observations.csv")
  ))
  expect_identical(original_observations, written_observations)

})
