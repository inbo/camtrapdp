test_that("write_camtrapdp() writes datapackage.json and CSV files to a
           directory and returns NULL invisibly", {
  skip_if_offline()
  x <- example_dataset()
  temp_dir <- file.path(tempdir(), "package")
  on.exit(unlink(temp_dir, recursive = TRUE))
  result <- write_camtrapdp(x, temp_dir)

  expect_identical(
    list.files(temp_dir),
    c("datapackage.json", "deployments.csv", "media.csv", "observations.csv")
  )
  expect_null(result)
  expect_invisible(write_camtrapdp(x, temp_dir))
})

test_that("write_camtrapdp() writes the unaltered example dataset as is", {
  skip_if_offline()
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
  expect_identical(written_datapackage, original_datapackage)

  # deployments.csv
  original_deployments <- suppressMessages(readr::read_delim(
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0.1/example/deployments.csv",
    delim = ","
  ))
  written_deployments <- suppressMessages(readr::read_delim(
    file.path(temp_dir, "deployments.csv")
  ))
  expect_identical(written_deployments, original_deployments)

  # media.csv
  original_media <- suppressMessages(readr::read_delim(
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0.1/example/media.csv",
    delim = ","
  ))
  written_media <- suppressMessages(readr::read_delim(
    file.path(temp_dir, "media.csv")
  ))
  expect_identical(written_media, original_media)

  # observations.csv
  original_observations <- suppressMessages(readr::read_delim(
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0.1/example/observations.csv",
    delim = ","
  ))
  written_observations <- suppressMessages(readr::read_delim(
    file.path(temp_dir, "observations.csv")
  ))
  expect_identical(written_observations, original_observations)
})
