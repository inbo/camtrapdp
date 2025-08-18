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

test_that("write_camtrapdp() writes a (filtered) dataset that can be read", {
  skip_if_offline()
  x <- example_dataset()
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  # Filter deployments and write to disk
  write_camtrapdp(filter_deployments(x, deploymentID == "00a2c20d"), temp_dir)

  expect_no_error(read_camtrapdp(file.path(temp_dir, "datapackage.json")))
  x_written <- read_camtrapdp(file.path(temp_dir, "datapackage.json"))
  expect_lt(nrow(deployments(x_written)), nrow(deployments(x)))
  expect_lt(nrow(media(x_written)), nrow(media(x)))
  expect_lt(nrow(observations(x_written)), nrow(observations(x)))
})

test_that("write_camtrapdp() writes the unaltered example dataset as is", {
  skip_if_offline()
  x <- example_dataset()
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  write_camtrapdp(x, temp_dir)

  # datapackage.json
  original_datapackage <- jsonlite::fromJSON(
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0.2/example/datapackage.json",
    simplifyDataFrame = FALSE, simplifyVector = TRUE
  )
  written_datapackage <- jsonlite::fromJSON(
    file.path(temp_dir, "datapackage.json"),
    simplifyDataFrame = FALSE, simplifyVector = TRUE
  )
  expect_identical(written_datapackage, original_datapackage)

  # deployments.csv
  original_deployments <- suppressMessages(readr::read_delim(
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0.2/example/deployments.csv",
    delim = ","
  ))
  written_deployments <- suppressMessages(readr::read_delim(
    file.path(temp_dir, "deployments.csv")
  ))
  expect_identical(written_deployments, original_deployments)

  # media.csv
  original_media <- suppressMessages(readr::read_delim(
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0.2/example/media.csv",
    delim = ","
  ))
  written_media <- suppressMessages(readr::read_delim(
    file.path(temp_dir, "media.csv")
  ))
  expect_identical(written_media, original_media)

  # observations.csv
  original_observations <- suppressMessages(readr::read_delim(
    "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0.2/example/observations.csv",
    delim = ","
  ))
  written_observations <- suppressMessages(readr::read_delim(
    file.path(temp_dir, "observations.csv")
  ))
  expect_identical(written_observations, original_observations)
})

test_that("write_camtrapdp() can write compressed files", {
  skip_if_offline()
  x <- example_dataset()
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  write_camtrapdp(x, temp_dir, compress = TRUE)

  expect_identical(
    list.files(temp_dir),
    c("datapackage.json", "deployments.csv.gz", "media.csv.gz",
      "observations.csv.gz")
  )
})

test_that("write_camtrapdp() returns the expected datapackage.json for the
           example dataset", {
  skip_if_offline()
  x <- example_dataset()
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Adapt x$taxonomic and x$contributors to test for https://github.com/inbo/camtrapdp/issues/185
  x <- x %>%
    update_taxon(
      from = "Anas platyrhynchos",
      to = list(
        scientificName = "Anas platyrhynchos",
        taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/DGP6",
        taxonRank = "species",
        vernacularNames.nld = "wilde eend" # Assigns NA for vernacularNames.eng
      )
    )
  contributors(x) <- contributors(x) # Assigns NA for e.g. firstName of INBO
  write_camtrapdp(x, temp_dir)

  expect_snapshot_file(file.path(temp_dir, "datapackage.json"))
})
