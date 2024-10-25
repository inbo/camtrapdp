test_that("write_eml() writes an eml.xml to a directory and returns eml
           invisibly", {
  skip_if_offline()
  x <- example_dataset()
  temp_dir <- file.path(tempdir(), "eml")
  on.exit(unlink(temp_dir, recursive = TRUE))
  result <- suppressMessages(write_eml(x, temp_dir))

  expect_identical(list.files(temp_dir), c("eml.xml"))
  expect_identical(
    result$dataset$title,
    paste(
      "Sample from: MICA - Muskrat and coypu camera trap observations in",
      "Belgium, the Netherlands and Germany"
    )
  )
  expect_type(result, "list")
  expect_invisible(suppressMessages(write_eml(x, temp_dir)))
})

test_that("write_eml() returns the expected eml.xml file for the example
           dataset", {
  skip_if_offline()
  x <- example_dataset()
  temp_dir <- file.path(tempdir(), "eml")
  on.exit(unlink(temp_dir, recursive = TRUE))
  result <- suppressMessages(write_eml(x, temp_dir))
  result$packageId <- "random_uuid" # Overwrite generated UUID

  expect_snapshot_file(
    file.path(temp_dir, "eml.xml"),
    transform = remove_uuid
  )
})

test_that("write_eml() supports disabling the derived paragraph", {
  skip_if_offline()
  x <- example_dataset()
  temp_dir <- file.path(tempdir(), "eml")
  on.exit(unlink(temp_dir, recursive = TRUE))
  result <- suppressMessages(write_eml(x, temp_dir, derived_paragraph = FALSE))

  expect_error(result$dataset$abstract$para[[2]]) # Subscript out of bounds
})

test_that("write_eml() sets contact/metadata provider to first creator if none
           have 'contact' role", {
  skip_if_offline()
  x <- example_dataset()
  # Remove contributor with role == contact
  x$contributors[[4]] <- NULL
  temp_dir <- file.path(tempdir(), "eml")
  on.exit(unlink(temp_dir, recursive = TRUE))
  result <- suppressMessages(write_eml(x, temp_dir))

  expect_identical(result$dataset$contact, result$dataset$creator[[1]])
  expect_identical(result$dataset$metadataProvider, result$dataset$creator[[1]])
})
