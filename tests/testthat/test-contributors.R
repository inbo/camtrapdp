test_that("contributors() returns a tibble", {
  skip_if_offline()
  x <- example_dataset()
  expect_s3_class(contributors(x), "tbl")
})

test_that("contributors()<- returns error when value is not a data frame", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(
    contributors(x) <- "not_a_data_frame",
    class = "camtrapdp_error_assignment_wrong_class"
  )
})

  )
})

test_that("contributors()<- assigns a dataframe as list to contributors", {
  skip_if_offline()
  x <- example_dataset()
  df <- data.frame(
    title = c("one", "two", "three"),
    firstName = 1:3,
    lastName = 1:3,
    email = 1:3,
    path = 1:3,
    role = 1:3,
    organization = 1:3)
  contributors(x) <- df
  expect_identical(class(x$contributors), "list")
  expect_identical(dplyr::as_tibble(df), contributors(x))
})

test_that("contributors() returns the expected columns", {
  skip_if_offline()
  x <- example_dataset()
  df <- data.frame(title = c("one", "two", "three"))
  contributors(x) <- df
  names_contributors <-
    c("title", "firstName", "lastName", "email", "path", "role", "organization")
  expect_identical(names(contributors(x)), names_contributors)
})

test_that("contributors() creates firstName and lastName as expected", {
  skip_if_offline()
  x <- example_dataset()
  contributors(x) <-
    data.frame(
      title = c(
        "first_name last_name", "one_name", "publisher",
        "rightsholder", "multiple_names_first multiple names"
      ),
      role = c(
        "contact", "principal investigator", "publisher",
        "rightsholder", "contributor"
      )
    )
  expect_identical(
    contributors(x)$firstName,
    c(
      "first_name", NA_character_, NA_character_,
      NA_character_, "multiple_names_first"
      )
  )
  expect_identical(
    contributors(x)$lastName,
    c(
      "last_name", NA_character_, NA_character_,
      NA_character_, "multiple names"
      )
  )
})
