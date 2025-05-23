test_that("contributors() returns a tibble", {
  skip_if_offline()
  x <- example_dataset()
  expect_s3_class(contributors(x), "tbl")
})

test_that("contributors() returns the expected columns (even when absent)", {
  skip_if_offline()
  x <- example_dataset()
  contributors(x) <- data.frame()
  expect_identical(
    names(contributors(x)),
    c("title", "firstName", "lastName", "email", "path", "role", "organization")
  )
})

test_that("contributors() creates firstName and lastName as expected", {
  skip_if_offline()
  x <- example_dataset()
  contributors(x) <- data.frame(
    title = c(
      "first_name last_name",
      "first_name multiple last names",
      "one_name",
      "a_publisher",
      "a_rightsholder"
    ),
    role = c(
      "contact",
      "contributor",
      "principal investigator",
      "publisher",
      "rightsholder"
    )
  )
  expect_identical(
    contributors(x)$firstName,
    c(
      "first_name",
      "first_name",
      NA_character_,
      NA_character_,
      NA_character_
    )
  )
  expect_identical(
    contributors(x)$lastName,
    c(
      "last_name",
      "multiple last names",
      NA_character_,
      NA_character_,
      NA_character_
    )
  )
})

test_that("contributors()<- assigns a dataframe as list to x$contributors", {
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
  expect_type(x$contributors, "list")
  expect_identical(dplyr::as_tibble(df), contributors(x))
})

test_that("contributors()<- returns error when value is not a data frame", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(
    contributors(x) <- "not_a_data_frame",
    class = "camtrapdp_error_assignment_wrong_class"
  )
})
