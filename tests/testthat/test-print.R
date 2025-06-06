test_that("print() returns output invisibly", {
  skip_if_offline()
  # No output
  expect_output(output <- withVisible(print(example_dataset())))
  # Returned object does not auto print
  expect_false(output$visible)
})

test_that("print() informs about the number of tables, their rows and unclass()", {
  skip_if_offline()
  x_no_additional <-
    example_dataset() %>%
    frictionless::remove_resource("individuals")

  # Default (additional resources removed)
  expect_output(
    print(x_no_additional),
    regexp = paste(
      "A Camera Trap Data Package \"camtrap-dp-example-dataset\" with 3 tables:",
      "* deployments: 4 rows",
      "* media: 423 rows",
      "* observations: 549 rows",
      "Use `unclass()` to print the Data Package as a list.",
      sep = "\n"
    ),
    fixed = TRUE
  )

  # Extra table added
  x_no_additional$data$extra <-
    data.frame(
      "col_1" = c(1, 2),
      "col_2" = c("a", "b"),
      stringsAsFactors = FALSE
    )
  expect_output(
    print(x_no_additional),
    regexp = "* extra: 2 rows",
    fixed = TRUE
  )
})

test_that("print() informs about additional resources", {
  skip_if_offline()
  expect_output(
    print(example_dataset()),
    regexp = paste(
      "And 1 additional resource:",
      "* individuals",
      sep = "\n"
    ),
    fixed = TRUE
  )

  # See test above for no additional resources
})
