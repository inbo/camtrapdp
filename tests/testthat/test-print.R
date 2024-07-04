test_that("print() returns output invisibly", {
  # No output
  expect_output(output <- withVisible(print(example_dataset())))
  # Returned object does not auto print
  expect_false(output$visible)
})

test_that("print() informs about the number of tables, their rows and unclass()", {
  x_no_additional <-
    example_dataset() %>%
    frictionless::remove_resource("individuals")

  # Default (additional resources removed)
  expect_output(
    print(x_no_additional),
    regexp = paste(
      "A Camera Trap Data Package with 3 tables:",
      "* deployments: 4 rows",
      "* media: 423 rows",
      "* observations: 549 rows",
      "Use `unclass()` to print the Data Package as a list.",
      sep = "\n"
    ),
    fixed = TRUE
  )

  # Extra table added
  x_no_additional$data$extra <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  expect_output(
    print(x_no_additional),
    regexp = "* extra: 2 rows",
    fixed = TRUE
  )
})

test_that("print() informs about additional resources and how to load these", {
  expect_output(
    print(example_dataset()),
    regexp = paste(
      "And 1 additional resource,",
      "which can be loaded with `frictionless::read_resource()`:"
    ),
    fixed = TRUE
  )
  expect_output(
    print(example_dataset()),
    regexp = "* individuals",
    fixed = TRUE
  )

  # See test above for no additional resources
})
