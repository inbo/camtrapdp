test_that("print() returns output invisibly", {
  # Test that there is any output at all
  expect_output(output <- withVisible(print(example_dataset())))
  # Test that the value (the returned object) does not auto print
  expect_false(output$visible)
})

test_that("print() informs about the number of resources", {
  # Test that information is provided about the number of resources
  expect_output(
    print(example_dataset()),
    regexp = "A Data Package with [0-9]+ resources:",
    fixed = FALSE
  )
  # Test for 5 resources
  five_resources <- example_dataset()
  purrr::pluck(five_resources, "resources") <-
    sample(five_resources$resources, 5, replace = TRUE)

  expect_output(
    print(five_resources),
    regexp = "A Data Package with 5 resources:",
    fixed = TRUE
  )
  # Test 0 resources
  zero_resources <- example_dataset()
  purrr::pluck(zero_resources, "resources") <- list()

  expect_output(
    print(five_resources),
    regexp = "A Data Package with 0 resources:",
    fixed = TRUE
  )
})

test_that("print() informs about the number of rows for every resource", {
  # Test that information is provided about the number of rows per resource
  expect_output(
    print(example_dataset()),
    regexp = "deployments: [0-9]+",
    fixed = FALSE
  )

})

test_that("print() can handle an input with non standard numer of resources", {

})

