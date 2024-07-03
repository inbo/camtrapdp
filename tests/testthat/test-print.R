test_that("print() returns output invisibly", {
  # Test that there is any output at all
  expect_output(output <- withVisible(print(example_dataset())))
  # Test that the value (the returned object) does not auto print
  expect_false(output$visible)
})

test_that("print() informs about the number of resources", {

})

test_that("print() informs about the number of rows for every resource", {

})

test_that("print() can handle an input with non standard numer of resources", {

})

