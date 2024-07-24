test_that("merge_camtrapdp() returns a valid camtrapdp object", {
  skip_if_offline()
  x1 <- example_dataset() %>%
    filter_deployments(deploymentID %in% c("00a2c20d", "29b7d356"))
  x2 <- example_dataset() %>%
    filter_deployments(deploymentID %in% c("577b543a", "62c200a9"))
  expect_no_error(
    check_camtrapdp(merge_camtrapdp(x1, x2, "new package name", "new title"))
  )
})

test_that("merge_camtrapdp() returns no duplicated deploymentID's", {
  skip_if_offline()
  x1 <- example_dataset()
  x2 <- example_dataset() %>%
    filter_deployments(deploymentID %in% c("00a2c20d"))
  original_deploymentIDs <- c(
    purrr::pluck(deployments(x1), "deploymentID"),
    purrr::pluck(deployments(x2), "deploymentID")
  )
  x_merged <-
    check_camtrapdp(merge_camtrapdp(x1, x1, "new package name", "new title"))
  new_deploymentIDs <- purrr::pluck(deployments(x_merged), "deploymentID")

  expect_true(any(duplicated(original_deploymentIDs)))
  expect_false(any(duplicated(new_deploymentIDs)))
})
