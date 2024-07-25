test_that("merge_camtrapdp() returns a valid camtrapdp object", {
  skip_if_offline()
  x1 <- example_dataset() %>%
    filter_deployments(deploymentID %in% c("00a2c20d", "29b7d356"))
  x2 <- example_dataset() %>%
    filter_deployments(deploymentID %in% c("577b543a", "62c200a9"))
  expect_no_error(
    suppressMessages(
      check_camtrapdp(
        merge_camtrapdp(x1, x2, "new_package_name", "new title")
        )
    )
  )
})

test_that("merge_camtrapdp() returns no duplicated deploymentID's", {
  skip_if_offline()
  x1 <- example_dataset() %>%
    filter_deployments(deploymentID %in% c("00a2c20d", "29b7d356", "577b543a"))
  x2 <- example_dataset() %>%
    filter_deployments(deploymentID %in% c("00a2c20d", "577b543a", "62c200a9"))
  original_deploymentIDs <- c(
    purrr::pluck(deployments(x1), "deploymentID"),
    purrr::pluck(deployments(x2), "deploymentID")
  )
  x_merged <- suppressMessages(
    merge_camtrapdp(x1, x2, "new_package_name", "new title")
  )
  new_deploymentIDs <- purrr::pluck(deployments(x_merged), "deploymentID")

  expect_true(any(duplicated(original_deploymentIDs)))
  expect_false(any(duplicated(new_deploymentIDs)))
})

test_that("merge_camtrapdp() returns message when ID's are replaced", {
  skip_if_offline()
  x1 <- example_dataset()
  x2 <- example_dataset() %>%
    filter_deployments(deploymentID %in% c("62c200a9")) %>%
    filter_media(mediaID %in% c("fb58a2b9", "0bb2566e", "a6a7a04c"))

  expect_message(
    merge_camtrapdp(x1, x2, "new_package_name", "new title")
    # regexp =  paste(
    #   "! `x1` and `x2` must have unique deploymentID's.",
    #   "`x1` and `x2` have duplicated deploymentID's: \"62c200a9\".",
    #   "Duplicated deploymentID's of `x2` are now replaced by \"07ce6950\" respectively.",
    #   "! `x1` and `x2` must have unique mediaID's.",
    #   "`x1` and `x2` have duplicated mediaID's: \"fb58a2b9\", \"0bb2566e\", and \"a6a7a04c\".",
    #   "Duplicated mediaID's of `x2` are now replaced by \"ba426f00\", \"8d5c0009\", and \"1689e0db\" respectively.",
    #   "! `x1` and `x2` must have unique observationID's.",
    #   "`x1` and `x2` have duplicated observationID's: \"a0431321\", \"fb58a2b9_1\", \"0bb2566e_1\", and \"a6a7a04c_1\".",
    #   "Duplicated observationID's of `x2` are now replaced by \"c6eeccc0\", \"a8452c14\", \"a48adc8a\", and \"b78e02ba\" respectively."
    #   )
  )
})
