test_that("media() returns a tibble", {
  skip_if_offline()
  x <- example_dataset()
  expect_s3_class(media(x), "tbl")
})

test_that("media() returns the media", {
  skip_if_offline()
  x <- example_dataset()
  expect_identical(media(x), x$data$media)
})

test_that("media<-() assigns a data frame (as tibble) as media", {
  skip_if_offline()
  x <- example_dataset()
  df <- data.frame(a = 1:3)
  media(x) <- df
  expect_identical(media(x), dplyr::as_tibble(df))
  expect_s3_class(media(x), "tbl")
})

test_that("media<-() returns error when value is not a data frame", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(
    media(x) <- "not_a_data_frame",
    class = "camtrapdp_error_assignment_wrong_class"
  )
})

test_that("filter_media() updates the taxonomic property as default", {
  skip_if_offline()
  x <- example_dataset()
  x_favorite <- filter_media(x, favorite == TRUE)
  remaining_taxa_obs <- unique(observations(x_favorite)$scientificName)
  remaining_taxa_tax <-
    purrr::map_chr(x_favorite$taxonomic, ~ purrr::pluck(.x, "scientificName"))
  expect_equal(remaining_taxa_obs, remaining_taxa_tax)

  x_filtered <-
    filter_media(x, captureMethod == "activityDetection", filePublic == FALSE)
  remaining_taxa_obs <-
    unique(observations(x_filtered)$scientificName) %>%
    sort()
  remaining_taxa_tax <-
    purrr::map_chr(x_filtered$taxonomic, ~ purrr::pluck(.x, "scientificName")) %>%
    sort()
  expect_equal(remaining_taxa_obs, remaining_taxa_tax)
})

test_that("filter_media() does not update the taxonomic property when update_metadata == FALSE", {
  skip_if_offline()
  x <- example_dataset()
  original_taxa <-
    purrr::map_chr(x$taxonomic, ~ purrr::pluck(.x, "scientificName")) %>%
    sort()
  x_favorite <- filter_media(x, favorite == TRUE, update_metadata = FALSE)
  remaining_taxa_tax <-
    purrr::map_chr(x_favorite$taxonomic, ~ purrr::pluck(.x, "scientificName"))
  expect_equal(original_taxa, remaining_taxa_tax)
})
