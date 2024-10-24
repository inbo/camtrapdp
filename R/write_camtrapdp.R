#' Write a Camera Trap Data Package to disk
#'
#' Writes a Camera Trap Data Package and its related Data Resources to disk as a
#' `datapackage.json` and CSV files.
#' Already existing CSV files of the same name will not be overwritten.
#' The function can also be used to download a Camera Trap Data Package in its
#' entirety.
#'
#' @inheritParams print.camtrapdp
#' @param directory Path to local directory to write files to.
#' @return `x` invisibly, as written to file.
#' @family write functions
#' @export
#' @examples
#' x <- example_dataset()
#'
#' # Change Data Package by filtering
#' x <- filter_deployments(x, deploymentID == "00a2c20d")
#'
#' # Write the Camera Trap Data Package to disk
#' (write_camtrapdp(x, directory = "my_directory"))
#'
#' # Check files
#' list.files("my_directory")
#'
#' # Clean up (don't do this if you want to keep your files)
#' unlink("my_directory", recursive = TRUE)
write_camtrapdp <- function(x, directory) {
  check_camtrapdp(x)

  # Update spatial, temporal and taxonomic scope in metadata
  x <- x %>%
    update_spatial() %>%
    update_temporal() %>%
    update_taxonomic()

  # Remove columns that were added by read_camtrapdp()
  media(x) <-
    media(x) %>%
    dplyr::select(-"eventID")
  # Do not use assign function for observations because it would update
  # x$taxonomic without the "taxon."-terms
  x$data$observations <-
    observations(x) %>%
    dplyr::select(-dplyr::starts_with("taxon."))

  # Add resources
  resources <- c("deployments", "media", "observations")
  for (resource in resources) {
    schema <- purrr::keep(x$resources, ~ .x$name == resource)[[1]]$schema
    x <- frictionless::add_resource(
      x, resource, data = x$data[[resource]], replace = TRUE, schema = schema
      )
    # Hack to circumvent that add_resource() adds schema verbosely
    resource_index <- purrr::detect_index(x$resources, ~ .x$name == resource)
    x$resources[[resource_index]]$schema <- schema
  }

  # Remove data
  x$data <- NULL

  frictionless::write_package(x, directory)
}
