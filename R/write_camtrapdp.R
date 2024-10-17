#' Write a Camera Trap Data Package to disk
#'
#' Writes a Camera Trap Data Package to disk as a datapackage.json and CSV
#' files.
#'
#' @inheritParams print.camtrapdp
#' @param directory Path to local directory to write files to.
#' @return
#' @export
#' @examples
#' x <- example_dataset()
#' x <- filter_deployments(x, deploymentID == "00a2c20d")
#' (write_camtrapdp(x, directory = "my_directory"))
#'
#' # Clean up (don't do this if you want to keep your files)
#' unlink("my_directory", recursive = TRUE)
write_camtrapdp <- function(x, directory) {
  check_camtrapdp(x)

  # Remove columns that were added by read_camtrapdp()
  media(x) <-
    media(x) %>%
    dplyr::select(-eventID)
  observations(x) <-
    observations(x) %>%
    dplyr::select(-dplyr::starts_with("taxon."))

  # Add resources
  resources <- c("deployments", "media", "observations")
  for (resource in resources) {
    schema <- purrr::keep(x$resources, ~ .x$name == resource)[[1]]$schema
    x <- frictionless::add_resource(
      x, resource, data = x$data[[resource]], replace = TRUE, schema = schema
      )
  }
  # Remove data
  x$data <- NULL

  frictionless::write_package(x, directory)
}
