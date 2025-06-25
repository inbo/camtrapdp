#' Write a Camera Trap Data Package to disk
#'
#' Writes a Camera Trap Data Package and its related Data Resources to disk as a
#' `datapackage.json` and CSV files.
#'
#' @inheritParams print.camtrapdp
#' @param directory Path to local directory to write files to.
#' @param ... Further arguments, passed to [frictionless::write_package()]
#'   (e.g. `compress = TRUE`).
#' @return `datapackage.json` and CSV files written to disk.
#' @family write functions
#' @export
#' @examples
#' x <- example_dataset()
#'
#' # Filter (and therefore change) the dataset
#' x <- filter_deployments(x, deploymentID == "00a2c20d")
#'
#' # Write the Camera Trap Data Package to disk
#' write_camtrapdp(x, directory = "my_directory")
#'
#' # Check files
#' list.files("my_directory")
#'
#' # Clean up (don't do this if you want to keep your files)
#' unlink("my_directory", recursive = TRUE)
write_camtrapdp <- function(x, directory, ...) {
  check_camtrapdp(x)

  # Remove columns and duplicates that were added by read_camtrapdp()
  media(x) <-
    media(x) %>%
    dplyr::select(-"eventID") %>%
    dplyr::distinct()
  # Do not use assign function for observations because it would update
  # x$taxonomic without the "taxon."-terms
  x$data$observations <-
    observations(x) %>%
    dplyr::select(-dplyr::starts_with("taxon."))

  # Update resources
  resource_names <- c("deployments", "media", "observations")
  for (resource_name in resource_names) {
    schema <- purrr::keep(x$resources, ~ .x$name == resource_name)[[1]]$schema
    x <- frictionless::add_resource(
      x,
      resource_name = resource_name,
      data = x$data[[resource_name]],
      schema = ,
      replace = TRUE,
      )
    # Hack to circumvent that add_resource() adds schema verbosely
    resource_index <- purrr::detect_index(x$resources, ~ .x$name == resource_name)
    x$resources[[resource_index]]$schema <- schema
  }

  # Remove data
  x$data <- NULL

  # Remove elements that are NA or empty list
  x$taxonomic <- clean_list(
    x$taxonomic,
    function(x) any(is.na(x)) || length(x) == 0L,
    recursive = TRUE
  )
  x$contributors <- clean_list(
    x$contributors,
    function(x) any(is.na(x)) || length(x) == 0L,
    recursive = TRUE
  )

  # Write files
  frictionless::write_package(x, directory, ...)

  # Return NULL rather than x, since x is no longer a valid camtrapdp object
  invisible(NULL)
}
