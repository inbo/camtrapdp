#' Get locations
#'
#' Extract unique locations of the deployments of a Camera Trap Data Package
#' object.
#'
#' @inheritParams version
#' @return `tibble()` data frame with the following columns:
#' - `locationID`
#' - `locationName`
#' - `latitude`
#' - `longitude`
#' - `coordinateUncertainty`
#'
#' @export
#' @examples
#' dataset <- example_dataset()
#' locations(dataset)
locations <- function(x) {
  # check_camtrapdp(x) # uncomment if check_camtrapdp() will exist
  deployments(x) %>%
    dplyr::distinct(.data$locationID,
                    .data$locationName,
                    .data$latitude,
                    .data$longitude,
                    .data$coordinateUncertainty)
}
