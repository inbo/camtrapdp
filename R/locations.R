#' Get locations
#'
#' Extract unique locations of the deployments of a Camera Trap Data Package
#' object.
#'
#' @param x a Camera Trap Data Package object.
#'
#' @return a tibble data.frame with the following columns:
#' - `locationID`
#' - `locationName`
#' - `latitude`
#' - `longitude`
#' - `coordinateUncertainty`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dataset <- read_camtrapdp("https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json")
#' locations(dataset)
#' }
locations <- function(x) {
  # check_camtrapdp(x) # uncomment if check_camtrapdp() will exist
  deployments(x) %>%
    dplyr::distinct(.data$locationID,
                    .data$locationName,
                    .data$latitude,
                    .data$longitude,
                    .data$coordinateUncertainty)
}
