#' Get locations of the deployments of a Camera Trap Data Package object
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
#'
locations <- function(x) {
  check(x)
  deployments(x) %>%
    dplyr::distinct(.data$locationID,
                    .data$locationName,
                    .data$latitude,
                    .data$longitude,
                    .data$coordinateUncertainty)
}
