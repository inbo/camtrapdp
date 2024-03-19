#' Get locations of the deployments of a Camtrap DP object
#'
#' Extract unique locations of the deployments of a Camptrap DP object.
#'
#' @param x a Camtrap DP object
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
