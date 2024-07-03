#' Get locations
#'
#' Gets the (unique) locations from the deployments of a Camera Trap Data
#' Package object.
#'
#' @inheritParams print
#' @return [tibble()] data frame with the locations, containing the following
#'   columns:
#'   - `locationID`
#'   - `locationName`
#'   - `latitude`
#'   - `longitude`
#'   - `coordinateUncertainty`
#' @family accessor functions
#' @export
#' @examples
#' x <- example_dataset()
#' locations(x)
locations <- function(x) {
  check_camtrapdp(x)
  deployments(x) %>%
    dplyr::distinct(
      .data$locationID,
      .data$locationName,
      .data$latitude,
      .data$longitude,
      .data$coordinateUncertainty
    )
}
