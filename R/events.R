#' Get events
#'
#' Gets the (unique) events from the observations of a Camera Trap Data
#' Package object.
#' Only observations with `observationLevel == "event"` are considered.
#'
#' @inheritParams print
#' @return [tibble()] data frame with the events, containing the following
#'   columns:
#'   - `deploymentID`
#'   - `eventID`
#'   - `eventStart`
#'   - `eventEnd`
#' @family accessor functions
#' @export
#' @examples
#' x <- example_dataset()
#' events(x)
events <- function(x) {
  check_camtrapdp(x)
  observations(x) %>%
    dplyr::filter(.data$observationLevel == "event") %>%
    dplyr::distinct(
      .data$deploymentID,
      .data$eventID,
      .data$eventStart,
      .data$eventEnd
    )
}
