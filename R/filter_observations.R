#' Filter based on observations
#'
#' Filter a Camera Trap Data Package object based on observations. Same syntax
#' and behavior as in dplyr's `filter()` function. This function does not filter
#' deployments, so absences are retained. It does not filter media either
#' since a media file can be used by more than one observation and observations
#' are not always hard linked to media.
#'
#' @inheritParams check_camtrapdp
#' @param ... filtering expressions. Same behavior as dplyr's `filter()`.
#' @return Camera Trap Data Package object.
#' @family filter functions
#' @export
#' @examples
#' library(dplyr)
#' dataset <- example_dataset()
#' filter_observations(dataset, observationType == "animal")
#' filter_observations(
#'   dataset,
#'   scientificName %in% c("Anas platyrhynchos", "Ardea cinerea")
#' )
#' # piping is allowed
#' dataset %>%
#'   filter_observations(
#'   observationLevel == "event", observationType == "animal"
#' )
filter_observations <- function(x, ...) {
  check_camtrapdp(x)

  # Filter observations
  observations <-
    observations(x) %>%
    dplyr::filter(...)

  # Filter media
  select_media_ids <-
    observations %>%
    dplyr::filter(observationLevel == "media") %>%
    dplyr::distinct(.data$mediaID) %>%
    dplyr::pull()
  select_event_ids <-
    observations %>%
    dplyr::filter(observationLevel == "event") %>%
    dplyr::distinct(.data$eventID) %>%
    dplyr::pull()
  media <-
    media(x) %>%
    dplyr::filter(
      # On mediaID for media-based obs
      .data$mediaID %in% select_media_ids |
      # On eventID for event-based obs
      .data$eventID %in% select_event_ids
    )

  # Assign filtered data
  x$data$media <- media
  x$data$observations <- observations

  return(x)
}
