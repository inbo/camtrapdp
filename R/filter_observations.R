#' Filter observations
#'
#' Subsets observations in a Camera Trap Data Package object, retaining all rows
#' that satisfy the conditions.
#'
#' - Deployments are not filtered.
#' - Media are filtered on associated `mediaID` (for media-based observations)
#' and `eventID` (for event-based observations).
#' Filter on `observationLevel == "media"` to only retain directly linked media.
#'
#' @inheritParams check_camtrapdp
#' @param ... Filtering conditions, see `dplyr::filter()`.
#' @return Filtered Camera Trap Data Package object.
#' @family filter functions
#' @export
#' @examples
#' x <- example_dataset()
#'
#' # Filtering returns x, so pipe with observations() to see the result
#' x %>%
#'   filter_observations(observationType == "animal") %>%
#'   observations()
#'
#' # Filtering on observations also affects associated media, but not deployments
#' x %>%
#'   filter_observations(scientificName == "Vulpes vulpes", observationLevel == "event") %>%
#'   media()
#' x %>%
#'   filter_observations(scientificName == "Vulpes vulpes", observationLevel == "media") %>%
#'   media()
#'
#' # Filtering on multiple conditions (combined with &)
#' x %>%
#'   filter_observations(
#'     deploymentID == "577b543a",
#'     scientificName %in% c("Martes foina", "Mustela putorius")
#'   ) %>%
#'   observations()
#'
#' # Filtering on datetimes is easiest with lubridate
#' library(lubridate, warn.conflicts = FALSE)
#' x %>%
#'   filter_observations(
#'     eventStart >= lubridate::as_datetime("2020-06-19 22:00:00"),
#'     eventEnd <= lubridate::as_datetime("2020-06-19 22:10:00")
#'   ) %>%
#'   observations()
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
