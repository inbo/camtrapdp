#' Filter media
#'
#' Subsets media in a Camera Trap Data Package object, retaining all rows that
#' satisfy the conditions.
#'
#' - Deployments are not filtered.
#' - Observations are filtered on associated `mediaID` (for media-based
#' observations) and `eventID` (for event-based observations).
#'
#' @inheritParams print
#' @param ... Filtering conditions, see `dplyr::filter()`.
#' @return `x` filtered.
#' @family filter functions
#' @export
#' @examples
#' x <- example_dataset()
#'
#' # Filtering returns x, so pipe with media() to see the result
#' x %>%
#'   filter_media(captureMethod == "timeLapse") %>%
#'   media()
#'
#' # Filtering on media also affects associated observations, but not deployments
#' x_filtered <- filter_media(x, favorite == TRUE)
#' observations(x_filtered)
#'
#' # Filtering on multiple conditions (combined with &)
#' x %>%
#'   filter_media(captureMethod == "activityDetection", filePublic == FALSE) %>%
#'   media()
#'
#' # Filtering on datetimes is easiest with lubridate
#' library(lubridate, warn.conflicts = FALSE)
#' x %>%
#'   filter_media(
#'     timestamp >= lubridate::as_datetime("2020-08-02 05:01:00"),
#'     timestamp <= lubridate::as_datetime("2020-08-02 05:02:00")
#'   ) %>%
#'   media()
filter_media <- function(x, ...) {
  check_camtrapdp(x)

  # Filter media
  media <-
    media(x) %>%
    dplyr::filter(...)

  # Filter observations
  select_media_ids <- unique(purrr::pluck(media, "mediaID"))
  select_event_ids <- unique(purrr::pluck(media, "eventID"))
  observations <-
    observations(x) %>%
    dplyr::filter(
      # On mediaID for media-based obs
      (.data$observationLevel == "media" & .data$mediaID %in% select_media_ids) |
      # On eventID for event-based obs
      (.data$observationLevel == "event" & .data$eventID %in% select_event_ids)
    )

  # Assign filtered data
  media(x) <- media
  observations(x) <- observations

  return(x)
}
