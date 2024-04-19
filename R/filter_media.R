#' Filter based on media
#'
#' Filter a Camera Trap Data Package object based on media. Same syntax and
#' behavior as in dplyr's `filter()` function. This function does not filter
#' deployments, so absences are retained. It filters observations (on
#' `mediaID`), but note that not all observations have these. It filters also
#' observations taken by the same deployments and same time range as the
#' filtered media.
#'
#' @inheritParams version
#' @param ... filtering expressions. Same behavior as dplyr's `filter()`.
#' @return Camera Trap Data Package object.
#' @family filter functions
#' @export
#' @examples
#' library(dplyr)
#' dataset <- example_dataset()
#' filter_media(dataset, favorite == TRUE)
#'
#' # piping is allowed
#' dataset %>%
#'   filter_media(
#'     timestamp > as.POSIXct("2020-08-04 08:01:00")
#' )
filter_media <- function(x, ...) {
  # Filter media
  media <- media(x)
  media <- dplyr::filter(media, ...)

  # Filters observations on `mediaID`
  obs <- observations(x) %>%
    dplyr::filter(.data$mediaID %in% media$mediaID)

  # Filter observations based on deploymentID and timestamp
  deploys <- unique(media$deploymentID)
  obs <- obs %>%
    dplyr::filter(.data$deploymentID %in% deploys)

  # Filter observations with eventStart <= timestamp <= eventEnd
  # where timestamp is any of the media timestamps
  obs <- obs %>%
    dplyr::rowwise() %>%
    dplyr::mutate(is_media_present = any(
      .data$eventStart <= media$timestamp &
        media$timestamp <= .data$eventEnd)) %>%
    dplyr::filter(.data$is_media_present == TRUE) %>%
    dplyr::select(-"is_media_present")

  # Assign filtered data
  x$data$media <- media
  x$data$observations <- obs

  return(x)
}
