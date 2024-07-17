#' Corrects date-time
#'
#' Corrects date-times in a Camera Trap Data package object. This function can
#' be used when the time settings of one or more deployments were wrong and need
#' to be corrected afterwards.
#'
#' @inheritParams print.camtrapdp
#' @param deploymentID One or more deploymentID's.
#' @param duration Date-time difference between the wrong and right time.
#' @return `x` with all date-times corrected.
#' @family transformation functions
#' @export
#' @examples
#' x <- example_dataset()
#' deploymentID <- c("00a2c20d", "29b7d356")
#' # calculate duration
#' library(lubridate)
#' wrong <- ymd_hms("2024-04-01T00:00:00", tz = "UTC")
#' right <- ymd_hms("2024-04-01T02:00:00", tz = "UTC")
#' duration <- as.duration(interval(wrong, right))
#' # correct time
#' x_corrected <- correct_time(x, deploymentID, duration)
#' # inspect results
#' deployments(x)
#' deployments(x_corrected)
correct_time <- function(x, deploymentID, duration) {
  check_camtrapdp(x)

  # correct deploymentStart and deploymentEnd of selected deployments
  deployments(x) <-
    deployments(x) %>%
    dplyr::mutate(
      deploymentStart =
        dplyr::if_else(
          deploymentID %in% deploymentID,
          deploymentStart + duration,
          deploymentStart
        ),
      deploymentEnd =
        dplyr::if_else(
          deploymentID %in% deploymentID,
          deploymentEnd + duration,
          deploymentEnd
        )
    )

  # correct eventStart and eventEnd of associated observations
  observations(x) <-
    observations(x) %>%
    dplyr::mutate(
      eventStart =
        dplyr::if_else(
          deploymentID %in% deploymentID,
          eventStart + duration,
          eventStart
        ),
      eventEnd =
        dplyr::if_else(
          deploymentID %in% deploymentID,
          eventEnd + duration,
          eventEnd
        )
    )

  # correct timestamp of associated media
  media(x) <-
    media(x) %>%
    dplyr::mutate(
      timestamp =
        dplyr::if_else(
          deploymentID %in% deploymentID,
          timestamp + duration,
          timestamp
        )
    )

  return(x)
}
