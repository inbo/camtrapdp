#' Corrects datetime
#'
#' Corrects all datetimes in a Camera Trap Data package object. This function
#' can be used when the time settings of one or more deployments were wrong and
#' need to be corrected afterwards.
#'
#' @inheritParams print.camtrapdp
#' @param deploymentID One or more deploymentID's.
#' @param duration Datetime difference between the wrong and right time.
#' @return `x` with all datetimes corrected.
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

  # deploymentID is valid
  if (any(!deploymentID %in% c(deployments(x)$deploymentID))) {
    cli::cli_abort(
      c(
        "{.arg deploymentID} is not a valid deploymentID of {.arg x}:",
        "i" = "{.arg deploymentID} is {.val {deploymentID}}",
        "i" = paste("Valid deploymentIDs of {.arg x} are ",
              "{.val {deployments(x)$deploymentID}}.")
      ),
      class = "camtrapdp_error_deploymentID_invalid"
    )
  }

  # duration is valid
  if (!inherits(duration, c("Duration", "POSIXct", "POSIXt"))) {
    cli::cli_abort(
      c(
        "{.arg duration} is not a valid datetime object:",
        "i" = "{.arg duration} has class {.val {class(duration)}}.",
        "i" = "The class of {.arg duration} must be Duration, POSIXct or POSIXt."
      ),
      class = "camtrapdp_error_duration_invalid"
    )
  }

  # hack to solve name problem with deploymentID in mutate
  depID <- deploymentID

  # correct deploymentStart and deploymentEnd of selected deployments
  deployments(x) <-
    deployments(x) %>%
    dplyr::mutate(
      deploymentStart =
        dplyr::if_else(
          .data$deploymentID %in% depID,
          .data$deploymentStart + duration,
          .data$deploymentStart
        ),
      deploymentEnd =
        dplyr::if_else(
          .data$deploymentID %in% depID,
          .data$deploymentEnd + duration,
          .data$deploymentEnd
        )
    )

  # correct eventStart and eventEnd of associated observations
  observations(x) <-
    observations(x) %>%
    dplyr::mutate(
      eventStart =
        dplyr::if_else(
          .data$deploymentID %in% depID,
          .data$eventStart + duration,
          .data$eventStart
        ),
      eventEnd =
        dplyr::if_else(
          .data$deploymentID %in% depID,
          .data$eventEnd + duration,
          .data$eventEnd
        )
    )

  # correct timestamp of associated media
  media(x) <-
    media(x) %>%
    dplyr::mutate(
      timestamp =
        dplyr::if_else(
          .data$deploymentID %in% depID,
          .data$timestamp + duration,
          .data$timestamp
        )
    )

  # set timestamIssues to FALSE
  deployments(x)$timestampIssues <- FALSE

  return(x)
}
