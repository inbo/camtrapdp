#' Correct datetime
#'
#' Corrects datetimes in a Camera Trap Data package object. Datetimes in
#' deployments and associated resources and metadata are corrected for the
#' the selected deploymentIDs. `timestampIssues` is set to FALSE.
#' This function can be used when the time settings of one or more deployments
#' are wrong and need to be corrected.
#'
#' @inheritParams print.camtrapdp
#' @param deploymentID One or more deploymentID's, either as a character string
#' or as a vector of character strings.
#' @param duration Datetime difference between the wrong and right time. An
#'   object of class [lubridate::Duration-class] or [difftime].
#' @return `x` with datetimes corrected.
#' @family transformation functions
#' @export
#' @examples
#' x <- example_dataset()
#'
#' # Deployment IDs with wrong timestamps
#' deploymentID <- c("00a2c20d", "29b7d356")
#'
#' # Calculate duration between wrong and right time
#' library(lubridate)
#' wrong <- ymd_hms("2024-04-01T00:00:00", tz = "UTC")
#' right <- ymd_hms("2024-04-01T02:00:00", tz = "UTC")
#' duration <- as.duration(interval(wrong, right))
#'
#' # Correct time
#' x_corrected <- correct_time(x, deploymentID, duration)
#'
#' # Inspect results
#' deployments(x)
#' deployments(x_corrected)
correct_time <- function(x, deploymentID, duration) {
  check_camtrapdp(x)

  deployment_ids <- purrr::pluck(deployments(x), "deploymentID")

  # deploymentID is valid
  if (any(!deploymentID %in% deployment_ids)) {
    cli::cli_abort(
      c(
        "{.arg deploymentID} is not a valid deploymentID of {.arg x}:",
        "i" = "{.arg deploymentID} is {.val {deploymentID}}",
        "i" = paste("Valid deploymentIDs of {.arg x} are ",
              "{.val {deployment_ids}}.")
      ),
      class = "camtrapdp_error_deploymentID_invalid"
    )
  }

  # duration is valid
  if (!inherits(duration, c("Duration", "difftime"))) {
    cli::cli_abort(
      c(
        "{.arg duration} is not a valid datetime interval object:",
        "i" = "{.arg duration} has class {.val {class(duration)}}.",
        "i" = "The class of {.arg duration} must be `Duration` (lubridate) or `difftime` (base)."
      ),
      class = "camtrapdp_error_duration_invalid"
    )
  }

  # warning: duplicated deploymentID's
  if (any(duplicated(deploymentID))) {
    cli::cli_warn(
      c(
        paste(
          "{.arg deploymentID} has duplicated values:",
          "{.val {deploymentID[duplicated(deploymentID)]}}"
        )
      ),
      class = "camtrapdp_warning_deploymentID_duplicated"
    )
  }

  # Get wrong deploymentStart of the first deploymentID (used in final message)
  wrong_deploymentStart <-
    deployments(x) %>%
    dplyr::filter(.data$deploymentID == {{ deploymentID }}[1]) %>%
    dplyr::pull(.data$deploymentStart)

  # Correct deploymentStart and deploymentEnd of selected deployments
  deployments(x) <-
    deployments(x) %>%
    dplyr::mutate(
      deploymentStart =
        dplyr::if_else(
          .data$deploymentID %in% {{ deploymentID }},
          .data$deploymentStart + duration,
          .data$deploymentStart
        ),
      deploymentEnd =
        dplyr::if_else(
          .data$deploymentID %in% {{ deploymentID }},
          .data$deploymentEnd + duration,
          .data$deploymentEnd
        )
    )

  # Correct eventStart and eventEnd of associated observations
  observations(x) <-
    observations(x) %>%
    dplyr::mutate(
      eventStart =
        dplyr::if_else(
          .data$deploymentID %in% {{ deploymentID }},
          .data$eventStart + duration,
          .data$eventStart
        ),
      eventEnd =
        dplyr::if_else(
          .data$deploymentID %in% {{ deploymentID }},
          .data$eventEnd + duration,
          .data$eventEnd
        )
    )

  # Correct timestamp of associated media
  media(x) <-
    media(x) %>%
    dplyr::mutate(
      timestamp =
        dplyr::if_else(
          .data$deploymentID %in% {{ deploymentID }},
          .data$timestamp + duration,
          .data$timestamp
        )
    )

  # Set timestamIssues to FALSE
  purrr::pluck(deployments(x), "timestampIssues") <- FALSE

  # Update temporal scope
  x <- update_temporal(x)

  # Get updated eventStart of the first deploymentID (used in final message)
  updated_deploymentStart <-
    deployments(x) %>%
    dplyr::filter(.data$deploymentID == {{ deploymentID }}[1]) %>%
    dplyr::pull(.data$deploymentStart)

  cli::cli_alert_success(
    "Timestamps in selected deployments, media and observations were shifted by
    {.val {duration}} (e.g. {.val {wrong_deploymentStart}} is now
    {.val {updated_deploymentStart}}).",
    class = "camtrapdp_info_correct_time"
  )

  return(x)
}
