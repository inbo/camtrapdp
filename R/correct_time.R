#' Shift date-times
#'
#' @description
#' Shifts date-times for selected deployments (and associated media and
#' observations) by a specified duration.
#' This function can be used to correct date-time issues such as incorrectly
#' set time zones.
#'
#' - Deployments: `deploymentStart` and `deploymentEnd` are updated and
#'   `timestampIssues` is set to `FALSE`.
#' - Media: `timestamp` is updated.
#' - Observations: `eventStart` and `eventEnd` are updated.
#' - Metadata (`x$temporal`) are updated to match the new temporal scope.
#'
#' @inheritParams print.camtrapdp
#' @param deployment_id One or more deploymentIDs.
#' @param duration Difference between the current and new date-times.
#'   Provide as a [lubridate::duration()] or [difftime].
#' @return `x` with shifted date-times.
#' @family transformation functions
#' @export
#' @examples
#' # Set desired duration between current and new date-times (e.g. 4 hours earlier)
#' library(lubridate, warn.conflicts = FALSE)
#' duration(-4, units = "hours")
#'
#' # Or calculate one based on two date-times
#' current <- ymd_hms("2024-04-01T04:00:00", tz = "UTC")
#' new <- ymd_hms("2024-04-01T00:00:00", tz = "UTC")
#' duration <- as.duration(interval(current, new))
#'
#' # Shift date-times for 2 deployments
#' x <- example_dataset()
#' x_shifted <- correct_time(x, c("00a2c20d", "29b7d356"), duration)
#'
#' # Inspect results
#' deployments(x)[, c("deploymentID", "deploymentStart", "deploymentEnd")]
#' deployments(x_shifted)[, c("deploymentID", "deploymentStart", "deploymentEnd")]
correct_time <- function(x, deployment_id, duration) {
  check_camtrapdp(x)

  # Remove duplicate values in deployment_id
  deployment_id <- unique(deployment_id)

  # Check provided deployment_id(s)
  valid_deployment_ids <- purrr::pluck(deployments(x), "deploymentID")
  invalid_deployment_ids <- setdiff(deployment_id, valid_deployment_ids)
  if (length(invalid_deployment_ids) > 0) {
    cli::cli_abort(
      "Can't find deployment{?s} with deploymentID
       {.val {invalid_deployment_ids}}.",
      class = "camtrapdp_error_deployment_id_invalid"
    )
  }

  # Check duration is valid
  if (!inherits(duration, c("Duration", "difftime"))) {
    cli::cli_abort(
      c(
        "{.arg duration} must be a duration, created with
         {.fun lubridate::duration} or {.fun difftime}.",
        "x" = "{.val {duration}} is {.type {duration}}."
      ),
      class = "camtrapdp_error_duration_invalid"
    )
  }

  # Get current deploymentStart of first selected deployment (used in message)
  current_datetime <-
    deployments(x) %>%
    dplyr::filter(.data$deploymentID == {{ deployment_id }}[1]) %>%
    dplyr::pull(.data$deploymentStart)

  # Shift deploymentStart and deploymentEnd of selected deployments
  # And set timestampIssues to FALSE
  deployments(x) <-
    deployments(x) %>%
    dplyr::mutate(
      deploymentStart = dplyr::if_else(
        .data$deploymentID %in% {{ deployment_id }},
        .data$deploymentStart + duration,
        .data$deploymentStart
      ),
      deploymentEnd = dplyr::if_else(
        .data$deploymentID %in% {{ deployment_id }},
        .data$deploymentEnd + duration,
        .data$deploymentEnd
      ),
      timestampIssues = dplyr::if_else(
        .data$deploymentID %in% {{ deployment_id }},
        FALSE,
        .data$timestampIssues
      )
    )

  # Shift timestamp of associated media
  media(x) <-
    media(x) %>%
    dplyr::mutate(
      timestamp = dplyr::if_else(
        .data$deploymentID %in% {{ deployment_id }},
        .data$timestamp + duration,
        .data$timestamp
      )
    )

  # Shift eventStart and eventEnd of associated observations
  observations(x) <-
    observations(x) %>%
    dplyr::mutate(
      eventStart = dplyr::if_else(
        .data$deploymentID %in% {{ deployment_id }},
        .data$eventStart + duration,
        .data$eventStart
      ),
      eventEnd = dplyr::if_else(
        .data$deploymentID %in% {{ deployment_id }},
        .data$eventEnd + duration,
        .data$eventEnd
      )
    )

  # Update temporal scope
  x <- update_temporal(x)

  # Get new deploymentStart of first selected deployment (used in message)
  new_datetime <-
    deployments(x) %>%
    dplyr::filter(.data$deploymentID == {{ deployment_id }}[1]) %>%
    dplyr::pull(.data$deploymentStart)

  # Return message
  cli::cli_alert_success(
    "Date-times in selected deployments, media and observations were shifted by
     {.val {duration}}. E.g. {.val {current_datetime}} is now
     {.val {new_datetime}}.",
    # wrap = TRUE,
    class = "camtrapdp_message_shift_time"
  )

  return(x)
}
