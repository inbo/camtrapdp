#' Validate a Camera Trap Data Package
#'
#' @inheritParams print.camtrapdp
#' @return A list with 2 data frames for issues in deployments and observations
#' respectively.
#' Each data frame with one row for each issue and five columns:
#'   \item{deploymentID, observationID}{Identifier of the deployment or
#'   observation}
#'   \item{field}{Camera Trap Data Package field}
#'   \item{severity}{Severity of the issue}
#'   \item{type}{Issue type}
#'   \item{message}{Issue message}
#' @family validate functions
#' @export
#' @examples
#' x <- example_dataset()
#' validate_camtrapdp(x)
validate_camtrapdp <- function(x) {
  invalid_deployments <- validate_deployments(x)
  invalid_observations <- validate_observations(x)

  issues <- list(
    deployments = invalid_deployments,
    observations = invalid_observations
  )

  invisible(issues)
}

#' Validate deployments of a Camera Trap Data Package
#'
#' @inheritParams print.camtrapdp
#' @return Data frame with one row for each issue and five columns:
#'   \item{deploymentID}{Identifier of the deployment}
#'   \item{field}{Camera Trap Data Package field}
#'   \item{severity}{Severity of the issue}
#'   \item{type}{Issue type}
#'   \item{message}{Issue message}
#' @family validate functions
#' @noRd
#' @examples
#' x <- example_dataset()
#' validate_deployments(x)
validate_deployments <- function(x) {
  deployments <- deployments(x)

  coordinates_missing <-
    deployments |>
    dplyr::filter(is.na(latitude) | is.na(longitude)) |>
    dplyr::select(deploymentID) |>
    dplyr::mutate(
      field = "latitude/longitude",
      severity = "error",
      type = "required_field_missing",
      message = "coordinates are required"
    )

  timestamp_missing <-
    deployments |>
    dplyr::filter(is.na(deploymentStart) | is.na(deploymentEnd)) |>
    dplyr::select(deploymentID) |>
    dplyr::mutate(
      field = "deploymentStart/deploymentEnd",
      severity = "error",
      type = "required_field_missing",
      message = "deploymentStart and deploymentEnd are required"
    )

  invalid_deployments <-
    dplyr::bind_rows(coordinates_missing, timestamp_missing) |>
    dplyr::arrange(deploymentID)

  return(invalid_deployments)
}

#' Validate observations of a Camera Trap Data Package
#'
#' @inheritParams print.camtrapdp
#' @return Data frame with one row for each issue and five columns:
#'   \item{observationID}{Identifier of the observation}
#'   \item{field}{Camera Trap Data Package field}
#'   \item{severity}{Severity of the issue}
#'   \item{type}{Issue type}
#'   \item{message}{Issue message}
#' @family validate functions
#' @noRd
#' @examples
#' x <- example_dataset()
#' validate_observations(x)
validate_observations <- function(x) {
  observations <- observations(x)

  timestamp_missing <-
    observations |>
    dplyr::filter(is.na(eventStart) | is.na(eventEnd)) |>
    dplyr::select(observationID) |>
    dplyr::mutate(
      field = "eventStart/eventEnd",
      severity = "error",
      type = "required_field_missing",
      message = "eventStart and eventEnd are required"
    )

  count_missing <-
    observations |>
    dplyr::filter(!is.na(scientificName) & is.na(count)) |>
    dplyr::select(observationID) |>
    dplyr::mutate(
      field = "count",
      severity = "issue",
      type = "required_field_missing",
      message = "count is required when scientificName is provided"
    )

  count_invalid <-
    observations |>
    dplyr::filter(count == 0) |>
    dplyr::select(observationID) |>
    dplyr::mutate(
      field = "count",
      severity = "error",
      type = "invalid_value",
      message = "count must be greater than 0"
    )

  invalid_observations <-
    dplyr::bind_rows(
      timestamp_missing,
      count_missing,
      count_invalid
      ) |>
    dplyr::arrange(observationID)

  return(invalid_observations)
}
