#' Filter deployments
#'
#' @description
#' Subsets deployments in a Camera Trap Data Package object, retaining all
#' rows that satisfy the conditions.
#'
#' - Media are filtered on associated `deploymentID`.
#' - Observations are filtered on associated `deploymentID`.
#' - Metadata (`x$spatial`, `x$temporal` and `x$taxonomic`) are updated to match
#' the filtered deployments.
#'
#' @inheritParams print.camtrapdp
#' @param ... Filtering conditions, see `dplyr::filter()`.
#' @return `x` filtered.
#' @family filter functions
#' @export
#' @examples
#' x <- example_dataset()
#'
#' # Filtering returns x, so pipe with deployments() to see the result
#' x %>%
#'   filter_deployments(deploymentID == "62c200a9") %>%
#'   deployments()
#'
#' # Filtering on deployments also affects associated media and observations
#' x_filtered <- filter_deployments(x, deploymentID == "62c200a9")
#' media(x_filtered)
#' observations(x_filtered)
#'
#' # Filtering on multiple conditions (combined with &)
#' x %>%
#'   filter_deployments(latitude > 51.0, longitude > 5.0) %>%
#'   deployments()
#'
#' # Filtering on dates is easiest with lubridate
#' library(lubridate, warn.conflicts = FALSE)
#' x %>%
#'   filter_deployments(
#'     deploymentStart >= lubridate::as_date("2020-06-19"),
#'     deploymentEnd <= lubridate::as_date("2020-08-30")
#'   ) %>%
#'   deployments()
filter_deployments <- function(x, ...) {
  check_camtrapdp(x)

  # Filter deployments
  deployments <-
    deployments(x) %>%
    dplyr::filter(...)

  # Filter media
  media <-
    media(x) %>%
    dplyr::filter(.data$deploymentID %in% deployments$deploymentID)

  # Filter observations
  observations <-
    observations(x) %>%
    dplyr::filter(.data$deploymentID %in% deployments$deploymentID)

  # Assign filtered data (also updates spatial, temporal and taxonomic scope)
  deployments(x) <- deployments
  media(x) <- media
  observations(x) <- observations

  return(x)
}
