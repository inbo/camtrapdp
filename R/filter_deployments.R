#' Filter deployments
#'
#' Subsets deployments in a Camera Trap Data Package object, retaining all
#' rows that satisfy a condition.
#' - Deployments are filtered on the conditions.
#' - Media are filtered on associated `deploymentID`.
#' - Observations are filtered on associated `deploymentID`.
#'
#' @inheritParams check_camtrapdp
#' @param ... Filtering conditions, see `dplyr::filter()`.
#' @return Camera Trap Data Package object.
#' @family filter functions
#' @export
#' @examples
#' x <- example_dataset()
#'
#' # Filtering returns x (the dataset)
#' filter_deployments(x, deploymentID == "62c200a9")
#'
#' # Use deployments() to get the filtered deployments
#' x %>%
#'   filter_deployments(deploymentID == "62c200a9") %>%
#'   deployments()
#'
#' # Filtering by multiple criteria
#' x %>%
#'   filter_deployments(latitude > 51.0 & longitude > 5.0) %>%
#'   deployments()
#'
#' # Associated media and observations are filtered as well
#' x_filtered <- filter_deployments(x, deploymentID == "62c200a9")
#' media(x_filtered)
#' observations(x_filtered)
filter_deployments <- function(x, ...) {
  check_camtrapdp(x)

  # Filter data
  deployments <-
    deployments(x) %>%
    dplyr::filter(...)
  media <-
    media(x) %>%
    dplyr::filter(.data$deploymentID %in% deployments$deploymentID)
  observations <-
    observations(x) %>%
    dplyr::filter(.data$deploymentID %in% deployments$deploymentID)

  # Assign filtered data
  x$data$deployments <- deployments
  x$data$media <- media
  x$data$observations <- observations

  return(x)
}
