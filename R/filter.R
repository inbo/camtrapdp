#' Define filter method for camtrapdp class
#'
#' Internal function for filtering deployments
#'
#' @inheritParams version
#'
#' @return Camera Trap Data Package object.
#' @family filter functions
#' @export
#' @examples
#' # example code
#' dataset <- example_dataset()
#' filter_deployments(dataset, locationName == "B_HS_val 2_processiepark")
filter_deployments <- function(x, ...) {

  # Check args validity
  # TODO

  # Filter deployments
  deploys <- deployments(x)
  deploys <- dplyr::filter(deploys, ...)

  obs <- observations(x) %>%
    dplyr::filter(.data$deploymentID %in% deploys$deploymentID)

  media <- media(x) %>%
    dplyr::filter(.data$deploymentID %in% deploys$deploymentID)

  # Assign filtered data to deployments, observations, media
  x$data$deployments <- deploys
  x$data$observations <- obs
  x$data$media <- media

  return(x)
}
