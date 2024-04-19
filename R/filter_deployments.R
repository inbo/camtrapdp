#' Filter based on deployments
#'
#' Filter a Camera Trap Data Package object based on deployments. Same syntax
#' and behavior as in dplyr's `filter()` function. The deployments will be filtered based on the provided conditions.
#' The media and the observations of the removed deployments will be removed as well.
#'
#' @inheritParams version
#'
#' @return Camera Trap Data Package object.
#' @family filter functions
#' @export
#' @examples
#' library(dplyr)
#' dataset <- example_dataset()
#' filter_deployments(dataset, locationName == "B_HS_val 2_processiepark")
#' filter_deployments(dataset, locationName != "B_HS_val 2_processiepark")
#' filter_deployments(dataset, deploymentStart > as.POSIXct("2021-01-01"))
#' # piping is allowed
#' dataset %>% filter_deployments(latitude > 51, longitude < 5)
filter_deployments <- function(x, ...) {

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
