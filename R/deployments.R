#' Get deployments
#'
#' Gets the deployments from a Camera Trap Data Package object.
#'
#' @inheritParams version
#' @return `tibble()` data frame with deployments.
#' @family accessor functions
#' @export
#' @examples
#' dataset <- example_dataset()
#' deployments(dataset)
deployments <- function(x) {
  # TOOD: check_camtrapdp
  x$data$deployments
}
