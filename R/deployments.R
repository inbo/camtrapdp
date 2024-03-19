#' Get deployments
#'
#' Gets the deployments from a Camera Trap Data Package object.
#'
#' @inheritParams version
#' @return Deployments tibble.
#' @family accessor functions
#' @export
deployments <- function(x) {
  # TOOD: check_camtrapdp
  x$data$deployments
}