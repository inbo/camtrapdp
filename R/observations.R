#' Get observations
#'
#' Gets the observations from a Camera Trap Data Package object.
#'
#' @inheritParams version
#' @return `tibble()` data frame with observations.
#' @family accessor functions
#' @export
#' @examples
#' dataset <- example_dataset()
#' observations(dataset)
observations <- function(x) {
  check_camtrapdp(x)
  x$data$observations
}
