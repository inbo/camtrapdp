#' Get observations
#'
#' Gets the observations from a Camera Trap Data Package object.
#'
#' @inheritParams version
#' @return `tibble()` data frame with observations.
#' @family accessor functions
#' @export
observations <- function(x) {
  x$data$observations
}
