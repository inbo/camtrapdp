#' Get observations
#'
#' Gets the observations from a Camera Trap Data Package object.
#'
#' @inheritParams version
#' @return Observations tibble.
#' @family accessor functions
#' @export
observations <- function(x) {
  check_camtrapdp(x)
  x$data$observations
}
