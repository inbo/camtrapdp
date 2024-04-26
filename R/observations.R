#' Get observations
#'
#' Gets the observations from a Camera Trap Data Package object.
#'
#' @inheritParams check_camtrapdp
#' @return [tibble()] data frame with observations.
#' @family accessor functions
#' @export
#' @examples
#' x <- example_dataset()
#' observations(x)
observations <- function(x) {
  check_camtrapdp(x)
  purrr::pluck(x, "data", "observations")
}
