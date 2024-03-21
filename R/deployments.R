#' Get deployments
#'
#' Gets the deployments from a Camera Trap Data Package object.
#'
#' @inheritParams version
#' @return [tibble()] data frame with deployments.
#' @family accessor functions
#' @export
#' @examples
#' x <- example_dataset()
#' deployments(x)
deployments <- function(x) {
  check_camtrapdp(x)
  purrr::pluck(x, "data", "deployments")
}
