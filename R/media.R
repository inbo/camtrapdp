#' Get media
#'
#' Gets the media from a Camera Trap Data Package object.
#'
#' @inheritParams check_camtrapdp
#' @return [tibble()] data frame with media.
#' @family accessor functions
#' @export
#' @examples
#' x <- example_dataset()
#' media(x)
media <- function(x) {
  check_camtrapdp(x)
  purrr::pluck(x, "data", "media")
}
