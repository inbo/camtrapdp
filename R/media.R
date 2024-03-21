#' Get media
#'
#' Gets the media from a Camera Trap Data Package object.
#'
#' @inheritParams version
#' @return [tibble()] data frame with media.
#' @family accessor functions
#' @export
#' @examples
#' x <- example_dataset()
#' media(x)
media <- function(x) {
  check_camtrapdp(x)
  x$data$media
}
