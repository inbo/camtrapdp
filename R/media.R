#' Get media
#'
#' Gets the media from a Camera Trap Data Package object.
#'
#' @inheritParams version
#' @return Media tibble.
#' @family accessor functions
#' @export
media <- function(x) {
  check_camtrapdp(x)
  x$data$media
}
