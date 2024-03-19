#' Get media
#'
#' Gets the media from a Camera Trap Data Package object.
#'
#' @inheritParams version
#' @return Media tibble.

media <- function(x) {
  x$data$media
}
