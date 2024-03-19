#' Get media
#'
#' Gets the media from a Camera Trap Data Package object.
#'
#' @inheritParams version
#' @return `tibble()` data frame with media.
#' @family accessor functions
#' @export
media <- function(x) {
  x$data$media
}
