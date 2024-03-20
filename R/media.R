#' Get media
#'
#' Gets the media from a Camera Trap Data Package object.
#'
#' @inheritParams version
#' @return `tibble()` data frame with media.
#' @family accessor functions
#' @export
#' @examples
#' dataset <- example_dataset()
#' media(dataset)
media <- function(x) {
  # check_camtrapdp(x) # uncomment if check_camtrapdp() will exist
  x$data$media
}
