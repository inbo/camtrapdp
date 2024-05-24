#' Get or set media
#'
#' @description
#' `media()` gets the media from a Camera Trap Data Package object.
#'
#' `media<-()` is the assignment equivalent.
#'   It should only be used within other functions, where the expected data
#'   structure can be guaranteed.
#'
#' @inheritParams check_camtrapdp
#' @return [tibble()] data frame with media.
#' @family accessor functions
#' @export
#' @examples
#' x <- example_dataset()
#' # Get media
#' media(x)
#'
#' # Set media (not recommended outside a function)
#' media(x) <- head(media(x), 1)
media <- function(x) {
  check_camtrapdp(x)
  purrr::pluck(x, "data", "media")
}

#' @rdname media
#' @param value A data frame to assign as media.
#' @export
'media<-' <- function(x, value) {
  if (!is.data.frame(value)) {
    cli::cli_abort(
      "{.arg value} must be a data.frame, not {.type {value}}.",
      class = "camtrapdp_error_assignment_wrong_class"
    )
  }
  purrr::pluck(x, "data", "media") <- dplyr::as_tibble(value)
  return(x)
}
