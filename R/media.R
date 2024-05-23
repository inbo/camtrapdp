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

#' @rdname media
#' @export
'media<-' <- function(x, value){
  if(!is.data.frame(value)){
    cli::cli_abort("{.arg value} is a {.obj_type_friendly {value}} but needs to
                    be a {.code data.frame}",
                   class = "camtrapdp_error_assignment_wrong_class")
  }

  purrr::assign_in(x, list("data", "media"), value)
}
