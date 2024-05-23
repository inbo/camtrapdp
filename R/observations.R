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
#' # Get the observations of x
#' observations(x)
#' # Change the observations of x
#' observations(x) <-
#'   observations(x)[observations(x)$observationLevel == "media", ]
observations <- function(x) {
  check_camtrapdp(x)
  purrr::pluck(x, "data", "observations")
}

#' @rdname observations
#' @param value data.frame to assign to the observations of a Camera Trap Data Package Object.
#' @export
'observations<-' <- function(x, value){
  if(!is.data.frame(value)){
    cli::cli_abort("{.arg value} is a {.obj_type_friendly {value}} but needs to
                    be a {.code data.frame}",
                   class = "camtrapdp_error_assignment_wrong_class")
  }

  purrr::assign_in(x, list("data", "observations"), value)
}
