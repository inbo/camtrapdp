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
#' observations(x)
observations <- function(x) {
  check_camtrapdp(x)
  purrr::pluck(x, "data", "observations")
}

#' @rdname observations
#' @export
'observations<-' <- function(x, value){
  if(!is.data.frame(value)){
    cli::cli_abort("{.arg value} is a {.obj_type_friendly {value}} but needs to
                    be a {.code data.frame}")
  }

  purrr::assign_in(x, "data", "observations", value)
}
