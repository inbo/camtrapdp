#' Get observations
#'
#' @description
#' `observations()` gets the observations from a Camera Trap Data Package object.
#'
#' `observations<-()` is the assignment equivalent.
#'   It should only be used within other functions, where the expected data
#'   structure can be guaranteed.
#'
#' @inheritParams check_camtrapdp
#' @return [tibble()] data frame with observations.
#' @family accessor functions
#' @export
#' @examples
#' x <- example_dataset()
#' # Get the observations
#' observations(x)
#'
#' # Set observations (not recommended outside a function)
#' observations(x) <- head(observations(x), 1)
observations <- function(x) {
  check_camtrapdp(x)
  purrr::pluck(x, "data", "observations")
}

#' @rdname observations
#' @param value A data frame to assign as observations.
#' @export
'observations<-' <- function(x, value) {
  if (!is.data.frame(value)) {
    cli::cli_abort("{.arg value} is a {.obj_type_friendly {value}} but needs to
                    be a {.code data.frame}",
                   class = "camtrapdp_error_assignment_wrong_class")
  }

  purrr::assign_in(x, list("data", "observations"), value)
}
