#' Get or set observations
#'
#' @description
#' `observations()` gets the observations from a Camera Trap Data Package
#'   object.
#'
#' `observations<-()` is the assignment equivalent.
#'   It should only be used within other functions, where the expected data
#'   structure can be guaranteed.
#'
#'   - Metadata (`x$taxonomic`) are updated to match the assigned observations.
#'
#' @inheritParams print.camtrapdp
#' @return A [tibble::tibble()] data frame with observations.
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
"observations<-" <- function(x, value) {
  if (!is.data.frame(value)) {
    cli::cli_abort(
      "{.arg value} must be a data.frame, not {.type {value}}.",
      class = "camtrapdp_error_assignment_wrong_class"
    )
  }
  purrr::pluck(x, "data", "observations") <- dplyr::as_tibble(value)

  # Update taxonomic scope in metadata
  x <- update_taxonomic(x)

  return(x)
}
