#' Check a Camera Trap Data Package object
#'
#' Checks if an object is a Camera Trap Data Package object with the required
#' properties.
#'
#' @param x Camera Trap Data Package object, as returned by
#'   `read_camtrapdp()`.
#' @return `x` invisibly or error.
#' @family check functions
#' @export
check_camtrapdp <- function(x) {
  frictionless::check_package(x)
  if (!("camtrapdp" %in% class(x))) {
    cli::cli_abort(
      "{.arg x} must be a Camera Trap Data Package object created with
       {.fun read_camtrapdp}.",
      class = "camtrapdp_error_object_invalid"
    )
  }

  invisible(x)
}
