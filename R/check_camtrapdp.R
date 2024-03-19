#' Check camtrapdp object
#'
#' Checks if an object is a Camera Trap Data Package object, i.e. it has the
#' class `camtrapdp`.
#'
#' @param x Camera Trap Data Package object
#' @return `TRUE` or error.
#' @family accessor functions
#' @noRd
check_camtrapdp <- function(x) {
  # TODO: use check_package() from frictionless
  if (!("camtrapdp" %in% class(x))) {
    cli::cli_abort(
      "{.arg x} must be a camtrapdp object created with {.fun read_camtrapdp}.",
      class = "camtrapdp_error_object_invalid"
    )
  }
  return(TRUE)
}
