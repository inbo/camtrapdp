#' Check a Camera Trap Data Package object
#'
#' Checks if an object is a Camera Trap Data Package object with the required
#' properties.
#'
#' @inheritParams version
#' @return `x` invisibly or error.
#' @family check functions
#' @noRd
check_camtrapdp <- function(x) {
  # TODO: use check_package() from frictionless
  if (!("camtrapdp" %in% class(x))) {
    cli::cli_abort(
      "{.arg x} must be a Camera Trap Data Package object created with
       {.fun read_camtrapdp}.",
      class = "camtrapdp_error_object_invalid"
    )
  }

  invisible(x)
}
