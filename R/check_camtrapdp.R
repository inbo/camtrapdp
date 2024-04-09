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
  frictionless::check_package(x)
  if (!("camtrapdp" %in% class(x))) {
    general_message <- paste("{.arg x} must be a Camera Trap Data Package",
                             "object created with {.fun read_camtrapdp}.")
    tip_message <- paste("Create a valid Camera Trap Data Package object with",
                         "{.fun read_camtrapdp}.")

    # Check x is a Camera Trap Data Package
    cli::cli_abort(
      c(
        general_message,
        "x" = "{.arg x} is a {.cls {class(x)}} object.",
        "i" = tip_message
      ),
      class = "camtrapdp_error_object_invalid"
    )
  }

  invisible(x)
}
