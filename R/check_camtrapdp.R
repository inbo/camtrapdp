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
  general_message <- paste(
    "{.arg x} must be a Camera Trap Data Package object created with",
    "{.fun read_camtrapdp}."
  )

  # Check if valid Data Package
  frictionless::check_package(x)

  # Check if class is present
  if (!("camtrapdp" %in% class(x))) {
    cli::cli_abort(
      c(
        general_message,
        "x" = "{.arg x} is missing a {.val camtrapdp} class."
      ),
      class = "camtrapdp_error_object_invalid"
    )
  }

  # Check if deployments, media, observations are data frames
  if (!is.data.frame(x$data$deployments)) {
    cli::cli_abort(
      c(
        general_message,
        "x" = "{.field deployments} must be a dataframe, but it is
               {.obj_type_friendly {x$data$deployments}}."
      ),
      class = "camtrapdp_error_data_invalid"
    )
  }
  if (!is.data.frame(x$data$media)) {
    cli::cli_abort(
      c(
        general_message,
        "x" = "{.field media} must be a dataframe, but it is
               {.obj_type_friendly {x$data$media}}."
      ),
      class = "camtrapdp_error_data_invalid"
    )
  }
  if (!is.data.frame(x$data$observations)) {
    cli::cli_abort(
      c(
        general_message,
        "x" = "{.field observations} must be a dataframe, but it is
               {.obj_type_friendly {x$data$observations}}."
      ),
      class = "camtrapdp_error_data_invalid"
    )
  }

  invisible(x)
}
