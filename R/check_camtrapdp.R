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

  general_message2 <- paste("{.arg x} must have deployments, media and",
                            "observations data frames present.")
  # Check deployments is a data frame
  if(!is.data.frame(x$data$deployments)) {
    cli::cli_abort(
      c(
        general_message2,
        "x" = "{.arg x} is missing a {.field deployments} attribute OR
        {.field deployments} is not a data frame.",
      ),
      class = "camtrapdp_error_data_invalid"
    )
  }

  # Check media is a data frame
  if(!is.data.frame(x$data$media)) {
    cli::cli_abort(
      c(
        general_message2,
        "x" = "{.arg x} is missing a {.field media} attribute OR
        {.field media} is not a data frame.",
      ),
      class = "camtrapdp_error_data_invalid"
    )
  }
  # Check observations is a data frame
  if(!is.data.frame(x$data$observations)) {
    cli::cli_abort(
      c(
        general_message2,
        "x" = "{.arg x} is missing a {.field observations} attribute OR
        {.field observations} is not a data frame.",
      ),
      class = "camtrapdp_error_data_invalid"
    )
  }

  invisible(x)
}
