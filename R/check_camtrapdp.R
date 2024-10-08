#' Check a Camera Trap Data Package object
#'
#' Checks if an object is a Camera Trap Data Package object with the required
#' properties.
#'
#' @inheritParams print.camtrapdp
#' @return `x` invisibly or an error.
#' @family check functions
#' @export
#' @examples
#' x <- example_dataset()
#' check_camtrapdp(x) # Invisible return of x if valid
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
               {.type {x$data$deployments}}."
      ),
      class = "camtrapdp_error_data_invalid"
    )
  }
  if (!is.data.frame(x$data$media)) {
    cli::cli_abort(
      c(
        general_message,
        "x" = "{.field media} must be a dataframe, but it is
               {.type {x$data$media}}."
      ),
      class = "camtrapdp_error_data_invalid"
    )
  }
  if (!is.data.frame(x$data$observations)) {
    cli::cli_abort(
      c(
        general_message,
        "x" = "{.field observations} must be a dataframe, but it is
               {.type {x$data$observations}}."
      ),
      class = "camtrapdp_error_data_invalid"
    )
  }

  invisible(x)
}
