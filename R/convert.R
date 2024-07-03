#' Convert a Camera Trap Data Package
#'
#' Converts a Camera Trap Data Package object that uses an older version of
#' the Camtrap DP standard to a newer version.
#'
#' @inheritParams print
#' @param convert_to Version to convert to.
#' @return `x` converted.
#' @family convert functions
#' @noRd
convert <- function(x, convert_to = "1.0") {
  # Convert until the version number matches the expected version
  # while (version(x) != convert_to) {
  #   x <- switch(version(x),
  #     # "0.1.6" = convert_0.1.6_to_1.0(x),
  #     "1.0" = x
  #   )
  # }
  x
}

# How a conversion function would look like
# #' Convert a Camtrap DP 0.1.6 to 1.0
# #' @noRd
# convert_0.1.6_to_1.0 <- function(x) {
#   # Do conversion steps
#   # Set version
#   attr(x, "version") <- "1.0"
#   x
# }
