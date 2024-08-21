#' Convert a Camera Trap Data Package
#'
#' Converts a Camera Trap Data Package object that uses an older version of
#' the Camtrap DP standard to a newer version.
#'
#' @inheritParams print.camtrapdp
#' @param convert_to Version to convert to.
#' @return `x` converted.
#' @family convert functions
#' @noRd
convert <- function(x, convert_to = "1.0.1") {
  # Convert until the version number matches the expected version
  while (version(x) != convert_to) {
    x <- switch(version(x),
      "1.0" = convert_1.0_to_1.0.1(x),
      "1.0.1" = x
    )
  }
  return(x)
}

#' Convert a Camtrap DP 1.0 to 1.0.1
#'
#' This is a patch version, no changes are made to the standard.
#' @noRd
convert_1.0_to_1.0.1 <- function(x) {
  version(x) <- "1.0.1"
  return(x)
}
