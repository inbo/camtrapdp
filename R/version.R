#' Get Camtrap DP version
#'
#' Extracts the version number used by a Camera Trap Data Package object.
#' This version number indicates what version of the [Camtrap DP standard](
#' https://camtrap-dp.tdwg.org) was used.
#'
#' The version number is derived as follows:
#' 1. The `version` attribute, if defined.
#' 2. A version number contained in `x$profile`, which is expected to
#' contain the URL to the used Camtrap DP standard.
#' 3. `x$profile` in its entirety (can be `NULL`).
#'
#' @param x Camera Trap Data Package object, as returned by
#'   [read_camtrapdp()].
#'   Also works on a Frictionless Data Package, as returned by
#'   [frictionless::read_package()].
#' @return Camtrap DP version number (e.g. `1.0`).
#' @family misc functions
#' @export
#' @examples
#' x <- example_dataset()
#' version(x)
version <- function(x) {
  # Get version from attribute
  attr_version <- attr(x, "version")
  if (!is.null(attr_version)) {
    return(attr_version)
  }

  # Get version from profile
  profile <- purrr::pluck(x, "profile", .default = NA)

  # Find pattern "camtrap-dp/<version>/" in e.g.
  # https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/camtrap-dp-profile.json
  pattern <- "camtrap-dp\\/([0-9A-Za-z]|\\.|-)+\\/"
  match <- grep(pattern, profile)
  if (length(match) > 0) {
    extracted_version <- regmatches(profile, regexpr(pattern, profile))
    extracted_version <- sub("camtrap-dp/", "", extracted_version, fixed = TRUE)
    extracted_version <- sub("/", "", extracted_version, fixed = TRUE)
    extracted_version
  } else {
    profile
  }
}

#' @rdname version
#' @param value Version number to assign to `x`.
#' @noRd
"version<-" <- function(x, value) {
  old <- version(x)
  new <- value

  # Update profile
  x$profile <- sub(old, new, x$profile, fixed = TRUE)

  # Update resource schemas
  camtrapdp_resource_names <- c("deployments", "media", "observations")
  x$resources <- purrr::map(x$resources, function(resource) {
    if (resource$name %in% camtrapdp_resource_names) {
      resource$schema <- sub(old, new, resource$schema, fixed = TRUE)
    }
    resource
  })

  # Update version
  attr(x, "version") <- new
  return(x)
}
