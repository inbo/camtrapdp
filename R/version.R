#' Get Camtrap DP version
#'
#' Extracts the version number used by a Camera Trap Data Package object.
#' This version number indicates what version of the [Camtrap DP standard](
#' https://camtrap-dp.tdwg.org) was used.
#'
#' The version number is derived as follows:
#' 1. The `version` attribute, if defined.
#' 2. A version number contained in `camtrapdp$profile`, which is expected to
#' contain the URL to the used Camtrap DP standard.
#' 3. `camtrapdp$profile` in its entirety (can be `NULL`).
#'
#' @param camtrapdp Camera Trap Data Package object, as returned by
#'   `read_camtrap_dp()`.
#'   Also works on a Frictionless Data Package, as returned by
#'   `frictionless::read_package()`.
#' @return Camtrap DP version number (e.g. `1.0`).
#' @export
version <- function(camtrapdp) {
  # Get version from attribute
  attr_version <- attr(camtrapdp, "version")
  if (!is.null(attr_version)) {
    return(attr_version)
  }

  # Get version from profile
  profile <- camtrapdp$profile
  if (is.null(profile)) {
    return(NA)
  }

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
