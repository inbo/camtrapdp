#' Get the Camtrap DP version used by a package
#'
#' Extracts the version number used by a Camera Trap Data Package, by searching
#' for `camtrap-dp/<version-number>` in `package$profile`, which is expected to
#' contain the URL of the used Camtrap DP standard.
#'
#' @inheritParams convert
#' @return Camtrap DP version number (e.g. `1.0`) or `package$profile` in its
#' entirety when no version number is found.
#' @noRd
get_version <- function(package) {
  profile <- package$profile # E.g. https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/camtrap-dp-profile.json

  # No profile defined
  if (is.null(profile)) {
    return(NA)
  }

  # Find pattern "camtrap-dp/<major>.<minor><.optional_patch>"
  pattern <- "camtrap-dp\\/\\d+(\\.\\d+){1,2}"
  match <- grep(pattern, profile)
  if (length(match) > 0) {
    extracted_version <- regmatches(profile, regexpr(pattern, profile))
    sub("camtrap-dp/", "", extracted_version, fixed = TRUE)
  } else {
    profile
  }
}
