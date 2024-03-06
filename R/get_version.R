#' Get the Camtrap DP version used by a dataset
#'
#' Extracts the Camtrap DP version number used by a dataset, by searching for
#' `camtrap-dp/<version-number>` in `dataset$profile`, which is expected to
#' contain the URL of the used Camtrap DP standard.
#' When no match is found, `dataset$profile` is returned in its entirety.
#'
#' @inheritParams convert
#' @return Camtrap DP version number (e.g. `1.0`) or `dataset$profile`.
#' @export
get_version <- function(dataset) {
  profile <- dataset$profile
  # E.g. https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/camtrap-dp-profile.json
  pattern <- "camtrap-dp\\/\\d+(\\.\\d+){1,2}"
  # Must be camtrap-dp/<major>.<minor><.optional_patch>
  match <- grep(pattern, profile)

  if (length(match) > 0) {
    extracted_version <- regmatches(profile, regexpr(pattern, profile))
    sub("camtrap-dp/", "", extracted_version, fixed = TRUE)
  } else {
    profile
  }
}
