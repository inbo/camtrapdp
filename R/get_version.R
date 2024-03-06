#' Get Camtrap DP version
#'
#' Gets the Camtrap DP version number used by a dataset.
#' The version number (`1`, `1.0`, `1.2.3`, etc.) is extracted from
#' `dataset$profile`, which should contain the URL of the used Camtrap DP
#' standard.
#' When no match is found, `dataset$profile` is returned in its entirety.
#'
#' @inheritParams convert
#' @return Camtrap DP version number or `dataset$profile`.
#' @export
get_version <- function(dataset) {
  profile <- dataset$profile # E.g. https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/camtrap-dp-profile.json
  pattern <- "camtrap-dp\\/\\d+(\\.\\d+){1,2}" # E.g. camtrap-dp/1, camtrap-dp/1.2.3
  match <- grep(pattern, pattern)

  if (length(match) > 0) {
    extracted_version <- regmatches(profile, regexpr(pattern, profile))
    sub("camtrap-dp/", "", extracted_version, fixed = TRUE)
  } else {
    profile
  }
}
