#' Read the example data
#'
#' This function is a helper that saves you from having to remember the url to
#' the example dataset based on the Mica data.
#'
#' @inheritParams read_camtrap_dp
#'
#' @return Camera Trap Data Package object.
#'
#' @examples example_dataset()
example_dataset <- function() {
  read_camtrap_dp(file = file.path(
    "https://raw.githubusercontent.com",
    "tdwg",
    "camtrap-dp", "1.0", "example", "datapackage.json"
  ))
}
