#' Create an example dataset based on the Mica data
#'
#' @param ...
#' @inheritParams read_camtrap_dp
#'
#' @return Camera Trap Data Package object.
#' @export
#'
#' @examples example_dataset()
example_dataset <- function(...) {
  read_camtrap_dp(file = "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json", ...)
}
