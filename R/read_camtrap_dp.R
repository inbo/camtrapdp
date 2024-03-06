#' Read a Camera Trap Data Package dataset
#'
#' Reads files from a [Camera Trap Data Package (Camtrap DP)](
#' https://camtrap-dp.tdwg.org) into memory.
#'
#' @param file Path or URL to a `datapackage.json` file.
#' @return List describing a Camera Trap Data Package.
#' @export
read_camtrap_dp <- function(file) {
  dataset <- frictionless::read_package(file)
  convert(dataset)
}
