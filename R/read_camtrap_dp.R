#' Read camtrap-dp formatted data
#'
#' This function reads camera trap data formatted following the [Camera Trap
#' Data Package (Camtrap DP)](https://github.com/tdwg/camtrap-dp) format.
#'
#'
#' @param path Path to the folder containing the camera trap data files.
#' @param multimedia If `TRUE`, read multimedia records into memory. If `FALSE`,
#'   ignore multimedia file to speed up reading larger Camtrap DP packages.
#'
#' @return A list of tibbles (dataframes).
#'
#' @export
#'
#' @importFrom datapackage read_package read_resource
#' @importFrom here here
#' @importFrom jsonlite read_json
#' @importFrom readr read_csv cols col_character col_number col_datetime
#'
#' @examples
#' \dontrun{
#' library(here)
#' # Read Camtrap DP package
#' camtrap_dp_dir <- here(
#'     "inst",
#'     "extdata",
#'     "mica-muskrat-and-coypu-20210302172233")
#' muskrat_coypu <- read_camtrap_dp(camtrap_dp_dir)
#'
#' # Read Camtrap DP package and ignore multimedia file
#' camtrap_dp_dir <- here(
#'     "inst",
#'     "extdata",
#'     "gmu8-monitoring-faunabeheerzone-8-20210301093537")
#' gmu8 <- read_camtrap_dp(camtrap_dp_dir, multimedia = FALSE)
#' }
read_camtrap_dp <- function(path, multimedia = TRUE) {
  # check multimedia
  assert_that(multimedia %in% c(TRUE, FALSE),
              msg = "multimedia must be a logical: TRUE or FALSE")
  # read files
  package <- read_package(file.path(path, "datapackage.json"))
  deployments <- read_resource(package, "deployments")
  observations <- read_resource(package, "observations")
  if (multimedia == TRUE) {
    multimedia <- read_resource(package, "multimedia")
  }

  # return list
  if (is.data.frame(multimedia)) {
    list(
      "metadata" = package,
      "deployments" = deployments,
      "multimedia" = multimedia,
      "observations" = observations
    )
  } else {
    list(
      "metadata" = package,
      "deployments" = deployments,
      "multimedia" = NULL,
      "observations" = observations
    )
  }
}
