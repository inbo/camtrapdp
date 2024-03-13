#' Read a Camera Trap Data Package
#'
#' Reads files from a [Camera Trap Data Package (Camtrap DP)](
#' https://camtrap-dp.tdwg.org) into memory.
#'
#' @param file Path or URL to a `datapackage.json` file.
#' @return Camera Trap Data Package object.
#' @export
read_camtrap_dp <- function(file) {
  # Read datapackage.json
  package <- frictionless::read_package(file)

  # Check version
  version <- version(package)
  supported_versions <- c("1.0")
  if (!version %in% supported_versions) {
    cli::cli_abort(
      c(
        "{.val {version}} is not a supported Camtrap DP version.",
        "i" = "Supported version{?s}: {.val {supported_versions}}."
      ),
      class = "camtrapdp_error_unsupported_version"
    )
  }

  # Create camtrapdp object
  camtrapdp <- package
  class(camtrapdp) <- c("camtrapdp", class(camtrapdp))
  attr(camtrapdp, "version") <- version

  # Read and attach csv data
  camtrapdp$data$deployments <-
    frictionless::read_resource(package, "deployments")
  camtrapdp$data$media <-
    frictionless::read_resource(package, "media")
  camtrapdp$data$observations <-
    frictionless::read_resource(package, "observations")

  # Convert
  convert(package, convert_to = "1.0")
}
