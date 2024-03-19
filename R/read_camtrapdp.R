#' Read a Camera Trap Data Package
#'
#' Reads files from a [Camera Trap Data Package (Camtrap DP)](
#' https://camtrap-dp.tdwg.org) into memory.
#'
#' @param file Path or URL to a `datapackage.json` file.
#' @return Camera Trap Data Package object.
#' @family read functions
#' @export
read_camtrapdp <- function(file) {
  # Read datapackage.json
  package <- suppressMessages(frictionless::read_package(file))

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
  x <- package
  class(x) <- c("camtrapdp", class(x))
  attr(x, "version") <- version

  # Read and attach csv data
  x$data$deployments <-
    frictionless::read_resource(package, "deployments")
  x$data$media <-
    frictionless::read_resource(package, "media")
  x$data$observations <-
    frictionless::read_resource(package, "observations")

  # Convert
  convert(x, convert_to = "1.0")
}
