#' Convert a Camera Trap Data Package
#'
#' Converts a Camera Trap Data Package that uses an older version of Camtrap DP
#' to the latest version (currently `1.0`).
#'
#' @param package Camera Trap Data Package, as returned by
#'   `frictionless::read_package()`.
#' @return A `camtrapdp` object.
#' @noRd
convert <- function(package) {
  # Check Camtrap DP version
  version <- get_version(package)
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

  # Create camtrap_dp object
  dataset <- package
  class(dataset) <- c("camtrap_dp", class(dataset))
  attr(dataset, "version") <- version

  # TODO: Read and attach csv data (currently same for all versions)
  # dataset$data$deployments <- frictionless::read_resource(dataset, "deployments")
  # dataset$data$media <- frictionless::read_resource(dataset, "media")
  # dataset$data$observations <- frictionless::read_resource(dataset, "observations")

  # Convert dataset
  while(version != "1.0") {
    dataset <- switch(
      version,
      "0.1.6" = convert_0.1.6_to_1.0(dataset),
      "1.0" = dataset
    )
    version <- attr(dataset, "version")
  }

  dataset
}

#' Convert a Camtrap DP 0.1.6 to 1.0
#' @noRd
convert_0.1.6_to_1.0 <- function(dataset) {
  # TODO: conversion steps for 0.1.6
  attr(dataset, "version") <- "1.0"
  dataset
}
