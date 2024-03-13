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

  # Convert package
  switch(
    version,
    # "0.1.6" = convert_from_0.1.6(package), # Example of conversion function
    "1.0" = return(package)
  )
}
