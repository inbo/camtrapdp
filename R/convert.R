#' Convert a dataset to the latest version of Camtrap DP
#'
#' Converts a dataset that uses an older version of Camtrap DP to the latest
#' version (currently `1.0`).
#'
#' @param dataset Camera Trap Data Package, as returned by `read_camtrap_dp()`.
#' @return Converted dataset.
#' @export
convert <- function(dataset) {
  # Check Camtrap DP version
  version <- get_version(dataset)
  supported_versions <- c("1.0")
  if (!version %in% supported_versions) {
    cli::cli_abort(
      c(
        "Can't recognize used Camtrap DP version {.val {version}}.",
        "i" = "Supported version{?s}: {.val {supported_versions}}."
      ),
      class = "camtrapdp_error_unsupported_version"
    )
  }

  # Convert dataset
  switch(
    version,
    # "0.1.6" = convert_from_0.1.6(dataset), # Example of conversion function
    "1.0" = return(dataset)
  )
}
