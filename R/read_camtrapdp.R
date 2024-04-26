#' Read a Camera Trap Data Package
#'
#' Reads files from a [Camera Trap Data Package (Camtrap DP)](
#' https://camtrap-dp.tdwg.org) into memory.
#'
#' @param file Path or URL to a `datapackage.json` file.
#' @param media_eventid Whether `eventID`s found in observations should be
#'   assigned to media.
#' @return Camera Trap Data Package object.
#' @family read functions
#' @export
read_camtrapdp <- function(file, media_eventid = TRUE) {
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
  x <- convert(x, convert_to = "1.0")

  # Add eventID to media
  if (media_eventid) {
    x$data$media <-
      dplyr::left_join(
        media(x),
        events(x),
        by = dplyr::join_by(
          "deploymentID",
          "timestamp" >= "eventStart",
          "timestamp" <= "eventEnd"
        )
      ) %>%
      dplyr::select(-"eventStart", -"eventEnd")
  }

  # Add taxonomic info to observations
  taxonomy <- build_taxonomy(x)
  if (!is.null(taxonomy)) {
    # Add taxon. as column suffix
    colnames(taxonomy) <- paste("taxon", colnames(taxonomy), sep = ".")

    # Join taxonomy with observations
    x$data$observations <-
      dplyr::left_join(
        observations(x),
        taxonomy,
        by = dplyr::join_by("scientificName" == "taxon.scientificName")
      )
  }

  return(x)
}
