#' Read a Camera Trap Data Package
#'
#' Reads files from a [Camera Trap Data Package (Camtrap DP)](
#' https://camtrap-dp.tdwg.org) into memory.
#'
#' @section Assign taxonomic information:
#'
#' Camtrap DP metadata has a `taxonomic` property that can contain extra
#' information for each `scientificName` found in observations.
#' Such information can include higher taxonomy (`family`, `order`, etc.) and
#' vernacular names in multiple languages.
#'
#' This function **will automatically include this taxonomic information in
#' observations**, as extra columns starting with `taxon.`.
#' It will then update the `taxonomic` scope in the metadata to the unique
#' [taxa()] found in the data.
#'
#' @section Assign eventIDs:
#'
#' Observations can contain two classifications at two levels:
#'
#' **Media-based** observations (`observationLevel = "media"`) are based on a
#' single media file and are directly linked to it via `mediaID`.
#'
#' **Event-based** observations (`observationLevel = "event"`) are based on an
#' event, defined as a combination of `eventID`, `eventStart` and `eventEnd`.
#' This event can consist of one or more media files, but is not directly linked
#' to these.
#'
#' This function **will automatically assign `eventID`s to media**, using
#' `media.deploymentID = event.deploymentID` and `eventStart <= media.timestamp
#' <= eventEnd`.
#' Note that this can result in media being linked to multiple events (and thus
#' being duplicated), for example when events and sub-events were defined.
#'
#' @section Update metadata:
#'
#' Camtrap DP metadata has a `spatial` and `temporal` property that contains the
#' spatial and temporal coverage of the package respectively.
#'
#' This function **will automatically update the spatial and temporal scopes**
#' in the metadata based on the data.
#' It also does this for the taxonomic scope (see higher).
#'
#' @param file Path or URL to a `datapackage.json` file.
#' @return A Camera Trap Data Package object.
#' @family read functions
#' @export
#' @examples
#' file <- "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json"
#' x <- read_camtrapdp(file)
#' x
read_camtrapdp <- function(file) {
  # Read datapackage.json
  package <- suppressMessages(frictionless::read_package(file))

  # Check version
  version <- version(package)
  supported_versions <- c("1.0", "1.0.1")
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
  # Assignment functions should not be used here, to bypass metadata update
  # and validation, which comes later
  purrr::pluck(x, "data", "deployments") <-
    frictionless::read_resource(package, "deployments")
  purrr::pluck(x, "data", "media") <-
    frictionless::read_resource(package, "media")
  purrr::pluck(x, "data", "observations") <-
    frictionless::read_resource(package, "observations")

  # Upgrade
  x <- upgrade(x, upgrade_to = "1.0.1")

  # Add eventID to media
  media(x) <-
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

  # Add taxonomic info to observations
  taxonomy <- taxonomic(x)
  if (!is.null(taxonomy)) {
    # Add taxon. as column suffix
    colnames(taxonomy) <- paste("taxon", colnames(taxonomy), sep = ".")

    # Join taxonomy with observations
    observations(x) <-
      dplyr::left_join(
        observations(x),
        taxonomy,
        by = dplyr::join_by("scientificName" == "taxon.scientificName")
      )
  }

  # Update temporal and spatial scope in metadata
  x <-
    x %>%
    update_temporal() %>%
    update_spatial()

  return(x)
}
