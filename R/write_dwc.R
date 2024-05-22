#' Transform a Camera Trap Data Package to a Darwin Core Archive
#'
#' Transforms a Camera Trap Data Package object to a [Darwin Core Archive](
#' https://dwc.tdwg.org/text/).
#'
#' @inheritParams check_camtrapdp
#' @param directory Path to local directory to write files to.
#' @return CSV and `meta.xml` files written to disk.
#'   And invisibly, a list of data frames with the transformed data.
#' @family transformation functions
#' @export
#' @section Transformation details:
#' This function **follows [Reyserhove et al. 2024](
#' https://doi.org/10.35035/doc-0qzp-2x37) recommendations** and transform data
#' to:
#' - An [Occurrence core](
#' https://docs.gbif.org/camera-trap-guide/en/#section-occurrence-core).
#' - An [Audubon/Audiovisual Media Description extension](
#' https://docs.gbif.org/camera-trap-guide/en/#section-ac-extension).
#' - A `meta.xml` file.
#'
#' Key features of the Darwin Core transformation:
#' - The Occurrence core contains one row per observation
#'   (`dwc:occurrenceID = observationID`).
#' - Only observations with `observationType = "animal"` and
#'   `observationLevel = "event"` are included, thus excluding observations that
#'   are (of) humans, vehicles, blanks, unknowns, unclassified and media-based.
#' - Deployment information is included in the Occurrence core, such as
#'   location, habitat, `dwc:samplingProtocol`, deployment duration in
#'   `dwc:samplingEffort` and `dwc:parentEventID = deploymentID` as grouping
#'   identifier.
#' - Event information is included in the Occurrence core, as event duration in
#'   `dwc:eventDate` and `dwc:eventID = eventID` as grouping identifier.
#' - Media files are included in the Audubon/Audiovisual Media Description
#'   extension, with a foreign key to the observation.
#'   A media file that is used for more than one observation is repeated.
#' - Metadata is used to set the following record-level terms:
#'   - `dwc:datasetID = id`.
#'   - `dwc:datasetName = title`.
#'   - `dwc:collectionCode`: first source in `sources`.
#'   - `dcterms:license`: license (`name`) in `licenses` with scope `data`.
#'     The license (`name`) with scope `media` is used as `dcterms:rights` in
#'     the Audubon Media Description extension.
#'   - `dcterms:rightsHolder`: first contributor in `contributors` with role
#'     `rightsHolder`.
#'   - `dwc:dataGeneralizations`: set if `coordinatePrecision` is defined.
#' @examples
#' x <- example_dataset()
#' write_dwc(x, directory = "my_directory")
#'
#' # Clean up (don't do this if you want to keep your files)
#' unlink("my_directory", recursive = TRUE)
write_dwc <- function(x, directory = ".") {
  check_camtrapdp(x)

  # Set properties from metadata or default to NA when missing
  dataset_name <- purrr::pluck(x, "title", .default = NA)
  dataset_id <- purrr::pluck(x, "id", .default = NA)
  collection_code <- purrr::pluck(x, "sources", 1, "title", .default = NA)
  license <-
    purrr::pluck(x, "licenses") %>%
    purrr::detect(~ !is.null(.$scope) && .$scope == "data") %>%
    purrr::pluck("name", .default = NA)
  media_license <-
    purrr::pluck(x, "licenses") %>%
    purrr::detect(~ !is.null(.$scope) && .$scope == "media") %>%
    purrr::pluck("name", .default = NA)
  rights_holder <-
    purrr::pluck(x, "contributors") %>%
    purrr::detect(~ !is.null(.$role) && .$role == "rightsHolder") %>%
    purrr::pluck("title", .default = NA)
  coordinate_precision <- purrr::pluck(x, "coordinatePrecision", .default = NA)

  # Filter dataset on observations (also affects media)
  x <- filter_observations(
    x,
    .data$observationLevel == "event",
    .data$observationType == "animal"
  )

  # Start transformation
  cli::cli_h2("Transforming data to Darwin Core")

  # Create dwc_occurrence
  dwc_occurrence <-
    observations(x) %>%
    dplyr::left_join(deployments(x), by = "deploymentID") %>%
    dplyr::arrange(.data$deploymentID, .data$eventStart) %>%
    dplyr::mutate(
      .keep = "none",
      type = "Image",
      license = license,
      rightsHolder = rights_holder,
      datasetID = dataset_id,
      collectionCode = collection_code,
      datasetName = dataset_name,
      basisOfRecord = "MachineObservation",
      dataGeneralizations = dplyr::if_else(
        !is.na(coordinate_precision),
        paste("coordinates rounded to", coordinate_precision, "degree"),
        NA_character_
      ),
      occurrenceID = .data$observationID,
      individualCount = .data$count,
      sex = dplyr::recode(
        .data$sex,
        "male" = "male",
        "female" = "female"
      ),
      lifeStage = dplyr::recode(
        .data$lifeStage,
        "adult" = "adult",
        "subadult" = "subadult",
        "juvenile" = "juvenile"
      ),
      behavior = .data$behavior,
      occurrenceStatus = "present",
      occurrenceRemarks = .data$observationComments,
      organismID = .data$individualID,
      eventID = .data$eventID,
      parentEventID = .data$deploymentID,
      eventDate = dplyr::if_else(
        .data$eventStart == .data$eventEnd,
        format(.data$eventStart, format = "%Y-%m-%dT%H:%M:%SZ"),
        paste(
          format(.data$eventStart, format = "%Y-%m-%dT%H:%M:%SZ"),
          format(.data$eventEnd, format = "%Y-%m-%dT%H:%M:%SZ"),
          sep = "/"
        )
      ),
      habitat = .data$habitat,
      samplingProtocol = "camera trap",
      samplingEffort = paste(
        format(.data$deploymentStart, format = "%Y-%m-%dT%H:%M:%SZ"),
        format(.data$deploymentEnd, format = "%Y-%m-%dT%H:%M:%SZ"),
        sep = "/"
      ),
      eventRemarks = paste0(
        # E.g. "camera trap with bait near burrow | tags: <t1, t2> | <comment>"
        dplyr::if_else(
          .data$baitUse,
          "camera trap with bait",
          "camera trap without bait"
        ),
        dplyr::if_else(
          is.na(.data$featureType),
          "",
          paste0(
            " near ",
            dplyr::recode(
              .data$featureType,
              "roadPaved" = "paved road",
              "roadDirt" = "dirt road",
              "trailHiking" = "hiking trail",
              "trailGame" = "game trail",
              "roadUnderpass" = "road underpass",
              "roadOverpass" = "road overpass",
              "roadBridge" = "road bridge",
              "culvert" = "culvert",
              "burrow" = "burrow",
              "nestSite" = "nest site",
              "carcass" = "carcass",
              "waterSource" = "water source",
              "fruitingTree" = "fruiting tree"
            )
          )
        ),
        dplyr::if_else(
          !is.na(.data$observationTags),
          paste0(" | tags: ", .data$observationTags),
          ""
        ),
        dplyr::if_else(
          !is.na(.data$deploymentComments),
          paste0(" | ", .data$deploymentComments),
          ""
        )
      ),
      locationID = .data$locationID,
      locality = .data$locationName,
      minimumDepthInMeters = .data$cameraDepth,
      maximumDepthInMeters = .data$cameraDepth,
      minimumDistanceAboveSurfaceInMeters = .data$cameraHeight,
      maximumDistanceAboveSurfaceInMeters = .data$cameraHeight,
      decimalLatitude = .data$latitude,
      decimalLongitude = .data$longitude,
      geodeticDatum = "EPSG:4326", # WGS84
      coordinateUncertaintyInMeters = .data$coordinateUncertainty,
      coordinatePrecision = coordinate_precision,
      identifiedBy = .data$classifiedBy,
      dateIdentified = format(
        .data$classificationTimestamp,
        format = "%Y-%m-%dT%H:%M:%SZ"
      ),
      identificationRemarks = paste0(
        # E.g. "classified by a machine with 89% certainty"
        dplyr::if_else(
          is.na(.data$classificationMethod),
          "classified",
          paste0("classified by a ", .data$classificationMethod)
        ),
        dplyr::if_else(
          is.na(.data$classificationProbability),
          "",
          paste0(" with ", .data$classificationProbability * 100, "% certainty")
        )
      ),
      taxonID = .data$taxon.taxonID,
      scientificName = .data$scientificName,
      kingdom = "Animalia"
    ) %>%
    dplyr::select(
      "type", "license", "rightsHolder", "datasetID", "collectionCode",
      "datasetName", "basisOfRecord", "dataGeneralizations", "occurrenceID",
      "individualCount", "sex", "lifeStage", "behavior", "occurrenceStatus",
      "occurrenceRemarks", "organismID", "eventID", "parentEventID",
      "eventDate", "habitat", "samplingProtocol", "samplingEffort",
      "eventRemarks", "locationID", "locality", "minimumDepthInMeters",
      "maximumDepthInMeters", "minimumDistanceAboveSurfaceInMeters",
      "maximumDistanceAboveSurfaceInMeters", "decimalLatitude",
      "decimalLongitude", "geodeticDatum", "coordinateUncertaintyInMeters",
      "coordinatePrecision", "identifiedBy", "dateIdentified",
      "identificationRemarks", "taxonID", "scientificName", "kingdom"
    )

  # Create audiovisual extension
  dwc_audiovisual <-
    observations(x) %>%
    dplyr::select(-"mediaID") %>%
    dplyr::left_join(
      media(x),
      by = c("deploymentID", "eventID"),
      relationship = "many-to-many" # Silence warning
    ) %>%
    dplyr::left_join(
      deployments(x),
      by = "deploymentID"
    ) %>%
    dplyr::arrange(.data$deploymentID, .data$timestamp, .data$fileName) %>%
    dplyr::mutate(
      .keep = "none",
      occurrenceID = .data$observationID,
      identifier = .data$mediaID,
      `dc:type` = dplyr::case_when(
        grepl("video", .data$fileMediatype) ~ "MovingImage",
        .default = "StillImage"
      ),
      comments = dplyr::case_when(
        !is.na(favorite) & !is.na(mediaComments) ~
          paste("marked as favorite", mediaComments, sep = " | "),
        !is.na(favorite) ~ "marked as favorite",
        .default = .data$mediaComments
      ),
      `dcterms:rights` = media_license,
      CreateDate = format(.data$timestamp, format = "%Y-%m-%dT%H:%M:%SZ"),
      captureDevice = .data$cameraModel,
      resourceCreationTechnique = dplyr::recode(
        .data$captureMethod,
        "activityDetection" = "activity detection",
        "timeLapse" = "time lapse"
      ),
      accessURI = .data$filePath,
      `dc:format` = .data$fileMediatype,
      serviceExpectation = dplyr::if_else(
        .data$filePublic,
        "online",
        "authenticate"
      )
    ) %>%
    dplyr::select(
      "occurrenceID", "identifier", "dc:type", "comments", "dcterms:rights",
      "CreateDate", "captureDevice", "resourceCreationTechnique", "accessURI",
      "dc:format", "serviceExpectation",
    )

  # Write files
  dwc_occurrence_path <- file.path(directory, "dwc_occurrence.csv")
  dwc_audiovisual_path <- file.path(directory, "dwc_audiovisual.csv")
  meta_xml_path <- file.path(directory, "meta.xml")
  cli::cli_h2("Writing files")
  cli::cli_ul(c(
    "{.file {dwc_occurrence_path}}",
    "{.file {dwc_audiovisual_path}}",
    "{.file {meta_xml_path}}"
  ))
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  readr::write_csv(dwc_occurrence, dwc_occurrence_path, na = "")
  readr::write_csv(dwc_audiovisual, dwc_audiovisual_path, na = "")
  file.copy(
    system.file("extdata", "meta.xml", package = "camtrapdp"), # Static meta.xml
    meta_xml_path
  )

  # Return list with Darwin Core data invisibly
  return <- list(
    dwc_occurrence = dplyr::as_tibble(dwc_occurrence),
    dwc_audiovisual = dplyr::as_tibble(dwc_audiovisual)
  )
  invisible(return)
}
