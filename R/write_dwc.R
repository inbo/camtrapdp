#' Transform a Camera Trap Data Package to a Darwin Core Archive
#'
#' Transforms a Camera Trap Data Package object to a [Darwin Core Archive](
#' https://dwc.tdwg.org/text/).
#'
#' @inheritParams print.camtrapdp
#' @param directory Path to local directory to write files to.
#' @return CSV and `meta.xml` files written to disk.
#'   And invisibly, a list of data frames with the transformed data.
#' @family transformation functions
#' @export
#' @section Transformation details:
#' This function **follows recommendations** in Reyserhove et al. (2023)
#' \doi{10.35035/doc-0qzp-2x37} and transform data to:
#' - An [Occurrence core](
#'   https://docs.gbif.org/camera-trap-guide/en/#section-occurrence-core).
#' - An [Audubon/Audiovisual Media Description extension](
#'   https://docs.gbif.org/camera-trap-guide/en/#section-ac-extension).
#' - A `meta.xml` file.
#'
#' Key features of the Darwin Core transformation:
#' - The Occurrence core contains one row per observation
#'   (`dwc:occurrenceID = observationID`).
#' - Only observations with `observationType = "animal"` and
#'   `observationLevel = "event"` are included, thus excluding observations that
#'   are (of) humans, vehicles, blanks, unknowns, unclassified and media-based.
#' - Observations classified by humans with 100% certainty get a
#'   `dwc:identificationVerificationStatus = "verified using recorded media"`.
#' - Deployment information is included in the Occurrence core, such as
#'   location, habitat, `dwc:samplingProtocol`, deployment duration in
#'   `dwc:samplingEffort` and `dwc:parentEventID = deploymentID` as grouping
#'   identifier.
#' - Event information is included in the Occurrence core, as event duration in
#'   `dwc:eventDate` and `dwc:eventID = eventID` as grouping identifier.
#' - Media files are included in the Audubon/Audiovisual Media Description
#'   extension, with a foreign key to the observation.
#'   A media file that is used for more than one observation is repeated.
#' - Metadata are used to set the following record-level terms:
#'   - `dwc:datasetID`: `x$id`.
#'   - `dwc:datasetName`: `x$title`.
#'   - `dwc:collectionCode`: first source in `x$sources`.
#'   - `dcterms:license`: license `name` (e.g. `CC0-1.0`) in `x$licenses` with
#'     scope `data`.
#'     The license `name` with scope `media` is used as `dcterms:rights` in the
#'     Audubon Media Description extension.
#'   - `dcterms:rightsHolder`: first contributor in `x$contributors` with role
#'     `rightsHolder`.
#'   - `dwc:dataGeneralizations`: set if `x$coordinatePrecision` is defined.
#' @examples
#' x <- example_dataset()
#' write_dwc(x, directory = "my_directory")
#'
#' # Clean up (don't do this if you want to keep your files)
#' unlink("my_directory", recursive = TRUE)
write_dwc <- function(x, directory) {
  check_camtrapdp(x)

  # Set properties from metadata or default to NA when missing
  dataset_name <- purrr::pluck(x, "title", .default = NA_character_)
  dataset_id <- purrr::pluck(x, "id", .default = NA_character_)
  collection_code <-
    purrr::pluck(x, "sources", 1, "title", .default = NA_character_)
  license <-
    purrr::pluck(x, "licenses") %>%
    purrr::detect(~ !is.null(.x$scope) && .x$scope == "data") %>%
    purrr::pluck("name", .default = NA_character_)
  media_license <-
    purrr::pluck(x, "licenses") %>%
    purrr::detect(~ !is.null(.x$scope) && .x$scope == "media") %>%
    purrr::pluck("name", .default = NA_character_)
  rights_holder <-
    purrr::pluck(x, "contributors") %>%
    purrr::detect(~ !is.null(.x$role) && .x$role == "rightsHolder") %>%
    purrr::pluck("title", .default = NA_character_)
  coordinate_precision <-
    purrr::pluck(x, "coordinatePrecision", .default = NA_character_)

  # Filter dataset on observations (also affects media)
  if (!is.null(x$gbifIngestion$observationLevel)) {
    if (x$gbifIngestion$observationLevel == "media") {
      x <- filter_observations(x, .data$observationLevel == "media")
    }
  } else {
    x <- filter_observations(x, .data$observationLevel == "event")
  }
  x <- filter_observations(x, .data$observationType == "animal")

  # Start transformation
  cli::cli_h2("Transforming data to Darwin Core")

  # Read data and add optional columns that are used in transformation
 deployments <- deployments(x)
 media <- media(x)
 observations_cols <- c("taxon.taxonID")
 observations <- expand_cols(observations(x), observations_cols)

  # Create Darwin Core Occurrence core
  occurrence <-
    observations %>%
    dplyr::left_join(deployments, by = "deploymentID") %>%
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
        dplyr::case_when(
          .data$baitUse == TRUE ~ "camera trap with bait",
          .data$baitUse == FALSE ~ "camera trap without bait",
          is.na(.data$baitUse) & !is.na(.data$featureType) ~ "camera trap",
          is.na(.data$baitUse) & is.na(.data$featureType) ~ ""
          ),
        dplyr::if_else(
          is.na(.data$featureType),
          "",
          paste0(
            " near ",
            dplyr::case_match(
              .data$featureType,
              "roadPaved" ~ "paved road",
              "roadDirt" ~ "dirt road",
              "trailHiking" ~ "hiking trail",
              "trailGame" ~ "game trail",
              "roadUnderpass" ~ "road underpass",
              "roadOverpass" ~ "road overpass",
              "roadBridge" ~ "road bridge",
              "culvert" ~ "culvert",
              "burrow" ~ "burrow",
              "nestSite" ~ "nest site",
              "carcass" ~ "carcass",
              "waterSource" ~ "water source",
              "fruitingTree" ~ "fruiting tree"
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
      identificationVerificationStatus = dplyr::if_else(
        .data$classificationMethod == "human" &
        .data$classificationProbability == 1,
        "verified using recorded media",
        NA_character_
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
          paste0(
            " with ", as.numeric(.data$classificationProbability) * 100,
            "% certainty"
          )
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
      "identificationVerificationStatus", "identificationRemarks", "taxonID",
      "scientificName", "kingdom"
    )

  # Create Audubon/Audiovisual Media Description extension
  multimedia <-
    observations %>%
    dplyr::select(-"mediaID") %>%
    dplyr::left_join(
      media,
      by = c("deploymentID", "eventID"),
      relationship = "many-to-many" # Silence warning
    ) %>%
    dplyr::left_join(
      deployments,
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
      resourceCreationTechnique = dplyr::case_match(
        .data$captureMethod,
        "activityDetection" ~ "activity detection",
        "timeLapse" ~ "time lapse"
      ),
      accessURI = .data$filePath,
      `dc:format` = .data$fileMediatype,
      serviceExpectation = dplyr::if_else(
        as.logical(.data$filePublic),
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
  occurrence_path <- file.path(directory, "occurrence.csv")
  multimedia_path <- file.path(directory, "multimedia.csv")
  meta_xml_path <- file.path(directory, "meta.xml")
  cli::cli_h2("Writing files")
  cli::cli_ul(c(
    "{.file {occurrence_path}}",
    "{.file {multimedia_path}}",
    "{.file {meta_xml_path}}"
  ))
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  readr::write_csv(occurrence, occurrence_path, na = "")
  readr::write_csv(multimedia, multimedia_path, na = "")
  file.copy(
    system.file("extdata", "meta.xml", package = "camtrapdp"), # Static meta.xml
    meta_xml_path
  )

  # Return list with Darwin Core data invisibly
  return <- list(
    occurrence = dplyr::as_tibble(occurrence),
    multimedia = dplyr::as_tibble(multimedia)
  )
  invisible(return)
}
