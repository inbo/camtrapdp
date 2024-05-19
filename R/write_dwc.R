#' Transform a Camera Trap Data Package to a Darwin Core Archive
#'
#' Transforms a Camera Trap Data Package object to a [Darwin Core Archive](
#' https://dwc.tdwg.org/text/).
#'
#' @inheritParams check_camtrapdp
#' @param directory Path to local directory to write files to.
#'   If `NULL`, a list of data frames is returned.
#' @return CSV and `meta.xml` files written to disk or a list of data frames
#'   when `directory = NULL`.
#' @family transformation functions
#' @export
#' @section Transformation details:
#' This function **follows [Reyserhove et al. 2024](
#' https://doi.org/10.35035/doc-0qzp-2x37) recommendations** and transform data
#' to:
#' - An [Occurrence core](
#' https://docs.gbif.org/camera-trap-guide/en/#section-occurrence-core).
#' - An [Audubon Media Description extension](
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
#' - Media files are included in the Audubon Media Description extension, with a
#'   foreign key to the observation.
#'   A media file that is used for more than one observation is repeated.
#' - Metadata is used to set the following record-level terms:
#'   - `dwc:datasetID = id`.
#'   - `dwc:datasetName = title`.
#'   - `dwc:collectionCode`: first source in `sources`.
#'   - `dcterms:license`: license in `licenses` with scope `data`.
#'     The license with scope `media` is used as `dcterms:rights` in
#'     the Audubon Media Description extension.
#'   - `dcterms:rightsHolder`: first contributor in `contributors` with role
#'     `rightsHolder`.
#'   - `dwc:dataGeneralizations`: set if `coordinatePrecision` is defined.
#' @examples
#' \dontrun{
#' x <- example_dataset()
#' write_dwc(x)
#' }
write_dwc <- function(x, directory = ".") {
  # Set properties from metadata
  # Use purrr::pluck() to force NA when metadata field is missing
  dataset_name <- purrr::pluck(x, "title", .default = NA)
  dataset_id <- purrr::pluck(x, "id", .default = NA)
  # Find contributor with rightsHolder role
  contributors <- purrr::pluck(x, "contributors", .default = NA)
  rightHoldersIndex <- grep("rightsHolder", contributors)
  rights_holder <-
    dplyr::coalesce(
      ifelse(
        !is.na(rightHoldersIndex),
        contributors[[rightHoldersIndex]]$title,
        NA),
      # If no rightsHolder assigned, set to organization of first contributor
      contributors[[1]]$organization,
      NA
  )
  collection_code <- purrr::pluck(x, "sources", .default = NA)[[1]]$title
  license <- dplyr::coalesce(
    purrr::keep(x$licenses, ~ .$scope == "data")[[1]]$name,
    purrr::keep(x$licenses, ~ .$scope == "data")[[1]]$path,
    NA
  )
  media_license <- dplyr::coalesce(
    purrr::keep(x$licenses, ~ .$scope == "media")[[1]]$path,
    NA
  )
  coordinate_precision <-
    purrr::pluck(x, "coordinatePrecision", .default = NA)

  # Read data
  deployments <- deployments(x)
  media <- media(x)
  observations_all <- observations(x)

  # Filter observations on animal observations (excluding humans, blanks, etc.)
  observations <- observations_all %>%
    dplyr::filter(.data$observationType == "animal") %>%
    # Keep only eventbased observations
    dplyr::filter(.data$observationLevel == "event")

  # Create dwc_occurrence
  dwc_occurrence <-
    deployments %>%
    dplyr::right_join(
      observations,
      by = "deploymentID"
    ) %>%
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
      dataGeneralizations = glue::glue(
        "coordinates rounded to {coordinate_precision} degree",
        .na = NULL
      ),
      occurrenceID = .data$observationID,
      individualCount = .data$count,
      sex = dplyr::recode(.data$sex,
                          "male" = "male",
                          "female" = "female"),
      lifeStage = dplyr::recode(.data$lifeStage,
                                "adult" = "adult",
                                "subadult" = "subadult",
                                "juvenile" = "juvenile"),
      behavior = .data$behavior,
      occurrenceStatus = "present",
      occurrenceRemarks = .data$observationComments,
      organismID = .data$individualID,
      eventID = .data$eventID,
      parentEventID = .data$deploymentID,
      eventDate =
        ifelse(
          .data$eventStart == .data$eventEnd,
          format(.data$eventStart, format = "%Y-%m-%dT%H:%M:%SZ"),
          paste0(
            format(.data$eventStart, format = "%Y-%m-%dT%H:%M:%SZ"), "/",
            format(.data$eventEnd, format = "%Y-%m-%dT%H:%M:%SZ")
            )
          ),
      habitat = .data$habitat,
      samplingProtocol = "camera trap",
      samplingEffort = glue::glue(
        "{start}/{end}",
        start = format(.data$eventStart, format = "%Y-%m-%dT%H:%M:%SZ"),
        end = format(.data$eventEnd, format = "%Y-%m-%dT%H:%M:%SZ")
      ),
      eventRemarks = stringr::str_squish(glue::glue(
        # E.g. "camera trap with bait near burrow | tags: <t1, t2> | <comment>"
        "{bait_use} {dep_feature} {dep_tags} {dep_comments}",
        bait_use = dplyr::case_when(
          .data$baitUse ~ "camera trap with bait",
          !.data$baitUse ~ "camera trap without bait"
        ) ,
        dep_feature_value = dplyr::recode(
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
        ),
        dep_feature = dplyr::case_when(
          !is.na(dep_feature_value) ~
            glue::glue("near {dep_feature_value}"),
          .default = ""
        ),
        dep_tags = dplyr::case_when(
          !is.na(.data$observationTags) ~
            glue::glue(" | tags: {.data$observationTags}"),
          .default = ""
        ),
        dep_comments = dplyr::case_when(
          !is.na(.data$deploymentComments) ~
            glue::glue(" | {.data$deploymentComments}"),
          .default = ""
        )
      )),
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
      identificationRemarks = stringr::str_squish(glue::glue(
        # E.g. "classified by a machine with a degree of certainty of 89%"
        "{classification_method} {classification_certainty}",
        classification_method = dplyr::case_when(
          !is.na(.data$classificationMethod) ~ glue::glue(
            "classified by a {.data$classificationMethod}"
          ),
          .default = ""
        ),
        degree_of_certainty = .data$classificationProbability * 100,
        classification_certainty = dplyr::case_when(
          !is.na(degree_of_certainty) ~ glue::glue(
            "with a degree of certainty of {degree_of_certainty}%"
          ),
          .default = ""
        )
      )),
      taxonID = .data$taxon.taxonID,
      scientificName = .data$scientificName,
      kingdom = "Animalia"
    ) %>%
    #Set column order
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

  # Create audiovisual core
  # Media can be linked to observations via mediaID
  dwc_audiovisual <-
    observations_all %>%
    dplyr::filter(!is.na(.data$mediaID)) %>%
    dplyr::left_join(media, by = "mediaID", suffix = c(".obs", "")) %>%
    dplyr::select(dplyr::all_of(
      c("observationID", "timestamp", colnames(media)
      ))) %>%
    dplyr::left_join(
      deployments,
      by = "deploymentID",
    ) %>%
    dplyr::arrange(.data$deploymentID, .data$timestamp, .data$fileName) %>%
    dplyr::mutate(
      .keep = "none",
      metadataLanguage = "http://id.loc.gov/vocabulary/iso639-2/eng",
      occurrenceID = .data$observationID,
      identifier = .data$mediaID,
      `dc:type` = dplyr::case_when(
        grepl("video", fileMediatype) ~ "MovingImage",
        .default = "StillImage"
      ),
      comments = dplyr::case_when(
        !is.na(favorite) & !is.na(mediaComments)
        ~ paste("marked as favorite", mediaComments, sep = " | "),
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
      serviceExpectation = dplyr::if_else(
        .data$filePublic,
        "online",
        "authenticate"
      ),
      `dc:format` = .data$fileMediatype
    ) %>%
    # Set column order
    dplyr::select(
      "metadataLanguage", "occurrenceID", "identifier", "dc:type", "comments",
      "dcterms:rights", "CreateDate", "captureDevice",
      "resourceCreationTechnique", "accessURI", "serviceExpectation",
      "dc:format"
    )

  # Return object or write files
  if (is.null(directory)) {
    list(
      dwc_occurrence = dplyr::as_tibble(dwc_occurrence),
      dwc_audiovisual = dplyr::as_tibble(dwc_audiovisual)
    )
  } else {
    dwc_occurrence_path <- file.path(directory, "dwc_occurrence.csv")
    dwc_audiovisual_path <- file.path(directory, "dwc_audiovisual.csv")
    meta_xml_path <- file.path(directory, "meta.xml")
    message(glue::glue(
      "Writing data to:",
      dwc_occurrence_path,
      dwc_audiovisual_path,
      meta_xml_path,
      .sep = "\n"
    ))
    if (!dir.exists(directory)) {
      dir.create(directory, recursive = TRUE)
    }
    readr::write_csv(dwc_occurrence, dwc_occurrence_path, na = "")
    readr::write_csv(dwc_audiovisual, dwc_audiovisual_path, na = "")
    # Get static meta.xml file from package extdata
    file.copy(
      from = system.file("extdata", "meta.xml", package = "camtrapdp"),
      to = meta_xml_path
    )
  }
}
