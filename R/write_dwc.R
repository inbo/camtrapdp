#' Transform Camtrap DP data to Darwin Core
#'
#' Transforms data from a [Camera Trap Data Package](
#' https://camtrap-dp.tdwg.org) to [Darwin Core](https://dwc.tdwg.org/).
#' The resulting CSV files can be uploaded to an [IPT](
#' https://www.gbif.org/ipt) for publication to GBIF.
#' A `meta.xml` file is included as well.
#' See `write_eml()` to create an `eml.xml` file.
#'
#' @param x A Camtrap DP, as read by [read_camtrap_dp()].
#' @param directory Path to local directory to write file(s) to.
#'   If `NULL`, then a list of data frames is returned instead, which can be
#'   useful for extending/adapting the Darwin Core mapping before writing with
#'   [readr::write_csv()].
#' @return CSV and `meta.xml` files written to disk or a list of data
#'   frames when `directory = NULL`.
#' @family publication functions
#' @export
#' @section Transformation details:
#' Data are transformed into an
#' [Occurrence core](https://rs.gbif.org/core/dwc_occurrence_2022-02-02.xml) and
#' [Audubon Media Description extension](https://rs.gbif.org/extension/ac/audubon_2020_10_06.xml).
#' This **follows recommendations** discussed and created by Peter Desmet,
#' John Wieczorek, Lien Reyserhove, Ben Norton and others.
#'
#' The following terms are set from the `package` metadata:
#' - **datasetName**: Title as provided in `package$title`.
#' - **datasetID**: Identifier as provided in `package$id`.
#'   Can be a DOI.
#' - **rightsHolder**: Rights holder as provided in `package$rightsHolder`.
#' - **collectionCode**: Platform name as provided in `package$platform$title`.
#' - **license**: License with scope `data` as provided in `package$licenses`.
#' - **rights** for media files: License with scope `media` as provided in
#'   `package$licenses`.
#' - **dwc:dataGeneralizations**: "coordinates rounded to
#'   `package$coordinatePrecision` degree".
#' - **coordinatePrecision**: `package$coordinatePrecision` (e.g. `0.001`).
#'
#' Key features of the Darwin Core transformation:
#' - Deployments (of camera traps) are parent events, with observations
#'   (machine observations) as child events. No information about the parent
#'   event is provided other than its ID, meaning that data can be expressed in
#'   an Occurrence Core with one row per observation and `parentEventID` shared
#'   by all occurrences in a deployment.
#' - Sequence-based observations share an `eventID` per sequence, image-based
#'   observations share an `eventID` per image.
#' - The image(s) an observation is based on are provided in the [Audubon Media
#'   Description extension](
#'   https://rs.gbif.org/extension/ac/audubon_2020_10_06.xml), with a foreign
#'   key to the observation.
#' - Excluded are records that document blank or unclassified media, vehicles
#'   and observations of humans.
write_dwc <- function(x, directory = ".") {
  # Set properties from metadata
  # Use purrr::pluck() to force NA when metadata field is missing
  dataset_name <- purrr::pluck(x, "title", .default = NA)
  dataset_id <- purrr::pluck(x, "id", .default = NA)
  rights_holder <- purrr::pluck(x, "rightsHolder", .default = NA)
  collection_code <- purrr::pluck(x, "platform", "title", .default = NA)
  license <- dplyr::coalesce(
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
  deployments <- dplyr::tibble(x$data$deployments)
  media <- dplyr::tibble(x$data$media)
  observations <- dplyr::tibble(x$data$observations)

  # Filter observations on animal observations (excluding humans, blanks, etc.)
  observations <- dplyr::filter(observations, .data$observationType == "animal")

  # Create dwc_occurrence
  dwc_occurrence <-
    deployments %>%
    dplyr::right_join(
      observations,
      by = "deploymentID",
      suffix = c(".dep", ".obs")
    ) %>%
    # TODO: check if timestamp == eventStart?
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
      sex = .data$sex,
      lifeStage = .data$lifeStage,
      behavior = .data$behavior,
      occurrenceStatus = "present",
      occurrenceRemarks = .data$observationComments,
      organismID = .data$individualID,
      eventID = .data$eventID,
      parentEventID = .data$deploymentID,
      eventDate = format(.data$eventStart, format = "%Y-%m-%dT%H:%M:%SZ"),
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
          .data$baitUse == "none" ~ "camera trap without bait",
          !is.na(.data$baitUse) ~
            glue::glue("camera trap with {.data$baitUse} bait"),
          .default = "camera trap",
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
          "fruitingTree" = "fruiting tree",
          "other" = "other feature",
          "none" = NA_character_
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
      "eventRemarks", "locationID", "locality", "decimalLatitude",
      "decimalLongitude", "geodeticDatum", "coordinateUncertaintyInMeters",
      "coordinatePrecision", "identifiedBy", "dateIdentified",
      "identificationRemarks", "taxonID", "scientificName", "kingdom"
    )

  # Create auduboncore
  # Media can be linked to observations via mediaID
  on_med <-
    observations %>%
    dplyr::filter(!is.na(.data$mediaID)) %>%
    dplyr::left_join(media, by = "mediaID", suffix = c(".obs", "")) %>%
    dplyr::select(dplyr::all_of(
      c("observationID", "timestamp", colnames(media)
      )))
  dwc_audubon <-
    on_med %>%
    dplyr::left_join(
      deployments,
      by = "deploymentID",
      suffix = c(".obs_med", ".dep")
    ) %>%
    dplyr::arrange(.data$deploymentID, .data$timestamp, .data$fileName) %>%
    dplyr::mutate(
      .keep = "none",
      occurrenceID = .data$observationID,
      identifier = .data$mediaID,
      `dc:type` = dplyr::case_when(
        grepl("video", fileMediatype) ~ "MovingImage",
        .default = "StillImage"
      ),
      comments = dplyr::case_when(
        !is.na(favorite) & !is.na(mediaComments)
        ~ paste("marked as favourite", mediaComments, sep = " | "),
        !is.na(favorite) ~ "marked as favourite",
        .default = .data$mediaComments
      ),
      `dcterms:rights` = media_license,
      CreateDate = format(.data$timestamp, format = "%Y-%m-%dT%H:%M:%SZ"),
      captureDevice = .data$cameraModel,
      resourceCreationTechnique = dplyr::recode(
        .data$captureMethod,
        "motionDetection" = "motion detection",
        "timeLapse" = "time lapse"
      ),
      accessURI = .data$filePath,
      # serviceExpectation = dplyr::if_else(
      #   .data$filePublic,
      #   "online",
      #   "authenticate"
      # ),
      `dc:format` = .data$fileMediatype
    ) %>%
    # Set column order
    dplyr::select(
      "occurrenceID", "identifier", "dc:type", "comments", "dcterms:rights",
      "CreateDate", "captureDevice", "resourceCreationTechnique", "accessURI",
      "dc:format"
    )

  # Return object or write files
  if (is.null(directory)) {
    list(
      dwc_occurrence = dplyr::as_tibble(dwc_occurrence),
      dwc_audubon = dplyr::as_tibble(dwc_audubon)
    )
  } else {
    dwc_occurrence_path <- file.path(directory, "dwc_occurrence.csv")
    dwc_audubon_path <- file.path(directory, "dwc_audubon.csv")
    meta_xml_path <- file.path(directory, "meta.xml")
    message(glue::glue(
      "Writing data to:",
      dwc_occurrence_path,
      dwc_audubon_path,
      meta_xml_path,
      .sep = "\n"
    ))
    if (!dir.exists(directory)) {
      dir.create(directory, recursive = TRUE)
    }
    readr::write_csv(dwc_occurrence, dwc_occurrence_path, na = "")
    readr::write_csv(dwc_audubon, dwc_audubon_path, na = "")
    # Get static meta.xml file from package extdata
    file.copy(
      from = system.file("extdata", "meta.xml", package = "camtraptor"),
      to = meta_xml_path
    )
  }
}
