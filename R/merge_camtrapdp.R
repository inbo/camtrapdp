#' Merge Camera Trap Data packages
#'
#' @param x1,x2 Camera Trap Data Package objects (as returned by
#' `read_camtrapdp()`), to be coerced to one.
#' @param name A short url-usable (and preferably human-readable) name for this
#' merged package.
#' @param title A string providing a title or one sentence description for this
#' merged package.
#' @return `x`
#' @family transformation functions
#' @export
#' @examples
#' x1 <- example_dataset() %>%
#'   filter_deployments(deploymentID %in% c("00a2c20d", "29b7d356"))
#' x2 <- example_dataset() %>%
#'   filter_deployments(deploymentID %in% c("577b543a", "62c200a9"))
#' x_merged <- merge_camtrapdp(x1, x2, "new_package_name", "New title")
merge_camtrapdp <- function(x1, x2, name, title) {
  check_camtrapdp(x1)
  check_camtrapdp(x2)
  x <- x1

  # merge resources
  deployments(x) <- dplyr::bind_rows(deployments(x1), deployments(x2))
  media(x) <- dplyr::bind_rows(media(x1), media(x2))
  observations(x) <- dplyr::bind_rows(observations(x1), observations(x2))

  # get ID's
  deploymentIDs <- purrr::pluck(deployments(x), "deploymentID")
  mediaIDs <- purrr::pluck(media(x), "mediaID")
  observationIDs <- purrr::pluck(observations(x), "observationID")

  # replace duplicated deploymentID's in x2
  if (any(duplicated(deploymentIDs))) {
    duplicated_deploymentID <- deploymentIDs[duplicated(deploymentIDs)]
    x2 <- replace_duplicated_deploymentID(x2, duplicated_deploymentID)

    # new merge with unique deploymentID's
    deployments(x) <- dplyr::bind_rows(deployments(x1), deployments(x2))
    media(x) <- dplyr::bind_rows(media(x1), media(x2))
    observations(x) <- dplyr::bind_rows(observations(x1), observations(x2))

    # inform user
    new_deploymentIDs <- vdigest_crc32(duplicated_deploymentID)
    cli::cli_alert_warning(
      c(
        paste(
          "{.arg x1} and {.arg x2} must have unique deploymentID's.\n",
          "{.arg x1} and {.arg x2} have duplicated deploymentID's:",
          "{.val {duplicated_deploymentID}}.\n",
          "Duplicated deploymentID's of {.arg x2} are now replaced by",
          "{.val {new_deploymentIDs}} respectively."
        )
      ),
      class = "camtrapdp_warning_unique_deploymentID"
    )
  }

  # set unique mediaID's
  if (any(duplicated(mediaIDs))) {
    duplicated_mediaID <- mediaIDs[duplicated(mediaIDs)]

    # give unique mediaDs to media
    media(x2) <-
      media(x2) %>%
      dplyr::mutate(
        mediaID =
          dplyr::if_else(
            .data$mediaID %in% duplicated_mediaID,
            vdigest_crc32(.data$mediaID),
            .data$mediaID
          )
      )

    # give unique mediaDs to observations
    observations(x2) <-
      observations(x2) %>%
      dplyr::mutate(
        mediaID =
          dplyr::if_else(
            .data$mediaID %in% duplicated_mediaID,
            vdigest_crc32(.data$mediaID),
            .data$mediaID
          )
      )

    # new merge with unique mediaID's
    media(x) <- dplyr::bind_rows(media(x1), media(x2))
    observations(x) <- dplyr::bind_rows(observations(x1), observations(x2))

    # inform user
    new_mediaIDs <- vdigest_crc32(duplicated_mediaID)
    cli::cli_alert_warning(
      c(
        paste(
          "{.arg x1} and {.arg x2} must have unique mediaID's.\n",
          "{.arg x1} and {.arg x2} have duplicated mediaID's:",
          "{.val {duplicated_mediaID}}.\n",
          "Duplicated mediaID's of {.arg x2} are now replaced by",
          "{.val {new_mediaIDs}} respectively."
        )
      ),
      class = "camtrapdp_warning_unique_mediaID"
    )
  }

  # set unique observationID's
  if (any(duplicated(observationIDs))) {
    duplicated_observationID <- observationIDs[duplicated(observationIDs)]

    # give unique observationIDs to observations
    observations(x2) <-
      observations(x2) %>%
      dplyr::mutate(
        observationID =
          dplyr::if_else(
            .data$observationID %in% duplicated_observationID,
            vdigest_crc32(.data$observationID),
            .data$observationID
          )
      )

    # new merge with unique observationID's
    observations(x) <- dplyr::bind_rows(observations(x1), observations(x2))

    # inform user
    new_observationIDs <- vdigest_crc32(duplicated_observationID)
    cli::cli_alert_warning(
      c(
        paste(
          "{.arg x1} and {.arg x2} must have unique observationID's.\n",
          "{.arg x1} and {.arg x2} have duplicated observationID's:",
          "{.val {duplicated_observationID}}.\n",
          "Duplicated observationID's of {.arg x2} are now replaced by",
          "{.val {new_observationIDs}} respectively."
        )
      ),
      class = "camtrapdp_warning_unique_observationID"
    )
  }

  # merge/update metadata
  x$name <- name
  x$id <- digest::digest(paste(x$title, x2$title), algo = "md5")
  x$created <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  x$title <- title
  x$contributors <- c(x1$contributors, x2$contributors)
  paragraph <- paste0(
    "This dataset is a combination of 2 datasets: ", x1$title, "and", x2$title,
    ".")
  x$version <- "1.0"
  x$keywords <- unique(x1$keywords, x2$keywords)
  x$image <- NULL
  x$homepage <- NULL
  x$sources <- c(x1$sources, x2$sources)
  x$licenses <- c(x1$licenses, x2$licences)
  x$bibliographicCitation <- NULL
  x$coordinatePrecision <-
    max(x1$coordinatePrecision, x2$coordinatePrecision, na.rm = TRUE)

  relatedIdentifiers_x1 <- list(
    relationType = "IsDerivedFrom",
    relatedIdentifier = x1$id,
    resourceTypeGeneral = "Data package",
    relatedIdentifierType = "id"
  )
  relatedIdentifiers_x2 <- list(
    relationType = "IsDerivedFrom",
    relatedIdentifier = x2$id,
    resourceTypeGeneral = "Data package",
    relatedIdentifierType = "id"
  )
  new_relatedIdentifiers <- list(relatedIdentifiers_x1, relatedIdentifiers_x2)
  x$relatedIdentifiers <-
    c(x1$relatedIdentifiers, x2$relatedIdentifiers, new_relatedIdentifiers)

  x$references <- c(x1$references, x2$references)

  x <-
    update_spatial(x) %>%
    update_temporal() %>%
    update_taxonomic()

  return(x)
}
