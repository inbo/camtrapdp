#' Merge camtrapdp packages
#'
#' @param x1
#' @param x2
#' @param name
#' @param title
#' @return `x`
#' @export
#' @examples
#' x1 <- example_dataset() %>%
#'   filter_deployments(deploymentID %in% c("00a2c20d", "29b7d356"))
#' x2 <- example_dataset() %>%
#'   filter_deployments(deploymentID %in% c("577b543a", "62c200a9"))
#' x_merged <- merge_camtrapdp(x1, x2, "new package name", "new title")
merge_camtrapdp <- function(x1, x2, name, title) {
  check_camtrapdp(x1)
  check_camtrapdp(x2)
  x <- x1

  # merge resources
  deployments(x) <- dplyr::bind_rows(deployments(x1), deployments(x2))
  media(x) <- dplyr::bind_rows(media(x1), media(x2))
  observations(x) <- dplyr::bind_rows(observations(x1), observations(x2))

  # check duplicated ID's
  deploymentIDs <- purrr::pluck(deployments(x), "deploymentID")
  observationIDs <- purrr::pluck(observations(x), "observationsID")
  mediaIDs <- purrr::pluck(media(x), "mediaID")

  # set a vectorised function for creating hash function digests
  vdigest_algo_crc32 <- digest::getVDigest(algo = "crc32")

  # assume duplicates are between packages, not within
  if (any(duplicated(deploymentIDs))) {
    duplicatedID <- deploymentIDs[duplicated(deploymentIDs)]

    # give unique deploymentIDs to deployments
    deployments(x2) <-
      deployments(x2) %>%
        dplyr::mutate(
          deploymentID =
            dplyr::if_else(
              .data$deploymentID %in% duplicatedID,
              vdigest_algo_crc32(.data$deploymentID),
              .data$deploymentID
            )
        )

    # give unique deploymentIDs to observations
    observations(x2) <-
      observations(x2) %>%
      dplyr::mutate(
        deploymentID =
          dplyr::if_else(
            .data$deploymentID %in% duplicatedID,
            vdigest_algo_crc32(.data$deploymentID),
            .data$deploymentID
          )
      )

    # give unique deploymentIDs to media
    media(x2) <-
      media(x2) %>%
      dplyr::mutate(
        deploymentID =
          dplyr::if_else(
            .data$deploymentID %in% duplicatedID,
            vdigest_algo_crc32(.data$deploymentID),
            .data$deploymentID
          )
      )

    new_deploymentIDs <- purrr::pluck(deployments(x), "deploymentID")

    # new merge with unique deploymentID's
    deployments(x) <- dplyr::bind_rows(deployments(x1), deployments(x2))
    media(x) <- dplyr::bind_rows(media(x1), media(x2))
    observations(x) <- dplyr::bind_rows(observations(x1), observations(x2))

    # inform user
    cli::cli_alert_warning(
      c(
        paste0(
          "{.arg x1} and {.arg x2} have duplicated deploymentID's:",
          "{.val {duplicatedID}}"
        ),
        "v" = paste0(
          "Duplicated deploymentID's of {.arg x2} now have new uniqe",
          "deploymentID's: {.val {new_deploymentIDs}}"
        )
      ),
      class = "camtrapdp_warning_unique_deploymentID"
    )

  }

  if (any(duplicated(observationIDs))) {
    duplicatedID <- observationIDs[duplicated(observationIDs)]

    cli::cli_alert_warning(
      "message",
      class = "camtrapdp_warning_unique_observationID"
    )
  }

  if (any(duplicated(mediaIDs))) {
    duplicatedID <- mediaIDs[duplicated(mediaIDs)]

    cli::cli_alert_warning(
      "message",
      class = "camtrapdp_warning_unique_mediaID"
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
