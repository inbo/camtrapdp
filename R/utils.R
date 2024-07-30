#' Create columns, but only if they are missing
#'
#' Adds columns using [dplyr::mutate()], but only if they are absent in the
#' provided data frame.
#'
#' @inherit dplyr::mutate
#' @family helper functions
#' @noRd
#' @examples
#' # Column "space" is not present yet, so it is added
#' mutate_when_missing(cars, space = "The final frontier")
#' # Column "speed" is present, so it is not added
#' mutate_when_missing(cars, speed = "warp 9")
mutate_when_missing <- function(.data, ...) {
  dots <- substitute(list(...))[-1]
  cols_to_check <- names(sapply(dots, deparse))
  columns_to_add <- cols_to_check[!cols_to_check %in% colnames(.data)]
  if (!rlang::is_empty(columns_to_add)) {
    .data <- dplyr::mutate(.data, ...)
  }
  return(.data)
}

#' Replace deploymentID's
#'
#' Replaces deploymentIDs in deployments, media and observations..
#'
#' @inheritParams print.camtrapdp
#' @param old_deploymentID deploymentID's to be replaced. Either a single ID or
#' a vector of ID's.
#' @param new_deploymentID replacement deploymentID's. Must be of the same
#' length as `old_deploymentID`
#' @return `x` with replaced deploymentID's
#' @family helper functions
#' @noRd
#' @examples
#' x <- example_dataset()
#' x_replaced <-
#' replace_deploymentID(
#' x, c("00a2c20d", "29b7d356"), c("new_deploymentID1", "new_deploymentID2")
#' # Inspect results
#' deployments(x_replaced)$deploymentID
replace_deploymentID <- function(x, old_deploymentID, new_deploymentID) {

  # Check length
  length_old <- length(old_deploymentID)
  length_new <- length(new_deploymentID)
  if (length_old != length_new) {
    cli::cli_abort(
      c(
        "{.arg old_deploymentID} and {.arg new_deploymentID} must have the same
        length.",
        "x" = "Length of {.arg old_deploymentID}({.val {length_old}}) is not
        equal to length of {.arg new_deploymentID}({.val {length_new}})."
      ),
      class = "camtrapdp_error_length_deploymentID"
    )
  }

  # Create a named vector for replacements
  replacement_map <- setNames(new_deploymentID, old_deploymentID)

  # replace deploymentIDs in deployments
  deployments(x) <-
    deployments(x) %>%
    dplyr::mutate(
      deploymentID =
        dplyr::if_else(
          .data$deploymentID %in% old_deploymentID,
          unname(replacement_map[.data$deploymentID]),
          .data$deploymentID
        )
    )

  # replace deploymentIDs in observations
  observations(x) <-
    observations(x) %>%
    dplyr::mutate(
      deploymentID =
        dplyr::if_else(
          .data$deploymentID %in% old_deploymentID,
          unname(replacement_map[.data$deploymentID]),
          .data$deploymentID
        )
    )

  # replace deploymentIDs in media
  media(x) <-
    media(x) %>%
    dplyr::mutate(
      deploymentID =
        dplyr::if_else(
          .data$deploymentID %in% old_deploymentID,
          unname(replacement_map[.data$deploymentID]),
          .data$deploymentID
        )
    )

  return(x)
}

#' Replace mediaID's
#'
#' Replaces mediaIDs in media and observations
#'
#' @inheritParams print.camtrapdp
#' @param old_mediaID mediaID's to be replaced. Either a single ID or a vector
#' of ID's.
#' @param new_mediaID replacement mediaID's. Must be of the same
#' length as `old_mediaID`
#' @return `x` with replaced mediaID's
#' @family helper functions
#' @noRd
#' @examples
#' replace_mediaID(
#' example_dataset(),
#' c("07840dcc", "401386c7"),
#' c("new_mediaID1", "new_mediaID2"))
replace_mediaID <- function(x, old_mediaID, new_mediaID) {

  # Check length
  length_old <- length(old_mediaID)
  length_new <- length(new_mediaID)
  if (length_old != length_new) {
    cli::cli_abort(
      c(
        "{.arg old_mediaID} and {.arg new_mediaID} must have the same length.",
        "x" = "Length of {.arg old_mediaID}({.val {length_old}}) is not equal to
        length of {.arg new_mediaID}({.val {length_new}})."
      ),
      class = "camtrapdp_error_length_mediaID"
    )
  }

  # Create a named vector for replacements
  replacement_map <- setNames(new_mediaID, old_mediaID)

  # replace mediaIDs in media
  media(x) <-
    media(x) %>%
    dplyr::mutate(
      mediaID =
        dplyr::if_else(
          .data$mediaID %in% old_mediaID,
          unname(replacement_map[.data$mediaID]),
          .data$mediaID
        )
    )

  # replace mediaIDs in observations
  observations(x) <-
    observations(x) %>%
    dplyr::mutate(
      mediaID =
        dplyr::if_else(
          .data$mediaID %in% old_mediaID,
          unname(replacement_map[.data$mediaID]),
          .data$mediaID
        )
    )

  return(x)
}

#' Replace observationID's
#'
#' Replaces observationIDs in observations
#'
#' @inheritParams print.camtrapdp
#' @param old_observationID observationID's to be replaced. Either a single ID
#' or a vector of ID's.
#' @param new_observationID replacement observationID's. Must be of the same
#' length as `old_observationID`
#' @return `x` with replaced observationID's
#' @family helper functions
#' @noRd
#' @examples
#' x <- example_dataset() %>% filter_observations(
#' observationID %in% c("705e6036", "07840dcc_1", "401386c7_1")
#' )
#' x_replaced <- replace_observationID(
#' x, c("705e6036", "07840dcc_1"), c("newID1", "newID2")
#' )
#' # Inspect values
#' observations(x_replaced)$observationID
replace_observationID <- function(x, old_observationID, new_observationID) {

  # Check length
  length_old <- length(old_observationID)
  length_new <- length(new_observationID)
  if (length_old != length_new) {
    cli::cli_abort(
      c(
        "{.arg old_observationID} and {.arg new_observationID} must have the
        same length.",
        "x" = "Length of {.arg old_observationID}({.val {length_old}}) is not
        equal to length of {.arg new_observationID}({.val {length_new}})."
      ),
      class = "camtrapdp_error_length_observationID"
    )
  }

  # Create a named vector for replacements
  replacement_map <- setNames(new_observationID, old_observationID)

  # replace observationIDs in observations
  observations(x) <-
    observations(x) %>%
    dplyr::mutate(
      observationID =
        dplyr::if_else(
          .data$observationID %in% old_observationID,
          unname(replacement_map[.data$observationID]),
          .data$observationID
        )
    )

  return(x)
}

#' Create hashes
#'
#' Set a vectorised function for creating hash function digests, using algorithm
#' "crc32".
#'
#' @param object vector or character string
#' @return hash summary as a character vector of the same length as the input
#' @family helper functions
#' @noRd
#' @examples
#' vdigest_crc32(c("00a2c20d", "29b7d356"))
vdigest_crc32 <- function(object) {
  vdigest_crc32 <- digest::getVDigest(algo = "crc32")
  return(vdigest_crc32(object))
}

#' Replace duplicated ID's when merging Camera Trap Data packages
#'
#' Replaces duplicated deploymentID's, mediaID's and observationID's between
#' two Camera Trap Data Packages with hashes generated by `vdigest_crc32`.
#' Used in `merge_camtrapdp()`.
#'
#'
#' @param x1,x2 Camera Trap Data Package objects, as returned by
#' `read_camtrapdp()`).
#' @return `x2` with duplicated ID's (compared to `x1`) replaced with hashes.
#' @family helper functions
#' @noRd
replace_duplicatedIDs <- function(x1, x2) {
  x <- x1

  # merge resources
  deployments(x) <- dplyr::bind_rows(deployments(x1), deployments(x2))
  media(x) <- dplyr::bind_rows(media(x1), media(x2))
  observations(x) <- dplyr::bind_rows(observations(x1), observations(x2))

  # get ID's
  deploymentIDs <- purrr::pluck(deployments(x), "deploymentID")
  mediaIDs <- purrr::pluck(media(x), "mediaID")
  observationIDs <- purrr::pluck(observations(x), "observationID")

  if (any(duplicated(deploymentIDs))) {
    # replace duplicated deploymentID's in `x2`
    duplicated_deploymentID <- deploymentIDs[duplicated(deploymentIDs)]
    replacement_deploymentID <- vdigest_crc32(duplicated_deploymentID)
    x2 <- replace_deploymentID(
      x2, duplicated_deploymentID, replacement_deploymentID
    )

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
          "{.val {replacement_deploymentID}} respectively."
        )
      ),
      class = "camtrapdp_warning_unique_deploymentID"
    )
  }

  if (any(duplicated(mediaIDs))) {
    # replace duplicated mediaID's in `x2`
    duplicated_mediaID <- mediaIDs[duplicated(mediaIDs)]
    replacement_mediaID <- vdigest_crc32(duplicated_mediaID)
    x2 <- replace_mediaID(x2, duplicated_mediaID, replacement_mediaID)

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
          "{.val {replacement_mediaID}} respectively."
        )
      ),
      class = "camtrapdp_warning_unique_mediaID"
    )
  }

  if (any(duplicated(observationIDs))) {
    # replace duplicated deploymentID's in `x2`
    duplicated_observationID <- observationIDs[duplicated(observationIDs)]
    replacement_observationID <- vdigest_crc32(duplicated_observationID)
    x2 <- replace_observationID(
      x2, duplicated_observationID, replacement_observationID
    )

    # new merge with unique observationID's
    observations(x) <- dplyr::bind_rows(observations(x1), observations(x2))
    # inform user

    cli::cli_alert_warning(
      c(
        paste(
          "{.arg x1} and {.arg x2} must have unique observationID's.\n",
          "{.arg x1} and {.arg x2} have duplicated observationID's:",
          "{.val {duplicated_observationID}}.\n",
          "Duplicated observationID's of {.arg x2} are now replaced by",
          "{.val {replacement_observationID}} respectively."
        )
      ),
      class = "camtrapdp_warning_unique_observationID"
    )
  }

  return(x2)
}

#' Normalize list elements
#'
#' Converts each list element to a named vector with consistent handling of
#' missing values (NA), using determined `unique_names`.
#'
#' @param data_list list to be normalized
#' @param unique_names the names that the list must have
#'
#' @return named vector with all `unique_names` present
#' @family helper functions
#' @noRd
#' @examples
#' data_list <- list(
#' title = "Peter Desmet",
#' email = "peter.desmet@inbo.be",
#' organization = "Research Institute for Nature and Forest (INBO)"
#' )
#' unique_names <- c("title", "email", "path", "role", "organization")
#' normalize_list(data_list, all_fields)
normalize_list <- function(data_list, unique_names) {
  vector <- purrr::map_vec(
    unique_names,
    ~ ifelse(!is.null(data_list[[.x]]), data_list[[.x]], NA)
  )
  names(vector) <- unique_names
  return(vector)
}
