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

#' Replace deploymentIDs
#'
#' Replaces deploymentIDs in deployments, media and observations..
#'
#' @inheritParams print.camtrapdp
#' @param old_deploymentID deploymentIDs to be replaced. Either a single ID or
#' a vector of IDs.
#' @param new_deploymentID replacement deploymentIDs. Must be of the same
#' length as `old_deploymentID`
#' @return `x` with replaced deploymentIDs
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

  # # Check length
  # length_old <- length(old_deploymentID)
  # length_new <- length(new_deploymentID)
  # if (length_old != length_new) {
  #   cli::cli_abort(
  #     c(
  #       "{.arg old_deploymentID} and {.arg new_deploymentID} must have the same
  #       length.",
  #       "x" = "Length of {.arg old_deploymentID}({.val {length_old}}) is not
  #       equal to length of {.arg new_deploymentID}({.val {length_new}})."
  #     ),
  #     class = "camtrapdp_error_length_deploymentID"
  #   )
  # }

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

#' Replace mediaIDs
#'
#' Replaces mediaIDs in media and observations
#'
#' @inheritParams print.camtrapdp
#' @param old_mediaID mediaIDs to be replaced. Either a single ID or a vector
#' of IDs.
#' @param new_mediaID replacement mediaIDs. Must be of the same
#' length as `old_mediaID`
#' @return `x` with replaced mediaIDs
#' @family helper functions
#' @noRd
#' @examples
#' replace_mediaID(
#' example_dataset(),
#' c("07840dcc", "401386c7"),
#' c("new_mediaID1", "new_mediaID2"))
replace_mediaID <- function(x, old_mediaID, new_mediaID) {

  # # Check length
  # length_old <- length(old_mediaID)
  # length_new <- length(new_mediaID)
  # if (length_old != length_new) {
  #   cli::cli_abort(
  #     c(
  #       "{.arg old_mediaID} and {.arg new_mediaID} must have the same length.",
  #       "x" = "Length of {.arg old_mediaID}({.val {length_old}}) is not equal to
  #       length of {.arg new_mediaID}({.val {length_new}})."
  #     ),
  #     class = "camtrapdp_error_length_mediaID"
  #   )
  # }

  # Create a named vector for replacements
  replacement_map <- new_mediaID
  names(replacement_map) <- old_mediaID

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

#' Replace observationIDs
#'
#' Replaces observationIDs in observations
#'
#' @inheritParams print.camtrapdp
#' @param old_observationID observationIDs to be replaced. Either a single ID
#' or a vector of IDs.
#' @param new_observationID replacement observationIDs. Must be of the same
#' length as `old_observationID`.
#' @return `x` with replaced observationIDs.
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

  # # Check length
  # length_old <- length(old_observationID)
  # length_new <- length(new_observationID)
  # if (length_old != length_new) {
  #   cli::cli_abort(
  #     c(
  #       "{.arg old_observationID} and {.arg new_observationID} must have the
  #       same length.",
  #       "x" = "Length of {.arg old_observationID}({.val {length_old}}) is not
  #       equal to length of {.arg new_observationID}({.val {length_new}})."
  #     ),
  #     class = "camtrapdp_error_length_observationID"
  #   )
  # }

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
#' @param object The object to be digested. This can be any R object that can be
#' serialized into a raw vector.
#' @return Hash summary as a character vector of the same length as the input
#' @family helper functions
#' @noRd
#' @examples
#' vdigest_crc32(c("00a2c20d", "29b7d356"))
vdigest_crc32 <- function(object) {
  vdigest_crc32 <- digest::getVDigest(algo = "crc32")
  return(vdigest_crc32(object))
}

#' Replace duplicated IDs when merging Camera Trap Data packages
#'
#' Replaces duplicated deploymentIDs, mediaIDs and observationIDs between
#' two Camera Trap Data Packages with hashes generated by `vdigest_crc32`.
#' Used in `merge_camtrapdp()`.
#'
#'
#' @param x1,x2 Camera Trap Data Package objects, as returned by
#' `read_camtrapdp()`).
#' @return `x2` with duplicated IDs (compared to `x1`) replaced with hashes.
#' @family helper functions
#' @noRd
replace_duplicatedIDs <- function(x1, x2) {
  x <- x1

  # merge resources
  deployments(x) <- dplyr::bind_rows(deployments(x1), deployments(x2))
  media(x) <- dplyr::bind_rows(media(x1), media(x2))
  observations(x) <- dplyr::bind_rows(observations(x1), observations(x2))

  # get IDs
  deploymentIDs <- purrr::pluck(deployments(x), "deploymentID")
  mediaIDs <- purrr::pluck(media(x), "mediaID")
  observationIDs <- purrr::pluck(observations(x), "observationID")

  if (any(duplicated(deploymentIDs))) {
    # replace duplicated deploymentIDs in `x2`
    duplicated_deploymentID <- deploymentIDs[duplicated(deploymentIDs)]
    replacement_deploymentID <- vdigest_crc32(duplicated_deploymentID)
    x2 <- replace_deploymentID(
      x2, duplicated_deploymentID, replacement_deploymentID
    )

    # new merge with unique deploymentIDs
    deployments(x) <- dplyr::bind_rows(deployments(x1), deployments(x2))
    media(x) <- dplyr::bind_rows(media(x1), media(x2))
    observations(x) <- dplyr::bind_rows(observations(x1), observations(x2))

    # inform user
    new_deploymentIDs <- vdigest_crc32(duplicated_deploymentID)
    cli::cli_alert_warning(
      c(
        paste(
          "{.arg x1} and {.arg x2} must have unique deploymentIDs.\n",
          "{.arg x1} and {.arg x2} have duplicated deploymentIDs:",
          "{.val {duplicated_deploymentID}}.\n",
          "Duplicated deploymentIDs of {.arg x2} are now replaced by",
          "{.val {replacement_deploymentID}} respectively."
        )
      ),
      class = "camtrapdp_warning_unique_deploymentID"
    )
  }

  if (any(duplicated(mediaIDs))) {
    # replace duplicated mediaIDs in `x2`
    duplicated_mediaID <- mediaIDs[duplicated(mediaIDs)]
    replacement_mediaID <- vdigest_crc32(duplicated_mediaID)
    x2 <- replace_mediaID(x2, duplicated_mediaID, replacement_mediaID)

    # new merge with unique mediaIDs
    media(x) <- dplyr::bind_rows(media(x1), media(x2))
    observations(x) <- dplyr::bind_rows(observations(x1), observations(x2))

    # inform user
    new_mediaIDs <- vdigest_crc32(duplicated_mediaID)
    cli::cli_alert_warning(
      c(
        paste(
          "{.arg x1} and {.arg x2} must have unique mediaIDs.\n",
          "{.arg x1} and {.arg x2} have duplicated mediaIDs:",
          "{.val {duplicated_mediaID}}.\n",
          "Duplicated mediaIDs of {.arg x2} are now replaced by",
          "{.val {replacement_mediaID}} respectively."
        )
      ),
      class = "camtrapdp_warning_unique_mediaID"
    )
  }

  if (any(duplicated(observationIDs))) {
    # replace duplicated deploymentIDs in `x2`
    duplicated_observationID <- observationIDs[duplicated(observationIDs)]
    replacement_observationID <- vdigest_crc32(duplicated_observationID)
    x2 <- replace_observationID(
      x2, duplicated_observationID, replacement_observationID
    )

    # new merge with unique observationIDs
    observations(x) <- dplyr::bind_rows(observations(x1), observations(x2))
    # inform user

    cli::cli_alert_warning(
      c(
        paste(
          "{.arg x1} and {.arg x2} must have unique observationIDs.\n",
          "{.arg x1} and {.arg x2} have duplicated observationIDs:",
          "{.val {duplicated_observationID}}.\n",
          "Duplicated observationIDs of {.arg x2} are now replaced by",
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
#' @param data_list list to be normalized.
#' @param unique_names the names that the list must have.
#' @return named vector with all `unique_names` present.
#' @family helper functions
#' @noRd
#' @examples
#' data_list <- list(
#' title = "Peter Desmet",
#' email = "peter.desmet@inbo.be",
#' organization = "Research Institute for Nature and Forest (INBO)"
#' )
#' unique_names <- c("title", "email", "path", "role", "organization")
#' normalize_list(data_list, unique_names)
normalize_list <- function(data_list, unique_names) {
  vector <- purrr::map_vec(
    unique_names,
    ~ ifelse(!is.null(data_list[[.x]]), data_list[[.x]], NA)
  )
  names(vector) <- unique_names
  return(vector)
}

#' Check if one element is equal to or a subset of another and vice versa
#'
#'
#' @param element1,element2 elements to compare.
#' @return logical.
#' @family helper functions
#' @noRd
#' @examples
#' element1 <- list(
#'   title = "Peter Desmet",
#'   email = "peter.desmet@inbo.be",
#'   organization = "Research Institute for Nature and Forest (INBO)"
#' )
#' element2 <- list(
#'   title = "Peter Desmet",
#'   email = "peter.desmet@inbo.be",
#'   path = "https://orcid.org/0000-0002-8442-8025",
#'   role = "principalInvestigator",
#'   organization = "Research Institute for Nature and Forest (INBO)"
#' )
#' is.subset(element1, element2)
is_subset <- function(element1, element2) {
  all(
    purrr::map_vec(names(element1), function(field) {
      if (is.na(element1[[field]])) {
        TRUE
      } else if (is.na(element2[[field]])) {
        TRUE
      } else {
        element1[[field]] == element2[[field]]
      }
    })
  )
}

#' Update a list of unique elements
#'
#' Updates a list of unique elements by adding a new element if it is not a
#' subset of any existing element in the list. It also removes any elements that
#' are subsets of the new element.
#'
#' @param unique_data A list of elements. Each element must be a vector or
#' list.
#' @param current_element A vector or list representing the current element to
#' be added to the list.
#' @return `unique_data`, a list of unique elements updated with the current
#' element, ensuring no element is a subset of another.
#' @family helper functions
#' @noRd
#' @examples
#' unique_data <- list(c(1, 2, 3), c(4, 5), c(1, 2, 3, 4, 5))
#' current_element <- c(2, 3)
#' update_unique(unique_data, current_element)
update_unique <- function(unique_data, current_element) {
  # Check if current element is already a subset of any element in unique_data
  is_already_present <-
    any(
      purrr::map_lgl(unique_data, ~ is_subset(current_element, .x))
    )
  if (!is_already_present) {
    # Remove subsets from unique_data
    subsets_to_remove <-
      purrr::map_lgl(unique_data, ~ is_subset(.x, current_element))
    unique_data <-
      unique_data[!subsets_to_remove] %>%
      c(list(current_element))
  }
  return(unique_data)
}

#' Remove duplicates and subsets
#'
#' Removes duplicate and subset elements from a list of lists. Elements are
#' considered subsets if all their non-NA fields match.
#'
#' @param data_list List of lists, where each inner list represents an element
#' with named fields.
#' @return List of lists with duplicates and subsets removed.
#' @family helper functions
#' @noRd
#' @examples
#' data_list <- list(
#' list(
#'   title = "Axel Neukermans",
#'   email = "axel.neukermans@inbo.be",
#'   path = "https://orcid.org/0000-0003-0272-9180",
#'   role = "contributor",
#'   organization = "Research Institute for Nature and Forest (INBO)"
#' ),
#' list(
#'   title = "Peter Desmet",
#'   email = "peter.desmet@inbo.be",
#'   path = "https://orcid.org/0000-0002-8442-8025",
#'   role = "principalInvestigator",
#'   organization = "Research Institute for Nature and Forest (INBO)"
#' ),
#' list(
#'   title = "Research Institute for Nature and Forest (INBO)",
#'   path = "https://inbo.be",
#'   role = "rightsHolder"
#' ),
#' list(
#'   title = "Peter Desmet",
#'   email = "peter.desmet@inbo.be",
#'   organization = "Research Institute for Nature and Forest (INBO)"
#' ),
#' list(
#'   title = "Research Institute for Nature and Forest (INBO)",
#'   path = "https://inbo.be",
#'   role = "rightsHolder"
#' )
#' )
#' remove_duplicates(data_list)
remove_duplicates <- function(data_list) {
  # Find all unique field names
  unique_names <-
    purrr::map(data_list, names) %>%
    unlist() %>%
    unique()

  # Normalize all elements
  normalized_data <-
    purrr::map(data_list, ~ normalize_list(.x, unique_names))

  # Reduce the list to unique elements using update_unique()
  unique_data <- Reduce(update_unique, normalized_data, init = list())

  # Convert back to original list format and remove NA's
  unique_data_list <-
    purrr::map(unique_data, function(x) {
      x <- as.list(x)
      x[!sapply(x, is.na)]
    })

  return(unique_data_list)
}
