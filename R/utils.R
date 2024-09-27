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

#' Expand columns
#'
#' Expands a data frame with columns. Added columns will have `NA_character_`
#' values, existing columns of the same name will not be overwritten.
#'
#' @param df A data frame.
#' @param colnames A character vector of column names.
#' @return Data frame expanded with columns that were not yet present.
#' @family helper functions
#' @noRd
expand_cols <- function(df, colnames) {
  cols_to_add <- setdiff(colnames, colnames(df))
  df[, cols_to_add] <- NA_character_
  return(df)
}

#' Check for duplicated IDs
#'
#' Checks for duplicated IDs in two Camera Trap Data Package objects combined.
#'
#' @param x1,x2 Camera Trap Data Package objects (as returned by
#' `read_camtrapdp()`), to be coerced to one.
#' @return List with logical for each type of ID, that indicates whether that
#' ID type has duplicates between x1 and x2.
#' @family helper functions
#' @noRd
check_duplicate_ids <- function(x1, x2) {
  result = list(
    deploymentID = FALSE, mediaID = FALSE, observationID = FALSE,
    eventID = FALSE)

  deploymentIDs <- c(
    unique(purrr::pluck(deployments(x1), "deploymentID")),
    unique(purrr::pluck(deployments(x2), "deploymentID"))
  )
  mediaIDs <- c(
    unique(purrr::pluck(media(x1), "mediaID")),
    unique(purrr::pluck(media(x2), "mediaID"))
  )
  observationIDs <- c(
    unique(purrr::pluck(observations(x1), "observationID")),
    unique(purrr::pluck(observations(x2), "observationID"))
  )
  eventIDs <- c(
    unique(purrr::pluck(media(x1), "eventID")),
    unique(purrr::pluck(media(x2), "eventID"))
  )

  # Check for duplicates
  if (any(duplicated(deploymentIDs))) {result$deploymentID <- TRUE}
  if (any(duplicated(mediaIDs))) {result$mediaID <- TRUE}
  if (any(duplicated(observationIDs))) {result$observationID <- TRUE}
  if (any(duplicated(eventIDs))) {result$eventID <- TRUE}

  return(result)
}

#' Add prefix to identifiers with duplicates
#'
#' Adds prefix to all values of each identifier that has duplicates.
#'
#' @inheritParams print.camtrapdp
#' @param prefix The prefix to add to the IDs.
#' @param results_duplicate_ids Output generated with `check_duplicate_ids()`.
#' List with logical for each type of ID, that indicates whether that ID type
#' has duplicates.
#' @return `x`
#' @family helper functions
#' @noRd
#' @examples
#' results_duplicate_ids <- list(deploymentID = TRUE, mediaID = TRUE,
#' observationID = TRUE, eventID = TRUE)
#' x <- add_prefix(example_dataset(), results_duplicate_ids, prefix = ".x")
add_prefix <- function(x, results_duplicate_ids) {

  # deploymentID
  if (results_duplicate_ids$deploymentID) {
    # Add prefix to deploymentIDs in deployments
    deployments(x) <-
      deployments(x) %>%
      dplyr::mutate(deploymentID = paste0(prefix, .data$deploymentID))

    # Add prefix to deploymentIDs in observations
    observations(x) <-
      observations(x) %>%
      dplyr::mutate(deploymentID = paste0(prefix, .data$deploymentID))

    # Add prefix to deploymentIDs in media
    media(x) <-
      media(x) %>%
      dplyr::mutate(deploymentID = paste0(prefix, .data$deploymentID))
  }

  # mediaID
  if (results_duplicate_ids$mediaID) {
    # Add prefix to mediaIDs in media
    media(x) <-
      media(x) %>%
      dplyr::mutate(
        mediaID = ifelse(
          !is.na(.data$mediaID), paste0(prefix, .data$mediaID), NA
          )
      )

    # Add prefix to mediaIDs in observations
    observations(x) <-
      observations(x) %>%
      dplyr::mutate(
        mediaID = ifelse(
          !is.na(.data$mediaID), paste0(prefix, .data$mediaID), NA
        )
      )
  }

  # observationID
  if (results_duplicate_ids$observationID) {
    # Add prefix to observationIDs in observations
    observations(x) <-
      observations(x) %>%
      dplyr::mutate(observationID = paste0(prefix, .data$observationID))
  }

  # eventID
  if (results_duplicate_ids$eventID) {
    # Add prefix to eventIDs in media
    media(x) <-
      media(x) %>%
      dplyr::mutate(
        eventID = ifelse(
          !is.na(.data$eventID), paste0(prefix, .data$eventID), NA
        )
      )

    # Add prefix to eventIDs in observations
    observations(x) <-
      observations(x) %>%
      dplyr::mutate(
        eventID = ifelse(
          !is.na(.data$eventID), paste0(prefix, .data$eventID), NA
        )
      )
  }

  return(x)
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
