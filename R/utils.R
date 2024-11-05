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

#' Lists the names of additional resources in a Camera Trap Data Package
#'
#' @inheritParams print.camtrapdp
#' @return Character vector with the additional resource names.
#' @family helper functions
#' @noRd
additional_resources <- function(x) {
  camtrapdp_resource_names <- c("deployments", "media", "observations")
  resource_names <- frictionless::resources(x)
  resource_names[!resource_names %in% camtrapdp_resource_names]
}

#' Merge additional resources
#'
#' Merges resources that are different from the required Camera Trap Data
#' Package resources (deployments, media and observations). Resources with the
#' same name are not combined, but prefixes are added to the resource names.
#'
#' @param xy_merged Merged Camera Trap Data Package
#' @inheritParams merge_camtrapdp
#'
#' @return `xy_merged` Merged Camera Trap Data Package
#' @family helper functions
#' @noRd
merge_additional_resources <- function(xy_merged, x, y, prefix) {
  camtrapdp_resources <- c("deployments", "media", "observations")
  x_resource_names <- frictionless::resources(x)
  y_resource_names <- frictionless::resources(y)
  x_additional_resources <-
    x_resource_names[!x_resource_names %in% camtrapdp_resources]
  y_additional_resources <-
    y_resource_names[!y_resource_names %in% camtrapdp_resources]

  all_additional_resources <- c(x_additional_resources, y_additional_resources)

  if (length(all_additional_resources) > 0) {
    duplicated_resources <- duplicated(all_additional_resources)
    duplicated_names <- all_additional_resources[duplicated_resources]

    # Add prefixes to resource names that are not unique
    if (any(duplicated_resources)) {
      purrr::map(duplicated_names, function(duplicated_name) {
        xy_index <-
          which(purrr::map(xy_merged$resources, "name") == duplicated_name)
        y_index <- which(purrr::map(y$resources, "name") == duplicated_name)
        xy_merged$resources[[xy_index]]$name <-
          paste0(prefix[1], "_", duplicated_name)
        y$resources[[y_index]]$name <- paste0(prefix[2], "_", duplicated_name)
        xy_merged$resources <<- append(xy_merged$resources, y$resources[y_index])
      })
    }

    # Add unique resources from y
    y_unique_resources <-
      y_additional_resources[!y_additional_resources %in% duplicated_names]
    purrr::map(y_unique_resources, function(resource_name) {
      index <- which(purrr::map(y$resources, "name") == resource_name)
      resource <- y$resources[index]
      xy_merged$resources <<- append(xy_merged$resources, resource)
    })
  }

  return(xy_merged)
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
#'   title = "Peter Desmet",
#'   email = "peter.desmet@inbo.be",
#'   organization = "Research Institute for Nature and Forest (INBO)"
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
#' is_subset(element1, element2)
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

#' Create list of contributors in EML format
#'
#' @param contributor_list List of contributors
#' @return List of contributors as emld responsibleParty objects.
#' @family helper functions
#' @noRd
create_eml_contributors <- function(contributor_list) {
  purrr::map(contributor_list, ~ EML::set_responsibleParty(
    givenName = .$first_name,
    surName = .$last_name,
    organizationName = .$organization, # Discouraged by EML, but used by IPT
    email = .$email,
    userId = if (!is.na(.$orcid)) {
      list(directory = "https://orcid.org/", .$orcid)
    } else {
      NULL
    },
    onlineUrl = .$path
  ))
}

#' Replace NULL values recursively
#'
#' Replaces NULL values with NA by recursively iterating through each element of
#' the input list.
#'
#' @param x A nested list.
#' @return A nested list identical to the input x, but with all NULL values
#' replaced by NA.
#' @family helper functions
#' @noRd
replace_null_recursive <- function(x) {
  purrr::map(x, function(element) {
    if (is.list(element) && !is.null(element)) {
      replace_null_recursive(element)
    } else {
      ifelse(is.null(element), NA, element)
    }
  })
}
