#' Merge Camera Trap Data packages
#'
#' @param x,y Camera Trap Data Package objects (as returned by
#' `read_camtrapdp()`), to be coerced to one.
#' @param prefix If there are duplicate IDs between x and y, prefixes will be
#' added to all the values of each identifier with duplicates, to disambiguate
#' them. Should be a character vector of length 2. By default, the prefixes are
#' the ID's of the Data Package.
#' @return `x`
#' @family transformation functions
#' @export
#' @section Merging details:
#' Deployments, media and observations are combined. If there are duplicate IDs
#' between x and y, prefixes will be added to all the values of each identifier
#' with duplicates, to disambiguate them.
#' The following properties are set:
#' - **name**: Set to NA.
#' - **id**: Set to NULL.
#' - **created**: Set to current timestamp.
#' - **title**: Set to NA.
#' - **contributors**: A combination is made and duplicates are removed.
#' - **description**: A combination is made.
#' - **version**: Set to 1.0.
#' - **keywords**: A combination is made and duplicates are removed.
#' - **image**: Set to NULL.
#' - **homepage**: Set to NULL.
#' - **sources**: A combination is made and duplicates are removed.
#' - **licenses**: A combination is made and duplicates are removed.
#' - **bibliographicCitation**: Set to NULL.
#' - **project**: List of the projects.
#' - **coordinatePrecision**: Set to the least precise `coordinatePrecision`.
#' - **spatial**: Reset based on the new deployments.
#' - **temporal**: Reset based on the new deployments.
#' - **taxonomic**: A combination is made and duplicates are removed.
#' - **relatedIdentifiers**: A combination is made and duplicates are removed.
#' - **references**: A combination is made and duplicates are removed.
#' @section Merging multiple Camera Trap Data Packages:
#' `merge_camtrapdp()` can be used in a pipe to merge multiple camtrap DP.
#' - x %>% merge_camtrapdp(y) %>% merge_camtrapdp(z)
#' @examples
#' x <- example_dataset() %>%
#'   filter_deployments(deploymentID %in% c("00a2c20d", "29b7d356"))
#' y <- example_dataset() %>%
#'   filter_deployments(deploymentID %in% c("577b543a", "62c200a9"))
#' x$id <- "1"
#' y$id <- "2"
#' xy_merged <- merge_camtrapdp(x, y)
merge_camtrapdp <- function(x, y, prefix = c(x$id, y$id)) {
  check_camtrapdp(x)
  check_camtrapdp(y)

  if (!is.null(x$id) & !is.null(y$id)) {
    if (x$id == y$id) {
      cli::cli_abort(
        c(
          paste0(
            "{.arg x} and {.arg y} should be different Camera Trap Data",
            "Package objects with unique identifiers."
          ),
          x = "{.arg x} and {.arg y} have the same id: {.value x$id}"
        ),
        class = "camtrapdp_error_camtrapdpid_duplicated"
      )
    }
  }

  # check if identifiers have duplicates
  results_duplicate_ids <- check_duplicate_ids(x, y)

  # Add prefix to identifiers with duplicates
  if (TRUE %in% results_duplicate_ids) {

    if (!is.character(prefix) || length(prefix) != 2) {
      cli::cli_abort(
        c(
          paste(
            "{.arg prefix} must be a character vector of length 2, not",
            "a {class(prefix)} object of length {length(prefix)}."
          )
        ),
        class = "camtrapdp_error_prefix_invalid"
      )
    }

    if (any(is.na(prefix))) {
      cli::cli_abort(
        "{.arg prefix} can't be 'NA'.",
        class = "camtrapdp_error_prefix_NA"
      )
    }

    x <- add_prefix(x, results_duplicate_ids, paste0(prefix[1], "_"))
    y <- add_prefix(y, results_duplicate_ids, paste0(prefix[2], "_"))
  }

  # Merge camtrap DP resources
  xy_merged <- x
  deployments(xy_merged) <- dplyr::bind_rows(deployments(x), deployments(y))
  media(xy_merged) <- dplyr::bind_rows(media(x), media(y))
  observations(xy_merged) <- dplyr::bind_rows(observations(x), observations(y))

  # Merge additional resources
  camtrapdp_resources <- c("deployments", "media", "observations")
  x_resource_names <- purrr::map(x$resources, ~ .[["name"]]) %>% unlist()
  y_resource_names <- purrr::map(x$resources, ~ .[["name"]]) %>% unlist()
  x_additional_resources <-
    x_resource_names[!x_resource_names %in% camtrapdp_resources]
  y_additional_resources <-
    y_resource_names[!y_resource_names %in% camtrapdp_resources]

  # Merge/update metadata
  xy_merged$name <- NA
  xy_merged$id <- NULL
  xy_merged$created <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  xy_merged$title <- NA
  xy_merged$contributors <- remove_duplicates(c(x$contributors, y$contributors))
  xy_merged$description <- paste(x$description, y$description, sep = "/n")
  xy_merged$version <- "1.0"
  xy_merged$keywords <- unique(c(x$keywords, y$keywords))
  xy_merged$image <- NULL
  xy_merged$homepage <- NULL
  xy_merged$sources <- remove_duplicates(c(x$sources, y$sources))
  xy_merged$licenses <- remove_duplicates(c(x$licenses, y$licenses))
  xy_merged$project <- list(x$project, y$project)
  xy_merged$bibliographicCitation <- NULL
  xy_merged$coordinatePrecision <-
    max(x$coordinatePrecision, y$coordinatePrecision, na.rm = TRUE)

  if (!is.null(x$id)) {
    relatedIdentifiers_x <- list(
      relationType = "IsDerivedFrom",
      relatedIdentifier = as.character(x$id),
      resourceTypeGeneral = "Data package",
      relatedIdentifierType = "id"
    )
  } else {
    relatedIdentifiers_x <- list()
  }
  if (!is.null(y$id)) {
    relatedIdentifiers_y <- list(
      relationType = "IsDerivedFrom",
      relatedIdentifier = as.character(y$id),
      resourceTypeGeneral = "Data package",
      relatedIdentifierType = "id"
    )
  } else {
    relatedIdentifiers_y <- list()
  }
  new_relatedIdentifiers <- list(relatedIdentifiers_x, relatedIdentifiers_y)
  xy_merged$relatedIdentifiers <- remove_duplicates(
    c(x$relatedIdentifiers, y$relatedIdentifiers, new_relatedIdentifiers)
  )

  xy_merged$references <- unique(c(x$references, y$references))
  xy_merged$directory <- "."

  xy_merged <- xy_merged %>%
    update_spatial() %>%
    update_temporal() %>%
    update_taxonomic()

  return(xy_merged)
}
