#' Merge two Camera Trap Data Packages
#'
#' Merges two Camera Trap Data Package objects into one.
#'
#' @param x,y Camera Trap Data Package objects, as returned by
#'  [read_camtrapdp()].
#' @return A single Camera Trap Data Package object that is the combination of
#'   `x` and `y`.
#' @family transformation functions
#' @export
#' @section Transformation details:
#'
#' Both `x` and `y` must have a unique dataset name `x$name` and `y$name`.
#' This name is used to prefix identifiers in the data that occur in both
#' datasets.
#' For example:
#' - `x` contains `deploymentID`s `c("a", "b")`.
#' - `y` contains `deploymentID`s `c("b", "c")`.
#' - Then merged `xy` will contain `deploymentID`s `c("a", "x_b", "y_b", "c")`.
#'
#' Data are merged as follows:
#' - Deployments are combined, with `deploymentID` kept unique.
#' - Media are combined, with `mediaID`, `deploymentID` and `eventID` kept
#' unique.
#' - Observations are combined, with `observationID`, `deploymentID`, `mediaID`
#' and `eventID` kept unique.
#' - Additional resources are retained, with the resource name kept unique.
#'
#' Metadata properties are merged as follows:
#' - **name**: Set to `NA`.
#' - **id**: Removed.
#' - **created**: Set to current timestamp.
#' - **title**: Set to `NA`.
#' - **contributors**: A combination is made and duplicates are removed.
#' - **description**: A combination is made.
#' - **version**: Set to `1.0`.
#' - **keywords**: A combination is made and duplicates are removed.
#' - **image**: Removed.
#' - **homepage**: Removed.
#' - **sources**: A combination is made and duplicates are removed.
#' - **licenses**: A combination is made and duplicates are removed.
#' - **bibliographicCitation**: Removed.
#' - **project**: List of the projects.
#' - **coordinatePrecision**: Set to the least precise `coordinatePrecision`.
#' - **spatial**: Reset based on the new deployments.
#' - **temporal**: Reset based on the new deployments.
#' - **taxonomic**: A combination is made and duplicates are removed.
#' - **relatedIdentifiers**: A combination is made and duplicates are removed.
#' - **references**: A combination is made and duplicates are removed.
#' @examples
#' x <- example_dataset() %>%
#'   filter_deployments(deploymentID %in% c("00a2c20d", "29b7d356"))
#' y <- example_dataset() %>%
#'   filter_deployments(deploymentID %in% c("577b543a", "62c200a9"))
#' x$id <- "x"
#' y$id <- "y"
#' merge_camtrapdp(x, y)
merge_camtrapdp <- function(x, y) {
  check_camtrapdp(x)
  check_camtrapdp(y)

  # Check names
  check_name <- function(name, arg) {
    if (is.null(name) || is.na(name) || !is.character(name)) {
      cli::cli_abort(
        c(
          "{.arg {arg}} must have a unique (character) name.",
          "i" = "Assign one to {.field {arg}$name}."
        ),
        class = "camtrapdp_error_name_invalid"
      )
    }
  }
  check_name(x$name, "x")
  check_name(y$name, "y")
  if (x$name == y$name) {
    cli::cli_abort(
      c(
        "{.arg x} and {.arg y} must have different unique names.",
        "x" = "{.field x$name} and {.field y$name} currently have the same
               value: {.val {x$name}}."
      ),
      class = "camtrapdp_error_name_duplicated"
    )
  }
  prefixes <- c(x$name, y$name)

  # Add prefix to duplicate identifiers in data
  x_original <- x
  x <- prefix_identifiers(x, y, x$id)
  y <- prefix_identifiers(y, x_original, y$id)

  # Merge Camera Trap DP resources
  xy_merged <- x
  deployments(xy_merged) <- dplyr::bind_rows(deployments(x), deployments(y))
  media(xy_merged) <- dplyr::bind_rows(media(x), media(y))
  observations(xy_merged) <- dplyr::bind_rows(observations(x), observations(y))

  # Merge additional resources
  xy_merged <- merge_additional_resources(xy_merged, x, y, c(x$id, y$id))

  # Merge/update metadata
  xy_merged$name <- NULL
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
