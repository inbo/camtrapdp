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
#' - **name**: Removed.
#' - **id**: Removed.
#' - **created**: Set to current timestamp.
#' - **title**: Removed.
#' - **contributors**: Combined, with duplicates removed.
#' - **description**: Combined as two paragraphs.
#' - **version**: Set to `1.0`.
#' - **keywords**: Combined, with duplicates removed.
#' - **image**: Removed.
#' - **homepage**: Removed.
#' - **sources**: Combined, with duplicates removed.
#' - **licenses**: Combined, with duplicates removed.
#' - **bibliographicCitation**: Removed.
#' - **project**: List of the projects.
#' - **coordinatePrecision**: Set to the least precise `coordinatePrecision`.
#' - **spatial**: Reset based on the new deployments.
#' - **temporal**: Reset based on the new deployments.
#' - **taxonomic**: Combined, with duplicates removed.
#' - **relatedIdentifiers**: Combined, with duplicates removed.
#' - **references**: Combined, with duplicates removed.
#' - Custom properties of `x` are also retained.
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

  # Create xy from x
  xy <- x

  # Merge resources
  xy$resources <- merge_resources(x, y, prefixes)

  # Merge data
  deployments(xy) <- merge_deployments(x, y, prefixes)
  media(xy) <- merge_media(x, y, prefixes)
  observations(xy) <- merge_observations(x, y, prefixes)

  # Merge/update metadata
  xy$name <- NULL
  xy$id <- NULL
  xy$created <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  xy$title <- NULL
  xy$contributors <- unique(c(x$contributors, y$contributors))
  xy$description <- paste(x$description, y$description, sep = "/n")
  xy$version <- "1.0"
  xy$keywords <- unique(c(x$keywords, y$keywords))
  xy$image <- NULL
  xy$homepage <- NULL
  xy$project <- list(x$project, y$project)
  xy$sources <- unique(c(x$sources, y$sources))
  xy$licenses <- unique(c(x$licenses, y$licenses))
  xy$bibliographicCitation <- NULL
  xy$coordinatePrecision <-
    max(x$coordinatePrecision, y$coordinatePrecision, na.rm = TRUE)
  xy$relatedIdentifiers <- unique(c(x$relatedIdentifiers, y$relatedIdentifiers))
  xy$references <- unique(c(x$references, y$references))
  xy$directory <- "."

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


  # Update scopes
  xy <-
    xy %>%
    update_spatial() %>%
    update_temporal() %>%
    update_taxonomic()

  return(xy)
}
