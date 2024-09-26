#' Merge Camera Trap Data packages
#'
#' @param x1,x2 Camera Trap Data Package objects (as returned by
#' `read_camtrapdp()`), to be coerced to one.
#' @param name A short url-usable (and preferably human-readable)
#' [name](https://specs.frictionlessdata.io/data-package/#name) for the
#' merged package.
#' @param title A string providing a
#' [title](https://specs.frictionlessdata.io/data-package/#title) or one
#' sentence description for the merged package.
#' @return `x`
#' @family transformation functions
#' @export
#' @examples
#' x1 <- example_dataset() %>%
#'   filter_deployments(deploymentID %in% c("00a2c20d", "29b7d356"))
#' x2 <- example_dataset() %>%
#'   filter_deployments(deploymentID %in% c("577b543a", "62c200a9"))
#' x_merged <- merge_camtrapdp(x1, x2)
merge_camtrapdp <- function(x1, x2) {
  check_camtrapdp(x1)
  check_camtrapdp(x2)

  # Replace duplicated IDs between `x1` and `x2` in `x2` with hashes
  x2 <- replace_duplicatedIDs(x1, x2)

  # Merge resources
  x <- x1
  deployments(x) <- dplyr::bind_rows(deployments(x1), deployments(x2))
  media(x) <- dplyr::bind_rows(media(x1), media(x2))
  observations(x) <- dplyr::bind_rows(observations(x1), observations(x2))

  # Merge/update metadata
  x$name <- NA
  # Create new ID
  x$id <- NA
  x$created <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  x$title <- NA
  x$contributors <- remove_duplicates(c(x1$contributors, x2$contributors))
  paragraph <- paste0(
    "This dataset is a combination of 2 datasets: ", x1$title, "and", x2$title,
    ".")
  x$version <- "1.0"
  x$keywords <- unique(x1$keywords, x2$keywords)
  x$image <- NULL
  x$homepage <- NULL
  x$sources <- remove_duplicates(c(x1$sources, x2$sources))
  x$licenses <- remove_duplicates(c(x1$licenses, x2$licenses))
  x$bibliographicCitation <- NULL
  x$coordinatePrecision <-
    max(x1$coordinatePrecision, x2$coordinatePrecision, na.rm = TRUE)
  if (!is.null(x1$id)) {
    relatedIdentifiers_x1 <- list(
      relationType = "IsDerivedFrom",
      relatedIdentifier = x1$id,
      resourceTypeGeneral = "Data package",
      relatedIdentifierType = "id"
    )
  } else {
    relatedIdentifiers_x1 <- list()
  }
  if (!is.null(x2$id)) {
    relatedIdentifiers_x2 <- list(
      relationType = "IsDerivedFrom",
      relatedIdentifier = x2$id,
      resourceTypeGeneral = "Data package",
      relatedIdentifierType = "id"
    )
  } else {
    relatedIdentifiers_x2 <- list()
  }
  new_relatedIdentifiers <- list(relatedIdentifiers_x1, relatedIdentifiers_x2)
  x$relatedIdentifiers <- remove_duplicates(
    c(x1$relatedIdentifiers, x2$relatedIdentifiers, new_relatedIdentifiers)
  )

  x$references <- remove_duplicates(c(x1$references, x2$references))

  x <-
    update_spatial(x) %>%
    update_temporal() %>%
    update_taxonomic()

  return(x)
}
