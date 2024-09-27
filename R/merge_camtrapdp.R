#' Merge Camera Trap Data packages
#'
#' @param x1,x2 Camera Trap Data Package objects (as returned by
#' `read_camtrapdp()`), to be coerced to one.
#' @param prefix If there are duplicate IDs in x1 an x2, these prefixes will be
#' added to all the values of each identifier with duplicates, to disambiguate
#' them. Should be a character vector of length 2.
#' @return `x`
#' @family transformation functions
#' @export
#' @examples
#' x1 <- example_dataset() %>%
#'   filter_deployments(deploymentID %in% c("00a2c20d", "29b7d356"))
#' x2 <- example_dataset() %>%
#'   filter_deployments(deploymentID %in% c("577b543a", "62c200a9"))
#' x_merged <- merge_camtrapdp(x1, x2)
merge_camtrapdp <- function(x1, x2, prefix = c("x.", "y.")) {
  check_camtrapdp(x1)
  check_camtrapdp(x2)

  # check if identifiers have duplicates
  results_duplicate_ids <- check_duplicate_ids(x1, x2)

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
        class = "camtrapdp_warning_prefix_invalid"
      )
    }

    if (any(is.na(prefix))) {
      cli::cli_abort(
        "{.arg prefix} can't be 'NA'.",
        class = "camtrapdp_warning_prefix_NA"
      )
    }

    x1 <- add_prefix(x1, results_duplicate_ids, prefix[1])
    x2 <- add_prefix(x2, results_duplicate_ids, prefix[2])
  }

  # Merge resources
  x <- x1
  deployments(x) <- dplyr::bind_rows(deployments(x1), deployments(x2))
  media(x) <- dplyr::bind_rows(media(x1), media(x2))
  observations(x) <- dplyr::bind_rows(observations(x1), observations(x2))

  # Merge/update metadata
  x$name <- NA
  x$id <- NA
  x$created <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  x$title <- NA
  x$contributors <- remove_duplicates(c(x1$contributors, x2$contributors))
  x$description <- paste(x1$description, x2$description, sep = "/n")
  x$version <- "1.0"
  x$keywords <- unique(x1$keywords, x2$keywords)
  x$image <- NULL
  x$homepage <- NULL
  x$sources <- remove_duplicates(c(x1$sources, x2$sources))
  x$licenses <- remove_duplicates(c(x1$licenses, x2$licenses))
  x$project <- NULL
  x$projects <- list(x1$project, x2$project)
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
