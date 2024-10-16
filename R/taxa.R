#' Get taxa
#'
#' @description
#' Gets the (unique) scientific names and associated taxonomic information from
#' the observations of a Camera Trap Data Package object.
#' Duplicate taxa (i.e. with the same `scientificName`) are removed, retaining
#' the taxon with (first) a `taxonID` and (second) the most taxonomic
#' information.
#'
#' @inheritParams print.camtrapdp
#' @return A [tibble::tibble()] data frame with the taxonomic information,
#'   containing at least a `scientificName` column.
#' @family accessor functions
#' @export
#' @examples
#' x <- example_dataset()
#' taxa(x)
taxa <- function(x) {
  check_camtrapdp(x)
  taxa <-
    observations(x) %>%
    dplyr::filter(!is.na(.data$scientificName)) %>%
    dplyr::select("scientificName", dplyr::starts_with("taxon.")) %>%
    dplyr::distinct() %>%
    dplyr::rename_with(~ sub("^taxon.", "", .x)) %>%
    dplyr::arrange(.data$scientificName)

  # Remove duplicates without taxonID
  if ("taxonID" %in% names(taxa)) {
    duplicates_without_taxonid <-
      taxa %>%
      dplyr::group_by(.data$scientificName) %>%
      dplyr::filter(dplyr::n() > 1) %>%
      dplyr::filter(is.na(.data$taxonID))
    taxa <- dplyr::anti_join(
      taxa,
      duplicates_without_taxonid,
      by = names(duplicates_without_taxonid)
    )
  }

  # Remove duplicates with the least information
  duplicates_with_least_info <-
    taxa %>%
    dplyr::mutate(columns_with_info = rowSums(!is.na(.))) %>%
    dplyr::group_by(.data$scientificName) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::arrange(dplyr::desc(.data$columns_with_info)) %>%
    dplyr::slice_tail(n = -1) %>% # Remove first row from group (with most info)
    dplyr::ungroup() %>%
    dplyr::select(-.data$columns_with_info)
  taxa <- dplyr::anti_join(
    taxa,
    duplicates_with_least_info,
    by = names(duplicates_with_least_info)
  )

  # Drop any columns that are empty (e.g as result of dropping duplicates)
  cols_to_keep <- colSums(is.na(taxa)) != nrow(taxa)
  taxa <- taxa[, cols_to_keep, drop = FALSE]

  return(taxa)
}
