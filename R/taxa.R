#' Get taxa
#'
#' Gets the (unique) scientific names and associated taxonomic information from
#' the observations of a Camera Trap Data Package object.
#'
#' - Duplicates are identified based on `scientificName` and, if present,
#' `taxonID`. The row with the highest number of non-missing taxonomic values is
#' kept. In case of ties, the last row is chosen.
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
  taxa <- observations(x) %>%
    dplyr::filter(!is.na(.data$scientificName)) %>%
    dplyr::select("scientificName", dplyr::starts_with("taxon.")) %>%
    dplyr::distinct() %>%
    dplyr::rename_with(~ sub("^taxon.", "", .x)) %>%
    dplyr::arrange(scientificName)

  # Identify duplicates with the least information, to remove
  duplicates_to_remove <- taxa %>%
    dplyr::mutate(count_non_na = rowSums(!is.na(.))) %>%
    {if ("taxonID" %in% names(taxa)) dplyr::group_by(., scientificName, taxonID)
      else dplyr::group_by(., scientificName)} %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::arrange(count_non_na) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-count_non_na)

  # Remove duplicates
  unique_taxa <-
    taxa %>%
    dplyr::anti_join(duplicates_to_remove, by = names(duplicates_to_remove))

  return(unique_taxa)
}
