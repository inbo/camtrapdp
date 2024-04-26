#' Get taxa
#'
#' Gets the (unique) scientific names and associated taxonomic information from
#' the observations of a Camera Trap Data Package object.
#'
#' @inheritParams check_camtrapdp
#' @return [tibble()] data frame with the taxonomic information, containing at
#'   least a `scientificName` column.
#' @family accessor functions
#' @export
#' @examples
#' x <- example_dataset()
#' taxa(x)
taxa <- function(x) {
  check_camtrapdp(x)
  observations(x) %>%
    dplyr::filter(!is.na(.data$scientificName)) %>%
    dplyr::select("scientificName", dplyr::starts_with("taxon.")) %>%
    dplyr::distinct() %>%
    dplyr::rename_with(~ sub("^taxon.", "", .x))
}
