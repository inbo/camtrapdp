#' Get taxonomy
#'
#' Gets the taxonomy information from the observations of a Camera Trap Data
#' Package object. Notice that this information could differ from the taxonomy
#' described in `taxonomic` slot.
#'
#' @inheritParams version
#' @return `tibble()` data.frame with taxonomic information. It contains at
#'   least one column called `scientificName`.
#' @family accessor functions
#' @export
#' @examples
#' dataset <- example_dataset()
#' taxonomy(dataset)
taxonomy <- function(x) {
  observations(x) %>%
    dplyr::distinct(.data$scientificName,
                    dplyr::starts_with("taxon."))
}
