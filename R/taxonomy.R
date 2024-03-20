#' Get taxonomy
#'
#' Gets the taxonomy information from the observations of a Camera Trap Data
#' Package object. Notice that this information could differ from the taxonomy
#' described in `taxonomic` slot.
#'
#' @inheritParams version
#' @return `tibble()` data.frame.
#' @family accessor functions
#' @export
taxonomy <- function(x) {
  observations(x) %>%
    dplyr::distinct(.data$scientificName)
}
