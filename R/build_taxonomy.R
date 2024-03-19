#' Parse the taxon information from a Camera Trap Data Package object
#'
#' @param x
#'
#' @return A tibble with the taxonomic information from a Camera Trap Data
#'   Package
#'
#' @examples
build_taxonomy <- function(x) {
  # Extract the taxonomic information only
  taxonomic_list <- x$taxonomic

  # Convert list into a tibble with list columns
  purrr::map(taxonomic_list, tibble::as_tibble) %>%
    purrr::list_rbind()

}
