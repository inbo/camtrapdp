#' Build a data frame with taxonomic information
#'
#' Builds a data frame from the `taxonomy` property in a Camera Trap Data
#' Package object.
#'
#' @inheritParams check_camtrapdp
#' @return Data frame with the taxonomic information.
#' @noRd
build_taxonomy <- function(x) {
  # Extract the taxonomic information
  taxonomic_list <- purrr::pluck(x, "taxonomic")

  # If there is no taxonomic information, return NULL
  if (is.null(taxonomic_list)) {
    return(NULL)
  }

  # Convert list into a data.frame
  purrr::map(
      taxonomic_list,
      purrr::list_flatten,
      name_spec = "{outer}.{inner}"
    ) %>%
    purrr::map(as.data.frame) %>%
    purrr::list_rbind()
}
