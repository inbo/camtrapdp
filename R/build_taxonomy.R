#' Build a data frame with taxonomic information
#'
#' Builds a data frame from the `taxonomy` property in a Camera Trap Data
#' Package object.
#'
#' @inheritParams version
#' @return `tibble()` data frame with the taxonomic information.
#' @noRd
build_taxonomy <- function(x) {
  # Extract the taxonomic information
  taxonomic_list <- purrr::pluck(x, "taxonomic")

  # If there is no taxonomic information, return NULL
  if (is.null(taxonomic_list)) {
    return(NULL)
  }

  # Convert list into a data.frame
  taxon_df <- purrr::map(
      taxonomic_list,
      purrr::list_flatten,
      name_spec = "{outer}.{inner}"
    ) %>%
    purrr::map(dplyr::as_tibble) %>%
    purrr::list_rbind()
  # Add prefix "tax." to all columns
  colnames(taxon_df) <- paste("tax", colnames(taxon_df), sep = ".")
  taxon_df
}
