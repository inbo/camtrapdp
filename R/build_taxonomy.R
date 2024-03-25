#' Build a data frame with taxonomic information
#'
#' Builds a data frame from the `taxonomy` property in a Camera Trap Data
#' Package object.
#'
#' @inheritParams version
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
  taxonomy_df <- purrr::map(
    taxonomic_list,
    purrr::list_flatten,
    name_spec = "{outer}.{inner}"
  ) %>%
    purrr::map(as.data.frame) %>%
    purrr::list_rbind()

  # Warn if there are duplicate species
  if (anyDuplicated(taxonomy_df$scientificName)) {
    cli::cli_warn(
      "Duplicate scientificNames present in {.arg x$taxonmic},
      only the first one will be returned.",
      class = "camtrapdp_warning_duplicate_scientificname"
    )
  }


  # Only keep the first row if a scientificName occurs more than once
  taxonomy_df <- dplyr::distinct(taxonomy_df, scientificName, .keep_all = TRUE)

  # Drop any columns that are now empty after dropping the duplicate records
  columns_to_keep <- colSums(is.na(taxonomy_df)) != nrow(taxonomy_df)
  taxonomy_df <- taxonomy_df[,columns_to_keep, drop = FALSE]

  # Return data.frame
  return(taxonomy_df)
}
