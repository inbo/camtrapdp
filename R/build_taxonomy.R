#' Build a data frame with taxonomic information
#'
#' Builds a data frame from the `taxonomy` property in a Camera Trap Data
#' Package object.
#'
#' @inheritParams print.camtrapdp
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
  taxonomy_df <-
    purrr::map(
      taxonomic_list,
      purrr::list_flatten,
      name_spec = "{outer}.{inner}"
    ) %>%
    purrr::map(as.data.frame) %>%
    purrr::list_rbind()

  # Warn if there are duplicate scientificNames
  scientific_names <- purrr::pluck(taxonomy_df, "scientificName")
  duplicate_names <- scientific_names[duplicated(scientific_names)]
  if (length(duplicate_names) > 0) {
    cli::cli_warn(
      c(
        "{length(duplicate_names)} duplicate {.field scientificName}{?s} found
         in taxonomy:",
        "i" = "Only the first {.val {duplicate_names}} will be used."
      ),
      class = "camtrapdp_warning_duplicate_scientificname"
    )
  }

  # Only keep the first row if a scientificName occurs more than once
  taxonomy_df <- dplyr::distinct(
    taxonomy_df,
    .data$scientificName,
    .keep_all = TRUE
  )

  # Drop any columns that are empty (e.g as result of dropping duplicates)
  cols_to_keep <- colSums(is.na(taxonomy_df)) != nrow(taxonomy_df)
  taxonomy_df <- taxonomy_df[, cols_to_keep, drop = FALSE]

  # Return data.frame
  return(taxonomy_df)
}
