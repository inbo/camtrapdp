#' Get taxonomic metadata as a data frame
#'
#' Gets taxonomic information from the `x$taxonomic` property in a Camera Trap
#' Data Package object and returns it as a data frame.
#'
#' @inheritParams print.camtrapdp
#' @return A [tibble::tibble()] data frame with the taxonomic information.
#' @noRd
taxonomic <- function(x) {
  # Extract the taxonomic information
  taxonomic_list <- purrr::pluck(x, "taxonomic")

  # If there is no taxonomic information, return NULL
  if (is.null(taxonomic_list)) {
    return(NULL)
  }

  # Convert list into a data.frame
  taxa <-
    purrr::map(
      taxonomic_list,
      purrr::list_flatten,
      name_spec = "{outer}.{inner}"
    ) %>%
    purrr::map(as.data.frame, stringsAsFactors = FALSE) %>%
    purrr::list_rbind() %>%
    dplyr::as_tibble()

  # Warn if there are duplicate scientificNames
  scientific_names <- purrr::pluck(taxa, "scientificName")
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
  taxa <- dplyr::distinct(
    taxa,
    .data$scientificName,
    .keep_all = TRUE
  )

  # Drop any columns that are empty (e.g as result of dropping duplicates)
  cols_to_keep <- colSums(is.na(taxa)) != nrow(taxa)
  taxa <- taxa[, cols_to_keep, drop = FALSE]

  # Return data.frame
  return(taxa)
}
