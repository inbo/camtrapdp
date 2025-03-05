#' Update taxon information
#'
#' @inheritParams print.camtrapdp
#' @param from `scientificName` to be updated.
#' @param to List with new taxon information. Must have a `scientificName`
#' property.
#' @return `x` with updated taxon information.
#' @family transformation functions
#' @export
#'
#' @examples
#' x <- example_dataset()
#' from <- "Anas platyrhynchos"
#' to <- list(
#' scientificName = "Anas",
#' taxonID = "https://www.checklistbank.org/dataset/9910/taxon/V8R",
#' taxonRank = "genus",
#' vernacularNames.eng = "dabbling ducks"
#' )
update_taxon <- function(x, from, to) {
  check_camtrapdp(x)

  if (!from %in% taxa(x)$scientificName) {
    cli::cli_warn(
      c(
        "{.arg {from}} is not found as a scientificName in the data.",
        "i" = "No taxon is updated."
      ),
      class = "camtrapdp_warning_taxon_not_found"
    )
    return(x)
  }

  if (!"scientificName" %in% names(to)) {
    cli::cli_abort(
      c(
        "{.arg {to}} must have a scientificName property."
      ),
      class = "camtrapdp_error_scientificname_missing"
    )
  }

  # Update taxonomy
  taxonomy_new <-
    taxa(x) %>%
    dplyr::filter(scientificName != from) %>%
    dplyr::bind_rows(to)

  # Add taxon. as column suffix
  colnames(taxonomy_new) <- paste("taxon", colnames(taxonomy_new), sep = ".")

  # Remove taxonomic information in observations
  observations(x) <-
    observations(x) %>%
    dplyr::select(-dplyr::starts_with("taxon."))

  # Change scientificName in observations
  observations(x) <-
    observations(x) %>%
    dplyr::mutate(
      scientificName = dplyr::if_else(
        .data$scientificName == from,
        to$scientificName,
        .data$scientificName
      )
    )

  # Join taxonomy with observations, x$taxonomic is automatically updated with the assignment function
  observations(x) <-
    dplyr::left_join(
      observations(x),
      taxonomy_new,
      by = dplyr::join_by("scientificName" == "taxon.scientificName")
    )

  # Return message
  cli::cli_inform(
    c(
      "v" = "Taxon {.val {from}} is replaced by {.val {to}}."
    ),
    class = "camtrapdp_message_update_taxon"
  )


  return(x)
}
