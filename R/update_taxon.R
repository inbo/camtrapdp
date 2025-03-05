#' Title
#'
#' @param x
#' @param from
#' @param to
#'
#' @return
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

  if (!"scientificName" %in% names(to)) {
    cli::cli_abort(
      c(
        "{.arg {to}} must have a scientificName property.",
        "i" = "Assign to {.field {to}$scientificName}."
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
}
