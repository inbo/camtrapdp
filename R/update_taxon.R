#' Update taxon information
#'
#' @description
#' Updates taxon information in both data and metadata.
#'
#' If `to`is a list containing a scientificName that already exists in the
#' dataset, duplicate taxa (i.e. with the same `scientificName`) are removed.
#' The function retains the taxon with (first) a `taxonID` and (second) the most
#' taxonomic information.
#'
#' Taxonomic information for a `scientificName` can be updated by replacing the
#' existing taxon with the same `scientificName` but different taxonomic details
#' provided in `to`.
#'
#'
#' @inheritParams print.camtrapdp
#' @param from `scientificName` to be updated.
#' @param to List with new taxon information. It must have a `scientificName`
#' property.
#' @return `x` with updated taxon information.
#' @family transformation functions
#' @export
#'
#' @examples
#' # Example 1: change scientificName
#' x <- example_dataset()
#' from <- "Ardea cinerea"
#' to <- list(
#' scientificName = "Ardea",
#' taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/32FH"
#'  )
#' x_updated <- update_taxon(x, from, to)
#' taxa(x_updated)
#'
#' # Example 2: update taxon information
#' from <- Anas platyrhynchos"
#' to <- list(
#' scientificName = "Anas platyrhynchos",
#' taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/DGP6",
#' taxonRank = "species",
#' vernacularNames.fr = "canard chipeau"
#' )
#'
#' x_updated <- update_taxon(x, from, to)
#' taxa(x_updated)
update_taxon <- function(x, from, to) {
  check_camtrapdp(x)

  if (!from %in% taxa(x)$scientificName) {
    cli::cli_warn(
      c(
        "{.arg {from}} is not found as a {.field scientificName} in the data.",
        "x" = "No taxon is updated."
      ),
      class = "camtrapdp_warning_taxon_not_found"
    )
    return(x)
  }

  if (!"list" %in% class(to)) {
    cli::cli_abort(
      c(
        "{.arg to} must be a named list.",
        "i" = "{.arg to} is not a list."
      ),
      class = "camtrapdp_error_class_invalid"
    )
  }

  if (any(names(to) == "")) {
    cli::cli_abort(
      c(
        "{.arg to} must be a named list.",
        "i" = "Not all elements of {.arg to} have a name."
      ),
      class = "camtrapdp_error_list_not_named"
    )
  }

  if (!"scientificName" %in% names(to)) {
    cli::cli_abort(
      c(
        "{.arg to} must have a {.field scientificName} property.",
        "i" = paste(
          "No element with name {.field scientificName} is found in",
          "{.arg to}."
          )
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
  taxon <- taxa(x) %>% filter(scientificName == to$scientificName)
  cli::cli_alert_info("Taxon {.val {from}} is replaced by:")
  cli::cli_dl(taxon, names(taxon))

  return(x)
}
