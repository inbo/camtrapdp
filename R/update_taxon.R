#' Update taxon
#'
#' @description
#' Updates information for a taxon in data and metadata.
#' This allows:
#' 1. Taxon update: when `to` and `from$scientificName` are the same, then the
#' existing taxon is updated with the provided information.
#' 2. Taxon rename: when `to` and `from$scientificName` are different and the
#' latter is new, then the existing taxon is renamed and updated with the
#' provided information.
#' 3. Taxon lump: when `to` and `from$scientificName` are different and the
#' latter is already present, then the existing taxon is lumped into the already
#' present taxon.
#' Information is retained from the taxon (provided or already present) with
#' (first) a `taxonID` and (second) the most information.
#'
#' @inheritParams print.camtrapdp
#' @param from `scientificName` of the taxon to update.
#' @param to Named list with taxon information, e.g.
#' `list(scientificName = "Ardea", taxonRank = "genus",
#' vernacularname.eng = "great herons")`.
#' @return `x` with updated taxon information.
#' @family transformation functions
#' @export
#' @examples
#' x <- example_dataset()
#'
#' # Update taxonomic information for "Anas platyrhynchos"
#' updated_x <- update_taxon(
#'   x,
#'   from = "Anas platyrhynchos",
#'   to = list (
#'     scientificName = "Anas platyrhynchos",
#'     taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/DGP6",
#'     taxonRank = "species",
#'     vernacularNames.fra = "canard colvert"
#'   )
#' )
#'
#' # Lump "Ardea cinerea" into already present taxon "Ardea".
#' # Provided information is ignored, because the present taxon has a taxonID.
#' updated_x <- update_taxon(
#'   x,
#'   from = "Ardea cinerea",
#'   to = list(scientificName = "Ardea", vernacularname.fra = "grands hérons")
#' )
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

  # Update taxa: remove "from", add "to"
  taxa <-
    taxa(x) %>%
    dplyr::filter(scientificName != from) %>%
    dplyr::bind_rows(to)
  colnames(taxa) <- paste("taxon", colnames(taxa), sep = ".")

  # Update observations: replace "from" with "to", replace taxon. information
  observations <-
    observations(x) %>%
    dplyr::select(-dplyr::starts_with("taxon.")) %>%
    dplyr::mutate(
      scientificName = dplyr::case_when(
        .data$scientificName == from ~ to$scientificName,
        .default = .data$scientificName
      )
    ) %>%
    dplyr::left_join(
      taxa,
      by = dplyr::join_by("scientificName" == "taxon.scientificName"),
      multiple = "first"
    )

  # Assign observations (also updates x$taxonomic)
  observations(x) <- observations

  # Return message
  taxon <- taxa(x) %>% dplyr::filter(scientificName == to$scientificName)
  cli::cli_alert_info("Taxon {.val {from}} is replaced by:")
  cli::cli_dl(taxon, names(taxon))

  return(x)
}
