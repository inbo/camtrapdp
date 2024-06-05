#' Helper function to compare a CSV file against the meta.xml file
#'
#' Compares the file name and column headers (without namespace) in a CSV file
#' with (respectively) the `location` and `fields` as defined in the `meta.xml`
#' file.
#'
#' @param file Path to CSV file.
#' @param core Name of core CSV file.
#' @inheritDotParams expect_identical info label
#' @noRd
#' @examples
#' expect_meta_match("tests/testthat/_snaps/write_dwc/multimedia.csv")
expect_meta_match <- function(file, core = "occurrence.csv", ...) {
  core_or_extension <- ifelse(basename(file) == core, "core", "extension")

  # Parse reference meta.xml from inst/extdata/meta.xml
  xml_list <-
    xml2::read_xml(system.file("extdata", "meta.xml", package = "camtrapdp")) %>%
    xml2::as_list()
  xml_file_location <-
    purrr::chuck(xml_list, "archive", core_or_extension, "files", "location") %>%
    unlist()
  xml_file_fields <-
    xml_list %>%
    purrr::chuck("archive", core_or_extension) %>%
    purrr::map_dfr(~ dplyr::tibble(
      index = as.numeric(attr(.x, which = "index")),
      term = attr(.x, which = "term")
    )) %>%
    dplyr::filter(!is.na(term)) %>%
    dplyr::mutate(field = basename(term), .keep = "unused")

  # Get fields from csv
  csv_file_cols <-
    readr::read_csv(file, show_col_types = FALSE) %>%
    colnames() %>%
    purrr::map_chr(~ sub("^[A-Za-z]+:", "", .x)) # Remove namespace like "dcterms:"
  csv_file_fields <-
    dplyr::tibble(field = csv_file_cols) %>%
    dplyr::mutate(index = as.integer(rownames(.)) - 1, .before = field) # Add index

  # Compare
  testthat::expect_identical(csv_file_fields, xml_file_fields, )
  testthat::expect_identical(basename(file), xml_file_location)
}
