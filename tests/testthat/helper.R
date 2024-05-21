#' testthat wrapper to compare DwC-A files against meta.xml file for fieldsnames
#'
#' @param file Filepath from of file from DwC-A file to compare against
#'   `meta.xml` included in the package.
#'   The basename can either be `dwc_occurrence.csv` or `dwc_audiovisual.csv`
#' @inheritDotParams expect_identical info label
#' @noRd
#' @examples
#' expect_fields("tests/testthat/_snaps/write_dwc/dwc_audiovisual.csv")
expect_fields <- function(file, ...) {
  xml_list <-
    xml2::read_xml(
      system.file("extdata", "meta.xml", package = "camtrapdp")
    ) %>%
    xml2::as_list()
  file_is_core <- basename(file) == "dwc_occurrence.csv"

  xml_fields <-
    xml_list %>%
    purrr::chuck("archive", ifelse(file_is_core, "core", "extension")) %>%
    purrr::map_dfr(~ dplyr::tibble(
      index = as.numeric(attr(.x, which = "index")),
      term = attr(.x, which = "term")
    )) %>%
    dplyr::filter(!is.na(term)) %>%
    dplyr::mutate(field = basename(term), .keep = "unused")

  file_cols <-
    readr::read_csv(file, show_col_types = FALSE) %>%
    readr::spec() %>%
    purrr::chuck(1) %>%
    names()

  file_fields <-
    # remove the namespace from the csv header, if present
    dplyr::tibble(field = stringr::str_extract(file_cols, "[a-zA-Z]+$")) %>%
    dplyr::mutate(index = as.integer(rownames(.)) - 1, .before = field)

  testthat::expect_identical(
    xml_fields,
    file_fields,
    ...
  )
}

#' testthat wrapper to compare DwC-A files against meta.xml file for file location
#'
#' @param file Filepath from of file from DwC-A file to compare against
#'   `meta.xml` included in the package.
#'   The basename can either be `dwc_occurrence.csv` or `dwc_audiovisual.csv`
#' @inheritDotParams expect_identical info label
#' @noRd
#' @examples
#' expect_location("tests/testthat/_snaps/write_dwc/dwc_audiovisual.csv")
expect_location <- function(file, ...) {
  xml_list <-
    xml2::read_xml(
      system.file("extdata", "meta.xml", package = "camtrapdp")
    ) %>%
    xml2::as_list()
  file_is_core <- basename(file) == "dwc_occurrence.csv"
  file_locations <-
    purrr::chuck(
      xml_list,
      "archive",
      ifelse(file_is_core, "core", "extension"),
      "files",
      "location"
    )
  testthat::expect_identical(unlist(file_locations), basename(file))
}
