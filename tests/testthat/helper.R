#' Get snapshot for write_dwc()
#'
#' Wrapper of `write_dwc()` that returns path of selected output file.
#' Needed for `testthat::expect_snapshot_file()` which expects the path of a
#' single file to compare against snapshot.
#' @inheritParams write_dwc
#' @param file Either `occurrence` or `audiovisual` to select which output file of
#'   `write_dwc()` to return.
#' @return Path of selected output file.
#' @noRd
#' @examples write_dwc_snapshot(mica, tempdir(), "occurrence")
write_dwc_snapshot <- function(package, directory, file){
  suppressMessages(write_dwc(package, directory))
  switch(
    file,
    occurrence = file.path(directory, "dwc_occurrence.csv"),
    audiovisual = file.path(directory, "dwc_audiovisual.csv")
  )
}

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
      system.file("extdata", "meta.xml", package = "camtraptor")
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
      system.file("extdata", "meta.xml", package = "camtraptor")
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
