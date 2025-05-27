#' Get individuals
#'
#' Gets the (unique) individuals from the observations of a Camera Trap Data
#' Package object.
#'
#' @inheritParams print.camtrapdp
#' @return A [tibble::tibble()] data frame with the individuals that have an
#' `individualID`, containing the following columns:
#'   - `individualID`
#'   - `scientificName`
#'   - `lifeStage`
#'   - `sex`
#' @family accessor functions
#' @export
#' @examples
#' x <- example_dataset()
#' individuals(x)
individuals <- function(x) {
  check_camtrapdp(x)
  observations(x) %>%
    dplyr::filter(!is.na(.data$individualID)) %>%
    dplyr::distinct(
      .data$individualID,
      .data$scientificName,
      .data$lifeStage,
      .data$sex
    )
}
