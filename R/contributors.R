#' Get contributors as a data frame
#'
#' Gets contributors from the `x$contributors` property in a Camera Trap Data
#' Package object and returns it as a data frame.
#'
#' @inheritParams print.camtrapdp
#' @return A [tibble::tibble()] data frame with the contributors.
#' @family accessor functions
#' @export
#' @examples
#' x <- example_dataset()
#' contributors(x)
contributors <- function(x) {
  contributors <-
    x$contributors %>%
    purrr::map(., ~ as.data.frame(.)) %>%
    purrr::list_rbind() %>%
    dplyr::as_tibble()
  return(contributors)
}
