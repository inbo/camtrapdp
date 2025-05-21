#' Get or set contributors
#'
#' @description
#' `contributors()` gets contributors from the `x$contributors` property in a
#' Camera Trap Data Package object and returns it as a tibble data frame.
#'
#' `contributors()<-` is the assignment equivalent.
#'
#' @inheritParams print.camtrapdp
#' @return A [tibble::tibble()] data frame with the contributors.
#' @family accessor functions
#' @export
#' @examples
#' x <- example_dataset()
#' contributors(x)
#'
#' # Set contributors
#' contributors(x) <- head(contributors(x), 1)
contributors <- function(x) {
  contributors <-
    x$contributors %>%
    purrr::map(., ~ as.data.frame(.)) %>%
    purrr::list_rbind() %>%
    dplyr::as_tibble()
  return(contributors)
}

#' @rdname contributors
#' @param value A data frame to assign as contributors.
#' @export
"contributors<-" <- function(x, value) {
  if (!is.data.frame(value)) {
    cli::cli_abort(
      "{.arg value} must be a data.frame, not {.type {value}}.",
      class = "camtrapdp_error_assignment_wrong_class"
    )
  }

  purrr::pluck(x, "contributors") <- purrr::transpose(value)
  return(x)
}
