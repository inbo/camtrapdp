#' Get or set contributors
#'
#' @description
#' `contributors()` gets contributors from the `x$contributors` property in a
#' Camera Trap Data Package object and returns it as a tibble data frame.
#'
#' `contributors()<-` is the assignment equivalent.
#'
#' @inheritParams print.camtrapdp
#' @return A [tibble::tibble()] data frame with the contributors, containing the
#'   following columns (columns absent in `x$contributors` will be created):
#'   - `title`
#'   - `firstName`: if absent, this will be set to the first word in `title`,
#'   except if it is a single word or the `role` is `rightsHolder` or
#'   `publisher`.
#'   - `lastName`: if absent, this will be set to the remaining words in
#'   `title`, with the same exceptions as `firstName`.
#'   - `email`
#'   - `path`
#'   - `role`
#'   - `organization`
#' @family accessor functions
#' @export
#' @examples
#' x <- example_dataset()
#' # Get contributors
#' contributors(x)
#'
#' # Set contributors
#' contributors(x) <- head(contributors(x), 1)
contributors <- function(x) {
  contributors <-
    x$contributors %>%
    purrr::map(., ~ as.data.frame(.)) %>%
    purrr::list_rbind() %>%
    dplyr::as_tibble() %>%
    mutate_when_missing(title = NA_character_) %>%
    mutate_when_missing(email = NA_character_) %>%
    mutate_when_missing(path = NA_character_) %>%
    mutate_when_missing(role = NA_character_) %>%
    mutate_when_missing(organization = NA_character_) %>%
    mutate_person_names() %>%
    dplyr::select(
      title,
      firstName,
      lastName,
      email,
      path,
      role,
      organization
    )
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
