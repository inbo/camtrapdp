#' Get contributors and build a data frame
#'
#' Gets the `x$contributors` property in a Camera Trap Data Package object and
#' builds a data frame with the contributors.
#'
#' @inheritParams print.camtrapdp
#' @return A data frame with the contributors.
#' @family accessor functions
#' @export
#' @examples
#' x <- example_dataset()
#' contributors(x)
contributors <- function(x) {
  orcid_regex <- "(\\d{4}-){3}\\d{3}(\\d|X)"
  contributors <-
    purrr::map_dfr(
      x$contributors,
      ~ as.data.frame(., stringsAsFactors = FALSE)
    ) %>%
    dplyr::filter(!.data$role %in% c("rightsHolder", "publisher")) %>%
    mutate_when_missing(path = character()) %>% # Guarantee path col
    dplyr::mutate(
      first_name = purrr::map_chr(
        .data$title,
        ~ strsplit(.x, " ", fixed = TRUE)[[1]][1] # First string before space
      ),
      last_name = purrr::map_chr(
        .data$title,
        ~ sub("^\\S* ", "", .x) # Remove string up until first space
      ),
      orcid = ifelse( # Move ORCID from path to separate column
        !is.na(regexpr(orcid_regex, .data$path)),
        regmatches(.data$path, regexpr(orcid_regex, .data$path)),
        NA_character_
      ),
      path = ifelse(
        grepl(orcid_regex, .data$path),
        NA_character_,
        .data$path
      )
    )
  return(contributors)
}

