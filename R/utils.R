#' Create columns if they are missing
#'
#' Adds columns using [dplyr::mutate()], but only if they are absent in the
#' provided data frame.
#'
#' @inherit dplyr::mutate
#' @return Data frame with the extra columns.
#' @family helper functions
#' @noRd
#' @examples
#' # The data frame cars contains 2 columns (speed and dist)
#' mutate_if_missing(
#'   cars,
#'   speed = "warp 9", # Present, will not be overwritten
#'   space = "The final frontier" # Absent, will be overwritten
#' )
mutate_if_missing <- function(.data, ...) {
  args <- rlang::quos(...)
  new_columns <- args[!names(args) %in% colnames(.data)]
  dplyr::mutate(.data, !!!new_columns)
}

#' Create first and last names from title if they are missing
#'
#' Adds columns `firstName` and `lastName` from respectively the first and
#' remaining words in `title`, but only if they are absent in the provided data
#' frame.
#' `firstName` and `lastName` will be set to `NA_character_` if `title` is a
#' single word, or if the role is `rightsHolder` or `publisher`.
#'
#' @param df A data frame with a `title` and `role` column.
#' @return Data frame with the extra columns.
#' @family helper functions
#' @noRd
#' @examples
#' df <- data.frame(
#'   title = c("John Doe", "Jane", "Research Institute"),
#'   role = c("contact", "contributor", "rightsHolder")
#' )
#' mutate_person_names(df)
mutate_person_names <- function(df) {
  df %>%
    dplyr::mutate(
      n_title = stringr::str_count(.data$title, "\\S+")
    ) %>%
    mutate_if_missing(
      firstName = dplyr::if_else(
        !(.data$role %in% c("rightsHolder", "publisher")) & .data$n_title > 1,
        stringr::str_extract(.data$title, "^\\S*"), # First word before space
        NA_character_
      ),
      lastName = dplyr::if_else(
        !is.na(.data$firstName),
        stringr::str_replace(.data$title, "^\\S* ", ""), # Remove first word
        NA_character_
      )
    ) %>%
    dplyr::select(-"n_title")
}

#' Expand columns
#'
#' Expands a data frame with columns. Added columns will have `NA_character_`
#' values, existing columns of the same name will not be overwritten.
#'
#' @param df A data frame.
#' @param colnames A character vector of column names.
#' @return Data frame expanded with columns that were not yet present.
#' @family helper functions
#' @noRd
expand_cols <- function(df, colnames) {
  cols_to_add <- setdiff(colnames, colnames(df))
  df[, cols_to_add] <- NA_character_
  return(df)
}

#' Lists the names of additional resources in a Camera Trap Data Package
#'
#' @inheritParams print.camtrapdp
#' @return Character vector with the additional resource names.
#' @family helper functions
#' @noRd
additional_resources <- function(x) {
  camtrapdp_resource_names <- c("deployments", "media", "observations")
  resource_names <- frictionless::resources(x)
  resource_names[!resource_names %in% camtrapdp_resource_names]
}

#' Create list of contributors in EML format
#'
#' @param contributors A data frame returned by `contributors(x)`.
#' @return List of contributors as `emld::responsibleParty` objects.
#' @family helper functions
#' @noRd
create_eml_contributors <- function(contributors) {
  orcid_regex <- "(\\d{4}-){3}\\d{3}(\\d|X)"
  contributor_list <-
    contributors %>%
    dplyr::mutate(
      # Move ORCID from path to separate column
      orcid = stringr::str_extract(.data$path, orcid_regex),
      path = dplyr::if_else(
        grepl(orcid_regex, .data$path),
        NA_character_,
        .data$path
      )
    ) %>%
    purrr::transpose()
  purrr::map(contributor_list, ~ EML::set_responsibleParty(
    givenName = .x$firstName,
    surName = .x$lastName,
    organizationName = .x$organization, # Discouraged by EML, but used by IPT
    email = .x$email,
    userId = if (!is.na(.x$orcid)) {
      list(directory = "https://orcid.org/", .x$orcid)
    } else {
      NULL
    },
    onlineUrl = .x$path
  ))
}

#' Clean list
#'
#' Removes all elements from a list that meet a criterion function, e.g.
#' [is.null()] for empty elements.
#' Removal can be recursive to guarantee elements are removed at any level.
#' Function is copied and adapted from `rlist::list.clean()` (MIT licensed), to
#' avoid requiring full `rlist` dependency.
#'
#' @param x List or vector.
#' @param fun Function returning `TRUE` for elements that should be removed.
#' @param recursive Whether list should be cleaned recursively.
#' @return Cleaned list.
#' @family helper functions
#' @noRd
clean_list <- function(x, fun = is.null, recursive = FALSE) {
  if (recursive) {
    x <- lapply(x, function(item) {
      if (is.list(item)) {
        clean_list(item, fun, recursive = TRUE)
      } else {
        item
      }
    })
  }
  "[<-"(x, vapply(x, fun, logical(1L)), NULL)
}
