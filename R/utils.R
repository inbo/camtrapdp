#' Create columns, but only if they are missing
#'
#' Adds columns using [dplyr::mutate()], but only if they are absent in the
#' provided data frame.
#'
#' @inherit dplyr::mutate
#' @family helper functions
#' @noRd
#' @examples
#' # Column "space" is not present yet, so it is added
#' mutate_when_missing(cars, space = "The final frontier")
#' # Column "speed" is present, so it is not added
#' mutate_when_missing(cars, speed = "warp 9")
mutate_when_missing <- function(.data, ...) {
  dots <- substitute(list(...))[-1]
  cols_to_check <- names(sapply(dots, deparse))
  columns_to_add <- cols_to_check[!cols_to_check %in% colnames(.data)]
  if (!rlang::is_empty(columns_to_add)) {
    .data <- dplyr::mutate(.data, ...)
  }
  return(.data)
}

#' Create names, but only when they are missing
#'
#' Adds columns `firstName` and `lastName` based on `.data$title` using
#' `mutate_when_missing()`.
#' If the title is a single string, or if the role is `rightsHolder` or
#' `publisher`, the value for both names is `NA_character_`.
#' Else, the first name is the first string before a space, the last name is the
#' string after the first space.

#'
#' @param df A data frame with a `title` and `role` column.
#' @family helper functions
#' @noRd
#' @examples
#' df <- data.frame(
#' title = c("John Doe", "Jane", "Research Institute"),
#' role = c("contact", "contributor", "rightsHolder")
#' )
#' mutate_names(df)

mutate_names <- function(df) {
  df %>%
    dplyr::mutate(
      n_title = stringr::str_count(.data$title, "\\S+")
    ) %>%
    mutate_when_missing(
      firstName = dplyr::if_else(
        !(.data$role %in% c("rightsHolder", "publisher")) &
          .data$n_title > 1,
        purrr::map_chr(
          .data$title,
          ~ strsplit(.x, " ", fixed = TRUE)[[1]][1] # First string before space
        ),
        NA_character_
      ),
      lastName = dplyr::if_else(
        !is.na(.data$firstName),
        # Remove string up until first space
        purrr::map_chr(.data$title, ~ sub("^\\S* ", "", .x)), #
        NA_character_
      )
    ) %>%
    dplyr::select(-n_title)
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
#' @param contributors A data frame with the contributors from
#' `contributors(x)`.
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
    givenName = .$firstName,
    surName = .$lastName,
    organizationName = .$organization, # Discouraged by EML, but used by IPT
    email = .$email,
    userId = if (!is.na(.$orcid)) {
      list(directory = "https://orcid.org/", .$orcid)
    } else {
      NULL
    },
    onlineUrl = .$path
  ))
}
